/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau, Thomas R. Murrills
-/
module

public meta import ImportGraph.Imports.Pretty
public meta import ImportGraph.Lean.MessageData
public meta import ImportGraph.Shake.Workspace
public meta import ImportGraph.Util.Diff.Message
public meta import ImportGraph.Util.Diff.Shake
public meta import ImportGraph.Widget.Collapsible
public meta import ImportGraph.Widget.Copy
public meta import Lean.Meta.Tactic.TryThis

/-!
# `import_diff`: comparing different import states

`import_diff` compares statistics for two import states in the module system.

## Future work

- More statistics, possibly: `olean` sizes, `prevs` and `depths` specific to the current package?
- Better table rendering
- Ability to inspect which imports they are?
- Tooling for the non-diff case, i.e. to view statistics on the current import state in isolation?
-/

public meta section

open Lean

namespace ImportGraph

syntax diffAbsolute :=
  (&"from" ppLine sepByIndentSemicolon(Parser.Module.import))?
  &"to" ppLine sepByIndentSemicolon(Parser.Module.import)

/-- Removes one of the current imports to this file. -/
syntax importMinus := &"-"
/-- Adds an import to the current imports of this file. -/
syntax importPlus := &"+"

/-- An import added to (`+`) or removed from (`-`) the imports of the current file to form the
final import state. -/
syntax diffCurrentItem :=
  withPosition(importMinus (colGt Parser.Module.import)?)
    <|> withPosition(importPlus colGt Parser.Module.import)

syntax diffCurrent := sepByIndentSemicolon(diffCurrentItem)

/--
`#import_diff` compares two import states with respect to the import hierarchy, and reports
statistics on the difference. (The compared imports do not need to be present in the current file.)

There are two ways to use `import_diff`:

**Relative** to the current file's imports, using `+` and `-`:

```
#import_diff
  +public import Foo
  -meta import Bar.Baz
  +import Bar
```

This will take the current file's imports as the initial import state, and diff against the final import state constructed by adding in the `+` imports and removing the `-` imports.

**Absolute**, using `to` and optionally `from`. The `from` imports are the initial import state, and the `to` imports the final one. If `from` is omitted, the current file's imports are used as the initial import state.

```
#import_diff
  from
    public import Foo
    meta import Bar
    import Baz
  to
    public import Foo
    meta import Bar.Baz
    import Bar
    import Baz
```

Note: in both cases, imports may be separated by `;` instead of newlines, if desired.

The output is a table of the form

```null
                 Δ   +   -   ∩
modules imported •   •   •   •

public           •   •   •   •
private          •   •   •   •
public meta      •   •   •   •
private meta     •   •   •   •

previous modules •   •   •   •
ir loaded        •   •   •   •

                 Δ   new old
build depth      •   •   •
```

(Changes due to `import all` are shown only if nonzero.)

**Columns:**
- `Δ`: the net changes (collapsing `+` and `-`)
- `+`: the size of the final state not present in the initial state
- `-`: the size of the initial state not present in the final state
- `∩`: the size of the overlap between the initial and final states

**Rows:**
- `modules imported`: The comparison of `modules imported` is module-system aware, and only
  includes modules that actually make it to the environment (i.e. whose `*.olean`s are actually
  loaded).
- `public`/`private`/...: The count of changes to phase and visibility. Note that this might exceed
  modules imported, as the availability of a module might change between the initial and final
  state while it is imported in both. E.g., a module may have been upgraded from a private import to
  a public import, or a module might now be imported as both meta and non-meta, while its imported
  status remained unchanged.
- `previous modules` compares the total number of prior modules in the build graph, ignoring module
  system annotations.
- `ir loaded`: compares the number of modules whose IR is loaded. `meta` imports cause more IR to
  be loaded, as the IR for every module above them is loaded, ignoring module system annotations.
  (Note: this is only accurate for `lake build`, as the server loads much more IR.)
- `build depth`: compares the old and new depth of the build graph (counting the current module),
  ignoring module system annotations.

### Notes

- Lean prepends `public import Init` and `public meta import Init` to each file behind the scenes
  (assuming you have not written `prelude`, which suppresses this behavior). However, the shared
  column (`∩`) deliberately ignores all contributions from these `Init` imports to provide a more
  useful sense of what two import states have in common. (`build depth` is the only statistic which
  *does* include contributions from `Init`.)
-/
syntax (name := importDiff) (priority := high)
  "#import_diff " (diffAbsolute <|> diffCurrent) : command

open Shake Elab Command

/-- An `Import` with a `ModIdx` in some `WorkspaceModel`.

Note that as a matter of implementation this extends `NeedsKind` (which provides a proof of
well-formedness of the underlying `VisibilityKind`) instead of `Import` (but provides exactly the
same data), and may be converted to and from an `Import` via `.toImport` and `.ofImport`. -/
structure WorkspaceModel.ImportWithIdx extends kind : NeedsKind where
  /-- The `Name` of the imported module. -/
  module : Name
  /-- The module index of the import in an ambient `WorkspaceModel`. -/
  modIdx : ModIdx
deriving BEq, Inhabited, Repr

/-- Bundle an `Import` and its module index in an ambient `WorkspaceModel` together in an
`ImportWithIdx`.  -/
def WorkspaceModel.ImportWithIdx.ofImport (imp : Import) (modIdx : ModIdx) :
    WorkspaceModel.ImportWithIdx where
  kind := .ofImport imp
  module := imp.module
  modIdx

@[inline] def WorkspaceModel.ImportWithIdx.toImport (i : ImportWithIdx) : Import :=
  { i with }

/-- Bundle an `Import` together with its module index in the given `WorkspaceModel`, if it can be
found. -/
@[inline] def WorkspaceModel.importWithIdx? (i : Import) (w : WorkspaceModel)
    : Option ImportWithIdx :=
  w.getModIdx? i.module |>.map (.ofImport i)

/-- Converts syntax for an import into a `NeedsKind`, `Name`, and `ModIdx` in the given workspace
model representing that import, throwing if the module name cannot be found. -/
def WorkspaceModel.importWithIdxOfSyntax (w : WorkspaceModel) :
    TSyntax ``Parser.Module.import → CommandElabM WorkspaceModel.ImportWithIdx
  | `(Parser.Module.import|
      $[public%$pubTk]? $[meta%$metaTk]? import $[all%$allTk]? $mod) => do
    if h : pubTk.isSome && allTk.isSome then
      let ref := mkNullNode #[pubTk.get (by grind), allTk.get (by grind)]
      throwErrorAt ref m!"Cannot use `public` with `all`."
    else
      if mod.getId.isAnonymous then
        throwUnsupportedSyntax
      let some modIdx := w.getModIdx? mod.getId
        | throwErrorAt mod "Could not find module `{mod}` in the workspace."
      let kind : NeedsKind :=
        { isExported := pubTk.isSome, isMeta := metaTk.isSome, isAll := allTk.isSome }
      pure { kind, module := mod.getId, modIdx }
  | _ => throwUnsupportedSyntax

open Diff

/-- Various transitions between two import states. -/
structure ImportTransition where
  /-- The initial and final direct imports being compared. -/
  directImportsTransition : Transition (Array WorkspaceModel.ImportWithIdx)
  /-- The initial and final sets of all modules imported (at any phase and visibility). -/
  importedTransition : Transition ModuleBitset
  /-- The initial and final transitive dependencies (module-system aware). -/
  providesTransition : Transition Provides
  /-- The initial and final modules prior to the importing module (ignoring module system
  annotations). -/
  prevsTransition    : Transition ModuleBitset
  /-- The initial and final sets of modules whose IR is loaded. (Note that `meta` import
  annotations cause IR to be loaded from all modules prior, ignoring module system annotations.)
  This is only accurate for `lake build`, as the server loads extra IR. -/
  irLoadedTransition : Transition ModuleBitset
  /-- The initial and final depths of the build graph. -/
  depthTransition    : Transition Nat
  /- TODO: prevsCurrentPackage, depthCurrentPackage? Currently `prevs` and `depth` also include
  upstream packages. -/

/-- A `Venn` diagram of what was added, lost, and shared between the initial and final import
states. Derived from `ImportTransition`, but also removes the
`public import Init / public meta import Init` implicitly added to every non-prelude file from the
`shared` field (and thus requires that information from the `WorkspaceModel`). -/
structure ImportVenn where
  /-- Venn diagram comparing the initial and final full sets of imported modules. Drops the imports
  provided by the implicit init import (even if dragged in by other imports). -/
  importedVenn : Venn ModuleBitset
  /-- Venn diagram comparing the initial and final transitive dependencies (module-system aware).
  Drops the transitive dependencies provided by the implicit `Init` import. -/
  providesVenn : Venn Provides
  /-- Venn diagram comparing the initial and final prior modules, ignoring module system
  annotations. Drops the modules provided by the implicit `Init` import. -/
  prevsVenn    : Venn ModuleBitset
  /-- Venn diagram comparing the initial and final loaded IR. Note that `meta` annotations load IR
  for every module prior to them, ignoring module system annotations. Drops the IR provided by the
  implicit `Init` import. -/
  irLoadedVenn : Venn ModuleBitset

/-- Captures statistics on the initial and final states (`.transitions`) as well as derived
information on what was added and lost. -/
structure ImportDiff extends transitions : ImportTransition, venns : ImportVenn

/-- Given direct imports in `ImportWithIdx`s, compute their transitive dependencies (what they
provide to the importing module). -/
def WorkspaceModel.providesOfImportWithIdxs (w : WorkspaceModel) (mods : Array ImportWithIdx)
    : Provides :=
  let needs := mods.foldl (init := w.initNeeds) fun needs { kind, modIdx .. } =>
    needs.union kind {modIdx}
  w⟦needs.linearize⟧

/-- Given direct imports in `ImportWithIdx`s, compute all modules prior to the module importing
them. Includes `Init` by default. -/
def WorkspaceModel.prevsOfImportWithIdxs (w : WorkspaceModel) (mods : Array ImportWithIdx)
    : ModuleBitset :=
  mods.foldl (init := w.initPrevs) fun acc i => acc ∪ w.mods[i.modIdx]!.prevs ∪ {i.modIdx}

/-- Given direct imports in `ImportWithIdx`s, compute the depth of the build graph of the importing
module. -/
def WorkspaceModel.depthOfImportWithIdxs (w : WorkspaceModel) (mods : Array ImportWithIdx) : Nat :=
  (mods.map (fun i => w.mods[i.modIdx]!.depth) |>.max?.getD <|
    -- Fall back to the depth of `Init` if no imports. Somewhat awkward to compute the depth
    -- without `Init` in the first place, though that would be preferable.
    w.getInitModIdx?.elim 0 (w.mods[·]!.depth)) + 1

/-- Given a `WorkspaceModel` and a transition between an initial and final sets of *direct*
imports, compute the transitions and Venn diagrams in `ImportDiff`. -/
def WorkspaceModel.toImportDiff (w : WorkspaceModel)
    (mods : Transition (Array WorkspaceModel.ImportWithIdx)) : ImportDiff :=
  let providesTransition := mods.map w.providesOfImportWithIdxs
  let prevsTransition    := mods.map w.prevsOfImportWithIdxs
  let irLoadedTransition       := providesTransition.map w.irNeeds
  let importedTransition := providesTransition.map (·.flatten)
  let initProvides := w.initProvides
  {
    directImportsTransition := mods
    importedTransition, providesTransition, prevsTransition, irLoadedTransition
    depthTransition := mods.map w.depthOfImportWithIdxs
    importedVenn := importedTransition.toVenn.ignoreShared initProvides.flatten
    providesVenn := providesTransition.toVenn.ignoreShared initProvides
    prevsVenn    := prevsTransition.toVenn.ignoreShared w.initPrevs
    irLoadedVenn := irLoadedTransition.toVenn.ignoreShared <| w.irNeeds initProvides
  }

/--
Renders an `ImportVenn` as a table.
-/
def ImportVenn.toTable (diff : ImportVenn) : Array (Table.Row 5) := Id.run do
  let basicNeedsKinds := #[NeedsKind.pub, .priv, .metaPub, .metaPriv]
  let privOfPrivNeedsKinds := #[NeedsKind.privOfPriv, .metaPrivOfPriv]
  let mut rows : Array (Table.Row 5) := #[
    Venn.header,
    "modules imported" |> diff.importedVenn.measure.toRow,
    Table.emptyRow]
  -- Add rows for each basic `NeedsKind` always, but only add `all`'s if nontrivial
  rows := rows ++ basicNeedsKinds.map fun k => s!"{k}" |> (diff.providesVenn.measureNeedsAt k).toRow
  if privOfPrivNeedsKinds.any fun k => diff.providesVenn.map (·.get k) |>.isChanged then
    rows := rows ++ privOfPrivNeedsKinds.map fun k =>
      -- display as `import all` instead of `all` to avoid ambiguity
      k.toImportString |> (diff.providesVenn.measureNeedsAt k).toRow
  rows := rows ++ #[
    Table.emptyRow,
    "previous modules" |> diff.prevsVenn.measure.toRow,
    "ir loaded" |> diff.irLoadedVenn.measure.toRow]
  return rows

/--
Renders an `ImportDiff` as a table. This is the table for the `ImportVenn` part plus the build
depth transition reported below. Other transitions are not reported.
-/
def ImportDiff.toTable (diff : ImportDiff) : Array (Table.Row 5) :=
  diff.venns.toTable ++ #[
    Table.emptyRow,
    Transition.header.rightPad,
    "build depth" |> diff.depthTransition.toRow |>.rightPad]

open ImportGraph Lean Meta Tactic.TryThis WorkspaceModel in
elab_rules : command
| `(#import_diff%$tk $spec) => do
  unless (← getEnv).header.isModule do
    logWarning m!"`#import_diff` with `+`/`-` or `from`/`to` only works in the module system.\n\n\
      Please use `#import_diff mod₁ mod₂ mod₃ ...`, or modulize this file."
    return
  -- Get the modules mentioned in the command and make sure we try to include them in the workspace
  -- model (in case they are e.g. new files not in the root)
  let modOfImportStx? : TSyntax ``Parser.Module.import → Option Name
    | `(Parser.Module.import| $[public]? $[meta]? import $[all]? $mod) =>
      let mod := mod.getId; if mod.isAnonymous then none else some mod
    | _ => none
  let modNames := match spec with
    | `(diffCurrent| $items:diffCurrentItem*) => items.getElems.filterMap fun
      | `(diffCurrentItem| +$imp)
      | `(diffCurrentItem| -$imp) => modOfImportStx? imp
      | _ => none
    | `(diffAbsolute| $[from $mods₀*]? to $mods₁*) =>
      mods₁.getElems.filterMap modOfImportStx?
        ++ mods₀.elim #[] (·.getElems.filterMap modOfImportStx?)
    | _ => #[]
  let w ← getWorkspaceModel (extraMods := modNames.push <|← getMainModule)
  -- Filter out errors about modules in `modNames`. It's nicer to report those
  -- later, with a correct `ref`, saying we couldn't find them in the workspace.
  let errors := w.errors.filter fun
    | .readImportsFailure mod ..
    | .noLibOfModule mod => !modNames.contains mod
  unless errors.isEmpty do
    throwErrorAt tk m!"Errors when building the import hierarchy:\n\n\
      {m!"\n\n".joinSep (errors.map toMessageData |>.toList)}"
  -- Imports of the current file:
  let currentImportWithIdxs : CommandElabM (Array ImportWithIdx) := do
    (← getEnv).header.imports.filterMapM fun imp => do
      let some impWithIdx := w.importWithIdx? imp
        | throwError "Could not find already-imported module `{imp.module}` in the workspace."
      pure impWithIdx
  -- Transition between the initial and final import states we want to diff:
  let modsTransition : Transition (Array ImportWithIdx) ← match spec with
    | `(diffAbsolute| $[from $initMods*]? to $finalMods*) =>
      let initial ← if let some initMods := initMods then
          initMods.getElems.mapM w.importWithIdxOfSyntax
        else currentImportWithIdxs
      let final ← finalMods.getElems.mapM w.importWithIdxOfSyntax
      pure { initial, final }
    | `(diffCurrent| $items:diffCurrentItem*) =>
      let initial ← currentImportWithIdxs
      let mut removed := #[]
      let mut added := #[]
      for item in items.getElems do
        match item with
        | `(diffCurrentItem| +$imp:import) => do
          let info ← w.importWithIdxOfSyntax imp
          if added.contains info then
            throwErrorAt item "`{imp}` has already been added to the import set."
          if initial.contains info then
            throwErrorAt item "The current file's imports already include `{imp}`."
          if removed.contains info then
            throwErrorAt item "`{imp}` has also been removed from the import set."
          added := added.push info
        | `(diffCurrentItem| -$imp:import) => do
          let info ← w.importWithIdxOfSyntax imp
          if removed.contains info then
            throwErrorAt item "`{imp}` has already been removed from the import set."
          unless initial.contains info do
            throwErrorAt item "The current file's imports do not include `{imp}`."
          if added.contains info then
            throwErrorAt item "`{imp}` has also been added to the import set."
          removed := removed.push info
        | `(diffCurrentItem| -%$dangling) =>
          let removable := (← getEnv).header.imports.filter fun imp =>
            !((`Init).isPrefixOf imp.module)
              && !removed.any (imp == ·.toImport)
          if removable.isEmpty then
            logWarningAt dangling m!"No imports can be removed."
          else
            let suggestions : Array Suggestion := removable.map (s!"-{·}")
            liftCoreM <| addSuggestions dangling suggestions
              (header := "The following imports in this file may be deleted:")
              (codeActionPrefix? := "")
              (diffGranularity := .char)
          return
        | _ => throwUnsupportedSyntax
      let final := initial.filter (!removed.contains ·) ++ added
      pure { initial, final }
    | _ => throwUnsupportedSyntax
  let diff := w.toImportDiff modsTransition

  -- Message niceties:
  let header := match diff.importedVenn.compare? with
    | some .gt => "Imports strictly increased.\n\n"
    | some .lt => "Imports strictly decreased.\n\n"
    | some .eq =>
      match diff.providesVenn.compare? with
      | some .gt => "No changes to imported modules, \
        but the availability of some modules increased.\n\n"
      | some .lt => "No changes to imported modules, \
        but the availability of some modules decreased.\n\n"
      | some .eq => "No changes to imports.\n\n"
      | none => "No changes to imported modules, \
        but the availability of some modules changed.\n\n"
    | none => ""
  -- collapsible legend
  let columnDescrs := [
    ("Δ", "net change from initial to final"),
    ("+", "number of modules in the final state not in the initial state"),
    ("-", "number of modules in the initial state not in the final state"),
    ("∩", "number of modules shared by the initial and final state. \
      (This ignores the implicit `Init` imports that are secretly inserted by Lean.)"),
  ]
  let rowDescrs := [
    ("modules imported", "the total number of modules that are actually \
      imported (in a module-system aware fashion)"),
    ("public/private/...", "the number of modules imported at the given \
      phase and visibility. Note that any module present at public visibility is \
      necessarily also present at private visibility."),
    ("previous modules", "the total number of modules prior to the \
      current module in the build graph, ignoring module system annotations."),
    ("ir loaded", "the IR loaded by Lean given these imports. (`meta` imports \
      ignore module system annotations, and load the IR of all prior modules.)"),
    ("build depth", "the total depth of the build graph, ignoring module system annotations. \
      Counts the importing module. Unlike other statistics, this does include `Init`."),
  ]
  let renderDescrs (msgs : List (String × String)) : MessageData :=
    indentD <| .bulletList <| msgs.map fun (item, content) => m!"{item}: {.nestD content}"
  let legend ← liftCoreM <| Widget.collapsible (summary := "Legend") m!"\
    Columns:{renderDescrs columnDescrs}\
    \n\n\
    Rows:{renderDescrs rowDescrs}"
  -- copyable reduced initial/final imports
  let importsMsg (header : MessageData) (p : Provides)
      : CoreM MessageData := do
    let imports := Import.pretty <| w.toRawImports <| p.reduce w
    let copyIcon ← Widget.copyToClipboard s!"{imports}"
    Widget.collapsible m!"{header} {copyIcon}" m!"{imports}"
  -- Helpful message upon writing just `#import_diff`
  let noArgsMsg :=
    if let `(diffCurrent|) := spec then
      m!"Try adding and removing imports from the current imports with `+`/`-`, or comparing \
        import blocks with `#import_diff (from ...)? to ...`.\n\n"
    else m!""
  logInfo m!"\
    {noArgsMsg}\
    {header}\
    {Table.formatSimple diff.toTable}\
    \n\n{legend}\
    {← liftCoreM <| importsMsg "Initial imports (reduced)" diff.providesTransition.initial}\
    {← liftCoreM <| importsMsg "Final imports (reduced)" diff.providesTransition.final}"

/-! ## Non-module `#import_diff` -/

/-- `#import_diff foo bar ...` computes the new transitive imports that are added to a given file when
modules `foo, bar, ...` are added to the set of imports of the file. More precisely, it computes the
import diff between when `foo, bar, ...` are added to the imports and when `foo, bar, ...` are removed
from the imports.

Note: the command also works when some of the modules passed as arguments are already present in the file's
imports. -/
elab (name := oldImportDiff) "#import_diff" n:ident* : command => do
  if (← getEnv).header.isModule then
    let new ← `(command| #import_diff $[+import $n]*)
    let msg ← liftCoreM <| Meta.Hint.mkSuggestionsMessage
        #[{ suggestion := new, diffGranularity := .word }] (← getRef)
        (codeActionPrefix? := none) (forceList := false)
    logWarning m!"`#import_diff` in the module system uses `+`/`-` or `from`/`to` syntax:\n{msg}"
    return
  let name_arr : Array Name := n.map (·.getId)
  let sp ← searchPathRef.get
  -- First, make sure the files exist.
  for name in name_arr do
    if (← sp.findWithExt "olean" name).isSome then continue
    throwError m!"File {name} cannot be found."
  let env ← getEnv
  -- Next, check for redundancies:
  let current_all_imports := env.allImportedModuleNames
  let redundancies := name_arr.filter current_all_imports.contains
  unless redundancies.isEmpty do
    let out := "\n".intercalate <| redundancies.map Name.toString |>.qsort (· < ·) |>.toList
    Lean.logInfo <| m!"The following are already imported (possibly transitively):\n{out}"
  -- Now compute the import diffs.
  let current_imports := env.imports
  let reduced_imports := env.imports.filter (!name_arr.contains ·.module)
  let extended_imports := current_imports ++ (name_arr.map ({ module := · }))
  let reduced_all_imports := (← Lean.importModules reduced_imports {}).allImportedModuleNames
  let extended_all_imports := (← Lean.importModules extended_imports {}).allImportedModuleNames
  let import_diff := extended_all_imports.filter (· ∉ reduced_all_imports)
  let out := "\n".intercalate <| import_diff.map Name.toString |>.qsort (· < ·) |>.toList
  Lean.logInfo s!"Found {import_diff.size} additional imports:\n{out}"
