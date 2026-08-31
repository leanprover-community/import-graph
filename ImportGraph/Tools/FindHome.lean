/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau, Thomas R. Murrills
-/
module

public meta import ImportGraph.Imports.Pretty
public meta import ImportGraph.Lean.MessageData
public meta import ImportGraph.Shake.Workspace
public meta import ImportGraph.Imports.ImportGraph -- for old `#find_home`
public meta import ImportGraph.Imports.RequiredModules -- for old `#find_home`
public meta import ImportGraph.Graph.TransitiveClosure -- for old `#find_home`
public import ImportGraph.Widget.Collapsible
public import ImportGraph.Widget.Copy
public import ImportGraph.Widget.GoToModule

/-!
# `#find_home`

This module provides the `#find_home for <cmd>` utility, which suggests places a given command (and its dependencies from the current file) can live in the module system.

## Future work

### UI

- Provide more information about the extracted dependencies of the given commands and declarations
  (e.g. *why* certain imports are necessary, what role certain declarations play). This information
  is available internally and needs only to be rendered helpfully.
- Provide more ranking options by default, and broadly improve the UI to be more informative (may
  involve moving off of `MessageData` to HTML)
- In the other direction, provide an "agent mode" that emits simplified textual information
- Provide more visibility into the import hierarchy. This may also be in the remit of related UX
  instead of `#find_home` per se.

### Functionality

- Handle non-modules
- Provide a simplified meta API for accessing `#find_home`'s composed functionality.
- Allow finding homes for commands which do not produce declarations
- Provide better support for moving declarations with same-file dependencies:
  - Capture the syntax needs of dependencies, e.g. via a stateful linter
  - Allow copying all of the dependencies at once
- Capture and copy over scopes/namespaces.
- Optionally leave behind `relocated ... to ...` commands
- Handle meta definitions.
- Allow for "mutation": find near-misses, where slight alterations to (1) the import hierarchy or
  (2) aspects of the current commands might allow other "homes" to be found.
- Allow for configurable queries, which could express e.g. e.g. "only consider modules which don't
  import <module A>" or "only consider modules downstream/upstream of <certain set of modules>" or
  "minimize the (nonzero) amount of category theory imported"
  - Handle configurable export preservation (e.g. "the highest place which provides this to
    <module>")
-/

meta section

open ImportGraph Shake Widget Lean Elab Command

namespace ImportGraph.Shake

/-- A warning to display if any declarations are `meta`, since `meta` declarations are not handled
properly yet. -/
private def DeclNeeds.metaWarning (env : Environment) (declNeeds : DeclNeeds) (cmd : String) :
    Option MessageData := do
  let metas := declNeeds.keysArray.filter (isMarkedMeta env) |>.map MessageData.ofConstName
  guard !metas.isEmpty
  return m!"Warning: Some declarations are marked meta. `{cmd}` does not yet handle meta IR; \
    the following is an approximation. Specifically:\n{.bulletList metas.toList}"

/--
⚠️ `#find_home` is currently experimental. Please report any wish-list features, possible ergonomic
improvements, or errors on GitHub or Zulip.

---

`#find_home for <cmd>` finds the highest modules in the import hierarchy in which `<cmd>` (and the
declarations produced during it) can live. This accounts for the syntax, constants, and executable
code produced during `<cmd>`, and respects the module system.

This includes any declarations which are dependencies of `<cmd>` from the current file, which
should be moved along with it. (Currently, `#find_home` does not account for the syntax of those
dependencies, nor does it suggest moving such dependencies individually.)

Note that `#find_home` may take a long time on its first run. It caches data about the module
hierarchy both in the `.lake` folder and interactively to make subsequent runs faster.

### Known limitations

- `#find_home` does not yet handle `meta` definitions.
- `#find_home` does not yet account for the syntax of dependent definitions.
- `#find_home` may not function correctly outside of the module system.
- For smaller miscellaneous limitations, see the module docstring.
-/
syntax (name := findHomeStx) "#find_home" ppSpace &"for" ppLine colGe command : command

elab_rules : command
| `(findHomeStx| #find_home%$tk for $cmd:command) => do
  let w ← getWorkspaceModel #[← getMainModule]
  if w.hasErrors then
    logErrorAt tk m!"Errors when building the import hierarchy:\n\n\
      {m!"\n\n".joinSep (w.errors.map toMessageData |>.toList)}"

  -- Elaborate command and capture new decls and decl needs
  let (declNeeds, newDecls) ← withElabCommandCapturingNeeds cmd
  if ← MonadLog.hasErrors then -- Also stop if the command produced errors
    return
  unless (← getEnv).header.isModule do
    logWarningAt tk "`#find_home` may not function correctly outside of the module system. \
      This may be addressed in the future."
  if newDecls.isEmpty then
    -- TODO: remove, allow finding homes for commands like `attribute`
    logWarningAt tk m!"This command did not produce any declarations."
    return
  if let some warning := declNeeds.metaWarning (← getEnv) "#find_home" then
    logWarningAt tk warning
  -- TODO: better handling of recursive import needs
  let importNeeds ← liftCoreM <| declNeeds.toSimultaneousImportNeeds w |>.run'
  -- logInfo m!"{← liftCoreM <| needs.toWidget env}"
  -- Previously defined declarations from the same file (excluding autogenerated declarations).
  let priorDecls := declNeeds.keysArray.filter fun decl =>
    !newDecls.contains decl && !(declNeeds.get! decl |>.isAutoDecl)

  -- Locate current module in hierarchy
  let some currentModIdx := w.getModIdx? (← getMainModule)
    | logErrorAt tk m!"Could not find current module `{← getMainModule}` in workspace."
  let currentLibIdx := w.libIdxOfModIdx! currentModIdx
  let currentPkgIdx := w.pkgIdxOfModIdx! currentModIdx
  let currentPrevs := w.getMod! currentModIdx |>.prevs
  let currentTransDeps := w.getMod! currentModIdx |>.transDeps

  -- Turn an array of module indices into go-to-def links that land after all needs in `declNeeds`
  let mkModLinks (mods : Array ModIdx) : CommandElabM (Array MessageData) := liftCoreM do
    let mut links := #[]
    for modIdx in mods do
      let modName := w.getMod! modIdx |>.name
      let mut decls : NameSet := {}
      for (_, need) in declNeeds do
        let some declsFromMod := need.fixedDecls[modName]? | continue
        decls := decls.insertMany declsFromMod.keysArray
      links := links.push <|← goToModuleOfDecls decls.toArray (fallbackModule := modName)
    return links

  -- Find minimal modules (the eponymous "homes")
  let minimals := importNeeds.providersByLib w
  -- TODO: currently this is a simple-minded check to see if it exists in the private scope.
  -- (Note that since `public` ⊆ `private`, this includes publicly-imported modules.)
  -- In the future we want to check the stance is preserved, allow meta if relevant, and so on.
  let minimalsProvidedHere := importNeeds.providersByLib w (league := currentTransDeps.get .priv)
  let otherSameLib :=
    minimals[currentLibIdx]?.getD #[] |>.filter (· != currentModIdx)
  let (aboveSameLib, adjSameLib) := otherSameLib.partition currentPrevs.has
  let providedHereSameLib :=
    minimalsProvidedHere[currentLibIdx]?.getD #[] |>.filter fun modIdx =>
      modIdx != currentModIdx && !(aboveSameLib.contains modIdx)

  -- Construct final `MessageData`
  let mut msgs := #[]
  -- Note that `providedHereSameLib` is disjoint from both `aboveSameLib` and `adjSameLib`.
  -- Note that `aboveSameLib.isEmpty` implies `providedHereSameLib` is empty.
  if aboveSameLib.isEmpty then
    msgs := msgs.push m!"In this library, this command \
      {if priorDecls.isEmpty then "is " else "and its dependencies from this file are "}\
      as high in the import hierarchy as {if priorDecls.isEmpty then "it" else "they"} can be\
      {if adjSameLib.isEmpty then "" else " above the current module"}."
  else
    unless aboveSameLib.isEmpty do
      let modLinks ← mkModLinks aboveSameLib
      let modLinks := modLinks.zipWith (fun msg isProvidedHere => if isProvidedHere then
        msg else m!"{msg} (not imported here)") (aboveSameLib.map (currentTransDeps.get .priv).has)
      msgs := msgs.push <|
        m!"This command {if priorDecls.isEmpty then "" else "and its dependencies "}\
          can be moved to the following module\
          {if aboveSameLib.size = 1 then "" else "s"} above this module:\n\
          {.bulletList modLinks.toList}"
    unless providedHereSameLib.isEmpty do
      let modLinks ← mkModLinks providedHereSameLib
      msgs := msgs.push <|← liftCoreM <|
        collapsible m!"This command can also be moved to modules which are highest in the \
          hierarchy among modules currently imported in this file, but are not highest among all \
          modules."
          m!"{.bulletList modLinks.toList}"
  unless adjSameLib.isEmpty do
    let modLinks ← mkModLinks adjSameLib
    msgs := msgs.push <|← liftCoreM <|
      collapsible m!"{if aboveSameLib.isEmpty then
        "However, this command can be moved to" else "This command can also be moved to"} \
        files adjacent to the current module in the import hierarchy."
        m!"{.bulletList modLinks.toList}"
  if aboveSameLib.isEmpty && adjSameLib.isEmpty then
    msgs := msgs.push <|
      m!"`#find_home` attempted to move the following new declaration\
        {if newDecls.size = 1 then "" else "s"}:\n\
      {.bulletList (newDecls.toList.map .ofConstName)}\
      {if priorDecls.isEmpty then m!"" else
        m!"\n\
          as well as the following existing declaration{if priorDecls.size = 1 then "" else "s"} \
          in this file, on which {if newDecls.size = 1 then "it depends" else "they depend"}:\n\
          {.bulletList (priorDecls.toList.map .ofConstName)}"}"
  let upstreams := minimals.filter fun libIdx _ => libIdx != currentLibIdx &&
  -- TODO: we assume all unequal packages are upstream. This is not necessarily the case.
    (w.pkgIdxOfLibIdx! libIdx != currentPkgIdx)
  unless upstreams.isEmpty do
    let mut upstreamTo := #[]
    for (libIdx, modIdxs) in upstreams do
      upstreamTo := upstreamTo.push <|← liftCoreM <|
        collapsible m!"To `{w.getLib! libIdx |>.name}` in `{w.pkgOfLibIdx! libIdx |>.origName}`:"
          m!"{.bulletList <| (← mkModLinks modIdxs).toList}"
    msgs := msgs.push <|← liftCoreM <|
      collapsible m!"Note: this command does not depend on the current package, \
        and may be upstreamed."
        m!"{m!"".joinSep upstreamTo.toList}"
  -- "More information" message:
  let reducedImps := w.toRawImports <| importNeeds.toNeeds.reduce w
  let moreInfo ← liftCoreM do
    -- TODO: more information from declNeeds.
    let minImports ← do
      if reducedImps.isEmpty then pure m!"This command does not require any imports." else
        let copyImports ← copyToClipboard s!"{Lean.Import.pretty reducedImps}" "[copy imports]"
        collapsible m!"Imports needed" m!"{copyImports}\n{Import.pretty reducedImps}"
    let producedConsts ← collapsible m!"New constants from this command"
      m!"{.bulletList (newDecls.toList.map MessageData.ofConstName)}"
    let priorDeclsMsg ← if priorDecls.isEmpty then pure m!"" else
      collapsible m!"Prior constants used in this command"
        m!"{.bulletList (priorDecls.toList.map MessageData.ofConstName)}"
    collapsible "More information" m!"{minImports}{producedConsts}{priorDeclsMsg}"
  let cmdRange := cmd.raw.getRangeWithTrailing?.get!
  let source := cmdRange.start.extract (← getFileMap).source cmdRange.stop |>.trimAscii
  -- TODO: copy over prior declarations as well
  let disclaimerComment := "-- NOTE: necessary scopes and namespaces may not have been copied over."
  let copySource ← liftCoreM do
    copyToClipboard s!"\n{disclaimerComment}\n{source}\n" (display :=
      .text s!"[copy source{if priorDecls.isEmpty then "" else " (without prior declarations)"}]")
  Lean.logInfo m!"{m!"\n".joinSep msgs.toList}\
    \n\n\
    {if priorDecls.isEmpty then m!"" else m!"Be sure to also move the following prior \
      declarations:\n\
      {.bulletList (priorDecls.toList.map MessageData.ofConstName)}\
      \n\n"}\
    {copySource}\n\n{moreInfo}"

/-!
## Non-module `#find_home`

This code is a fallback since `#find_home for` does not yet work outside the module system.
-/

/--
Warning: this declaration does not respect the module system, and should only be used outside of it.

Find locations as high as possible in the import hierarchy
where the named declaration could live.
-/
def Lean.Name.findHome (n : Name) (env : Option Environment) : CoreM NameSet := do
  let current? := match env with | some env => env.header.mainModule | _ => default
  let required := (← n.requiredModules).toArray.erase current?
  let imports := (← getEnv).importGraph.transitiveClosure
  let mut candidates : NameSet := {}
  for (n, i) in imports do
    if required.all fun r => n == r || i.contains r then
      candidates := candidates.insert n
  for c in candidates do
    for i in candidates do
      if imports.find? i |>.getD {} |>.contains c then
        candidates := candidates.erase i
  return candidates

/--
`#find_home <ident>` is in the process of being deprecated. Instead, use
```
#find_home for
<command>
```
where `<command>` declares `<ident>`. This ensures that the imports necessary for the syntax and
tactics used in the declaration are present too.

The following describes the functionality outside of the module system, which may not work:

Find locations as high as possible in the import hierarchy
where the named declaration could live.
Using `#find_home!` will forcefully remove the current file.
Note that this works best if used in a file with `import Mathlib`.

The current file could still be the only suggestion, even using `#find_home! lemma`.
The reason is that `#find_home!` scans the import graph below the current file,
selects all the files containing declarations appearing in `lemma`, excluding
the current file itself and looks for all least upper bounds of such files.

For a simple example, if `lemma` is in a file importing only `A.lean` and `B.lean` and
uses one lemma from each, then `#find_home! lemma` returns the current file.
-/
syntax (name := oldFindHomeStx) "#find_home" "!"? ident : command

elab_rules : command
| `(oldFindHomeStx| #find_home%$tk $[!%$bang]? $n:ident) => do
  if (← getEnv).header.isModule then
    throwError m!"`#find_home <ident>` does not work in the module system. Instead, use\n\
      ```\n\
      #find_home for\n\
      <command>\n\
      ```\n\
      where `<command>` declares `<ident>`. This ensures that the imports necessary for the syntax \
      and tactics used in the declaration are present too."
  else liftCoreM do
    unless n.raw.isMissing do
      let n ← realizeGlobalConstNoOverloadWithInfo n
      let env? ← bang.mapM fun _ => getEnv
      let homes ← (← n.findHome env?).toArray.mapM goToModule
      logInfoAt tk m!"{homes}"
