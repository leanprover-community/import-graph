/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau, Thomas R. Murrills
-/
module

public meta import Lean.Elab.Command
public meta import Lean.Widget.UserWidget
public meta import ImportGraph.Imports.ImportGraph
public meta import ImportGraph.Imports.Pretty
public meta import ImportGraph.Lean.MessageData
public meta import ImportGraph.Shake.Workspace
public meta import ImportGraph.Util.TableMessage
public meta import Lean.Meta.Tactic.TryThis
public import ImportGraph.Widget.Collapsible
public import ImportGraph.Widget.Copy
public import ImportGraph.Widget.GoToModule

public meta section

open Lean

namespace ImportGraph

syntax diffAbsolute :=
  (&"from" ppLine sepByIndentSemicolon(Parser.Module.import))?
  &"to" ppLine sepByIndentSemicolon(Parser.Module.import)

/-- Removes an import among the current imports to this file. -/
syntax importMinus := &"-"
syntax importPlus := &"+"

syntax diffCurrentItem :=
  withPosition(importMinus (colGt Parser.Module.import)?)
    <|> withPosition(importPlus colGt Parser.Module.import)

syntax diffCurrent := sepByIndentSemicolon(diffCurrentItem)

syntax "#import_diff " (diffAbsolute <|> diffCurrent) : command

open Shake Elab Command

structure WorkspaceModel.ImportWithIdx extends kind : NeedsKind where
  module : Name
  modIdx : ModIdx
deriving BEq, Inhabited, Repr

def WorkspaceModel.ImportWithIdx.ofImport (imp : Import) (modIdx : ModIdx) :
    WorkspaceModel.ImportWithIdx where
  kind := .ofImport imp
  module := imp.module
  modIdx

@[inline] def WorkspaceModel.ImportWithIdx.toImport (i : ImportWithIdx) : Import :=
  { i with }

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

namespace ImportDiff

open ImportDiff

/-- If an integer is (strictly) greater than 0, display it with `+`. -/
def Int.toStringWithSign (i : Int) := if i > 0 then s!"+{i}" else s!"{i}"

/-- A simple structure with two states, `initial : α` and `final : α`, which models a transition. -/
structure Transition α where
  /-- The state before `final`. -/
  initial : α
  /-- The state after `initial`. -/
  final : α

namespace Transition

@[inline] def map (t : Transition α) (f : α → β) : Transition β where
  initial := f t.initial
  final := f t.final

end Transition

/-- A `Transistion` with an `initial : Nat` and a `final : Nat`. -/
abbrev TransitionMeasure := Transition Nat

namespace TransitionMeasure

@[inline] def delta (t : TransitionMeasure) : Int := (t.final : Int) - t.initial

def header : Table.Row 4 := #v["", "Δ", "new", "old"]

def toRow (t : TransitionMeasure) (rowHeader : String) : Table.Row 4 :=
  #v[rowHeader, t.delta.toStringWithSign, s!"{t.final}", s!"{t.initial}"]

end TransitionMeasure

/-- A Venn diagram characterizing the changes between some (implicit, unspecified) initial and
final state. -/
structure Venn α where
  /-- What the final state has gained that was not present in the initial state. -/
  plus   : α
  /-- What the initial state has lost that is no longer present in the final state. -/
  minus  : α
  /-- What the initial and final state have in common. -/
  shared : α

def Transition.toVennBy (t : Transition α) (sdiff : α → α → β) (inter : α → α → β) :
    Venn β where
  plus := sdiff t.final t.initial
  minus := sdiff t.initial t.final
  shared := (inter t.initial t.final)

namespace Venn

@[inline] def map (v : Venn α) (f : α → β) : Venn β where
  plus := f v.plus
  minus := f v.minus
  shared := f v.shared

def ofBitsetTransition (t : Transition Bitset) (ignoredShared : Bitset := ∅) : Venn Bitset :=
  t.toVennBy (sdiff := (· \ ·)) (inter := fun a b => (a ∩ b) \ ignoredShared)

def ofProvidesTransition (t : Transition Provides) (ignoredShared : Provides := ∅) :
    Venn Provides :=
  t.toVennBy
    (sdiff := (· \ ·))
    (inter := fun a b => (a.map₂ b (· ∩ ·)) \ ignoredShared |>.linearize)

/-- A `v : VennMeasure` represents no change iff `v.plus = v.minus = 0`. -/
@[inline] def isUnchanged (v : Venn α) (isZero : α → Bool) := isZero v.plus && isZero v.minus
/-- A `v : VennMeasure` represents a change of some kind iff `v.plus` is nonzero or `v.minus` is
nonzero. -/
@[inline] def isChanged (v : Venn α) (isZero : α → Bool) := !(v.isUnchanged isZero)
/-- Whether the `Venn` diagram implicitly represents a strict increase (`.gt`), a strict decrease
(`.lt`), no change (`.eq`), or incomparable final and initial states.  -/
@[inline] def compare? (v : Venn α) (isZero : α → Bool) : Option Ordering :=
  match isZero v.plus, isZero v.minus with
  | true, true => some .eq
  | true, false => some .lt
  | false, true => some .gt
  | false, false => none

end Venn

/-- Measurements of the parts of a Venn diagram comparing an (implicit, unspecified) initial state
and final state. -/
abbrev VennMeasure := Venn Nat

namespace VennMeasure

/-- The net difference between the implicit initial and final state, collapsing the independent
measurements of `plus` and `minus` together. -/
@[inline] def delta (v : VennMeasure) : Int := (v.plus : Int) - v.minus

open ImportGraph ImportDiff

/-- A header row for a table consisting of rows created by `VennMeasure.toRow`. -/
def header : Table.Row 5 := #v["", "Δ", "+", "-", "∩"]

/-- A row in a table for a single `VennMeasure` which follows the order in `VennMeasure.header`. -/
def toRow (rowHeader : String) (t : VennMeasure) : Table.Row 5 := #v[
  rowHeader,
  t.delta.toStringWithSign,
  s!"+{t.plus}",
  s!"-{t.minus}",
  s!"{t.shared}"]

-- kind of internal, delete?
def toTable (rows : Array <| Option (String × VennMeasure)) : Array (Table.Row 5) :=
  #[header] ++ rows.map fun
    | none => Table.emptyRow
    | some (rowHeader, v) => v.toRow rowHeader

/-- A `v : VennMeasure` represents no change iff `v.plus = v.minus = 0`. -/
@[inline] nonrec def isUnchanged (v : VennMeasure) := v.isUnchanged (· = 0)
/-- A `v : VennMeasure` represents a change of some kind iff `v.plus` is nonzero or `v.minus` is
nonzero. -/
@[inline] nonrec def isChanged (v : VennMeasure) := !v.isUnchanged
/-- Whether the `VennMeasure` represents -/
@[inline] nonrec def compare? (v : VennMeasure) : Option Ordering :=
  v.compare? (· = 0)

end VennMeasure

@[inline] def Venn.measureBitset (v : Venn Bitset) : VennMeasure := v.map (·.size)

def Venn.measureNeedsAt (k : NeedsKind) (needs : Venn Needs) : VennMeasure :=
  needs.map (·.get k |>.size)

end ImportDiff

open ImportDiff

structure ImportTransition where
  directImportsTransition : Transition (Array WorkspaceModel.ImportWithIdx)
  importedTransition : Transition ModuleBitset
  providesTransition : Transition Provides
  prevsTransition    : Transition ModuleBitset
  irLoadedTransition : Transition ModuleBitset
  depthTransition    : TransitionMeasure
  -- TODO: prevsCurrentPackage, depthCurrentPackage?

structure ImportVenn where
  importedVenn : Venn ModuleBitset
  providesVenn : Venn Provides
  prevsVenn    : Venn ModuleBitset
  irLoadedVenn : Venn ModuleBitset

structure ImportDiff extends transitions : ImportTransition, venns : ImportVenn

def WorkspaceModel.providesOfImportWithIdxs (w : WorkspaceModel) (mods : Array ImportWithIdx)
    : Provides :=
  let needs := mods.foldl (init := ∅) fun needs { kind, modIdx .. } =>
    needs.union kind {modIdx}
  w⟦w.implicitInitNeeds needs |>.linearize⟧

def WorkspaceModel.prevsOfImportWithIdxs (w : WorkspaceModel) (mods : Array ImportWithIdx)
    : ModuleBitset :=
  mods.foldl (init := ∅) fun acc i => acc ∪ w.mods[i.modIdx]!.prevs ∪ {i.modIdx}

def WorkspaceModel.depthOfImportWithIdxs (w : WorkspaceModel) (mods : Array ImportWithIdx) : Nat :=
  mods.map (fun i => w.mods[i.modIdx]!.depth) |>.max?.getD 0

def WorkspaceModel.toImportDiff (w : WorkspaceModel)
    (mods : Transition (Array WorkspaceModel.ImportWithIdx)) : ImportDiff :=
  let providesTransition := mods.map w.providesOfImportWithIdxs
  let prevsTransition    := mods.map w.prevsOfImportWithIdxs
  let irLoadedTransition       := providesTransition.map w.irNeeds
  let importedTransition := providesTransition.map (·.flatten)
  {
    directImportsTransition := mods
    importedTransition, providesTransition, prevsTransition, irLoadedTransition
    depthTransition := mods.map w.depthOfImportWithIdxs
    importedVenn := .ofBitsetTransition importedTransition
      (ignoredShared := w.initProvides.flatten)
    providesVenn := .ofProvidesTransition providesTransition
      (ignoredShared := w.initProvides)
    prevsVenn    := .ofBitsetTransition prevsTransition -- should we exclude init from build graph?
    irLoadedVenn := .ofBitsetTransition irLoadedTransition
      (ignoredShared := w.irNeeds w.initProvides)
  }

def ImportDiff.toTable (diff : ImportDiff) : Array (Table.Row 5) := Id.run do
  let basicNeedsKinds := #[NeedsKind.pub, .priv, .metaPub, .metaPriv]
  let privOfPrivNeedsKinds := #[NeedsKind.privOfPriv, .metaPrivOfPriv]
  let mut rows : Array (Table.Row 5) := #[
    VennMeasure.header,
    "modules imported" |> diff.importedVenn.measureBitset.toRow,
    Table.emptyRow]
  -- Add rows for each basic `NeedsKind` always, but only add `all`'s if nontrivial
  rows := rows ++ basicNeedsKinds.map fun k => s!"{k}" |> (diff.providesVenn.measureNeedsAt k).toRow
  if privOfPrivNeedsKinds.any (diff.providesVenn.measureNeedsAt · |>.isChanged) then
    rows := rows ++ privOfPrivNeedsKinds.map fun k =>
      -- display as `import all` instead of `all` to avoid ambiguity
      k.toImportString |> (diff.providesVenn.measureNeedsAt k).toRow
  rows := rows ++ #[
    Table.emptyRow,
    "previous modules" |> diff.prevsVenn.measureBitset.toRow,
    "ir loaded" |> diff.irLoadedVenn.measureBitset.toRow,
    Table.emptyRow,
    TransitionMeasure.header.rightPad,
    "build depth" |> diff.depthTransition.toRow |>.rightPad]
  return rows

open Meta.Tactic.TryThis WorkspaceModel in
elab_rules : command
| `(#import_diff%$tk $spec) => do
  let w ← getWorkspaceModel (extraMods := #[← getMainModule])
  if w.hasErrors then
    throwErrorAt tk m!"Errors when building the import hierarchy:\n\n\
      {m!"\n\n".joinSep (w.errors.map toMessageData |>.toList)}"
  let currentImportWithIdxs : CommandElabM (Array ImportWithIdx) := do
    (← getEnv).header.imports.filterMapM fun imp => do
      if (`Init).isPrefixOf imp.module then return none
      let some modIdx := w.getModIdx? imp.module
        | throwError "Could not find already-imported module `{imp.module}` in the workspace."
      pure (some <| .ofImport imp modIdx)
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
  let header := match diff.importedVenn.compare? (·.isEmpty) with
    | some .gt => s!"Imports strictly increased.\n\n"
    | some .lt => s!"Imports strictly decreased.\n\n"
    | some .eq => s!"No changes to imports.\n\n"
    | none => ""
  logInfo <| s!"{header}{Table.formatSimple diff.toTable}"
  -- TODO: show each import. Should exclude "obvious" user-specified imports, right? No, no, the entire point is to see the other ones. So we should show the user-specified ones but just annotate.
  -- TODO: should also allow removing imports that "can't" really be removed, and just say that this is virtual?
  -- TODO: With no arguments, show current stats in a nicer table? Maybe with totals?
  -- TODO: Hmm, show totals somehow anyway. Total public increase, total private increase (including public/private only?)⋯
  -- Equals is confusing
  -- table would be good

/-- `#import_diff foo bar ...` computes the new transitive imports that are added to a given file when
modules `foo, bar, ...` are added to the set of imports of the file. More precisely, it computes the
import diff between when `foo, bar, ...` are added to the imports and when `foo, bar, ...` are removed
from the imports.

Note: the command also works when some of the modules passed as arguments are already present in the file's
imports. -/
elab "#import_diff" n:ident* : command => do
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
