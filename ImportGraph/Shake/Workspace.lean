/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.Shake.Algebra
public import ImportGraph.Shake.DeclNeeds
public import ImportGraph.WorkspaceModel.Build
public import ImportGraph.WorkspaceModel.Model
public import Lean.Elab.Command

import ImportGraph.Shake.EnvExtension
import Std.Data.HashMap.AdditionalOperations

/-!
# `WorkspaceModel` and shake

This file defines interactions between a `WorkspaceModel` and shake. The import hierarchy and workspace model data allows us to turn needs into imports and  Notably, it provides

- `WorkspaceModel.toRawImports`, which converts the indices in a `Needs` into `Import`s (without)
  doing any reduction)
- `DeclNeeds.toSimultaneousImportNeeds`, which bakes the needs expressed in `DeclNeeds` into a
  single `ImportNeeds`
- `ImportNeeds.providersByLib`, which calculates (per library) which modules provide the given
  `ImportNeeds`
-/

public section

open ImportGraph Shake Lean

namespace ImportGraph

/-- Assuming that the indices in `Needs` correspond to module indices in the provided workspace
model, record an `Import` for each set index in `Needs` in some order. This does **not** remove
redundant imports. By default, this skips modules with prefix `Init`. -/
def WorkspaceModel.toRawImports (w : WorkspaceModel) (n : Needs) (skipInit := true) :
  Array Import := Id.run do
  let mut out := #[]
  for (k, i) in n.highToLow do
    let module := w.getMod! i |>.name
    if skipInit && (`Init).isPrefixOf module then continue
    out := out.push { k with module, importAll := k.isAll }
  return out

namespace Shake

-- TODO: make this take in a monadic interface for the module index, so that we can use it both for
-- an environment and more generally
/-- Collapse the `ImportNeeds` of all declarations in `DeclNeeds`. This is incorrect for splitting
up declarations, but adequate for moving them to a single place.  -/
def DeclNeeds.toSimultaneousImportNeeds
    (w : WorkspaceModel)
    (declNeeds : DeclNeeds)
    (declImportNeeds : ImportNeeds := {}) :
    StanceM ImportNeeds := do
  let mut declImportNeeds := declImportNeeds
  for (decl, declNeeds) in declNeeds do
    withTraceNode `ImportGraph.Shake
      (fun _ => return m!"`{.ofConstName decl}`") do←
    let some stance ← getStance? decl | continue
    for (modName, usedDecls) in declNeeds.fixedDecls do
      withTraceNode `ImportGraph.Shake (collapsed := false)
        (fun _ => return m!"Uses module `{modName}`") do←
      let some modIdx := w.idxOfMod[modName]? | continue
      for (usedDecl, ks) in usedDecls do
        withTraceNode `ImportGraph.Shake
          (fun _ => return m!"Uses decl `{.ofConstName usedDecl}`") do←
        let some usedStance ← getStance? usedDecl | continue
        let mut usedKs : DeclDeclNeedsKindSet := {}
        for k in ks do
          trace[ImportGraph.Shake] "{k.pretty}"
          if usedKs.contains k then continue
          usedKs := usedKs.insert k
          if let .comptime <| .indirect _ mods := k then
            for modName in mods do
              let some modIdx := w.idxOfMod[modName]? | continue
              trace[ImportGraph.Shake] "indirect usage of `{modName}`"
              declImportNeeds := declImportNeeds.union
                { isExported := false, isMeta := false, allowMeta := true } {modIdx}
          let k := stance.toImportNeedsKind k usedStance
          declImportNeeds := declImportNeeds.union k {modIdx}
  return declImportNeeds

/-- Computes a mapping `LibIdx → Array ModIdx` assigning to each library in the workspace model an
array of modules which provide `needs : ImportNeeds` and are minimal (by sets of previous modules).
Modules which provide strict supersets of other candidate modules are knocked out.

It then ranks the "winning" modules first by import depth
*among imports from the same library* (i.e. not counting imports of upstream libraries towards the
depth), then by number of previous modules, then alphabetically.

`league : ModuleBitset` specifies the modules among which the competition is conducted. If `none`,
this is all modules. -/
def ImportNeeds.providersByLib (w : WorkspaceModel) (needs : ImportNeeds)
    (league : Option ModuleBitset := none) :
    Std.HashMap LibIdx (Array ModIdx) := Id.run do
  let mut minimals : Std.HashMap LibIdx (Array (Option (ModIdx × ModuleBitset))) := {}
  for h : i in 0...(Hierarchy.size w) do
    if needs.isProvidedBy w[i] && league.elim true (·.has i) then
      let iLibPrevs := (w.getMod! i).prevs
      minimals := minimals.incorporateBelowAt (w.libIdxOfModIdx! i) (i, iLibPrevs)
        fun (_, iPrevs) (_, jPrevs) => iPrevs.lt jPrevs
  return minimals.map fun libIdx arr => (arr.reduceOption.qsort fun (i,pᵢ) (j,pⱼ) =>
    (compare (w.libDepth! i libIdx) (w.libDepth! j libIdx)) -- passing `libIdx` only for efficiency
      |>.then (compare pᵢ.size pⱼ.size)
      |>.then (Name.cmp (w.getMod! i).name (w.getMod! j).name) -- for stability if all else fails
      |>.isLT).map (·.1)

initialize registerTraceClass `ImportGraph.Shake
