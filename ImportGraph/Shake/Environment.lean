module

public import ImportGraph.Shake.Algebra
public import ImportGraph.Shake.DeclNeeds

import ImportGraph.Shake.Algebra
import ImportGraph.Lean.Environment

/-!
# Import hierarchy from the `Environment`

This file takes an orthogonal approach to interacting with an import hierarchy than
`WorkspaceModel`: namely, it builds a similar shake-style import hierarchy by looking at the
imports in the `Environment`.

However, this is (1) insufficient in general (2) tricky, since there are divergences between which
imports are loaded in the language server vs. with `lake build`. Likewise, imports of imports are
not guaranteed to have data associated with them.

Yet, it is still useful for small commands which only need to inspect a local view of imports,
and do not need to cross module system barriers in the import hierarchy under `lake build` (in
which case higher modules will not be loaded, e.g. if they are privately imported). Interacting
with the environment is faster than constructing a workspace model, so this is preferable in the
simple cases it can work for.

It is therefore the case that this API should **never** be used while using `WorkspaceModel`s. The
`ModIdx` of a `WorkspaceModel` out of which we build a `Needs` has no connection to the `ModuleIdx`
in an `Environment` out of which we'd build the same `Needs`.
-/

open Lean ImportGraph Shake Lean

namespace ImportGraph.Shake.Lean.Environment

public section

/-- Computes the transitive closure of a set of imports with respect to an import hierarchy
`transDeps`, as far as the environment allows. -/
def transitiveClosureOf (env : Environment)
    (imps : Array Import) (transDeps : ArrayHierarchy) (base : Provides := .empty) : Provides :=
  imps.foldl (init := base) fun needs imp =>
    needs ∪ transDeps⟦(id (α := Nat) (env.getModuleIdx! imp.module), imp)⟧

/-- The current transitive imports as they are provided to the current environment. -/
@[inline] def currentTransNeeds (env : Environment)
    (transDeps : ArrayHierarchy) (excluding : NameSet := {}) : Provides :=
  env.transitiveClosureOf (env.header.imports.filter (!excluding.contains ·.module)) transDeps

/-- Creates an `Array Needs` of transitive dependencies among modules present in the environment.
Assumes that modules in the environment are topologically sorted.

**Caution:** Lean imports more modules when in the language server than during a typical
`lake build`. As such, this should *only* be used in cases where `Needs` information for the
modules guaranteed to be present in the environment during build is sufficient, or else behavior
should be gated on the value of the option `Elab.inServer`. -/
partial def mkTransDeps (env : Environment) : ArrayHierarchy := Id.run do
  let mut transDeps := Array.mkEmpty env.header.moduleData.size
  for h : i in 0...env.header.moduleData.size do
    let mod := env.header.moduleData[i]
    let mut transImps := Needs.reflOf i
    for imp in mod.imports do
      -- As per the module system, not every import-of-an-import is also imported.
      let some j := env.getModuleIdx? imp.module | continue
      let some transDepsj := transDeps[j]?
        -- We expect a topological order. Break if Lean breaks this.
        | panic! "Nontopological order encountered:\n\
            `{imp.module}` is imported by `{env.header.modules[i]!.module}`, \
            but comes afterwards in the environment"
          continue
      transImps := transImps ∪ (transDepsj ≫ imp)
    transDeps := transDeps.push transImps.linearize
  return transDeps

/-- Assuming that the indices in `Needs` correspond to module indices **in the provided
environment**, record an `Import` for each set index in `Needs` in some order. Note that this
should **not** be used in tandem with a `WorkspaceModel`, which uses different indices for modules
than those used in the environment. -/
def toRawImports (env : Environment)
    (n : Needs) (skipInit := true) : Array Import := Id.run do
  let mut out := #[]
  for (k, i) in n.highToLow do
    let some { module .. } := env.header.modules[i]?
      | panic! s!"Could not find module at index `{i}`"; continue
    if skipInit && (`Init).isPrefixOf module then continue
    out := out.push { k with module, importAll := k.isAll }
  return out

/-- Like `DeclNeeds.toSimultaneousImportNeeds`, but uses the environment's notion of `ModuleIdx`
instead of a workspace model's. -/
def toSimultaneousImportNeeds (env : Environment)
    (declNeeds : DeclNeeds) (declImportNeeds : ImportNeeds := {}) : StanceM ImportNeeds := do
  let mut declImportNeeds := declImportNeeds
  for (decl, declNeeds) in declNeeds do
    withTraceNode `ImportGraph.Shake
      (fun _ => return m!"`{.ofConstName decl}`") do←
    let some stance ← getStance? decl | continue
    for (modName, usedDecls) in declNeeds.fixedDecls do
      withTraceNode `ImportGraph.Shake (collapsed := false)
        (fun _ => return m!"Uses module `{modName}`") do←
      let some modIdx := env.getModuleIdx? modName | continue
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
              let some modIdx := env.getModuleIdx? modName | continue
              trace[ImportGraph.Shake] "indirect usage of `{modName}`"
              declImportNeeds := declImportNeeds.union
                { isExported := false, isMeta := false, allowMeta := true } {modIdx}
          let k := stance.toImportNeedsKind k usedStance
          declImportNeeds := declImportNeeds.union k {modIdx}
  return declImportNeeds
