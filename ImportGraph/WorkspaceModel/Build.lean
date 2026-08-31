/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.WorkspaceModel.Model
public import ImportGraph.WorkspaceModel.Summary
public import ImportGraph.Shake.Algebra

import Lean.Elab.ParseImportsFast
import ImportGraph.Lake

/-!
# Building a `WorkspaceModel`

This file builds a `WorkspaceModel` (the import hierarchy and other intradependencies) from a
`WorkspaceSummary` (bare information about the lake workspace, such as library globs). This means
traversing the modules for all packages and libraries and parsing their imports, then recording
these relationships in the `WorkspaceModel`.

## Future work

## Functionality

- We could preserve `shake` annotations, as these are relevant to the hierarchy

## Performance

- We could probably parallelize the import source reading.
- We could possibly hybridize with reading oleans when available instead of re-parsing imports.
- We could cache the component of the model for upstream packages more persistently, since those
  dependency graphs are (probably) not going to change
- We could consider bundling this all into the exe, and see if it's actually more performant to
  just push it all over json
-/

open ImportGraph Lean System Lake Shake

public section

namespace ImportGraph.WorkspaceModel

instance : Hierarchy WorkspaceModel where
  size w := w.mods.size
  getDeps w i h := w.mods[i].transDeps
  getDeps? w i := w.mods[i]?.map (·.transDeps)
  getDeps! w i := w.mods[i]!.transDeps

-- TODO: deduce this from core's lakefile instead of hardcoding it, ideally during `Emit`
-- Somehow exclude "LakeMain", "LeanIR", "Leanc", "LeanChecker"? Or will these be ignored by not being above the current package?
/-- A hardcoded list of the four libraries we want to consider in the toolchain (`Init`, `Std`, `Lean`, `Lake`) with directories relative to the `sysroot`. -/
private def toolchainLibs (sysroot : FilePath) : Array Lake.LibrarySummary :=
  #[{ name := `Init, srcDir := sysroot / "src" / "lean" },
    { name := `Std, srcDir := sysroot / "src" / "lean" },
    { name := `Lean, srcDir := sysroot / "src" / "lean"
      globs := #[`Lean, `Lean.Compiler.IR.EmitLLVM, `Lean.Compiler.LCNF.Probing] },
    { name := `Lake, srcDir := sysroot / "src" / "lean" / "lake"
      globs := #[`Lake.*] }]

-- TODO: just alter `parseImports'` to account for this.
private def hasImplicitPrelude (header : Lean.ModuleHeader) : Bool :=
  header.imports[0]?.isEqSome { module := `Init } &&
  header.imports[1]?.isEqSome { module := `Init, isMeta := true }

/--
Collects the imports of the module (transitively) into the `WorkspaceModel`.

Note that this does not include all files local to a library (and so e.g. avoids including scratch
files), but does *allow* `mod` to be merely local to a library without e.g. appearing in its root
file, if we do call `collect` on such a `mod`. This mirrors `LeanLib.recCollectLocalModules`, and
brings in all transitively-imported library-local modules imported by `mod`.
-/
private partial def collect (mod : Name) (wm : WorkspaceModel) : IO WorkspaceModel := do
  if wm.idxOfMod.contains mod then return wm
  let some libIdx := wm.libIdxOfMod? mod | return { wm with
    errors := wm.errors.push <| .noLibOfModule mod }
  let srcFile := (wm.getLib! libIdx).srcPathOfMod mod
  let headerE ← observing do Lean.parseImports' (← IO.FS.readFile srcFile) srcFile.toString
  let header ← match headerE with
    | .ok header => pure header
    | .error err =>
      return { wm with errors := wm.errors.push <| .readImportsFailure mod srcFile err }
  let mut wm := wm
  for imp in header.imports do
    wm ← collect imp.module wm
  -- All local imports of `mod` are indexed now; gather the libraries they live in.
  let pkgIdx := wm.pkgIdxOfLibIdx! libIdx

  -- Module aggregates. TODO: consider consolidating?
  let modIdx := wm.mods.size -- the position of `modData` in `mods` below
  let mut transPkgDeps : PackageBitset := ∅
  let mut transLibDeps : LibraryBitset := ∅
  let mut transDeps := Needs.reflOf modIdx
  let mut prevs := ∅
  -- TODO: dynamically extend arrays?
  let mut depthsPerLib := Array.replicate wm.libs.size 0
  let mut depthsPerPkg := Array.replicate wm.packages.size 0
  for imp in header.imports do
    -- TODO: Potentially we ought to record an error if we can't find it here.
    if let some j := wm.idxOfMod[imp.module]? then
      let impModData := wm.getMod! j
      transLibDeps := transLibDeps ∪ impModData.transLibDeps ∪ {impModData.libIdx}
      transPkgDeps := transPkgDeps ∪ impModData.transPkgDeps ∪ {impModData.pkgIdx}
      transDeps := transDeps ∪ impModData.transDeps ≫ imp
      prevs := prevs ∪ impModData.prevs ∪ {j}
      depthsPerLib := depthsPerLib.zipWith max impModData.depthsPerLib
      depthsPerPkg := depthsPerPkg.zipWith max impModData.depthsPerPkg
  transDeps := transDeps.linearize
  depthsPerLib := depthsPerLib.modify libIdx (· + 1)
  depthsPerPkg := depthsPerPkg.modify pkgIdx (· + 1)
  let isPrelude := hasImplicitPrelude header
  let modData : WorkspaceModel.Module := { header with
    name := mod, srcFile, isPrelude, prevs, depthsPerLib, depthsPerPkg
    transDeps, transLibDeps, transPkgDeps, libIdx, pkgIdx }
  return { wm with
    idxOfMod := wm.idxOfMod.insert mod modIdx
    mods := wm.mods.push modData
    packages := wm.packages.modify pkgIdx fun p => { p with mods := insert modIdx p.mods }
    libs := wm.libs.modify libIdx fun l =>
      { l with mods := insert modIdx l.mods, revealedDeps := l.revealedDeps ∪ transLibDeps } }

end WorkspaceModel

open WorkspaceModel in
/--
Elaborate a `WorkspaceSummary` into a `WorkspaceModel` by following imports from the
libraries' roots, plus `extraMods` (curated modules not reachable from any root — e.g. a new
file not yet added to `Mathlib.lean`) and their imports. We do *not* walk the filesystem, so
scratch files that no root imports and that aren't in `extraMods` stay out. The resulting
`mods` array is topologically sorted (see `collect`).

The model is built incrementally: after the packages/libraries scaffold is in place we seed
an otherwise-empty model and grow it with `collect`, which fills every field except the
transitive closures `Module.transDeps`/`prevs` (the seam below). Until that seam runs the
returned model is partial.

Deliberately unoptimized: collection is one sequential recursive pass. Parallel header
parsing and caching the (per-toolchain, fixed) core graph are later iterations. -/
def Lake.WorkspaceSummary.toWorkspaceModel (ws : WorkspaceSummary)
    (extraMods : Array Name := #[]) : IO WorkspaceModel := do
  -- Phase 1: handle packages and libraries
  let toolchainPkgIdx := ws.packages.size
  let mut packages : Array WorkspaceModel.Package := ws.packages.map fun pkg =>
    { toBasePackage := pkg.toBasePackage
      deps := Bitset.ofArray pkg.deps ∪ {toolchainPkgIdx}
      -- Filled in later:
      libs := ∅, mods := ∅ }
  let toolchainName := ws.version.toToolchainName
  packages := packages.push
    { baseName := toolchainName, origName := `lean4, wsIdx := toolchainPkgIdx
      dir := ws.sysroot, leanLibDir := ws.sysroot / "lib" / "lean", deps := ∅
      -- Filled in later:
      libs := ∅, mods := ∅ }
  let mut libs : Array WorkspaceModel.Library := #[]
  for pkg in ws.packages do
    for l in pkg.libs do
      libs := libs.push { l with pkgIdx := pkg.wsIdx, revealedDeps := ∅, mods := ∅ }
  for lib in toolchainLibs ws.sysroot do
    libs := libs.push { lib with pkgIdx := toolchainPkgIdx, revealedDeps := ∅, mods := ∅ }
  for lib in libs, libIdx in 0...libs.size do
    packages := packages.modify lib.pkgIdx fun p => { p with libs := insert libIdx p.libs }
  -- Phase 2: handle modules and import dependencies
  -- Grow the model by following imports from each library's roots (packages in reverse
  -- order to reduce recursion), then from the extra modules.
  let mut wm : WorkspaceModel :=
    { ws with packages, libs, mods := #[], idxOfMod := ∅ }
  -- This array should be relatively small, so bite the copying here to ensure
  -- we have no sharing bugs later
  let allGlobs := wm.libs.flatMap (fun { globs, srcDir .. } => globs.map ((·,srcDir))) |>.reverse
  for (glob, srcDir) in allGlobs do
    for (mod, _) in glob.modulesIn srcDir do
      wm ← collect mod wm
  for mod in extraMods do
    wm ← collect mod wm
  return wm

/-- An interactive cache for the `WorkspaceModel`. -/
initialize WorkspaceModel.cacheRef : IO.Ref (Option WorkspaceModel) ← IO.mkRef none

-- TODO: bulletproof the case where `extraMods` refers to modules outside of default targets/
-- outside of `lean_libs`.
/-- Gets the workspace model. This reads the `workspaceModelCache` `IO.Ref` if `useCache := true` (the default). Note that this does not perform validation. However, note that at least if the
imports to the current file are changed, the file will be restarted. We do not yet guarantee
validity of the cache in the case where adjacent file imports are changed.

Note that this also implicitly relies on the workspace summary json cache, but that cache does not
contain module data and *is* validated (by `getWorkspaceSummary`). -/
def getWorkspaceModel (extraMods : Array Name := #[])
    (useCache := true) (cwd : Option System.FilePath := none) :
    IO WorkspaceModel := do
  if useCache then if let some wm ← WorkspaceModel.cacheRef.get then
    return wm
  let summary ← getWorkspaceSummary cwd
  let wm ← summary.toWorkspaceModel extraMods
  WorkspaceModel.cacheRef.set wm
  return wm

end ImportGraph
