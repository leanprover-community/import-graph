/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lake.Config.Workspace
public import ImportGraph.WorkspaceModel.Summary.Core

import ImportGraph.Lake

public section

open ImportGraph Lean Lake System

namespace ImportGraph.Lake

private def computeInputHash (leanGitHash : String) (ver : Option ToolchainVer)
    (manifestFile packageOverridesFile : FilePath)
    (packageConfigs : Array FilePath) : IO Hash := do
  let mut hash := Hash.ofHashable ver
  hash := hash.mix <| Hash.ofText leanGitHash
  hash := hash.mix <|← Hash.ofText <$> IO.FS.readFile manifestFile
  if ← packageOverridesFile.pathExists then
    try
      hash := hash.mix <|← Hash.ofText <$> IO.FS.readFile packageOverridesFile
    catch _ => pure () -- ignore it if something went wrong
  -- Note: `packageConfigs` should (and by default does) include the root package's config as well.
  for configFile in packageConfigs do
    hash := hash.mix <|← Hash.ofText <$> IO.FS.readFile configFile
  return hash

/-- Computes the hash for the given workspace to persist in the summary. This should agree with the
recomputed hash from the workspace summary if no changes are made to the package configuration. -/
nonrec def Workspace.computeInputHash (ws : Lake.Workspace) : IO Hash := do
  -- Note: we avoid the override with `ws.lakeEnv.lean.githash` instead of `ws.lakeEnv.leanGithash`.
  -- It's possible the opposite choice is more useful.
  computeInputHash ws.lakeEnv.lean.githash (← ToolchainVer.ofDir? ws.dir)
    (manifestFile := ws.manifestFile)
    (packageOverridesFile := ws.packageOverridesFile)
    (packageConfigs := ws.packages.map (·.configFile))

/-- Recomputes the input hash for the `WorkspaceSummary` by re-hashing the files at the given
paths. Also mixes in the hash for the given lean version. -/
def WorkspaceSummary.recomputedInputHash (leanGitHash : String) (ws : WorkspaceSummary) :
    IO Hash := do
  computeInputHash leanGitHash (← ToolchainVer.ofDir? ws.dir)
    (manifestFile := ws.manifestFile)
    (packageOverridesFile := ws.packageOverridesFile)
    (packageConfigs := ws.packages.map (·.configFile))

/-- Recomputes the hash of the data referred to by the paths in `WorkspaceSummary` and compares it
to the hash in `WorkspaceSummary`, using the current lean process's git hash.

If `wsDir?` is provided, ensures that the workspace directory provided in the summary is the same
as the given `wsDir`, else considers it not up-to-date. -/
def WorkspaceSummary.isUpToDate (ws : WorkspaceSummary) (wsDir? : Option FilePath := none) :
    IO Bool := do
  try
    if let some wsDir := wsDir? then
      unless (← IO.FS.realPath ws.dir).normalize == (← IO.FS.realPath wsDir).normalize do
        return false
    return (← ws.recomputedInputHash Lean.githash).val == ws.inputHash
  catch _ =>
    return false

/-- Summarize a loaded `Lake.Workspace` for transport over Json. -/
def WorkspaceSummary.ofWorkspace (ws : Lake.Workspace)
    (version : Option ToolchainVer) (inputHash : Hash) : WorkspaceSummary where
  dir := ws.dir
  sysroot := ws.lakeEnv.lean.sysroot
  leanLibDir := ws.lakeEnv.lean.leanLibDir
  lakeSrcDir := ws.lakeEnv.lake.srcDir
  leanSrcDir := ws.lakeEnv.lean.srcDir
  version := version
  -- Note: we avoid the override with `ws.lakeEnv.lean.githash` instead of `ws.lakeEnv.leanGithash`.
  leanGitHash := ws.lakeEnv.lean.githash
  inputHash := inputHash.val
  manifestFile := ws.manifestFile
  packageOverridesFile := ws.packageOverridesFile
  packages := ws.packages.map fun pkg => { pkg with
    leanLibDir := pkg.leanLibDir
    -- NOTE: if `depPkgs` changes, we should use whatever API allows us to get the indices of the
    -- dependent pacakges. It's acceptable if these become transitive dependencies instead of
    -- direct dependencies; in this case, we should rename `PackageSummary.deps` to `transDeps`.
    deps := pkg.depPkgs.map (·.wsIdx)
    libs := pkg.leanLibs.filterMap fun lib => do
      -- This is a hack to allow us to test the import hierarchy while within `importGraph`.
      -- This does not add `ImportGraphTest` to the hierarchy outside of `importGraph`.
      unless pkg.isRoot && lib.name == `ImportGraphTest do
        -- TODO: include non-default targets with a flag instead of excluding them entirely
        guard <| pkg.defaultTargets.contains lib.name
      return {
        name := lib.name
        srcDir := lib.srcDir
        roots := lib.roots
        globs := lib.config.globs
      }
  }
