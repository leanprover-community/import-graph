/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lake.Config.Workspace
public import ImportGraph.WorkspaceModel.Summary.Cache

import ImportGraph.Lake

public section

open ImportGraph Lean Lake System

namespace ImportGraph.Lake

/-- Computes the hash for the given workspace to persist in the summary. This should agree with the
recomputed hash from the workspace summary if no changes are made to the package configuration. -/
nonrec def Workspace.computeInputHash (ws : Lake.Workspace) : IO Hash := do
  -- Note: we avoid the override with `ws.lakeEnv.lean.githash` instead of `ws.lakeEnv.leanGithash`.
  -- It's possible the opposite choice is more useful.
  computeInputHash ws.lakeEnv.lean.githash (← ToolchainVer.ofDir? ws.dir)
    (manifestFile := ws.manifestFile)
    (packageOverridesFile := ws.packageOverridesFile)
    (packageConfigs := ws.packages.map (·.configFile))

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
