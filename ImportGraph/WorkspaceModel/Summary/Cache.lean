/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lake.Build.Trace
public import ImportGraph.WorkspaceModel.Summary.Core

import ImportGraph.Util.Modules

/-!
# Workspace summary cache validation

Computes hashes from the paths stored in a workspace summary. This file does not import Lake
workspace configuration or loading; extraction from a live workspace belongs in `Summary.Lake`.
-/

public section

open Lean Lake System

namespace ImportGraph.Lake

/-- Internal helper shared by cache validation and the workspace-summary executable.
Hashes the toolchain identity and workspace configuration files. -/
def computeInputHash (leanGitHash : String) (ver : Option ToolchainVer)
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

end ImportGraph.Lake
