/-
Copyright (c) 2026 Thomas Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas Murrills
-/
module

public import Lake.Config.Workspace
public import Lean.Data.Json
public import ImportGraph.WorkspaceModel.Base

import ImportGraph.Lake

/-!
# Transporting a Lake workspace summary over Json

This file defines `WorkspaceSummary` for summarizing a Lake workspace and `getWorkspaceSummary` for
transporting the bare minimum over a process boundary. (This is then used to compute a
`WorkspaceModel`, including import hierarchy data, with `getWorkspaceModel` from
`ImportGraph.WorkspaceModel.Build`.)

The motivation for this is the need to inspect the broader import hierarchy and lake workspace from
within the language server. However, loading the lake workspace from within the lake language
server causes a crash, so we must call out to an exe (`import-graph-workspace-summary`) across a
process boundary, and have it send back this data as json, which we then use (within the language
server) to compute the much richer `WorkspaceModel`.

`getWorkspaceSummary` also caches the resulting json in the `.lake` folder to avoid future external
calls if possible. Note that the module set is *not* included in the transported or cached json;
these are recomputed from the roots and globs stored in the json.

This shares `BaseWorkspace` with `WorkspaceModel`.
-/

public section

open Lean System Lake

namespace ImportGraph.Lake

/-- A summary of `lean_lib` data for transport over Json. -/
abbrev LibrarySummary := BaseLibrary

/-- A summary of a lake package for transport over Json. All paths are absolute. -/
structure PackageSummary extends BasePackage where
  /-- The Lake indices of the package's *direct* dependencies (Lake's `depPkgs`). -/
  deps : Array Nat
  /-- The package's `lean_lib`s. -/
  libs : Array LibrarySummary
  -- TODO: include other targets, e.g. `exe`'s, for import analysis.
deriving ToJson, FromJson, Repr, Inhabited

/-- A summary of the lake workspace suitable for transport over `Json`. This may be obtained with
`getWorkspaceSummary` and enriched into a model of the workspace and its intradependencies via
`getWorkspaceModel`. -/
structure WorkspaceSummary extends BaseWorkspace where
  /-- The packages of the workspace, in Lake's workspace order (root first); each
  package's position is its `lakeIdx`. -/
  packages : Array PackageSummary
  /-- The hash of inputs to this workspace summary: the lakefile, the lake manifest, and the
  toolchain version. -/
  inputHash : Hash
deriving ToJson, FromJson, Repr, Inhabited

def computeSummaryInputHash (ver : ToolchainVer)
    (manifestFile rootConfigFile : System.FilePath) : IO Hash := do
  let hash := Hash.ofHashable ver
  let hash := hash.mix <|← Hash.ofText <$> IO.FS.readFile manifestFile
  return hash.mix <|← Hash.ofText <$> IO.FS.readFile rootConfigFile

/-- Recomputes the hash of the data referred to by the paths in `WorkspaceSummary` and compares it
to the hash in `WorkspaceSummary`. -/
def WorkspaceSummary.isUpToDate (ws : WorkspaceSummary) : IO Bool := do
  let some newVer ← ToolchainVer.ofDir? ws.dir
    | throw (.userError s!"Could not find toolchain file in {ws.dir}")
  let newHash ← computeSummaryInputHash newVer ws.manifestFile ws.rootConfigFile
  return newHash == ws.inputHash

/-- Summarize a loaded `Lake.Workspace` for transport over Json. -/
def WorkspaceSummary.ofWorkspace (ws : Lake.Workspace)
    (version : ToolchainVer) (inputHash : Hash) : WorkspaceSummary where
  dir := ws.dir
  sysroot := ws.lakeEnv.lean.sysroot
  version := version
  inputHash
  manifestFile := ws.manifestFile
  rootConfigFile := ws.root.configFile
  packages := ws.packages.map fun pkg => { pkg with
    leanLibDir := pkg.leanLibDir
    deps := pkg.depPkgs.map (·.wsIdx)
    libs := pkg.leanLibs.filterMap fun lib => do
      -- TODO: include non-default targets with a flag instead of excluding them entirely
      guard <| pkg.defaultTargets.contains lib.name
      return {
        name := lib.name
        srcDir := lib.srcDir
        roots := lib.roots
        globs := lib.config.globs
      }
  }

/-- The name of the executable with root `ImportGraph.WorkspaceModel.Emit`.
Should be synchronized with the lakefile. -/
def WorkspaceSummary.exeName : String := "import-graph-workspace-summary"

/-- The "obvious" `lakeDir` given a workspace directory. TODO: Really, we ought to read this off of
the `lakeDir` field from the `lake-manifest.json` instead of just trying to append `.lake`. -/
private def lakeDirPath (wsDir : Option FilePath) : IO System.FilePath :=
  return (← wsDir.getDM IO.currentDir) / ".lake"

/-- A (new) folder in the given `.lake` directory for storing import graph data. -/
def importGraphBuildDirPath (lakeDir : System.FilePath) : System.FilePath :=
  lakeDir / "importGraph"

/-- Given a special-purpose build folder in the lake directory, the path to
`workspace-summary.json`, where we cache the workspace summary. -/
def WorkspaceSummary.cachePath (importGraphBuildDirPath : System.FilePath) : System.FilePath :=
  importGraphBuildDirPath / "workspace-summary.json"

-- TODO: think about this more. Is it really better than `withTempFile`?
/-- Atomically write `content` to `path` via a sibling temp file + rename. -/
private def atomicWriteFileViaTempSibling (path : FilePath) (content : String) : IO Unit := do
  let dir := path.parent.getD "."
  IO.FS.createDirAll dir
  -- Unique temp name IN THE SAME DIRECTORY, so the rename stays on one filesystem.
  let stamp ← IO.monoNanosNow
  let tmp := dir / s!"{path.fileName.getD "temp"}.{stamp}.tmp"
  try
    IO.FS.writeFile tmp content   -- open, write, deterministic close+flush
    IO.FS.rename tmp path         -- atomic same-fs replace
  catch e =>
    try IO.FS.removeFile tmp catch _ => pure ()  -- best-effort cleanup
    throw e

/--
Get the workspace summary by calling out to `lake exe import-graph-workspace-summary`, which emits
json that this function parses. (This is a workaround for the fact that loading the language server
in the language server causes a crash.)

Before calling out to the executable, this function checks a cache file in the `.lake` folder and
determines whether it's up-to-date. If so, it skips the executable call. If not, and it does call
out to the executable, then we also write the result to that cache file.
-/
def getWorkspaceSummary (wsDir : Option FilePath := none) : IO WorkspaceSummary := do
  let lakeDirPath ← lakeDirPath wsDir
  unless ← lakeDirPath.isDir do
    throw (.userError "Could not find `.lake` folder at {lakeFolderPath}")
  let importGraphBuildDirPath := importGraphBuildDirPath lakeDirPath
  let cachePath := WorkspaceSummary.cachePath importGraphBuildDirPath
  if ← cachePath.pathExists then
    let ws ← jsonOfString s!"Failed to get workspace summary from cache file at {cachePath}"
      (← IO.FS.readFile cachePath)
    if ← ws.isUpToDate then
      return ws
  let out ← IO.Process.run {
    cmd := "lake"
    args := #["exe", WorkspaceSummary.exeName]
    cwd := wsDir
    /-
    Search-path variables inherited from the spawning process (e.g. the language server) describe *its* setup and should not leak into a fresh `lake` invocation.
    -/
    env := #[("LEAN_PATH", none), ("LEAN_SRC_PATH", none), ("LAKE", none)] }
  -- Note: `.lake` is expected to still exist from the earlier check
  atomicWriteFileViaTempSibling cachePath out
  jsonOfString "Failed to get workspace summary" out
where jsonOfString errMsgHeader str : IO WorkspaceSummary := do
  let json ← IO.ofExcept <| Json.parse str |>.mapError
    (s!"{errMsgHeader}: invalid JSON:\n{·}")
  IO.ofExcept <| (fromJson? json : Except String WorkspaceSummary).mapError
    (s!"{errMsgHeader}: malformed workspace summary JSON:\n{·}")

end ImportGraph.Lake
