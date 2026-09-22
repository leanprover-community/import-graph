/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lake.Config.Workspace
public import Lean.Data.Json
public import ImportGraph.WorkspaceModel.Base

meta import Lean.Elab.Term.TermElabM
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
  /-- The package's config file (absolute). We use this (only) for hashing. -/
  configFile : FilePath
deriving ToJson, FromJson, Repr, Inhabited

/-- A summary of the lake workspace suitable for transport over `Json`. This may be obtained with
`getWorkspaceSummary` and enriched into a model of the workspace and its intradependencies via
`getWorkspaceModel`. -/
structure WorkspaceSummary extends BaseWorkspace where
  /-- The packages of the workspace, in Lake's workspace order (root first); each
  package's position is its `lakeIdx`. -/
  packages : Array PackageSummary
  /--
  The hash of inputs to this workspace summary: the lakefile (and the lakefiles of required
  packages), the lake manifest, the `package-overrides.json`, the `lean-toolchain` file, and
  the lean githash. -/
  inputHash : Hash
  /-- The `.lake/package-overrides.json` filepath (absolute). May not exist. -/
  packageOverridesFile : FilePath
deriving ToJson, FromJson, Repr, Inhabited

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
    return (← ws.recomputedInputHash Lean.githash) == ws.inputHash
  catch _ =>
    return false

/-- Summarize a loaded `Lake.Workspace` for transport over Json. -/
def WorkspaceSummary.ofWorkspace (ws : Lake.Workspace)
    (version : Option ToolchainVer) (inputHash : Hash) : WorkspaceSummary where
  dir := ws.dir
  sysroot := ws.lakeEnv.lean.sysroot
  version := version
  -- Note: we avoid the override with `ws.lakeEnv.lean.githash` instead of `ws.lakeEnv.leanGithash`.
  leanGitHash := ws.lakeEnv.lean.githash
  inputHash
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

/--
Get the workspace summary by calling out to `lake exe import-graph-workspace-summary`, which emits
json that this function parses. (This is a workaround for the fact that loading the language server
in the language server causes a crash.)

Before calling out to the executable, this function checks a cache file in the `.lake` folder and
determines whether it's up-to-date. If so, it skips the executable call. If not, and it does call
out to the executable, then we also write the result to that cache file.

If `readCache := false`, do not read from the cache, but still write to it.
-/
def getWorkspaceSummary (wsDir : Option FilePath := none) (readCache := true) :
    IO WorkspaceSummary := do
  let lakeDirPath ← lakeDirPath wsDir
  unless ← lakeDirPath.isDir do
    throw (.userError s!"Could not find `.lake` folder at {lakeDirPath}")
  let importGraphBuildDirPath := importGraphBuildDirPath lakeDirPath
  let cachePath := WorkspaceSummary.cachePath importGraphBuildDirPath
  if ← pure readCache <&&> cachePath.pathExists then
    try
      let ws ← jsonOfString s!"Failed to get workspace summary from cache file at {cachePath}"
        (← IO.FS.readFile cachePath)
      if ← ws.isUpToDate (wsDir? := ← wsDir.getDM IO.currentDir) then
        return ws
    catch _ => pure () -- Regenerate if we failed the above for any reason
  let out ← IO.Process.run {
    cmd := "lake"
    args := #["exe", WorkspaceSummary.exeName]
    cwd := wsDir
    /-
    Search-path variables inherited from the spawning process (e.g. the language server) describe *its* setup and should not leak into a fresh `lake` invocation.
    -/
    env := #[("LEAN_PATH", none), ("LEAN_SRC_PATH", none)] }
  -- Note: `.lake` is expected to still exist from the earlier check.
  -- We regenerate the cache if the result of this fails to parse, so we don't
  -- take pains to prevent bad caches due to killing the process mid-write.
  try IO.FS.writeFile cachePath out catch ex =>
    throw (IO.userError s!"Failed to write workspace summary cache:\n{ex}")
  jsonOfString "Failed to get workspace summary" out
where jsonOfString errMsgHeader str : IO WorkspaceSummary := do
  let json ← IO.ofExcept <| Json.parse str |>.mapError
    (s!"{errMsgHeader}: invalid JSON:\n{·}")
  IO.ofExcept <| (fromJson? json : Except String WorkspaceSummary).mapError
    (s!"{errMsgHeader}: malformed workspace summary JSON:\n{·}")

end ImportGraph.Lake
