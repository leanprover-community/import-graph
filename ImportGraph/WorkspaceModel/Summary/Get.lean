/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.WorkspaceModel.Summary.Core

import ImportGraph.WorkspaceModel.Summary.Cache

open ImportGraph Lean Lake System

namespace ImportGraph.Lake

/--
Get the workspace summary by calling out to `lake exe import-graph-workspace-summary`, which emits
json that this function parses. (This is a workaround for the fact that loading the language server
in the language server causes a crash.)

Before calling out to the executable, this function checks a cache file in the `.lake` folder and
determines whether it's up-to-date. If so, it skips the executable call. If not, and it does call
out to the executable, then we also write the result to that cache file.

If `readCache := false`, do not read from the cache, but still write to it.
-/
public def getWorkspaceSummary (wsDir : Option FilePath := none) (readCache := true) :
    IO WorkspaceSummary := do
  let lakeDirPath ← lakeDirPath wsDir
  unless ← lakeDirPath.isDir do
    throw (.userError s!"Could not find `.lake` folder at {lakeDirPath}")
  let cacheDirPath := WorkspaceSummary.cacheDirPath lakeDirPath
  let cachePath := WorkspaceSummary.cachePath cacheDirPath
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
  try
    IO.FS.createDirAll cacheDirPath
    IO.FS.writeFile cachePath out
  catch ex =>
    throw (IO.userError s!"Failed to write workspace summary cache:\n{ex}")
  jsonOfString "Failed to get workspace summary" out
where jsonOfString errMsgHeader str : IO WorkspaceSummary := do
  let json ← IO.ofExcept <| Json.parse str |>.mapError
    (s!"{errMsgHeader}: invalid JSON:\n{·}")
  IO.ofExcept <| (fromJson? json : Except String WorkspaceSummary).mapError
    (s!"{errMsgHeader}: malformed workspace summary JSON:\n{·}")
