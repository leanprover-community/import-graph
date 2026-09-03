/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

import Lake.Load.Workspace
import ImportGraph.Lake
import ImportGraph.WorkspaceModel.Summary

/-!
# Root of `lake exe import-graph-workspace-summary`

This file provides the `main` function for `import-graph-workspace-summary`, which is used to
extract information about the lake workspace from the language server by providing a process
boundary. This avoids a crash that results from loading the lake workspace in the language server.

Note that this does not interact with the workspace summary cache. Managing the cache is the
responsibility of the spawner (`getWorkspaceSummary`).
-/

open Lean ImportGraph Lake

-- TODO: explore making this a lake script or facet, since we're essentially loading the lake
-- workspace twice by calling this with `lake exe`.
public def main (args : List String) : IO UInt32 := do
  let wsDir : System.FilePath ← do
    match args with
    | [] => IO.currentDir
    | [dir] => IO.FS.realPath dir
    | _ => IO.eprintln "Expected either no arguments or a path to a package's root."; return 2
  let (elan?, lean?, lake?) ← findInstall?
  let some lean := lean?
    | IO.eprintln "error: no Lean installation found"; return 1
  let lake := lake?.getD (.ofLean lean)
  let lakeEnv ← (Env.compute lake lean elan?).toIO (IO.userError ·)
  let cfg : LoadConfig := { lakeEnv, wsDir }
  let (ws?, log) ← (loadWorkspace cfg).run?
  if log.any (·.level matches .error) then
    IO.eprintln s!"error: Errors were produced while loading the Lake workspace at {wsDir}.\n\
      Log:\n{log}"; return 1
  let some ws := ws?
    | IO.eprintln s!"error: Failed to load the Lake workspace at {wsDir}.\n\
        Log:\n{log}"; return 1
  let ver ← ws.getToolchainVer
  let hash ← computeSummaryInputHash ver ws.manifestFile ws.root.configFile
  let json := toJson (WorkspaceSummary.ofWorkspace ws ver hash)
  IO.println json.compress
  return 0
