/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.Util.Modules
public import Lake.Config.Workspace

import Lake.Load.Workspace

/-!
# Lake workspace loading

Workspace loading is intended for executables. Library consumers needing only module traversal
and the associated instances should import `ImportGraph.Util.Modules` instead.
-/

open Lean Lake

namespace ImportGraph.Lake.IO

/-- Loads the lake workspace from the current directory (or, if specified, from `wsDir?`) in `IO`.

Note that in the language server, the current working directory is the workspace root, so this may
use the current working directory of elaboration. However, it may not itself be called directly
during elaboration, as this causes the language server to crash. Therefore, for use "in" the
language server, it must be called across a process boundary via an exe. -/
public def getWorkspace (wsDir? : Option System.FilePath := none) : IO Workspace := do
  let wsDir ← wsDir?.getDM IO.currentDir
  let (elan?, lean?, lake?) ← findInstall?
  let some lean := lean?
    | throw (.userError "error: no Lean installation found")
  let lake := lake?.getD (.ofLean lean)
  let lakeEnv ← (Env.compute lake lean elan?).toIO (IO.userError ·)
  let (ws?, log) ← (Lake.loadWorkspace { lakeEnv, wsDir }).run?
  if log.any (·.level matches .error) then
    throw <| .userError
      s!"error: Errors were produced while loading the Lake workspace at {wsDir}.\n\
        Log:\n{log}"
  let some ws := ws?
    | throw <| .userError s!"error: Failed to load the Lake workspace at {wsDir}.\n\
        Log:\n{log}"
  return ws

end ImportGraph.Lake.IO
