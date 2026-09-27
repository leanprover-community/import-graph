/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.WorkspaceModel.Summary.Core
public import ImportGraph.WorkspaceModel.Summary.Get

/-!
Importing this file downstream is "safe", and only transitively introduces the following small Lake
dependencies:
```
public import Lake.Config.Glob
public import Lake.Util.Version
```
Both of these files do not transitively import any further Lake dependencies. They are necessary
for the types `Glob` and `ToolchainVer` in `WorkspaceSummary` field types.

For Lake interaction with `WorkspaceSummary`, import `ImportGraph.WorkspaceModel.Summary.Lake`.
-/

assert_not_exists Lake.Workspace --
