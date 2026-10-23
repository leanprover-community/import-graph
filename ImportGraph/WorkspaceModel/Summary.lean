/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.WorkspaceModel.Summary.Core
public import ImportGraph.WorkspaceModel.Summary.Get

/-!
Importing this file downstream is "safe", and introduces the following small Lake dependencies
for its public field types:
```
public import Lake.Config.Glob
public import Lake.Util.Version
```
These provide `Glob` and `ToolchainVer` in `WorkspaceSummary` field types. Cache validation also
uses `Lake.Build.Trace` for hashing. None of these imports brings in Lake workspace loading or
the build type-family axioms.

For Lake interaction with `WorkspaceSummary`, import `ImportGraph.WorkspaceModel.Summary.Lake`.
-/

assert_not_exists Lake.Workspace -- ensure we do not import deep lake internals
