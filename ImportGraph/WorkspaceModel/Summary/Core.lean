/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lean.Data.Json
public import ImportGraph.WorkspaceModel.Base

import ImportGraph.Util.Modules

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
  the lean githash.

  We unwrap lake's `Hash` into a `UInt64` to avoid a public lake dependency. -/
  inputHash : UInt64
  /-- The `.lake/package-overrides.json` filepath (absolute). May not exist. -/
  packageOverridesFile : FilePath
deriving ToJson, FromJson, Repr, Inhabited

/-- The name of the executable with root `ImportGraph.WorkspaceModel.Emit`.
Should be synchronized with the lakefile. -/
def WorkspaceSummary.exeName : String := "import-graph-workspace-summary"

/-- The "obvious" `lakeDir` given a workspace directory. TODO: Really, we ought to read this off of
the `lakeDir` field from the `lake-manifest.json` instead of just trying to append `.lake`. -/
def lakeDirPath (wsDir : Option FilePath) : IO System.FilePath :=
  return (← wsDir.getDM IO.currentDir) / ".lake"

/-- A (new) folder in the given `.lake` directory for storing import graph data. -/
def importGraphBuildDirPath (lakeDir : System.FilePath) : System.FilePath :=
  lakeDir / "importGraph"

/-- The directory in which the cache lives. This is currently
`importGraphBuildDirPath := .lake/importGraph/`, a directory exclusively for special import graph
data such as the workspace summary cache. -/
def WorkspaceSummary.cacheDirPath := importGraphBuildDirPath

/-- Given a special-purpose build folder in the lake directory, the path to
`workspace-summary.json`, where we cache the workspace summary. -/
def WorkspaceSummary.cachePath (cacheDir : System.FilePath) : System.FilePath :=
  cacheDir / "workspace-summary.json"

end ImportGraph.Lake
