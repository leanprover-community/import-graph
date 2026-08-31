/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lake.Config.Glob
public import Lake.Util.Version

import ImportGraph.Lake

/-!
# Basic Lake workspace data

This file defines the `BaseWorkspace` shared by both the `WorkspaceSummary`, which is transported
as Json across a process boundary (extracted from loading the lake workspace), and the
`WorkspaceModel`, which is computed from the `WorkspaceSummary` (but doesn't need some of its
fields). See `ImportGraph.WorkspaceModel.WorkspaceSummary` and `ImportGraph.WorkspaceModel.Model`.
-/

public section

open Lean System Lake

namespace ImportGraph.Lake

/-- Basic data for a `lean_lib`. -/
structure BaseLibrary where
  /-- The library's name. -/
  name : Name
  /-- The directory relative to which the library's module names locate source files
  (absolute). -/
  srcDir : FilePath
  /-- The library's root module names. -/
  roots : Array Name := #[name]
  /-- The globs specifying the library's buildable modules. -/
  globs : Array Lake.Glob := #[name]
deriving ToJson, FromJson, Repr, BEq, Inhabited

/-- Basic data for a lake package. All paths are absolute. -/
structure BasePackage where
  /-- The package's assigned name (`Package.baseName`). -/
  baseName : Name
  /-- The package's original name (`Package.origName`). -/
  origName : Name
  /-- Lake's index for the package (`Package.wsIdx`) Together with `baseName`, this disambiguates
  packages. -/
  wsIdx : Nat
  /-- The package's root directory (absolute). -/
  dir : FilePath
  /-- The directory holding the package's compiled module artifacts (`.olean`s etc.),
  e.g. `<dir>/.lake/build/lib/lean`. -/
  leanLibDir : FilePath
deriving ToJson, FromJson, Repr, BEq, Inhabited

/-- The prefix we use for modelling the Lean toolchain, which is simply `toolchain`. -/
def toolchainPrefix := `toolchain

/-- A `ToolchainVer` as a `Name`. -/
@[inline] def ToolchainVer.toToolchainName (ver : ToolchainVer) :=
  Name.str toolchainPrefix ver.toString

/-- Whether a name is of the form `toolchain.<ver>`. -/
@[inline] def isToolchainName (n : Name) :=
  match n with | .str base _ => base == toolchainPrefix | _ => false

/-- The `ToolchainVer` extracted from a name of the form `toolchain.<ver>`. -/
@[inline] def versionOfToolchainName? (n : Name) : Option ToolchainVer :=
  match n with
  | .str base ver => do
    guard <| base == toolchainPrefix
    ToolchainVer.ofString ver
  | _ => none

deriving instance Inhabited for ToolchainVer

/-- The basic data of a Lake workspace shared by both the transported `WorkspaceSummary` and the
rich, computed `WorkspaceModel`. -/
structure BaseWorkspace where
  /-- The workspace root directory (absolute). -/
  dir : FilePath
  /-- The Lean toolchain's sysroot (absolute). -/
  sysroot : FilePath
  /-- The Lean toolchain's version. -/
  version : ToolchainVer
  /-- The path to the lake manifest. Should be uniform, but is allowed to change in lake internals,
  so just in case. -/
  manifestFile : System.FilePath
  /-- The lakefile of the root package. -/
  rootConfigFile : FilePath
deriving ToJson, FromJson, Repr, BEq, Inhabited
