/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.Lean.Json
public import ImportGraph.Shake.Basic
public import ImportGraph.WorkspaceModel.Base
public import Lean.Message -- only for `ToMessageData` on `WorkspaceModel.Error`

/-!
# A model of the Lake workspace

This file introduces `WorkspaceModel`, which carries all of the packages, `lean_lib`s, and modules
of the lake workspace, together with dependency arrows extracted from imports.

We use `Bitset`s when possible for efficiency. Note that every module lives in a flat `Array`, as
does every library, regardless of what package or library it comes from. Relationships between
modules, libraries, and packages are stored in each, \using the (unique, global) index of a given
module/library/package as a proxy wherever possible.

The toolchain is recorded as a "pseudo-package" with four libraries (`Std`, `Init`, `Lean`, and
`Lake`).

## Future work

- We currently ignore `lean_lib`s which are not default lake targets. We should instead simply
  record whether a given library is default or not, and handle that in downstream logic.
- There is likely room for performance improvement by e.g. not eagerly computing certain data.
- We currently ignore targets that are not `lean_lib`s entirely.
- We could make the indexing system more typesafe. Currently `ModIdx`/`LibIdx`/`PkgIdx` are just
  `abbrev`'s for `Nat`.

We build a `WorkspaceModel` from a `WorkspaceSummary` using `getWorkspaceModel` in
`ImportGraph.WorkspaceModel.Build`. The `WorkspaceSummary` itself is extracted from an external call to a `lake` process which loads the workspace, then cached in the build folder.
-/

open Lean System ImportGraph Lake Lake Shake

public section

namespace ImportGraph

/-- A `Bitset` whose bit positions are indices for a `WorkspaceModel`'s indexed datatypes (e.g.
`WorkspaceModel.packages`). These indices are `PkgIdx`s. -/
abbrev PackageBitset := Bitset

/-- A `Bitset` whose bit positions are indices for a `WorkspaceModel`'s indexed datatypes (e.g.
`WorkspaceModel.libs`). These indices are `LibIdx`s. -/
abbrev LibraryBitset := Bitset

/-- A `Bitset` whose bit positions are indices for a `WorkspaceModel`'s indexed datatypes (e.g.
`WorkspaceModel.mods`). These indices are `ModIdx`s (not to be confused with `ModuleIdx`s). -/
abbrev ModuleBitset := Bitset

/-- The index of a `lean_lib` in a `WorkspaceModel`'s `Bitset`s and `Array`s. Caution: simply an
`abbrev` for `Nat`. -/
abbrev LibIdx := Nat
/-- The index of a lake package in a `WorkspaceModel`'s `Bitset`s and `Array`s. -/
abbrev PkgIdx := Nat
/-- The index of a module in a `WorkspaceModel`'s `Bitset`s and `Array`s.

Note that this is **not** the same as a `ModuleIdx` in a given environment. However, both are
presently defs for `Nat`s, so use caution to avoid using one in place of the other. -/
abbrev ModIdx := Nat

namespace WorkspaceModel

/--
One package of the model: a Lake package, or the toolchain pseudo-package (last; see the
module docstring). All paths are absolute.
-/
structure Package extends BasePackage where
  -- TODO: `revealedPkgDeps` from module imports; `transDeps`.
  /-- Dependency arrows: the package's *direct* dependencies, as resolved by Lake (plus the
  toolchain pseudo-package). -/
  deps : PackageBitset
  /-- Workspace relation: the libraries belonging to the package. -/
  libs : LibraryBitset
  /-- Workspace relation: the modules belonging to the package (the union over `libs`). -/
  mods : ModuleBitset
deriving Repr, BEq, Inhabited

/--
One Lean library of the model, including the pseudo-libraries `Init`/`Std`/`Lean`/`Lake`
of the toolchain pseudo-package.
-/
structure Library extends BaseLibrary where
  /-- The libraries transitively imported by modules in this library. (May not contain its own index if no module in the library imports something from the library.) -/
  revealedDeps : LibraryBitset
  /-- Relational: the enumerated modules contained *in* the library (found on disk under
  its `roots`/`globs`). Not necessarily everything that gets built when the library is built. -/
  mods : ModuleBitset
  /-- Relational: the package the library belongs to. -/
  pkgIdx : PkgIdx
deriving Repr, BEq, Inhabited

/--
One module of the model. Modules enter the model by enumerating the source trees of a
chosen set of libraries (see `ImportGraph.WorkspaceModel.Build`); imports of modules
outside that enumeration remain visible in `imports` but have no bits anywhere.
-/
structure Module extends ModuleHeader where
  /-- The module's name. -/
  name : Name
  /-- The module's source filepath (absolute). -/
  srcFile : FilePath
  /-- Whether the module has the `prelude` keyword (and hence no implicit `Init` imports). -/
  isPrelude : Bool
  /-- Dependency arrows: Transitive dependencies in the module system.  -/
  transDeps : Provides
  /-- Dependency arrows: The transitively reachable libraries this module depends on. Does not
  necessarily include its own library. -/
  transLibDeps : LibraryBitset
  /-- Dependency arrows: The transitively reachable packages this module depends on. Does not
  necessarily include its own package. -/
  transPkgDeps : PackageBitset
  /-- Dependency arrows: Every module that must be built before the current module. -/
  prevs : ModuleBitset
  -- TODO: consider calculating lazily instead of eagerly.
  /-- Dependency statistic: Per library, the longest chain of modules from that library that must
  be built before building this modules, including this module. `0` indicates non-dependence on the
  library. The `Array` index is the library's `LibIdx`. -/
  depthsPerLib : Array Nat
  /-- Dependency statistic: Per package, the longest chain of modules from a given package that
  must be built before building this modules, including this module. `0` indicates non-dependence on the package. The `Array` index is the package's `PkgIdx`. -/
  depthsPerPkg : Array Nat
  /-- Workspace relation: The library this module belongs to. -/
  libIdx : LibIdx
  /-- Workspace relation: the package the module belongs to. Redundant, but we store it here for
  convenience. -/
  pkgIdx : PkgIdx
deriving Repr, Inhabited

end WorkspaceModel

/-- Errors that may be produced when creating the `WorkspaceModel`. -/
inductive WorkspaceModel.Error where
| /-- Failed to read the imports from a given file (possibly because we failed to locate the
  file). -/
  readImportsFailure (mod : Name) (modPath : System.FilePath) (ioError : IO.Error) : Error
| /-- The given module does not participate in a `lean_lib`. -/
  noLibOfModule (mod : Name)
deriving Repr, Inhabited, ToJson, FromJson

instance : ToMessageData WorkspaceModel.Error where
  toMessageData
    | .noLibOfModule mod => m!"Could not find library for module `{mod}`."
    | .readImportsFailure mod path ioError =>
      m!"Failed to read imports of `{mod}`:{indentD ioError.toString}\n\n\
        Path to module: {path}"

/-- A model of the lake workspace, containing all packages, libraries, and modules and their
relationships, as well as dependency arrows between them extracted from parsed source imports. -/
structure WorkspaceModel extends BaseWorkspace where
  /-- The packages: Lake packages in Lake's order, with the toolchain "package" last. The index in
  this array is a `PkgIdx` and matches the index used in other package `Array`s and
  `PackageBitset`s. -/
  packages : Array WorkspaceModel.Package
  /-- The libraries of all packages in package order (toolchain libraries last). The index of the
  library in this array is a `LibIdx` and matches its index in other library `Array`s and
  `LibraryBitset`s. -/
  libs : Array WorkspaceModel.Library
  /-- The modules of all packages and libraries in some topological order, with imported modules
  coming first and the modules that import them afterwards. The index of a module in this array
  is a `ModIdx` and matches the index in other module `Array`s and `ModuleBitset`s. -/
  mods : Array WorkspaceModel.Module
  /-- Module name → module index. -/
  idxOfMod : Std.HashMap Name ModIdx
  /-- Errors collected when creating the workspace model. -/
  errors : Array WorkspaceModel.Error := #[]
deriving Repr, Inhabited

namespace WorkspaceModel

/-- Whether errors have been produced when creating the `WorkspaceModel`. -/
@[inline] def hasErrors (w : WorkspaceModel) := !w.errors.isEmpty

/-! ## Lookups -/

variable (m : WorkspaceModel)

/-- The index of the toolchain pseudo-package (the last package). -/
def toolchainPkgIdx : PkgIdx := m.packages.size - 1

/-- The index of the module named `mod`, if it is in the model. -/
@[inline] def getModIdx? (mod : Name) : Option ModIdx := m.idxOfMod[mod]?

/-- The index of the package with original name `name`, if any. -/
@[inline] def getPkgIdx? (origName : Name) : Option PkgIdx :=
  m.packages.findIdx? (·.origName == origName)

/-- The index of the library named `name`, if any. (Library names are not necessarily
unique across packages, so we ask for the package index as well.) -/
@[inline] def getLibIdx? (pkgIdx : PkgIdx) (libName : Name) : Option LibIdx :=
  m.libs.findIdx? fun lib => lib.pkgIdx == pkgIdx && lib.name == libName

@[inline] def getMod! (modIdx : ModIdx) : Module  := m.mods[modIdx]!
@[inline] def getLib! (libIdx : LibIdx) : Library := m.libs[libIdx]!
@[inline] def getPkg! (pkgIdx : PkgIdx) : Package := m.packages[pkgIdx]!

@[inline] def libOfMod! (mod : Module) : Library := m.getLib! mod.libIdx
@[inline] def pkgOfMod! (mod : Module) : Package := m.getPkg! mod.pkgIdx
@[inline] def pkgOfLib! (lib : Library) : Package := m.getPkg! lib.pkgIdx

@[inline] def libIdxOfModIdx! (modIdx : ModIdx) : LibIdx := m.getMod! modIdx |>.libIdx
@[inline] def pkgIdxOfModIdx! (modIdx : ModIdx) : PkgIdx := m.getMod! modIdx |>.pkgIdx
@[inline] def pkgIdxOfLibIdx! (libIdx : LibIdx) : PkgIdx := m.getLib! libIdx |>.pkgIdx

@[inline] def libOfModIdx! (modIdx : ModIdx) : Library := m.getLib! <| m.libIdxOfModIdx! modIdx
@[inline] def pkgOfModIdx! (modIdx : ModIdx) : Package := m.getPkg! <| m.pkgIdxOfModIdx! modIdx
@[inline] def pkgOfLibIdx! (libIdx : LibIdx) : Package := m.getPkg! <| m.libIdxOfModIdx! libIdx

@[inline] def libDepth! (modIdx : ModIdx) (libIdx := m.libIdxOfModIdx! modIdx) : Nat :=
  m.getMod! modIdx |>.depthsPerLib[libIdx]!
@[inline] def pkgDepth! (modIdx : ModIdx) (pkgIdx := m.pkgIdxOfModIdx! modIdx) : Nat :=
  m.getMod! modIdx |>.depthsPerPkg[pkgIdx]!

/-!
## Lake lifts

This section contains lifts of basic lake functions (usually of the same name) to `WorkspaceModel`.
-/

/-- `LeanLib.isLocalModule`, but lifted to `WorkspaceModel.Library`. -/
@[inline] def Library.isLocalModule (l : Library) (mod : Name) : Bool :=
  l.roots.any (·.isPrefixOf mod) || l.globs.any (·.matches mod)

@[inline] def rawLibIdxOfMod? (libs : Array WorkspaceModel.Library) (mod : Name) : Option LibIdx :=
  libs.findIdx? (·.isLocalModule mod)

@[inline] def libIdxOfMod? (mod : Name) : Option LibIdx :=
  m.libs.findIdx? (·.isLocalModule mod)

/-- Like `Lake.Module.leanLibFile`, but for `WorkspaceModel.Module`. -/
@[inline] def Module.leanLibFile (mod : Module) (ext : String) : FilePath :=
  modToFilePath (m.pkgOfMod! mod).leanLibDir mod.name ext

@[inline] def Library.srcPathOfMod (lib : Library) (mod : Name) : FilePath :=
  modToFilePath lib.srcDir mod "lean"

@[inline] def srcPathOfMod? (mod : Name) : Option FilePath :=
  m.libIdxOfMod? mod |>.map (modToFilePath m.libs[·]!.srcDir mod "lean")

@[inline] def Module.srcPath (mod : Module) : FilePath :=
  modToFilePath (m.libOfMod! mod).srcDir mod.name "lean"

end ImportGraph.WorkspaceModel
