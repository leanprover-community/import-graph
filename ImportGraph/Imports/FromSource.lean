/-
Copyright (c) 2025 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Thomas R. Murrills
-/
module

public import Lean.Elab.ParseImportsFast
public import Lean.Util.Path

/-!
# Source-File-Based Import Analysis

This module provides functions for analyzing imports by parsing source files directly,
as an alternative to `Environment`-based analysis (e.g. in `ImportGraph.Imports`). Specifically:

- `parseImports?`, `System.FilePath.parseImports'`: Parse direct imports from a single
  string or file
- `Lean.ModuleHeader.filterInit`: Remove `Init` imports from a parsed header
- `parseCurrentHeader`: parse the imports of the current file from the `FileMap`
- `modToRelFilePath`: like `modToFilePath`, but does not insert a leading file separator
- `findModuleSource?`: locate the source file of a module, via the source search path
  (`LEAN_SRC_PATH`), the `.olean` search path (`LEAN_PATH`), or the working directory
- `findTransitiveImportsFromSource`: Compute a nameset of the transitive closure of imports from
  source files. Note, however, that this does not respect the module system.
-/

public section

open Lean System

namespace ImportGraph

-- TODO: consider returning `prelude`. `parseImports'` translates a `prelude` into `Init` imports.
/-- Like `parseImports'`, but pure. Instead of returning the `fileName:pos <msg>` error of
`parseImports'` of `parseImports'`, returns `(fileMap, pos, <msg>)`. -/
def parseImports? (input : String) : Except (FileMap × Position × String) ModuleHeader := do
  let s := ParseImports.main input (ParseImports.whitespace input {})
  let some err := s.error?
    | return { s with }
  let fileMap := input.toFileMap
  let pos := fileMap.toPosition s.pos
  throw (fileMap, pos, err)

/-
Parse the header of the source file at `path`, returning its `ModuleHeader`.

This is a thin wrapper around `Lean.parseImports'` which:
- Reads the file from disk
- Parses the import statements

Note that it does not filter out `Init` modules. See `ModuleHeader.filterInit`.
-/
def System.FilePath.parseImports' (path : System.FilePath) : IO ModuleHeader := do
  Lean.parseImports' (← IO.FS.readFile path) path.toString

/-- Removes every import in the `Init` namespace (`Init` itself and `Init.*`)
from a `ModuleHeader`. -/
def Lean.ModuleHeader.filterInit (m : ModuleHeader) : ModuleHeader :=
  { m with imports := m.imports.filter fun imp => !(`Init).isPrefixOf imp.module }

/-
Parses the header of the current file via the source string present in the ambient `FileMap`.
-/
def parseCurrentHeader {m} [Monad m] [MonadLog m] [MonadLiftT IO m] :
    m (TSyntax `Lean.Parser.Module.header × Parser.ModuleParserState × MessageLog) := do
  Parser.parseHeader (Parser.mkInputContext (← getFileMap).source (← getFileName))

/-
Parse all imports in a source file at `path` and return their module names.

This is a thin wrapper around `Lean.parseImports'` that:
- Reads the file from disk
- Parses the import statements
- Filters out `Init` (part of the prelude)

Note: This only sees syntactic imports in the source file.
It does not account for what declarations are actually used.
-/
@[deprecated "Use `ImportGraph.System.FilePath.parseImports'` and `ImportGraph.Lean.ModuleHeader.filterInit` instead"
  (since := "2026-09-13")]
public def findImportsFromSource (path : System.FilePath) : IO (Array Name) := do
  -- Note: we use `filter` rather than `erase`, since module-system files may contain
  -- both an implicit `public import Init` and a `meta import Init`, so `Init` can
  -- appear more than once in the parsed imports.
  return (← Lean.parseImports' (← IO.FS.readFile path) path.toString).imports
    |>.map (·.module) |>.filter (· != `Init)

/-- Like `modToFilePath`, but does not demand `base`. Example: `modToRelFilePath A.B.C "lean"`
results in `A/B/C.lean`. (Note that `modToFilePath "" mod ext` inserts a leading file separator,
and would result in `/A/B/C.lean`.) -/
def modToRelFilePath (mod : Name) (ext : String) : FilePath :=
  go mod |>.addExtension ext
where
  go : Name → FilePath
  | Name.str .anonymous h => h
  | Name.str p h => go p / h
  | _ => panic! "ill-formed import"

/-
Find the source file of the module `mod`, trying in order:

1. the source search path (`Lean.getSrcSearchPath`), i.e. `LEAN_SRC_PATH` together with the
   Lean installation's own `src/lean` directory — under `lake exe` / `lake env`, `LEAN_SRC_PATH`
   contains the source directories of every package in the workspace, including dependencies
   (e.g. `.lake/packages/...`);
2. the `.olean` search path (`LEAN_PATH`);
3. the working-directory-relative path (`modToRelFilePath`).

Returns `none` if no source file of `mod` can be found.
-/
def findModuleSource? (mod : Name) : IO (Option FilePath) := do
  if let some path ← Lean.SearchPath.findModuleWithExt (← getSrcSearchPath) "lean" mod then
    return some path
  if let some path ← Lean.SearchPath.findModuleWithExt (← searchPathRef.get) "lean" mod then
    return some path
  let path := modToRelFilePath mod "lean"
  if ← path.pathExists then
    return some path
  return none

open ImportGraph in
/-
Compute the transitive closure of imports starting from a source file.

Returns a `NameSet` of all modules that are transitively imported by the given file,
by recursively parsing source files.

Module source files are located via `findModuleSource?`; in particular, under
`lake exe` / `lake env` the sources of dependency packages are found via `LEAN_SRC_PATH`,
and the toolchain's own modules via the Lean installation's `src/lean` directory.
A module whose source file cannot be found is no longer silently treated as a leaf
(which would return an incomplete closure): all such modules are collected and reported
by throwing an `IO.userError`.

If `rootFilter` is `some root`, only modules with root `root` are traversed and returned.
Note that this prunes the traversal itself rather than filtering the final result:
a `root` module that is only reachable through a non-`root` module will not be found.

**Example:**
```lean
-- Get all transitive Mathlib imports
let imports ← findTransitiveImportsFromSource "Mathlib/Algebra/Ring/Basic.lean" (some `Mathlib)

-- Get all transitive imports regardless of namespace
let allImports ← findTransitiveImportsFromSource "MyFile.lean"
```
-/
def findTransitiveImportsFromSource (startPath : System.FilePath)
    (rootFilter : Option Name := none) : IO NameSet := do
  let mut visited : NameSet := {}
  let mut missing : NameSet := {}
  let mut queue := #[]

  -- Initialize with direct imports from the start file
  for imp in (← startPath.parseImports').filterInit.imports do
    match rootFilter with
    | some root => if imp.module.getRoot == root then queue := queue.push imp.module
    | none => queue := queue.push imp.module

  -- Process queue with DFS
  while h : queue.size > 0 do
    let module := queue.back
    queue := queue.pop

    if visited.contains module then continue
    visited := visited.insert module

    -- Locate the module's source file via the search paths or the working directory;
    -- record it as missing if no source can be found.
    if let some path ← findModuleSource? module then
      for imp in (← path.parseImports').filterInit.imports do
        -- If `rootFilter := none`, `matchesRoot = true`.
        let matchesRoot := rootFilter.elim true (imp.module.getRoot == ·)
        if matchesRoot && !visited.contains imp.module then
            queue := queue.push imp.module
    else
      missing := missing.insert module

  unless missing.toArray.isEmpty do
    throw <| IO.userError <|
      "findTransitiveImportsFromSource: could not find source files for the following modules: " ++
        ", ".intercalate (missing.toArray.qsort Name.lt |>.map toString).toList

  return visited
