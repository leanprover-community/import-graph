/-
Copyright (c) 2025 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Thomas R. Murrills
-/
module

public import Lean.Elab.ParseImportsFast

/-!
# Source-File-Based Import Analysis

This module provides functions for analyzing imports by parsing source files directly,
as an alternative to `Environment`-based analysis (e.g. in `ImportGraph.Imports`) Specifically:

- `ImportGraph.parseImports?`, `System.FilePath.parseImports'`: Parse direct imports from a single
  string or file
- `parseCurrentHeader`: parse the imports of the current file from the `FileMap`
- `modToRelFilePath`: like `modToFilePath`, but does not insert a leading file separator
- `findTransitiveImportsFromSource`: Compute a nameset of the transitive closure of imports from
  source files

-- TODO: update?
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

/--
Parse all imports in a source file at `path` and return their module names.

This is a thin wrapper around `Lean.parseImports'` which:
- Reads the file from disk
- Parses the import statements

Note that it does not filter out `Init` modules. See `ModuleHeader.filterInit`.
-/
def System.FilePath.parseImports' (path : System.FilePath) : IO ModuleHeader := do
  Lean.parseImports' (← IO.FS.readFile path) (path.fileName.getD "<input>")

/-- Removes `Init` imports from `ModuleHeader`. -/
def Lean.ModuleHeader.filterInit (m : ModuleHeader) : ModuleHeader :=
  { m with imports := m.imports.filter fun imp => !(`Init).isPrefixOf imp.module }

/--
Parses the header of the current file via the source string present in the ambient `FileMap`.
-/
def parseCurrentHeader {m} [Monad m] [MonadLog m] [MonadLiftT IO m] :
    m (TSyntax `Lean.Parser.Module.header × Parser.ModuleParserState × MessageLog) := do
  Parser.parseHeader (Parser.mkInputContext (← getFileMap).source (← getFileName))

/--
Parse all imports in a source file at `path` and return their module names.

This is a thin wrapper around `Lean.parseImports'` that:
- Reads the file from disk
- Parses the import statements
- Filters out `Init` (part of the prelude)

Note: This only sees syntactic imports in the source file.
It does not account for what declarations are actually used.
-/
@[deprecated "Use `System.FilePath.parseImports'` and `Lean.ModuleHeader.filterInit` instead"
  (since := "2026-07-18")]
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

open ImportGraph in
/--
Compute the transitive closure of imports starting from a source file.

Returns a `NameSet` of all modules that are transitively imported by the given file,
by recursively parsing source files.

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

    -- Convert module name to file path; assume findable from current search ref
    let path := modToRelFilePath module "lean"
    if ← path.pathExists then
      for imp in (← path.parseImports').filterInit.imports do
        -- If `rootFilter := none`, `matchesRoot = true`.
        let matchesRoot := rootFilter.elim true (imp.module.getRoot == ·)
        if matchesRoot && !visited.contains imp.module then
            queue := queue.push imp.module

  return visited
