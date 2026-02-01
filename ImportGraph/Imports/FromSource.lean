/-
Copyright (c) 2025 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
module

public import Lean.Elab.ParseImportsFast

/-!
# Source-File-Based Import Analysis

This module provides functions for analyzing imports by parsing source files directly,
as an alternative to the Environment-based functions in `ImportGraph.Imports`.

## Functions

- `findImportsFromSource`: Parse direct imports from a single file
- `findTransitiveImportsFromSource`: Compute transitive closure of imports from source files
-/

open Lean System

/--
Parse all imports in a source file at `path` and return their module names.

This is a thin wrapper around `Lean.parseImports'` that:
- Reads the file from disk
- Parses the import statements
- Filters out `Init` (part of the prelude)

Note: This only sees syntactic imports in the source file.
It does not account for what declarations are actually used.
-/
public def findImportsFromSource (path : System.FilePath) : IO (Array Name) := do
  return (← Lean.parseImports' (← IO.FS.readFile path) path.toString).imports
    |>.map (·.module) |>.erase `Init

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
public def findTransitiveImportsFromSource
  (startPath : System.FilePath)
  (rootFilter : Option Name := none)
  : IO NameSet := do
  let mut visited : NameSet := {}
  let mut queue := #[]

  -- Initialize with direct imports from the start file
  for imp in ← findImportsFromSource startPath do
    match rootFilter with
    | some root => if imp.getRoot == root then queue := queue.push imp
    | none => queue := queue.push imp

  -- Process queue with BFS
  while h : queue.size > 0 do
    let module := queue[0]
    queue := queue.eraseIdx 0

    if visited.contains module then continue
    visited := visited.insert module

    -- Convert module name to file path
    let path := System.mkFilePath (module.components.map (·.toString)) |>.addExtension "lean"

    if ← path.pathExists then
      for imp in ← findImportsFromSource path do
        match rootFilter with
        | some root =>
          if imp.getRoot == root && !visited.contains imp then
            queue := queue.push imp
        | none =>
          if !visited.contains imp then
            queue := queue.push imp

  return visited
