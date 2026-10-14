import ImportGraph.Imports.FromSource
import Lean.Elab.Command

/-!
# Tests for Source-Based Import Analysis

Tests for `findImportsFromSource` and `findTransitiveImportsFromSource`.
-/

open Lean System ImportGraph

-- Test basic import parsing
/-- info: #[`ImportGraphTest.Unused] -/
#guard_msgs in
#eval do
  let header ← System.FilePath.parseImports' ("ImportGraphTest" / "Used.lean")
  -- Filter to only ImportGraph modules
  return header.imports.map Import.module |>.filter
    fun (n : Name) => n.getRoot ∈ [`ImportGraph, `ImportGraphTest]

-- Test transitive imports without filter
/-- info: #[`ImportGraphTest.Unused] -/
#guard_msgs in
#eval do
  let transitive ← findTransitiveImportsFromSource "ImportGraphTest/Used.lean"
  -- Filter to only ImportGraph modules
  let filtered := transitive.toArray.filter (fun (n : Name) => n.getRoot ∈ [`ImportGraph, `ImportGraphTest])
  return filtered.qsort Name.lt

-- Test transitive imports with ImportGraph filter
/-- info: #[] -/
#guard_msgs in
#eval do
  let transitive ← findTransitiveImportsFromSource "ImportGraphTest/Used.lean" (some `ImportGraph)
  return transitive.toArray.qsort Name.lt

-- Test on a file with transitive imports
/-- info: #[`ImportGraph.Tools.ImportDiff, `ImportGraphTest.Used] -/
#guard_msgs in
#eval do
  let header ← System.FilePath.parseImports' ("ImportGraphTest" / "FileWithTransitiveImports.lean")
  -- Filter to only ImportGraph modules
  return header.imports.map Import.module |>.filter
    fun (n : Name) => n.getRoot ∈ [`ImportGraph, `ImportGraphTest]

/-
info: #[`ImportGraphTest.Unused, `ImportGraphTest.Used, `ImportGraph.Imports.ImportGraph, `ImportGraph.Tools.ImportDiff]
-/
#guard_msgs in
#eval do
  let transitive ← findTransitiveImportsFromSource "ImportGraphTest/FileWithTransitiveImports.lean"
  -- Filter to only ImportGraph modules
  let filtered := transitive.toArray.filter (fun (n : Name) => n.getRoot ∈ [`ImportGraph, `ImportGraphTest])
  return filtered.qsort Name.lt

-- Test that `findModuleSource?` locates a toolchain module via the source search path
/-- info: true -/
#guard_msgs in
#eval do
  return (← findModuleSource? `Lean.Parser.Module).isSome

-- Test that `findModuleSource?` returns `none` for a module with no source anywhere
/-- info: true -/
#guard_msgs in
#eval do
  return (← findModuleSource? `ThisModuleDoesNotExist).isNone

-- Test that the transitive closure extends beyond a file's direct imports:
-- `Lean.Parser.Module` is not a direct import of `FromSource.lean`, but is reachable
-- by resolving its direct import `Lean.Elab.ParseImportsFast` via the source search path.
/-- info: true -/
#guard_msgs in
#eval do
  let transitive ← findTransitiveImportsFromSource "ImportGraph/Imports/FromSource.lean"
  return transitive.contains `Lean.Parser.Module

-- Test that unresolvable modules are reported rather than silently treated as leaves
/-
info: "findTransitiveImportsFromSource: could not find source files for the following modules: ThisModuleDoesNotExist"
-/
#guard_msgs in
#eval do
  let fixture : FilePath := ".lake" / "build" / "findTransitiveImportsFromSource-missing-fixture.lean"
  IO.FS.writeFile fixture "import ThisModuleDoesNotExist\n"
  try
    let _ ← findTransitiveImportsFromSource fixture
    return "no error raised!"
  catch e =>
    return e.toString
