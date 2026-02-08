import ImportGraph.Imports.FromSource

/-!
# Tests for Source-Based Import Analysis

Tests for `findImportsFromSource` and `findTransitiveImportsFromSource`.
-/

open Lean System

-- Test basic import parsing
/-- info: #[`ImportGraphTest.Unused] -/
#guard_msgs in
#eval do
  let imports ← findImportsFromSource "ImportGraphTest/Used.lean"
  -- Filter to only ImportGraph modules
  return imports.filter (fun (n : Name) => n.getRoot ∈ [`ImportGraph, `ImportGraphTest])

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
  let imports ← findImportsFromSource "ImportGraphTest/FileWithTransitiveImports.lean"
  -- Filter to only ImportGraph modules
  return imports.filter (fun (n : Name) => n.getRoot ∈ [`ImportGraph, `ImportGraphTest])

/--
info: #[`ImportGraphTest.Unused, `ImportGraphTest.Used, `ImportGraph.Imports.ImportGraph, `ImportGraph.Tools.ImportDiff]
-/
#guard_msgs in
#eval do
  let transitive ← findTransitiveImportsFromSource "ImportGraphTest/FileWithTransitiveImports.lean"
  -- Filter to only ImportGraph modules
  let filtered := transitive.toArray.filter (fun (n : Name) => n.getRoot ∈ [`ImportGraph, `ImportGraphTest])
  return filtered.qsort Name.lt
