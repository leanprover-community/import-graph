import ImportGraph.FromSource

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
  return imports

-- Test transitive imports without filter
/-- info: #[`ImportGraphTest.Unused] -/
#guard_msgs in
#eval do
  let transitive ← findTransitiveImportsFromSource "ImportGraphTest/Used.lean"
  return transitive.toArray.qsort Name.lt

-- Test transitive imports with ImportGraph filter
/-- info: #[] -/
#guard_msgs in
#eval do
  let transitive ← findTransitiveImportsFromSource "ImportGraphTest/Used.lean" (some `ImportGraph)
  return transitive.toArray.qsort Name.lt

-- Test on a file with transitive imports
/-- info: #[`ImportGraph.Meta, `ImportGraphTest.Used] -/
#guard_msgs in
#eval do
  let imports ← findImportsFromSource "ImportGraphTest/FileWithTransitiveImports.lean"
  return imports

/--
info: #[`ImportGraph.Imports, `ImportGraph.Meta, `ImportGraph.RequiredModules, `ImportGraphTest.Unused, `ImportGraphTest.Used,
  `Lean.CoreM, `Lean.Environment, `Lean.MonadEnv, `Lean.Data.NameMap, `Lean.Elab.Command, `Lean.Server.GoTo,
  `Lean.Util.FoldConsts, `Lean.Widget.UserWidget]
-/
#guard_msgs in
#eval do
  let transitive ← findTransitiveImportsFromSource "ImportGraphTest/FileWithTransitiveImports.lean"
  return transitive.toArray.qsort Name.lt
