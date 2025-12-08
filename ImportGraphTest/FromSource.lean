import ImportGraph.FromSource

/-!
# Tests for Source-Based Import Analysis

Tests for `findImportsFromSource` and `findTransitiveImportsFromSource`.
-/

open Lean System

-- Test basic import parsing
#eval do
  let imports ← findImportsFromSource "ImportGraphTest/Used.lean"
  IO.println s!"Direct imports of Used.lean: {imports}"

-- Test transitive imports without filter
#eval do
  let transitive ← findTransitiveImportsFromSource "ImportGraphTest/Used.lean"
  IO.println s!"Transitive imports of Used.lean (no filter): {transitive.toArray.qsort Name.lt}"

-- Test transitive imports with ImportGraph filter
#eval do
  let transitive ← findTransitiveImportsFromSource "ImportGraphTest/Used.lean" (some `ImportGraph)
  IO.println s!"Transitive ImportGraph imports of Used.lean: {transitive.toArray.qsort Name.lt}"

-- Test on a file with transitive imports
#eval do
  let imports ← findImportsFromSource "ImportGraphTest/FileWithTransitiveImports.lean"
  IO.println s!"Direct imports of FileWithTransitiveImports.lean: {imports}"

#eval do
  let transitive ← findTransitiveImportsFromSource "ImportGraphTest/FileWithTransitiveImports.lean"
  IO.println s!"Transitive imports of FileWithTransitiveImports.lean: {transitive.toArray.qsort Name.lt}"
