module
-- The union of ImportGraph imports in Mathlib at the audited snapshot.
public meta import ImportGraph.Tools
public meta import ImportGraph.Imports.Redundant
public meta import ImportGraph.Imports.RequiredModules
public meta import ImportGraph.Lean.Environment
public import ImportGraph.Widget.GoToModule
public meta import ImportGraph.Graph.TransitiveClosure
public meta import ImportGraph.Imports.ImportGraph
meta import ImportGraphTest.PublicExports.Check
run_cmd ImportGraphTest.PublicExports.assertNoProofLeaks #["importGraph"]
