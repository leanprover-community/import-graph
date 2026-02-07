module

public import ImportGraph.Export.DotFile
public import ImportGraph.Export.Gexf
public import ImportGraph.Graph.Filter
public import ImportGraph.Graph.TransitiveClosure
public import ImportGraph.Imports.FromSource
public import ImportGraph.Imports.ImportGraph
public import ImportGraph.Imports.Redundant
public import ImportGraph.Imports.RequiredModules
public import ImportGraph.Imports.Unused
public import ImportGraph.Lean.Environment
public import ImportGraph.Lean.Name
public import ImportGraph.Lean.WithImportModules
public import ImportGraph.Util.FindSorry
public meta import ImportGraph.Export.DotFile
public meta import ImportGraph.Export.Gexf
public meta import ImportGraph.Graph.Filter
public meta import ImportGraph.Graph.TransitiveClosure
public meta import ImportGraph.Imports.FromSource
public meta import ImportGraph.Imports.ImportGraph
public meta import ImportGraph.Imports.Redundant
public meta import ImportGraph.Imports.RequiredModules
public meta import ImportGraph.Imports.Unused
public meta import ImportGraph.Lean.Environment
public meta import ImportGraph.Lean.Name
public meta import ImportGraph.Lean.WithImportModules
public meta import ImportGraph.Util.FindSorry

import Lean

open Lean

-- deprecated 2026-02-01
#eval do
  logWarning "`ImportGraph.Imports` is deprecated! use a subset of`import ImportGraph` instead."
