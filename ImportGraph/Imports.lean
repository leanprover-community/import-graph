module

public import ImportGraph.Imports.FromSource
public import ImportGraph.Imports.ImportGraph
public import ImportGraph.Imports.Redundant
public import ImportGraph.Imports.RequiredModules
public import ImportGraph.Imports.Unused
public import Lean

open Lean

-- deprecated 2026-02-01
#eval do
  logWarning "`ImportGraph.Imports` is deprecated! use `import ImportGraph` instead."
