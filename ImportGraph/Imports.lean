module

public import ImportGraph
public meta import ImportGraph

open Lean

-- deprecated 2026-02-01
#eval do
  logWarning "`ImportGraph.Imports` is deprecated! use `import ImportGraph` instead."
