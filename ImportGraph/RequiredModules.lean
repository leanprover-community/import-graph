module

public import ImportGraph.Imports.RequiredModules
public import Lean

open Lean

-- deprecated 2026-02-01
#eval do
  logWarning "`ImportGraph.RequiredModules` is deprecated! use `import ImportGraph.Imports.RequiredModules` instead."
