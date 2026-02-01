module

public meta import ImportGraph.Imports.FromSource
public import Lean

open Lean

-- deprecated 2026-02-01
#eval do
  logWarning "`ImportGraph.FromSource` is deprecated! use `import ImportGraph.Imports.FromSource` instead."
  pure ()
