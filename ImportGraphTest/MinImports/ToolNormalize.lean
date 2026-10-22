module

import ImportGraph.Lean.Syntax
import ImportGraph.Tools
meta import ImportGraph.Tools.NormImports

open ImportGraph Shake Lean

def x := Lean.Syntax.getLeadingPos?

#min_imports

/--
warning: Imports are minimal, but can be normalized:
  import ImportGraph.Lean.Syntax
  import ImportGraph.Tools
  ̵m̵e̵t̵a̵ ̵i̵m̵p̵o̵r̵t̵ ̵I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵T̵o̵o̵l̵s̵.̵N̵o̵r̵m̵I̵m̵p̵o̵r̵t̵s̵
-/
#guard_msgs in
run_cmd
  logAllMinImports #[]
  resetMinImports
