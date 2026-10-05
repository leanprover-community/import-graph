module

import ImportGraph.Tools
public import ImportGraph.Lean.Syntax
meta import ImportGraph.Tools.MinImports

open ImportGraph Lean

#min_imports

-- Uses `ImportGraph.Lean.Syntax` only privately
def x := Lean.SourceInfo.getLeading

-- We simulate being at the end of the file by running the `runLaterReporter` manually:
/--
warning: Imports can be reduced:
  import ImportGraph.Tools
  p̵u̵b̵l̵i̵c̵ ̵import ImportGraph.Lean.Syntax
  ̵m̵e̵t̵a̵ ̵i̵m̵p̵o̵r̵t̵ ̵I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵T̵o̵o̵l̵s̵.̵M̵i̵n̵I̵m̵p̵o̵r̵t̵s̵
-/
#guard_msgs in
run_cmd
  runLaterReporter.fulfill #[
    ← `(command| open ImportGraph Lean),
    ← `(command| def x := true),
    ← `(command| #min_imports)]
    (runLaterReporter.getRequests (← getEnv))
  runLaterReporter.reset
