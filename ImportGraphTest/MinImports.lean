module

import ImportGraph.Tools
public import ImportGraph.Lean.Syntax
meta import ImportGraph.Tools.MinImports

#min_imports

-- Uses `ImportGraph.Lean.Syntax` only privately
def x := Lean.SourceInfo.getLeading

-- We simulate being at the end of the file by inspecting and running all of the `runReporter`'s individually:
open Lean
/--
warning: Imports can be reduced:
  import ImportGraph.Tools
  p̵u̵b̵l̵i̵c̵ ̵import ImportGraph.Lean.Syntax
  ̵m̵e̵t̵a̵ ̵i̵m̵p̵o̵r̵t̵ ̵I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵T̵o̵o̵l̵s̵.̵M̵i̵n̵I̵m̵p̵o̵r̵t̵s̵
-/
#guard_msgs in
run_cmd
  let runLaterRequests := ImportGraph.runLaterReporter.ext.getState (← getEnv)
  for request in runLaterRequests do
    request.data #[← `(command| #min_imports)]
    request.stopProgressIndicator

/- Prevent the request from the original `#min_imports` from reaching the end of the file (which
would be noisy) -/
run_cmd
  modifyEnv fun env => ImportGraph.runLaterReporter.ext.setState env #[]
