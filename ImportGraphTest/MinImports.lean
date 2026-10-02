module

public import ImportGraph.Lean.Syntax
import ImportGraph.Tools.MinImports

#min_imports

-- Uses `ImportGraph.Lean.Syntax` only privately
def x := Lean.SourceInfo.getLeading

-- We simulate being at the end of the file by inspecting and running all of the `runReporter`'s individually:
open Lean
/--
@ +0:0...12
warning: Imports can be reduced:
  p̵u̵b̵l̵i̵c̵ ̵import ImportGraph.Lean.Syntax
  import ImportGraph.Tools.MinImports
-/
#guard_msgs (positions := true) in
run_cmd
  let runLaterRequests := ImportGraph.runLaterReporter.ext.getState (← getEnv)
  for request in runLaterRequests do
    request.data #[← `(command| #min_imports)]
    request.stopProgressIndicator

/- Prevent the request from the original `#min_imports` from reaching the end of the file (which
would be noisy) -/
run_cmd
  modifyEnv fun env => ImportGraph.runLaterReporter.ext.setState env #[]
