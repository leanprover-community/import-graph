module

import ImportGraph.Tools.MinImports
import ImportGraphTest.MinImports.NewSyntax

open ImportGraph Shake Lean

#foo

def x := true

#min_imports

/-- info: Imports are minimal. -/
#guard_msgs in
run_cmd
  logAllMinImports #[
    ← `(command| open ImportGraph Lean),
    ← `(command| #foo),
    ← `(command| def x := true),
    ← `(command| #min_imports)]
  resetMinImports
