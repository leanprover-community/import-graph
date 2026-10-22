module

import ImportGraph.Tools.MinImports
import ImportGraphTest.MinImports.NewSyntax

open ImportGraph Shake

#foo

#min_imports

-- Captures `#foo` through Lean's extra mod use recording during elaboration
/-- info: Imports are minimal. -/
#guard_msgs in
run_cmd logAllMinImports #[]; resetMinImports
