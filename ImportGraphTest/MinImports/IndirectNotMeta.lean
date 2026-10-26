module

import ImportGraph.Tools.MinImports
import ImportGraphTest.MinImports.Indirect.Attr

/-!
Ensure that we do not accidentally demand `foo` at the meta phase just because it has an indirect
mod use demanding `ImportGraphTest.MinImports.Indirect.Attr`.
-/

open ImportGraph Shake Lean

theorem useFoo : 1 + 0 = 1 := foo 1

#min_imports

/-- info: Imports are minimal. -/
#guard_msgs in
run_cmd
  logAllMinImports #[
    ← `(command| open ImportGraph Shake Lean),
    ← `(command| theorem useFoo : 1 + 0 = 1 := foo 1),
    ← `(command| #min_imports)]
  resetMinImports
