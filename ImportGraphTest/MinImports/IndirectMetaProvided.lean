module

import ImportGraph.Tools.MinImports
import ImportGraphTest.MinImports.Indirect.MetaAttr

/-!
Here `Indirect.Attr`, which imposes an indirect mod use on `foo`, is only available via
`MetaAttr`'s `public meta import`. That indirect use must not cause `#min_imports` to suggest a
new non-meta `import ImportGraphTest.MinImports.Indirect.Attr`.
-/

open ImportGraph Shake Lean

theorem useFoo : bar + 0 = bar := foo bar

#min_imports

-- The indirect need on `Indirect.Attr` (which allows meta) used to be turned into a non-meta need,
-- and would insert an incorrect import.
/-- info: Imports are minimal. -/
#guard_msgs in
run_cmd
  logAllMinImports #[
    ← `(command| open ImportGraph Shake Lean),
    ← `(command| theorem useFoo : bar + 0 = bar := foo bar),
    ← `(command| #min_imports)]
  resetMinImports
