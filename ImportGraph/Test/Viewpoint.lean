module

import ImportGraph.Tools.FindHome
import ImportGraph.Test.FakeHome
import ImportGraph.Test.SecondRealHome
import Lean.Data.Json

/-
`bar₁` is from `ImportGraph.Test.ComponentHome1`
`bar₂` is from `ImportGraph.Test.ComponentHome2`

`ImportGraph.Test.RealHome` imports both
So does `ImportGraph.Test.SecondRealHome`
What if we include `foo`
from `ImportGraph.Test.RealHome`?
-/


def y := false

macro "aa" : term => ``(true)

-- /Users/thomas/.elan/toolchains/leanprover--lean4---v4.34.0-rc1/src/lean/Lean/Data/Json.lean

#find_home for
def x' := bar₁ && bar₂ && foo

/-
- [x] finish polishing pipeline: "transport" → read imports → full Model; need baseline!; hybrid approach olean + source?
- [ ] create ranking (packages + libraries) → create message from ranking
  - [ ] exclude current file or...register declarations as coming from current file? This amounts to a **prevs mask**, or iteration only through prevs in the first place. What allows mutation? abstracting games. Not sure about **depths**. Should they have been recorded?
- [ ] handle syntax from the current file correctly?
- [ ] handle case where there are no dependencies from suggested package (upstreaming + current lib)
- [ ] exclude core (and Cli?) from upstream suggestions (exclude all leanprover/*?)
- [ ] exclude root files

- include current file and transitively reachable local files; always okay to put these at the end, depending on the order we choose, if they're not present yet? Right?




- suggestion to add current file to root module...? Easy to hardcode, harder to do other things

The Human Condition - Hannah Arendt
-/



structure IdxOf (a : Array β) where
  toNat : Nat
