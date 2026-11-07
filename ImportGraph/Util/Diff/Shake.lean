module

public import ImportGraph.Util.Diff.Basic
public import ImportGraph.Shake.Algebra

public section

namespace ImportGraph.Diff

open ImportGraph Diff Shake

instance : Diff.Measureable Bitset where
  measure := (·.size)

instance : VennLike Bitset where
  sdiff := (· \ ·)
  inter := (· ∩ ·)
  isEmpty := (·.isEmpty)

/-- Measures the Venn diagram of `Bitset`s at the given `NeedsKind`. -/
def Venn.measureNeedsAt (k : NeedsKind) (needs : Venn Needs) : Venn.Measured :=
  needs.map (·.get k |>.size)

instance : Diff.Measureable Provides where
  measure := (·.flatten.size)

instance : VennLike Provides where
  sdiff := (· \ ·)
  inter a b := a.map₂ b (· ∩ ·) |>.linearize
  isEmpty := (·.isEmpty)
