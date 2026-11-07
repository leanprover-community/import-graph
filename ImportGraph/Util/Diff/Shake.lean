/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
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
