module

public import ImportGraph.Util.Diff.Basic
public import ImportGraph.Shake.Basic

open ImportGraph Diff Shake

instance : Diff.Measureable Bitset where
  measure b := b.size

instance : VennLike Provides Provides where


def ofBitsetTransition (t : Transition Bitset) (ignoredShared : Bitset := ∅) : Venn Bitset :=
  t.toVennBy (sdiff := (· \ ·)) (inter := fun a b => (a ∩ b) \ ignoredShared)

def ofProvidesTransition (t : Transition Provides) (ignoredShared : Provides := ∅) :
    Venn Provides :=
  t.toVennBy
    (sdiff := (· \ ·))
    (inter := fun a b => (a.map₂ b (· ∩ ·)) \ ignoredShared |>.linearize)

@[inline] def Venn.measureBitset (v : Venn Bitset) : VennMeasure := v.map (·.size)

def Venn.measureNeedsAt (k : NeedsKind) (needs : Venn Needs) : VennMeasure :=
  needs.map (·.get k |>.size)
