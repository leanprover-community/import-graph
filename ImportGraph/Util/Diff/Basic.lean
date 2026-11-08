/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public section

open Lean

namespace ImportGraph

namespace Diff

protected class Measurable α where
  measure : α → Nat
export Measurable (measure)

instance : Diff.Measurable Nat := ⟨id⟩

open Diff

/-- If an integer is (strictly) greater than 0, display it with `+`. -/
def Int.toStringWithSign (i : Int) := if i > 0 then s!"+{i}" else s!"{i}"

/-- A simple structure with two states, `initial : α` and `final : α`, which models a transition. -/
structure Transition (α : Type u) where
  /-- The state before `final`. -/
  initial : α
  /-- The state after `initial`. -/
  final : α
deriving Inhabited, Hashable, BEq, Repr

namespace Transition

/-- Apply `f` to both the initial and final state. -/
@[inline] def map (t : Transition α) (f : α → β) : Transition β where
  initial := f t.initial
  final := f t.final

/-- The difference between the measures of the `final` state and `initial` state. -/
@[inline] def delta (t : Transition α) [Diff.Measurable α] : Int :=
  (measure t.final : Int) - measure t.initial

/-- `abbrev` for `Transition Nat`. The result of measuring a `Transition` with
`Transition.measure`. -/
protected abbrev Measured := Transition Nat

@[inline] def measure (t : Transition α) [Diff.Measurable α] : Transition.Measured :=
  t.map Diff.measure

end Transition

/-- A Venn diagram characterizing the changes between some (implicit, unspecified) initial and
final state. -/
structure Venn (α : Type u) where
  /-- What the final state has gained that was not present in the initial state. -/
  plus   : α
  /-- What the initial state has lost that is no longer present in the final state. -/
  minus  : α
  /-- What the initial and final state have in common. -/
  shared : α
deriving Inhabited, Hashable, BEq, Repr

/-- Convert a `Transition α` into a `Venn β` according to the provided notions of set difference
and interesection. Note that this is slightly more general than using a `VennLike α` instance, as
it allows changing the type to `β` and does not require `isEmpty`. -/
@[inline] def Transition.toVennBy (t : Transition α) (sdiff : α → α → β) (inter : α → α → β) :
    Venn β where
  plus := sdiff t.final t.initial
  minus := sdiff t.initial t.final
  shared := inter t.initial t.final

/-- Provides the structure necessary to view `α` as "setlike" enough to support `Venn`, namely a
means of taking the set difference, a means of intersection, and a notion of emptiness. This
typeclass is homogeneous, i.e. we assume that `α` itself is sufficient for representing both states
and the differences/shared aspects between them. -/
class VennLike α where
  /-- A "set difference" such that `VennLike.sdiff a b` tells us what `a` has that `b` does not.  -/
  protected sdiff : α → α → α
  /--
  An "intersection" such that `VennLike.inter a b` tells us what is common to both `a` and `b`.
  -/
  protected inter : α → α → α
  /-- A notion of emptiness. `VennLike.isEmpty <| VennLike.sdiff a b = true` should imply that, for
  the purposes of the Venn diagram, `a` is a "subset" of `b`. (Both directions should indicate
  equality.) Likewise, `VennLike.isEmpty <| VennLike.inter a b` should imply that `a` is disjoint
  from `b`. -/
  protected isEmpty : α → Bool

/-- Convert an `initial` and `final` state into their Venn diagram using the type's `VennLike`
instance. -/
def Transition.toVenn (t : Transition α) [VennLike α] : Venn α :=
  t.toVennBy VennLike.sdiff VennLike.inter

namespace Venn

/-- Apply `f` to each field of `v`. -/
@[inline] def map (v : Venn α) (f : α → β) : Venn β where
  plus := f v.plus
  minus := f v.minus
  shared := f v.shared

/-- A `v : Venn α` represents no change iff both `isEmpty v.plus` and `isEmpty v.minus`. -/
@[inline] def isUnchangedBy (v : Venn α) (isEmpty : α → Bool) :=
  isEmpty v.plus && isEmpty v.minus

/-- A `v : Venn α` represents a change of some kind iff `v.plus` is not `isEmpty`  or\
`v.minus` is not `isEmpty`. -/
@[inline] def isChangedBy (v : Venn α) (isEmpty : α → Bool) := !(v.isUnchangedBy isEmpty)

/-- Whether the `Venn` diagram implicitly represents a strict increase (`.gt`), a strict decrease
(`.lt`), no change (`.eq`), or incomparable final and initial states.  -/
@[inline] def compareBy? (v : Venn α) (isEmpty : α → Bool) : Option Ordering :=
  match isEmpty v.plus, isEmpty v.minus with
  | true,  true  => some .eq
  | true,  false => some .lt
  | false, true  => some .gt
  | false, false => none

/-- The net difference between the implicit initial and final state, collapsing the independent
measurements of `plus` and `minus` together. -/
@[inline] def delta (v : Venn α) [Diff.Measurable α] : Int :=
  (Diff.measure v.plus : Int) - Diff.measure v.minus

/-- A `v : VennMeasure` represents no change iff both `v.plus` and `v.minus` satisfy
`VennLike.isEmpty`. -/
@[inline] def isUnchanged (v : Venn α) [VennLike α] :=
  v.isUnchangedBy VennLike.isEmpty

/-- A `v : VennMeasure` represents a change of some kind iff either `v.plus` is not empty or
`v.minus` is not empty, according to `VennLike.isEmpty`. -/
@[inline] def isChanged (v : Venn α) [VennLike α] :=
  !v.isUnchanged

/-- Whether the `Venn` diagram implicitly represents a strict increase (`.gt`), a strict decrease
(`.lt`), no change (`.eq`), or incomparable final and initial states, according to
`VennLike.isEmpty`. -/
@[inline] def compare? (v : Venn α) [VennLike α] : Option Ordering :=
  v.compareBy? VennLike.isEmpty

/-- Measurements of the parts of a Venn diagram comparing an (implicit, unspecified) initial state
and final state. Abbrev for `Venn Nat`. -/
protected abbrev Measured := Venn Nat

end Diff.Venn
