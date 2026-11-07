module

public import ImportGraph.Util.TableMessage

public section

open Lean

namespace ImportGraph

namespace Diff

protected class Measureable α where
  measure : α → Nat
export Measureable (measure)

instance : Diff.Measureable Nat := ⟨id⟩

open Diff

/-- If an integer is (strictly) greater than 0, display it with `+`. -/
def Int.toStringWithSign (i : Int) := if i > 0 then s!"+{i}" else s!"{i}"

/-- A simple structure with two states, `initial : α` and `final : α`, which models a transition. -/
structure Transition α where
  /-- The state before `final`. -/
  initial : α
  /-- The state after `initial`. -/
  final : α

namespace Transition

@[inline] def map (t : Transition α) (f : α → β) : Transition β where
  initial := f t.initial
  final := f t.final

@[inline] def deltaBy (t : Transition α) (f : α → α → β) : β := f t.final t.initial

@[inline] def delta (t : Transition α) [Diff.Measureable α] : Int :=
  (Diff.measure t.final : Int) - Diff.measure t.initial

end Transition

/-- A `Transition` with an `initial : Nat` and a `final : Nat`. -/
abbrev TransitionMeasure := Transition Nat

@[inline] def Transition.measure (t : Transition α) [Diff.Measureable α] : TransitionMeasure :=
  t.map Diff.measure

namespace TransitionMeasure

def header : Table.Row 4 := #v["", "Δ", "new", "old"]

def toRow (t : TransitionMeasure) (rowHeader : String) : Table.Row 4 :=
  #v[rowHeader, t.delta.toStringWithSign, s!"{t.final}", s!"{t.initial}"]

end TransitionMeasure

/-- A Venn diagram characterizing the changes between some (implicit, unspecified) initial and
final state. -/
structure Venn α where
  /-- What the final state has gained that was not present in the initial state. -/
  plus   : α
  /-- What the initial state has lost that is no longer present in the final state. -/
  minus  : α
  /-- What the initial and final state have in common. -/
  shared : α

@[inline] def Transition.toVennBy (t : Transition α) (sdiff : α → α → β) (inter : α → α → β) :
    Venn β where
  plus := sdiff t.final t.initial
  minus := sdiff t.initial t.final
  shared := inter t.initial t.final

class HVennLike α β where
  protected sdiff : α → α → β
  protected inter : α → α → β
  -- TODO: isEmpty field? isDisjoint?

abbrev VennLike α := HVennLike α α

def Transition.toVenn (t : Transition α) [HVennLike α β] : Venn β :=
  t.toVennBy HVennLike.sdiff HVennLike.inter

namespace Venn

@[inline] def map (v : Venn α) (f : α → β) : Venn β where
  plus := f v.plus
  minus := f v.minus
  shared := f v.shared

/-- A `v : VennMeasure` represents no change iff `v.plus = v.minus = 0`. -/
@[inline] def isUnchangedBy (v : Venn α) (isEmpty : α → Bool) :=
  isEmpty v.plus && isEmpty v.minus

/-- A `v : VennMeasure` represents a change of some kind iff `v.plus` is nonzero or `v.minus` is
nonzero. -/
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
@[inline] def delta (v : Venn α) [Diff.Measureable α] : Int :=
  (Diff.measure v.plus : Int) - Diff.measure v.minus

/-- A `v : VennMeasure` represents no change iff `v.plus = v.minus = 0`. -/
@[inline] def isUnchanged (v : Venn α) [Diff.Measureable α] :=
  v.isUnchangedBy (Diff.measure · = 0)

/-- A `v : VennMeasure` represents a change of some kind iff `v.plus` is nonzero or `v.minus` is
nonzero. -/
@[inline] def isChanged (v : Venn α) [Diff.Measureable α] :=
  !v.isUnchanged

@[inline] def compare? (v : Venn α) [Diff.Measureable α] : Option Ordering :=
  v.compareBy? (Diff.measure · = 0)

end Venn

/-- Measurements of the parts of a Venn diagram comparing an (implicit, unspecified) initial state
and final state. -/
abbrev VennMeasure := Venn Nat

namespace VennMeasure

open ImportGraph Diff

/-- A header row for a table consisting of rows created by `VennMeasure.toRow`. -/
def header : Table.Row 5 := #v["", "Δ", "+", "-", "∩"]

/-- A row in a table for a single `VennMeasure` which follows the order in `VennMeasure.header`. -/
def toRow (rowHeader : String) (t : VennMeasure) : Table.Row 5 := #v[
  rowHeader,
  t.delta.toStringWithSign,
  s!"+{t.plus}",
  s!"-{t.minus}",
  s!"{t.shared}"]

end Diff.VennMeasure
