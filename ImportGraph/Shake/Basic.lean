/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.Shake.Core

/-!
## Utilities for Shake types

This file provides basic API for `Needs`, `NeedsKind`, and `Bitset`.
-/

open Lean ImportGraph Shake

public section

-- TODO: consider alternative implementations?
/-- Counts the number of set bits in the binary representation of `n`. -/
def ImportGraph.Nat.numSetBits (n : Nat) : Nat := Id.run do
  let mut n := n
  let mut acc := 0
  while n != 0 do
    n := n &&& (n - 1) -- clears the lowest set bit
    acc := acc + 1
  return acc

namespace ImportGraph.Shake

namespace Bitset

/-- The number of set bits in a `Bitset`. -/
@[inline] def size (s : Bitset) : Nat := s.toNat.numSetBits

/-- Whether the `Bitset` contains any elements. -/
@[inline] def isEmpty (s : Bitset) : Bool := s.toNat == 0

/-- The minimum size of the ambient index set necessary to hold `Bitset`.  -/
@[inline] def univSize (s : Bitset) : Nat := if s.isEmpty then 0 else s.toNat.log2 + 1

/-- The full set `{0, ..., n - 1}`. -/
@[inline] def univ (n : Nat) : Bitset := ⟨(1 <<< n) - 1⟩

/-- The set `s` without the element `i`. -/
@[inline] def erase (s : Bitset) (i : Nat) : Bitset :=
  if s.has i then s ^^^ {i} else s

/-- The highest set bit of a `Bitset`, if there is one. -/
@[inline] def max? (s : Bitset) : Option Nat :=
  if s.isEmpty then none else s.toNat.log2
/-- The lowest set bit of a `Bitset`, if there is one. -/
@[inline] def min? (s : Bitset) : Option Nat :=
  if s.isEmpty then none else s.toNat ^^^ (s.toNat &&& (s.toNat - 1)) |>.log2

-- TODO: notation? use `∩`?
/-- `a.le b` iff every bit set in `a` is also set in `b`. -/
@[inline] def le (a b : Bitset) : Bool := a.toNat &&& b.toNat == a.toNat
/-- `a.le b` iff every bit set in `a` is also set in `b`, and `b` is not equal to `a`. -/
@[inline] def lt (a b : Bitset) : Bool := a.le b && a != b

instance : HasSubset Bitset where
  Subset a b := a.le b

instance : DecidableRel (α := Bitset) (β := Bitset) Subset
  | a, b => if h : a.le b then .isTrue h else .isFalse h

instance : SDiff Bitset where
  sdiff a b := { toNat := a.toNat &&& (a.toNat ^^^ b.toNat) }

/-- Fold over the set bit indices in a `Bitset` from high to low. This is more efficient for large
bitsets than `Bitset.foldl`.-/
@[specialize]
def foldr (s : Bitset) (init : α) (f : α → Nat → α) : α := Id.run do
  let mut n := s.toNat
  let mut acc := init
  while n != 0 do
    let i := n.log2 -- highest bit idx
    acc := f acc i
    n := n ^^^ (1 <<< i)
  return acc

/-- Fold over the set bit indices in a `Bitset` from low to high. This is less efficient for large
bitsets than `Bitset.foldr`.-/
@[specialize]
def foldl (s : Bitset) (init : α) (f : α → Nat → α) : α := Id.run do
  let mut n := s.toNat
  let mut acc := init
  while n != 0 do
    let cleared := n &&& (n - 1) -- clears the lowest set bit
    let i := (n ^^^ cleared).log2 -- index of the lowest set bit
    acc := f acc i
    n := cleared
  return acc

/-! ## Array -/

/-- Regarding `idxs : Array Nat` as a set of `Bitset` indices, create a bitset which contains each
`idx ∈ idxs`. -/
@[inline] def ofArray (idxs : Array Nat) : Bitset := idxs.foldl (init := ∅) (flip insert)

-- TODO: test: is `s.size` cheap enough and array resizing costly enough to warrant
-- `(init := .emptyWithCapacity s.size)`?
/-- The set bit indices of a `Bitset` in order from low to high. -/
@[inline] def toIdxs (s : Bitset) : Array Nat := s.foldl #[] (·.push ·)

/-- The set bit indices of a `Bitset` in order from high to low. -/
@[inline] def toRevIdxs (s : Bitset) : Array Nat := s.foldr #[] (·.push ·)

/-! ## ForIn -/

/-- Iterates through the set bit indices of a `Bitset` from high to low. -/
@[specialize] protected def forInRev {m} [Monad m] {β} (s : Bitset) (init : β)
    (f : Nat → β → m (ForInStep β)) : m (ForInStep β) := do
  let mut n := s.toNat
  let mut acc := init
  while n != 0 do
    let i := n.log2
    match ← f i acc with
    | d@(.done _) => return d
    | .yield b => acc := b
    n := n ^^^ (1 <<< i)
  return .yield acc

/-- A `Bitset` with a `ForIn` instance that traverses the bitset's indices from the highest index
to the lowest. This is the most efficient traversal of a bitset for large bitsets. -/
structure HighToLow where
  toBitset : Bitset
deriving Repr, Inhabited

@[inline, inherit_doc HighToLow]
def highToLow (b : Bitset) : HighToLow := ⟨b⟩

instance {m} [Monad m] : ForIn m HighToLow Nat where
  forIn br b f := ForInStep.value <$> br.toBitset.forInRev b f

@[specialize]
protected def forIn {m} [Monad m] {β} (s : Bitset) (init : β)
    (f : Nat → β → m (ForInStep β)) : m (ForInStep β) := do
  let mut n := s.toNat
  let mut acc := init
  while n != 0 do
    let cleared := n &&& (n - 1) -- clears the lowest set bit
    let i := (n ^^^ cleared).log2 -- index of the lowest set bit
    match ← f i acc with
    | d@(.done _) => return d
    | .yield b => acc := b
    n := cleared
  return .yield acc

/-- A `Bitset` with a `ForIn` instance that traverses the bitset's indices from the highest index
to the lowest. `Bitset.HighToLow` (`Bitset.highToLow`) is more efficient for large `Bitset`s. -/
structure LowToHigh where
  toBitset : Bitset
deriving Repr, Inhabited

@[inline, inherit_doc LowToHigh]
def lowToHigh (b : Bitset) : LowToHigh := ⟨b⟩

instance {m} [Monad m] : ForIn m LowToHigh Nat where
  forIn b init f := ForInStep.value <$> b.toBitset.forIn init f

/-! ## Representations -/

deriving instance ToJson, FromJson for Bitset

/-- Converts a `Bitset` to a string of the form `"◻◼◻◻◻◼◻◼◼◻◻◼"`, where indices run left-to-right
from `0` and `◻` represents absence at that index, while `◼` represents presence.

By default, this shows only as many squares as necessary (or `"◻"` for an empty bitset), and so in
the nonempty case the last square will always be `◼`. Instead, `univSize? : Option Nat` can be
provided to set the total number of indices, and will either truncate (even if higher bits are set)
or pad with `◻` as appropriate. -/
def toString (b : Bitset) (univSize? : Option Nat := none) : String := Id.run do
  let mut chars := []
  let b := if let some univSize := univSize? then b ∩ univ univSize else b
  let mut lastIdx := univSize?.getD b.univSize
  -- Build the string character-by-character in reverse order
  for i in b.highToLow do
    for _ in i<...lastIdx do
      chars := '◻' :: chars
    chars := '◼' :: chars
    lastIdx := i
  for _ in 0...lastIdx do
    chars := '◻' :: chars
  if chars.isEmpty && !univSize?.isEqSome 0 then
    chars := '◻' :: chars
  return .ofList chars

instance : ToString Bitset where
  toString b := b.toString

end Bitset

namespace Needs

/-! ## `ForIn` -/

/-- Iterates through `NeedsKinds.all`, and for each `NeedsKind`, iterates through the set bit
indices of the corresponding `Bitset` from highest to lowest. -/
@[specialize] protected def forInRev {m} [Monad m] {β : Type} (n : Needs) (init : β)
    (f : (NeedsKind × Nat) → β → m (ForInStep β)) : m (ForInStep β) := do
  let mut acc := init
  for k in NeedsKind.all do
    match ← (n.get k).forInRev acc fun i b => f (k, i) b with
    | d@(.done _) => return d
    | .yield b => acc := b
  return .yield acc

/-- A `Needs` with a `ForIn` instance that traverses the component bitset's indices from the
highest index to the lowest. This is the most efficient traversal of a bitset. Does not actually
reverse the indices in the bitsets. -/
structure HighToLow where
  toNeeds : Needs

@[inherit_doc HighToLow]
def highToLow (b : Needs) : HighToLow := ⟨b⟩

instance {m} [Monad m] : ForIn m HighToLow (NeedsKind × Nat) where
  forIn needs init f := ForInStep.value <$> needs.toNeeds.forInRev init f

/-- Iterates through `NeedsKinds.all`, and for each `NeedsKind`, iterates through the set bit
indices of the corresponding `Bitset` from lowest to highest. -/
@[specialize] protected def forIn {m} [Monad m] {β : Type} (n : Needs) (init : β)
    (f : (NeedsKind × Nat) → β → m (ForInStep β)) : m (ForInStep β) := do
  let mut acc := init
  for k in NeedsKind.all do
    match ← (n.get k).forIn acc fun i b => f (k, i) b with
    | d@(.done _) => return d
    | .yield b => acc := b
  return .yield acc

/-- A `Needs` with a `ForIn` instance that traverses the component bitset's indices from the
lowest index to the highest. `b.highToLow` is more efficient. -/
structure LowToHigh where
  toNeeds : Needs

@[inherit_doc LowToHigh]
def lowToHigh (b : Needs) : LowToHigh := ⟨b⟩

instance {m} [Monad m] : ForIn m LowToHigh (NeedsKind × Nat) where
  forIn needs init f := ForInStep.value <$> needs.toNeeds.forIn init f

/-! ## Operations -/

deriving instance BEq for Needs

/-- Includes `i` in the field of `Needs` corresponding to `k`. -/
def single (i : Nat) (k : NeedsKind) : Needs := empty.set k {i}

/-- Apply `f` to the component bitset of `n : Needs` at the given `NeedsKind`.
Rephrasing of `f (n.get k)` for readability. -/
@[inline] def applyAt (n : Needs) (k : NeedsKind) (f : Bitset → β) : β := f <| n.get k

/-- Whether `f : Bitset → Bool` is `true` for any of the component bitsets of the given `Needs`. -/
@[inline] def any (n : Needs) (f : Bitset → Bool) : Bool := NeedsKind.all.any (n.applyAt · f)
/-- Whether `f : NeedsKind → Bitset → Bool` is `true` for any of the component bitsets of the given
`Needs` at the bitset's corresponding `NeedsKind`. -/
@[inline] def anyWithKind (n : Needs) (f : NeedsKind → Bitset → Bool) : Bool :=
  NeedsKind.all.any fun k => n.applyAt k (f k)

/-- Whether `f : Bitset → Bool` is `true` for all of the component bitsets of the given `Needs`. -/
@[inline] def all (n : Needs) (f : Bitset → Bool) : Bool := NeedsKind.all.all (n.applyAt · f)
/-- Whether `f : NeedsKind → Bitset → Bool` is `true` for any of the component bitsets of the given
`Needs` at the bitset's corresponding `NeedsKind`. -/
@[inline] def allWithKind (n : Needs) (f : NeedsKind → Bitset → Bool) : Bool :=
  NeedsKind.all.all fun k => n.applyAt k (f k)

/-- Folds `f` over the bitsets in `Needs` (in the order of `NeedsKind.all`). -/
@[inline] def fold (n : Needs) (f : α → Bitset → α) (init : α) : α :=
  NeedsKind.all.foldl (init := init) fun a k => n.applyAt k (f a)
/-- Folds `f` over the bitsets in `Needs` at their corresponding `NeedsKind`s (in the order of
`NeedsKind.all`). -/
@[inline] def foldWithKind (n : Needs) (f : α → NeedsKind → Bitset → α) (init : α) : α :=
  NeedsKind.all.foldl (init := init) fun a k => n.applyAt k (f a k)

/-- Applies `f` to all component `Bitset`s of the given `Needs`. -/
@[specialize f] def map (n : Needs) (f : Bitset → Bitset) : Needs where
  pub      := f n.pub
  priv     := f n.priv
  metaPub  := f n.metaPub
  metaPriv := f n.metaPriv
  privOfPriv     := f n.privOfPriv
  metaPrivOfPriv := f n.metaPrivOfPriv

/-- Applies `f` to all component `Bitset`s of the given `Needs` at their corresponding
`NeedsKind`s. -/
@[specialize f] def mapWithKind (n : Needs) (f : NeedsKind → Bitset → Bitset) : Needs where
  pub      := f .pub n.pub
  priv     := f .priv n.priv
  metaPub  := f .metaPub n.metaPub
  metaPriv := f .metaPriv n.metaPriv
  privOfPriv     := f .privOfPriv n.privOfPriv
  metaPrivOfPriv := f .metaPrivOfPriv n.metaPrivOfPriv

/-- Applies `f` "pointwise" to the pairs of bitsets at each given `NeedsKind`.
`f (n₁.get k) (n₂.get k) = (n₁.map₂ n₂ f).get k` for all `k : NeedsKind`. -/
@[specialize f] def map₂ (n₁ n₂ : Needs) (f : Bitset → Bitset → Bitset) : Needs where
  pub      := f n₁.pub n₂.pub
  priv     := f n₁.priv n₂.priv
  metaPub  := f n₁.metaPub n₂.metaPub
  metaPriv := f n₁.metaPriv n₂.metaPriv
  privOfPriv     := f n₁.privOfPriv n₂.privOfPriv
  metaPrivOfPriv := f n₁.metaPrivOfPriv n₂.metaPrivOfPriv

/-- Applies `f` "pointwise" to the pairs of bitsets at each given `NeedsKind`.
`f k (n₁.get k) (n₂.get k) = (n₁.mapWithKind₂ n₂ f).get k` for all `k : NeedsKind`. -/
@[specialize f] def mapWithKind₂ (n₁ n₂ : Needs)
    (f : NeedsKind → Bitset → Bitset → Bitset) : Needs where
  pub      := f .pub n₁.pub n₂.pub
  priv     := f .priv n₁.priv n₂.priv
  metaPub  := f .metaPub n₁.metaPub n₂.metaPub
  metaPriv := f .metaPriv n₁.metaPriv n₂.metaPriv
  privOfPriv     := f .privOfPriv n₁.privOfPriv n₂.privOfPriv
  metaPrivOfPriv := f .metaPrivOfPriv n₁.metaPrivOfPriv n₂.metaPrivOfPriv

/-- Whether all component bitsets are empty. -/
@[inline] def isEmpty (n : Needs) : Bool := n.all (·.isEmpty)
/-- Whether the component bitset at `k : NeedsKind` is empty. -/
@[inline] def isEmptyAt (k : NeedsKind) (n : Needs) : Bool := n.get k |>.isEmpty

instance : SDiff Needs where
  sdiff a b := a.map₂ b (· \ ·)

/- Note: we run this a lot, so implement it directly and ensure it stays up-to-date with
`NeedsKind.all` via proof. -/
/-- Whether each field of the first `Needs` is contained within the corresponding field of the
second. Use with caution: this does not necessarily indicate that one `Needs` subsumes another. See
also `Needs.coveredBy` for testing against an import hierarchy. -/
@[inline] def directLe (n m : Needs) : Bool :=
  n.pub.le m.pub &&
  n.priv.le m.priv &&
  n.metaPub.le m.metaPub &&
  n.metaPriv.le m.metaPriv &&
  n.privOfPriv.le m.privOfPriv &&
  n.metaPrivOfPriv.le m.metaPrivOfPriv

theorem directLe_eq_allWithKind_le :
    directLe = fun n m => n.allWithKind fun k nb => nb.le <| m.get k := by
  ext; simp [directLe, allWithKind, applyAt, NeedsKind.all, get, Bool.and_assoc]

end Needs

namespace NeedsKind

instance : ToString NeedsKind where
  toString
    | .pub      => "public"
    | .metaPub  => "public meta"
    | .priv     => "private"
    | .metaPriv => "private meta"
    | .privOfPriv     => "all"
    | .metaPrivOfPriv => "meta all"

/-- The `NeedsKind`s which land (directly) in the private scope. -/
@[inline, expose, grind] def toPrivate   : Array NeedsKind :=
  #[.priv, .privOfPriv, .metaPriv, .metaPrivOfPriv]
/-- The `NeedsKind`s which land (directly) in the public scope. -/
@[inline, expose, grind] def toPublic    : Array NeedsKind := #[.pub, .metaPub]
/-- The `NeedsKind`s which (directly) demand the private scope. -/
@[inline, expose, grind] def fromPrivate : Array NeedsKind := #[.privOfPriv, .metaPrivOfPriv]
/-- The `NeedsKind`s which (directly) demand the public scope. -/
@[inline, expose, grind] def fromPublic  : Array NeedsKind := #[.priv, .pub, .metaPriv, .metaPub]

/-- The `NeedsKind`s which land (directly) in the given scope. -/
@[inline, expose] def to (vis : Environment.Visibility) : Array NeedsKind :=
  match vis with
  | .public  => toPublic
  | .private => toPrivate
/-- The `NeedsKind`s which (directly) demand the given scope. -/
@[inline, expose] def «from» (vis : Environment.Visibility) : Array NeedsKind :=
  match vis with
  | .public  => fromPublic
  | .private => fromPrivate

/-- The `NeedsKind`s which land in the private scope after taking into account `public` ⊆ `private`
on both ends. This is simply `NeedsKind.all`, but can help record why we're using it. -/
@[inline, expose, grind] def toPrivateTrans   : Array NeedsKind := NeedsKind.all
/-- The `NeedsKind`s which land in the public scope after taking into account `public` ⊆ `private`
on both ends. This is simply `toPublic`, but can help record why we're using it. -/
@[inline, expose, grind] def toPublicTrans    : Array NeedsKind := toPublic
/-- The `NeedsKind`s which demand the private scope after taking into account `public` ⊆ `private`
on both ends. -/
@[inline, expose, grind] def fromPrivateTrans : Array NeedsKind := fromPrivate
/-- The `NeedsKind`s which demand the public scope after taking into account `public` ⊆ `private`
on both ends. This is simply `NeedsKind.all`, but can help record why we're using it. -/
@[inline, expose, grind] def fromPublicTrans  : Array NeedsKind := NeedsKind.all

/-- The scope (directly) demanded by the `NeedsKind`. -/
@[inline, simp, expose] def source (k : NeedsKind) : Environment.Visibility :=
  if k.isAll then .private else .public
/-- The scope targeted by the `NeedsKind`. -/
@[inline, simp, expose] def target (k : NeedsKind) : Environment.Visibility :=
  if k.isExported then .public else .private

/-- Whether `k` places the `src` in the `tgt` visibility, after taking into account `public` ⊆
`private` on both ends. Note that we abstractly consider `NeedsKind` to be a single arrow between
scopes (i.e. `privOfPriv` only relates the private scope to the private scope), but in this
function we allow linearization (i.e. composition with `public` ↪ `private`) -/
@[inline] def connects (src tgt : Environment.Visibility) (k : NeedsKind) : Bool :=
  match src, tgt with
  | .public,  .private => true
  | .public,  .public  => k.isExported
  | .private, .private => k.isAll
  | .private, .public  => false

/-- Whether `k` yields something in the scope `tgt`. This is always true when `tgt` is `.private`
thanks to `public` ⊆ `private` on the target side, and is `k.isExported` when `.public`. -/
def yields (tgt : Environment.Visibility) (k : NeedsKind) : Bool :=
  match tgt with
  | .public  => k.isExported
  | .private => true

/-- Whether `k` demands something in the scope `src`. This is always true when `src` is `.public`
thanks to `public` ⊆ `private` on the source side, and is `k.isAll` when `.private`. -/
def demands (src : Environment.Visibility) (k : NeedsKind) : Bool :=
  match src with
  | .public  => true
  | .private => k.isAll

set_option linter.unusedVariables false in
def andThen (k₁ k₂ : NeedsKind) (connectable : k₁.target = k₂.source := by grind) :
    NeedsKind where
  isMeta := k₁.isMeta || k₂.isMeta
  isExported := k₂.isExported
  isAll := k₁.isAll && k₂.isAll
  not_isExported_and_isAll := by grind [k₂.not_isExported_and_isAll]

@[simp, grind =] theorem target_andThen_eq_right_target (k₁ k₂ : NeedsKind)
    (connectable : k₁.target = k₂.source) : (andThen k₁ k₂).target = k₂.target := by
  grind [target, andThen]

@[simp, grind =] theorem source_andThen_eq_left_source (k₁ k₂ : NeedsKind)
    (connectable : k₁.target = k₂.source) : (andThen k₁ k₂).source = k₁.source := by
  simp only [target, source] at connectable
  grind only [source, andThen, k₁.not_isExported_and_isAll]

@[simp] theorem andThen_assoc (k₁ k₂ k₃: NeedsKind)
    (connectable₁₂ : k₁.target = k₂.source) (connectable₂₃ : k₂.target = k₃.source) :
    (k₁.andThen k₂).andThen k₃ = k₁.andThen (k₂.andThen k₃) := by
  grind only [andThen, k₁.not_isExported_and_isAll]

end ImportGraph.Shake.NeedsKind
