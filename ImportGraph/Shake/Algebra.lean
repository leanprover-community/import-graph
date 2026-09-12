/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.Shake.Basic
import Std.Data.HashMap.AdditionalOperations

/-!
# Algebra of an import hierarchy

This file provides algebraic/compositional relationships between module system dpendencies in an
import hierarchy.

Notably, we provide transitive closure operators relative to an import hierarchy, which relies on
the nontrivial composition of module system imports.

## Future work

- Document this more thoroughly.
- Consider making the `Hierarchy` API more featureful.
-/

public section

open Lean ImportGraph Shake

namespace ImportGraph.Shake

protected class HPostcomp (α) (β) (γ : outParam (Type u)) where
  protected hpostcomp : α → β → γ

scoped infixl:80 " ≫ " => HPostcomp.hpostcomp

protected class HTransClosure (α) (β) (γ : outParam (Type u)) where
  protected htransClosure : α → β → γ

/-- `𝓘⟦n⟧` is the transitive closure of `n` with respect to the hierarchy `𝓘`. -/
scoped notation:max I "⟦" n "⟧" => HTransClosure.htransClosure I n

/-- An import hierarchy, with a `size` and dependencies (`Provides`) for each index. -/
class Hierarchy (α) where
  size : α → Nat
  getDeps : (a : α) → (i : Nat) → (i < size a) → Provides
  -- TODO: do we need these?
  getDeps? : α → Nat → Option Provides
  getDeps! : α → Nat → Provides

/-- An `Array Provides`. -/
abbrev ArrayHierarchy := Array Provides

@[inline] instance : Hierarchy ArrayHierarchy where
  size a := a.size
  getDeps a i _ := a[i]
  getDeps? a i := a[i]?
  getDeps! a i := a[i]!

instance {H} [Hierarchy H] : GetElem? H Nat Provides (fun (a : H) i => i < Hierarchy.size a) where
  getElem := Hierarchy.getDeps
  getElem? := Hierarchy.getDeps?
  getElem! := Hierarchy.getDeps!

/-- Equips a monad with a `Hierarchy` state. -/
abbrev HierarchyT (H) [Hierarchy H] := StateT H

/-- Given an abstract `NeedsKind` `[kImp⟩` and a collection of prearrows `j [k⟩ ·` (`Provides`),
add to `base` the composed prearrows `j [k⟩[imp⟩ ·` where composition is possible. Does not account
for `public` ⊆ `private` on the codomain side (see `linearize`).

Note that this does *not* add the original collection of prearrows to `base`. -/
def Needs.addAndThen (impTransDeps : Needs) (kImp : NeedsKind)
    (base : Needs := ∅) : Needs := Id.run do
  let mut composed := base
  -- `{ j [k⟩[kImp⟩ · | j, k s.t. (j [k⟩ ·) ∈ impTransDeps ∧ k.target = kImp.source }`
  if _ : kImp.isAll then
    for h : k in NeedsKind.toPrivate do
      composed := composed.union
        (k.andThen kImp (by simp; grind))
        (impTransDeps.get k)
  else
    for h : k in NeedsKind.toPublic do
      composed := composed.union
        (k.andThen kImp (by simp; grind))
        (impTransDeps.get k)
  return composed

@[inline] def Needs.andThen (impTransDeps : Needs) (kImp : NeedsKind) : Needs :=
  impTransDeps.addAndThen kImp (base := ∅)

scoped instance : Shake.HPostcomp Needs NeedsKind Needs where
  hpostcomp n k := n.andThen k

/-- Given an abstract `Import` `[kImp⟩` and a collection of prearrows `j [k⟩ ·` (`Provides`), add
to `base` the composed prearrows `j [k⟩[imp⟩ ·` where composition is possible.

Note that this does *not* add the original collection of prearrows to `base`. -/
@[inline] def Lean.Import.addAndThen (impTransDeps : Needs) (imp : Import)
    (base : Needs := ∅) : Needs := Id.run do
  impTransDeps.addAndThen (NeedsKind.ofImport imp) base

/-- Given an abstract import `[k⟩` and a collection of prearrows `j [k⟩ ·` (`Needs`), forms
the composed prearrows `j [k'⟩[k⟩ ·` where composition is possible. -/
@[inline] def Lean.Import.andThen (impTransDeps : Needs) (imp : Import) : Needs :=
  imp.addAndThen (base := .empty) impTransDeps

scoped instance : Shake.HPostcomp Needs Import Needs where
  hpostcomp n imp := imp.andThen n

/-- Given an import hierarchy of arrows `j' [_⟩ j` and a preimport `i [imp⟩ ·`, forms the set of
prearrows obtained by transitively closing `i [imp⟩ ·` with respect to the import hierarchy. This
is `i [imp⟩ ·` together with compositions `j [_⟩ i [imp⟩ ·`. `transDeps` is assumed to be
reflexified. -/
@[inline] def NeedsKind.transitiveClosureSingle {H} [Hierarchy H] (i : Nat) (k : NeedsKind)
    (transDeps : H) : Needs :=
  transDeps[i]! ≫ k

scoped instance {H} [Hierarchy H] : Shake.HTransClosure H (Nat × NeedsKind) Needs where
  htransClosure := fun transDeps (i, imp) => imp.transitiveClosureSingle i transDeps

/-- Given an import hierarchy of arrows `j' [_⟩ j` and a preimport `i [imp⟩ ·`, forms the set of
prearrows obtained by transitively closing `i [imp⟩ ·` with respect to the import hierarchy. This
is `i [imp⟩ ·` together with compositions `j [_⟩ i [imp⟩ ·`. `transDeps` is assumed to be
reflexified.  -/
@[inline] def Lean.Import.transitiveClosureSingle {H} [Hierarchy H] (i : Nat) (imp : Import)
    (transDeps : H) : Needs :=
  transDeps[i]! ≫ imp

@[inline] scoped instance {H} [Hierarchy H] : Shake.HTransClosure H (Nat × Import) Needs where
  htransClosure := fun transDeps (i, imp) => imp.transitiveClosureSingle i transDeps

/-- Given a set of prearrows `i [k⟩ ·` and an import hierarchy, includes in `base` the compositions
of arrows `j [k'⟩ i [k⟩ ·` where composition is possible. Assumes every index is valid. -/
def Hierarchy.addAndThen {H} [Hierarchy H] (transDeps : H) (n : Needs)
    (base := Needs.empty) : Needs := Id.run do
  let mut composed := base
  for (k, i) in n.highToLow do
    composed := composed ∪ transDeps[i]! ≫ k
  composed

/-- Given a set of prearrows `i [k⟩ ·` and an import hierarchy, forms the compositions of arrows
`j [k'⟩ i [k⟩ ·` where composition is possible. -/
@[inline] def Hierarchy.andThen {H} [Hierarchy H] (transDeps : H) (n : Needs) : Needs :=
  Hierarchy.addAndThen transDeps n (base := .empty)

scoped instance {H} [Hierarchy H] : Shake.HPostcomp H Needs Needs where
  hpostcomp transDeps n := Hierarchy.andThen transDeps n

@[inline] def Needs.addTransitiveClosure {H} [Hierarchy H] (base n : Needs) (transDeps : H) :
    Needs :=
  Hierarchy.addAndThen transDeps n (base := base ∪ n)

-- TODO: should this linearize and/or promise a `Provides`?
/-- `n ∪ (transDeps ≫ n)` -/
@[inline] def Needs.transitiveClosure {H} [Hierarchy H] (n : Needs) (transDeps : H) : Needs :=
  Hierarchy.addAndThen transDeps n (base := n)

scoped instance {H} [Hierarchy H] : Shake.HTransClosure H Needs Needs where
  htransClosure transDeps n := n.transitiveClosure transDeps

/--
Includes the public visibilities in the corresponding private visibilities, to represent a
"provides" relationship. A `k : Needs` is "linear" iff it accounts for `public` ⊆ `private` (and
likewise for both being `meta`). Accounting for this on the target side means that `k.pub ⊆ k.priv`
(public imports are available privately) and accounting for it on the source side means that
`k.privOfPriv ⊆ k.priv` (importing the private scope privately implies importing the public scope
privately).
-/
@[inline] def Needs.linearize (a : Needs) : Needs :=
  { a with
    priv := a.priv ∪ a.pub ∪ a.privOfPriv
    metaPriv := a.metaPriv ∪ a.metaPub ∪ a.metaPrivOfPriv }

/-- Whether `public` ⊆ `private` for the given `Needs`, on both the meta and non-meta levels. -/
@[inline] def Needs.isLinear (a : Needs) : Bool :=
  a.pub ∪ a.privOfPriv ⊆ a.priv && a.metaPub ∪ a.metaPrivOfPriv ⊆ a.metaPriv

/-- Removes private needs which can be inferred by accounting for `public` ⊆ `private` on both the
source and target side. See `Needs.linearize` for details. -/
@[inline] def Needs.antilinearize (a : Needs) : Needs := { a with
  priv := a.priv \ (a.pub ∪ a.privOfPriv)
  metaPriv := a.metaPriv \ (a.metaPub ∪ a.metaPrivOfPriv) }

@[inline] def Needs.isAntilinear (a : Needs) : Bool :=
  (a.pub ∪ a.privOfPriv) ∩ a.priv == {} && (a.metaPub ∪ a.metaPrivOfPriv) ∩ a.metaPriv == {}

/-- A `Provides` providing to a module the aspects of that module which it provides to itself. Note
that a module does *not* provide its own non-meta scopes as meta dependencies to itself.
Linearized. -/
@[inline] def Needs.reflOf (i : Nat) : Provides := { Needs.empty with
  pub := {i}
  priv := {i}
  privOfPriv := {i} }

/--
Adds in the reflexive availabilities of a given module, which are just the public and private
availabilities and not the meta lifted versions. This matches what is available within a given
module. Equivalent to `a ∪ .reflOf i`.

Note that this operation does *not* necessarily commute with transitive closure.
-/
@[inline] def Needs.reflexify (i : Nat) (a : Needs) : Needs := { a with
  pub := a.pub ∪ {i}
  priv := a.priv ∪ {i}
  privOfPriv := a.privOfPriv ∪ {i} }

/-- Clears all dependencies at the given index. -/
@[inline] def Needs.clearAt (i : Nat) (a : Needs) : Needs :=
  a.map (· \ {i})

/-- Checks if the `Provides` hierarchy `transDeps` provides arrows `j [k⟩ i` for all
`(j [k⟩ ·) ∈ needs`. Assumes `transDeps` is well-formed as a `Provides` hierarchy (i.e. linearized
and reflexified). -/
@[inline] def Needs.providedToBy {H} [Hierarchy H] (needs : Needs) (i : Nat) (transDeps : H) :
    Bool :=
  needs.directLe <| transDeps[i]!

/-- Checks if the prearrows `j [k⟩ ·` in `n₁` are included in the arrows provided by the transitive
closure of `n₂` with respect to the import hierarchy. Linearizes `n₂` first, which ensures `n₁` is
not penalized for itself being linearized and respecting `public` ⊆ `private`. Assumes `transDeps`
is a well-formed `Provides` hierarchy, i.e. linearized and reflexified. -/
@[inline] def Needs.subsumedBy {H} [Hierarchy H] (n₁ n₂ : Needs) (transDeps : H) : Bool :=
  n₁.directLe transDeps⟦n₂.linearize⟧

/--
Returns an antilinearized `reduced : Needs` such that
```
a ≤ transDeps⟦reduced.linearize⟧
```
and `reduced` is minimal (perhaps non-uniquely) among such `Needs`.

The returned `reduced` is antilinearized, and thus suitable for converting to imports.

Does not assume `a` is linearized.
-/
def Needs.reduce {H} [Hierarchy H] (a : Needs) (transDeps : H) : Needs := Id.run do
  let mut reduced := a.linearize
  let a := a.antilinearize -- avoids unnecessary checks
  -- ensure we handle public/private first, since these may reduce meta
  for k in #[NeedsKind.pub, .priv, .privOfPriv, .metaPub, .metaPriv, .metaPrivOfPriv] do
    for i in a.get k |>.highToLow do
      if reduced.has k i then -- `(k, i)` may have been eliminated already
        reduced := (reduced \ transDeps⟦(i, k)⟧.linearize).union k {i}
  return reduced.antilinearize

/-- Attempts to insert `a` among the set `as` of minimal elements as a new minimal element
according to `lt`. Clears elements of `as` that are above `a`, and ignores `a` if we already have
an element lower than `a`.  -/
@[inline] def Array.incorporateBelow? (as : Array (Option α)) (a : α)
    (lt : α → α → Bool) : Array (Option α) := Id.run do
  let mut as := as
  for i in 0...as.size do
    let some aᵢ := as[i]! | continue
    if lt a aᵢ then
      -- Erase elements of `as` that are (strictly) above `a`
      as := as.set! i none
    else if lt aᵢ a then
      -- If `a` is strictly below any pre-existing element, we don't need to add it
      return as
  return as.push a

/-- At `k`, attempts to insert `a` among the set `as` of minimal elements as a new minimal element
according to `lt`. Clears elements of `as` that are above `a`, and ignores `a` if we already have
an element lower than `a`.  -/
@[inline] def Std.HashMap.incorporateBelowAt? {κ} [BEq κ] [Hashable κ]
    (map : Std.HashMap κ (Array (Option α))) (k : κ) (a : α) (lt : α → α → Bool) :
    Std.HashMap κ (Array (Option α)) := map.alter k fun arr? =>
      arr?.getD #[] |>.incorporateBelow? a lt

/-- The minimal elements of `xs`, according to `lt`. -/
@[inline] def Array.minimals (xs : Array α) (lt : α → α → Bool) : Array α := Id.run do
  let mut m : Array (Option α) := #[]
  for x in xs do
    m := m.incorporateBelow? x lt
  return m.reduceOption

/-- The minimal values of `xs` under `val` according to `lt`, organized and compared per `key`
value. See `minimalsPer` for a version without `val`; `val` is essentially an optimization. -/
@[inline] def Array.minimalValuesPer {κ} [BEq κ] [Hashable κ]
    (xs : Array α) (key : α → κ) (val : α → β) (lt : β → β → Bool) :
    Std.HashMap κ (Array β) := Id.run do
  let mut m : Std.HashMap κ (Array (Option β)) := ∅
  for x in xs do
    m := m.incorporateBelowAt? (key x) (val x) lt
  return m.map fun _ vals => vals.reduceOption

/-- The minimal elements of `xs` according to `lt`, organized and compared per `key` value. -/
@[inline] def Array.minimalsPer {κ} [BEq κ] [Hashable κ]
    (xs : Array α) (key : α → κ) (lt : α → α → Bool) :
    Std.HashMap κ (Array α) :=
  xs.minimalValuesPer key id lt

end ImportGraph.Shake
