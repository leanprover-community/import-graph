/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public meta import ImportGraph.Shake.Algebra
import Lean.Elab.Command

/-!
# Tests for the `Shake` dependency algebra

Every sweep below is deterministic. The exhaustive ones range over all `Needs` on a one- or
two-module universe (`Model.needsOfNat`); the sampled ones range over pseudorandom
hierarchy/`Needs` pairs on a four-module universe (`Model.randomPairs`). A failure prints the
encoding of the offending case, which reproduces it.
-/

open Lean ImportGraph Shake NeedsKind

namespace ImportGraphTest.Shake

/-! ## `NeedsKind` -/

meta section NeedsKind

def shortName : NeedsKind → String
  | .pub => "pub" | .priv => "priv" | .privOfPriv => "all"
  | .metaPub => "𝓶pub" | .metaPriv => "𝓶priv" | .metaPrivOfPriv => "𝓶all"

/-- All ordered pairs of `NeedsKind`s. -/
def NeedsKind.allPairs : Array (NeedsKind × NeedsKind) :=
  NeedsKind.all.flatMap fun k₁ => NeedsKind.all.map ((k₁, ·))

/-- info: #["pub", "priv", "𝓶pub", "𝓶priv", "all", "𝓶all"] -/
#guard_msgs in #eval NeedsKind.all.map shortName

/-! ### Composition -/

def pad (n : Nat) (s : String) : String := s ++ "".pushn ' ' (n - s.length)

/-- The composition table of `NeedsKind.andThen`, with the first factor on the left. -/
meta def compositionTable : String :=
  let row (cells : List String) := (String.join <| cells.map (pad 7)).trimAsciiEnd.toString
  -- push meta to the end instead of `all` for a cleaner table
  let ks := NeedsKind.all.insertionSort (!·.isMeta && ·.isMeta) |>.toList
  "\n".intercalate <|
    row ("(↓) ≫ (→) " :: ks.map shortName) ::
      ks.map fun k₁ =>
        row <| pad 10 (shortName k₁) :: ks.map fun k₂ =>
          if h : k₁.target = k₂.source then shortName (andThen k₁ k₂) else "-"

/--
info:

(↓) ≫ (→) pub    priv   all    𝓶pub   𝓶priv  𝓶all
pub       pub    priv   -      𝓶pub   𝓶priv  -
priv      -      -      priv   -      -      𝓶priv
all       -      -      all    -      -      𝓶all
𝓶pub      𝓶pub   𝓶priv  -      𝓶pub   𝓶priv  -
𝓶priv     -      -      𝓶priv  -      -      𝓶priv
𝓶all      -      -      𝓶all   -      -      𝓶all
-/
#guard_msgs in
#eval IO.println s!"\n\n{compositionTable}"

-- `NeedsKind.andThen` agrees with composition read off the scopes it connects.
/-- info: 36 cases passed -/
#guard_msgs in
#eval check kindPairs describeKinds fun (k₁, k₂) => andThen? k₁ k₂ == comp? k₁ k₂

-- `NeedsKind.ofImport` reads back the flags of an import.
/-- info: 6 cases passed -/
#guard_msgs in
#eval check kinds.toArray shortName fun k => NeedsKind.ofImport (importOf k) == k

-- The `to`/`from` partitions are exactly the kinds `NeedsKind.andThen` may compose on either side.
/-- info: 6 cases passed -/
#guard_msgs in
#eval check kinds.toArray shortName fun k =>
  (NeedsKind.to k.target).contains k && (NeedsKind.from k.source).contains k &&
  !(NeedsKind.to (if k.target == .public then .private else .public)).contains k &&
  !(NeedsKind.from (if k.source == .public then .private else .public)).contains k

end NeedsKind

/-! ## Post-composition -/

section Postcomposition

-- `Needs.andThen` composes exactly the composable prearrows.
/-- info: 4096 cases passed -/
#guard_msgs in
#eval checkRange (needsCount 2) fun m =>
  let n := needsOfNat 2 m
  NeedsKind.all.all fun k => n ≫ k == needsOf ((arrowsOf 2 n).andThen k)

-- `Needs.addAndThen` adds to its base rather than replacing it.
/-- info: 4096 cases passed -/
#guard_msgs in
#eval checkRange (needsCount 2) fun m =>
  let n := needsOfNat 2 m
  NeedsKind.all.all fun k => n.addAndThen k (base := n) == n ∪ (n ≫ k)

-- Post-composing by an `Import` agrees with post-composing by its `NeedsKind`.
/-- info: 4096 cases passed -/
#guard_msgs in
#eval checkRange (needsCount 2) fun m =>
  let n := needsOfNat 2 m
  kinds.all fun k => (importOf k).andThen n == n ≫ k

-- Post-composition preserves `∅` and distributes over `∪`.
/-- info: 4096 cases passed -/
#guard_msgs in
#eval checkRange (needsCount 1 * needsCount 1) fun m =>
  let n₁ := needsOfNat 1 (m % needsCount 1)
  let n₂ := needsOfNat 1 (m / needsCount 1)
  NeedsKind.all.all fun k =>
    (∅ : Needs) ≫ k == (∅ : Needs) && (n₁ ∪ n₂) ≫ k == (n₁ ≫ k) ∪ (n₂ ≫ k)

-- Post-composition is associative: `(n ≫ k₁) ≫ k₂ = n ≫ (k₁ ≫ k₂)`.
/-- info: 64 cases passed -/
#guard_msgs in
#eval checkRange (needsCount 1) fun m =>
  let n := needsOfNat 1 m
  kindPairs.all fun (k₁, k₂) =>
    match andThen? k₁ k₂ with
    | some k => (n ≫ k₁) ≫ k₂ == n ≫ k
    | none => true

end Postcomposition

/-! ## Linearization -/

section Linearization

/-- info: 4096 cases passed -/
#guard_msgs in
#eval checkRange (needsCount 2) fun m =>
  let n := needsOfNat 2 m
  n.linearize == needsOf (arrowsOf 2 n).linearize &&
  n.antilinearize == needsOf (arrowsOf 2 n).antilinearize

-- Each lands where it says it does, and fixes what is already there.
/-- info: 4096 cases passed -/
#guard_msgs in
#eval checkRange (needsCount 2) fun m =>
  let n := needsOfNat 2 m
  n.linearize.isLinear && n.antilinearize.isAntilinear &&
  (!n.isLinear || n.linearize == n) && (!n.isAntilinear || n.antilinearize == n)

-- Both are idempotent, and each absorbs the other.
/-- info: 4096 cases passed -/
#guard_msgs in
#eval checkRange (needsCount 2) fun m =>
  let n := needsOfNat 2 m
  n.linearize.linearize == n.linearize &&
  n.antilinearize.antilinearize == n.antilinearize &&
  n.antilinearize.linearize == n.linearize &&
  n.linearize.antilinearize == n.antilinearize

-- `antilinearize n ⊆ n ⊆ linearize n`, and neither moves the public scopes.
/-- info: 4096 cases passed -/
#guard_msgs in
#eval checkRange (needsCount 2) fun m =>
  let n := needsOfNat 2 m
  n.antilinearize.directLe n && n.directLe n.linearize &&
  n.linearize.pub == n.pub && n.linearize.metaPub == n.metaPub &&
  n.antilinearize.pub == n.pub && n.antilinearize.metaPub == n.metaPub

-- `linearize` is monotone; `antilinearize` is not (it is only a choice of representative).
/-- info: 4096 cases passed -/
#guard_msgs in
#eval checkRange (needsCount 1 * needsCount 1) fun m =>
  let n₁ := needsOfNat 1 (m % needsCount 1)
  let n₂ := needsOfNat 1 (m / needsCount 1)
  !n₁.directLe n₂ || n₁.linearize.directLe n₂.linearize

#guard
  let n₁ := Needs.single 0 .priv
  let n₂ := n₁ ∪ Needs.single 0 .pub
  n₁.directLe n₂ && !n₁.antilinearize.directLe n₂.antilinearize

end Linearization

/-! ## Reflexification -/

section Reflexification

-- `reflexify` adjoins `reflOf`, which covers the public and private scopes but not the meta ones.
/-- info: 12288 cases passed -/
#guard_msgs in
#eval checkRange (3 * needsCount 2) fun m =>
  let i := m / needsCount 2
  let n := needsOfNat 2 (m % needsCount 2)
  Needs.reflOf i == needsOf (reflArrows i) &&
  Needs.reflexify i n == n ∪ Needs.reflOf i &&
  (Needs.reflOf i).isLinear &&
  Needs.unreflexify i (Needs.reflexify i n) == Needs.unreflexify i n

end Reflexification

/-! ## Transitive closure -/

section TransitiveClosure

/-- Pseudorandom `(hierarchy, needs)` encodings on a four-module universe. -/
meta def hierarchyCases : Array (Nat × Nat) := randomPairs 20260903 200

meta def describeCase (c : Nat × Nat) : String := s!"({c.1}, {c.2})"

/-- The `Provides` hierarchy generated by a case, in the model and in `Needs`. -/
meta def hierarchyOf (c : Nat × Nat) : Array Arrows × ArrayHierarchy :=
  (providesOfNat 4 c.1, hierarchyOfNat 4 c.1)

-- The generated hierarchies are reflexified, linearized, and transitively closed.
/-- info: 200 cases passed -/
#guard_msgs in
#eval check hierarchyCases describeCase fun c =>
  let (_, transDeps) := hierarchyOf c
  (Array.range 4).all fun i =>
    let p := transDeps[i]!
    p.isLinear && (Needs.reflOf i).directLe p && p.transitiveClosure transDeps == p

-- `Hierarchy.andThen` and `Needs.transitiveClosure` agree with the model.
/-- info: 200 cases passed -/
#guard_msgs in
#eval check hierarchyCases describeCase fun c =>
  let (provides, transDeps) := hierarchyOf c
  let n := needsOfNat 4 c.2
  let a := arrowsOf 4 n
  Hierarchy.andThen transDeps n == needsOf (a.postcompose provides) &&
  transDeps⟦n⟧ == needsOf (a.transitiveClosure provides)

-- Against a reflexified hierarchy, closing is inflationary and idempotent, and the union with `n`
-- in `Needs.transitiveClosure` is redundant.
/-- info: 200 cases passed -/
#guard_msgs in
#eval check hierarchyCases describeCase fun c =>
  let (_, transDeps) := hierarchyOf c
  let n := needsOfNat 4 c.2
  n.directLe transDeps⟦n⟧ &&
  Hierarchy.andThen transDeps n == transDeps⟦n⟧ &&
  transDeps⟦transDeps⟦n⟧⟧ == transDeps⟦n⟧

-- Closing a `Needs` is the union of the closures of its prearrows.
/-- info: 200 cases passed -/
#guard_msgs in
#eval check hierarchyCases describeCase fun c =>
  let (_, transDeps) := hierarchyOf c
  let n := needsOfNat 4 c.2
  transDeps⟦n⟧ == (arrowsOf 4 n).foldl (init := ∅) fun acc (i, k) => acc ∪ transDeps⟦(i, k)⟧

-- `Needs.coveredBy` and `Needs.subsumedBy` agree with the model.
/-- info: 200 cases passed -/
#guard_msgs in
#eval check hierarchyCases describeCase fun c =>
  let (provides, transDeps) := hierarchyOf c
  let n := needsOfNat 4 c.2
  let a := arrowsOf 4 n
  let n' := needsOfNat 4 (c.2 >>> (6 * 4))
  ((Array.range 4).all fun i => n.coveredBy i transDeps == a.le provides[i]!) &&
  n.subsumedBy n' transDeps == a.le ((arrowsOf 4 n').linearize.transitiveClosure provides)

end TransitiveClosure

/-! ## Reduction -/

section Reduction

/-- Whether `reduced` covers `a`, in the sense of `Needs.reduce`. -/
meta def covers (a reduced : Needs) (transDeps : ArrayHierarchy) : Bool :=
  a.subsumedBy reduced transDeps

/-- Whether dropping any single prearrow of `reduced` loses coverage of `a`. Coverage is monotone
in `reduced`, so this is exactly `⊆`-minimality. -/
meta def isMinimal (a reduced : Needs) (transDeps : ArrayHierarchy) : Bool :=
  (arrowsOf 4 reduced).all fun (i, k) => !covers a (reduced.sub k {i}) transDeps

-- The stated invariant: the reduction covers its input, is antilinearized, and stays within it.
/-- info: 200 cases passed -/
#guard_msgs in
#eval check hierarchyCases describeCase fun c =>
  let (_, transDeps) := hierarchyOf c
  let a := needsOfNat 4 c.2
  let reduced := a.reduce transDeps
  covers a reduced transDeps && reduced.isAntilinear && reduced.directLe a.linearize

-- The reduction is minimal.
/-- info: 200 cases passed -/
#guard_msgs in
#eval check hierarchyCases describeCase fun c =>
  let (_, transDeps) := hierarchyOf c
  let a := needsOfNat 4 c.2
  isMinimal a (a.reduce transDeps) transDeps

-- Reduction sees its input only up to linearization, and is idempotent.
/-- info: 200 cases passed -/
#guard_msgs in
#eval check hierarchyCases describeCase fun c =>
  let (_, transDeps) := hierarchyOf c
  let a := needsOfNat 4 c.2
  let reduced := a.reduce transDeps
  a.linearize.reduce transDeps == reduced && reduced.reduce transDeps == reduced

/-! ### Worked examples -/

/-- The reduction of `a` against the hierarchy generated by `imports`, as a set of prearrows. -/
meta def reduceOf (imports : Imports) (a : Arrows) : IO Unit :=
  IO.println <| toString <| arrowsOf imports.size <|
    (needsOf a).reduce (imports.provides.map needsOf)

/-- `1` publicly imports `0`, `2` imports `1`, and `3` imports all of `2`. -/
meta def chain : Imports :=
  #[∅, [(0, .pub)], [(1, .priv)], [(2, .privOfPriv)]]

-- A direct dependency is kept when nothing else provides it.
/-- info: {(0 [public⟩ ·)} -/
#guard_msgs in
#eval reduceOf chain [(0, .pub)]

-- `1` carries `0` publicly, so needing both privately reduces to needing `1`.
/-- info: {(1 [private⟩ ·)} -/
#guard_msgs in
#eval reduceOf chain [(0, .priv), (1, .priv)]

-- `2` imports `1` privately, so `2` does not carry `1`.
/-- info: {(1 [private⟩ ·), (2 [private⟩ ·)} -/
#guard_msgs in
#eval reduceOf chain [(1, .priv), (2, .priv)]

-- `public ⊆ private` on the target side: a public need subsumes the private one.
/-- info: {(0 [public⟩ ·)} -/
#guard_msgs in
#eval reduceOf chain [(0, .pub), (0, .priv)]

-- Needing the private scope of `2` is not implied by needing its public scope.
/-- info: {(2 [all⟩ ·)} -/
#guard_msgs in
#eval reduceOf chain [(2, .priv), (2, .privOfPriv)]

-- The meta phase is not implied by the non-meta one.
/-- info: {(1 [private⟩ ·), (1 [private meta⟩ ·)} -/
#guard_msgs in
#eval reduceOf chain [(1, .priv), (1, .metaPriv), (0, .priv)]

-- Importing all of `3` also brings along everything `3` imports privately.
/-- info: {(3 [all⟩ ·)} -/
#guard_msgs in
#eval reduceOf chain [(3, .privOfPriv), (2, .priv), (0, .priv)]

end Reduction

/-! ## Minimal elements -/

section Minimals

/-- The `lt`-minimal elements accumulated by `Std.HashMap.incorporateBelowAt`, where `lt` is strict
inclusion of bit patterns. -/
meta def minimalsOf (xs : List Nat) : List Nat :=
  let lt (a b : Nat) := a != b && a &&& b == a
  let m := xs.foldl (init := (∅ : Std.HashMap Unit (Array (Option Nat)))) fun m x =>
    m.incorporateBelowAt () x lt
  (m.getD () #[]).toList.reduceOption.mergeSort (· ≤ ·)

/-- info: [1, 2] -/
#guard_msgs in
#eval IO.println <| toString <| minimalsOf [3, 1, 7, 2, 3]

/-- info: [3] -/
#guard_msgs in
#eval IO.println <| toString <| minimalsOf [7, 3, 11]

/-- info: [] -/
#guard_msgs in
#eval IO.println <| toString <| minimalsOf []

end Minimals

end ImportGraphTest.Shake
