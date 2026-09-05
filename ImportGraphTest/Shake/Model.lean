/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.Shake.Algebra

/-!
# A reference model for the `Shake` dependency algebra

`Needs` packs a set of prearrows `j [k⟩ ·` into six `Bitset`s. Here we model the same data
naively, as a list of pairs, and reimplement the operations of `ImportGraph.Shake.Algebra` against
that model. `ImportGraphTest.Shake.Algebra` tests the two implementations against each other.

The model is deliberately derived from a different starting point than the implementation:
composition is read off the source and target *scopes* of a `NeedsKind` rather than computed from
its fields, and the enumeration of `NeedsKind`s is generated from the underlying booleans rather
than taken from `NeedsKind.all`.
-/

public section

open Lean ImportGraph Shake

namespace ImportGraphTest.Shake.Model

/-! ## `NeedsKind`s -/

/-- Every `NeedsKind`, generated from the underlying booleans. -/
def kinds : List NeedsKind :=
  [false, true].flatMap fun isExported =>
  [false, true].flatMap fun isMeta =>
  [false, true].filterMap fun isAll =>
    if h : isExported && isAll then none
    else some { isExported, isMeta, isAll }

/-- The `NeedsKind` taking the scope `src` to the scope `tgt` at the given phase, if there is one.
There is no arrow from a private scope to a public one. -/
def kindOf? (src tgt : Environment.Visibility) (isMeta : Bool) : Option NeedsKind :=
  match src, tgt with
  | .public,  .public  => some { isExported := true,  isMeta }
  | .public,  .private => some { isExported := false, isMeta }
  | .private, .private => some { isExported := false, isMeta, isAll := true }
  | .private, .public  => none

deriving instance DecidableEq for Environment.Visibility

/-- `NeedsKind.andThen`, as a partial operation. -/
def andThen? (k₁ k₂ : NeedsKind) : Option NeedsKind :=
  if h : k₁.target = k₂.source then some (k₁.andThen k₂ h) else none

/-- Composition of `NeedsKind`s, read off the scopes they connect: `k₁` extends along `k₂` exactly
when `k₁`'s target scope is `k₂`'s source scope, and the composite runs from `k₁`'s source to
`k₂`'s target, at the meta phase if either factor is. -/
def comp? (k₁ k₂ : NeedsKind) : Option NeedsKind :=
  if k₁.target == k₂.source then
    kindOf? k₁.source k₂.target (k₁.isMeta || k₂.isMeta)
  else
    none

/-- The `Import` carrying the flags of `k`. -/
def importOf (k : NeedsKind) : Import :=
  { module := .anonymous, isExported := k.isExported, isMeta := k.isMeta, importAll := k.isAll }

/-- A short name for a `NeedsKind`, for tabulating. -/
def shortName : NeedsKind → String
  | .pub => "pub" | .priv => "priv" | .privOfPriv => "all"
  | .metaPub => "mpub" | .metaPriv => "mpriv" | .metaPrivOfPriv => "mall"

/-! ## Arrows -/

/-- A prearrow `j [k⟩ ·`. -/
abbrev Arrow := Nat × NeedsKind

/-- A set of prearrows, modeling `Needs`. -/
abbrev Arrows := List Arrow

namespace Arrows

-- def contains (a : Arrows) (x : Arrow) : Bool := a.toList.contains x

-- def insert (a : Arrows) (x : Arrow) : Arrows := if a.contains x then a else ⟨x :: a.toList⟩

-- def ofList (l : List Arrow) : Arrows := l.foldl (·.insert ·) {}

-- def filter (a : Arrows) (p : Arrow → Bool) : Arrows := ⟨a.toList.filter p⟩

-- def filterMap (a : Arrows) (f : Arrow → Option Arrow) : Arrows := ofList (a.toList.filterMap f)

instance : Union Arrows := ⟨fun a b => b.foldl (·.insert ·) a⟩

/-- `a.le b` iff every prearrow of `a` is a prearrow of `b`. -/
def le (a b : Arrows) : Bool := a.all b.contains

instance : BEq Arrows := ⟨fun a b => a.le b && b.le a⟩

instance : ToString Arrows where
  toString a := "{" ++ ", ".intercalate (a.map fun (j, k) => s!"({j} [{k}⟩ ·)") ++ "}"

/-! ### Operations -/

/-- Post-composition with `[k⟩`; models `Needs.andThen`. -/
def andThen (a : Arrows) (k : NeedsKind) : Arrows :=
  a.filterMap fun (j, k') => (comp? k' k).map (j, ·)

/-- Post-composition with `public ↪ private`; models `Needs.linearize`. -/
def linearize (a : Arrows) : Arrows :=
  a ∪ a.filterMap fun (j, k) => (kindOf? k.source .private k.isMeta).map (j, ·)

/-- Removal of the prearrows implied by `public ↪ private`; models `Needs.antilinearize`. -/
def antilinearize (a : Arrows) : Arrows :=
  a.filter fun (j, k) =>
    match kindOf? k.source .public k.isMeta with
    | some k' => k.isExported || !a.contains (j, k')
    | none => true

end Arrows

/-- The prearrows a module provides to itself; models `Needs.reflOf`. -/
def reflArrows (i : Nat) : Arrows := [(i, .pub), (i, .priv), (i, .privOfPriv)]

/-! ## Import hierarchies -/

/-- The direct imports of each module: `imports[i]` are the arrows `j [k⟩ i`. Every module is
assumed to import only modules of strictly smaller index. -/
abbrev Imports := Array Arrows

/-- The transitively closed, linearized, reflexified prearrows into each module. -/
def Imports.provides (imports : Imports) : Array Arrows := Id.run do
  let mut provides : Array Arrows := .emptyWithCapacity imports.size
  for i in 0...imports.size do
    let mut p := reflArrows i
    for (j, k) in imports[i]! do
      p := p ∪ provides[j]!.andThen k
    provides := provides.push p.linearize
  return provides

/-- Extension of each prearrow of `a` backwards along the hierarchy; models `Hierarchy.andThen`. -/
def Arrows.postcompose (a : Arrows) (provides : Array Arrows) : Arrows :=
  a.foldl (init := {}) fun acc (j, k) => acc ∪ provides[j]!.andThen k

/-- Models `Needs.transitiveClosure`. -/
def Arrows.transitiveClosure (a : Arrows) (provides : Array Arrows) : Arrows :=
  a ∪ a.postcompose provides

/-! ## Conversion -/

/-- The prearrows of `n` with source index below `univSize`. -/
def arrowsOf (univSize : Nat) (n : Needs) : Arrows :=
  (List.range univSize).flatMap fun i =>
    kinds.filterMap fun k => if n.has k i then some (i, k) else none

/-- The `Needs` containing exactly the prearrows of `a`. -/
def needsOf (a : Arrows) : Needs :=
  a.foldl (init := .empty) fun n (i, k) => n.union k {i}

/-! ## Enumeration and sampling -/

/-- The `Needs` on `univSize` modules encoded by `m`: bit `6 * i + b` records the presence of the
prearrow `i [kinds[b]⟩ ·`. -/
def needsOfNat (univSize m : Nat) : Needs :=
  needsOf <| (List.range univSize).flatMap fun i =>
    kinds.zipIdx.filterMap fun (k, b) => if m.testBit (6 * i + b) then some (i, k) else none

/-- The number of `Needs` on `univSize` modules. -/
def needsCount (univSize : Nat) : Nat := 1 <<< (6 * univSize)

/-- The direct imports on `univSize` modules encoded by `m`: for the pair `j < i`, bit
`6 * (i * (i - 1) / 2 + j) + b` records whether `i` imports `j` with kind `kinds[b]`. -/
def importsOfNat (univSize m : Nat) : Imports :=
  .ofFn (n := univSize) fun i =>
    (List.range i.val).flatMap fun j =>
      kinds.zipIdx.filterMap fun (k, b) =>
        if m.testBit (6 * (i.val * (i.val - 1) / 2 + j) + b) then some (j, k) else none

/-- A 64-bit linear congruential generator. Its high bits keep the sampled sweeps deterministic
without being visibly structured. -/
def lcg (s : Nat) : Nat := (6364136223846793005 * s + 1442695040888963407) % (1 <<< 64)

/-- `n` pseudorandom pairs, generated from `seed`. -/
def randomPairs (seed n : Nat) : Array (Nat × Nat) := Id.run do
  let mut s := seed
  let mut out := Array.emptyWithCapacity n
  for _ in 0...n do
    s := lcg s
    let x := s >>> 16
    s := lcg s
    out := out.push (x, s >>> 16)
  return out

/-- The `Provides` hierarchy of the direct imports encoded by `m`, in the model. -/
def providesOfNat (univSize m : Nat) : Array Arrows := (importsOfNat univSize m).provides

/-- The `Provides` hierarchy of the direct imports encoded by `m`, as an `ArrayHierarchy`. -/
def hierarchyOfNat (univSize m : Nat) : ArrayHierarchy := (providesOfNat univSize m).map needsOf

/-! ## Checking -/

/-- `s`, padded on the right to `n` characters. -/
def pad (n : Nat) (s : String) : String := s ++ "".pushn ' ' (n - s.length)

/-- Checks `p` on every case, printing the number of cases checked or else the first failure. -/
def check {α} (cases : Array α) (descr : α → String) (p : α → Bool) : IO Unit :=
  IO.println <|
    match cases.find? (!p ·) with
    | some a => s!"failed: {descr a}"
    | none => s!"{cases.size} cases passed"

/-- Checks `p` on `0, ..., n - 1`. -/
def checkRange (n : Nat) (p : Nat → Bool) : IO Unit := check (Array.range n) toString p

end ImportGraphTest.Shake.Model
