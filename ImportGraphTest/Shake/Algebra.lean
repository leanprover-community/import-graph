/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public meta import ImportGraph.Shake.Algebra
import Lean.Elab.Command

/-!
# Tests for the import hierarchy algebra
-/

open Lean ImportGraph Shake NeedsKind

namespace ImportGraph.Shake

-- We use `public meta section` for `#guard`
public meta section

section NeedsKind

def shortName : NeedsKind → String
  | .pub => "pub" | .priv => "priv" | .privOfPriv => "all"
  | .metaPub => "𝓶pub" | .metaPriv => "𝓶priv" | .metaPrivOfPriv => "𝓶all"

/-- All ordered pairs of `NeedsKind`s. -/
def NeedsKind.allPairs : Array (NeedsKind × NeedsKind) :=
  NeedsKind.all.flatMap fun k₁ => NeedsKind.all.map ((k₁, ·))

/-- info: #["pub", "priv", "𝓶pub", "𝓶priv", "all", "𝓶all"] -/
#guard_msgs in #eval NeedsKind.all.map shortName

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

-- `NeedsKind.andThen` agrees with composition
#guard Id.run do
  let mut okay := true
  for (k₁, k₂) in NeedsKind.allPairs do
    let fromConnecting := connecting? k₁.source k₂.target (k₁.isMeta || k₂.isMeta)
    if h : k₁.target = k₂.source then
      okay := okay && some (andThen k₁ k₂) == fromConnecting
  return okay

-- If this were real, we'd use a monad interface `[MonadRand m]`. Here, we can get by with `RandT`.

variable [RandomGen γ] (g : γ) [Monad m]

abbrev RandT (γ : Type u) [RandomGen γ] := StateT γ
abbrev RandM (γ : Type u) [RandomGen γ] := RandT γ Id

nonrec abbrev Std.RandT := RandT StdGen
nonrec abbrev Std.RandM := RandM StdGen

nonrec abbrev Std.RandM.run (gen : StdGen) (x : Std.RandM α) := x.run gen
nonrec abbrev Std.RandM.run' (gen : StdGen) (x : Std.RandM α) := x.run' gen

nonrec def RandT.repeat (runs : Nat) (x : RandT γ m α) : RandT γ m (Array α) := do
  let mut a := #[]
  for _ in 0...runs do
    a := a.push (← x)
  return a

nonrec def RandT.all (runs : Nat) (x : RandT γ m Bool) : RandT γ m Bool := do
  for _ in 0...runs do
    unless ← x do
      return false
  return true

def randNatM (lo hi : Nat) : RandT γ m Nat :=
  modifyGet fun g => randNat g lo hi

def randBoolM : RandT γ m Bool :=
  modifyGet randBool

-- We know `NeedsKind.all.size > 0`.
protected def NeedsKind.randM : RandT γ m NeedsKind :=
  return NeedsKind.all[← randNatM 0 (NeedsKind.all.size - 1)]!

protected def Bitset.randM (univSize : Nat) : RandT γ m Bitset :=
  return { toNat := ← randNatM 0 (2 ^ univSize - 1) } -- `hi` is inclusive in `randNat`

protected def Bitset.randNonemptyM (univSize : Nat) : RandT γ m Bitset :=
  return { toNat := ← randNatM 1 (2 ^ univSize - 1) } -- `hi` is inclusive in `randNat`

def Array.randSubset (arr : Array α) : RandT γ m (Array α) :=
  return (← Bitset.randM arr.size).extractArray arr

def Array.randNonemptySubset (arr : Array α) : RandT γ m (Array α) :=
  return (← Bitset.randNonemptyM arr.size).extractArray arr

/-- What random needs we allow. -/
inductive RandNeedsConfig where
| publicOnly (usePrivOfPriv := true)
| metaOnly (usePrivOfPriv := true)
| any (usePrivOfPriv := true)
deriving Inhabited, Repr

def RandNeedsConfig.usePrivOfPriv : RandNeedsConfig → Bool
  | publicOnly usePrivOfPriv => usePrivOfPriv
  | metaOnly usePrivOfPriv => usePrivOfPriv
  | any usePrivOfPriv => usePrivOfPriv

def RandNeedsConfig.useMeta : RandNeedsConfig → Bool
  | publicOnly _ => false
  | _ => true

def RandNeedsConfig.useNonMeta : RandNeedsConfig → Bool
  | metaOnly _ => false
  | _ => true

def RandNeedsConfig.randM : RandT γ m RandNeedsConfig := do
  let usePrivOfPriv ← randBoolM
  match ← randNatM 0 2 with
  | 0 => return .publicOnly usePrivOfPriv
  | 1 => return .metaOnly usePrivOfPriv
  | 2 => return .any usePrivOfPriv
  | _ => panic! "randNat returned value outside of intended range"

protected def Needs.randM (hierarchySize : Nat)
    (cfg : RandNeedsConfig := .any) :
    RandT γ m Needs :=
  return {
    priv := ← if cfg.useNonMeta then Bitset.randM hierarchySize else pure ∅
    pub := ← if cfg.useNonMeta then Bitset.randM hierarchySize else pure ∅
    metaPub := ← if cfg.useMeta then Bitset.randM hierarchySize else pure ∅
    metaPriv := ← if cfg.useMeta then Bitset.randM hierarchySize else pure ∅
    privOfPriv :=
      ← if cfg.useNonMeta && cfg.usePrivOfPriv then Bitset.randM hierarchySize else pure ∅
    metaPrivOfPriv :=
      ← if cfg.useMeta && cfg.usePrivOfPriv then Bitset.randM hierarchySize else pure ∅
  }

-- TODO: should maybe choose randomly how many are in each "layer"?
/-- A topologically sorted array of needs, i.e. one in which each `Needs` only refers to prior
ones. If the config is `none`, it's randomized each time. This allows for more module-tomodule
variation in quality, perhaps more like a real repo. -/
def Needs.randArrayM (hierarchySize : Nat)
    (cfg? : Option RandNeedsConfig := none) :
    RandT γ m (Array Needs) := do
  let mut ns := Array.emptyWithCapacity hierarchySize
  for i in 0...hierarchySize do
    let cfg ← cfg?.getDM RandNeedsConfig.randM
    let need ← Needs.randM i cfg
    ns := ns.push need
  return ns

/-- Assumes that `ns` in topologically sorted, i.e. that the needs refer only to indices existing
prior in the array. We don't use this more generally because usually there are other things we want
to do in the loop. -/
def ArrayHierarchy.ofArrayNeeds (ns : Array Needs) : ArrayHierarchy := Id.run do
  let mut transDeps := Array.emptyWithCapacity ns.size
  for h : i in 0...ns.size do
    let n := ns[i].linearize
    transDeps := transDeps.push <| transDeps⟦n⟧.reflexify i
  return transDeps

protected def ArrayHierarchy.randM (hierarchySize : Nat) (cfg? : Option RandNeedsConfig := none) :
    RandT γ m ArrayHierarchy :=
  return .ofArrayNeeds (← Needs.randArrayM hierarchySize cfg?)

def ArrayHierarchy.toString (h : ArrayHierarchy) (dividers := true) : String :=
  "\n".intercalate (h.zipIdx.map fun (n, idx) => n.toString (some (idx + 1)) dividers).toList

def testSeed : StdGen := mkStdGen 373737

-- Note how each row ends with `Needs.reflOf _`, and the `Needs` are linearized.
/--
info:

│⠇│
│⠘│⠇│
│⠃│⠀│⠇│
│⠟│⠚│⠛│⠇│
│⠿│⠷│⠶│⠲│⠇│
-/
#guard_msgs in
run_cmd
  let transDeps : ArrayHierarchy := Std.RandM.run' testSeed <| ArrayHierarchy.randM 5
  logInfo m!"\n\n{transDeps.toString}"

/-- A `Needs` which exhibits every set of `NeedsKind`s in one of its columns. -/
def allNeeds : Needs := Id.run do
  let mut needs := Needs.empty
  /- The column is both the index at which we modify `needs` and the "column" of the `Needs` as a
  bitset at that index. -/
  for col in 0...(2 ^ NeedsKind.all.size) do
    let colBitset := { toNat := col : Bitset }
    for kIdx in colBitset.highToLow do
      needs := needs.union NeedsKind.all[kIdx]! {col}
  return needs

/--
info: │⠀│⠁│⠂│⠃│⠈│⠉│⠊│⠋│⠐│⠑│⠒│⠓│⠘│⠙│⠚│⠛│⠄│⠅│⠆│⠇│⠌│⠍│⠎│⠏│⠔│⠕│⠖│⠗│⠜│⠝│⠞│⠟│⠠│⠡│⠢│⠣│⠨│⠩│⠪│⠫│⠰│⠱│⠲│⠳│⠸│⠹│⠺│⠻│⠤│⠥│⠦│⠧│⠬│⠭│⠮│⠯│⠴│⠵│⠶│⠷│⠼│⠽│⠾│⠿│
-/
#guard_msgs in
run_cmd logInfo allNeeds.toString

-- Post-composition is associative: `(n ≫ k₁) ≫ k₂ = n ≫ (k₁ ≫ k₂)`.
#guard NeedsKind.allPairs.all fun (k₁, k₂) =>
  if h : k₁.target = k₂.source then
    (allNeeds ≫ k₁) ≫ k₂ == allNeeds ≫ (k₁.andThen k₂)
  else true

end NeedsKind

section Linearization

/- Linearity means the upper and lower dots (public and private-of-private) in a given column
should never appear without the middle dot (private). -/
/--
info: │⠀│⠃│⠂│⠃│⠘│⠛│⠚│⠛│⠐│⠓│⠒│⠓│⠘│⠛│⠚│⠛│⠆│⠇│⠆│⠇│⠞│⠟│⠞│⠟│⠖│⠗│⠖│⠗│⠞│⠟│⠞│⠟│⠰│⠳│⠲│⠳│⠸│⠻│⠺│⠻│⠰│⠳│⠲│⠳│⠸│⠻│⠺│⠻│⠶│⠷│⠶│⠷│⠾│⠿│⠾│⠿│⠶│⠷│⠶│⠷│⠾│⠿│⠾│⠿│
-/
#guard_msgs in run_cmd logInfo allNeeds.linearize.toString

/- Antilinearity means that the middle dot (private) in a given column should *never* appear when
either of the other two dots appear in that column. -/
/--
info: │⠀│⠁│⠂│⠁│⠈│⠉│⠊│⠉│⠐│⠑│⠒│⠑│⠈│⠉│⠊│⠉│⠄│⠅│⠄│⠅│⠌│⠍│⠌│⠍│⠔│⠕│⠔│⠕│⠌│⠍│⠌│⠍│⠠│⠡│⠢│⠡│⠨│⠩│⠪│⠩│⠠│⠡│⠢│⠡│⠨│⠩│⠪│⠩│⠤│⠥│⠤│⠥│⠬│⠭│⠬│⠭│⠤│⠥│⠤│⠥│⠬│⠭│⠬│⠭│
-/
#guard_msgs in run_cmd logInfo allNeeds.antilinearize.toString

#guard allNeeds.linearize.isLinear
#guard !allNeeds.linearize.isAntilinear
#guard allNeeds.antilinearize.isAntilinear
#guard !allNeeds.antilinearize.isLinear

-- We expect `linearize` and `antilinearize` to not only be idempotent but to form a flip-flop
-- semigroup action, as there should always be enough information to recover the other.
#guard allNeeds.linearize.linearize == allNeeds.linearize
#guard allNeeds.linearize.antilinearize == allNeeds.antilinearize
#guard allNeeds.antilinearize.linearize == allNeeds.linearize
#guard allNeeds.antilinearize.antilinearize == allNeeds.antilinearize

-- `antilinearize n ⊆ n ⊆ linearize n`, and both preserve non-private scopes (and respect those
-- inclusions in their private scopes).
#guard allNeeds.antilinearize.directLe allNeeds
#guard allNeeds.directLe allNeeds.linearize
#guard
  let linearized := allNeeds.linearize
  let antilinearized := allNeeds.antilinearize
  NeedsKind.all.all fun k =>
    if k matches .priv || k matches .metaPriv then
      antilinearized.get k ⊆ allNeeds.get k && allNeeds.get k ⊆ linearized.get k
    else
      antilinearized.get k = allNeeds.get k && allNeeds.get k = linearized.get k

end Linearization

section Reflexification

-- `reflexify` adds all non-meta scopes to the column of `Needs` at the specified index
/-- info: │⠀│⠂│⠀│⠇│ -/
#guard_msgs in run_cmd logInfo (Needs.empty.union .priv {1} |>.reflexify 3 |>.toString)

/-- info: │⠀│⠀│⠀│⠇│ -/
#guard_msgs in run_cmd logInfo (Needs.reflOf 3 |>.toString)

/-- info: │⠀│⠂│⠀│⠀│ -/
#guard_msgs in run_cmd
  logInfo (Needs.empty.union .priv {1} |>.reflexify 3 |>.clearAt 3 |>.toString (some 4))

end Reflexification

section TransitiveClosure

open Elab Command

-- Test that the test is running!
/--
info:

│⠇│
│⠘│⠇│
│⠃│⠀│⠇│
│⠟│⠚│⠛│⠇│
│⠿│⠷│⠶│⠲│⠇│
-/
#guard_msgs in
run_cmd
  let testTest : Std.RandT CommandElabM Bool := do
    let h : ArrayHierarchy ← ArrayHierarchy.randM 5
    logInfo s!"\n\n{h.toString}"
    return true
  unless ← testTest.all 1 |>.run' testSeed do
    throwError "Somehow, returned false"

-- The generated hierarchies are reflexified, linearized, and transitively closed, and acting on
-- `Needs` with these hierarchies behaves as expected.
#guard_msgs in
run_cmd
  let test : Std.RandT CommandElabM Bool := do
    let size := 30
    let h ← ArrayHierarchy.randM size
    unless h.size = size do
      throwError "Incorrect hierarchy size"
    for h' : idx in 0...h.size do
      unless h[idx].isLinear do
        throwError "Not linear at {idx}:\n{h.toString}"
      unless Needs.reflOf idx |>.directLe h[idx] do
        throwError "Not reflexified at {idx}:\n{h.toString}"
      unless h⟦h[idx]⟧ == h[idx] do
        throwError "Not transitively closed at {idx}:\n{h.toString}"
    let needs ← Needs.randM size
    unless needs.directLe h⟦needs⟧ do
      throwError "Random needs is not subset of its transitive closure:\n{needs.toString}\n\n\
        {h.toString}"
    unless h⟦h⟦needs⟧⟧ == h⟦needs⟧ do
      throwError "Transitive closure is not idempotent:\n{needs.toString}\n\n\
        {h.toString}"
    unless h⟦needs⟧.linearize == h⟦needs.linearize⟧ do
      throwError "Transitive closure does not commute with linearization:\n{needs.toString}\n\n\
        {h.toString}"
    unless h ≫ needs == h⟦needs⟧ do
      -- TODO: does this mean the definition should change?
      throwError "Transitive closure is not the same as composition:\n{needs.toString}\n\n\
        {h.toString}"
    let aggregateByPrearrows := needs.foldWithKind (init := ∅) fun acc k b => Id.run do
      let mut acc := acc
      for i in b.highToLow do
        acc := acc ∪ h⟦(i, k)⟧
      return acc
    unless h⟦needs⟧ == aggregateByPrearrows do
      throwError "Transitive closure is not the same as transitively closing each prearrow:\n\
        {needs.toString}\n\n{h.toString}"
    return true
  unless ← test.all 100 |>.run' testSeed do
    throwError "Tests failed."

end TransitiveClosure

section Reduction

/-- Whether dropping any single prearrow of `reduced` loses coverage of `a`. Coverage is monotone
in `reduced`, so this is exactly `⊆`-minimality. -/
meta def Needs.isMinimalSubsumerOf (reduced a : Needs) (transDeps : ArrayHierarchy) : Bool :=
  reduced.allWithKind fun k b => b.all fun i =>
    !a.subsumedBy (reduced.sub k {i}) transDeps

open Elab Command

#guard_msgs in
run_cmd
  let test : Std.RandT CommandElabM Bool := do
    let size := 30
    let h ← ArrayHierarchy.randM size
    let needs ← Needs.randM size
    let reduced := needs.reduce h
    unless reduced.isAntilinear do
      throwError "Reduced needs are not antilinear:\n\n\
        reduced: {reduced}\n\nneeds: {needs}\n\n{h}"
    unless reduced.directLe needs.linearize do
      throwError "Reduced needs are not included within linearized version of needs:\n\n\
        reduced: {reduced}\n\nneeds: {needs}\n\n{h}"
    unless needs.subsumedBy reduced h do
      throwError "Reduced needs do not subsume needs:\n\n\
        reduced: {reduced}\n\nneeds: {needs}\n\n{h}"
    let isMinimal := reduced.allWithKind fun k b => b.all fun i =>
      !needs.subsumedBy (reduced.sub k {i}) h
    unless isMinimal do
      throwError "Reduced needs are not minimal:\n\n\
        reduced: {reduced}\n\nneeds: {needs}\n\n{h}"
    unless needs.linearize.reduce h == reduced do
      throwError "Reduced needs changed with linearization:\n\n\
        reduced: {reduced}\n\nneeds: {needs}\n\n{h}"
    unless reduced.reduce h == reduced do
      throwError "Reduction is not idempotent:\n\n\
        reduced: {reduced}\n\nneeds: {needs}\n\n{h}"
    return true
  unless ← test.all 100 |>.run' testSeed do
    throwError "Tests failed."

end Reduction

section Minimals

/-- info: #[◻◻◻, ◼◻◻, ◻◼◻, ◼◼◻, ◻◻◼, ◼◻◼, ◻◼◼, ◼◼◼] ↦ #[◻◻◻] -/
#guard_msgs in
run_cmd do
  let data := (0...2^3).toArray.map Bitset.mk
  if h : data ≠ #[] then
    let univSize := Array.max (data.map (·.univSize)) (by simp; grind)
    logInfo s!"{data.map (·.toString univSize)} ↦ \
      {data.minimals (· ⊆ ·) |>.map (·.toString univSize)}"

/-- info: #[◼◻◻, ◻◼◻, ◼◼◻, ◻◻◼, ◼◻◼, ◻◼◼, ◼◼◼] ↦ #[◼◻◻, ◻◼◻, ◻◻◼] -/
#guard_msgs in
run_cmd do
  let data := (1...2^3).toArray.map Bitset.mk
  if h : data ≠ #[] then
    let univSize := Array.max (data.map (·.univSize)) (by simp; grind)
    logInfo s!"{data.map (·.toString univSize)} ↦ \
      {data.minimals (· ⊆ ·) |>.map (·.toString univSize)}"

/-- info: #[◼◼◻, ◼◻◼, ◻◼◼, ◼◼◼] ↦ #[◼◼◻, ◼◻◼, ◻◼◼] -/
#guard_msgs in
run_cmd do
  let data := (1...2^3).toArray.map Bitset.mk
  let data := data.filter (·.size > 1)
  if h : data ≠ #[] then
    let univSize := Array.max (data.map (·.univSize)) (by simp; grind)
    logInfo s!"{data.map (·.toString univSize)} ↦ \
      {data.minimals (· ⊆ ·) |>.map (·.toString univSize)}"

/-- info: #[◼◼◻, ◼◻◼, ◻◼◼, ◼◼◼, ◼◼◻, ◼◻◼, ◻◼◼, ◼◼◼, ◼◼◻, ◼◻◼, ◻◼◼, ◼◼◼] ↦ #[◼◼◻, ◼◻◼, ◻◼◼] -/
#guard_msgs in
run_cmd do
  let data := (1...2^3).toArray.map Bitset.mk
  let data := data ++ data ++ data
  let data := data.filter (·.size > 1)
  if h : data ≠ #[] then
    let univSize := Array.max (data.map (·.univSize)) (by simp; grind)
    logInfo s!"{data.map (·.toString univSize)} ↦ \
      {data.minimals (· ⊆ ·) |>.map (·.toString univSize)}"

end Minimals
