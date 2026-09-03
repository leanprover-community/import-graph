/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills

Copyright (c) 2023 Mario Carneiro. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mario Carneiro, Sebastian Ullrich
-/
module

public import Lean.Environment
public import Lean.Setup

/-!
# `shake` core types

This file copies types and functions from `Lake.CLI.Shake` wholesale in order to make them public,
with some modifications to `Needs` in order to suit our purposes (namely, we benefit from keeping
track of private dependence in order to uniformly handle declarations from the same file, whereas
shake can get away with handling `import all` specially.)

More utilities on these types are defined in `ImportGraph.Shake.Basic`.
-/

namespace ImportGraph.Shake

open Lean

public section

/-!
## Bitset

This section is copied without modification, and should be removed if Shake API becomes public.
-/

/-- We use `Nat` as a bitset for doing efficient set operations.
The bit indexes will usually be a module index. -/
structure Bitset where
  toNat : Nat
deriving Inhabited, DecidableEq, Repr

namespace Bitset

instance : EmptyCollection Bitset where
  emptyCollection := { toNat := 0 }

instance : Insert Nat Bitset where
  insert i s := { toNat := s.toNat ||| (1 <<< i) }

instance : Singleton Nat Bitset where
  singleton i := insert i ∅

instance : Inter Bitset where
  inter a b := { toNat := a.toNat &&& b.toNat }

instance : Union Bitset where
  union a b := { toNat := a.toNat ||| b.toNat }

instance : XorOp Bitset where
  xor a b := { toNat := a.toNat ^^^ b.toNat }

def has (s : Bitset) (i : Nat) : Bool := s.toNat.testBit i

end Bitset

/-!
# Needs and NeedsKind

This section is modified from shake's version to allow us to reason about `import all`s in the
import hierarchy instead of dynamically.
-/

/-- An "atomic" module dependency. Note that we consider an inclusion of the public scope and an
inclusion of the private scope to be two separate dependencies. Modules may be related by multiple
`NeedsKind`s; the presence of any `NeedsKind` implies some dependency between the modules.  -/
structure NeedsKind where
  /-- Represents `public (meta)? import`: an import of the public scope of the source into the
  public scope of the target. `false` means that the public scope of the source is instead imported
  into the private scope of the target. -/
  isExported : Bool
  /-- Represents `(public)? meta import (all)?`: a lifting of non-meta declarations to the meta
  phase. -/
  isMeta : Bool
  /-- Represents `(meta)? import all`: an import of the private scope into the private scope.
  `false` means the private scope of the source is not accessed. -/
  isAll : Bool := false
  not_isExported_and_isAll : ¬(isExported ∧ isAll) := by grind
deriving Inhabited, BEq, Repr, Hashable, Ord

namespace NeedsKind

@[match_pattern] abbrev priv : NeedsKind := { isExported := false, isMeta := false }
@[match_pattern] abbrev pub  : NeedsKind := { isExported := true,  isMeta := false }
@[match_pattern] abbrev metaPriv : NeedsKind := { isExported := false, isMeta := true }
@[match_pattern] abbrev metaPub  : NeedsKind := { isExported := true,  isMeta := true }
@[match_pattern] abbrev privOfPriv  : NeedsKind :=
  { isExported := false,  isMeta := false, isAll := true }
@[match_pattern] abbrev metaPrivOfPriv : NeedsKind :=
  { isExported := false, isMeta := true, isAll := true }
/- Note that since the private scope cannot be needed in the public scope, we do not consider the
cases with `isExported := true` and `isAll := true`. -/

def NeedsKind.setIsAll (k : NeedsKind) : NeedsKind :=
  { isMeta := k.isMeta, isExported := false, isAll := true }

def NeedsKind.unsetIsAll (k : NeedsKind) : NeedsKind :=
  { isMeta := k.isMeta, isExported := k.isExported, isAll := false }

@[expose] def all : Array NeedsKind := #[pub, priv, metaPub, metaPriv, privOfPriv, metaPrivOfPriv]

/-- Assumes that `¬(imp.isExported ∧ imp.importAll)`. -/
def ofImport : Lean.Import → NeedsKind
  | { isExported := true, isMeta := true, .. } => .metaPub
  | { isExported := true, isMeta := false, .. } => .pub
  | { isExported := false, isMeta := true,  importAll := false, .. } => .metaPriv
  | { isExported := false, isMeta := false, importAll := false, .. } => .priv
  | { isExported := false, isMeta := false, importAll := true,  .. } => .privOfPriv
  | { isExported := false, isMeta := true,  importAll := true,  .. } => .metaPrivOfPriv

end NeedsKind

/-- Logically, a map `NeedsKind → Set <modules>`. -/
structure Needs where
  pub : Bitset
  priv : Bitset
  metaPub : Bitset
  metaPriv : Bitset
  privOfPriv : Bitset
  metaPrivOfPriv : Bitset
deriving Inhabited, Repr

/-- An abbreviation for `Needs`, but intended to express what is provided to a given scope.
Notably, we expect this to be both transitively closed and linearized (i.e., to have also taken
`public` ⊆ `private` into account) but do not enforce this for ease of building up `Provides`. -/
abbrev Provides := Needs

def Needs.empty : Needs := {
  pub := ∅
  priv := ∅
  metaPub := ∅
  metaPriv := ∅
  privOfPriv := ∅
  metaPrivOfPriv := ∅
}

instance : EmptyCollection Needs := ⟨.empty⟩

@[inline, expose] def Needs.get (needs : Needs) (k : NeedsKind) : Bitset :=
  match k with
  | .pub => needs.pub
  | .priv => needs.priv
  | .metaPub => needs.metaPub
  | .metaPriv => needs.metaPriv
  | .privOfPriv => needs.privOfPriv
  | .metaPrivOfPriv => needs.metaPrivOfPriv

@[inline, expose] def Needs.has (needs : Needs) (k : NeedsKind) (i : ModuleIdx) : Bool :=
  needs.get k |>.has i

@[inline, expose] def Needs.set (needs : Needs) (k : NeedsKind) (s : Bitset) : Needs :=
  match k with
  | .pub   => { needs with pub := s }
  | .priv  => { needs with priv := s }
  | .metaPub => { needs with metaPub := s }
  | .metaPriv => { needs with metaPriv := s }
  | .privOfPriv => { needs with privOfPriv := s }
  | .metaPrivOfPriv => { needs with metaPrivOfPriv := s }

def Needs.modify (needs : Needs) (k : NeedsKind) (f : Bitset → Bitset) : Needs :=
  needs.set k (f (needs.get k))

def Needs.union (needs : Needs) (k : NeedsKind) (s : Bitset) : Needs :=
  needs.modify k (· ∪ s)

def Needs.sub (needs : Needs) (k : NeedsKind) (s : Bitset) : Needs :=
  needs.modify k (fun s' => s' ^^^ (s' ∩ s))

instance : Union Needs where
  union a b := {
    pub := a.pub ∪ b.pub
    priv := a.priv ∪ b.priv
    metaPub := a.metaPub ∪ b.metaPub
    metaPriv := a.metaPriv ∪ b.metaPriv
    privOfPriv := a.privOfPriv ∪ b.privOfPriv
    metaPrivOfPriv := a.metaPrivOfPriv ∪ b.metaPrivOfPriv }
