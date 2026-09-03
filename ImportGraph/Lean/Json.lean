/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lean.Data.Json

/-! # Extra `ToJson` and `FromJson` instances -/

open Lean

public section

instance : ToJson UInt32 where
  toJson uint := uint.toNat

instance : FromJson UInt32 where
  fromJson? uint := fromJson? (α := Nat) uint |>.map .ofNat

deriving instance ToJson, FromJson, Repr for IO.Error
