/-
Copyright (c) 2026 Thomas Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas Murrills
-/
module

public import Lean.Data.PersistentHashSet

namespace ImportGraph

open Lean

/-- Iterates through the first set, inserting elements into the second set unless they exist
already. -/
public def Lean.PHashSet.union {α} [BEq α] [Hashable α] (as bs : PHashSet α) :
    PHashSet α := Id.run do
  let mut bs := bs
  for a in as do
    unless bs.contains a do
      bs := bs.insert a
  return bs
