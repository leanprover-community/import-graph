/-
Copyright (c) 2026 Dionisio Chiuratto Agourakis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Dionisio Chiuratto Agourakis
-/
module

public import Lean.Data.PersistentHashSet

/-! # Additional API for `Lean.PersistentHashSet` (a.k.a. `Lean.PHashSet`) -/

public section

namespace Lean.PersistentHashSet

/-- Iterates through the first set, inserting elements into the second set unless they exist
already. -/
def union {α} [BEq α] [Hashable α] (as bs : PersistentHashSet α) : PersistentHashSet α :=
  Id.run do
    let mut bs := bs
    for a in as do
      unless bs.contains a do
        bs := bs.insert a
    return bs

end Lean.PersistentHashSet
