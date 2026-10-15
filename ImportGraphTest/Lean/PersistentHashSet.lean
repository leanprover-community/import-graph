/-
Copyright (c) 2026 Dionisio Chiuratto Agourakis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Dionisio Chiuratto Agourakis
-/
module

public meta import ImportGraph.Lean.PersistentHashSet

/-! # Tests for the `Lean.PersistentHashSet` extension functions -/

open Lean

/-- Build a `PersistentHashSet` from a list, for testing. -/
private meta def testSet (xs : List Nat) : PersistentHashSet Nat :=
  xs.foldl (init := {}) fun s a => s.insert a

-- The union contains the elements of both sets.
/-- info: true -/
#guard_msgs in
#eval ((testSet [1, 2]).union (testSet [2, 3])).contains 1

/-- info: true -/
#guard_msgs in
#eval ((testSet [1, 2]).union (testSet [2, 3])).contains 2

/-- info: true -/
#guard_msgs in
#eval ((testSet [1, 2]).union (testSet [2, 3])).contains 3

-- Shared elements are not duplicated.
/-- info: true -/
#guard_msgs in
#eval ((testSet [1, 2]).union (testSet [2, 3])).toList.length == 3

-- The union with the empty set contains exactly the original elements.
/-- info: true -/
#guard_msgs in
#eval ((testSet [1, 2]).union {}).toList.length == 2

/-- info: true -/
#guard_msgs in
#eval (({} : PersistentHashSet Nat).union (testSet [1, 2])).toList.length == 2

-- The union of a set with itself contains each element exactly once.
/-- info: true -/
#guard_msgs in
#eval ((testSet [1, 2, 3]).union (testSet [1, 2, 3])).toList.length == 3
