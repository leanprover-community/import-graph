/-
Copyright (c) 2026 Dionisio Chiuratto Agourakis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Dionisio Chiuratto Agourakis
-/
module

public meta import ImportGraph.Lean.List

/-! # Tests for the `List` extension functions -/

-- Prepending disjoint lists is plain concatenation.
/-- info: true -/
#guard_msgs in
#eval List.prependWithoutDuplicating [1, 2] [3, 4] == [1, 2, 3, 4]

-- Dot notation on the first list is supported.
/-- info: true -/
#guard_msgs in
#eval [1, 2].prependWithoutDuplicating [3, 4] == [1, 2, 3, 4]

-- Elements of the second list are never duplicated.
/-- info: true -/
#guard_msgs in
#eval List.prependWithoutDuplicating [1, 2] [2, 3] == [1, 2, 3]

-- Duplicates within the first list itself are removed, keeping the last occurrence.
/-- info: true -/
#guard_msgs in
#eval List.prependWithoutDuplicating [1, 1, 2] [] == [1, 2]

-- Prepending to the empty list is the identity.
/-- info: true -/
#guard_msgs in
#eval List.prependWithoutDuplicating [1, 2, 3] [] == [1, 2, 3]

-- Prepending the empty list is the identity on the second argument.
/-- info: true -/
#guard_msgs in
#eval List.prependWithoutDuplicating ([] : List Nat) [1, 2] == [1, 2]
