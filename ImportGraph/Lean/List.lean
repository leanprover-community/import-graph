/-
Copyright (c) 2026 Dionisio Chiuratto Agourakis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Dionisio Chiuratto Agourakis
-/
module

public import Init

/-! # Additional API for `List` -/

public section

namespace List

/-- Essentially `(as ++ bs).deleteDuplicatesRev`, keeping later-occurring elements. -/
def prependWithoutDuplicating [BEq α] (as bs : List α) : List α :=
  match as with
  | [] => bs
  | a :: as =>
    let new := prependWithoutDuplicating as bs
    if new.contains a then new else a :: new

end List
