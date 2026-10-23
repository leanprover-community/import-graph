/-
Copyright (c) 2026 Thomas Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas Murrills
-/
module

namespace ImportGraph

/-- Essentially `(as ++ bs).deleteDuplicatesRev`, keeping later-occurring elements. -/
public def List.prependWithoutDuplicating {α} [BEq α] (as bs : List α) : List α :=
  match as with
  | [] => bs
  | a :: as =>
    let new := List.prependWithoutDuplicating as bs
    if new.contains a then new else a :: new
