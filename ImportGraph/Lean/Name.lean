/-
Copyright (c) 2023 Jon Eugster. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jon Eugster
-/
module

import Lean.Meta.Match.MatcherInfo
public import Lean.CoreM

/-!
TODO: Some declarations in this file are duplicated from mathlib, but especially `isBlacklisted`
is deemed to specific for upstreaming to Batteries.
-/
namespace ImportGraph

open Lean Meta Elab

namespace Lean.Name

/-- Note: copied from `Mathlib.Lean.Name` -/
private def isBlackListed (declName : Name) : CoreM Bool := do
  if declName.toString.startsWith "Lean" then return true
  let env ← getEnv
  pure $ declName.isInternalDetail
   || isAuxRecursor env declName
   || isNoConfusion env declName
  <||> isRec declName <||> isMatcher declName

/--
Retrieve all names in the environment satisfying a predicate.

Note: copied from `Mathlib.Lean.Name`
-/
public def allNames (p : Name → Bool) : CoreM (Array Name) := do
  (← getEnv).constants.foldM (init := #[]) fun names n _ => do
    if p n && !(← isBlackListed n) then
      return names.push n
    else
      return names

/--
Retrieve all names in the environment satisfying a predicate,
gathered together into a `HashMap` according to the module they are defined in.

Note: copied from `Mathlib.Lean.Name`
-/
public def allNamesByModule (p : Name → Bool) : CoreM (Std.HashMap Name (Array Name)) := do
  (← getEnv).constants.foldM (init := ∅) fun names n _ => do
    if p n && !(← isBlackListed n) then
      let some m ← findModuleOf? n | return names
      -- TODO use `modify`/`alter` when available
      match names[m]? with
      | some others => return names.insert m (others.push n)
      | none => return names.insert m #[n]
    else
      return names

end ImportGraph.Lean.Name

section deprecated

namespace Lean.Name.ImportGraph

@[deprecated ImportGraph.Lean.Name.allNamesByModule (since := "2026-11-14")]
public abbrev allNamesByModule := ImportGraph.Lean.Name.allNamesByModule

@[deprecated ImportGraph.Lean.Name.allNames (since := "2026-11-14")]
public abbrev allNames := ImportGraph.Lean.Name.allNames

/-- Returns the very first part of a name: for `ImportGraph.Lean.NameMap` it
returns `ImportGraph`. -/
@[deprecated Lean.Name.getRoot +typeChanged (since := "2026-11-14")]
public def getModule (name : Name) (s := "") : Name :=
  match name with
    | .anonymous => .mkSimple s
    | .num _ _ => panic s!"panic in `getModule`: did not expect numerical name: {name}."
    | .str pre s => getModule pre s

end Lean.Name.ImportGraph
