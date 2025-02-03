/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Lean.CoreM
import Lean.Data.NameMap
import Lean.Environment
import Lean.Util.FoldConsts
import Batteries.Data.NameSet

namespace Lean

/-- Return the name of the module in which a declaration was defined. -/
def Environment.getModuleFor? (env : Environment) (declName : Name) : Option Name :=
  match env.getModuleIdxFor? declName with
  | none =>
    if env.constants.map₂.contains declName then
      env.header.mainModule
    else
      none
  | some idx => env.header.moduleNames[idx.toNat]!

open Lean

/--
Return the names of the modules in which constants used in the specified declaration were defined.

Note that this will *not* account for tactics and syntax used in the declaration,
so the results may not suffice as imports.
-/
def Name.requiredModules (n : Name) : CoreM NameSet := do
  let env ← getEnv
  let mut requiredModules : NameSet := {}
  let ci ← getConstInfo n
  for n in ci.getUsedConstantsAsSet do
    match env.getModuleFor? n with
    | some m =>
      if ¬ (`Init).isPrefixOf m then
        requiredModules := requiredModules.insert m
    | none => pure ()
  return requiredModules

/--
Return the names of the constants used in the specified declarations,
and the constants they use transitively.
-/
def NameSet.transitivelyUsedConstants (s : NameSet) : CoreM NameSet := do
  let mut usedConstants : NameSet := {}
  let mut toProcess : NameSet := s
  while !toProcess.isEmpty do
    let current := toProcess.min.get!
    toProcess := toProcess.erase current
    usedConstants := usedConstants.insert current
    for m in (← getConstInfo current).getUsedConstantsAsSet do
      if !usedConstants.contains m then
        toProcess := toProcess.insert m
  return usedConstants

/--
Return the names of the constants used in the specified declaration,
and the constants they use transitively.
-/
def Name.transitivelyUsedConstants (n : Name) : CoreM NameSet :=
  NameSet.transitivelyUsedConstants {n}

/--
Return the names of the modules in which constants used transitively
in the specified declarations were defined.

Note that this will *not* account for tactics and syntax used in the declaration,
so the results may not suffice as imports.
-/
def NameSet.transitivelyRequiredModules (s : NameSet) (env : Environment) : CoreM NameSet := do
  let mut requiredModules : NameSet := {}
  for m in (← s.transitivelyUsedConstants) do
    if let some module := env.getModuleFor? m then
      requiredModules := requiredModules.insert module
  return requiredModules

/--
Return the names of the modules in which constants used transitively
in the specified declaration were defined.

Note that this will *not* account for tactics and syntax used in the declaration,
so the results may not suffice as imports.
-/
def Name.transitivelyRequiredModules (n : Name) (env : Environment) : CoreM NameSet :=
  NameSet.transitivelyRequiredModules {n} env

/--
Finds all constants defined in the specified module,
and identifies all modules containing constants which are transitively required by those constants.
-/
def Environment.transitivelyRequiredModules (env : Environment) (module : Name) : CoreM NameSet := do
  let constants := env.constants.map₁.values.map (·.name)
    |>.filter (! ·.isInternal)
    |>.filter (env.getModuleFor? · = some module)
  (NameSet.ofList constants).transitivelyRequiredModules env

/--
Computes all the modules transitively required by the specified modules.
Should be equivalent to calling `transitivelyRequiredModules` on each module, but shares more of the work.
-/
partial def Environment.transitivelyRequiredModules' (env : Environment) (modules : List Name) (verbose : Bool := false) :
    CoreM (NameMap NameSet) := do
  let N := env.header.moduleNames.size
  let mut c2m : NameMap (BitVec N) := {}
  let mut pushed : NameSet := {}
  let mut result : NameMap NameSet := {}
  for m in modules do
    if verbose then
      IO.println s!"Processing module {m}"
    let mut r : BitVec N := 0
    for n in env.header.moduleData[(env.header.moduleNames.idxOf? m).getD 0]!.constNames do
      if ! n.isInternal then
      -- This is messy: Mathlib is big enough that writing a recursive function causes a stack overflow.
      -- So we use an explicit stack instead. We visit each constant twice:
      -- once to record the constants transitively used by it,
      -- and again to record the modules which defined those constants.
      let mut stack : List (Name × Option NameSet) := [⟨n, none⟩]
      pushed := pushed.insert n
      while !stack.isEmpty do
        match stack with
        | [] => panic! "Stack is empty"
        | (c, used?) :: tail =>
          stack := tail
          match used? with
          | none =>
            if !c2m.contains c then
              let used := (← getConstInfo c).getUsedConstantsAsSet
              stack := ⟨c, some used⟩ :: stack
              for u in used do
                if !pushed.contains u then
                  stack := ⟨u, none⟩ :: stack
                  pushed := pushed.insert u
          | some used =>
            let usedModules : NameSet :=
              used.fold (init := {}) (fun s u => if let some m := env.getModuleFor? u then s.insert m else s)
            let transitivelyUsed : BitVec N :=
              used.fold (init := toBitVec usedModules) (fun s u => s ||| ((c2m.find? u).getD 0))
            c2m := c2m.insert c transitivelyUsed
      r := r ||| ((c2m.find? n).getD 0)
    result := result.insert m (toNameSet r)
  return result
where
  toBitVec {N : Nat} (s : NameSet) : BitVec N :=
    s.fold (init := 0) (fun b n => b ||| BitVec.twoPow _ ((env.header.moduleNames.idxOf? n).getD 0))
  toNameSet {N : Nat} (b : BitVec N) : NameSet :=
    env.header.moduleNames.zipIdx.foldl (init := {}) (fun s (n, i) => if b.getLsbD i then s.insert n else s)

/--
Return the names of the modules in which constants used in the current file were defined.

Note that this will *not* account for tactics and syntax used in the file,
so the results may not suffice as imports.
-/
def Environment.requiredModules (env : Environment) : NameSet := Id.run do
  let localConstantInfos := env.constants.map₂
  let mut requiredModules : NameSet := {}
  for (_, ci) in localConstantInfos do
    for n in ci.getUsedConstantsAsSet do
      match env.getModuleFor? n with
      | some m =>
        if ¬ (`Init).isPrefixOf m then
          requiredModules := requiredModules.insert m
      | none => pure ()
  return requiredModules

end Lean
