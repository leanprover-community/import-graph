/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Lean


namespace Lean.NameSet

instance : Singleton Name NameSet where
  singleton := fun n => (∅ : NameSet).insert n

instance : Union NameSet where
  union := fun s t => s.fold (fun t n => t.insert n) t

instance : Inter NameSet where
  inter := fun s t => s.fold (fun r n => if t.contains n then r.insert n else r) {}

instance : SDiff NameSet where
  sdiff := fun s t => t.fold (fun s n => s.erase n) s

def ofList (l : List Name) : NameSet :=
  l.foldl (fun s n => s.insert n) {}

def ofArray (a : Array Name) : NameSet :=
  a.foldl (fun s n => s.insert n) {}

end Lean.NameSet

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
partial def Environment.transitivelyRequiredModules' (env : Environment) (modules : List Name) :
    CoreM (NameMap NameSet) := do
  let mut c2m : NameMap NameSet := {}
  let mut result : NameMap NameSet := {}
  for m in modules do
    let mut r : NameSet := {}
    for n in env.header.moduleData[(env.header.moduleNames.getIdx? m).getD 0]!.constNames do
      -- This is messy: Mathlib is big enough that writing a recursive function causes a stack overflow.
      -- So we use an explicit stack instead. We visit each constant twice:
      -- once to record the constants transitively used by it,
      -- and again to record the modules which defined those constants.
      let mut stack : Array (ConstantInfo × Option NameSet) := #[⟨← getConstInfo n, none⟩]
      while !stack.isEmpty do
        let (ci, used?) := stack.back
        stack := stack.pop
        match used? with
        | none =>
          if !c2m.contains ci.name then
            let used := ci.getUsedConstantsAsSet
            stack := stack.push ⟨ci, some used⟩
            for u in used do
              if !c2m.contains u then
                stack := stack.push ⟨← getConstInfo u, none⟩
        | some used =>
          let transitivelyUsed : NameSet := used.fold (init := used) (fun s u => s.union ((c2m.find? u).getD {}))
          c2m := c2m.insert ci.name transitivelyUsed
      r := r.union ((c2m.find? n).getD {})
    result := result.insert m r
  return result

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
