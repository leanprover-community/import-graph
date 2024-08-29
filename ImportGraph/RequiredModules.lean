/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Lean

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
Return the names of the constants used in the specified declaration,
and the constants they use transitively.
-/
def Name.transitivelyUsedConstants (n : Name) : CoreM NameSet := do
  let mut usedConstants : NameSet := {}
  let mut thisBatch : NameSet := NameSet.empty.insert n
  let mut nextBatch : NameSet := {}
  let mut done : NameSet := {}
  while !thisBatch.isEmpty do
    for n in thisBatch do
      done := done.insert n
      for m in (← getConstInfo n).getUsedConstantsAsSet do
        if !done.contains m then
          usedConstants := usedConstants.insert m
          nextBatch := nextBatch.insert m
    thisBatch := nextBatch
    nextBatch := {}
  return usedConstants

/--
Return the names of the modules in which constants used transitively
in the specified declaration were defined.

Note that this will *not* account for tactics and syntax used in the declaration,
so the results may not suffice as imports.
-/
def Name.transitivelyRequiredModules (n : Name) : CoreM NameSet := do
  let env ← getEnv
  let mut requiredModules : NameSet := {}
  for m in (← n.transitivelyUsedConstants) do
    match env.getModuleFor? m with
    | some m =>
      if ¬ (`Init).isPrefixOf m then
        requiredModules := requiredModules.insert m
    | none => pure ()
  return requiredModules

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
