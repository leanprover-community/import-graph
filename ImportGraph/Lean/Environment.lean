/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
module

public import Lean.Environment

namespace Lean

/-- Return the name of the module in which a declaration was defined.
Returns the main module for declarations defined in the current environment. -/
public def Environment.getModuleFor? (env : Environment) (declName : Name) (skipRealize := false) :
    Option Name :=
  match env.getModuleIdxFor? declName with
  | none =>
    if env.findAsync? declName skipRealize |>.isSome then
      env.header.mainModule
    else none
  | some idx => env.header.moduleNames[idx.toNat]!

@[inline]
public def Environment.getModuleIdx! (env : Environment) (moduleName : Name) : ModuleIdx :=
  env.getModuleIdx? moduleName |>.get!

end Lean
