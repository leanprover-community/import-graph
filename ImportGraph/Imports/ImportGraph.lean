/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public import Lean.Environment
public import Lean.Data.NameMap.Basic

namespace Lean.Environment

/--
Find the imports of a given module.
-/
public def importsOf (env : Environment) (n : Name) : Array Name :=
  let imports :=
    if n = env.header.mainModule then
      env.header.imports.map Import.module
    else match env.getModuleIdx? n with
      | .some idx => env.header.moduleData[idx.toNat]!.imports.map Import.module
      | .none => #[]
  -- Note: we use `filter` rather than `erase`, since module-system files may contain
  -- both an implicit `public import Init` and a `meta import Init`, so `Init` can
  -- appear more than once in the parsed imports.
  imports.filter (· != `Init)

/--
Construct the import graph of the current file.
-/
public partial def importGraph (env : Environment) : NameMap (Array Name) :=
  let main := env.header.mainModule
  let imports := env.importsOf main
  imports.foldl (fun m i => process env i m) (({} : NameMap _).insert main imports)
    |>.erase Name.anonymous
where
  process (env) (i) (m) : NameMap (Array Name) :=
    if m.contains i then
      m
    else
      let imports := env.importsOf i
      imports.foldr (fun i m => process env i m) (m.insert i imports)
