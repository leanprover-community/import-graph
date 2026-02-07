/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public meta import Lean.Elab.Command
public meta import Lean.Widget.UserWidget
public meta import ImportGraph.Imports.ImportGraph

public meta section

open Lean

/-- `#import_diff foo bar ...` computes the new transitive imports that are added to a given file when
modules `foo, bar, ...` are added to the set of imports of the file. More precisely, it computes the
import diff between when `foo, bar, ...` are added to the imports and when `foo, bar, ...` are removed
from the imports.

Note: the command also works when some of the modules passed as arguments are already present in the file's
imports. -/
elab "#import_diff" n:ident* : command => do
  let name_arr : Array Name := n.map (·.getId)
  let sp ← searchPathRef.get
  -- First, make sure the files exist.
  for name in name_arr do
    if (← sp.findWithExt "olean" name).isSome then continue
    throwError m!"File {name} cannot be found."
  let env ← getEnv
  -- Next, check for redundancies:
  let current_all_imports := env.allImportedModuleNames
  let redundancies := name_arr.filter current_all_imports.contains
  unless redundancies.isEmpty do
    let out := "\n".intercalate <| redundancies.map Name.toString |>.qsort (· < ·) |>.toList
    Lean.logInfo <| m!"The following are already imported (possibly transitively):\n{out}"
  -- Now compute the import diffs.
  let current_imports := env.imports
  let reduced_imports := env.imports.filter (!name_arr.contains ·.module)
  let extended_imports := current_imports ++ (name_arr.map ({ module := · }))
  let reduced_all_imports := (← Lean.importModules reduced_imports {}).allImportedModuleNames
  let extended_all_imports := (← Lean.importModules extended_imports {}).allImportedModuleNames
  let import_diff := extended_all_imports.filter (· ∉ reduced_all_imports)
  let out := "\n".intercalate <| import_diff.map Name.toString |>.qsort (· < ·) |>.toList
  Lean.logInfo s!"Found {import_diff.size} additional imports:\n{out}"
