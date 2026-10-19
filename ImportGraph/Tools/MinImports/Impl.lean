/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public import Lean.Elab.Command

import Lean.Widget.UserWidget
import ImportGraph.Imports.RequiredModules
import ImportGraph.Imports.Redundant

public section

open Lean

/--
Return the names of the modules in which constants used in the current file were defined,
with modules already transitively imported removed.

Note that this will *not* account for tactics and syntax used in the file,
so the results may not suffice as imports.
-/
def Lean.Environment.minimalRequiredModules (env : Environment) : Array Name :=
  let required := env.requiredModules.toArray.erase env.header.mainModule
  let redundant := findRedundantImports env required
  required.filter fun n => ¬ redundant.contains n

/--
Try to compute a minimal set of imports for this file,
by analyzing the declarations.

This must be run at the end of the file,
and is not aware of syntax and tactics,
so the results will likely need to be adjusted by hand.
-/
def runMinImports : Elab.Command.CommandElabM Unit := do
  let imports := (← getEnv).minimalRequiredModules.qsort (·.toString < ·.toString)
    |>.toList.map (fun n => "public import " ++ n.toString)
  logInfo <| Format.joinSep imports "\n"
