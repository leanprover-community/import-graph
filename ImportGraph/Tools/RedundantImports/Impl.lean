/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public import Lean.Elab.Command

import Lean.Widget.UserWidget
import ImportGraph.Imports.Redundant

public section

open Lean

/--
List the imports in this file which can be removed
because they are transitively implied by another import.
-/
def runRedundantImports : Elab.Command.CommandElabM Unit := do
  let redundant := (← Elab.Command.liftCoreM do redundantImports)
  if redundant.isEmpty then
    logInfo "No transitively redundant imports found."
  else
    logInfo <| "Found the following transitively redundant imports:\n" ++
      m!"{Format.joinSep redundant.toList "\n"}"
