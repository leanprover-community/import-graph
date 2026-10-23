/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public meta import ImportGraph.Imports.Redundant
public meta import ImportGraph.Tools.NormImports -- ensure `#norm_imports` replacement works
public meta import Lean.Meta.Tactic.TryThis

public meta section

open Lean Elab Command Meta Tactic

/--
List the imports in this file which can be removed
because they are transitively implied by another import.
-/
elab tk:"#redundant_imports" : command => do
  if (← getEnv).header.isModule then
    liftCoreM <| TryThis.addSuggestion tk "#norm_imports"
      (footer := m!"\n\nPlease use `#norm_imports` to normalize imports in the module system. \
        `#redundant_imports` does not function correctly in the module system.")
    return
  let redundant ← liftCoreM do redundantImports
  if redundant.isEmpty then
    logInfo "No transitively redundant imports found."
  else
    logInfo <| "Found the following transitively redundant imports:\n" ++
      m!"{Format.joinSep redundant.toList "\n"}"
