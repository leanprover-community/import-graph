/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public import Lean.CoreM
import ImportGraph.Graph.TransitiveClosure
import ImportGraph.Imports.ImportGraph
import ImportGraph.Imports.RequiredModules

open Lean

/--
Returns a `List (Name × List Name)` with a key for each module `n` in `amongst`,
whose corresponding value is the list of modules `m` in `amongst` which are transitively imported by `n`,
but no declaration in `n` makes use of a declaration in `m`.
-/
public def unusedTransitiveImports (amongst : List Name) (verbose : Bool := false) : CoreM (List (Name × List Name)) := do
  let env ← getEnv
  let transitiveImports := env.importGraph.transitiveClosure
  let transitivelyRequired ← env.transitivelyRequiredModules' amongst verbose
  amongst.mapM fun n => do return (n,
    let unused := (transitiveImports.find? n).getD {} \ (transitivelyRequired.find? n |>.getD {})
    amongst.filter (fun m => unused.contains m))
