/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public meta import ImportGraph.Tools.RedundantImports.Impl

public meta section
open Lean Elab Command

/--
List the imports in this file which can be removed
because they are transitively implied by another import.
-/
elab "#redundant_imports" : command => runRedundantImports
