/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau, Thomas R. Murrills
-/
module

public meta import ImportGraph.Tools.FindHome.Impl

public meta import ImportGraph.Widget.Copy.Registration
public meta import ImportGraph.Widget.Collapsible.Registration
public meta import ImportGraph.Widget.GoToModule.Registration

public meta section
open Lean Elab Command

@[command_elab ImportGraph.Shake.findHomeStx]
def elabFindHomeFor : CommandElab := ImportGraph.Shake.runFindHomeFor

@[command_elab oldFindHomeStx]
def elabOldFindHome : CommandElab := runOldFindHome
