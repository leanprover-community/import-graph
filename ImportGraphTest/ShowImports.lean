module

public meta import ImportGraph.Lean.Syntax
public import ImportGraph.Lean.Syntax
public import ImportGraph.Imports.FromSource
import ImportGraph.Tools.ShowImports

open ImportGraph

/--
info: [click-to-copy] (Will copy:
  import ImportGraph.Imports.FromSource
  import ImportGraph.Lean.Syntax) Imports needed:

import ImportGraph.Imports.FromSource
import ImportGraph.Lean.Syntax

▼ New constants produced by this command:
  • x (private)
-/
#guard_msgs in
#show_imports for
def x := let _ := parseImports?; Lean.Syntax.unsetLeading

/--
info: [click-to-copy] (Will copy:
  public import ImportGraph.Lean.Syntax) Imports needed:

public import ImportGraph.Lean.Syntax

▼ New constants produced by this command:
  • y
-/
#guard_msgs in
#show_imports for
@[expose] public def y := Lean.Syntax.unsetLeading

/--
info: [click-to-copy] (Will copy:
  import ImportGraph.Imports.FromSource
  import ImportGraph.Lean.Syntax) Imports needed:

import ImportGraph.Imports.FromSource
import ImportGraph.Lean.Syntax

▼ New constants produced by this command:
  • z
▼ Constants from the current file needed by this command:
  • x (private)
▼ Extra shake import needs recorded by this command:
  • import Init.Tactics
-/
#guard_msgs in
#show_imports for
public theorem z : True := by
  let _ := x
  trivial
