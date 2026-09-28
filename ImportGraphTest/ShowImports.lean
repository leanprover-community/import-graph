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

▼ New declarations produced by this command:
  • x
-/
#guard_msgs in
#show_imports for
def x := let _ := parseImports?; Lean.Syntax.unsetLeading

/--
info: [click-to-copy] (Will copy:
  public import ImportGraph.Lean.Syntax) Imports needed:

public import ImportGraph.Lean.Syntax

▼ New declarations produced by this command:
  • y
-/
#guard_msgs in
#show_imports for
@[expose] public def y := Lean.Syntax.unsetLeading
