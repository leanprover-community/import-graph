module

public import ImportGraphTest.MinImports.ToolImport
import ImportGraph.Lean.Syntax

/-! Ensure that we preserve a transitively-imported `#min_import`s, separately. -/

open ImportGraph Shake

#min_imports

def x := Lean.SourceInfo.getLeading

/--
warning: Imports can be reduced:
  p̵u̵b̵l̵i̵c̵ ̵import I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵T̵e̵s̵t̵.̵M̵i̵n̵I̵m̵p̵o̵r̵t̵s̵.̵T̵o̵o̵l̵I̵m̵p̵o̵r̵t̵
  ̵i̵m̵p̵o̵r̵t̵ ̵ImportGraph.Lean.Syntax
  ̲i̲m̲p̲o̲r̲t̲ ̲I̲m̲p̲o̲r̲t̲G̲r̲a̲p̲h̲.̲T̲o̲o̲l̲s̲.̲M̲i̲n̲I̲m̲p̲o̲r̲t̲s̲
-/
#guard_msgs in
run_cmd
  logAllMinImports #[
    ← `(command| open ImportGraph Shake),
    ← `(command| #min_imports),
    ← `(command| def x := Lean.SourceInfo.getLeading)]

run_cmd resetMinImports
