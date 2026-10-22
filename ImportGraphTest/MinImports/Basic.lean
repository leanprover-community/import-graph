module

import ImportGraph.Tools
public import ImportGraph.Lean.Syntax
public import ImportGraph.Imports.Pretty
meta import ImportGraph.Tools.MinImports

open ImportGraph Shake Lean

#min_imports

-- Uses `ImportGraph.Lean.Syntax` only privately
def x := Lean.SourceInfo.getLeading

-- We simulate being at the end of the file by logging all requests eagerly.
-- This accounts for all constants up to this point, plus the manually-provided syntax.
/--
warning: Imports can be reduced:
  import I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵T̵o̵o̵l̵s̵
  ̵p̵u̵b̵l̵i̵c̵ ̵i̵m̵p̵o̵r̵t̵ ̵ImportGraph.Lean.Syntax
  p̵u̵b̵l̵i̵c̵ ̵import I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵I̵m̵p̵o̵r̵t̵s̵.̵P̵r̵e̵t̵t̵y̵
  ̵m̵e̵t̵a̵ ̵i̵m̵p̵o̵r̵t̵ ̵I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵T̵o̵o̵l̵s̵.̵M̵i̵n̵I̵m̵p̵o̵r̵t̵s̵I̲m̲p̲o̲r̲t̲G̲r̲a̲p̲h̲.̲T̲o̲o̲l̲s̲
-/
#guard_msgs in
run_cmd
  logAllMinImports #[
    ← `(command| open ImportGraph Lean),
    ← `(command| def x := true),
    ← `(command| #min_imports)]

@[expose] public def x' := Lean.SourceInfo.getLeading

/- Note that the same `#min_imports` request we eagerly processed above will, when processed at
this point or later, take into account the new declaration. -/
/--
warning: Imports can be reduced:
  i̵m̵p̵o̵r̵t̵ ̵I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵T̵o̵o̵l̵s̵public import ImportGraph.Lean.Syntax ̵
  ̲
  ̲p̵u̵b̵l̵i̵c̵import I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵I̵m̵p̵o̵r̵t̵s̵.̵P̵r̵e̵t̵t̵y̵ ̵m̵e̵t̵a̵ ̵i̵m̵p̵o̵r̵t̵
  ̵I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵T̵o̵o̵l̵s̵.̵M̵i̵n̵I̵m̵p̵o̵r̵t̵s̵I̲m̲p̲o̲r̲t̲G̲r̲a̲p̲h̲.̲T̲o̲o̲l̲s̲
-/
#guard_msgs in
run_cmd
  logAllMinImports #[
    ← `(command| open ImportGraph Lean),
    ← `(command| def x := true),
    ← `(command| #min_imports)]

run_cmd withoutExporting <| recordExtraModUse `ImportGraph.Imports.Pretty false

-- Records the new extra mod use, but (properly) demotes it to private.
/--
warning: Imports can be reduced:
  i̵m̵p̵o̵r̵t̵ ̵I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵T̵o̵o̵l̵s̵public import ImportGraph.Lean.Syntax ̵
  ̲
  ̲p̵u̵b̵l̵i̵c̵import ImportGraph.Imports.Pretty ̵
  ̲m̵e̵t̵a̵ ̵import I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵T̵o̵o̵l̵s̵.̵M̵i̵n̵I̵m̵p̵o̵r̵t̵s̵I̲m̲p̲o̲r̲t̲G̲r̲a̲p̲h̲.̲T̲o̲o̲l̲s̲
-/
#guard_msgs in
run_cmd
  logAllMinImports #[
    ← `(command| open ImportGraph Lean),
    ← `(command| def x := true),
    ← `(command| #min_imports)]

-- Prevent the end of the file from logging noisily at the actual `#min_imports` invocation.
run_cmd resetMinImports
