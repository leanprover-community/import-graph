import ImportGraph.Tools
import ImportGraph.Lean.Syntax

/-! This file is deliberately not a module. -/

def x := Lean.SourceInfo.getLeading

#min_imports

open ImportGraph Shake Lean
/--
warning: `#min_imports` currently only produces modulized imports. Luckily, all necessary imports are module files, so the following suggestion may assist in modulizing this file.

⚠️ If you accept this suggestion, you will need to add the `module` keyword above the suggested import block to fix the file's imports, and (`@[expose]`) `public section` below the block to fix the rest of the file.

Modulization may affect the declarations and which imports are necessary, so re-run `#min_imports` after modulization.
  ⏎
  ̲
  ̲p̲u̲b̲l̲i̲c̲ ̲import I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵T̵o̵o̵l̵s̵
  ̵i̵m̵p̵o̵r̵t̵ ̵ImportGraph.Lean.Syntax
  ̲
  ̲i̲m̲p̲o̲r̲t̲ ̲I̲m̲p̲o̲r̲t̲G̲r̲a̲p̲h̲.̲T̲o̲o̲l̲s̲
-/
#guard_msgs in
run_cmd
  logAllMinImports #[]
  resetMinImports
