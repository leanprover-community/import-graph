import ImportGraphTest.MinImports.NonModule

/-! This file is deliberately not a module. -/

def y := x -- use the non-module `ImportGraphTest.MinImports.NonModule`

#min_imports

open ImportGraph Shake Lean
/--
warning: `#min_imports` currently only produces modulized imports, but some necessary imports are non-module files. These files would not be importable in a module file:

• ImportGraphTest.MinImports.NonModule
-/
#guard_msgs in
run_cmd
  logAllMinImports #[]
  resetMinImports
