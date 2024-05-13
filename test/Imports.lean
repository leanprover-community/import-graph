import ImportGraph.Imports
import ImportGraph.RequiredModules

open Lean

def importTest : CoreM Unit := do
  let x ← redundantImports
  logInfo s!"{x.toArray}"

/--
info: Found the following transitively redundant imports:
ImportGraph.RequiredModules
-/
#guard_msgs in
#redundant_imports

/-- info: import ImportGraph.Imports -/
#guard_msgs in
#minimize_imports

/-- info: [ImportGraph.Imports] -/
#guard_msgs in
#find_home importTest


open Elab Command in
elab "#my_test" : command => do
  -- functionality of `#redundant_imports`
  let expected := #[`ImportGraph.RequiredModules]
  let ri ← liftCoreM redundantImports
  if (ri.toArray != expected) then
    logError s!"Failed: `redundantImports` returned {ri.toArray} instead of {expected}"

  -- functionality of `#find_home`
  let expected := #[`ImportGraph.Imports]
  let mi ← liftCoreM <| Lean.Name.findHome `importTest none
  if (mi.toArray != expected) then
      logError s!"Failed: `findHome` returned {mi.toArray} instead of {expected}"

  -- functionality of `#find_home!`
  let expected := #[`ImportGraph.Imports]
  let mi! ← liftCoreM <| Lean.Name.findHome `importTest (← getEnv)
  if (mi!.toArray != expected) then
      logError s!"Failed: `findHome (!)` returned {mi!.toArray} instead of {expected}"

  logInfo s!"{mi.toArray}"
  pure ()

/-- info: #[ImportGraph.Imports] -/
#guard_msgs in
#my_test
