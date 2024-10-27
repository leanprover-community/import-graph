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
#min_imports

/-- info: [ImportGraph.Imports] -/
#guard_msgs in
#find_home importTest

open Elab Command


/--
Reports unused transitive imports amongst the specified modules.
-/
elab "#unused_transitive_imports" names:ident* : command => do
  let imports := (names.map Syntax.getId).toList
  let unused ← Elab.Command.liftCoreM (unusedTransitiveImports imports)
  for (n, u) in unused do
    if !u.isEmpty then
    logInfo <| s!"Transitively unused imports of {n}:\n{"\n".intercalate (u.map (fun i => s!"  {i}"))}"

-- This test case can be removed after nightly-2024-10-24, because these imports have been cleaned up.
/--
info: Transitively unused imports of Init.Control.StateRef:
  Init.System.IO
---
info: Transitively unused imports of Init.System.IO:
  Init.Control.Reader
-/
#guard_msgs in
#unused_transitive_imports Init.Control.StateRef Init.System.IO Init.Control.Reader Init.Control.Basic

-- This is a spurious unused transitive import, because it relies on notation from `Init.Core`.
/--
info: Transitively unused imports of Init.Control.Basic:
  Init.Core
-/
#guard_msgs in
#unused_transitive_imports Init.Control.Basic Init.Core

elab "#transitivelyRequiredModules_test" : command => do
  let env ← getEnv
  let unused ← liftCoreM <| env.transitivelyRequiredModules `ImportGraph.RequiredModules
  logInfo s!"{unused.contains `Init.Data.Option.Lemmas}"

/-- info: true -/
#guard_msgs in
#transitivelyRequiredModules_test

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
