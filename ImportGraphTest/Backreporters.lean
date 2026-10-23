module

public meta import ImportGraph.Util.RunLater
public meta import Lean.Meta.Tactic.TryThis
public meta import Lean.Elab.Command

/-! This module tests backreporters through `ImportGraph.Util.RunLater`. -/

open ImportGraph Lean Elab Command

run_cmd -- Mark 1
  let ref ← getRef
  runLater <| liftCoreM <|
    Meta.Tactic.TryThis.addSuggestion ref
      s!"Hello (1) from `{ref.reprint.getD "couldn't reprint" |>.trimAscii}`"
  runLater <| liftCoreM <|
    Meta.Tactic.TryThis.addSuggestion ref
      s!"Hello (2) from `{ref.reprint.getD "couldn't reprint" |>.trimAscii}`"

/--
info: Try this:
  [apply] Hello (1) from `run_cmd -- Mark 1`
---
info: Try this:
  [apply] Hello (2) from `run_cmd -- Mark 1`
-/
#guard_msgs in
run_cmd do
  let requests := runLaterReporter.getRequests (← getEnv)
  runLaterReporter.fulfill #[] requests
  runLaterReporter.reset

run_cmd -- Mark 2
  let ref ← getRef
  runLater do
    throwError m!"Hello (failure 1) from `{ref.reprint.getD "couldn't reprint" |>.trimAscii}`"
  runLater do
    throwError m!"Hello (failure 2) from `{ref.reprint.getD "couldn't reprint" |>.trimAscii}`"

/--
error: Request failed:

Hello (failure 1) from `run_cmd -- Mark 2`
---
error: Request failed:

Hello (failure 2) from `run_cmd -- Mark 2`
-/
#guard_msgs in
run_cmd do
  let requests := runLaterReporter.getRequests (← getEnv)
  runLaterReporter.fulfill #[] requests
  runLaterReporter.reset
