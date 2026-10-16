/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lean.Elab.Command

open Lean Elab Command

namespace ImportGraph

/--
This function "runs" a series of "linter-likes" (for any provided meaning of "run" and
"linter-like") in the same way that core runs linters, handling state backtracking and infotree
collection appropriately. See `Lean.Elab.Command.runLinters` for reference.

Each linter-like is run under a trace node, with trace class given by `traceCls` and message given
by `traceMsg`. The `failureMsgHeader` is prepended (with two newlines) to any exception thrown by
the linter-like.

Unlike `runLinters`, this appends any new messages, infotrees, and code quality metrics to the
final `CommandElabM` state. If running this from inside another linter, Lean will
collect these from the resulting state. Otherwise, these may be extracted by keeping track of the
number of trees and code quality metrics from before, and comparing to after.
-/
public def runLinterLikes {α} (traceCls : Name) (linterLikes : Array α)
    (run : α → CommandElabM Unit)
    (traceMsg : α → Except Exception Unit → CommandElabM MessageData)
    (failureMsgHeader : α → MessageData) :
    CommandElabM Unit := do
  let producedInfoTrees ← IO.mkRef ({} : PersistentArray InfoTree)
  let producedCodeQualityEntries ← IO.mkRef (#[] : Array Linter.CodeQualityLogEntry)
  for linter in linterLikes do
    withTraceNode traceCls (traceMsg linter) do
      let savedState ← get
      let originalSize := savedState.infoState.trees.size
      try
        run linter
      catch
        | Exception.error ref msg =>
          logException (.error ref m!"{failureMsgHeader linter}\n\n{msg}")
        | ex@(Exception.internal ..) =>
          logException ex
      finally
        /- Capture and record new additions to the infotrees and code quality metrics recorded by
        the linter itself -/
        let newInfoState ← getInfoState
        let newState := Linter.codeQualityLogExt.getState (← get).env
        if newInfoState.enabled then
          producedInfoTrees.modify fun old =>
            (newInfoState.trees.foldl (·.push ·) old (start := originalSize))
        let oldStateSize := (Linter.codeQualityLogExt.getState (env := savedState.env)).size
        producedCodeQualityEntries.modify (· ++ newState.extract oldStateSize)
        -- Pass along messages and traces
        modify fun s => { savedState with messages := s.messages, traceState := s.traceState }
  /- Record the aggregated new infotrees and code quality metrics (produced by the linters) in the
  final command state -/
  let producedInfoTrees ← producedInfoTrees.get
  let producedCodeQualityEntries ← producedCodeQualityEntries.get
  modifyEnv fun env =>
    Linter.codeQualityLogExt.modifyState env (· ++ producedCodeQualityEntries)
  modifyInfoState fun s => { s with trees := s.trees ++ producedInfoTrees }
