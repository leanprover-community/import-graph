/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.Util.Backreporter

/-!
## `runLater` for running commands at the end of the file

This module provides a means for metaprograms (especially command elaborators) to file requests for
arbitrary actions `x : CommandElabM Unit` or functions `f : Array Syntax → CommandElabM Unit` to be
run at the end of the file, where the `Array Syntax` is the array of commands parsed in the file.

For example, a command elaborator
-/

namespace ImportGraph

open Lean Elab Command Backreporter

public section

/-- Runs arbitrary `Array Syntax → CommandElabM Unit` requests at the end of the file on the
module's syntax, in the same manner as a linter (i.e. only preserving messages and the trace state
between requests). -/
initialize runLaterReporter : Backreporter (Array Syntax → CommandElabM Unit) ←
  registerBackreporter fun cmds requests => do
    for request in requests do
      let savedState ← get
      try
        request.data cmds
        -- Wait for the message to be reported instead of running `stopProgressIndicator` here
      catch
        | Exception.error ref msg =>
          logException (.error ref m!"Request to run failed:\n\n{msg}")
        | ex@(Exception.internal _ _) =>
          logException ex
      finally
        modify fun s => { savedState with messages := s.messages, traceState := s.traceState }

/-- Runs `x` at the end of the file. May log messages, but cannot persistently alter the
environment or access infotrees.

`x` will be run with the terminal command's ref as the ambient ref. Bundle position info into `x` in
order to log on the intended ranges.

If `progressIndication := .atCommand` (the default) and both `Elab.async` and `Elab.inServer` are
`true`, this creates a yellow bar which disappears once `x` is run at the end of the file. Use
`.at (ref : Syntax)` to show the progress bar at `ref` (note: this is clamped to the position range
of the current command) and `.quiet` to show no progress bar at all. -/
@[inline] def runLater (x : CommandElabM Unit)
    (progressIndication := ProgressIndication.atCommand) : CommandElabM Unit :=
  runLaterReporter.sendRequest (fun _ => x) progressIndication

/-- Runs `x` at the end of the file. May log messages, but cannot persistently alter the
environment or access infotrees.

`x` will be run with the terminal command's ref as the ambient ref. Bundle position info into `x` in
order to log on the intended ranges. -/
@[inline] def runLaterWithoutIndicator (env : Environment) (x : CommandElabM Unit) :
    Environment :=
  runLaterReporter.sendSilentRequest env (fun _ => x)

/-- Runs `f` at the end of the file on the module's full `Array Syntax`. May log messages, but
cannot persistently alter the environment or access infotrees.

`f` will be run with the terminal command's ref as the ambient ref; bundle position info into `f` in
order to log on the intended ranges, e.g. `f := fun cmds => withRef ref ...`

If `progressIndication := .atCommand` (the default) and both `Elab.async` and `Elab.inServer` are
`true`, this creates a yellow bar which disappears once `f cmds` is run at the end of the file. Use
`.at (ref : Syntax)` to show the progress bar at `ref` (note: this is clamped to the position range
of the current command) and `.quiet` to show no progress bar at all. -/
@[inline] def runLaterOnModuleSyntax (f : Array Syntax → CommandElabM Unit)
    (progressIndication := ProgressIndication.atCommand) : CommandElabM Unit :=
  runLaterReporter.sendRequest f progressIndication

/-- Runs `f` at the end of the file on the module's full `Array Syntax`. May log messages, but
cannot persistently alter the environment or access infotrees.

`f` will be run with the terminal command's ref as the ambient ref; bundle position info into `f` in
order to log on the intended ranges, e.g. `f := fun cmds => withRef ref ...` -/
@[inline] def runLaterOnModuleSyntaxWithoutIndicator (env : Environment)
    (f : Array Syntax → CommandElabM Unit) : Environment :=
  runLaterReporter.sendSilentRequest env f
