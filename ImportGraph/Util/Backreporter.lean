/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lean.Elab.Command

/-!
# Backreporters

A `Backreporter` enables command elaborators to send "requests" to be fulfilled at the end of the file, while interactively displaying a progress indicator (yellow bar) at the command until the requests are processed at the end of the file.

A backreporter whose requests can carry data of type `α` may be registered with `registerBackreporter`, which asks for some
`run : Array Syntax → Array (Request α) → CommandElabM Unit` which processes the full array of accumulated requests at the end of the file in a `ModuleLinter` and clears the progress bar indicators.

Users of the API may send a request to a given `br : Backreporter` with
`br.sendRequest (data : α) : CommandElabM Unit`, which sends a request carrying `data α` and creates the progress bar, which by default is at the command.

`sendRequest` may optionally be provided with a `ProgressIndication` flag to specify the location via provided `ref : Syntax` (e.g. `.at ref`) or specify that no progress bar should be created at all (`.quiet`). (The request will still be filed and processed at the end of the file.)

Note: due to how Lean reuses command snapshots, the progress bar indicators associated with requests are not robust, and may remain cleared even if the requests must be reprocessed. For example, if a request is sent at some point in the file and the file is allowed to elaborate the completion (thus clearing the progress bar indicator), and then the file is interactively edited below the request site, we cannot make a progress bar indicator reappear at the request site above the edit. Thus no yellow bar will appear for this duration even though the request should be reprocessed at the end of the file.

Further, no progress bar (nor its associated promise) will be created when on the command line (when `Elab.inServer` is false) or when `Elab.async` is false.

As such, this means that the resolved state of the promise and/or its presence should not be used as an indicator of whether the request has been fulfilled.

Progress indicators can be cleared manually with `Request.stopProgressIndicator`.

## Implementation notes

-- TODO: rewrite

A backreporter is a `ModuleLinter` plus a per-backreporter environment extension holding the
file's requests. Storing requests in the environment (rather than, say, alongside the linter
in its `IO.Ref`) is what makes them well-behaved during interactive editing: re-elaborating a
command replaces its requests, deleting a command deletes them, and incrementally reused
commands have their requests replayed — including their in-flight progress state.

Progress bars are implemented by logging, at request time, a snapshot task over the requesting
command's syntax, backed by a `Unit`-typed `IO.Promise`. The promise is intentionally a bare
completion signal: report content must flow through messages, not through the promise, since
an incrementally reused command keeps its already-resolved snapshot task, which would pin a
stale report (promise resolution is first-wins). The same first-wins semantics is what makes
`markCompleted` safe to expose: the worst it can do is clear a bar early. The snapshot task is
built with `IO.Promise.resultD`, so even a promise dropped without resolution (e.g. when
elaboration of the rest of the file is abandoned) cannot block reporting.
-/

open Lean Elab Command Language

public  section

namespace ImportGraph

namespace Backreporter

/--
A request, typically created by `Backreporter.sendRequest`, and stored in a `Backreporter`'s
(non-persistent) environment extension to be processed by that `Backreporter` at the end of a file.
It carries `data : α` to affect how it is processed and an optional private `IO.Promise` that
governs an interactive progress bar indicator.

`Request`s enable a command elaborator to send data to the end of the file while displaying to the
user that processing is ongoing at the site the request was sent via the (yellow) progress bar
indicator.
-/
structure Request (α : Type) where private mk ::
  /-- The content of a `Request`, necessary for processing the request later on. -/
  data : α
  /-- The promise backing this request's progress task (usually corresponding to a yellow bar), if
  progress was requested.

  Resolving it does *not* necessarily signal fulfillment of the request. The request may need to be
  re-processed due to interactive editing even though the snapshot (and thus promise) is reused.
  (E.g., if the file is edited below the request's send site, but the request is processed at the
  end of the file.) If request fulfillment information is needed mid-file, it should be recorded in
  `data`.

  Use `Request.stopProgressIndicator` to manually resolve the promise.

  If `Elab.inServer` is `false` or `Elab.async` is `false` when the request is created, this is
  `none`. -/
  private promise? : Option (IO.Promise Unit)

/-- A request with no progress promise.

To file a request, use `Backreporter.sendRequest`. -/
@[inline] def Request.mkPure (data : α) : Request α :=
  { data, promise? := none }

/-- A request that uses the provided promise as its promise. `Request.sendRequest` should be
preferred to constructing requests manually this way.

To file a request, use `Backreporter.sendRequest`. -/
@[inline] def Request.mkFromPromise (data : α) (promise : IO.Promise Unit) : Request α :=
  { data, promise? := some promise }

/--
Stops the progress indicator (yellow bar) for the given `Request`.

Note that the progress task cannot be resumed, even if the request ought to be reprocessed, e.g.
due to interactive editing occurring after the request but before its intended processing site. As
such this should not be used to request fulfillment.
-/
@[inline] def Request.stopProgressIndicator (r : Request α) : BaseIO Unit :=
  r.promise?.forM (·.resolve ())

/-- Whether this request was filed with a progress indicator (whether or not it has since been cleared). This may be `false` if e.g. `Elab.async` is `false`. -/
@[inline] def Request.hasProgressIndicator (req : Request α) : Bool :=
  req.promise?.isSome

/-- If this request has a progress bar indicator, whether it has been cleared.

Note that progress indicators inevitably remain cleared even if the request ought to be reprocessed
due to interactive editing. As such, the return value should **not** be used to determine whether a
`Request` has been fulfilled.

Returns `none` if the request has never had a progress task. -/
@[inline] def Request.isCleared? (req : Request α) : BaseIO (Option Bool) :=
  req.promise?.mapM (·.isResolved)

end Backreporter

/--
A `Backreporter` consists of

1. a non-persistent environment extension that stores filed requests (data and optional progress-bar
  promises)
2. a handler `run : Array Syntax → Array (Backreporter.Request α) → CommandElabM Unit` that "
  "fulfills" the registered requests, run at the end of the file in a `ModuleLinter`.

Note that even the effects of request fulfillment are non-persistent, as `ModuleLinter`s cannot
affect the environment. `Backreporter`s are designed for interactive use, and `Request`s carry
`IO.Promise`s that govern progress bar indicators created at the site the request was sent. These
are cleared at the end of the file when the handler is run. They may also be cleared earlier with
`Request.stopProgressIndicator`.

Note that due to how command snapshots are reused during elaboration, a progress indicator cannot
be restarted once it has been resolved without re-sending the request. This means that interactive
editing below the request site after the request has already been fulfilled at least once may
require the request to be reprocessed (to e.g. log a new message), but the progress bar indicator
will not reappear. Likewise, the resolved state of the progress bar indicator should not in general
be used to determine whether a given request must be (re)processed or not.
-/
structure Backreporter (α : Type) where
  /-- The backreporter's name, which is the same as the associated `ModuleLinter`'s name. -/
  name : Name
  /-- The handler: receives the syntax of every command in the file together with the requests
  filed during elaboration (in file order), and runs at the end of the file. Exposed here so
  that it can also be invoked directly, e.g. on requests built with `Request.mkPure`. -/
  run : Array Syntax → Array (Backreporter.Request α) → CommandElabM Unit
  /-- Non-persistent extension which holds `Request`s (data & progress-bar promises). Assuming this
  `Backreporter` was created with `registerBackreporter`, this extension is `.mainOnly`. -/
  ext : EnvExtension (Array (Backreporter.Request α))
deriving Nonempty

/-- Gets the currently-filed `Request`s for the given `Backreporter`. -/
@[inline] def Backreporter.getRequests {α} (env : Environment) (b : Backreporter α) :
    Array (Request α) :=
  b.ext.getState env

/-- Modifies the current requests filed with the given `Backreporter`. Note that requests should
only be added with `Backreporter.sendRequest`. -/
@[inline] def Backreporter.modify {α} (env : Environment) (b : Backreporter α)
    (f : Array (Request α) → Array (Request α)) (asyncDecl : Name := .anonymous) : Environment :=
  b.ext.modifyState env f (asyncDecl := asyncDecl)

/--
Registers a `BackReporter` that can receive interactive `Request α`s (via a non-persistent
environment extension, registered here) and handles them via `run` at the end of the file in a
`ModuleLinter`. The first argument to `run` is the module's command syntax passed through from the
`ModuleLinter`, and the second is the array of all `Request`s that have been filed.

Note that `run` has no access to info trees; if infotree information is necessary, it should be bundled into `α` so that it can be sent along with the request.

Note that `run` should not use the resolved state of the `Request`'s progress bar to determine
whether the request has been fulfilled, as interactive editing may require that the requests are
reprocessed even after the progress bar indicator has been resolved. See `Backreporter` and
`Request` for details.

All progress bar indicators are cleared by the `ModuleLinter` this function registers, and `run`
does not need to do so.
-/
def registerBackreporter
    (run : Array Syntax → Array (Backreporter.Request α) → CommandElabM Unit)
    (name : Name := by exact decl_name%) :
    IO (Backreporter α) := do
  let ext ← registerEnvExtension (pure #[]) (asyncMode := .mainOnly)
  addModuleLinter {
    name
    run cmds := do
      let requests := ext.getState (← getEnv)
      try
        run cmds requests
      finally
        -- Unconditional, just in case.
        for req in requests do
          req.stopProgressIndicator
  }
  return { name, run, ext }

namespace Backreporter

/-- Whether and how to show a progress indicator (yellow bar) in `Backreporter.sendRequest`.-/
inductive ProgressIndication where
  /-- Do not create or show any indicator. -/
  | quiet
  /-- Show the indicator at the command currently being elaborated. -/
  | atCommand
  /-- Show the indicator at the position of `ref`. `ref` may be non-canonical. However, it must be
  within the current command being elaborated. Lean clamps ranges outside the current command range
  to the command range. -/
  | at (ref : Syntax)

/-- The explicit syntax location to show progress, if there is one. `.atCommand` yields `none`. -/
def ProgressIndication.toSyntax? : ProgressIndication → Option Syntax
  | .at ref => ref
  | _ => none

/--
Files a request with the given `Backreporter` with content `data`, which will be processed at the end of the file by `b.run`.

By default, this shows a progress indicator (yellow bar) at the current command until requests are
processed at the end of the file. This behavior can be controlled by providing
`p : ProgressIndication`:
- `.atCommand` (default): shows indicator at current command.
- `.at (ref : Syntax)`: shows indicator at the location given by the position info of `Syntax`.
- `.quiet`: does not show any indicator.

Does not show an indicator under any circumstances if `Elab.inServer` is `false` or `Elab.async` is
`false`, but still files the request, and the request will be processed.
-/
def sendRequest (b : Backreporter α) (data : α)
    (progressIndication : ProgressIndication := .atCommand) :
    CommandElabM Unit := do
  let opts ← getOptions
  let promise? ←
    if !(Elab.inServer.get opts) || !(Elab.async.get opts) || progressIndication matches .quiet then
      pure none
    else some <$> IO.Promise.new
  modifyEnv fun env => b.ext.modifyState env (·.push { data, promise? })
  if let some promise := promise? then
    -- An unfinished snapshot task over `ref` marks it as in-progress in the language server.
    logSnapshotTask {
      -- `stx?` allows infotree lookup at `stx?` to force tasks, but we don't want to block there.
      stx? := none
      -- We want to use `ref?`'s range even if synthetic, whereas `defaultReportingRange` would
      -- demand that it be `canonicalOnly`.
      reportingRange := .ofOptionInheriting <| progressIndication.toSyntax?.bind (·.getRange?)
      -- TODO: should we inherit from context?
      cancelTk? := none
      -- `resultD` (rather than `result!`) ensures the task finishes even if the promise is
      -- dropped without resolution, e.g. when elaboration of the rest of the file is abandoned.
      task := promise.resultD () |>.map (sync := true) fun _ =>
        SnapshotTree.mk { desc := s!"backreport from `{b.name}`", diagnostics := .empty } #[]
    }

end Backreporter

end ImportGraph
