/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau, Thomas R. Murrills
-/
module

public meta import ImportGraph.Imports.FromSource
public meta import ImportGraph.Imports.Pretty
public meta import ImportGraph.Imports.Redundant -- for deprecated `minimalRequiredModules`
public meta import ImportGraph.Imports.RequiredModules -- for deprecated `minimalRequiredModules`
public meta import ImportGraph.Shake.Environment
public meta import ImportGraph.Util.RunLater

/-!
# `#min_imports`

This module provides `#min_imports`, which minimizes the imports according to which are actually
used in the declarations and syntax of the current file.

`#min_imports` uses the `runLater` `Backreporter` so that it can be run at any point in the current
file.

## Future work

- Split out into API
- Turn into an optional (expensive) linter
- `#min_imports so far`
-/

open ImportGraph Lean Elab Command Shake

namespace ImportGraph.Shake

open Lean Environment in
/-- **Does not work within the module system.** Get the names of all needed imports. -/
public meta def Lean.Environment.minimalRequiredModules (env : Environment) : Array Name :=
  let required := env.requiredModules.toArray.erase env.header.mainModule
  let redundant := findRedundantImports env required
  required.filter fun n => ¬ redundant.contains n

/-- Get the `DeclNeeds` of all new constants in the `Environment`, given the `Array Syntax` of all
commands in the file. Ignores any syntax (at the top level) with kind in `ignoredKinds`. -/
meta def getModuleDeclNeeds (cmds : Array Syntax) (ignoredKinds : NameSet := {}):
    CommandElabM (DeclNeeds × Std.HashMap Name (Option Stance)) := do
  let mut declNeeds := ∅
  -- TODO: more accurate targeting of declarations and commands.
  for (decl, _) in (← getEnv).constants.map₂ do
    declNeeds := calcDeclConstInfoNeeds decl (← getEnv) declNeeds
  for cmd in cmds do
    unless ignoredKinds.contains cmd.getKind do
      declNeeds ← declNeeds.calcSyntaxNeeds (← getEnv) (declNeeds.keysArray) cmd
  liftCoreM <| StanceM.run <| declNeeds.calcIRNeeds

/--
Minimize the imports of the entire current module.

This command may be written anywhere in the file. It will wait until the file is done elaborating,
then minimize imports for the whole file, taking into account declarations that come after it as
well.

Currently, it accounts for constants, syntax, shake records, and runtime IR (except for
attributes like `@[inline]`), but not yet meta IR from new `meta def`s.

To normalize imports without taking into account whether they are used or not, see `#norm_imports`.

This command is aware of the module system.
-/
elab (name := minImportsStx) tk:"#min_imports" : command => do
  unless (← getEnv).header.isModule do
    logWarning m!"`#min_imports` currently only works within the module system."
  runLaterOnModuleSyntax fun cmds => withRef tk do
    let (declNeeds, s) ← getModuleDeclNeeds cmds (ignoredKinds := {``minImportsStx})
    if let some warning := declNeeds.metaWarning? (← getEnv) "#min_imports" then
      logWarning warning
    let transDeps := (← getEnv).mkTransDeps

    -- ignore direct imports of `#min_imports` for the transitive import calculation, reduce them
    -- among themselves, and add them back in afterwards
    let ignoring : NameSet := {`ImportGraph.Tools.MinImports, `ImportGraph.Tools, `ImportGraph}
    let ignoredImports := (← getEnv).header.imports.filter (ignoring.contains ·.module)
    let ignoredReduced ←
      if ignoredImports.isEmpty then pure #[] else
        let ignoredTransNeeds := (← getEnv).transitiveClosureOf ignoredImports transDeps
        pure <| (← getEnv).toRawImports <| ignoredTransNeeds.reduce transDeps

    let importNeeds ← liftCoreM do StanceM.run' (s := s) do
      (← getEnv).toSimultaneousImportNeeds declNeeds
    let reducedNeeds := importNeeds.toNeeds.reduce transDeps
    let reducedImps := (← getEnv).toRawImports reducedNeeds ++ ignoredReduced

    let (header, _, log) ← parseCurrentHeader
    if log.hasErrors then
      -- This should be impossible.
      throwError m!"The current imports failed to parse. Errors:\n\
        {m!"\n".joinSep <| log.toList.map (·.data)}"
    let sourceImps := headerToImportRefsWithWhitespace header
    let some (msg, errs) ← liftCoreM <| Import.mkImportSuggestionMessage tk reducedImps sourceImps
      | logInfo m!"Imports are minimal."

    let formattingChangeAtMost := Import.beqUpToOrder (sourceImps.map (·.1.toImport)) reducedImps
    let normalizationChangeAtMost :=
      -- Whether the current imports are provided by the reduced imports
      (← getEnv).currentTransNeeds transDeps |>.subsumedBy reducedNeeds transDeps
    let errMsgOrConnector := if errs.isEmpty then ":" else
      let disclaimer := s!"some comments could not be carried over. \
        Please review the source comment that will be inserted after the imports."
      if formattingChangeAtMost || normalizationChangeAtMost then
        s!".\n\nHowever, {disclaimer}\n"
      else
        s!", but {disclaimer}\n"
    if formattingChangeAtMost then
      logInfo m!"Imports are minimal, but can be reformatted{errMsgOrConnector}{msg}"
    else if normalizationChangeAtMost then
      logWarning m!"Imports are minimal, but can be normalized{errMsgOrConnector}{msg}"
    else
      logWarning m!"Imports can be reduced{errMsgOrConnector}{msg}"
