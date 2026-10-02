/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public import Lean.Elab.Command

public meta import ImportGraph.Shake.DeclNeeds
public meta import ImportGraph.Imports.FromSource
public meta import ImportGraph.Imports.Pretty
public meta import ImportGraph.Lean.MessageData
public meta import ImportGraph.Shake.Environment
public meta import ImportGraph.Shake.Workspace
public meta import ImportGraph.Util.RunLater
public meta import ImportGraph.Imports.RequiredModules -- for deprecated `minimalRequiredModules`
public meta import ImportGraph.Imports.Redundant -- for deprecated `minimalRequiredModules`

/-!
# `#min_imports`

## Future work

- Split out into API
- Turn into an optional linter
-/

open ImportGraph Lean Elab Command Shake

/-- Get the `DeclNeeds` of all new constants in the `Environment`, given the `Array Syntax` of all
commands in the file.-/
meta def getModuleDeclNeeds (cmds : Array Syntax) :
    CommandElabM (DeclNeeds × Std.HashMap Name (Option Stance)) := do
  let mut declNeeds := ∅
  -- TODO: more accurate targeting of declarations and commands.
  for (decl, _) in (← getEnv).constants.map₂ do
    declNeeds := calcDeclConstInfoNeeds decl (← getEnv) declNeeds
  for cmd in cmds do
    declNeeds ← declNeeds.calcSyntaxNeeds (← getEnv) (declNeeds.keysArray) cmd
  liftCoreM <| StanceM.run <| declNeeds.calcIRNeeds

/-- **Does not work within the module system.** Get the names of all needed imports. -/
public meta def Lean.Environment.minimalRequiredModules (env : Environment) : Array Name :=
  let required := env.requiredModules.toArray.erase env.header.mainModule
  let redundant := findRedundantImports env required
  required.filter fun n => ¬ redundant.contains n

/--
Minimize the imports

-/
elab tk:"#min_imports" : command => do
  unless (← getEnv).header.isModule do
    logWarning m!"`#min_imports` currently only works within the module system."
  runLaterOnModuleSyntax fun cmds => withRef tk do
    let (declNeeds, s) ← getModuleDeclNeeds cmds
    if let some warning := declNeeds.metaWarning? (← getEnv) "#min_imports" then
      logWarning warning

    let importNeeds ← liftCoreM do StanceM.run' (s := s) do
      (← getEnv).toSimultaneousImportNeeds declNeeds
    let transDeps := (← getEnv).mkTransDeps
    let reducedNeeds := importNeeds.toNeeds.reduce transDeps
    let reducedImps := (← getEnv).toRawImports reducedNeeds

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
