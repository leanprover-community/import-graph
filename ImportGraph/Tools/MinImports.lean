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
public meta import ImportGraph.Shake.Environment
public meta import ImportGraph.Shake.Workspace
public meta import ImportGraph.Util.RunLater
public meta import ImportGraph.Imports.RequiredModules -- for deprecated `minimalRequiredModules`
public meta import ImportGraph.Imports.Redundant -- for deprecated `minimalRequiredModules`

/-!
`#min_imports`

-- TODO-NOW: docs
-/

open ImportGraph Lean Elab Command Shake

meta def getModuleDeclNeeds (cmds : Array Syntax) :
    CommandElabM (DeclNeeds × Std.HashMap Name (Option Stance)) := do
  let mut declNeeds := ∅
  -- TODO: more accurate targeting of declarations and commands.
  for (decl, _) in (← getEnv).constants.map₂ do
    declNeeds := calcDeclConstInfoNeeds decl (← getEnv) declNeeds
  for cmd in cmds do
    declNeeds ← declNeeds.calcSyntaxNeeds (← getEnv) (declNeeds.keysArray) cmd
  liftCoreM <| StanceM.run <| declNeeds.calcIRNeeds

elab tk:"#min_imports" : command => do
  unless (← getEnv).header.isModule do
    -- TODO-NOW: fall back to old behavior instead?
    logWarning m!"`#min_imports` currently only works in the module system. This restriction may be
      lifted soon."
  runLaterWithSyntax fun cmds => withRef tk do
  let (declNeeds, s) ← getModuleDeclNeeds cmds

  let importNeeds ← liftCoreM do StanceM.run' (s := s) do
    (← getEnv).toSimultaneousImportNeeds declNeeds
  let reducedImps := (← getEnv).toRawImports <| importNeeds.toNeeds.reduce (← getEnv).mkTransDeps

  let (header, _, log) ← parseCurrentHeader
  if log.hasErrors then
    -- This should be impossible.
    throwError m!"The current imports failed to parse. Errors:\n\
      {m!"\n".joinSep <| log.toList.map (·.data)}"
  let sourceImps := headerToImportRefsWithWhitespace header

  let some (msg, errs) ← liftCoreM <| Import.mkImportSuggestionMessage tk reducedImps sourceImps
    | logInfo m!"Imports are minimal."
  let formattingChangeAtMost := Import.beqUpToOrder (sourceImps.map (·.1.toImport)) reducedImps
  -- TODO: if imports are the same but not normalized, different message
  if errs.isEmpty then
    if formattingChangeAtMost then
      logInfo m!"Imports can be reformatted, but are otherwise minimal:{msg}"
    else
      logWarning m!"Imports can be reduced:{msg}"
  else
    let disclaimer := "some comments could not be carried over. \
      Please review the source comment that will be inserted after the imports."
    if formattingChangeAtMost then
      logInfo m!"Imports can be reformatted, but are otherwise minimal.\n\n\
        However, {disclaimer}\n\
        {msg}"
    else
      logWarning m!"Imports can be reduced, but {disclaimer}\n\
        {msg}"

public meta def Lean.Environment.minimalRequiredModules (env : Environment) : Array Name :=
  let required := env.requiredModules.toArray.erase env.header.mainModule
  let redundant := findRedundantImports env required
  required.filter fun n => ¬ redundant.contains n
