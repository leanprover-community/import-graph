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
import ImportGraph.Imports.RequiredModules -- for deprecated `minimalRequiredModules`
import ImportGraph.Imports.Redundant -- for deprecated `minimalRequiredModules`

/-
Comments were present when importing `ImportGraph.Util.RunLater`, but this module is now imported differently as `import ImportGraph.Util.RunLater`.
Decide if the following original comments still apply:
```
-- public meta import ImportGraph.Tools.NeedsGrid
public meta import ImportGraph.Util.RunLater
```

Comments were present when importing `Lean.Elab.Command`, but this module is now imported differently as `public import Lean.Elab.Command`.
Decide if the following original comments still apply:
```
import all Lean.Elab.Command -- for `recordUsedSyntaxKinds`
```

The following imports did not appear in the new import list, but had comments around them:
```
-- import all ImportGraph.Tools.NeedsGrid
import all ImportGraph.Shake.DeclNeeds
```

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

/--
Return the names of the modules in which constants used in the current file were defined,
with modules already transitively imported removed.

Note that this will *not* account for tactics and syntax used in the file,
so the results may not suffice as imports.
-/
@[deprecated
  "Use `Environment.mkTransDeps` and `Needs.reduce` to handle imports in the module system"
  (since := "2026-10-01")]
def Lean.Environment.minimalRequiredModules (env : Environment) : Array Name :=
  let required := env.requiredModules.toArray.erase env.header.mainModule
  let redundant := findRedundantImports env required
  required.filter fun n => ¬ redundant.contains n
