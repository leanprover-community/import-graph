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
public meta import ImportGraph.Lean.MessageData
public meta import ImportGraph.Shake.EnvExtension
public meta import ImportGraph.Shake.Environment

/-!
# `#min_imports`

This module provides `#min_imports`, which minimizes the imports according to which are actually
used in the declarations and syntax of the current file. It respects module-system relevant
dependencies.

Note: testing `#min_imports` should not use `#guard_msgs`, as `#min_imports` reports back from the
end of the file. Instead, this file exposes `logAllMinImports` and `resetMinImports` for testing.

## Future work

- Refactor into more generic API usable by other infrastructure (in progress)
- Improve the ergonomics around waiting for a message (loading widget, yellow bar)
- Split out into API
- Turn into an optional (expensive) linter
- Account for non-persistent uses like `example`, `#check`, `#eval` under an option
- `#min_imports so far`
-/

open ImportGraph Shake Lean Elab Command

namespace ImportGraph.Shake

/-! ## Old `#min_imports` infrastructure -/

open Lean Environment in
/-- **Does not work within the module system.** Get the names of all needed imports. -/
public meta def Lean.Environment.minimalRequiredModules (env : Environment) : Array Name :=
  let required := env.requiredModules.toArray.erase env.header.mainModule
  let redundant := findRedundantImports env required
  required.filter fun n => ¬ redundant.contains n

/-! ## Modern `#min_imports` -/

/--
Minimize the imports of the entire current module in the module system.

This command may be written anywhere in the file, and will still take into account the whole file.
As such, the command will remain silent until the file finishes elaborating.

Currently, it accounts for constants, syntax, shake records, and runtime IR (except for
attributes like `@[inline]`), but not yet meta IR from new `meta def`s.

To eliminate redundancies among the current file's imports without taking into account whether they
are actually used or not, see `#norm_imports`.

This command currently only suggests module-system import blocks. It may still help with modulizing
non-module files if all necessary imports are already modules, or by indicating which necessary
imports are non-modules.
-/
syntax (name := minImportsStx) "#min_imports" : command

/-- Get the `DeclNeeds` of all new constants in the `Environment`, given the `Array Syntax` of all
commands in the file. Ignores any syntax (at the top level) with kind in `ignoredKinds`. -/
meta def getModuleDeclNeeds (cmds : Array Syntax) (ignoredKinds : NameSet := {}) :
    CommandElabM (DeclNeeds × Std.HashMap Name (Option Stance) × List ExtraModUse) := do
  let mut declNeeds := ∅
  let env ← getEnv
  -- TODO: more accurate targeting of declarations and commands.
  for (decl, _) in env.constants.map₂ do
    declNeeds := calcDeclConstInfoNeeds decl env declNeeds
  for cmd in cmds do
    unless ignoredKinds.contains cmd.getKind do
      declNeeds ← declNeeds.calcSyntaxNeeds env (declNeeds.keysArray) cmd
  let (finalDeclNeeds, s) ← liftCoreM <| StanceM.run <| declNeeds.calcIRNeeds
  -- Eagerly erases extra mod uses of the `ignoredKinds`. Technically this is overzealous, but
  -- syntax records extra mod uses in a coarse fashion.
  let ignoredMods := ignoredKinds.toArray.filterMap fun ignoredKind => do
    let idx ← env.getModuleIdxFor? ignoredKind
    return env.allImportedModuleNames[idx]!
  let extraModUses := getNewExtraModUses env |>.1.filter fun use =>
    !(ignoredMods.contains use.module)
  return (finalDeclNeeds, s, extraModUses)

/-- Log a `#min_imports` message at the given `tk`, looking at the provided `cmds` syntax and all
constants in the environment. -/
public meta def logMinImports (tk : Syntax) (cmds : Array Syntax) :
    CommandElabM Unit := withRef tk do
  let (declNeeds, s, extraModUses) ← getModuleDeclNeeds cmds (ignoredKinds := {``minImportsStx})
  if let some warning := declNeeds.metaWarning? (← getEnv) "#min_imports" then
    logWarning warning
  let transDeps := (← getEnv).mkTransDeps

  let importNeeds ← liftCoreM do StanceM.run' (s := s) do
    (← getEnv).toSimultaneousImportNeeds declNeeds
  let importNeeds := (← getEnv).importNeedsOfExtraModUses (init := importNeeds) extraModUses
  let mut reducedNeeds := importNeeds.toNeeds.reduce transDeps
  let mut reducedImps := (← getEnv).toRawImports reducedNeeds

  /- If `#min_imports` is not provided, preserve the largest direct import of tooling from
  ImportGraph (but demote it to private), or else add it in. (Note: the module index itself should
  always exist if we are able to use `#min_imports`.) In the future, `#min_imports` should delete
  itself. -/
  if let some minImportsModuleIdx := (← getEnv).getModuleIdx? `ImportGraph.Tools.MinImports then
    unless transDeps⟦importNeeds.toNeeds⟧.any (·.has minImportsModuleIdx) do
      let env ← getEnv
      -- In order from broadest to narrowest
      let possibleDirectImports :=
        #[`ImportGraph, `ImportGraph.Tools, `ImportGraph.Tools.MinImports].filterMap fun modName =>
          return (← env.getModuleIdx? modName, modName)
      let (toolImportIdx, module) :=
        (possibleDirectImports.find? fun (_, mod) => env.header.imports.any (·.module == mod)).getD
          (minImportsModuleIdx, `ImportGraph.Tools.MinImports)
      let toolImport := { module, isExported := false, isMeta := false }
      -- We use this later for checking if the modification is just a normalization.
      reducedNeeds := reducedNeeds.union { toolImport with } {toolImportIdx}
      reducedImps := reducedImps.push toolImport

  let (header, _, log) ← parseCurrentHeader
  if log.hasErrors then
    -- This should be impossible.
    throwError m!"The current imports failed to parse. Errors:\n\
      {m!"\n".joinSep <| log.toList.map (·.data)}"
  let sourceImps := headerToImportRefsWithWhitespace header
  let some (msg, errs) ← liftCoreM <| Import.mkImportSuggestionMessage tk reducedImps sourceImps
    | logInfo m!"Imports are minimal."

  let disclaimer := s!"some comments could not be carried over. \
      Please review the source comment that will be inserted after the imports."

  let env ← getEnv
  if !env.header.isModule then
    let nonModuleImps := reducedImps.filter fun imp =>
      !env.header.moduleData[env.getModuleIdx? imp.module |>.get!]!.isModule
    if nonModuleImps.isEmpty then
      -- TODO: allow suggesting the module keyword + section too in `ImportGraph.Imports.Pretty`
      logWarning m!"`#min_imports` currently only produces modulized imports. Luckily, all \
        necessary imports are module files, so the following suggestion may assist in modulizing \
        this file.\n\n\
        ⚠️ If you accept this suggestion, you will need to add the `module` keyword above the \
        suggested import block to fix the file's imports, and (`@[expose]`) \
        `public section` below the block to fix the rest of the file.\n\n\
        Modulization may affect the declarations and which imports are necessary, so re-run \
        `#min_imports` after modulization.\
        {if errs.isEmpty then "" else s!"\n\nNote that {disclaimer}\n"}{msg}"
    else
      logWarning m!"`#min_imports` currently only produces modulized imports, but some \
        necessary imports are non-module files. These files would not be importable in a module \
        file:\n\n{.bulletList <| nonModuleImps.toList.map (·.module)}"
  else
    let formattingChangeAtMost := Import.beqUpToOrder (sourceImps.map (·.1.toImport)) reducedImps
    let normalizationChangeAtMost :=
      -- Whether the current imports are provided by the reduced imports
      -- Need to add `Init` to emulate it being a real file:
      let reducedNeeds := env.implicitInitNeeds reducedNeeds
      (← getEnv).currentTransNeeds transDeps |>.subsumedBy reducedNeeds transDeps
    let errMsgOrConnector := if errs.isEmpty then ":" else
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

/-!
### Bespoke infrastructure

Ideally, this should be replaced with more general backreporting infrastructure.
-/

/-- A bespoke (nonpersistent) environment extension for storing the syntax ranges of any
`#min_imports` commands. A module linter later in the file reads this and reports on the given
range(s). -/
meta initialize minImportsRequests : EnvExtension (List Syntax) ←
  registerEnvExtension (pure [])

/-- Logs at all requests in the `minImportsRequests` extension. Exposed for testing. -/
@[inline] public meta def logAllMinImports (cmds : Array Syntax) : CommandElabM Unit := do
  for tk in minImportsRequests.getState (← getEnv) do
    -- This recomputes per request, but multiple requests are rare.
    logMinImports tk cmds

/-- A module linter that implements `#min_imports` commands by reading from requests lodged by the
`#min_imports` elaborator in the `minImportsRequests` env extension. -/
meta def minImportsModuleLinter : ModuleLinter where
  run cmds := do
    -- We only rarely write `#min_imports`, so special case this and exit as soon as possible.
    -- TODO: can we exit faster via some other mechanism, e.g. `IO.Ref` or `set_option`?
    if minImportsRequests.getState (← getEnv) |>.isEmpty then
      return
    logAllMinImports cmds

meta initialize addModuleLinter minImportsModuleLinter

/-- Erases all `#min_imports` requests up until this point. Exposed for testing so that the
extension may remain private API. -/
public meta def resetMinImports : CommandElabM Unit :=
  modifyEnv fun env => minImportsRequests.setState env []

elab_rules : command
| `(#min_imports%$tk) => do
  modifyEnv fun env => minImportsRequests.modifyState env (tk :: ·)
