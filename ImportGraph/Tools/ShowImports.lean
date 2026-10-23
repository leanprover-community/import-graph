/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public meta import ImportGraph.Imports.Pretty
public meta import ImportGraph.Lean.MessageData
public meta import ImportGraph.Shake.Environment
public import ImportGraph.Widget.Collapsible
public import ImportGraph.Widget.Copy

/-!
# `#show_imports for <cmd>`

This module provides the simple command `#show_imports for <cmd>` which shows a minimal set of
imports needed to elaborate `<cmd>`, including the import needs of the declarations produced during
that command, any prior declarations from the same file, and the syntax of the command.

Note that this currently does not work for `example`.

## Future work

- Better attribution of import needs to declarations/syntax/etc.
-/

meta section

open ImportGraph Shake Widget Lean Elab Command

namespace ImportGraph.Shake

/--
`#show_imports for <cmd>` shows a minimal set of imports needed by `<cmd>`. This includes

- The import needs of any declarations elaborated in the command
- The import needs of any previously-defined constants from the same file used in those
  declarations
- The syntax needs of the current command (but not the syntax needs of same-file dependencies of
  constants produced in the command, which we do not have access to)
- Any extra shake records produced by the command. (These may be due to syntax, tactics, etc.)

The imports may be copied by clicking the "copy" icon.
-/
elab tk:"#show_imports" ppSpace &"for" ppLine cmd:command : command => do
  unless (← getEnv).header.isModule do
    logWarningAt tk "`#show_imports` may not function correctly outside of the module system. \
      This may be addressed in the future."
  if cmd.raw.find? (·.isOfKind  ``Parser.Command.example) |>.isSome then
    logWarningAt tk "Note: `#show_imports` does not yet handle `example`s. Please create a \
      temporary declaration."
  let transDeps := (← getEnv).mkTransDeps
  let (declNeeds, newDecls, newExtraModUses) ← withElabCommandCapturingNeeds cmd
  let importNeeds ← liftCoreM ((← getEnv).toSimultaneousImportNeeds declNeeds).run'
  let importNeeds := (← getEnv).importNeedsOfExtraModUses (init := importNeeds) newExtraModUses
  let reduced := (← getEnv).toRawImports <| importNeeds.toNeeds.reduce transDeps

  let importsNeeded ← if reduced.isEmpty then pure m!"This command does not need any imports." else
    let prettyImports := Import.pretty reduced
    let copyIcon ← liftCoreM <| copyToClipboard s!"{prettyImports}"
    pure m!"{copyIcon} Imports needed:\n\n\
      {prettyImports}"
  let moreInfo ← liftCoreM do
    -- Show a collapsible bulleted list of constants sorted appropriately with the given header.
    -- TODO: factor this out?
    let showConsts (consts : Array Name) (header : MessageData) : CoreM MessageData := do
      -- TODO: also show meta
      let consts := consts.map (fun n => (privateToUserName n, isPrivateName n))
        |>.qsort (·.1.toString < ·.1.toString)
      collapsible header (initiallyOpen := consts.size ≤ 5)
        m!"{.bulletList <| consts.toList.map fun (userName, isPrivate) =>
          m!"{.ofConstName userName}{if isPrivate then " (private)" else ""}"}"
    let new ← if newDecls.isEmpty then
        pure m!"This command did not produce any new constants."
      else
        showConsts newDecls m!"New constants produced by this command:"
    let prior ← do
      let prior := declNeeds.keysArray.filter (!newDecls.contains ·)
      if prior.isEmpty then pure m!"" else
        showConsts prior m!"Constants from the current file needed by this command:"
    let extraModUses ← if newExtraModUses.isEmpty then pure m!"" else
      collapsible m!"Extra shake import needs recorded by this command:"
        (initiallyOpen := false) -- Shake records may be odd, verbose, and include `Init`
        m!"{.bulletList <| newExtraModUses.map fun { module, isExported, isMeta } => m!"\
          {if isExported then "public " else ""}\
          {if isMeta then "meta " else ""}\
          import {module}"}"
    pure m!"{new}{prior}{extraModUses}"
  if let some warning := declNeeds.metaWarning? (← getEnv) "#show_imports" then
    logWarningAt tk warning
  logInfoAt tk m!"{importsNeeded}\n\n\
    {moreInfo}"
