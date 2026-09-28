/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public meta import ImportGraph.Imports.Pretty
public meta import ImportGraph.Lean.MessageData
public meta import ImportGraph.Shake.Environment
-- public meta import ImportGraph.Imports.ImportGraph -- for old `#find_home`
-- public meta import ImportGraph.Graph.TransitiveClosure -- for old `#find_home`
-- public meta import ImportGraph.Imports.RequiredModules -- for old `#find_home`
public import ImportGraph.Widget.Collapsible
public import ImportGraph.Widget.Copy
-- public import ImportGraph.Util.GoTo

/-!
# `#show_imports for <cmd>`

This module provides the simple command `#show_imports for <cmd>` which shows a minimal set of
imports needed to elaborate `<cmd>`, including the import needs of the declarations produced during
that command, any prior declarations from the same file, and the syntax of the command.

Note that this currently does not work for `example`.
-/

meta section

open ImportGraph Shake Widget Lean Elab Command

namespace ImportGraph.Shake

/--
`#show_imports for <cmd>` shows a minimal set of imports needed by `<cmd>`. This includes

- The import needs of the declarations produced during elaboration
- The import needs of any previously-defined declarations from the same file used in those
  declarations
- The syntax needs of the current command (but not of its dependencies)

The imports may be copied by clicking the "copy" icon.
-/
elab tk:"#show_imports" ppSpace &"for" ppLine cmd:command : command => do
  unless (← getEnv).header.isModule do
    logWarningAt tk "`#show_imports` may not function correctly outside of the module system. \
      This may be addressed in the future."
  let transDeps := (← getEnv).mkTransDeps
  let (declNeeds, newDecls) ← withElabCommandCapturingNeeds cmd
  let importNeeds ← liftCoreM ((← getEnv).toSimultaneousImportNeeds declNeeds).run'
  let reduced := (← getEnv).toRawImports <| importNeeds.toNeeds.reduce transDeps
  let prettyImports := Import.pretty reduced
  let copyIcon ← liftCoreM <| copyToClipboard s!"{prettyImports}"
  let moreInfo ← liftCoreM do
    if newDecls.isEmpty then pure m!"" else
      let new ← collapsible m!"New declarations produced by this command"
        m!"{.bulletList <| newDecls.toList.map MessageData.ofConstName}"
      let prior ← do
        let prior := declNeeds.keysArray.filter (!newDecls.contains ·)
        if prior.isEmpty then pure m!"" else
          collapsible m!"Declarations from the current file needed by this command"
            m!"{.bulletList <| prior.toList.map MessageData.ofConstName}"
      pure m!"{new}{prior}"
  let metas := newDecls.filter (isMarkedMeta (← getEnv)) |>.map MessageData.ofConstName
  unless metas.isEmpty do
    logWarningAt tk
      m!"Warning: Some declarations are marked meta. `#show_imports` does not yet handle meta IR; \
        the following is an approximation.\n{.bulletList metas.toList}"
  logInfoAt tk m!"{copyIcon}Imports needed:\n\n\
    {prettyImports}\n\n\
    {moreInfo}"
