/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public meta import ImportGraph.Imports.FromSource
public meta import ImportGraph.Imports.Pretty
public meta import ImportGraph.Shake.Environment
public meta import Lean.Elab.Command

/-!
# `#norm_imports` for interactive normalization of import blocks

This file provides `#norm_imports`, which, when run in some file, suggests normalizing that file's
imports by

1. removing redundant imports that are implied by other imports
2. sorting and grouping the imports in a standard order

`#norm_imports` currently *only* works in the module system. This restriction may be removed in the
future.

Note that this does not take into account dependencies from the current file, which should be handled by `#min_imports`.

## Future work

- Make this work outside of the module system.
- Allow configuration of the formatting behavior in accordance with `Import.pretty`'s options.
- Allow sorting by "height" of the source library in the package dependency graph, e.g. `Lean`
  modules coming first/last, etc.
-/

open ImportGraph Shake Lean Elab Command

namespace ImportGraph.NormImports

-- TODO: mention `#min_imports` when available
/-- Normalizes the imports of the current file. This removes rendundant imports and formats the
resulting import block in a standard fashion, ensuring that the same modules are available at the
same visibilities and phases. It does **not** take into account the declarations or usages of those
modules in the current file.

`#norm_imports` will keep any direct imports of `ImportGraph.Tools.NormImports`,
`ImportGraph.Tools`, or `ImportGraph` in place, while ignoring them for the calculation
of the redundant imports. -/
elab tk:"#norm_imports" : command => do
  unless (← getEnv).header.isModule do
    -- TODO: handle non-modules. The internals should still work.
    throwError "`#norm_imports` currently only works in the module system."
  let transDeps := (← getEnv).mkTransDeps

  -- ignore direct imports of `#norm_imports` for the transitive import calculation, reduce them
  -- among themselves, and add them back in afterwards
  let ignoring : NameSet := {`ImportGraph.Tools.NormImports, `ImportGraph.Tools, `ImportGraph}
  let ignoredImports : Array Import := (← getEnv).header.imports.filter (ignoring.contains ·.module)
  let ignoredReduced ←
    if ignoredImports.isEmpty then pure #[] else
      let ignoredTransNeeds := (← getEnv).transitiveClosureOf ignoredImports transDeps
      pure <| (← getEnv).toRawImports <| ignoredTransNeeds.reduce transDeps

  let currentTransNeeds := (← getEnv).currentTransNeeds transDeps (excluding := ignoring)
  let reducedImps := ((← getEnv).toRawImports <| currentTransNeeds.reduce transDeps) ++
    ignoredReduced

  -- TODO: allow the user to filter out the `#norm_imports` import?
  let (header, _, log) ← parseCurrentHeader
  if log.hasErrors then
    -- This should be impossible.
    throwError m!"The current imports failed to parse. Errors:\n\
      {m!"\n".joinSep <| log.toList.map (·.data)}"
  let impsWithRefs := headerToImportRefsWithWhitespace header
  let some (msg, errs) ← liftCoreM <| Import.mkImportSuggestionMessage tk reducedImps impsWithRefs
    | logInfo m!"Imports are normalized."
  if errs.isEmpty then
    -- Note: the widget nature of `diffGranularity := .word` effectively gives us a newline
    -- before `{msg}`, meaning we don't need one here.
    logWarning m!"Imports can be normalized:{msg}"
  else
    logWarning m!"Imports can be normalized, but some comments could not be carried over. \
      Please review the comment that will be inserted after the imports.\n{msg}"
