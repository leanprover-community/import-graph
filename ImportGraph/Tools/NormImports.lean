/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public meta import ImportGraph.Tools.NormImports.Impl

public meta section
open Lean Elab Command

/-- Normalizes the imports of the current file. This removes rendundant imports and formats the
resulting import block in a standard fashion, ensuring that the same modules are available at the
same visibilities and phases. It does **not** take into account the declarations or usages of those
modules in the current file.

`#norm_imports` will keep any direct imports of `ImportGraph.Tools.NormImports`,
`ImportGraph.Tools`, or `ImportGraph` in place, while ignoring them for the calculation
of the redundant imports. -/
elab tk:"#norm_imports" : command => ImportGraph.NormImports.runNormImports tk
