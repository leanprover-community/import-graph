/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau, Thomas R. Murrills
-/
module

public meta import ImportGraph.Widget.GoToModule.Core

/-!
# A Go-To-Module widget

Provides `goToModule` through a meta import without re-exporting the widget's implementation
datatypes or their generated theorems. Import `ImportGraph.Widget.GoToModule.Declarations` for
`goToModuleOfDecls`, `goToModuleOfDecl`, and declaration-range helpers.
-/
