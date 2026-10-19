/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau, Thomas R. Murrills
-/
module
public import ImportGraph.Widget.GoToModule.Registration
import ImportGraph.Widget.GoToModule.CoreImpl
/-! A narrow ordinary interface to the widget implementation. The private import keeps the props
structure's generated theorems out of re-exporters; the widget registration stays public. -/

public section
open Lean
namespace ImportGraph.Widget

/-- A position past the end of any file, assuming fewer than 4294967296 lines. -/
def pastEndOfFile : Lsp.Position := { line := 1 <<< 32, character := 0 }

/-- A clickable link to a module, with an optional position and display text. -/
def goToModule (modName : Name) (pos : Lsp.Position := ⟨0,0⟩)
    (overrideText : Option String := none) : CoreM MessageData :=
  goToModuleImpl modName pos overrideText

end ImportGraph.Widget
