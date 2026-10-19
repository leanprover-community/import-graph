/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau, Thomas R. Murrills
-/
module
public import Lean.Widget.UserWidget
public section
open Lean
namespace ImportGraph.Widget

/-- Widget props for `ImportGraph.Widget.GoToModule`. -/
structure GoToModuleProps where
  modName : Name
  pos : Lsp.Position := { line := 0, character := 0 }
  overrideText : Option String := none
deriving Server.RpcEncodable

end ImportGraph.Widget
