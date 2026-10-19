/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau, Thomas R. Murrills
-/
module
public import Lean.Widget.UserWidget
import ImportGraph.Widget.GoToModule.Registration
public section
open Lean
namespace ImportGraph.Widget

/-- Widget props for `ImportGraph.Widget.GoToModule`. -/
structure GoToModuleProps where
  modName : Name
  pos : Lsp.Position := { line := 0, character := 0 }
  overrideText : Option String := none
deriving Server.RpcEncodable

/-- A position past the end of any file, assuming no file has more than 4294967296 lines. -/
def pastEndOfFileImpl : Lsp.Position := { line := 1 <<< 32, character := 0 }

/-- Creates a clickable link which takes the user to the provided module.

By default, takes the user to the top of the module. Providing `pos` will bring the user to that
position in the file.

By default, the module name is used as the link's text. The text can be overridden by providing
`overrideText`.
-/
def goToModuleImpl (modName : Name) (pos : Lsp.Position := ⟨0,0⟩)
    (overrideText : Option String := none) : CoreM MessageData := do
  let p : GoToModuleProps := { modName, pos, overrideText }
  let posMsg := if pos.line ≥ pastEndOfFileImpl.line then m!"end" else
    m!"{pos.line + 1}:{pos.character}"
  return .ofWidget
    (← Widget.WidgetInstance.ofHash GoToModule.javascriptHash <|
      Server.RpcEncodable.rpcEncode p)
    -- Note that `Lsp.Position`s are 0-indexed, hence the `+1`.
    -- (However, Lean uses 0-indexed columns in messages.)
    m!"{overrideText.getD <| toString modName} ({posMsg})"

end ImportGraph.Widget
