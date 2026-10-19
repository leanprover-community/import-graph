/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import ImportGraph.Widget.Collapsible.Registration
public meta import Lean.Widget.UserWidget

/-!
# Collapsible `MessageData` widget

`collapsible summary body` produces a `MessageData` that
renders in the infoview as a dropdown `<details>` element:
```
▼ summary
  body
```

`summary` is the header, and `body` is shown only when expanded.
Both `summary` and `body` are `MessageData`.

Unlike `MessageData.trace`, this carries no trace styling or `cls` tag, and unfortunately is not
lazy.

## Future work

- Make the body lazy (i.e. only constructed when the dropdown is expanded).
-/

open Lean Server

public section

namespace ImportGraph.Widget

/-- Props for the `Collapsible` widget. -/
structure CollapsibleProps where
  /-- The header `MessageData`. -/
  summary       : WithRpcRef MessageData
  /-- The hideable body revealed when the dropdown is expanded. -/
  body          : WithRpcRef MessageData
  /-- Whether the dropdown starts expanded. -/
  initiallyOpen : Bool := false
deriving Server.RpcEncodable

/-- Build a `MessageData` that renders as a collapsible dropdown: `summary` is
the header and `body` is the `MessageData` revealed when expanded. For example:
```
⯈ This is the header!
```
may be clicked to expand to
```
▼ This is the header!
  And this is the body.
```

`initiallyOpen` controls whether the dropdown starts expanded (default `false`).

Note that the `body` is already indented, and e.g. `indentD body` may insert an unwanted extra
line.
-/
public def collapsible {m : Type → Type} [Monad m] [MonadLiftT CoreM m]
    [AddMessageContext m] (summary body : MessageData)
    (initiallyOpen : Bool := false) : m MessageData := do
  let summary ← addMessageContext summary
  let body ← addMessageContext body
  let props : CollapsibleProps := {
    summary := ← (WithRpcRef.mk summary : CoreM _)
    body := ← (WithRpcRef.mk body : CoreM _ )
    initiallyOpen }
  return .ofWidget (← Widget.WidgetInstance.ofHash Collapsible.javascriptHash
    (Server.RpcEncodable.rpcEncode props))
    m!"▼ {summary}{indentD body}\n"

end ImportGraph.Widget
