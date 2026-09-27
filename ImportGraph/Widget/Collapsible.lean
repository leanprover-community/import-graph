/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

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

public meta section

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

/-- The dropdown widget: a `<details>` component with `MessageData` header and body.

Note: The body is mounted lazily. Since an ordinary closed `<details>` component would still mount
the body even if it were closed, we track whether the `<details>` component has ever been opened
manually and mount it on first open. It then stays mounted. -/
@[widget_module]
def Collapsible : Widget.Module where javascript :=
r#"
import * as React from 'react'
import { InteractiveMessageData } from '@leanprover/infoview'
const h = React.createElement

export default function (props) {
  const { summary, body, initiallyOpen } = props
  const [open, setOpen] = React.useState(!!initiallyOpen)
  const [everOpened, setEverOpened] = React.useState(!!initiallyOpen)
  const onClick = e => {
    // Clicks inside React portals (e.g. pinned tooltips) bubble here through
    // the React tree, but their DOM nodes live outside the summary.
    if (!(e.target instanceof Node)) return
    if (!e.currentTarget.contains(e.target)) return
    e.preventDefault()
    setEverOpened(true)
    setOpen(o => !o)
  }
  return h('details', { open },
    h('summary',
      { style: { cursor: 'pointer', userSelect: 'none' }, onClick },
      h(InteractiveMessageData, { msg: summary })),
    everOpened && h('div',
      { style: { marginLeft: '1em', marginTop: '0.25em' } },
      h(InteractiveMessageData, { msg: body })))
}
"#

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
