/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module
public import Lean.Widget.UserWidget
public meta import Lean.Widget.UserWidget
public section
open Lean Server
namespace ImportGraph.Widget

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

end ImportGraph.Widget
