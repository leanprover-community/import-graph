/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public meta import Lean.Widget.UserWidget

/-!
# A "copy to clipboard" infoview widget

`copyToClipboard` creates a `MessageData` widget that copies a string to the
clipboard when clicked. This may be rendered as some combination of a clickable "copy" codicon and
clickable text. See `copyToClipboard` and `CopyDisplay` for details.

Note: we use `CopyDisplay` instead of e.g. an `Option String` for text and a top-level
`hasIcon := Bool` for readability and to avoid situations where the user may remove both the icon
and the text accidentally.
-/

open Lean Server

public meta section

namespace ImportGraph.Widget

/--
Props for the copy widget.

* `display`: if `some s`, render `s` as the label of a clickable link; if `none`,
  render just a copy icon.
* `copyText`: the string written to the clipboard on click.
* `hasIcon`: whether to show the copy/check codicon. Only consulted when `display`
  is `some`.
-/
structure CopyProps where
  display  : Option String := none
  copyText : String
  hasIcon  : Bool := true
  deriving Server.RpcEncodable

/-- The copy-to-clipboard widget. Renders a blue link (if `display := some s`) and/or a clickable
codicon button. Clicking either the text or the icon copies `copyText`. -/
@[widget_module]
def Copy : Widget.Module where javascript := r#"
import * as React from 'react'
const h = React.createElement

export default function ({ display, copyText, hasIcon }) {
  const [copied, setCopied] = React.useState(false)
  const timer = React.useRef(null)

  const onClick = () => {
    // The click is a user gesture, so the clipboard write is allowed.
    void navigator.clipboard.writeText(copyText)
    setCopied(true)
    clearTimeout(timer.current)
    timer.current = setTimeout(() => setCopied(false), 1000)
  }
  React.useEffect(() => () => clearTimeout(timer.current), [])

  const icon = 'codicon codicon-' + (copied ? 'check' : 'copy')
  const title = copied ? 'Copied!' : 'Copy to clipboard'

  // Icon-only: the codicon is the button.
  if (display == null)
    return h('a', { onClick, title, className: 'link pointer dim ' + icon })

  // Link text, optionally preceded by the codicon (flips copy → check in place).
  return h('a', { onClick, title, className: 'link pointer dim' },
    hasIcon && h('span', { className: 'font-codicon ' + icon }),
    hasIcon && ' ',
    display)
}
"#

/--
How `copyToClipboard` is displayed in the infoview. May be:

* `.iconOnly` to just display the "copy" codicon
* `.text s (hasIcon := true)` to display `s : String` (with or without a preceding codicon)
* `.copiedText (hasIcon := true)` to display the copied text (with or without a preceding codicon)

Everything displayed is clickable.
-/
inductive CopyDisplay where
| /-- Displays some string with or without a preceding codicon.

  Note that this only needs to be used if `hasIcon := false`. Otherwise, you may take advantage of
  the coercion from `String` to `CopyDisplay`. -/
  text (s : String) (hasIcon := true)
| /-- Displays just a "copy" codicon. This turns into a check when clicked. -/
  iconOnly
| /-- Displays whatever text is to be copied, with or without a preceding codicon. -/
  copiedText (hasIcon := true)

instance : Coe String CopyDisplay where
  coe s := .text s

/-- Build a `MessageData` widget that, when clicked, copies `copyText` to the clipboard.

By default, this is simply a clickable "copy" codicon, but text may be provided through the
`display` argument. Specifically:

* `display := .iconOnly` (the default) renders just a clickable "copy" codicon.
* `display := .text s (hasIcon := true)` or equivalently `display := (s : String)` (in the
  `hasIcon := true` case) displays `s : String` (with or without a preceding codicon)
* `display := .copiedText (hasIcon := true)` displays the copied text (with or without a preceding
  codicon)

Use as e.g. `let msg ← copyToClipboard "text to be copied" (display := "(click to copy!)")`. -/
def copyToClipboard (copyText : String) (display : CopyDisplay := .iconOnly) :
    CoreM MessageData := do
  let (display, hasIcon) := match display with
    | .iconOnly => (none, true)
    | .copiedText hasIcon => (copyText, hasIcon)
    | .text s hasIcon => (s, hasIcon)
  let props : CopyProps := { display, copyText, hasIcon }
  return .ofWidget
    (← Widget.WidgetInstance.ofHash Copy.javascriptHash <| Server.RpcEncodable.rpcEncode props)
    m!"[click-to-copy]{if let some display := display then m!" {display}\n" else m!" "}\
      {m!"(Will copy:{indentD copyText})"}"

end ImportGraph.Widget
