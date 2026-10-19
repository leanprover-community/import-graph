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

/-- The copy-to-clipboard widget. Renders a blue link (if `display := some s`) and/or a clickable
codicon button. Clicking either the text or the icon copies `copyText`. -/
@[widget_module]
def Copy : Widget.Module where javascript := r#"
import * as React from 'react'
const h = React.createElement

const states = {
  idle:   { iconName: 'copy',  title: 'Copy to clipboard' },
  copied: { iconName: 'check', title: 'Copied!',    linger: 1000 },
  failed: { iconName: 'error', title: 'Copy failed; see console', linger: 3000,
    color: 'var(--vscode-errorForeground)' },
}

export default function ({ display, copyText, hasIcon }) {
  const [status, setStatus] = React.useState('idle')
  const timer = React.useRef(null)

  // Flash the state for 'copied' or 'failed' briefly, then fall back to 'idle'.
  const flash = action => {
    setStatus(action)
    clearTimeout(timer.current)
    timer.current = setTimeout(() => setStatus('idle'), states[action].linger ?? 1000)
  }

  const onClick = async () => {
    try {
      // Called synchronously in the click handler, so it still counts as a
      // user gesture; awaiting afterwards is fine. The catch covers both a
      // rejected write (permission denied, document not focused) and a
      // synchronous throw when navigator.clipboard is undefined.
      await navigator.clipboard.writeText(copyText)
      flash('copied')
    } catch (e) {
      console.warn('Lean widget ImportGraph.Widget.copyToClipboard failed to copy to clipboard:', e)
      flash('failed')
    }
  }

  React.useEffect(() => () => clearTimeout(timer.current), [])

  const { iconName, title, color } = states[status]

  const icon = 'codicon codicon-' + iconName
  const style = color ? { color } : undefined

  // Icon-only: the codicon is the button.
  if (display == null)
    return h('a', { onClick, title, style, className: 'link pointer dim ' + icon })

  // Show the (x) to grab attention if we failed.
  const showIcon = hasIcon || status === 'failed'

  // Link text, optionally preceded by the codicon.
  return h('a', { onClick, title, style, className: 'link pointer dim' },
    showIcon && h('span', { className: 'font-codicon ' + icon }),
    showIcon && ' ',
    display)
}
"#

end ImportGraph.Widget
