module

public meta import Lean.Elab.Command
public meta import ImportGraph.Widget.Collapsible

open ImportGraph Widget Lean Elab Command

elab "#collapse" : command => liftCoreM do
  let msgs := [
    ← collapsible m!"a header! (with interactivity: {.ofConstName ``True})"
      m!"a body (with interactivity: {.ofConstName ``True})",
    ← collapsible m!"a header!" (← collapsible m!"a nested header" m!"a body"),
    ← collapsible m!"a header!"
      m!"{← collapsible m!"a nested header" m!"a body"}\
        {← collapsible m!"an adjacent nested header" m!"another body"}",
  ]
  logInfo <| m!"\n".joinSep msgs

-- Note that the extra newline in the second case is due to approximating the newline behavior in
-- fallback `MessageData`. A second newline does not appear in the infoview.
/--
info: ▼ a header! (with interactivity: True)
  a body (with interactivity: True)

▼ a header!
  ▼ a nested header
    a body
  ⏎

▼ a header!
  ▼ a nested header
    a body
  ▼ an adjacent nested header
    another body
-/
#guard_msgs in
#collapse
