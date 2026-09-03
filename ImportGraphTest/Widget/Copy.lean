module

public import ImportGraph.Widget.Copy
public import Lean.Elab.Command

open ImportGraph Widget Lean Elab Command

elab "#copy" : command => liftCoreM do
  let msgs := [
    ← copyToClipboard "some copied text",
    ← copyToClipboard "some more copied text" .copiedText,
    ← copyToClipboard "some more copied text" "override text",
  ]
  logInfo m!"{m!"\n\n".joinSep msgs}"

/--
info: [click-to-copy] (Will copy:
  some copied text)

[click-to-copy] some more copied text
(Will copy:
  some more copied text)

[click-to-copy] override text
(Will copy:
  some more copied text)
-/
#guard_msgs in
#copy
