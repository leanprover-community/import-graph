/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lean.Message

/-! # Extra utilities for `Lean.MessageData` -/

public section

open Lean

/-- Given `[msg₁, msg₂, ...]`, creates a bulleted list of the form
```
• msg₁
• msg₂
...
```
By default, a single-message list `[msg]` is still rendered as `• msg`. If instead
`forceList := false`, then a single-message list `[msg]` is rendered simply as `msg`.
-/
public def Lean.MessageData.bulletList (msgs : List MessageData) (forceList := true) :
    MessageData := Id.run do
  unless forceList do
    if let [msg] := msgs then
      return msg
  m!"\n".joinSep (msgs.map (m!"• {.nest 2 ·}"))
