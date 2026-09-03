/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lean.Syntax

import all Lean.Syntax -- We use `all` to access `updateLeadingAux`.

public section

namespace Lean

deriving instance Ord for Syntax.Range

/-- Like `Lean.Syntax.updateLeading`, but preserves the starting position of the syntax if it
exists (instead of setting it to `0`). See the docstring of `updateLeading` for more details. -/
def Syntax.updateLeadingPreservingStart : Syntax → Syntax :=
  fun stx => (replaceM updateLeadingAux stx).run' (stx.getPos?.getD 0)

@[inherit_doc Syntax.updateLeadingPreservingStart]
def TSyntax.updateLeadingPreservingStart {ks} (stx : TSyntax ks) : TSyntax ks :=
  ⟨stx.raw.updateLeadingPreservingStart⟩

/-- Gets the leading whitespace of `.original` `SourceInfo`, or `none` if not `.original`. -/
def SourceInfo.getLeading? : SourceInfo → Option Substring.Raw
  | .original (leading := leading) .. => leading
  | _ => none

/-- Gets the leading whitespace of `.original` `SourceInfo`, or the empty substring if not
`.original`. -/
@[inline] def SourceInfo.getLeading (info : SourceInfo) : Substring.Raw :=
  info.getLeading?.getD "".toRawSubstring

/-- Gets the trailing whitespace of `.original` `SourceInfo`, or the empty substring if not
`.original`. -/
@[inline] def SourceInfo.getTrailing (info : SourceInfo) : Substring.Raw :=
  info.getTrailing?.getD "".toRawSubstring

/-- Clear the `leading` whitespace of the given syntax if the head `SourceInfo` is `.original`, and
otherwise leave it unchanged. See `Syntax.unsetTrailing` for removing trailing whitespace. -/
def Syntax.unsetLeading (stx : Syntax) : Syntax :=
  stx.setHeadInfo <|
    match stx.getHeadInfo with
    | .original _ pos trailing endPos => .original "".toRawSubstring pos trailing endPos
    | info => info

/-- Get the start position of the leading whitespace of `.original` `SourceInfo`, or `none` if it
is not `.original`. -/
def SourceInfo.getOriginalLeadingPos? : SourceInfo → Option String.Pos.Raw
  | .original (leading := leading) .. => some leading.startPos
  | _ => none

/-- Get the start position of the leading whitespace of `.original` `SourceInfo`, or the start
position of `.synthetic` `SourceInfo` (and `none` otherwise).

If `canonicalOnly := false` (the default), also returns `none` on non-canonical `.synthetic`
`SourceInfo`. -/
def SourceInfo.getLeadingPos? (info : SourceInfo) (canonicalOnly := false) :
    Option String.Pos.Raw :=
  match info, canonicalOnly with
  | .original (leading := leading) ..,              _     => some leading.startPos
  | .synthetic (pos := pos) (canonical := true) .., _
  | .synthetic (pos := pos) ..,                     false => some pos
  | _,                                              _     => none

/--
Get the start position of the leading whitespace of the `Syntax` if it is original, or the
start position if synthetic (and `none` otherwise).

If `canonicalOnly := false` (the default), also returns `none` on non-canonical synthetic `Syntax`.
-/
@[inline] def Syntax.getLeadingPos? (stx : Syntax) (canonicalOnly := false) :
    Option String.Pos.Raw :=
  stx.getHeadInfo.getLeadingPos? canonicalOnly

end Lean
