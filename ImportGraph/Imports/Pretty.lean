/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lean.CoreM

meta import Lean.Parser.Module.Syntax
import ImportGraph.Lean.Syntax
import Lean.Meta.Hint

/-!
# Pretty-printing imports

This module defines the following utilities for pretty-printing imports:
- `ImportGraph.Lean.Import.pretty` and `prettyHeader` for printing `Array Import` as source blocks
  or headers, respectively.
  - These take an optional `Import.FormatBehavior` parameter to control import sorting and
    grouping. By default, imports are grouped by visibility (`public`/(private)/`all`), sorted by
    phase and module name, and visibility groups are separated by extra newlines.
- `headerToImportRefs`(`WithWhitespace`) to track source positions and comments around existing
  imports in source
- `prettyWithSourceWhitespace` to pretty-print new `Import`s while attaching comments from source
  header syntax. This allows us to reformat existing imports while preserving e.g. `shake`
  annotations (and any other informative comments).
  - If comments cannot be carried over (or may no longer apply), this is (by default) explained in
    a comment shown below the import block.
- `mkImportSuggestionMessage`, which creates a suggestion reformatting imports. This is used by
  `#norm_imports`.

## Future Work

- Some parts of this API only work in the module system. In general, we should also support
  non-modules.
-/

public section

namespace ImportGraph

open ImportGraph Lean Syntax

namespace Lean

/-- Extends `Import` with ``stx : TSyntax `Lean.Parser.Module.import`` to allow reporting at the
given import and processing of whitespace. -/
structure ImportRef extends Import where
  stx : TSyntax ``Parser.Module.import
deriving Repr, Inhabited, BEq

/-- Returns the module `Ident` (following `(public)? (meta)? import (all)?` of a given
`ImportRef`. Returns `.missing` if the identifier somehow has a dangling dot (the parser should,
however, never succeed in producing such syntax) or is otherwise malformed. -/
def ImportRef.getIdent (i : ImportRef) : Ident :=
  match i.stx with
  | `(Parser.Module.import| $[public]? $[meta]? import $[all]? $n:ident) => n
  | _ => ⟨.missing⟩

/-- Destructures header syntax (`(module)? (prelude)? $imports*`) into an array of `Import`s
together with the import syntax that gave rise to them. Creates an array of `.missing` when the
syntax is malformed. See also `headerToImports`. -/
def headerToImportStx (header : TSyntax ``Parser.Module.header) :
    TSyntaxArray ``Parser.Module.import :=
  match header with
  | `(Parser.Module.header| $[module%$moduleTk]? $[prelude]? $imports*) =>
    imports
  | _ => #[⟨.missing⟩]

/-- Destructures header syntax (`(module)? (prelude)? $imports*`) into an array of `Import`s
together with the import syntax that gave rise to them. See also `headerToImports`. -/
def getModule (header : TSyntax ``Parser.Module.header) : Option Syntax :=
  match header with
  | `(Parser.Module.header| $[module%$moduleTk]? $[prelude]? $_*) => moduleTk
  | _ => none

/-- Destructures header syntax (`(module)? (prelude)? $imports*`) into an array of `Import`s
together with the import syntax that gave rise to them. See also `headerToImports`. -/
def headerToImportRefs (header : TSyntax ``Parser.Module.header) : Array ImportRef :=
  match header with
  | `(Parser.Module.header| $[module%$moduleTk]? $[prelude]? $imports*) =>
    imports.map fun
      | stx@`(Parser.Module.import|
          $[public%$publicTk]? $[meta%$metaTk]? import $[all%$allTk]? $n:ident) =>
        { module := n.getId
          importAll := allTk.isSome
          isExported := publicTk.isSome || moduleTk.isNone
          isMeta := metaTk.isSome
          stx := ⟨stx⟩ }
      | _ => { module := `illformedStx, stx := ⟨.missing⟩ }
  | _ => #[{ module := `illformedStx, stx := ⟨.missing⟩ }]

namespace Import

/-- Considers imports with `public` to come first; then those without `all`; then those with
`meta`; then compares the modules alphabetically. -/
def comparePretty (i₁ i₂ : Import) : Ordering :=
  (compare i₁.isExported i₂.isExported).swap -- `public import < import`
    |>.then (compare i₁.importAll i₂.importAll) -- `import < import all`
    |>.then (compare i₁.isMeta i₂.isMeta).swap -- `meta import < import`
    |>.then (Name.cmp i₁.module i₂.module)

/-- Considers imports with `public` to come first; then those without `all`; then those with
`meta`; then compares the modules alphabetically; then compares starting source position, c
considering those without a starting position to come first. -/
def _root_.ImportGraph.Lean.ImportRef.comparePretty (i₁ i₂ : ImportRef) : Ordering :=
  i₁.toImport.comparePretty i₂.toImport |>.then <| compare i₁.stx.raw.getPos? i₂.stx.raw.getPos?

/-- Uses `Import.comparePretty`. Not tagged as an instance by default. -/
local instance instImportOrdPretty : Ord Import where
  compare := Import.comparePretty

/-- Uses `ImportRef.comparePretty`. Not tagged as an instance by default. -/
local instance instImportRefOrdPretty : Ord ImportRef where
  compare := ImportRef.comparePretty

/-- Whether two `Array Import`s contain the same imports when considered as a (multi)set. -/
def beqUpToOrder (imps₁ imps₂ : Array Import) : Bool :=
  imps₁.qsortOrd == imps₂.qsortOrd

/-- Whitespace (including comments) surrounding an import. -/
/- TODO: consider using `Substring.Raw` or string slices for efficiency. These strings are usually
small and may be manipulated, and the `Substring.Raw` API is unfriendly, so for convenience we just
use `String`s. -/
structure Whitespace where
  /-- The leading whitespace after regrouping whitespace so that `trailing` does not
  include newlines. Includes exactly one newline of ASCII whitespace at the end of `leading` if
  there are non-whitespace characters in it (which is suitable for normalized `import` comments),
  and no ASCII whitespace at the beginning. If there are no non-ASCII-whitespace characters in
  `leading`, it is empty. -/
  -- TODO: consider storing the newline behavior in a `Bool`, since the insistence on a
  -- final newline is overly specific to `import`s.
  leading : String
  /-- The trailing whitespace after regrouping whitespace so that the `trailing` whitespace has no
  newlines. May have ASCII whitespace on the left. -/
  trailing : String
deriving Repr, Inhabited, Hashable, BEq, ToJson, FromJson

namespace Whitespace

-- TODO: could avoid `length` if we stuck with `Substring.Raw`
/--
Sorts whitespace first by `leading` length, then `trailing` length, then alphabetical in each.
-/
instance : Ord Whitespace where
  compare ws₁ ws₂ :=
    (compare ws₁.leading.length ws₂.leading.length).swap -- Longer leading comments come first
      |>.then (compare ws₁.trailing.length ws₂.trailing.length) -- Annotations come last
      |>.then (compare ws₁.leading ws₂.leading) -- Alphabetical for completeness
      |>.then (compare ws₂.trailing ws₂.trailing)

instance : Ord (Import × Whitespace) where
  compare := fun (i₁, ws₁) (i₂, ws₂) =>
    compare i₁ i₂ |>.then (compare ws₁ ws₂)

/-- Formats `a` with the whitespace `ws` wrapped around it. -/
@[inline] def around (a : α) (ws : Whitespace) [ToFormat α] : Format :=
  f!"{ws.leading}{a}{ws.trailing}"

/-- Whitespace with empty strings for both the `leading` and `trailing` values. -/
@[inline] def empty : Whitespace where
  leading  := ""
  trailing := ""

/-- Whether both the `leading` and `trailing` fields of `Whitespace` are empty. -/
@[inline] def isEmpty (ws : Whitespace) : Bool :=
  ws.leading.isEmpty && ws.trailing.isEmpty

-- For convenience with `joinSep`
instance : ToFormat (Import × Whitespace) where
  format := fun (imp, ws) => ws.around imp

end Whitespace

/-
TODO: Consider the edge case:
```
import Foo
-- Comment about Foo

-- Comment about Bar
import Bar
```
We could detect this, but don't yet.
Note: we'd need to not count newlines that appear within `/- -/`.

We also don't handle
```
/-
Copyright ...
-/
import
```
but that's only a problem in non-modules, which we don't handle anyway yet.

We also don't account for trailing comments on lines after the line of the final import.
-/
/-- Convert parsed header syntax into `ImportRef`s after regrouping leading and trailing whitespace
so that trailing whitespace has no newlines, and extract the leading and trailing whitespace into a
useful ASCII-whitespace normalized form in `Whitespace`. -/
def _root_.ImportGraph.headerToImportRefsWithWhitespace (header : TSyntax ``Parser.Module.header) :
    Array (ImportRef × Whitespace) :=
  let imps := headerToImportRefs header.updateLeadingPreservingStart
  imps.map fun imp =>
    let leading := imp.stx.raw.getHeadInfo.getLeading.trim
    let leading := if leading.isEmpty then leading.toString else leading.toString ++ "\n"
    -- trailing already does not include `'\n'` after `updateLeadingPreservingStart`.
    -- Unlike `leading`, we preserve any initial ASCII whitespace.
    let trailing := imp.stx.raw.getTailInfo.getTrailing.toString
    (imp, { leading, trailing })

/-- A configuration option guiding the behavior of import block pretty-printing functions (e.g
`ImportGraph.Lean.Import.pretty`) which determines the order and grouping of imports. -/
protected inductive FormatBehavior where
| /-- Does not sort or group imports at all. -/
  none
| /-- Sorts an array of imports first by visibility (`public` first, then private (no token), then
  `all`), then by phase (`meta` imports first), then alphabetically. -/
  sorted
| /-- Groups imports by visibility (first `public`, then private (no token), then `all`) with an
  extra newline in between groups. Within groups, sorts first by the `meta` token then
  alphabetically within groups. If `splitMeta := true` (default: `false`), also inserts an extra
  newline between `meta` imports and non-`meta` imports. -/
  grouped (splitMeta := false)

@[inline, inherit_doc FormatBehavior.sorted]
def sortPretty (imports : Array Import) : Array Import := imports.qsortOrd

@[inline, inherit_doc FormatBehavior.sorted]
def _root_.ImportGraph.Lean.ImportRef.sortPretty (imports : Array ImportRef) : Array ImportRef :=
  imports.qsortOrd

/-- Pretty-print an array of `Import`s as a block of import statements (not including `module` and/
or `prelude`). The grouping and sorting behavior may be controlled by the `formatAs` argument.

By default, this function groups imports by visibility (`public`, private (no token), or `all`)
with an extra newline in between groups, and within groups, sorts first by the `meta` token then
alphabetically. See `Import.FormatBehavior` for more details. -/
@[inline] def pretty (imports : Array Import)
    (formatAs := Import.FormatBehavior.grouped) : Format :=
  match formatAs with
  | .grouped splitMeta =>
    let imps := (sortPretty imports).toList.splitBy fun i₁ i₂ =>
      i₁.isExported == i₂.isExported && i₁.importAll == i₂.importAll &&
        (!splitMeta || i₁.isMeta == i₂.isMeta)
    f!"\n\n".joinSep <| imps.map (f!"\n".joinSep ·)
  | .sorted =>
    f!"\n".joinSep (sortPretty imports).toList
  | .none =>
    f!"\n".joinSep imports.toList

/-- Pretty-print an array of `Import`s as a full header, including te `module` and `prelude` tokens
as given by `isModule` (default: `true`) and `isPrelude` (default: `false`). -/
def prettyHeader (imports : Array Import) (isModule : Bool := true) (isPrelude : Bool := false)
    (formatAs := Import.FormatBehavior.grouped) : Format := Id.run do
  let mut fmts := #[]
  if isModule  then fmts := fmts.push f!"module\n" -- Extra newline after `module` in all cases
  if isPrelude then fmts := fmts.push f!"prelude"
  fmts := fmts.push (pretty imports formatAs)
  return f!"\n".joinSep fmts.toList

/-- Descriptions of cases in which the import formatting procedure does not know how to proceed. -/
structure FormatErrors where
  /-- If we have multiple versions of the same import, and each have their own nontrivial trailing
  whitespace, we don't necessarily know how to combine them. In this case, each `ref.toImport`
  matches the `imp` exactly. -/
  multipleTrailing : Array (Import × Array (ImportRef × String)) := #[]
  /-- If a module was originally imported in one manner with nontrivial trailing whitespace, but we
  now import it in a different manner (e.g. if it was imported multiple times with different
  modifiers, and we've normalized the imports), then the user should review the whitespace (which
  may be a shake annotation) to make sure it still makes sense. We also tell the user to review the
  leading whitespace, just in case. -/
  reviewWhitespace : Array (Name × Array Import × Array (ImportRef × Whitespace)) := #[]
  /-- If we no longer use some imports that had nontrivial whitespace, record them here. -/
  unusedWithComments : Array (ImportRef × Whitespace) := #[]
deriving BEq, Repr, Inhabited

def FormatErrors.isEmpty (errs : FormatErrors) :=
  errs.multipleTrailing.isEmpty && errs.reviewWhitespace.isEmpty && errs.unusedWithComments.isEmpty

/-- Assumes whitespace has been created with `headerToImportRefsWithWhitespace`. -/
def collectWithWhitespaceFromSource (newImps : Array Import)
    (sourceImps : Array (ImportRef × Whitespace)) :
    Array (Import × Whitespace) × Import.FormatErrors := Id.run do
  let newModNames := NameSet.ofArray <| newImps.map (·.module)

  let mut sourcesByName : NameMap (Array (ImportRef × Whitespace)) := {}
  let mut unusedWithComments : Array (ImportRef × Whitespace) := #[]
  -- let mut impsWithRefs : Array (Import × Array (ImportRef × Whitespace)) := #[]
  for sourceImpWithWs@(imp, ws) in sourceImps do
    -- TODO: just linear scan fine instead of `NameSet`?
    if newModNames.contains imp.module then
      sourcesByName := sourcesByName.alter imp.module fun rs =>
        rs.getD #[] |>.push sourceImpWithWs
    else unless ws.isEmpty do
      unusedWithComments := unusedWithComments.push sourceImpWithWs

  let mut usedSources : Std.TreeSet (ImportRef × Whitespace) fun a b =>
    compare a.1 b.1 |>.then <| compare a.2 b.2 := {}
  let mut impsWithWs : Array (Import × Whitespace) := #[] -- The final array
  let mut multipleTrailing : Array (Import × (Array (ImportRef × String))) := #[]
  for newImp in newImps do
    -- Aggregate leading if there are multiple comments
    -- TODO: consider turning this into an error
    let mut totalLeading := ""
    -- Expect one trailing, but collect multiple for error reporting
    let mut trailings := #[]
    let some existing := sourcesByName.get? newImp.module
      | impsWithWs := impsWithWs.push (newImp, .empty); continue
    for source@(ref, { leading, trailing }) in existing do
      if newImp == ref.toImport then
        usedSources := usedSources.insert source
        unless leading.isEmpty do
          totalLeading := totalLeading ++ leading
        unless trailing.isEmpty do
          trailings := trailings.push (ref, trailing)
    if trailings.size > 1 then
      multipleTrailing := multipleTrailing.push (newImp, trailings)
    let trailing := if let #[(_, trailing)] := trailings then trailing else ""
    impsWithWs := impsWithWs.push (newImp, { leading := totalLeading, trailing })

  let mut reviewWhitespace := #[]
  -- All `sources` only mention modules that are imported in one way or another.
  for (module, sources) in sourcesByName do
    let differentlyImported := sources.filter fun source@(_, ws) =>
      !ws.isEmpty && !usedSources.contains source
    unless differentlyImported.isEmpty do
      let actualImps := impsWithWs.filterMap fun (imp, _) =>
        if imp.module == module then some imp else none
      /- TODO: if `actualImps.size = 1`, should we accumulate all the leading whitespace and attach
      it? Current position: no, since it may describe the reason for the modifiers in natural
      language in a manner that may be outdated now. -/
      reviewWhitespace := reviewWhitespace.push (module, actualImps, differentlyImported)

  return (impsWithWs, { unusedWithComments, multipleTrailing, reviewWhitespace : FormatErrors })

instance : ToFormat FormatErrors where
  format | { multipleTrailing, reviewWhitespace, unusedWithComments } => Id.run do
    let mut msg := #[]
    for (imp, refsWithTrailing) in multipleTrailing do
      let annotations := f!"\n".joinSep (refsWithTrailing.map fun (ref, trailing) =>
        f!"{ref.toImport}{trailing}").toList
      msg := msg.push
        f!"Multiple annotations were given for `{imp}`. Decide which should still be applied:\n\
        ```\n\
        {annotations}\n\
        ```"
    for (module, imps, sources) in reviewWhitespace do
      let mut sourceMsg := #[]
      for (ref, ws) in sources do
        sourceMsg := sourceMsg.push (ws.around ref.toImport)
      -- Like `MessageData.andList`, but for `Format`
      let andList (xs) := match xs with
        | [] => f!"– none –"
        | [x] => x
        | [x₀, x₁] => f!"{x₀} and {x₁}"
        | xs@(_ :: _ :: _) => f!"{f!", ".joinSep xs.dropLast}, and {xs.getLast (by grind)}"
      msg := msg.push f!"Comments were present when importing `{module}`, but this module is \
        now imported differently as {andList (imps.map (f!"`{·}`") |>.toList)}.\n\
        Decide if the following original comments still apply:\n\
        ```\n\
        {f!"\n\n".joinSep sourceMsg.toList}\n\
        ```"
    unless unusedWithComments.isEmpty do
      let mut sourceMsg := #[]
      for (ref, ws) in unusedWithComments do
        sourceMsg := sourceMsg.push (ws.around ref.toImport)
      msg := msg.push f!"The following imports did not appear in the new import list, but had \
        comments around them:\n\
        ```\n\
        {f!"\n\n".joinSep sourceMsg.toList}\n\
        ```\n"
    return f!"\n\n".joinSep msg.toList

/-- Wraps the `Whitespace` associated with each import around it, and formats the array of imports
as an import block according to `formatAs`. -/
def prettyWithWhitespace (imps : Array (Import × Whitespace))
    (formatAs := Import.FormatBehavior.grouped) : Format := Id.run do
  let imps := if formatAs matches .none then imps else imps.qsortOrd
  if let .grouped splitMeta := formatAs then
    let groups := imps.toList.splitBy fun (i₁,_) (i₂,_) =>
      i₁.isExported == i₂.isExported && i₁.importAll == i₂.importAll &&
        (!splitMeta || i₁.isMeta == i₂.isMeta)
    f!"\n\n".joinSep (groups.map (f!"\n".joinSep ·))
  else
    f!"\n".joinSep imps.toList

/-- Formats the modified `imps` and attaches whitespace from the corresponding import in
`sourceImps` when doing so is unambiguous. Ambiguity encountered while assigning nontrivial
whitespace is recorded in the returned `Array Import.FormatError`.

If `includeErrorsAsComment := true` (the default), the errors are included as a source comment
following the formatted import block.

We assume `sourceImps` has been created by `ImportGraph.headerToImportRefsWithWhitespace`. -/
@[inline] def prettyWithSourceWhitespace (imps : Array Import)
    (sourceImps : Array (ImportRef × Whitespace)) (fmtBehavior := Import.FormatBehavior.grouped)
    (includeErrorsAsComment := true) :
    Format × Import.FormatErrors :=
  let (impsWithWs, errs) := collectWithWhitespaceFromSource imps sourceImps
  let impsFmt := prettyWithWhitespace impsWithWs fmtBehavior
  if errs.isEmpty || !includeErrorsAsComment then (impsFmt, errs) else
    let msg := f!"\
      {impsFmt}\n\
      \n\
      /-\n\
      {errs}\n\
      -/"
    (msg, errs)

/-- Create a message that suggests replacing `sourceImps` with `newImps`. Includes errors as a
comment. Returns `none` if the suggestion is would not modify the source at all (including
whitespace). -/
def mkImportSuggestionMessage (ref : Syntax) (newImps : Array Import)
    (sourceImps : Array (ImportRef × Whitespace)) (formatAs := Import.FormatBehavior.grouped)
    (toCodeActionTitle? : Option (String → String) := some fun _ => "Modify imports")
    (includeErrorsAsComment := true) :
    CoreM (Option (MessageData × Import.FormatErrors)) := do
  let (msg, errs) :=
    Import.prettyWithSourceWhitespace newImps sourceImps formatAs includeErrorsAsComment
  let stxRef := mkNullNode (sourceImps.map (·.1.stx.raw))
  let sourceSubstr : Substring.Raw := {
    str := (← getFileMap).source
    startPos := stxRef.getLeadingPos?.getD (stxRef.getPos?.get!)
    -- Ensure we include any annotation after the last import
    stopPos := stxRef.getTrailingTailPos?.get! }
  let (sourceSubstr, str) :=
    -- We want two newlines in front of the suggestion to separate it from `module`.
    -- Either chop these off the source if we can, or add them to our new string.
    -- Chopping off allows us to avoid unsightly whitespace at the top of the suggestion.
    let str := msg.pretty (width := Std.Format.getWidth <|← getOptions)
    if let some sourceSubstr := sourceSubstr.dropPrefix? "\n\n".toRawSubstring then
      (sourceSubstr, str)
    else
      (sourceSubstr, s!"\n\n{str}")
  if sourceSubstr.toString == str then
    return none
  else
    -- TODO: trivial case of no imports suggested
    -- Need `.ofRange` here to insist on overwriting annotations on the last import
    let msg ← Meta.Hint.mkSuggestionsMessage #[{
        suggestion := str
        span? := Syntax.ofRange ⟨sourceSubstr.startPos, sourceSubstr.stopPos⟩
        -- The diff view often gets confused by imports that are shown in the error comment.
        diffGranularity := if errs.isEmpty || !includeErrorsAsComment then .word else .none
        toCodeActionTitle? }]
      ref none (forceList := false)
    return (msg, errs)
