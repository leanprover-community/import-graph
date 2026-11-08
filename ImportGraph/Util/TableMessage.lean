/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module


import Lean.Elab.Command

/-!
# Formatting tables

This module provides basic utilities for formatting tables with aligned columns.
For now, we only supports tables of strings.

## Future work

- Specifying dividers and borders, padding characters, right/left alignment, column widths
  - Ideally this is granular, e.g. per cell/row/column
- Composing and stacking tables
- Widget version using HTML tables to avoid awkward reflowing + allow `MessageData`
-/

namespace ImportGraph

open ImportGraph

public section

/-- Pad a string on the right with `c : Char` (by default `' '`), until it reaches length `n`. Does
not trim strings already over length `n` down to length `n`. -/
@[inline] def String.rightPad (n : Nat) (s : String) (char : Char := ' ') :=
  s.pushn char (n - s.positions.length)

/-- Pad a string on the left with `c : Char` (by default `' '`), until it reaches length `n`. Does
not trim strings already over length `n` down to length `n`. -/
@[inline] def String.leftPad (n : Nat) (s : String) (char : Char := ' ') :=
  "".pushn char (n - s.positions.length) ++ s

namespace Table

/-- A row with a fixed number of elements in a table such as `Array (Table.Row 5)`. (The `String`s
in a `Row` are allowed to have any length.) -/
protected abbrev Row (n : Nat) := Vector String n

/-- Given an array of table rows, arranges them in a table. Each cell is left-aligned, and
separated from the next cell in the row by a space. Spaces at the end of each row are trimmed. -/
protected def formatSimple (rows : Array (Table.Row n)) : String := Id.run do
  if h : rows.size > 0 then
    let mut colWidths := rows[0].map (·.positions.length)
    for h : i in 1...rows.size do
      colWidths := colWidths.zipWith (max · ·.positions.length) rows[i]
    let mut table := ""
    for hRow : rowIdx in 0...rows.size do
      for hCol : i in 0...n do
        if h : i + 1 < n then
          table := table ++ rows[rowIdx][i].rightPad colWidths[i] |>.push ' '
        else
          -- The last column needs neither padding nor a `' '` separator
          -- However, it may still have spaces (e.g. empty cell) so trim them; do not drop `'\n'`.
          table := table ++ rows[rowIdx][i] |>.dropEndWhile (· = ' ') |>.copy
      unless rowIdx + 1 == rows.size do
        table := table.push '\n'
    return table
  else return ""

/-- An empty row in a table. -/
def emptyRow {n} : Table.Row n := Vector.replicate n ""

/-- Increase the size of a row by padding it on the right with empty cells. -/
def Row.rightPad (row : Table.Row n) (h : n ≤ m := by grind) : Table.Row m :=
  (show max m n = m by grind) ▸ row.rightpad m ""
