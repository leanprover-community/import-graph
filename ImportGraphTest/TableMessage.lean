module

public meta import ImportGraph.Util.TableMessage

import Lean.Elab.Command

open ImportGraph

/--
info: Table:

          header1 h2   header3!      h₄
row₁:     item1   foo  bar           baz
row₂:     item₂   foo₂ bar₂          baz₂

row₃:     1       2    (long item 3) 4
shortrow: a₁      a₂
finalrow: 0       1    1             0
-/
#guard_msgs in
run_cmd do
  let mut rows : Array (Table.Row 5) := #[]
  rows := rows.push #v["", "header1", "h2", "header3!", "h₄"]
  let shortRow : Table.Row 3 := #v["shortrow:", "a₁", "a₂"]
  rows := rows ++ #[
    #v["row₁:", "item1", "foo", "bar", "baz"],
    #v["row₂:", "item₂", "foo₂", "bar₂", "baz₂"],
    Table.emptyRow,
    #v["row₃:", "1", "2", "(long item 3)", "4"],
    shortRow.rightPad,
    #v["finalrow:", "0", "1", "1", "0"]
  ]
  Lean.logInfo m!"Table:\n\n{Table.formatSimple rows}"
