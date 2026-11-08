module

public import ImportGraph.Util.Diff.Basic
public import ImportGraph.Util.TableMessage

public section

namespace ImportGraph.Diff

open ImportGraph.Diff

namespace Transition

/-- A header row for a table consisting of rows created by `Transition.toRow`. -/
def header : Table.Row 4 := #v["", "Δ", "new", "old"]

def toRow (rowHeader : String) (t : Transition α) [Diff.Measurable α] : Table.Row 4 :=
  #v[rowHeader, t.delta.toStringWithSign, s!"{Diff.measure t.final}", s!"{Diff.measure t.initial}"]

end Transition

namespace Venn

/-- A header row for a table consisting of rows created by `Venn.toRow`. -/
def header : Table.Row 5 := #v["", "Δ", "+", "-", "∩"]

/-- A row in a table for a single `VennMeasure` which follows the order in `VennMeasure.header`. -/
def toRow (rowHeader : String) (t : Venn α) [Diff.Measurable α]: Table.Row 5 := #v[
  rowHeader,
  t.delta.toStringWithSign,
  s!"+{measure t.plus}",
  s!"-{measure t.minus}",
  s!"{measure t.shared}"]

end Venn
