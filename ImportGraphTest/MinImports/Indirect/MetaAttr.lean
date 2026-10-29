module

public import ImportGraphTest.MinImports.Indirect.Defs
public meta import ImportGraphTest.MinImports.Indirect.Attr

/-!
Import `foo` (publicly) as non-meta, but import its indirect use (publicly) as `meta`.
-/

public def bar : Nat := 0
