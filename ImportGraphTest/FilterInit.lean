import ImportGraph.Imports.FromSource

/-!
# Tests for `Lean.ModuleHeader.filterInit`

`filterInit` intentionally removes the entire `Init` namespace (`Init` itself and `Init.*`),
unlike the deprecated `findImportsFromSource`, which only removed `Init` itself
(see issue #149). These tests pin down both behaviors.
-/

open Lean System ImportGraph

-- `filterInit` drops an explicit `Init.*` import from a `prelude` file.
/-- info: #[`Foo] -/
#guard_msgs in
#eval do
  match parseImports? "prelude\nimport Init.Data.Nat.Basic\nimport Foo\n" with
  | .ok header => return header.filterInit.imports.map Import.module
  | .error (_, _, msg) => throw (IO.userError msg)

-- Filtering on `·.module != `Init` instead (the deprecated `findImportsFromSource` behavior)
-- keeps explicit `Init.*` imports.
/-- info: #[`Init.Data.Nat.Basic, `Foo] -/
#guard_msgs in
#eval do
  match parseImports? "prelude\nimport Init.Data.Nat.Basic\nimport Foo\n" with
  | .ok header => return header.imports.filter (fun imp => Import.module imp != `Init) |>.map Import.module
  | .error (_, _, msg) => throw (IO.userError msg)

-- `filterInit` drops `Init` itself (including the duplicated implicit `Init` imports that
-- module-system files can produce) as well as `Init.*` imports.
/-- info: #[`Foo.Bar] -/
#guard_msgs in
#eval do
  match parseImports? "import Init\nimport Init.Data.Nat.Basic\nimport Foo.Bar\n" with
  | .ok header => return header.filterInit.imports.map Import.module
  | .error (_, _, msg) => throw (IO.userError msg)

-- A plain header gets an implicit `Init` import, which `filterInit` removes.
/-- info: #[`Foo] -/
#guard_msgs in
#eval do
  match parseImports? "import Foo\n" with
  | .ok header => return header.filterInit.imports.map Import.module
  | .error (_, _, msg) => throw (IO.userError msg)
