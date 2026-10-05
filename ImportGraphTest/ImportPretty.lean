module

public meta import ImportGraph.Imports.FromSource



public meta import Lean.Elab.Command -- some comment

public meta import ImportGraph.Imports.Pretty

open ImportGraph Lean Elab Command

elab tk:"#pretty_current_imports " p:("without_public")? : command => do
  let (header, _) ← parseCurrentHeader
  let refs := headerToImportRefsWithWhitespace header
  let some (msg, _) ← liftCoreM <| Import.mkImportSuggestionMessage tk
    -- suggest `meta import Lean.Elab.Command` and `public import ImportGraph.Imports.Pretty`
    #[{ module := `Lean.Elab.Command, isMeta := true, isExported := p.isNone },
      { module := `ImportGraph.Imports.Pretty, isExported := true}]
    refs
    | throwError "Could not create message"
  logInfoAt tk msg

/--
info:
  public meta import I̵m̵p̵o̵r̵t̵G̵r̵a̵p̵h̵.̵I̵m̵p̵o̵r̵t̵s̵.̵F̵r̵o̵m̵S̵o̵u̵r̵c̵e̵
  ̵
  ̵
  ̵
  ̵p̵u̵b̵l̵i̵c̵ ̵m̵e̵t̵a̵ ̵i̵m̵p̵o̵r̵t̵ ̵Lean.Elab.Command -- some comment
  public m̵e̵t̵a̵ ̵import ImportGraph.Imports.Pretty
-/
#guard_msgs in
#pretty_current_imports

/--
info:
  [apply] public import ImportGraph.Imports.Pretty
  ⏎
  meta import Lean.Elab.Command
  ⏎
  /-
  Comments were present when importing `Lean.Elab.Command`, but this module is now imported differently as `meta import Lean.Elab.Command`.
  Decide if the following original comments still apply:
  ```
  public meta import Lean.Elab.Command -- some comment
  ```
  -/
-/
#guard_msgs in
#pretty_current_imports without_public

run_cmd do
  let source := "module\n\nimport B -- trailing\npublic meta import C\nimport all D\n" ++
    "-- leading\npublic import A\n\n#check Nat\n"
  let expected := "module\n\npublic meta import C\n-- leading\npublic import A\n\n" ++
    "import B -- trailing\nimport all D\n\n#check Nat\n"
  let inputCtx := Parser.mkInputContext source "Test.lean"
  let (header, _, log) ← liftIO <| Parser.parseHeader inputCtx
  if log.hasErrors then
    throwError "Test header failed to parse"
  let sourceImports := headerToImportRefsWithWhitespace header
  let imports := sourceImports.map fun (ref, whitespace) => (ref.toImport, whitespace)
  let formatted := Import.prettyWithWhitespaceGroupedByVisibility imports
  let some edit := Import.mkImportBlockEdit source sourceImports formatted
    | throwError "Expected import formatting to produce an edit"
  unless edit.apply source == expected do
    throwError "Unexpected formatted source:\n{edit.apply source}"

  let inputCtx := Parser.mkInputContext expected "Test.lean"
  let (header, _, log) ← liftIO <| Parser.parseHeader inputCtx
  if log.hasErrors then
    throwError "Formatted test header failed to parse"
  let sourceImports := headerToImportRefsWithWhitespace header
  let imports := sourceImports.map fun (ref, whitespace) => (ref.toImport, whitespace)
  let formatted := Import.prettyWithWhitespaceGroupedByVisibility imports
  unless (Import.mkImportBlockEdit expected sourceImports formatted).isNone do
    throwError "Correctly grouped imports should not produce an edit"
