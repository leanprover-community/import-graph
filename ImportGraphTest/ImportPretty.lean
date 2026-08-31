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
