module

public meta import ImportGraph.Shake.Workspace
public import Lean.Elab.Command

import ImportGraphTest.Shake.Algebra

open ImportGraph Shake Lean Elab Command

elab "#show_decl_needs" colGe cmd:command : command => do
  let (declNeeds, newDecls) ← elabCommandCapturingNeeds cmd
  let w ← getWorkspaceModel (extraMods := #[← getMainModule])
  let (importNeeds, stances) ← liftCoreM <| declNeeds.toSimultaneousImportNeeds w |>.run
  let stanceMsg : MessageData := .bracket (l := "{") (r := "}") <|
    (m!"," ++ Format.line).joinSep <|
      stances.toList.mergeSort (·.1.cmp ·.1 |>.isLE) |>.map fun (n, s) =>
        m!"{.ofConstName n} ↦ {match s with | some s => s.toString | _ => "none"}"
  logInfo m!"\
    New decls:{indentD (newDecls.toList.map MessageData.ofConstName)}\
    \nNeeds:{indentD declNeeds.toMessageData}\
    \nStances:{indentD stanceMsg}\
    \nImportNeeds:{indentD <| importNeeds.toString}"

/--
info: New decls:
  [x]
Needs:
  {x ↦ {fixedDecls := {Init.Prelude ↦ {true ↦ [used in value, used in runtime IR], Bool ↦ [used in type]}}}}
Stances:
  {Bool ↦ @[no_expose] public, x ↦ private runtime, true ↦ @[no_expose] public}
ImportNeeds:
  │⠒⠀│
-/
#guard_msgs in
#show_decl_needs def x : Bool := true

/--
info: New decls:
  [x']
Needs:
  {x' ↦ {fixedDecls := {Init.Prelude ↦ {true ↦ [used in value, used in runtime IR], Bool ↦ [used in type]}}}}
Stances:
  {Bool ↦ @[no_expose] public, x' ↦ public runtime, true ↦ @[no_expose] public}
ImportNeeds:
  │⠚⠀│
-/
#guard_msgs in
#show_decl_needs public def x' : Bool := true

/--
info: New decls:
  [x'']
Needs:
  {x'' ↦ {fixedDecls := {Init.Prelude ↦ {true ↦ [used in value, used in runtime IR], Bool ↦ [used in type]}}}}
Stances:
  {Bool ↦ @[no_expose] public, x'' ↦ @[expose] public runtime, true ↦ @[no_expose] public}
ImportNeeds:
  │⠊⠀│
-/
#guard_msgs in
#show_decl_needs @[expose] public def x'' : Bool := true

/--
info: New decls:
  [x''']
Needs:
  {x''' ↦ {fixedDecls := {Init.Prelude ↦ {true ↦ [used in value], Bool ↦ [used in type]}}}}
Stances:
  {Bool ↦ @[no_expose] public, x''' ↦ @[expose] public, true ↦ @[no_expose] public}
ImportNeeds:
  │⠈⠀│
-/
#guard_msgs in
#show_decl_needs @[expose] public noncomputable def x''' : Bool := true
