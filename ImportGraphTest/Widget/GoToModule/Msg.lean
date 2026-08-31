module

public meta import ImportGraphTest.Widget.GoToModule.Decls
public meta import Lean.Elab.Command
public meta import ImportGraph.Widget.GoToModule

open ImportGraph Widget Lean Elab Command

elab "#go_to" : command => liftCoreM do
  let msgs := [
    ← goToModule `ImportGraphTest.Widget.GoToModule.Decls,
    -- Note that Lsp positions are 0-indexed, so this should show up as line 4
    ← goToModule `ImportGraphTest.Widget.GoToModule.Decls (pos := ⟨3,7⟩)
      (overrideText := "Some module"),
    m!"end of `bar`:",
    ← goToModuleOfDecl ``bar,
    m!"end of `bar` (last declaration):",
    ← goToModuleOfDecls #[``bar, ``foo] (.end true),
    m!"line after `bar`:",
    ← goToModuleOfDecls #[``bar, ``foo] (.end false),
    m!"line before `foo`:",
    ← goToModuleOfDecls #[``bar, ``foo] (.start true),
    m!"start of `foo`:",
    ← goToModuleOfDecls #[``bar, ``foo] (.start false),
  ]
  logInfo <| m!"\n".joinSep msgs

-- Note that these positions are only visible in fallback `MessageData`.
/--
info: ImportGraphTest.Widget.GoToModule.Decls (1:0)
Some module (4:7)
end of `bar`:
ImportGraphTest.Widget.GoToModule.Decls (12:0)
end of `bar` (last declaration):
ImportGraphTest.Widget.GoToModule.Decls (12:0)
line after `bar`:
ImportGraphTest.Widget.GoToModule.Decls (11:16)
line before `foo`:
ImportGraphTest.Widget.GoToModule.Decls (6:0)
start of `foo`:
ImportGraphTest.Widget.GoToModule.Decls (7:0)
-/
#guard_msgs in
#go_to
