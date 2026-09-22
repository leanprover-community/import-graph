module

import ImportGraph.WorkspaceModel.Build
import Lean.Elab.Command

open ImportGraph Lean Shake

/--
info: Has `ImportGraphTest.Shake.Workspace`: true

Has `ImportGraph.WorkspaceModel.Build`: true

Has `ImportGraph.WorkspaceModel.Summary`: true

`ImportGraph.WorkspaceModel.Summary` > `ImportGraph.WorkspaceModel.Build`: [⠃]

`ImportGraph.WorkspaceModel.Build` > `ImportGraph.WorkspaceModel.Summary`: [⠀]

[A|B|C|aB|bc]
ImportGraphTest.Shake.Workspace.A: [⠇|⠀|⠀|⠀|⠀]  ⏎
ImportGraphTest.Shake.Workspace.B: [⠀|⠇|⠀|⠀|⠀]  ⏎
ImportGraphTest.Shake.Workspace.C: [⠀|⠀|⠇|⠀|⠀]  ⏎
ImportGraphTest.Shake.Workspace.aB: [⠂|⠃|⠀|⠇|⠀]  ⏎
ImportGraphTest.Shake.Workspace.bc: [⠀|⠂|⠂|⠂|⠇]
-/
#guard_msgs in
run_cmd
  let testDir := `ImportGraphTest.Shake.Workspace
  let aMod  := testDir ++ `A
  let bMod  := testDir ++ `B
  let cMod  := testDir ++ `C
  let abMod := testDir ++ `aB
  let bcMod := testDir ++ `bc
  let testMods := #[aMod, bMod, cMod, abMod, bcMod]
  let extraMods := #[← getMainModule, bcMod]
  let _ ← getWorkspaceModel (extraMods := extraMods)
  -- ensure no errors on cache and non-cache paths post-cache creation
  let _ ← getWorkspaceModel (extraMods := extraMods)
  let _ ← getWorkspaceModel (extraMods := extraMods)
    (readInteractiveCache := false)
  let _ ← getWorkspaceModel (extraMods := extraMods)
    (readPersistentCache := false)
  let w ← getWorkspaceModel (extraMods := extraMods)
    (readInteractiveCache := false) (readPersistentCache := false)

  let mut msgs := #[]
  let mainModule ← getMainModule
  let buildMod := `ImportGraph.WorkspaceModel.Build
  let summaryMod := `ImportGraph.WorkspaceModel.Summary
  msgs := msgs.push m!"Has `{mainModule}`: \
    {w.mods.any (·.name == mainModule)}"
  msgs := msgs.push m!"Has `{buildMod}`: \
    {w.mods.any (·.name == buildMod)}"
  msgs := msgs.push m!"Has `{summaryMod}`: \
    {w.mods.any (·.name == summaryMod)}"
  let some buildModIdx := w.getModIdx? buildMod
    | throwError "Could not find index for `{buildMod}`."
  let some summaryModIdx := w.getModIdx? summaryMod
    | throwError "Could not find index for `{summaryMod}`."

  msgs := msgs.push m!"`{summaryMod}` > `{buildMod}`: \
    {w.getMod! buildModIdx |>.transDeps.toStringAt summaryModIdx}"
  msgs := msgs.push m!"`{buildMod}` > `{summaryMod}`: \
    {w.getMod! summaryModIdx |>.transDeps.toStringAt buildModIdx}"

  let testModIdxs ← testMods.mapM fun mod => do
    let some modIdx := w.getModIdx? mod | throwError "Could not find index for {mod}"
    pure (mod, modIdx)
  let testDepStrings := testModIdxs.map fun modModIdx =>
    let chars := testModIdxs.map fun depModIdx =>
      w.getMod! modModIdx.2 |>.transDeps.brailleCellAt depModIdx.2 |>.toString
    s!"{modModIdx.1}: [{"|".intercalate chars.toList}]"
  let header := s!"[{"|".intercalate <| testMods.map (·.getString!) |>.toList}]"
  msgs := msgs.push m!"{header}\n{"  \n".intercalate <| testDepStrings.toList}"

  logInfo <| m!"\n\n".joinSep msgs.toList
