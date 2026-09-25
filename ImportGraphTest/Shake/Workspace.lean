module

meta import ImportGraph.WorkspaceModel.Build
import Lean.Elab.Command

/-!
The following tests assume that `ImportGraph.WorkspaceModel.Build` has
`public import ImportGraph.WorkspaceModel.Summary`, and (only) transitively,
`public import ImportGraph.WorkspaceModel.Base`.

It also tests a small artificial hierarchy in the adjacent folder `ImportGraphTest.Shake.Workspace`

- modules `A`, `B`,`C` with no imports
- module `aB`: `import A; public import B`
- module `bc`: `import aB; import C` (receives `B` and `C` privately)

Recall that a public (transitive) import looks like `⠃` and a private import looks like `⠂`.
-/

open ImportGraph Lean Lake Shake

/--
info: Has `ImportGraphTest.Shake.Workspace`: true

`ImportGraph.WorkspaceModel.Summary` > `ImportGraph.WorkspaceModel.Build`: [⠃]

`ImportGraph.WorkspaceModel.Build` > `ImportGraph.WorkspaceModel.Summary`: [⠀]

`ImportGraph.WorkspaceModel.Base` > `ImportGraph.WorkspaceModel.Build`: [⠃]

[A|B|C|aB|bc]
ImportGraphTest.Shake.Workspace.A: [⠇|⠀|⠀|⠀|⠀]
ImportGraphTest.Shake.Workspace.B: [⠀|⠇|⠀|⠀|⠀]
ImportGraphTest.Shake.Workspace.C: [⠀|⠀|⠇|⠀|⠀]
ImportGraphTest.Shake.Workspace.aB: [⠂|⠃|⠀|⠇|⠀]
ImportGraphTest.Shake.Workspace.bc: [⠀|⠂|⠂|⠂|⠇]

Package of `ImportGraph.WorkspaceModel.Build`: importGraph

Library of `ImportGraph.WorkspaceModel.Build`: ImportGraph

Package of `ImportGraphTest.Shake.Workspace.A`: importGraph

Library of `ImportGraphTest.Shake.Workspace.A`: ImportGraphTest
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
  let extraMods := #[bcMod]
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
  let baseMod := `ImportGraph.WorkspaceModel.Base
  msgs := msgs.push m!"Has `{mainModule}`: \
    {w.mods.any (·.name == mainModule)}"
  let some buildModIdx := w.getModIdx? buildMod
    | throwError "Could not find index for `{buildMod}`."
  let some summaryModIdx := w.getModIdx? summaryMod
    | throwError "Could not find index for `{summaryMod}`."
  let some baseModIdx := w.getModIdx? baseMod
    | throwError "Could not find index for `{baseMod}`."

  msgs := msgs.push m!"`{summaryMod}` > `{buildMod}`: \
    {w.getMod! buildModIdx |>.transDeps.toStringAt summaryModIdx}"
  msgs := msgs.push m!"`{buildMod}` > `{summaryMod}`: \
    {w.getMod! summaryModIdx |>.transDeps.toStringAt buildModIdx}"
  msgs := msgs.push m!"`{baseMod}` > `{buildMod}`: \
    {w.getMod! buildModIdx |>.transDeps.toStringAt baseModIdx}"

  let testModIdxs ← testMods.mapM fun mod => do
    let some modIdx := w.getModIdx? mod | throwError "Could not find index for {mod}"
    pure (mod, modIdx)
  let testDepStrings := testModIdxs.map fun modModIdx =>
    let chars := testModIdxs.map fun depModIdx =>
      w.getMod! modModIdx.2 |>.transDeps.brailleCellAt depModIdx.2 |>.toString
    s!"{modModIdx.1}: [{"|".intercalate chars.toList}]"
  let header := s!"[{"|".intercalate <| testMods.map (·.getString!) |>.toList}]"
  msgs := msgs.push m!"{header}\n{"\n".intercalate <| testDepStrings.toList}"

  msgs := msgs.push m!"Package of `{buildMod}`: {w.pkgOfModIdx! buildModIdx |>.origName}"
  msgs := msgs.push m!"Library of `{buildMod}`: {w.libOfModIdx! buildModIdx |>.name}"
  let (testMod, testModIdx) := testModIdxs[0]!
  msgs := msgs.push m!"Package of `{testMod}`: {w.pkgOfModIdx! testModIdx |>.origName}"
  msgs := msgs.push m!"Library of `{testMod}`: {w.libOfModIdx! testModIdx |>.name}"

  logInfo <| m!"\n\n".joinSep msgs.toList

-- The following test checks that we've found the correct toolchain data.
/--
info: `Lake.lean` in `lakeSrcDir`: true
`Init.lean` in `leanSrcDir`: true
`Init.olean` in `leanLibDir`: true
-/
#guard_msgs in
run_cmd do
  let s ← getWorkspaceSummary
  logInfo m!"\
    `Lake.lean` in `lakeSrcDir`: {← s.lakeSrcDir / "Lake.lean" |>.pathExists}\n\
    `Init.lean` in `leanSrcDir`: {← s.leanSrcDir / "Init.lean" |>.pathExists}\n\
    `Init.olean` in `leanLibDir`: {← s.leanLibDir / "Init.olean" |>.pathExists}"
