module

import ImportGraph.Tools.FindHome
import ImportGraphTest.FindHome.FakeHome
import ImportGraphTest.FindHome.SecondRealHome
import Lean.Data.Json
public meta import Lean.Elab.Command
public meta import ImportGraph.WorkspaceModel.Summary

/-
`bar₁` is from `ImportGraphTest.FindHome.ComponentHome1`
`bar₂` is from `ImportGraphTest.FindHome.ComponentHome2`
`foo` is from `ImportGraphTest.FindHome.RealHome`

`A.RealHome` imports both, as does `ImportGraphTest.FindHome.SecondRealHome`
-/

/--
info: This command can be moved to the following modules above this module:
• ImportGraphTest.FindHome.RealHome (end)
• ImportGraphTest.FindHome.SecondRealHome (end)

[click-to-copy] [copy source]
(Will copy:
  ⏎
  -- NOTE: necessary scopes and namespaces may not have been copied over.
  def x₁₂ := bar₁ && bar₂
  )

▼ More information
  ▼ Imports needed
    [click-to-copy] [copy imports]
    (Will copy:
      import ImportGraphTest.FindHome.ComponentHome1
      import ImportGraphTest.FindHome.ComponentHome2)
    import ImportGraphTest.FindHome.ComponentHome1
    import ImportGraphTest.FindHome.ComponentHome2
  ▼ New constants from this command
    • x₁₂
-/
#guard_msgs in
#find_home for
def x₁₂ := bar₁ && bar₂

-- Importing `foo` eliminated `SecondRealHome`
/--
info: This command can be moved to the following module above this module:
• ImportGraphTest.FindHome.RealHome (7:0)

[click-to-copy] [copy source]
(Will copy:
  ⏎
  -- NOTE: necessary scopes and namespaces may not have been copied over.
  def x₁₂foo := bar₁ && bar₂ && foo
  )

▼ More information
  ▼ Imports needed
    [click-to-copy] [copy imports]
    (Will copy:
      import ImportGraphTest.FindHome.RealHome)
    import ImportGraphTest.FindHome.RealHome
  ▼ New constants from this command
    • x₁₂foo
-/
#guard_msgs in
#find_home for
def x₁₂foo := bar₁ && bar₂ && foo
