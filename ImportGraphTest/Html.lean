import ImportGraph

-- The `lake exe graph ...` command below expects ToTarget.lean to have already
-- been built, see the comment in `ImportGraphTest/Dot.lean`.
import ImportGraphTest.ToTarget

def runGraphHtml (extraArgs : Array String) : IO String := do
  let out ← IO.Process.output {
    cmd := "lake"
    args := #["exe", "graph", "--to", "ImportGraphTest.ToTarget"] ++ extraArgs ++
      #["ImportGraphTest/produced.html"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"`lake exe graph` failed:\n{out.stderr}"
  IO.FS.readFile "ImportGraphTest/produced.html"

/-- info: true -/
#guard_msgs in
#eval show IO Unit from do
  let html ← runGraphHtml #[]
  IO.println <| html.contains
    "\"https://leanprover-community.github.io/mathlib4_docs/\""

/-- info: true -/
#guard_msgs in
#eval show IO Unit from do
  let html ← runGraphHtml #["--doc-url", "https://example.org/docs"]
  IO.println <| html.contains "\"https://example.org/docs/\""
