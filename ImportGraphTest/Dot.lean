import ImportGraph.Cli

def readFile (path : System.FilePath) : IO String :=
  IO.FS.readFile path

def runGraphCommand : IO Unit := do
  let _ ← IO.Process.output {
    cmd := "lake"
    args := #["exe", "graph", "--to", "ImportGraphTest.Used", "ImportGraphTest/produced.dot"]
  }

def compareOutputs (expected : String) (actual : String) : IO Bool := do
  let expectedLines := expected.splitOn "\n" |>.filter (·.trim.length > 0) |>.map String.trim
  let actualLines := actual.splitOn "\n" |>.filter (·.trim.length > 0) |>.map String.trim
  pure (expectedLines == actualLines)

/-- info: Test passed: The graph command output matches the expected.dot file. -/
#guard_msgs in
#eval show IO Unit from do
  runGraphCommand
  let expectedOutput ← readFile "ImportGraphTest/expected.dot"
  let actualOutput ← readFile "ImportGraphTest/produced.dot"
  let isEqual ← compareOutputs expectedOutput actualOutput
  if isEqual then
    IO.println "Test passed: The graph command output matches the expected.dot file."
  else
    IO.println "Test failed: The graph command output does not match the expected.dot file."
    IO.println s!"Expected:\n{expectedOutput}"
    IO.println s!"Actual:\n{actualOutput}"
