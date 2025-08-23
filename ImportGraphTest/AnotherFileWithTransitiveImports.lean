import ImportGraph.Imports
import ImportGraphTest.Unused

/--
info: Found 2 additional imports:
ImportGraphTest.Used
ImportGraphTest.FileWithTransitiveImports
-/
#guard_msgs in
#import_diff ImportGraphTest.FileWithTransitiveImports

/--
info: Found 23 additional imports:
ImportGraphTest.Used
ImportGraphTest.FileWithTransitiveImports
Cli.Basic
Batteries.Lean.IO.Process
Lake.Util.Error
Lake.Util.EStateT
Lake.Util.Lift
Lake.Util.Log
Lake.Util.Compare
Lake.Util.DRBMap
Lake.Util.RBArray
Lake.Util.Name
Lake.Util.FilePath
Lake.Util.JsonObject
Lake.Util.Date
Lake.Util.Version
Lake.Config.Defaults
Lake.Load.Manifest
ImportGraph.CurrentModule
ImportGraph.Lean.Name
ImportGraph.Gexf
ImportGraph.Cli
ImportGraphTest.Dot
-/
#guard_msgs in
#import_diff ImportGraphTest.FileWithTransitiveImports ImportGraphTest.Dot


/-- error: File SomeBogusFilename cannot be found. -/
#guard_msgs in
#import_diff SomeBogusFilename
