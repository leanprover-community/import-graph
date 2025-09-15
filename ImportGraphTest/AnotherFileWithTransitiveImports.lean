import ImportGraph.Imports
import ImportGraphTest.Unused
import ImportGraphTest.FileWithTransitiveImports

/--
info: The following are already imported (possibly transitively):
ImportGraphTest.FileWithTransitiveImports
---
info: Found 2 additional imports:
ImportGraphTest.FileWithTransitiveImports
ImportGraphTest.Used
-/
#guard_msgs in
#import_diff ImportGraphTest.FileWithTransitiveImports

/--
info: The following are already imported (possibly transitively):
ImportGraphTest.FileWithTransitiveImports
ImportGraphTest.Used
---
info: Found 2 additional imports:
ImportGraphTest.FileWithTransitiveImports
ImportGraphTest.Used
-/
#guard_msgs in
#import_diff ImportGraphTest.FileWithTransitiveImports ImportGraphTest.Used


/-- error: File SomeBogusFilename cannot be found. -/
#guard_msgs in
#import_diff SomeBogusFilename
