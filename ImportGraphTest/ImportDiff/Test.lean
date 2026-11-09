module

import ImportGraph.Tools.ImportDiff

/- In these tests, we use `substring := true` to avoid reporting `build depth`, which includes
potentially fragile contributions from the implicit `Init` imports. -/

/--
info: Imports strictly increased.

                 Δ  +   -   ∩
modules imported +1 +1  -0  0

public           0  +0  -0  0
private          +1 +1  -0  0
public meta      0  +0  -0  0
private meta     0  +0  -0  0

previous modules +1 +1  -0  0
ir loaded        0  +0  -0  0

                 Δ  new old
build depth      +1
-/
#guard_msgs (substring := true) in
#import_diff
  from
  to import ImportGraphTest.ImportDiff.Mods.B

/--
info: Imports strictly increased.

                 Δ  +   -   ∩
modules imported +3 +3  -0  0

public           0  +0  -0  0
private          +3 +3  -0  0
public meta      0  +0  -0  0
private meta     0  +0  -0  0

previous modules +3 +3  -0  0
ir loaded        0  +0  -0  0

                 Δ  new old
build depth      +2
-/
#guard_msgs (substring := true) in
#import_diff
  from
  to import ImportGraphTest.ImportDiff.Mods.AB

/--
info: Imports strictly increased.

                 Δ  +   -   ∩
modules imported +3 +3  -0  0

public           0  +0  -0  0
private          +2 +2  -0  0
public meta      0  +0  -0  0
private meta     +1 +1  -0  0

previous modules +3 +3  -0  0
ir loaded        +1 +1  -0  0

                 Δ  new old
build depth      +2
-/
#guard_msgs (substring := true) in
#import_diff
  from
  to import ImportGraphTest.ImportDiff.Mods.metaAB

/--
info:                  Δ  +   -   ∩
modules imported 0  +1  -1  2

public           0  +0  -0  0
private          -1 +1  -2  1
public meta      0  +0  -0  0
private meta     +1 +1  -0  0

previous modules 0  +1  -1  2
ir loaded        +1 +1  -0  0

                 Δ  new old
build depth      0
-/
#guard_msgs (substring := true) in
#import_diff
  from import ImportGraphTest.ImportDiff.Mods.AB
  to import ImportGraphTest.ImportDiff.Mods.metaAB

/--
info: No changes to imported modules, but the availability of some modules changed.

                 Δ  +   -   ∩
modules imported 0  +0  -0  3

public           0  +0  -0  0
private          -3 +0  -3  0
public meta      +3 +3  -0  0
private meta     +3 +3  -0  0

previous modules 0  +0  -0  3
ir loaded        +3 +3  -0  0

                 Δ  new old
build depth      0
-/
#guard_msgs (substring := true) in
#import_diff
  from import ImportGraphTest.ImportDiff.Mods.AB
  to public meta import ImportGraphTest.ImportDiff.Mods.AB

/--
info: No changes to imported modules, but the availability of some modules increased.

                 Δ  +   -   ∩
modules imported 0  +0  -0  3

public           0  +0  -0  0
private          0  +0  -0  3
public meta      0  +0  -0  0
private meta     +3 +3  -0  0

previous modules 0  +0  -0  3
ir loaded        +3 +3  -0  0

                 Δ  new old
build depth      0
-/
#guard_msgs (substring := true) in
#import_diff
  from import ImportGraphTest.ImportDiff.Mods.AB
  to import ImportGraphTest.ImportDiff.Mods.AB; meta import ImportGraphTest.ImportDiff.Mods.AB

/--
info:                  Δ +   -   ∩
modules imported 0 +1  -1  0

public           0 +0  -0  0
private          0 +1  -1  0
public meta      0 +0  -0  0
private meta     0 +0  -0  0

previous modules 0 +1  -1  0
ir loaded        0 +0  -0  0

                 Δ new old
build depth      0
-/
#guard_msgs (substring := true) in
#import_diff
  from import ImportGraphTest.ImportDiff.Mods.A
  to import ImportGraphTest.ImportDiff.Mods.B

/-! ## Messages and errors -/

/--
@ +1:27...28
info: The following imports in this file may be deleted:
  • -i̲m̲p̲o̲r̲t̲ ̲I̲m̲p̲o̲r̲t̲G̲r̲a̲p̲h̲.̲T̲o̲o̲l̲s̲.̲I̲m̲p̲o̲r̲t̲D̲i̲f̲f̲
-/
#guard_msgs (positions := true) in
#import_diff +import Lean; -

/--
▼ Legend
  Columns:
    • Δ: net change from initial to final
    • +: number of modules in the final state not in the initial state
    • -: number of modules in the initial state not in the final state
    • ∩: number of modules shared by the initial and final state. (This ignores the implicit `Init` imports that are secretly inserted by Lean.)
  ⏎
  Rows:
    • modules imported: the total number of modules that are actually imported (in a module-system aware fashion)
    • public/private/...: the number of modules imported at the given phase and visibility. Note that any module present at public visibility is necessarily also present at private visibility.
    • previous modules: the total number of modules prior to the current module in the build graph, ignoring module system annotations.
    • ir loaded: the IR loaded by Lean given these imports. (`meta` imports ignore module system annotations, and load the IR of all prior modules.)
    • build depth: the total depth of the build graph, ignoring module system annotations. Unlike other statistics, this does include `Init`.
▼ Initial imports (reduced) [click-to-copy] (Will copy:
  import ImportGraphTest.ImportDiff.Mods.A)
  import ImportGraphTest.ImportDiff.Mods.A
▼ Final imports (reduced) [click-to-copy] (Will copy:
  import ImportGraphTest.ImportDiff.Mods.A)
  import ImportGraphTest.ImportDiff.Mods.A
-/
#guard_msgs (substring := true) in
#import_diff
  from import ImportGraphTest.ImportDiff.Mods.A
  to import ImportGraphTest.ImportDiff.Mods.A

-- Helper message exists:
/--
Try adding and removing imports from the current imports with `+`/`-`, or comparing import blocks with `#import_diff (from ...)? to ...`.
-/
#guard_msgs (substring := true) in
#import_diff

-- Nice message about unfound module in the library:
/--
@ +1:23...60
error: Could not find module `ImportGraphTest.ImportDiff.Mods.ABCDE` in the workspace.
-/
#guard_msgs (positions := true) in
#import_diff to import ImportGraphTest.ImportDiff.Mods.ABCDE

-- Nice message when it has no library, too:
/--
@ +1:23...26
error: Could not find module `Foo` in the workspace.
-/
#guard_msgs (positions := true) in
#import_diff to import Foo

/--
warning: `#import_diff` in the module system uses `+`/`-` or `from`/`to` syntax:

  #import_diff
  ̲  ̲+̲i̲m̲p̲o̲r̲t̲ ̲ImportGraphTest
  ̲  ̲+̲i̲m̲p̲o̲r̲t̲ ̲ImportGraph
-/
#guard_msgs in
#import_diff ImportGraphTest ImportGraph
