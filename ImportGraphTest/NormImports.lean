module

import all Lean.Syntax

import Lean.Widget
import Std.WP.Monad
import Std.Async
-- `Std.*` shouldn't come before `Lean.*` simply because it has fewer components
import Lean.Meta.Sym.Simp
import Lean.Server.FileWorker.WidgetRequests

public meta import ImportGraph.Tools.NormImports

public import ImportGraph.Imports.Pretty


        meta import ImportGraph.Tools.NormImports

    public meta import ImportGraph.Tools

import ImportGraph.Lean.EnvExtension -- redundant

    public import ImportGraph.Shake.EnvExtension

/- Extra comments below the header, which should be ignored -/

/--
warning: Imports can be normalized, but some comments could not be carried over. Please review the comment that will be inserted after the imports.

  [apply] public meta import ImportGraph.Tools
  public import ImportGraph.Imports.Pretty
  public import ImportGraph.Shake.EnvExtension
  ⏎
  -- `Std.*` shouldn't come before `Lean.*` simply because it has fewer components
  import Lean.Meta.Sym.Simp
  import Lean.Server.FileWorker.WidgetRequests
  import Lean.Widget
  import Std.Async
  import Std.WP.Monad
  ⏎
  import all Lean.Syntax
  ⏎
  /-
  The following imports did not appear in the new import list, but had comments around them:
  ```
  import ImportGraph.Lean.EnvExtension -- redundant
  ```
  ⏎
  -/
-/
#guard_msgs in
#norm_imports
