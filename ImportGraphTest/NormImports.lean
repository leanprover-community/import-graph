module


import all Lean.Syntax

public meta import ImportGraph.Tools.NormImports

public import ImportGraph.Imports.Pretty



import ImportGraph.Lean.EnvExtension -- redundant

    public import ImportGraph.Shake.EnvExtension

/- Extra comments below the header, which should be ignored -/

/--
warning: Imports can be normalized, but some comments could not be carried over. Please review the comment that will be inserted after the imports.

  [apply] public import ImportGraph.Imports.Pretty
  public import ImportGraph.Shake.EnvExtension
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
