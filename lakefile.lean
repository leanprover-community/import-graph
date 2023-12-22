import Lake
open Lake DSL

package «importGraph» where
  -- add package configuration options here

require Cli from git "https://github.com/leanprover/lean4-cli" @ "main"
require std from git "https://github.com/joneugster/std4" @ "upstream_from_mathlib_for_graph"

@[default_target]
lean_lib «ImportGraph» where
  -- add library configuration options here

/-- `lake exe graph` constructs import graphs in `.dot` or graphical formats. -/
@[default_target]
lean_exe «graph» where
  root := `Main
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true
