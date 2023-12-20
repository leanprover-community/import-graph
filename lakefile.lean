import Lake
open Lake DSL

package «importGraph» where
  -- add package configuration options here

require std from git "https://github.com/leanprover/std4" @ "9dd24a3493cceefa2bede383f21e4ef548990b68"
require Cli from git "https://github.com/leanprover/lean4-cli" @ "main"

@[default_target]
lean_lib «ImportGraph» where
  -- add library configuration options here

/-- `lake exe graph` constructs import graphs in `.dot` or graphical formats. -/
@[default_target]
lean_exe «graph» where
  root := `ImportGraph.Main
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true
