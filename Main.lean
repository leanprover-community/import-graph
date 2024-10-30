import ImportGraph.Cli

/-!
# `lake exe graph`

This is a replacement for Lean 3's `leanproject import-graph` tool.
-/

open Cli

/-- Setting up command line options and help text for `lake exe graph`. -/
def graph : Cmd := `[Cli|
  graph VIA importGraphCLI; ["0.0.3"]
  "Generate representations of a Lean import graph. \
   By default generates the import graph up to `Mathlib`. \
   If you are working in a downstream project, use `lake exe graph --to MyProject`."

  FLAGS:
    "show-transitive";         "Show transitively redundant edges."
    "to" : Array ModuleName;   "Only show the upstream imports of the specified modules."
    "from" : Array ModuleName; "Only show the downstream dependencies of the specified modules."
    "exclude-meta";            "Exclude any files starting with `Mathlib.[Tactic|Lean|Util|Mathport]`."
    "include-direct";          "Include directly imported files from other libraries"
    "include-deps";            "Include used files from other libraries (not including Lean itself and `std`)"
    "include-std";             "Include used files from the Lean standard library (implies `--include-deps`)"
    "include-lean";            "Include used files from Lean itself (implies `--include-deps` and `--include-std`)"
    "mark-package";            "Visually highlight the package containing the first `--to` target (used in combination with some `--include-XXX`)."

  ARGS:
    ...outputs : String;  "Filename(s) for the output. \
      If none are specified, generates `import_graph.dot`. \
      Automatically chooses the format based on the file extension. \
      Currently supported formats are `.dot`, `.gexf`, `.html`, \
      and if you have `graphviz` installed then any supported output format is allowed."
]


/-- `lake exe graph` -/
def main (args : List String) : IO UInt32 :=
  graph.validate args
