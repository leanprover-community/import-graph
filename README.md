# import-graph

A simple tool to create import graphs of lake packages.


## Requirements

For creating different output formats than `.dot` (for example to create a `.pdf` file), you should have `graphviz` installed.

## Usage

You can import this in any lean projects by the following line to your `lakefile.lean`:

```lean
require importGraph from git "https://github.com/leanprover-community/import-graph" @ "main"
```

After running `lake update -R importGraph` in your project, you can create import graphs with

```bash
lake exe graph
```

A typical command is `lake exe graph --reduce --to MyModule my_graph.pdf` where `MyModule` follows the same module naming you would use to `import` it in lean.
See `lake exe graph --help` for more options.

## Commands

There are a few commands implemented, which help you analysing the imports of a file. These are accessible by adding `import ImportGraph.Imports` to your lean file.

* `#redundant_imports`: lists any transitively redundant imports in the current module.
* `#minimize_imports`: attempts to construct a minimal set of imports for the declarations
  in the current file.
  (Must be run at the end of the file. Tactics and macros may result in incorrect output.)
* `#find_home decl`: suggests files higher up the import hierarchy to which `decl` could be moved.

## Credits

This code has been extracted from [mathlib](https://github.com/leanprover-community/mathlib4) and has mainly been written by Scott Morrison and a few other mathlib contributors.
