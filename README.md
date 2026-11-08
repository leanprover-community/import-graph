# importGraph

A simple tool to create import graphs of lake packages.


## Requirements

For creating different output formats than `.dot` (for example to create a `.pdf` file), you should have [`graphviz`](https://graphviz.org/) installed.

## Usage

If you are using mathlib, the tool will already be available. If not, see installation notes below.

Once available in your project, you can create import graphs with

```bash
lake exe graph
```

A typical command is

```bash
lake exe graph --to MyModule my_graph.pdf
```
where `MyModule` follows the same module naming you would use to `import` it in lean. See `lake exe graph --help` for more options.

You can specify multiple sources and targets e.g. as
```bash
lake exe graph --from MyModule1,MyModule2 --to MyModule3,MyModule4 my_graph.pdf
```

### Troubleshoot

* make sure to `lake build` your project (or the specified `--to` module) before using `lake exe graph`!

### Json

To create a Json file, you can use `.xdot_json` as output type:

```bash
lake exe graph my_graph.xdot_json
```

### HTML

```bash
lake exe graph my_graph.html
```

creates a stand-alone HTML file visualising the import structure.

Clicking a node opens its page in the Mathlib documentation. For another project, pass the base URL
of its documentation:

```bash
lake exe graph --to MyProject --doc-url https://example.org/docs/ my_graph.html
```

## Commands

There are a few commands implemented, which help you analysing the imports of a file. These are accessible by adding `import ImportGraph.Tools` to your lean file. Some commands respect the module system, and some do not.

**Module-system aware commands:**

* `#norm_imports`: removes transitively redundant imports from the current file and suggests the normalized result.
* `#min_imports`: attempts to construct a minimal set of imports for the commands in the current file, and suggests the result. This may be run at any point in the file, and will still take the whole file into account.
* `#find_home for <cmd>`: suggests files higher up in the import hierarchy to which the command and any dependencies from the same file could be moved.
* `#show_imports for <cmd>`: shows the minimal imports needed for a given command, and allows clicking to copy the imports. Also shows new constants produced by the command and shake records.

**Commands not yet aware of the module system:**

* `#redundant_imports`: lists any transitively redundant imports in the current module.
* `#import_diff foo bar ...` computes the new transitive imports that are added to a given file when
  modules `foo, bar, ...` are added to the set of imports of the file.

## Widgets

There are a few widget for the infoview available under `ImportGraph.Widget`:

- `collapsible summary body` renders a dropdown `<details>` element.
- `copyToClipboard copyText` adds a click-action which copies `copyText` into clipboard.
- `goToModule name` displays the name of the module with a click-action to jump to said module.
- `goToModuleOfDecl decl` displays the name of the module where the decl live and jumps to the decl on click
- `goToModuleOfDecls decls` displays the name of the module where the decls live and jumps to the decls on click

These can be used in anywhere where `MessageData` is used:

```lean
open Lean ImportGraph.Widget in
#eval show CoreM _ from do
  logInfo <| ← collapsible "summary" "body"
```

which renders a message

```
▼ summary
  body
```

## Source-File-Based Import Analysis

The `ImportGraph.Imports.FromSource` module provides functions for analyzing imports by parsing source files directly, without requiring a built environment. This is useful for scripts, linters, and tools that need to analyze imports quickly or on files that haven't been compiled yet.

### Functions

Add `import ImportGraph.Imports.FromSource` to your Lean script to access:

* `findImportsFromSource (path : System.FilePath) : IO (Array Name)`: Parse direct imports from a single file.
* `findTransitiveImportsFromSource (startPath : System.FilePath) (rootFilter : Option Name := none) : IO NameSet`: Compute the transitive closure of imports from source files.

### Example

```lean
import ImportGraph.Imports.FromSource

-- Get all transitive Mathlib imports from a file
#eval do
  let imports ← findTransitiveImportsFromSource "Mathlib/Algebra/Ring/Basic.lean" (some `Mathlib)
  IO.println s!"Transitive Mathlib imports: {imports.toArray.qsort Name.lt}"
```

## Other executables

`lake exe unused_transitive_imports m1 m2 ...`

For each specified module `m`, prints those `n` from the argument list which are imported, but transitively unused by `m`.

## Installation

The installation works exactly like for any [Lake package](https://reservoir.lean-lang.org/),
see [Lake docs](https://github.com/leanprover/lean4/tree/master/src/lake#supported-sources).

*This only relevant if your project does not already require `importGraph` through another lake package (e.g. mathlib). If it does, do not follow these instructions; instead just use the tool with `lake exe graph`!*

You can import this in any lean projects by the following line to your `lakefile.lean`:

```lean
require "leanprover-community" / "importGraph" @ git "main"
```

or, if you have a `lakefile.toml`, it would be

```toml
[[require]]
name = "importGraph"
source = "leanprover-community"
rev = "main"
```

Then, you might need to call `lake update -R importGraph` in your project.

## Contribution

Please open PRs/Issues if you have troubles or would like to contribute new features!

Available documentation:

- [Repository Structure](./doc/REPO_STRUCTURE.md)

## Credits

The main tool has been extracted from [mathlib](https://github.com/leanprover-community/mathlib4),
originally written by Kim Morrison and other mathlib contributors.

The HTML visualisation has been incorporated from
[a project by Eric Wieser](https://github.com/eric-wieser/mathlib-import-graph).

### Maintainers

Primarily maintained by [Jon Eugster](https://leanprover.zulipchat.com/#narrow/dm/385895-Jon-Eugster), Kim Morrison, and the wider leanprover community.
