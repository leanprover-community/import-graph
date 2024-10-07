# Visualised import graph

## Instructions

To test this, place a file `imports.gexf` inside this directory. You can create such a file with

```
lake exe graph html-template/imports.gexf
```

Then open `index.html` in any browser and you should see the graph.

## Development

Currently `lake exe graph output.html` will use the files here to create a stand-alone
HTML file. It does so by search-replacing the JS-scripts, the `fetch('imports.gexf')`
statement, and the `<h1>` header.

Therefore any modifications to these lines need to be reflected in `ImportGraph/Cli.lean`!

# Credits

This tool has been adapted from its [Lean 3 version](https://github.com/eric-wieser/mathlib-import-graph) written by Eric Wieser, which was published under the [MIT License](./LICENSE_source)
included here.

Adaptation by Jon Eugster.
