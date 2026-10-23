# Repository structure

## Executables

Defining an executable called with `lake exe` requires defining a `_root_.main` for this executable. These `main` functions must
not be included in any library, therefore they are located under `executables/`.

## Commands

Utility commands like `#find_home` should all be available when `import ImportGraph.Tools`. This is the intended endpoint for downstream
users to import the complete suite of available commands.

## `Lake` imports

The library `ImportGraph` should not import stuff from `Lake`, especially not the `Workspace`. It is fine if executables make such imports.
Thus the files importing parts of `Lake` are located under a separate library `ImportGraphLake`.

## namespaces and `Lean` extensions

Every declaration should be in the namespace `ImportGraph` to avoid
pollution of other namespaces.

Extensions to existing types from `Lean` can still be in the namespace `ImportGraph.___`. For example, consider `ImportGraph.Lean.Name.allNames`.
After the user added `open ImportGraph`, they can then use dot-notation `myName.allNames` as expected.
