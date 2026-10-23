# Repository structure

## Executables

Defining an executable called with `lake exe` requires defining a `_root_.main` for this executable. These `main` functions must
not be included in any library, therefore they are located under `executables/`.

## Commands

Utility commands like `#find_home` should all be available when `import ImportGraph.Tools`. This is the intended endpoint for downstream
users to import the complete suite of available commands.
