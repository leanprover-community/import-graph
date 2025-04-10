import ImportGraph.Imports

open Lean

/--
`lake exe unused_transitive_imports m1 m2 ...`

For each specified module `m`, prints those `n` from the argument list which are imported, but transitively unused by `m`.
-/
def main (args : List String) : IO UInt32 := do
  let (flags, args) := args.partition (fun s => s.startsWith "-")
  let mut modules := args.map (fun s => s.toName)
  Core.withImportModules modules.toArray do
    let r ← unusedTransitiveImports modules (verbose := flags.contains "-v" || flags.contains "--verbose")
    for (n, u) in r do
      IO.println s!"{n}: {u}"
    return 0
