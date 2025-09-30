/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public import Lean.Elab.Command
public import Lean.Server.GoTo
public import Lean.Widget.UserWidget
public import ImportGraph.RequiredModules

public section

open Lean

namespace Lean.Environment

/--
Find the imports of a given module.
-/
def importsOf (env : Environment) (n : Name) : Array Name :=
  if n = env.header.mainModule then
    env.header.imports.map Import.module
  else match env.getModuleIdx? n with
    | .some idx => env.header.moduleData[idx.toNat]!.imports.map Import.module |>.erase `Init
    | .none => #[]

/--
Construct the import graph of the current file.
-/
partial def importGraph (env : Environment) : NameMap (Array Name) :=
  let main := env.header.mainModule
  let imports := env.header.imports.map Import.module
  imports.foldl (fun m i => process env i m) (({} : NameMap _).insert main imports)
    |>.erase Name.anonymous
where
  process (env) (i) (m) : NameMap (Array Name) :=
    if m.contains i then
      m
    else
      let imports := env.importsOf i
      imports.foldr (fun i m => process env i m) (m.insert i imports)

/--
Return the redundant imports (i.e. those transitively implied by another import)
amongst a candidate list of imports.
-/
partial def findRedundantImports (env : Environment) (imports : Array Name) : NameSet :=
  let run := visit env.importGraph imports
  let (_, seen) := imports.foldl (fun ⟨v, s⟩ n => run v s n) ({}, {})
  seen
where
  visit (Γ) (targets) (visited) (seen) (n) : NameSet × NameSet :=
    if visited.contains n then
      (visited, seen)
    else
      let imports := (Γ.find? n).getD #[]
      let (visited', seen') := imports.foldl (fun ⟨v, s⟩ t => visit Γ targets v s t) (visited, seen)
      (visited'.insert n,
        imports.foldl (fun s t => if targets.contains t then s.insert t else s) seen')

end Lean.Environment

namespace Lean.NameMap

/--
Compute the transitive closure of an import graph.
-/
public partial def transitiveClosure (m : NameMap (Array Name)) : NameMap NameSet :=
  m.foldl (fun r n i => process r n i) {}
where
  process (r : NameMap NameSet) (n : Name) (i : Array Name) : NameMap NameSet :=
    if r.contains n then
      r
    else
      let r' := i.foldr (fun i r => process r i ((m.find? i).getD #[])) r
      let t := i.foldr
        (fun j s => ((r'.find? j).getD {}).foldl NameSet.insert s)
        (.ofList i.toList)
      r'.insert n t

/--
Compute the transitive reduction of an import graph.

Typical usage is `transitiveReduction (← importGraph)`.
-/
def transitiveReduction (m : NameMap (Array Name)) : NameMap (Array Name) :=
  let c := transitiveClosure m
  m.foldl (fun r n a =>
    r.insert n (a.foldr (fun i b => b.filter (fun j => ! ((c.find? i).getD {}).contains j)) a)) {}

/-- Restrict an import graph to only the downstream dependencies of some set of modules. -/
def downstreamOf (m : NameMap (Array Name)) (targets : NameSet) : NameMap (Array Name) :=
  let tc := transitiveClosure m
  let P (n : Name) := targets.contains n || ((tc.find? n).getD {}).any fun j => targets.contains j
  m.foldl (init := {}) fun r n i =>
    if P n then
      r.insert n (i.filter P)
    else
      r

/-- Restrict an import graph to only the transitive imports of some set of modules. -/
def upstreamOf (m : NameMap (Array Name)) (targets : NameSet) : NameMap (Array Name) :=
  let tc := transitiveClosure m
  let P (n : Name) := targets.contains n || targets.any fun t => ((tc.find? t).getD {}).contains n
  m.foldl (init := {}) fun r n i =>
    if P n then
      r.insert n (i.filter P)
    else
      r

/--
Filter the list of edges `… → node` inside `graph` by the function `filter`.

Any such upstream node `source` where `filter source` returns true will be replaced
by all its upstream nodes. This results in a list of all unfiltered nodes
in the `graph` that either had an edge to `node`
or had an indirect edge to `node` going through filtered nodes.

Will panic if the `node` is not in the `graph`.
-/
partial
def transitiveFilteredUpstream (node : Name) (graph : NameMap (Array Name))
    (filter : Name → Bool) (replacement : Option Name := none):
    List Name :=
  (graph.get! node).toList.flatMap fun source =>
    ( if filter source then
        -- Add the transitive edges going through the filtered node `source`.
        -- If there is a replacement node, add an additional edge `repl → node`.
        match replacement with
        | none => transitiveFilteredUpstream source graph filter
        | some repl => .cons repl <| transitiveFilteredUpstream source graph filter
      -- If the node is not filtered, we leave the edge `source → node` intact.
      else [source]).eraseDups

/--
Filters the `graph` removing all nodes where `filter n` returns false. Additionally,
replace edges from removed nodes by all the transitive edges.

This means there is a path between two nodes in the filtered graph iff there exists
such a path in the original graph.

If the optional `(replacement : Name)` is provided, a corresponding node will be
added together with edges to all nodes which had an incoming edge from any
filtered node.
-/
def filterGraph (graph : NameMap (Array Name)) (filter : Name → Bool)
    (replacement : Option Name := none) : NameMap (Array Name) :=
  -- Create a list of all files imported by any of the filtered files
  -- and remove all imports starting with `Mathlib` to avoid loops.
  let replImports := graph.toList.flatMap
    (fun ⟨n, i⟩ => if filter n then i.toList else [])
    |>.eraseDups |>.filter (¬ Name.isPrefixOf `Mathlib ·) |>.toArray
  let graph := graph.filterMap (fun node edges => if filter node then none else some <|
    -- If the node `node` is not filtered, modify the `edges` going into `node`.
    edges.toList.flatMap (fun source =>
      if filter source then
        transitiveFilteredUpstream source graph filter (replacement := replacement)
      else [source]) |>.eraseDups.toArray)
  -- Add a replacement node if provided.
  match replacement with
  | none => graph
  | some repl => graph.insert repl replImports

end Lean.NameMap

/--
Returns a `List (Name × List Name)` with a key for each module `n` in `amongst`,
whose corresponding value is the list of modules `m` in `amongst` which are transitively imported by `n`,
but no declaration in `n` makes use of a declaration in `m`.
-/
def unusedTransitiveImports (amongst : List Name) (verbose : Bool := false) : CoreM (List (Name × List Name)) := do
  let env ← getEnv
  let transitiveImports := env.importGraph.transitiveClosure
  let transitivelyRequired ← env.transitivelyRequiredModules' amongst verbose
  amongst.mapM fun n => do return (n,
    let unused := (transitiveImports.find? n).getD {} \ (transitivelyRequired.find? n |>.getD {})
    amongst.filter (fun m => unused.contains m))

def Core.withImportModules (modules : Array Name) {α} (f : CoreM α) : IO α := do
  initSearchPath (← findSysroot)
  unsafe Lean.withImportModules (modules.map (fun m => {module := m})) {} (trustLevel := 1024)
    fun env => Prod.fst <$> Core.CoreM.toIO
        (ctx := { fileName := "<CoreM>", fileMap := default }) (s := { env := env }) do f

/--
Return the redundant imports (i.e. those transitively implied by another import)
of a specified module (or the current module if `none` is specified).
-/
def redundantImports (n? : Option Name := none) : CoreM NameSet := do
  let env ← getEnv
  let imports := env.importsOf (n?.getD (env.header.mainModule))
  return env.findRedundantImports imports
