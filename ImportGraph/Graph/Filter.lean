/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public import Lean.Data.NameMap.Basic
import Std.Data.TreeMap.AdditionalOperations
import ImportGraph.Graph.TransitiveClosure

/-!
# Filtering a graph

A "graph" in this context is a `NameMap (Array Name)`.
This file defines some functions to filter a graph.
-/

namespace Lean.NameMap

/--
Compute the transitive reduction of an import graph.

Typical usage is `transitiveReduction (← importGraph)`.
-/
public def transitiveReduction (m : NameMap (Array Name)) : NameMap (Array Name) :=
  let c := transitiveClosure m
  m.foldl (fun r n a =>
    r.insert n (a.foldr (fun i b => b.filter (fun j => ! ((c.find? i).getD {}).contains j)) a)) {}

/-- Restrict an import graph to only the downstream dependencies of some set of modules. -/
public def downstreamOf (m : NameMap (Array Name)) (targets : NameSet) : NameMap (Array Name) :=
  let tc := transitiveClosure m
  let P (n : Name) := targets.contains n || ((tc.find? n).getD {}).any fun j => targets.contains j
  m.foldl (init := {}) fun r n i =>
    if P n then
      r.insert n (i.filter P)
    else
      r

/-- Restrict an import graph to only the transitive imports of some set of modules. -/
public def upstreamOf (m : NameMap (Array Name)) (targets : NameSet) : NameMap (Array Name) :=
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
private partial def transitiveFilteredUpstream (node : Name) (graph : NameMap (Array Name))
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
public def filterGraph (graph : NameMap (Array Name)) (filter : Name → Bool)
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
