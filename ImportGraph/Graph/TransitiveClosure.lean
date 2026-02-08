/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public import Lean.Data.NameMap.Basic

/-!
# Transitive closure of a graph

A "graph" in this context is a `NameMap (Array Name)`.
-/

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
