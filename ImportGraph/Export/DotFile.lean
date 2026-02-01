/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Jon Eugster
-/
module

public import Lean.Data.NameMap.Basic

open Lean

/--
Helper which only returns `true` if the `module` is provided and the name `n` lies
inside it.
 -/
private def isInModule (module : Option Name) (n : Name) := match module with
  | some m => m.isPrefixOf n
  | none => false

/--
Write an import graph, represented as a `NameMap (Array Name)` to the ".dot" graph format.
* Nodes in the `unused` set will be shaded light gray.
* If `markedPackage` is provided:
  * Nodes which start with the `markedPackage` will be highlighted in green and drawn closer together.
  * Edges from `directDeps` into the module are highlighted in green
  * Nodes in `directDeps` are marked with a green border and green text.
  * Nodes in `withSorry` are highlighted in gold.
-/
public def asDotGraph
    (graph : NameMap (Array Name))
    (unused : NameSet := ∅)
    (header := "import_graph")
    (markedPackage : Option Name := none)
    (withSorry : NameSet := ∅)
    (directDeps : NameSet := ∅)
    (from_ to : NameSet := ∅):
    String := Id.run do
  let mut lines := #[s!"digraph \"{header}\" " ++ "{"]
  for (n, is) in graph do
    let shape := if from_.contains n then "invhouse" else if to.contains n then "house" else "ellipse"
    if markedPackage.isSome ∧ directDeps.contains n then
      -- note: `fillcolor` defaults to `color` if not specified
      let fill := if withSorry.contains n then
          "#ffd700"
        else if unused.contains n then
          "#e0e0e0"
        else
          "white"
      lines := lines.push s!"  \"{n}\" [style=filled, fontcolor=\"#4b762d\", color=\"#71b144\", fillcolor=\"{fill}\", penwidth=2, shape={shape}];"
    else if withSorry.contains n then
      lines := lines.push s!"  \"{n}\" [style=filled, fillcolor=\"#ffd700\", shape={shape}];"
    else if unused.contains n then
      lines := lines.push s!"  \"{n}\" [style=filled, fillcolor=\"#e0e0e0\", shape={shape}];"
    else if isInModule markedPackage n then
      -- mark node
      lines := lines.push s!"  \"{n}\" [style=filled, fillcolor=\"#96ec5b\", shape={shape}];"
    else
      lines := lines.push s!"  \"{n}\" [shape={shape}];"
    -- Then add edges
    for i in is do
      if isInModule markedPackage n then
        if isInModule markedPackage i then
          -- draw the main project close together
          lines := lines.push s!"  \"{i}\" -> \"{n}\" [weight=100];"
        else
          -- mark edges into the main project
          lines := lines.push s!"  \"{i}\" -> \"{n}\" [penwidth=2, color=\"#71b144\"];"
      else
        lines := lines.push s!"  \"{i}\" -> \"{n}\";"
  lines := lines.push "}"
  return "\n".intercalate lines.toList
