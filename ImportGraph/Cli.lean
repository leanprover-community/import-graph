/-
Copyright (c) 2023 Scott Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Scott Morrison
-/
import Cli.Basic
import Batteries.Lean.IO.Process
import Batteries.Lean.Util.Path
import ImportGraph.CurrentModule
import ImportGraph.Imports
import ImportGraph.Lean.Name

open Cli

open Lean
open ImportGraph

/-- Write an import graph, represented as a `NameMap (Array Name)` to the ".dot" graph format. -/
def asDotGraph (graph : NameMap (Array Name)) (header := "import_graph") : String := Id.run do
  let mut lines := #[s!"digraph \"{header}\" " ++ "{"]
  for (n, is) in graph do
    for i in is do
      lines := lines.push s!"  \"{i}\" -> \"{n}\";"
  lines := lines.push "}"
  return "\n".intercalate lines.toList

open Lean Core System

open IO.FS IO.Process Name in
/-- Implementation of the import graph command line program. -/
def importGraphCLI (args : Cli.Parsed) : IO UInt32 := do
  let to ← match args.flag? "to" with
  | some to => pure <| to.as! ModuleName
  | none => getCurrentModule
  let from? := match args.flag? "from" with
  | some fr => some <| fr.as! ModuleName
  | none => none
  searchPathRef.set compile_time_search_path%
  let dotFile ← try unsafe withImportModules #[{module := to}] {} (trustLevel := 1024) fun env => do
    let mut graph := env.importGraph
    if let Option.some f := from? then
      graph := graph.downstreamOf (NameSet.empty.insert f)
    if ¬(args.hasFlag "include-deps") then
      let p := ImportGraph.getModule to
      graph := graph.filterMap (fun n i =>
        if p.isPrefixOf n then (i.filter (isPrefixOf p)) else none)
    if args.hasFlag "exclude-meta" then
      -- Mathlib-specific exclusion of tactics
      let filterMathlibMeta : Name → Bool := fun n => (
        isPrefixOf `Mathlib.Tactic n ∨
        isPrefixOf `Mathlib.Lean n ∨
        isPrefixOf `Mathlib.Mathport n ∨
        isPrefixOf `Mathlib.Util n)
      graph := graph.filterGraph filterMathlibMeta (replacement := `«Mathlib.Tactics»)
    if args.hasFlag "reduce" then
      graph := graph.transitiveReduction
    return asDotGraph graph
  catch err =>
    -- TODO: try to build `to` first, so this doesn't happen
    throw <| IO.userError <| s!"{err}\nIf the error above says `unknown package`, " ++
      s!"try if `lake build {to}` fixes the issue"
    throw err
  match args.variableArgsAs! String with
  | #[] => writeFile "import_graph.dot" dotFile
  | outputs => for o in outputs do
     let fp : FilePath := o
     match fp.extension with
     | none
     | "dot" => writeFile fp dotFile
     | some ext => try
        _ ← runCmdWithInput "dot" #["-T" ++ ext, "-o", o] dotFile
      catch ex =>
        IO.eprintln s!"Error occurred while writing out {fp}."
        IO.eprintln s!"Make sure you have `graphviz` installed and the file is writable."
        throw ex
  return 0
