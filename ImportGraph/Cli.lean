/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Cli.Basic
import Batteries.Lean.IO.Process
import ImportGraph.CurrentModule
import ImportGraph.Imports
import ImportGraph.Lean.Name
import ImportGraph.Gexf

open Cli

open Lean
open ImportGraph

/--
Write an import graph, represented as a `NameMap (Array Name)` to the ".dot" graph format.
* Nodes in the `unused` set will be shaded light gray.
* Nodes which start with the `markedModule` and edges into them will be highlighted in green.
-/
def asDotGraph
    (graph : NameMap (Array Name))
    (unused : NameSet := {})
    (header := "import_graph")
    (markedModule : Option Name := none) :
    String := Id.run do

  let mut lines := #[s!"digraph \"{header}\" " ++ "{"]
  for (n, is) in graph do
    if unused.contains n then
      lines := lines.push s!"  \"{n}\" [style=filled, fillcolor=\"#e0e0e0\"];"
    else if isInModule markedModule n then
      -- mark node
      lines := lines.push s!"  \"{n}\" [style=filled, fillcolor=\"#96ec5b\"];"
    else
      lines := lines.push s!"  \"{n}\";"
    -- Then add edges
    for i in is do
      if isInModule markedModule n then
        if isInModule markedModule i then
          -- draw the main project close together
          lines := lines.push s!"  \"{i}\" -> \"{n}\" [weight=100];"
        else
          -- mark edges into the main project
          lines := lines.push s!"  \"{i}\" -> \"{n}\" [penwidth=2, color=\"#71b144\"];"
      else
        lines := lines.push s!"  \"{i}\" -> \"{n}\";"
  lines := lines.push "}"
  return "\n".intercalate lines.toList

open Lean Core System

open IO.FS IO.Process Name in
/-- Implementation of the import graph command line program. -/
def importGraphCLI (args : Cli.Parsed) : IO UInt32 := do
  -- file extensions that should be created
  let extensions : Array String := match args.variableArgsAs! String with
    | #[] => #["dot"]
    | outputs => outputs.foldl (fun acc (o : String) =>
      match FilePath.extension o with
       | none => if acc.contains "dot" then acc else acc.push "dot"
       | some "gexf" => if acc.contains "gexf" then acc else acc.push "gexf"
       | some "html" => if acc.contains "gexf" then acc else acc.push "gexf"
       -- currently all other formats are handled by passing the `.dot` file to
       -- graphviz
       | some _ => if acc.contains "dot" then acc else acc.push "dot" ) #[]

  let to ← match args.flag? "to" with
  | some to => pure <| to.as! ModuleName
  | none => getCurrentModule
  let from? := match args.flag? "from" with
  | some fr => some <| fr.as! ModuleName
  | none => none
  searchPathRef.set compile_time_search_path%

  let outFiles ← try unsafe withImportModules #[{module := to}] {} (trustLevel := 1024) fun env => do
    let p := ImportGraph.getModule to
    let ctx := { options := {}, fileName := "<input>", fileMap := default }
    let state := { env }
    let mut graph := env.importGraph
    let unused ←
      match args.flag? "to"  with
      | some _ =>
        let used ← Prod.fst <$> (CoreM.toIO (env.transitivelyRequiredModules to) ctx state)
        pure <| graph.fold (fun acc n _ => if used.contains n then acc else acc.insert n) NameSet.empty
      | none => pure NameSet.empty
    if let Option.some f := from? then
      graph := graph.downstreamOf (NameSet.empty.insert f)
    let toModule := ImportGraph.getModule to
    let includeLean := args.hasFlag "include-lean"
    let includeStd := args.hasFlag "include-std" || includeLean
    let includeDeps := args.hasFlag "include-deps" || args.hasFlag "mark-module" || includeStd
    let filter (n : Name) : Bool :=
      toModule.isPrefixOf n ||
      bif isPrefixOf `Std n then includeStd else
      bif isPrefixOf `Lean n || isPrefixOf `Init n then includeLean else
      includeDeps
    graph := graph.filterMap (fun n i => if filter n then (i.filter filter) else none)
    if args.hasFlag "exclude-meta" then
      -- Mathlib-specific exclusion of tactics
      let filterMathlibMeta : Name → Bool := fun n => (
        isPrefixOf `Mathlib.Tactic n ∨
        isPrefixOf `Mathlib.Lean n ∨
        isPrefixOf `Mathlib.Mathport n ∨
        isPrefixOf `Mathlib.Util n)
      graph := graph.filterGraph filterMathlibMeta (replacement := `«Mathlib.Tactics»)
    if !args.hasFlag "show-transitive" then
      graph := graph.transitiveReduction

    let markedModule : Option Name := if args.hasFlag "mark-module" then p else none

    -- Create all output files that are requested
    let mut outFiles : HashMap String String := {}
    if extensions.contains "dot" then
      let dotFile := asDotGraph graph (unused := unused) (markedModule := markedModule)
      outFiles := outFiles.insert "dot" dotFile
    if extensions.contains "gexf" then
      -- filter out the top node as it makes the graph less pretty
      let graph₂ := match args.flag? "to" with
        | none => graph.filter (fun n _ => n != to)
        | some _ => graph
      let (out, _) ← CoreM.toIO (Graph.toGexf graph₂ p) ctx state
      outFiles := outFiles.insert "gexf" out
    return outFiles

  catch err =>
    -- TODO: try to build `to` first, so this doesn't happen
    throw <| IO.userError <| s!"{err}\nIf the error above says `unknown package`, " ++
      s!"try if `lake build {to}` fixes the issue"
    throw err

  match args.variableArgsAs! String with
  | #[] => writeFile "import_graph.dot" (outFiles.find! "dot")
  | outputs => for o in outputs do
     let fp : FilePath := o
     match fp.extension with
     | none
     | "dot" => writeFile fp (outFiles.find! "dot")
     | "gexf" => IO.FS.writeFile fp (outFiles.find! "gexf")
     | "html" =>
        -- use `html-template/index.html` and insert any dependencies to make it
        -- a stand-alone HTML file.
        let gexFile := (outFiles.find! "gexf")
        -- The directory where the import-graph soure is located
        let exeDir := (FilePath.parent (← IO.appPath) |>.get!) / ".." / ".." / ".."
        let mut html ← IO.FS.readFile <| ← IO.FS.realPath ( exeDir / "html-template" / "index.html")
        for dep in (#[
            "vendor" / "sigma.min.js",
            "vendor" / "graphology.min.js",
            "vendor" / "graphology-library.min.js" ] : Array FilePath) do
          let depContent ← IO.FS.readFile <| ← IO.FS.realPath (exeDir / "html-template" / dep)
          html := html.replace s!"<script src=\"{dep}\"></script>"
            s!"<script>{depContent}</script>"
        html := html.replace "fetch(\"imports.gexf\").then((res) => res.text()).then(render_gexf)"
          s!"render_gexf(\"{gexFile.replace "\n" ""|>.replace "\"" "\\\""}\")"
          |>.replace "<h1>Import Graph</h1>"
          s!"<h1>Import Graph for {to}</h1>"
        IO.FS.writeFile fp html
     | some ext => try
        _ ← runCmdWithInput "dot" #["-T" ++ ext, "-o", o] (outFiles.find! "dot")
      catch ex =>
        IO.eprintln s!"Error occurred while writing out {fp}."
        IO.eprintln s!"Make sure you have `graphviz` installed and the file is writable."
        throw ex
  return 0
