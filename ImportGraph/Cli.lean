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
* If `markedModule` is provided:
  * Nodes which start with the `markedModule` will be highlighted in green and drawn closer together.
  * Edges from `directDeps` into the module are highlighted in green
  * Nodes in `directDeps` are marked with a green border and green text.
-/
def asDotGraph
    (graph : NameMap (Array Name))
    (unused : NameSet := {})
    (header := "import_graph")
    (markedModule : Option Name := none)
    (directDeps : NameSet := {}) :
    String := Id.run do

  let mut lines := #[s!"digraph \"{header}\" " ++ "{"]
  for (n, is) in graph do
    if markedModule.isSome ∧ directDeps.contains n then
      -- note: `fillcolor` defaults to `color` if not specified
      let fill := if unused.contains n then "#e0e0e0" else "white"
      lines := lines.push s!"  \"{n}\" [style=filled, fontcolor=\"#4b762d\", color=\"#71b144\", fillcolor=\"{fill}\", penwidth=2];"
    else if unused.contains n then
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
  let extensions : Std.HashSet String := match args.variableArgsAs! String with
    | #[] => Std.HashSet.empty.insert "dot"
    | outputs => outputs.foldl (fun acc (o : String) =>
      match FilePath.extension o with
       | none => acc.insert "dot"
       | some "gexf" => acc.insert "gexf"
       | some "html" => acc.insert "gexf"
       -- currently all other formats are handled by passing the `.dot` file to
       -- graphviz
       | some _ => acc.insert "dot" ) {}

  let to ← match args.flag? "to" with
  | some to => pure <| to.as! ModuleName
  | none => getCurrentModule
  let from? := match args.flag? "from" with
  | some fr => some <| fr.as! ModuleName
  | none => none
  searchPathRef.set compile_time_search_path%

  let outFiles ← try unsafe withImportModules #[{module := to}] {} (trustLevel := 1024) fun env => do
    let p := ImportGraph.getModule to
    let mut graph := env.importGraph
    let unused ←
      match args.flag? "to"  with
      | some _ =>
        let ctx := { options := {}, fileName := "<input>", fileMap := default }
        let state := { env }
        let used ← Prod.fst <$> (CoreM.toIO (env.transitivelyRequiredModules to) ctx state)
        pure <| graph.fold (fun acc n _ => if used.contains n then acc else acc.insert n) NameSet.empty
      | none => pure NameSet.empty
    if let Option.some f := from? then
      graph := graph.downstreamOf (NameSet.empty.insert f)
    let toModule := ImportGraph.getModule to
    let includeLean := args.hasFlag "include-lean"
    let includeStd := args.hasFlag "include-std" || includeLean
    let includeDeps := args.hasFlag "include-deps" || includeStd
    -- Note: `includeDirect` does not imply `includeDeps`!
    -- e.g. if the module contains `import Lean`, the node `Lean` will be included with
    -- `--include-direct`, but not included with `--include-deps`.
    let includeDirect := args.hasFlag "include-direct"

    -- `directDeps` contains files which are not in the module
    -- but directly imported by a file in the module
    let directDeps : NameSet := graph.fold (init := .empty) (fun acc n deps =>
      if toModule.isPrefixOf n then
        deps.filter (!toModule.isPrefixOf ·) |>.foldl (init := acc) NameSet.insert
      else
        acc)

    let filter (n : Name) : Bool :=
      toModule.isPrefixOf n ||
      bif isPrefixOf `Std n then includeStd else
      bif isPrefixOf `Lean n || isPrefixOf `Init n then includeLean else
      includeDeps
    let filterDirect (n : Name) : Bool :=
      includeDirect ∧ directDeps.contains n

    graph := graph.filterMap (fun n i =>
      if filter n then
        -- include node regularly
        (i.filter (fun m => filterDirect m || filter m))
      else if filterDirect n then
        -- include node as direct dependency; drop any further deps.
        some #[]
      else
        -- not included
        none)
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
    let mut outFiles : Std.HashMap String String := {}
    if extensions.contains "dot" then
      let dotFile := asDotGraph graph (unused := unused) (markedModule := markedModule) (directDeps := directDeps)
      outFiles := outFiles.insert "dot" dotFile
    if extensions.contains "gexf" then
      -- filter out the top node as it makes the graph less pretty
      let graph₂ := match args.flag? "to" with
        | none => graph.filter (fun n _ => n != to)
        | some _ => graph
      let gexFile := Graph.toGexf graph₂ p env
      outFiles := outFiles.insert "gexf" gexFile
    return outFiles

  catch err =>
    -- TODO: try to build `to` first, so this doesn't happen
    throw <| IO.userError <| s!"{err}\nIf the error above says `unknown package`, " ++
      s!"try if `lake build {to}` fixes the issue"
    throw err

  match args.variableArgsAs! String with
  | #[] => writeFile "import_graph.dot" (outFiles["dot"]!)
  | outputs => for o in outputs do
     let fp : FilePath := o
     match fp.extension with
     | none
     | "dot" => writeFile fp (outFiles["dot"]!)
     | "gexf" => IO.FS.writeFile fp (outFiles["gexf"]!)
     | "html" =>
        let gexfFile := (outFiles["gexf"]!)
        -- use `html-template/index.html` and insert any dependencies to make it
        -- a stand-alone HTML file.
        -- note: changes in `index.html` might need to be reflected here!
        let exeDir := (FilePath.parent (← IO.appPath) |>.get!) / ".." / ".." / ".."
        let mut html ← IO.FS.readFile <| ← IO.FS.realPath ( exeDir / "html-template" / "index.html")
        for dep in (#[
            "vendor" / "sigma.min.js",
            "vendor" / "graphology.min.js",
            "vendor" / "graphology-library.min.js" ] : Array FilePath) do
          let depContent ← IO.FS.readFile <| ← IO.FS.realPath (exeDir / "html-template" / dep)
          html := html.replace s!"<script src=\"{dep}\"></script>" s!"<script>{depContent}</script>"
        -- inline the graph data
        -- note: changes in `index.html` might need to be reflected here!
        let escapedFile := gexfFile.replace "\n" "" |>.replace "\"" "\\\""
        html := html
          |>.replace "fetch(\"imports.gexf\").then((res) => res.text()).then(render_gexf)" s!"render_gexf(\"{escapedFile}\")"
          |>.replace "<h1>Import Graph</h1>" s!"<h1>Import Graph for {to}</h1>"
        IO.FS.writeFile fp html
     | some ext => try
        _ ← runCmdWithInput "dot" #["-T" ++ ext, "-o", o] (outFiles["dot"]!)
      catch ex =>
        IO.eprintln s!"Error occurred while writing out {fp}."
        IO.eprintln s!"Make sure you have `graphviz` installed and the file is writable."
        throw ex
  return 0
