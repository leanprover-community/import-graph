/-
Copyright (c) 2024 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Anne Baanen, Johan Commelin
-/
import Cli.Basic
import ImportGraph.Imports

/-!
# `lake exe hoard_factor`

Computes a measure of unused declarations per module.
-/

open Lean Core System

-- Copied from `UnusedTransitiveImports` with modifications to allow running from another Lean package.
def Core.withImportModules (modules : Array Name) {α} (f : CoreM α) : IO α := do
  searchPathRef.set (← addSearchPathFromEnv compile_time_search_path%)
  unsafe Lean.withImportModules (modules.map (fun m => {module := m})) {} (trustLevel := 1024)
    fun env => Prod.fst <$> Core.CoreM.toIO
        (ctx := { fileName := "<CoreM>", fileMap := default }) (s := { env := env }) do f

def Lean.Environment.getModuleInfo? (env : Environment) (module : Name) : Option ModuleData := do
  let idx ← env.getModuleIdx? module
  env.header.moduleData.get? idx

def Lean.Name.isBlackListed {m} [Monad m] [MonadEnv m] (declName : Name) : m Bool := do
  if declName == ``sorryAx then return true
  if declName matches .str _ "inj" then return true
  if declName matches .str _ "noConfusionType" then return true
  let env ← getEnv
  pure <| declName.isInternalDetail
   || isAuxRecursor env declName
   || isNoConfusion env declName
  <||> isRec declName -- <||> isMatcher declName

/-- Compute the local size and transitive size of each module's declarations. -/
def Lean.Environment.importSizes (env : Environment) (importMap : NameMap NameSet)
    (verbose : Bool := false) :
    CoreM (NameMap Nat × NameMap Nat) := do
  let modules := (env.header.moduleNames.zip env.header.moduleData)
  let mut importSizes : NameMap Nat := .empty
  let mut localSizes : NameMap Nat := .empty

  let mut iteration := 0

  for (name, module) in modules do
    let names := module.constNames
    let localSize := names.size
    localSizes := localSizes.insert name localSize

    -- Note that the `getD` assigns a size of `0` to all `Init` files.
    let importSize : Nat := ((importMap.find? name).getD ∅).fold (init := 0)
      (fun size i => size + localSizes.find! i)
    importSizes := importSizes.insert name importSize

    iteration := iteration + 1
    if verbose && iteration % 100 == 0 then
      println!"Processed imports for {name}"

  pure (localSizes, importSizes)

def Lean.Environment.hoardFactor (env : Environment) (decls : Array Name)
    (excludeLean : Bool := true) (excludeMeta : Bool := false) (verbose : Bool := false) : CoreM Float := do
  if verbose then
    println!"Constructing the import graph."
  let mut decls := decls
  let mut graph := env.importGraph
  if excludeLean || excludeMeta then
    let filterLean : Name → Bool := fun n => excludeLean && (
      Name.isPrefixOf `Lean n ∨
      Name.isPrefixOf `Init n ∨
      Name.isPrefixOf `Std n)
    let filterMeta : Name → Bool := fun n => excludeMeta && (
      Name.isPrefixOf `Batteries.CodeAction n ∨
      Name.isPrefixOf `Batteries.Tactic n ∨
      Name.isPrefixOf `Mathlib.Tactic n ∨
      Name.isPrefixOf `Mathlib.Lean n ∨
      Name.isPrefixOf `Mathlib.Mathport n ∨
      Name.isPrefixOf `Mathlib.Util n)
    graph := graph.filterMap fun n i =>
      if filterLean n || filterMeta n then
        -- not included
        none
      else
        -- include node regularly
        i.filter fun m => !(filterLean m || filterMeta m)
    if verbose then
      println!"Found {graph.size} modules."
    decls := decls.filter fun n => match env.getModuleFor? n with
      | some mod => !(filterLean mod || filterMeta mod)
      | none => true
  if verbose then
    println!"Found {decls.size} declarations and {graph.size} modules."
  let transitiveImports := graph.transitiveClosure
  let (localSizes, importSizes) ← env.importSizes transitiveImports verbose

  if verbose then
    println!"Preprocessing approximately {decls.size} declarations."
  let ⟨N, c2m⟩ ← env.transitivelyRequiredModulesForDecls' decls verbose
  if verbose then
    println!"Declarations preprocessed. Preprocessing modules."

  let mut sum : Nat := 0
  let mut tot : Nat := 0
  let mut iteration : Nat := 0

  if verbose then
    println!"Entering main loop."
  for decl in decls do
    if ← decl.isBlackListed then continue
    let requiredModules := c2m.find! decl

    iteration := iteration + 1
    if verbose && iteration % 100 == 0 then
      println!"{decl}: {sum.toFloat / tot.toFloat}"
    match env.getModuleFor? decl with
    | none => panic!"Could not determine module for {decl}"
    | some module => do
      tot := tot + importSizes.findD module 0
      let transImports := transitiveImports.findD module ∅
      for (n, i) in env.header.moduleNames.zipWithIndex do
        if !requiredModules.getLsbD i
            && transImports.contains n then do
          sum := sum + localSizes.find! n
  if verbose then
    println!"Processed {iteration} declarations."

  return sum.toFloat / tot.toFloat

def Lean.Environment.moduleHoardFactor (env : Environment) (modules : Array Name)
    (excludeLean : Bool := true) (excludeMeta : Bool := false) (verbose : Bool := false) : CoreM Float := do
  if verbose then
    println!"Constructing the import graph."
  let mut modules := modules
  let mut graph := env.importGraph
  if excludeLean || excludeMeta then
    let filterLean : Name → Bool := fun n => excludeLean && (
      Name.isPrefixOf `Lean n ∨
      Name.isPrefixOf `Init n ∨
      Name.isPrefixOf `Std n)
    let filterMeta : Name → Bool := fun n => excludeMeta && (
      Name.isPrefixOf `Batteries.CodeAction n ∨
      Name.isPrefixOf `Batteries.Tactic n ∨
      Name.isPrefixOf `Mathlib.Tactic n ∨
      Name.isPrefixOf `Mathlib.Lean n ∨
      Name.isPrefixOf `Mathlib.Mathport n ∨
      Name.isPrefixOf `Mathlib.Util n)
    graph := graph.filterMap fun n i =>
      if filterLean n || filterMeta n then
        -- not included
        none
      else
        -- include node regularly
        i.filter fun m => !(filterLean m || filterMeta m)
  if verbose then
    println!"Found {graph.size} modules."
  let transitiveImports := graph.transitiveClosure
  let (localSizes, importSizes) ← env.importSizes transitiveImports verbose

  let requiredModuleMap ← env.transitivelyRequiredModules' (graph.1.toArray.map Sigma.fst).toList verbose
  if verbose then
    println!"Declarations preprocessed. Preprocessing modules."

  let mut sum : Nat := 0
  let mut tot : Nat := 0
  let mut iteration : Nat := 0

  if verbose then
    println!"Entering main loop."
  for (module, requiredModules) in requiredModuleMap do
    iteration := iteration + 1
    if verbose && iteration % 100 == 0 then
      println!"{module}: {sum.toFloat / tot.toFloat}"
    tot := tot + importSizes.findD module 0
    sum := sum + importSizes.findD module 0
    for n in requiredModules do
      sum := sum - localSizes.find! n
  if verbose then
    println!"Processed {iteration} modules."

  return sum.toFloat / tot.toFloat

open IO.FS IO.Process Name in
/-- Implementation of the hoard factor command line program. -/
def hoardFactorCLI (args : Cli.Parsed) : IO UInt32 := do
  -- Default target is `Mathlib`
  let targets : Array Name := match args.variableArgsAs! Cli.ModuleName with
    | #[] => #[`Mathlib]
    | targets => targets

  let factor ← Core.withImportModules targets do
    let env ← getEnv
    if args.hasFlag "module" then
      env.moduleHoardFactor targets (excludeLean := !args.hasFlag "include-lean") (excludeMeta := args.hasFlag "exclude-meta") (verbose := args.hasFlag "verbose")
    else
      let mut decls : Array Name := #[]
      if args.hasFlag "all" then
        -- TODO: improve the standard library so the next line can be `env.constants.keys`
        decls := (env.constants.map₁.toArray.append env.constants.map₂.toArray).map (·.fst)
      else for m in targets do
        let names := env.header.moduleData[(env.header.moduleNames.getIdx? m).getD 0]!.constNames
        decls := decls.append names
      env.hoardFactor decls (excludeLean := !args.hasFlag "include-lean") (excludeMeta := args.hasFlag "exclude-meta") (verbose := args.hasFlag "verbose")

  println!"Hoard factor: {factor}"
  return 0

open Cli

/-- Setting up command line options and help text for `lake exe hoard_factor`. -/
def hoard_factor : Cmd := `[Cli|
  hoard_factor VIA hoardFactorCLI; ["0.0.1"]
  "Compute a measure of unused declarations per module."

  FLAGS:
    "all";                "Compute the factor for all declarations made or imported in the module. \
The default is to compute the factor for the new declarations in the module itself."
    "include-lean";       "Include the core Lean packages: Init, Lean, Std."
    "exclude-meta";       "Exclude tactic-specific modules during the computations."
    "module";             "Analyze dependencies for modules as a whole instead of per declaration."
    "verbose";            "Output more information during computations."

  ARGS:
    ...targets : String;  "Module name(s) to analyze. \
      Default value is `Mathlib`."
]

/-- `lake exe hoard_factor` -/
def main (args : List String) : IO UInt32 :=
  hoard_factor.validate args
