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
def Lean.Environment.importSizes (env : Environment) (verbose : Bool := false) :
    CoreM (NameMap NameSet × NameMap Nat × NameMap Nat) := do
  let modules := (env.header.moduleNames.zip env.header.moduleData)
  let mut importSizes : NameMap Nat := .empty
  let mut localSizes : NameMap Nat := .empty
  let importMap := env.importGraph.transitiveClosure
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

  pure (importMap, localSizes, importSizes)

def Lean.Environment.hoardFactor (env : Environment) (verbose : Bool := false) : CoreM Float := do
  -- TODO: improve the standard library so the next line can be `env.constants.keys`
  let decls := (env.constants.map₁.toArray.append env.constants.map₂.toArray).map (·.fst)
  if verbose then
    println!"Preprocessing approximately {decls.size} declarations."
  let ⟨N, c2m⟩ ← env.transitivelyRequiredModulesForDecls' decls verbose
  if verbose then
    println!"Declarations preprocessed. Preprocessing modules."
  let (transitiveImports, localSizes, importSizes) ← env.importSizes verbose

  let mut sum : Nat := 0
  let mut tot : Nat := 0
  let mut iteration : Nat := 0

  if verbose then
    println!"Entering main loop."
  for (decl, requiredModules) in c2m do
    if ← decl.isBlackListed then continue

    iteration := iteration + 1
    if verbose && iteration % 1000 == 0 then
      println!"{decl}: {sum.toFloat / tot.toFloat}"
    match env.getModuleFor? decl with
    | none => panic!"Could not determine module for {decl}"
    | some module => do
      tot := tot + importSizes.find! module
      let transImports := transitiveImports.findD module ∅
      for (n, i) in env.header.moduleNames.zipWithIndex do
        if !requiredModules.getLsbD i
            && transImports.contains n then do
          sum := sum + localSizes.find! n
  if verbose then
    println!"Processed {iteration} declarations."

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
    env.hoardFactor (verbose := args.hasFlag "verbose")

  println!"Hoard factor: {factor}"
  return 0

open Cli

/-- Setting up command line options and help text for `lake exe hoard_factor`. -/
def hoard_factor : Cmd := `[Cli|
  hoard_factor VIA hoardFactorCLI; ["0.0.1"]
  "Compute a measure of unused declarations per module."

  FLAGS:
    "verbose";            "Output more information during computations."

  ARGS:
    ...targets : String;  "Module name(s) to analyze. \
      Default value is `Mathlib`."
]

/-- `lake exe hoard_factor` -/
def main (args : List String) : IO UInt32 :=
  hoard_factor.validate args
