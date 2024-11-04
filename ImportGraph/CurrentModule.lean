/-
Copyright (c) 2023 Jon Eugster. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jon Eugster
-/
import Lake.Load.Manifest

open Lean (Name)

namespace ImportGraph

/-- Read the name of the main module from the `lake-manifest`. -/
def getCurrentModule : IO Name := do

  match (← Lake.Manifest.load? ⟨"lake-manifest.json"⟩) with
  | none =>
    -- TODO: should this be caught?
    pure .anonymous
  | some manifest =>
    -- TODO: This assumes that the `package` and the default `lean_lib`
    -- have the same name up to capitalisation.
    -- Would be better to read the `.defaultTargets` from the
    -- `← getRootPackage` from `Lake`, but I can't make that work with the monads involved.
    return manifest.name.capitalize

/--
Helper which only returns `true` if the `module` is provided and the name `n` lies
inside it.
 -/
def isInModule (module : Option Name) (n : Name) := match module with
  | some m => m.isPrefixOf n
  | none => false
