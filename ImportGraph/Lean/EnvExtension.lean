module

public import Lean.EnvExtension

/-! # Extra utilities for environment extensions -/

public section

open Lean

namespace Lean.SimplePersistentEnvExtension

/-- Modifies the `List α` of entries of a `SimplePersistentEnvExtension`. -/
def modifyEntries {α σ} (env : Environment)
    (ext : SimplePersistentEnvExtension α σ) (f : List α → List α)
    (asyncMode : EnvExtension.AsyncMode := ext.toEnvExtension.asyncMode)
    (asyncDecl : Name := Name.anonymous) : Environment :=
  PersistentEnvExtension.modifyState ext env (fun (entries, s) => (f entries, s))
    asyncMode asyncDecl

/-- Sets the `List α` of entries of a `SimplePersistentEnvExtension`. -/
def setEntries {α σ} (env : Environment)
    (ext : SimplePersistentEnvExtension α σ) (entries : List α)
    (asyncMode : EnvExtension.AsyncMode := ext.toEnvExtension.asyncMode)
    (asyncDecl : Name := Name.anonymous) : Environment :=
  PersistentEnvExtension.modifyState ext env (fun (_, s) => (entries, s))
    asyncMode asyncDecl

end Lean.SimplePersistentEnvExtension
