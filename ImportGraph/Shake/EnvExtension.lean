/-
Copyright (c) 2026 Thomas Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas Murrills
-/
module

public import Lean.ExtraModUses
public import ImportGraph.Lean.EnvExtension

import all Lean.ExtraModUses

/-!
# Shake extension API

This module provides basic API wrappers and boilerplate for shake extensions. This is mainly for
readability, to ensure that the correct aspects of the state are being managed: different
extensions use different parts of the state for specific purposes.

In particular, it provides `withFreshShakeRecords` for running an action after resetting the shake
extension state, allowing for capture of what mod uses that action produced.
-/

open Lean

public section

namespace ImportGraph.Shake

local instance : Ord Name := ⟨Name.quickCmp⟩

deriving instance Repr, Hashable, Ord for IndirectModUse

open ImportGraph Shake

/-- Resets the new entries in the `indirectModUse` extension. Note that the state is never altered
in the course of the file, as it only represents imported entries. Only the entries are gotten/
reset. -/
@[inline] def resetNewIndirectModUses (env : Environment)
    (asyncMode : EnvExtension.AsyncMode := indirectModUseExt.toEnvExtension.asyncMode)
    (asyncDecl : Name := Name.anonymous) :
    Environment :=
  indirectModUseExt.setEntries env [] asyncMode asyncDecl

@[inline] def getNewIndirectModUses (env : Environment)
    (asyncMode : EnvExtension.AsyncMode := indirectModUseExt.toEnvExtension.asyncMode) :
    List IndirectModUse :=
  indirectModUseExt.getEntries env asyncMode

@[inline] def setNewIndirectModUses (env : Environment) (entries : List IndirectModUse)
    (asyncMode : EnvExtension.AsyncMode := indirectModUseExt.toEnvExtension.asyncMode)
    (asyncDecl : Name := Name.anonymous) :
    Environment :=
  indirectModUseExt.setEntries env entries asyncMode asyncDecl

/-- Gets and resets the new indirect mod uses recorded in the `indirectModUse` extension. Note that
the state per se is never altered in the course of the file, as it only represents imported
entries. Only the entries list is gotten/reset. -/
def getResetNewIndirectModUses (env : Environment)
    (asyncMode : EnvExtension.AsyncMode := indirectModUseExt.toEnvExtension.asyncMode)
    (asyncDecl : Name := Name.anonymous) :
    List IndirectModUse × Environment :=
  letI indirect := indirectModUseExt.getEntries env asyncMode
  (indirect, resetNewIndirectModUses env asyncMode asyncDecl)

/-- A wrapper for `extraModUses.toEnvExtension.asyncMode` to allow it to appear as an `optParam` in
a public-facing type. -/
@[inline] def extraModUsesAsyncMode := extraModUses.toEnvExtension.asyncMode

@[inline] def resetNewExtraModUses (env : Environment) :
    Environment :=
  PersistentEnvExtension.setState extraModUses env ([], {})

@[inline] def getNewExtraModUses (env : Environment)
    (asyncMode : EnvExtension.AsyncMode := extraModUsesAsyncMode)
    (asyncDecl : Name := Name.anonymous) :
    List ExtraModUse × PHashSet ExtraModUse :=
  PersistentEnvExtension.getState extraModUses env asyncMode asyncDecl

@[inline] def setNewExtraModUses (env : Environment)
    (entries : List ExtraModUse)
    (state : PHashSet ExtraModUse) :
    Environment :=
  PersistentEnvExtension.setState extraModUses env (entries, state)

/-- Gets and resets the new extra mod uses in the `extraModUses` extension. Note that the state
does not include imported entries. -/
def getResetExtraModUses (env : Environment)
    (asyncMode : EnvExtension.AsyncMode := extraModUsesAsyncMode)
    (asyncDecl : Name := Name.anonymous) :
    (List ExtraModUse × PHashSet ExtraModUse) × Environment :=
  (getNewExtraModUses env asyncMode asyncDecl, resetNewExtraModUses env)

/-- A wrapper for `isExtraRevModUseExt.toEnvExtension.asyncMode` to allow it to appear as an
`optParam` in a public-facing type. -/
@[inline] def isExtraRevModUseExtAsyncMode := isExtraRevModUseExt.toEnvExtension.asyncMode

/-- Gets the state of the `extraModUses` extension. -/
@[inline] def getNewExtraRevModUse (env : Environment) : Bool :=
  !(isExtraRevModUseExt.getEntries env |>.isEmpty)

/-- Resets the state of the `extraModUses` extension. -/
@[inline] def resetNewExtraRevModUse (env : Environment)
    (asyncMode : EnvExtension.AsyncMode := isExtraRevModUseExtAsyncMode)
    (asyncDecl : Name := Name.anonymous) :
    Environment :=
  if getNewExtraRevModUse env then
    isExtraRevModUseExt.setEntries env [] asyncMode asyncDecl else env

/-- Resets the state of the `extraModUses` extension. -/
@[inline] def setNewExtraRevModUse (env : Environment) (isRev : Bool)
    (asyncMode : EnvExtension.AsyncMode := isExtraRevModUseExtAsyncMode)
    (asyncDecl : Name := Name.anonymous) :
    Environment :=
  if getNewExtraRevModUse env == isRev then env else
    isExtraRevModUseExt.setEntries env (if isRev then [()] else []) asyncMode asyncDecl

/-- Merges the state of the `extraModUses` extension (using "or" semantics). -/
@[inline] def mergeNewExtraRevModUse (env : Environment) (old : Bool)
    (asyncMode : EnvExtension.AsyncMode := isExtraRevModUseExtAsyncMode)
    (asyncDecl : Name := Name.anonymous) :
    Environment :=
  if old then setNewExtraRevModUse env old asyncMode asyncDecl else env

/-- Gets and resets the state of the `extraModUses` extension. -/
def getResetNewExtraRevModUse (env : Environment)
    (asyncMode : EnvExtension.AsyncMode := isExtraRevModUseExtAsyncMode)
    (asyncDecl : Name := Name.anonymous) :
    Bool × Environment :=
  if isExtraRevModUseExt.getEntries env |>.isEmpty then
    (false, env)
  else
    (true, isExtraRevModUseExt.setEntries env [] asyncMode asyncDecl)

/-- Erases any new shake records from the current module. -/
def resetShakeRecords (env : Environment) (asyncMode : EnvExtension.AsyncMode := .sync)
    (asyncDecl : Name := Name.anonymous) : Environment :=
  letI env := resetNewIndirectModUses env asyncMode asyncDecl
  letI env := resetNewExtraModUses env
  resetNewExtraRevModUse env asyncMode asyncDecl

/-- Essentially `(as ++ bs).deleteDuplicatesRev`, keeping later-occurring elements. -/
private def List.prependWithoutDuplicating [BEq α] (as bs : List α) : List α :=
  match as with
  | [] => bs
  | a :: as =>
    let new := List.prependWithoutDuplicating as bs
    if new.contains a then new else a :: new

/-- Iterates through the first set, inserting elements into the second set unless they exist
already. -/
private def Lean.PHashSet.union {α} [BEq α] [Hashable α] (as bs : PHashSet α) :
    PHashSet α := Id.run do
  let mut bs := bs
  for a in as do
    unless bs.contains a do
      bs := bs.insert a
  return bs

open EnvExtension

/-- Copies new extra mod uses from `src` and adds them to `dest`. Does not erase extra mod uses
already in `dest`. The same as `Lean.copyExtraModUses`, but passes async modes. -/
def copyExtraModUses (src dest : Environment)
    (srcAsyncMode := extraModUsesAsyncMode)
    (destAsyncMode := extraModUsesAsyncMode) (destAsyncDecl := Name.anonymous) :
    Environment := Id.run do
  let mut env := dest
  for entry in extraModUses.getEntries src srcAsyncMode do
    if !(extraModUses.getState env destAsyncMode destAsyncDecl).contains entry then
      env := extraModUses.addEntry env entry destAsyncMode destAsyncDecl
  env

/-- Copies new indirect mod uses from `src` and adds them to `dest`. Does not erase extra mod uses
already in `dest`. -/
def copyIndirectModUses (src dest : Environment)
    (srcAsyncMode := indirectModUseExt.toEnvExtension.asyncMode)
    (destAsyncMode := indirectModUseExt.toEnvExtension.asyncMode)
    (destAsyncDecl := Name.anonymous) :
    Environment := Id.run do
  let mut dest := dest
  for i in indirectModUseExt.getEntries src srcAsyncMode do
    dest := indirectModUseExt.addEntry dest i destAsyncMode destAsyncDecl
  return dest

/-- Copies a new rev mod use from `src` and to `dest`, preserving the one in `dest` if present. -/
def copyExtraRevModUse (src dest : Environment)
    (srcAsyncMode := isExtraRevModUseExtAsyncMode)
    (destAsyncMode := isExtraRevModUseExtAsyncMode) (destAsyncDecl := Name.anonymous) :
    Environment :=
  if (isExtraRevModUseExt.getEntries src (asyncMode := srcAsyncMode)).isEmpty ||
    (isExtraRevModUseExt.getEntries src destAsyncMode ).isEmpty
  then dest else isExtraRevModUseExt.addEntry dest () destAsyncMode destAsyncDecl

-- Note: the asyncmodes of all these extensions are `.sync`.
/-- Copies all new shake records from `src` to `dest`. Does not erase the entries in `dest`. -/
@[inline] def copyShakeRecords (src dest : Environment)
    (srcAsyncMode := AsyncMode.sync)
    (destAsyncMode := AsyncMode.sync)
    (destAsyncDecl := Name.anonymous) : Environment :=
  copyExtraModUses src dest
    (srcAsyncMode  := srcAsyncMode)
    (destAsyncMode := destAsyncMode)
    (destAsyncDecl := destAsyncDecl)
  |> copyIndirectModUses src
    (srcAsyncMode  := srcAsyncMode)
    (destAsyncMode := destAsyncMode)
    (destAsyncDecl := destAsyncDecl)
  |> copyExtraRevModUse src
    (srcAsyncMode  := srcAsyncMode)
    (destAsyncMode := destAsyncMode)
    (destAsyncDecl := destAsyncDecl)

/-- Resets the shake extension entries (the records from the current module), then restores them
after running the given action, merging any new records into the new ones. -/
def withFreshShakeRecords [Monad m] [MonadEnv m] [MonadFinally m] {α} (x : m α) : m α := do
  let oldEnv ← getEnv
  modifyEnv resetShakeRecords
  try x finally modifyEnv fun newEnv => copyShakeRecords oldEnv newEnv
