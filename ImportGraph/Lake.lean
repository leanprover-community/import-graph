/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Thomas R. Murrills
-/
module

public import Lake.Config.Workspace

import Lake.Load.Workspace

/-
# Extra `Lake` utilities

This file defines a few convenient lake utilities, notably:

- `for ... in ... do` instances that allow iterating through lake modules:
  - Given `dir : System.FilePath`, `for (mod, dirEntry) in dir.modules (root := ...) do` iterates
    in modules under the directory `dir` (descending into subdirectories)
  - Given `glob : Lake.Glob`, `for (mod, dirEntry) in glob.modulesIn dir do` iterates through all
    modules in `dir` matching `glob`.
- `ImportGraph.IO.getWorkspace : IO Lake.Workspace`: a version of
  `Lake.loadWorkspace : LoggerIO Lake.Workspace` for use in `IO` which constructs the IO
  environment variables and finds the lean installation.
-/

open Lean Lake

public section

namespace ImportGraph

namespace System.FilePath

/-- Allows iteration over the modules (any `*.lean` file) under `dir` via
`for (mod, dirEntry) in mods do`. Usually invoked via `for (mod, dirEntry) in dir.modules do`; see
`ImportGraph.System.FilePath.modules` for details. -/
structure Modules where
  dir : System.FilePath
  root : Name := .anonymous

/-- Allows iteration over the modules (any `*.lean` file) under `dir` via
`for (mod, dirEntry) in dir.modules do`, descending into subdirectories (top-down, left-to-right),
and constructing the module name according to the directory names and `*.lean` filenames.

For example, if `dir` contains `A/B/C.lean`, this iteration visits `(A.B.C, ⟨"A/B", "C.lean"⟩)`.

If `root` is provided, then `root` is prepended to the module names created from paths in the
directory, e.g. if `dir` contains `A/B/C.lean` and we iterate through
``for (mod, dirEntry) in dir.modules (root := `Foo)``, this iteration visits
`(Foo.A.B.C, ⟨"A/B", "C.lean"⟩)`.

By default, no root is inferred. -/
@[inline] def modules (dir : System.FilePath) (root := Name.anonymous) : Modules := { dir, root }

@[specialize]
partial def Modules.forInAux [Monad m] [MonadLiftT IO m] {β}
    (dir : System.FilePath) (pre : Name) (b : β)
    (f : Name × IO.FS.DirEntry → β → m (ForInStep β)) : m (ForInStep β) := do
  let mut b := b
  for entry in ← dir.readDir do
    if (← liftM (m := IO) <| entry.path.isDir) then
      match (← Modules.forInAux entry.path (.str pre entry.fileName) b f) with
      | ForInStep.yield b' => b := b'
      | ForInStep.done b'  => return ForInStep.done b'
    else if entry.path.extension.isEqSome "lean" then
      let mod := .str pre <| (System.FilePath.withExtension entry.fileName "").toString
      match (← f (mod, entry) b) with
      | ForInStep.yield b' => b := b'
      | ForInStep.done b'  => return ForInStep.done b'
  return ForInStep.yield b

instance [Monad m] [MonadLiftT IO m] : ForIn m Modules (Name × IO.FS.DirEntry) where
  forIn spec init f := ForInStep.value <$> Modules.forInAux spec.dir spec.root init f

/--
Splits `path` into the `DirEntry` for its enclosing directory and file name, as `readDir` would
report it. `path.parent` (rather than `path.withFileName ""`) is used for `root` so that it carries
no trailing separator and so that `root / fileName` round-trips back to `path` (matching the entries
produced when iterating through a directory).
-/
def toDirEntry (path : System.FilePath) : IO.FS.DirEntry where
  root := path.parent.getD ""
  fileName := path.fileName.getD ""

end System.FilePath

open ImportGraph

namespace Lake

/-! ## Missing instances -/

deriving instance Hashable for SemVerCore
deriving instance Hashable for StdVer
deriving instance Hashable for LeanVer
deriving instance Hashable for Date
deriving instance Hashable for ToolchainVer

deriving instance ToJson, FromJson for Lake.Glob

/-! ## `for ... in` instances -/

/-- Allows iteration over the matched modules via `for (mod, dirEntry) in globMods do`. This is
typically constructed via `glob.modulesIn dir`. -/
structure Glob.Modules where
  glob : Glob
  dir : System.FilePath

/-- Allows iteration over the modules matched by a `Lake.Glob` in `dir` via
`for (mod, dirEntry) in glob.modulesIn dir do`. -/
@[inline] def Glob.modulesIn (dir : System.FilePath) (glob : Glob) : Glob.Modules :=
  { glob, dir }

/--
Iterates over the module names selected by `glob`, resolving submodule globs against the `.lean`
files found under `dir`.

Auxiliary to the `ForIn` instance for `Glob.Modules`.
-/
@[specialize]
def Glob.Modules.forIn [Monad m] [MonadLiftT IO m] {β}
    (spec : Glob.Modules) (init : β)
    (f : Name × IO.FS.DirEntry → β → m (ForInStep β)) : m β := do
  match spec.glob with
  | .one n =>
    -- Like Lake's `Glob.forEachModuleIn`, which yields `n` unconditionally: we must not require
    -- `n`'s source file to exist, so build its `DirEntry` without touching the filesystem.
    let modFile := modToFilePath spec.dir n "lean"
    return (← f (n, modFile.toDirEntry) init).value
  | .submodules n =>
    let modDir := modToFilePath spec.dir n ""
    -- `ForIn.forIn`, not the `forIn` being defined here (which the local name would shadow).
    ForIn.forIn (modDir.modules (root := n)) init f
  | .andSubmodules n =>
    let modFile := modToFilePath spec.dir n "lean"
    match ← f (n, modFile.toDirEntry) init with
    | ForInStep.done b => return b
    | ForInStep.yield b =>
      let modDir := modToFilePath spec.dir n ""
      ForIn.forIn (modDir.modules (root := n)) b f

instance [Monad m] [MonadLiftT IO m] : ForIn m Glob.Modules (Name × IO.FS.DirEntry) where
  forIn := Glob.Modules.forIn

/-! ## Misc. -/

/-- Gets the toolchain version from a `Lake.Workspace`. -/
def Workspace.getToolchainVer (ws : Lake.Workspace) : IO ToolchainVer := do
  let some ver ← ToolchainVer.ofDir? ws.dir
    | throw (.userError s!"Could not find toolchain file in {ws.dir}")
  return ver

namespace IO

/-- Loads the lake workspace from the current directory (or, if specified, from `wsDir?`) in `IO`.

Note that in the language server, the current working directory is the workspace root, so this may
use the current working directory of elaboration. However, it may not itself be called directly
during elaboration, as this causes the language server to crash. Therefore, for use "in" the
language server, it must be called across a process boundary via an exe. -/
public def getWorkspace (wsDir? : Option System.FilePath := none) : IO Workspace := do
  let wsDir ← wsDir?.getDM IO.currentDir
  let (elan?, lean?, lake?) ← findInstall?
  let some lean := lean?
    | throw (.userError "error: no Lean installation found")
  let lake := lake?.getD (.ofLean lean)
  let lakeEnv ← (Env.compute lake lean elan?).toIO (IO.userError ·)
  let (ws?, log) ← (Lake.loadWorkspace { lakeEnv, wsDir }).run?
  if log.any (·.level matches .error) then
    throw <| .userError
      s!"error: Errors were produced while loading the Lake workspace at {wsDir}.\n\
        Log:\n{log}"
  let some ws := ws?
    | throw <| .userError s!"error: Failed to load the Lake workspace at {wsDir}.\n\
        Log:\n{log}"
  return ws

end ImportGraph.Lake.IO
