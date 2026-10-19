/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public meta import ImportGraph.Tools.FindHome
public meta import ImportGraph.Tools.ImportDiff
public meta import ImportGraph.Tools.MinImports
public meta import ImportGraph.Tools.NormImports
public meta import ImportGraph.Tools.RedundantImports

/-!
# Tools for analyzing imports.

The `#find_home` and `#norm_imports` frontends publicly meta-import ordinary implementation modules,
whose backend imports are private. A meta import alone still re-exports theorems; keeping backend
imports private avoids exporting those proofs while retaining the commands. Widget registrations remain public so that
their persistent environment extensions are available when rendering command output.

Provides the commands

* `#redundant_imports` which lists any transitively redundant imports in the current module.
* `#min_imports` which attempts to construct a minimal set of imports for the declarations
  in the current file.
  (Must be run at the end of the file. Tactics and macros may result in incorrect output.)
* `#find_home decl` suggests files higher up the import hierarchy to which `decl` could be moved.
* `#import_diff foo bar ...` computes the new transitive imports that are added to a given file when
  modules `foo, bar, ...` are added to the set of imports of the file.

-/
