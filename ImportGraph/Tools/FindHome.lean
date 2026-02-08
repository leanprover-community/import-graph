/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau
-/
module

public meta import Lean.Elab.Command
public meta import Lean.Widget.UserWidget
public meta import ImportGraph.Imports.RequiredModules
public meta import ImportGraph.Imports.ImportGraph
public meta import ImportGraph.Graph.TransitiveClosure

public meta section

open Lean

/--
Find locations as high as possible in the import hierarchy
where the named declaration could live.
-/
def Lean.Name.findHome (n : Name) (env : Option Environment) : CoreM NameSet := do
  let current? := match env with | some env => env.header.mainModule | _ => default
  let required := (← n.requiredModules).toArray.erase current?
  let imports := (← getEnv).importGraph.transitiveClosure
  let mut candidates : NameSet := {}
  for (n, i) in imports do
    if required.all fun r => n == r || i.contains r then
      candidates := candidates.insert n
  for c in candidates do
    for i in candidates do
      if imports.find? i |>.getD {} |>.contains c then
        candidates := candidates.erase i
  return candidates

open Server in
/-- Tries to resolve the module `modName` to a source file URI.
This has to be done in the Lean server
since the `Environment` does not keep track of source URIs. -/
@[server_rpc_method]
def getModuleUri (modName : Name) : RequestM (RequestTask Lsp.DocumentUri) :=
  RequestM.asTask do
    let some uri ← documentUriFromModule? modName
      | throw $ RequestError.invalidParams s!"couldn't find URI for module '{modName}'"
    return uri

structure GoToModuleLinkProps where
  modName : Name
  deriving Server.RpcEncodable

/-- When clicked, this widget component jumps to the source of the module `modName`,
assuming a source URI can be found for the module. -/
@[widget_module]
def GoToModuleLink : Widget.Module where
  javascript := "
    import * as React from 'react'
    import { EditorContext, useRpcSession } from '@leanprover/infoview'

    export default function(props) {
      const ec = React.useContext(EditorContext)
      const rs = useRpcSession()
      return React.createElement('a',
        {
          className: 'link pointer dim',
          onClick: async () => {
            try {
              const uri = await rs.call('getModuleUri', props.modName)
              ec.revealPosition({ uri, line: 0, character: 0 })
            } catch {}
          }
        },
        props.modName)
    }
  "

open Elab Command in
/--
Find locations as high as possible in the import hierarchy
where the named declaration could live.
Using `#find_home!` will forcefully remove the current file.
Note that this works best if used in a file with `import Mathlib`.

The current file could still be the only suggestion, even using `#find_home! lemma`.
The reason is that `#find_home!` scans the import graph below the current file,
selects all the files containing declarations appearing in `lemma`, excluding
the current file itself and looks for all least upper bounds of such files.

For a simple example, if `lemma` is in a file importing only `A.lean` and `B.lean` and
uses one lemma from each, then `#find_home! lemma` returns the current file.
-/
elab "#find_home" bang:"!"? n:ident : command => do
  let stx ← getRef
  let mut homes : Array MessageData := #[]
  let n ← liftCoreM <| realizeGlobalConstNoOverloadWithInfo n
  let env? ← bang.mapM fun _ => getEnv
  for modName in (← Elab.Command.liftCoreM do n.findHome env?) do
    let p : GoToModuleLinkProps := { modName }
    homes := homes.push $ .ofWidget
      (← liftCoreM $ Widget.WidgetInstance.ofHash
        GoToModuleLink.javascriptHash $
        Server.RpcEncodable.rpcEncode p)
      (toString modName)
  logInfoAt stx[0] m!"{homes}"
