/-
Copyright (c) 2026 Thomas R. Murrills. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison, Paul Lezeau, Thomas R. Murrills
-/
module
public import Lean.Widget.UserWidget
public meta import Lean.Widget.UserWidget
public section
open Lean Server
namespace ImportGraph.Widget

open Server in
/-- Tries to resolve the module `modName` to a source file URI.
This has to be done in the Lean server
since the `Environment` does not keep track of source URIs. -/
@[server_rpc_method]
public meta def getModuleUri (modName : Name) : RequestM (RequestTask Lsp.DocumentUri) :=
  RequestM.asTask do
    let some uri ← documentUriFromModule? modName
      | throw <| RequestError.invalidParams s!"Couldn't find URI for module `{modName}`"
    return uri

/-- A clickable link to the source of a module, at the supplied position. -/
@[widget_module]
def GoToModule : Widget.Module where javascript := r#"
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
          const uri = await rs.call('ImportGraph.Widget.getModuleUri', props.modName)
          ec.revealPosition({ uri, line: props.pos.line, character: props.pos.character })
        } catch {}
      }
    },
    props.overrideText || props.modName)
}
"#

end ImportGraph.Widget
