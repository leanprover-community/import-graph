-- Deliberately omit `module`: ordinary imports load private dependencies too.
import ImportGraph.Tools

open Lean in
run_cmd do
  let env ← getEnv
  for (name, info) in env.constants do
    if (`Lake).isPrefixOf name && (info matches .axiomInfo _) then
      throwError "Unexpected Lake axiom after importing import-graph tools: {name}"
  if (env.find? `ImportGraph.Lake.IO.getWorkspace).isSome then
    throwError "Lake workspace loader leaked through import-graph tools"
