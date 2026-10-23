-- Deliberately omit `module`: ordinary imports load private dependencies too.
import ImportGraph
import ImportGraph.Tools

open Lean in
run_cmd do
  let env ← getEnv
  let modules := env.header.moduleNames
  for moduleName in [`Lake.Config.Workspace, `Lake.Load.Workspace, `Lake.Config.Kinds,
      `Lake.Build.Data, `Lake.Build.Facets, `Lake.Build.Infos, `Lake.DSL.Targets,
      `ImportGraph.Lake, `ImportGraph.WorkspaceModel.Summary.Lake,
      `ImportGraph.WorkspaceModel.Emit] do
    if modules.contains moduleName then
      throwError "Unexpected workspace/build import: {moduleName}"
  unless modules.contains `ImportGraph.WorkspaceModel.Summary.Cache &&
      (env.find? `ImportGraph.Lake.WorkspaceSummary.isUpToDate).isSome do
    throwError "Workspace summary cache validation was not imported"
  for (name, info) in env.constants do
    if (`Lake).isPrefixOf name && (info matches .axiomInfo _) then
      throwError "Unexpected Lake axiom after importing import-graph tools: {name}"
  if (env.find? `ImportGraph.Lake.IO.getWorkspace).isSome then
    throwError "Lake workspace loader leaked through import-graph tools"
