module
public import ImportGraph

-- The explicit full-library import retains the existing widget and backend APIs.
#guard_msgs (drop info) in
#check ImportGraph.Shake.NeedsKind.andThen_assoc
#guard_msgs (drop info) in
#check ImportGraph.Widget.GoToModuleProps
#guard_msgs (drop info) in
#check ImportGraph.Widget.RelativeRangeLocation
#guard_msgs (drop info) in
#check ImportGraph.Widget.goToModuleOfDecls
#guard_msgs (drop info) in
#synth Lean.Server.RpcEncodable ImportGraph.Widget.GoToModuleProps
