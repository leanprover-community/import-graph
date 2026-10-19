module
public import Lean
public section
namespace ImportGraphTest.PublicExports
open Lean Elab Command Meta

/-- Inspect every materialized, named public proof declaration, including generated theorems. -/
def proofLeaks (packages : Array String) : CommandElabM (Array Name) := liftTermElabM do
  let env ← getEnv
  let pub := env.setExporting true
  let mut leaks := #[]
  for (n, ci) in env.constants do
    if isPrivateName n || (pub.find? n).isNone then continue
    let some idx := env.getModuleIdxFor? n | continue
    let some pkg := env.getModulePackageByIdx? idx | continue
    unless packages.contains pkg do continue
    if ← isProp ci.type then leaks := leaks.push n
  return leaks.qsort (fun a b => a.toString < b.toString)

def assertNoProofLeaks (packages : Array String) : CommandElabM Unit := do
  let leaks ← proofLeaks packages
  unless leaks.isEmpty do
    throwError m!"Public proof declarations from restricted packages ({leaks.size}):\n{MessageData.joinSep (leaks.toList.map MessageData.ofConstName) m!"\n"}"
end ImportGraphTest.PublicExports
