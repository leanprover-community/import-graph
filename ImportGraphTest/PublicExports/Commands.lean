module
public import ImportGraphTest.PublicExports.Facade
meta import ImportGraphTest.PublicExports.Check

-- Use the commands through an additional re-export boundary, without importing their internals.
#guard_msgs (drop info, drop warning) in
#redundant_imports

#guard_msgs (drop info, drop warning) in
#min_imports

#guard_msgs (drop info, drop warning) in
#import_diff

#guard_msgs (drop info, drop warning) in
#norm_imports

#guard_msgs (drop info, drop warning) in
#find_home for
def exportBoundaryExample : Nat := Nat.succ 0

-- Legacy syntax is still registered and gives its normal module-system diagnostic.
/--
error: `#find_home <ident>` does not work in the module system. Instead, use
```
#find_home for
<command>
```
where `<command>` declares `<ident>`. This ensures that the imports necessary for the syntax and tactics used in the declaration are present too.
-/
#guard_msgs in
#find_home exportBoundaryExample

run_cmd ImportGraphTest.PublicExports.assertNoProofLeaks #["importGraph"]
