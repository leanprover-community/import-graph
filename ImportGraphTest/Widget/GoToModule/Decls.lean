module

public section
--     | ← cursor lands (just before) here (in one test)
example : True := trivial

namespace GoToModule

def foo := true

example : True := trivial

def bar := false

example : True := trivial
