module

public import ImportGraphTest.FindHome.ComponentHome1
public import ImportGraphTest.FindHome.ComponentHome2

public def foo := true

def x' := bar₁.xor bar₂ && foo
-- Takes us to right below `foo` :)
public def somethingElse := 0
