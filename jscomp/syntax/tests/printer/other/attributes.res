@attr(: int)
let x = 1

@attr(?var)
let x = 1

@attr(?var when x === 1)
let x = 1

%ext(: let x: int)
%ext(:
  let x: int
  let y: float
) 

%%ext("A"; "B")

@@superPrivate(
  @module("./logo.svg") external logo: string = "default"
)

@inlinePrivate(
  @module("./logo.svg") external logo: string = "default"
)
let x = 1

#b(@this (t => unit), @this ((x,t) => unit))

#b(@this (t => unit))

#b((t => unit))
