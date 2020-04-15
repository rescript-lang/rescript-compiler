

(**
for objects its size is dynamic
let xx = object ... end

its tag is 248

field 0  is an array
obj[0] = table.methods
field 1 is its id

```
class point ..
```
point is quarduple, produced by 
Caml_internalOO.make_class


*)
[@@@bs.config {
flags = [|"-drawlambda"|]}]
let u = object 
  val x0 = 1 
  val x1 = 2
  val x2 = 3
  val x3 = 4
  val x4 = 5
  val x5 = 6
end