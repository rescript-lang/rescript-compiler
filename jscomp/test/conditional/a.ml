
let u =
#if B #then
    B.value
#elif C #then
    C.value
#else 
  assert false
#end
