
let u =
#if defined B then
    B.value
#elif defined C then
    C.value
#else 
  assert false
#end
