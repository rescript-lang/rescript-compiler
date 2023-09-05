@val("console.log")
/** we should also allow js function call from an external js module 
    
*/
external log: 'a => unit = "?ignore"

let v = u => {
  log(u)
  u
}

@val("t") external test_f: module(Set.OrderedType) => unit = "?update_dummy"

let v = u => test_f(module(String))

module type X = module type of String

let u = (v: module(X)) => v

let s = u(module(String))
