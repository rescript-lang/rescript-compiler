module Language =
  struct let toString _ = "a"
         let name _ = 2 end
let language = "a"
let (shortName,name) = language |. Language.(toString, name)