type t = 
  [ `a [@as "x"]
  | `b ]


let revData = [%raw {| {"x":"a","b":"b"} |}]  
let data = [%raw {| {"a":"x","b":"b"} |}]  
external get : 'a -> 'b -> 'c = "" [@@bs.get_index]

let tToJs (x : t ) : string = 
  get data x 

let vFromJsOpt (s : string) : t option = 
  get revData s 

let vFromJs  (s : string) : t = 
  get revData s   
