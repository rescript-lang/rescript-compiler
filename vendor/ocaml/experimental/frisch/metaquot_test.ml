let loc1 = Location.in_file "111"
let loc2 = Location.in_file "222"

let x = [%expr foobar]
let pat = [%pat? _ as x]

let e = [%expr fun (x, [%p pat]) -> x + [%e x] + 1]
let () = Format.printf "%a@." (Printast.expression 0) e

;;[@@metaloc loc2]

let e = [%expr fun (x, [%p pat]) -> x + [%e x] + 1] [@metaloc loc1]
let () = Format.printf "%a@." (Printast.expression 0) e

let e = [%expr fun (x, [%p pat]) -> x + [%e x] + 1]
let () = Format.printf "%a@." (Printast.expression 0) e


let mytype = [%type: int list]
let s = [%str type t = A of [%t mytype] | B of string]
let () = Format.printf "%a@." Printast.implementation s


let f = function
  | ([%expr [%e? x] + 1]
    | [%expr 1 + [%e? x]]) as e0 -> [%expr succ [%e x]] [@metaloc e0.pexp_loc]
  | e -> e
