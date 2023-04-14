type nameGen = (string, int) Hashtbl.t

let parens xs = "(" ^ (xs |> String.concat ", ") ^ ")"
let comment x = "/* " ^ x ^ " */"

let genericsString ~typeVars =
  match typeVars == [] with
  | true -> ""
  | false -> "<" ^ String.concat "," typeVars ^ ">"

let quotes x = "\"" ^ x ^ "\""

let fieldAccess ~label value = value ^ "." ^ label
