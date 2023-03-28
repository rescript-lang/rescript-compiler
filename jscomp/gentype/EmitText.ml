type nameGen = (string, int) Hashtbl.t

let name ~nameGen s =
  match Hashtbl.find nameGen s with
  | n ->
    Hashtbl.replace nameGen s (n + 1);
    s ^ string_of_int (n + 1)
  | exception Not_found ->
    Hashtbl.replace nameGen s 0;
    s

let parens xs = "(" ^ (xs |> String.concat ", ") ^ ")"
let comment x = "/* " ^ x ^ " */"


let genericsString ~typeVars =
  match typeVars == [] with
  | true -> ""
  | false -> "<" ^ String.concat "," typeVars ^ ">"


let newNameGen () = Hashtbl.create 1
let quotes x = "\"" ^ x ^ "\""

let addComment ~comment x = "\n/* " ^ comment ^ " */\n  " ^ x
let arrayAccess ~index value = value ^ "[" ^ string_of_int index ^ "]"
let fieldAccess ~label value = value ^ "." ^ label
