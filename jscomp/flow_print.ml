open Flow_tree

let rec print_type = function
  | T_name (name, tl, comment) ->
    let comment = match comment with
      | Some comment -> "/* " ^ comment ^ " */"
      | None -> ""
    in
    let args = match List.map print_type tl with
      | [] -> ""
      | tl -> "<" ^ (String.concat ", " tl) ^ ">"
    in
    comment ^ name ^ args
  | T_fun (args, ret) ->
    let args_str = args
      |> List.mapi (fun i t -> "p" ^ (string_of_int i) ^ ": " ^ (print_type t))
      |> String.concat ", "
    in
    let ret = print_type ret in
    "(" ^ args_str ^ ") => " ^ ret
  | T_obj fields ->
    let fields_str = fields
      |> List.map (fun f -> f.field_name ^ ": " ^ (print_type f.field_type))
      |> String.concat ", "
    in
    "{" ^ fields_str ^ "}"
  | T_tuple types ->
    let types_str = types |> List.map print_type |> String.concat ", " in
    "[" ^ types_str ^ "]"
    
let print_type_decl decl =
  let type_str = print_type decl.decl_type in
  "type " ^ decl.decl_name ^ " = " ^ type_str

let print_decl decl =
  let type_ = print_type decl.decl_type in
  "declare export var " ^ decl.decl_name ^ ": " ^ type_ ^ ";"

let print prog =
  let types = List.map print_type_decl prog.prog_types in
  let decls = List.map print_decl prog.prog_exports in
  "// @flow\n\n" ^
  (String.concat "\n\n" types) ^
  "\n\n" ^
  (String.concat "\n\n" decls) ^
  "\n"
