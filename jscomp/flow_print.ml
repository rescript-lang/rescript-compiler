open Flow_tree

let print_tvar id = "TVAR_" ^ (string_of_int id)

let print_tvars = function
  | [] -> ""
  | ids -> "<" ^ (String.concat ", " (List.map print_tvar ids)) ^ ">"

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
    comment ^ name.id ^ args
  | T_tvar id -> print_tvar id
  | T_bool -> "0 | 1"
  | T_fun {func_args; func_ret; func_tvars} ->
    let args_str = func_args
      |> List.mapi (fun i t -> "p" ^ (string_of_int i) ^ ": " ^ (print_type t))
      |> String.concat ", "
    in
    let ret = print_type func_ret in
    let tvars = print_tvars func_tvars in
    tvars ^ "(" ^ args_str ^ ") => " ^ ret
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
  let tvars = print_tvars decl.decl_tvars in
  let type_str = print_type decl.decl_type in
  "type " ^ decl.decl_name.id ^ tvars ^ " = " ^ type_str

let print_decl decl =
  let type_ = print_type decl.decl_type in
  "declare export var " ^ decl.decl_name.id ^ ": " ^ type_ ^ ";"

let print prog =
  let types = List.map print_type_decl prog.prog_types in
  let decls = List.map print_decl prog.prog_exports in
  "// @flow\n\n" ^
  (String.concat "\n\n" types) ^
  "\n\n" ^
  (String.concat "\n\n" decls) ^
  "\n"
