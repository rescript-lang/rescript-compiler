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
    
let print_decl decl =
  match decl with
  | Decl_type (tvars, desc) ->
      let tvars = print_tvars tvars in
      let type_str = print_type desc.decl_type in
      "type " ^ desc.decl_name.id ^ tvars ^ " = " ^ type_str
  | Decl_val desc ->
      let type_ = print_type desc.decl_type in
      "declare export var " ^ desc.decl_name.id ^ ": " ^ type_ ^ ";"

let print prog =
  let decls = List.map print_decl prog.prog_decls in
  "// @flow\n\n" ^
  (String.concat "\n\n" decls) ^
  "\n"
