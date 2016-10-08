open Flow_tree

module SSet = Set.Make(String)
module SMap = Map.Make(String)

type state = {
  env: Env.t;
  mutable used_types: string list;
  mutable types: decl list;
  mutable exports: decl list;
}

let print_id s id = Ident.name id

let rec print_path s path =
  let path = Env.normalize_path None s.env path in
  match path with
  | Path.Pident id -> print_id s id
  | Path.Pdot (p, str, _) -> print_path s p ^ "__" ^ str
  | Path.Papply _ -> "Dunno_what_Papply_is"
  
let rec p_sigs s sigs = List.iter (p_sig s) sigs
  
and p_sig s = function
  | Types.Sig_value (id, val_desc) ->
    let decl_name = Ident.name id in
    let decl_type = p_type_expr s val_desc.val_type in
    s.exports <- {decl_name; decl_type} :: s.exports
  | Sig_type _ -> () (* Types are declared on demand *)
  | Sig_typext _ -> ()
  | Sig_module _ -> () (* Non-toplevel values are not exposed *)
  | Sig_modtype _ -> ()
  | Sig_class _ -> ()
  | Sig_class_type _ -> ()

and p_type_decl s type_decl =
  match type_decl.Types.type_kind with
  | Type_abstract ->
    begin match type_decl.type_manifest with
    | Some expr -> p_type_expr s expr
    | None -> p_any "abstract w/o manifest"
    end
  | Type_record _ -> p_any "Type_record"
  | Type_variant _ -> p_any "Type_variant"
  | Type_open -> p_any "Type_open"

and p_type_expr s type_expr =
  match type_expr.desc with
  | Tvar str ->
    p_any ("Tvar " ^ (string_of_int type_expr.id))
  | Tarrow (label, left, right, c) -> p_arrow s (label, left, right, c)
  | Ttuple tl -> T_tuple (List.map (p_type_expr s) tl)
  | Tconstr (path, tl, _) ->
    let name = Path.name path in
    begin match name, tl with
    | "unit", _ -> p_named "void"
    | "string", _ -> p_named "string"
    | "int", _ | "float", _ -> p_named "number"
    | "bool", _ -> p_any "bool"
    | "array", _ -> p_named ~tl:[p_type_expr s (List.hd tl)] "Array"
    | "Js.t", [{desc = Tobject (t, _); _}] ->
      p_js_obj s t
    | _ ->
      let decl_name = print_path s path in
      if not (List.mem decl_name s.used_types) then begin
        s.used_types <- decl_name :: s.used_types;
        let decl_type = p_type_decl s (Env.find_type path s.env) in
        let decl = {decl_name; decl_type} in
        s.types <- decl :: s.types
      end;
      p_named ~tl:(List.map (p_type_expr s) tl) decl_name
    end
  | Tobject _ -> p_any "Tobject"
  | Tfield _ -> p_any "Tfield" (* Shouldn't ever happen? *)
  | Tnil -> p_any "Tnil"
  | Tlink t -> p_type_expr s t
  | Tsubst t -> (* TODO: what is this? *) p_type_expr s t
  | Tvariant _ -> p_any "Tvariant"
  | Tunivar _ -> p_any "Tunivar"
  | Tpoly (t, tl) -> p_type_expr s t
  | Tpackage _ -> p_any "Tpackage"

and p_arrow s t =
  let rec collect (label, left, right, c) = match right.Types.desc with
    | Tarrow (label, l, r, c) ->
      let (params, result) = collect (label, l, r, c) in
      (left :: params, result)
    | Tlink right -> collect (label, left, right, c)
    | _ -> ([left], right)
  in
  let (params, ret) = collect t in
  let params = List.map (p_type_expr s) params in
  T_fun (params, p_type_expr s ret)
  
and p_js_obj s t =
  let rec loop t acc =
    match t.Types.desc with
    | Tfield (field_name, _, left, right) ->
      {field_name; field_type=(p_type_expr s left)} :: (loop right acc)
    | Tnil -> acc
    | _ -> acc
  in
  let fields = loop t [] in
  T_obj fields

and p_any comment = p_named ~comment "any"

and p_named ?(tl=[]) ?comment name = T_name (name, tl, comment)

let mk_type_name name used_names =
  let is_unique name = not (SSet.mem name used_names) in
  let rec loop i =
    let new_name = name ^ "$" ^ (string_of_int i) in
    if is_unique new_name then new_name else loop (i + 1)
  in
  if is_unique name then name else loop 0

let get_type_map types exports =
  let used_names = List.fold_left (fun names e ->
    print_endline ("add export " ^ e.decl_name);
    SSet.add e.decl_name names
  ) SSet.empty exports in
  let (_, type_map) = List.fold_left (fun (used_names, type_map) t ->
    print_endline ("add type " ^ t.decl_name);
    let name = mk_type_name t.decl_name used_names in
    let type_names = SSet.add name used_names in
    let type_map = SMap.add t.decl_name name type_map in
    (type_names, type_map)
  ) (used_names, SMap.empty) types in
  SMap.iter (fun k v -> print_endline (k ^ " -> " ^ v)) type_map;
  type_map

let rename_types prog type_map =
  let get_name name =
    if SMap.mem name type_map then SMap.find name type_map else name
  in
  let rec rename t =
    match t with
    | T_name (name, tl, comment) ->
      let name = get_name name in
      let tl = List.map rename tl in
      T_name (name, tl, comment)
    | T_fun (args, ret) -> T_fun (List.map rename args, rename ret)
    | T_obj fields ->
      let map f = {
        field_name = f.field_name;
        field_type = rename f.field_type;
      } in
      T_obj (List.map map fields)
    | T_tuple tl -> T_tuple (List.map rename tl)
  in
  let prog_types = List.map (fun d ->
    {decl_name = get_name d.decl_name; decl_type = rename d.decl_type}
  ) prog.prog_types in
  let prog_exports = List.map (fun d ->
    {d with decl_type = rename d.decl_type}
  ) prog.prog_exports in
  {prog_types; prog_exports}

let print_signature env sigs =
  let s = {env; used_types = []; types = []; exports = []} in
  List.iter (p_sig s) sigs;
  let prog = {
    prog_types = List.rev s.types;
    prog_exports = List.rev s.exports;
  } in
  let type_map = get_type_map s.types s.exports in
  let prog = rename_types prog type_map in
  Flow_print.print prog
