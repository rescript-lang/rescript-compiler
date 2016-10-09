open Flow_tree

module SSet = Set.Make(String)
module SMap = Map.Make(String)

type state = {
  env: Env.t;
  mutable used_types: string list;
  mutable type_ids: id SMap.t;
  mutable val_ids: id SMap.t;
  
  mutable types: decl list;
  mutable exports: decl list;
}

let get_type_id s name =
  if SMap.mem name s.type_ids then SMap.find name s.type_ids
  else begin
    let id = {id = name} in
    s.type_ids <- SMap.add name id s.type_ids;
    id
  end

let get_val_id s name =
  if SMap.mem name s.val_ids then SMap.find name s.val_ids
  else begin
    let id = {id = name} in
    s.val_ids <- SMap.add name id s.val_ids;
    id
  end

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
    let decl_name = get_val_id s (Ident.name id) in
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
    | None -> p_any s "abstract w/o manifest"
    end
  | Type_record _ -> p_any s "Type_record"
  | Type_variant _ -> p_any s "Type_variant"
  | Type_open -> p_any s "Type_open"

and p_type_expr s type_expr =
  match type_expr.desc with
  | Tvar str ->
    p_any s ("Tvar " ^ (string_of_int type_expr.id))
  | Tarrow (label, left, right, c) -> p_arrow s (label, left, right, c)
  | Ttuple tl -> T_tuple (List.map (p_type_expr s) tl)
  | Tconstr (path, tl, _) ->
    let name = Path.name path in
    begin match name, tl with
    | "unit", _ -> p_type_name s "void"
    | "string", _ -> p_type_name s "string"
    | "int", _ | "float", _ -> p_type_name s "number"
    | "bool", _ -> p_any s "bool"
    | "array", _ -> p_type_name ~tl:[p_type_expr s (List.hd tl)] s "Array"
    | "Js.t", [{desc = Tobject (t, _); _}] ->
      p_js_obj s t
    | _ ->
      let type_name = print_path s path in
      if not (List.mem type_name s.used_types) then begin
        s.used_types <- type_name :: s.used_types;
        let decl_name = get_type_id s type_name in
        let decl_type = p_type_decl s (Env.find_type path s.env) in
        let decl = {decl_name; decl_type} in
        s.types <- decl :: s.types
      end;
      p_type_name ~tl:(List.map (p_type_expr s) tl) s type_name
    end
  | Tobject _ -> p_any s "Tobject"
  | Tfield _ -> p_any s "Tfield" (* Shouldn't ever happen? *)
  | Tnil -> p_any s "Tnil"
  | Tlink t -> p_type_expr s t
  | Tsubst t -> (* TODO: what is this? *) p_type_expr s t
  | Tvariant _ -> p_any s "Tvariant"
  | Tunivar _ -> p_any s "Tunivar"
  | Tpoly (t, tl) -> p_type_expr s t
  | Tpackage _ -> p_any s "Tpackage"

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

and p_any s comment = p_type_name ~comment s "any"

and p_type_name ?(tl=[]) ?comment s name =
  T_name (get_type_id s name, tl, comment)

(* In Flow there is one namespace for both types and values. *)
let rename_types s =
  let used_names = ref (
    SMap.fold (fun _ id names -> SSet.add id.id names) s.val_ids SSet.empty
  ) in
  let is_unique name = not (SSet.mem name !used_names) in
  let rec next_unique name i =
    let new_name = name ^ "$" ^ (string_of_int i) in
    if is_unique new_name then new_name else next_unique name (i + 1)
  in
  SMap.iter (fun _ id ->
    let name = id.id in
    if not (is_unique name) then begin
      let name = next_unique name 0 in
      used_names := SSet.add name !used_names;
      id.id <- name
    end
  ) s.type_ids

let print_signature env sigs =
  let s = {
    env;
    used_types = [];
    type_ids = SMap.empty;
    val_ids = SMap.empty;
    types = [];
    exports = [];
  } in
  List.iter (p_sig s) sigs;
  rename_types s;
  let prog = {
    prog_types = List.rev s.types;
    prog_exports = List.rev s.exports;
  } in
  Flow_print.print prog
