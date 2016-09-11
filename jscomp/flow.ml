type state = {
  env: Env.t;
  mutable used_types: string list;
  mutable before: string list;
}

let print_id s id = Ident.name id

let rec print_path s path =
  let path = Env.normalize_path None s.env path in
  match path with
  | Path.Pident id -> print_id s id
  | Path.Pdot (p, str, _) -> print_path s p ^ "__" ^ str
  | Path.Papply _ -> "Dunno_what_Papply_is"

let rec p_sig s (sig_ : Types.signature) =
  let lines = List.map (p_sig_item s) sig_ in
  let lines = List.filter (fun l -> l <> "") lines in
  String.concat "\n\n" lines

and p_sig_item s = function
  | Sig_value (id, val_desc) ->
    let name = Ident.name id in
    let desc = p_type_expr s val_desc.val_type in
    "declare export var " ^ name ^ ": " ^ desc ^ ";"
  | Sig_type _ -> "" (* Types are declared on demand *)
  | Sig_typext _ -> ""
  | Sig_module _ -> "" (* Non-toplevel values are not exposed *)
  | Sig_modtype _ -> ""
  | Sig_class _ -> ""
  | Sig_class_type _ -> ""

and p_type_decl s type_decl =
  match type_decl.Types.type_kind with
  | Type_abstract ->
    begin match type_decl.type_manifest with
    | Some expr -> p_type_expr s expr
    | None -> "/* abstract w/o manifest */ mixed"
    end
  | Type_record _ -> p_any "Type_record"
  | Type_variant _ -> p_any "Type_variant"
  | Type_open -> p_any "Type_open"

and p_type_expr s type_expr =
  match type_expr.desc with
  | Tvar str ->
    let str = match str with
      | Some x -> x
      | None -> "-" in
    p_any ("Tvar " ^ str)
  | Tarrow (label, left, right, c) -> p_arrow s (label, left, right, c)
  | Ttuple tl -> "[" ^ (String.concat ", " (List.map (p_type_expr s) tl)) ^ "]"
  | Tconstr (path, tl, _) ->
    let name = Path.name path in
    begin match name, tl with
    | "unit", _ -> "void"
    | "string", _ -> "string"
    | "int", _ | "float", _ -> "number"
    | "bool", _ -> "any"
    | "array", _ -> "Array<" ^ (p_type_expr s (List.hd tl)) ^ ">"
    | "Js.t", [{desc = Tobject (t, _); _}] ->
      p_js_obj s t
    | _ ->
      let path_ = print_path s path in
      if not (List.mem path_ s.used_types) then begin
        s.used_types <- path_ :: s.used_types;
        let t_def = p_type_decl s (Env.find_type path s.env) in
        s.before <- ("type " ^ path_ ^ " = " ^ t_def ^ ";") :: s.before
      end;
      path_
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
  let (params, result) = collect t in
  let params = List.mapi (fun i p ->
    "p" ^ (string_of_int i) ^ ": " ^ (p_type_expr s p)
  ) params in
  "(" ^ (String.concat ", " params) ^ ") => " ^ (p_type_expr s result)
  
and p_js_obj s t =
  let rec loop t acc =
    match t.Types.desc with
    | Tfield (name, _, left, right) ->
      (name ^ ": " ^ (p_type_expr s left)) :: (loop right acc)
    | Tnil -> acc
    | _ -> acc
  in
  let fields = loop t [] in
  "{" ^ (String.concat ", " fields) ^ "}"

and p_any comment = "/* " ^ comment ^ " */any"

let print_signature env sigs =
  let s = {env; used_types = []; before = []} in
  let src = p_sig s sigs in
  let before = String.concat "" (List.map (fun x -> x ^ "\n\n") s.before) in
  "// @flow\n\n" ^ before ^ src
