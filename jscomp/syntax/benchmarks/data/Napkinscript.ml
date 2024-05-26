module MiniBuffer : sig
  type t
  val add_char : t -> char -> unit
  val add_string : t -> string -> unit
  val contents : t -> string
  val create : int -> t
  val flush_newline : t -> unit
  val length : t -> int
  val unsafe_get : t -> int -> char
end  = struct
  type t = {
    mutable buffer : bytes;
    mutable position : int;
    mutable length : int;
  }

  let create n =
   let n = if n < 1 then 1 else n in
   let s = (Bytes.create [@doesNotRaise]) n in
   {buffer = s; position = 0; length = n}

  let contents b = Bytes.sub_string b.buffer 0 b.position

  let unsafe_get b ofs =
    Bytes.unsafe_get b.buffer ofs

  let length b = b.position

  (* Can't be called directly, don't add to the interface *)
  let resize_internal b more =
    let len = b.length in
    let new_len = ref len in
    while b.position + more > !new_len do new_len := 2 * !new_len done;
    if !new_len > Sys.max_string_length then begin
      if b.position + more <= Sys.max_string_length
      then new_len := Sys.max_string_length
    end;
    let new_buffer = (Bytes.create [@doesNotRaise]) !new_len in
    (* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
       this tricky function that is slow anyway. *)
    Bytes.blit b.buffer 0 new_buffer 0 b.position [@doesNotRaise];
    b.buffer <- new_buffer;
    b.length <- !new_len

  let add_char b c =
    let pos = b.position in
    if pos >= b.length then resize_internal b 1;
    Bytes.unsafe_set b.buffer pos c;
    b.position <- pos + 1

  let add_string b s =
    let len = String.length s in
    let new_position = b.position + len in
    if new_position > b.length then resize_internal b len;
    Bytes.blit_string s 0 b.buffer b.position len [@doesNotRaise];
    b.position <- new_position

  (* adds newline and trims all preceding whitespace *)
  let flush_newline b =
    let position = ref (b.position) in
    while (Bytes.unsafe_get b.buffer (!position - 1)) = ' ' && !position >= 0 do
      position := !position - 1;
    done;
    b.position <- !position;
    add_char b '\n'
end

module Doc = struct
  type mode = Break | Flat

  type line_style =
    | Classic (* fits? -> replace with space *)
    | Soft (* fits? -> replaced with nothing *)
    | Hard (* always included, forces breaks in parents *)

  type t =
    | Nil
    | Text of string
    | Concat of t list
    | Indent of t
    | IfBreaks of {yes: t; no: t}
    | LineSuffix of t
    | LineBreak of line_style
    | Group of {should_break: bool; doc: t}
    | CustomLayout of t list
    | BreakParent
    (* | Cursor *)

  let nil = Nil
  let line = LineBreak Classic
  let hard_line = LineBreak Hard
  let soft_line = LineBreak Soft
  let text s = Text s
  let concat l = Concat l
  let indent d = Indent d
  let if_breaks t f = IfBreaks {yes = t; no = f}
  let line_suffix d = LineSuffix d
  let group d = Group {should_break = false; doc = d}
  let breakable_group ~force_break d = Group {should_break = force_break; doc = d}
  let custom_layout gs = CustomLayout gs
  let break_parent = BreakParent
  (* let cursor = Cursor *)

  let space = Text " "
  let comma = Text ","
  let dot = Text "."
  let dotdot = Text ".."
  let dotdotdot = Text "..."
  let less_than = Text "<"
  let greater_than = Text ">"
  let lbrace = Text "{"
  let rbrace = Text "}"
  let lparen = Text "("
  let rparen = Text ")"
  let lbracket = Text "["
  let rbracket = Text "]"
  let question = Text "?"
  let tilde = Text "~"
  let equal = Text "="
  let trailing_comma = IfBreaks {yes = comma; no = nil}
  let double_quote = Text "\""

  let propagate_forced_breaks doc =
    let rec walk doc = match doc with
    | Text _ | Nil | LineSuffix _ ->
      (false, doc)
    | BreakParent ->
      (true, Nil)
    | LineBreak Hard ->
      (true, doc)
    | LineBreak (Classic | Soft) ->
      (false, doc)
    | Indent children ->
      let (child_forces_break, new_children) = walk children in
      (child_forces_break, Indent new_children)
    | IfBreaks {yes = true_doc; no = false_doc} ->
      let (false_force_break, false_doc) = walk false_doc in
      if false_force_break then
        let (_, true_doc) = walk true_doc in
        (true, true_doc)
      else
        let force_break, true_doc = walk true_doc in
        (force_break, IfBreaks {yes = true_doc; no = false_doc})
    | Group {should_break = force_break; doc = children} ->
      let (child_forces_break, new_children) = walk children in
      let should_break = force_break || child_forces_break in
      (should_break, Group {should_break; doc = new_children})
    | Concat children ->
      let (force_break, new_children) = List.fold_left (fun (force_break, new_children) child ->
        let (child_forces_break, new_child) = walk child in
        (force_break || child_forces_break, new_child::new_children)
      ) (false, []) children
      in
      (force_break, Concat (List.rev new_children))
    | CustomLayout children ->
      (* When using CustomLayout, we don't want to propagate forced breaks
       * from the children up. By definition it picks the first layout that fits
       * otherwise it takes the last of the list.
       * However we do want to propagate forced breaks in the sublayouts. They
       * might need to be broken. We just don't propagate them any higher here *)
      let children = match walk (Concat children) with
      | (_, Concat children) -> children
      | _ -> assert false
      in
      (false, CustomLayout children)
    in
    let (_, processed_doc) = walk doc in
    processed_doc

  let join ~sep docs =
    let rec loop acc sep docs =
      match docs with
      | [] -> List.rev acc
      | [x] -> List.rev (x::acc)
      | x::xs -> loop (sep::x::acc) sep xs
    in
    Concat(loop [] sep docs)

  let rec fits w doc = match doc with
    | _ when w < 0 -> false
    | [] -> true
    | (_ind, _mode, Text txt)::rest -> fits (w - String.length txt) rest
    | (ind, mode, Indent doc)::rest -> fits w ((ind + 2, mode, doc)::rest)
    | (_ind, Flat, LineBreak break)::rest ->
        if break = Hard then true
        else
          let w = if break = Classic then w - 1 else w in
          fits w rest
    | (_ind, _mode, Nil)::rest -> fits w rest
    | (_ind, Break, LineBreak _break)::_rest -> true
    | (ind, mode, Group {should_break = force_break; doc})::rest ->
      let mode = if force_break then Break else mode in
      fits w ((ind, mode, doc)::rest)
    | (ind, mode, IfBreaks {yes = break_doc; no = flat_doc})::rest ->
        if mode = Break then
          fits w ((ind, mode, break_doc)::rest)
        else
          fits w ((ind, mode, flat_doc)::rest)
    | (ind, mode, Concat docs)::rest ->
      let ops = List.map (fun doc -> (ind, mode, doc)) docs in
      fits w (List.append ops rest)
    (* | (_ind, _mode, Cursor)::rest -> fits w rest *)
    | (_ind, _mode, LineSuffix _)::rest -> fits w rest
    | (_ind, _mode, BreakParent)::rest -> fits w rest
    | (ind, mode, CustomLayout (hd::_))::rest ->
      (* TODO: if we have nested custom layouts, what we should do here? *)
      fits w ((ind, mode, hd)::rest)
    | (_ind, _mode, CustomLayout _)::rest ->
      fits w rest

  let to_string ~width doc =
    let doc = propagate_forced_breaks doc in
    let buffer = MiniBuffer.create 1000 in

    let rec process ~pos line_suffices stack =
      match stack with
      | ((ind, mode, doc) as cmd)::rest ->
        begin match doc with
        | Nil | BreakParent ->
          process ~pos line_suffices rest
        | Text txt ->
          MiniBuffer.add_string buffer txt;
          process ~pos:(String.length txt + pos) line_suffices rest
        | LineSuffix doc ->
          process ~pos ((ind, mode, doc)::line_suffices) rest
        | Concat docs ->
          let ops = List.map (fun doc -> (ind, mode, doc)) docs in
          process ~pos line_suffices (List.append ops rest)
        | Indent doc ->
          process ~pos line_suffices ((ind + 2, mode, doc)::rest)
        | IfBreaks {yes = break_doc; no = flat_doc} ->
          if mode = Break then
            process ~pos line_suffices ((ind, mode, break_doc)::rest)
          else
            process ~pos line_suffices ((ind, mode, flat_doc)::rest)
        | LineBreak line_style  ->
          if mode = Break then (
            begin match line_suffices with
            | [] ->
              MiniBuffer.flush_newline buffer;
              MiniBuffer.add_string buffer (String.make ind ' ' [@doesNotRaise]);
              process ~pos:ind [] rest
            | _docs ->
              process ~pos:ind [] (List.concat [List.rev line_suffices; cmd::rest])
            end
          ) else (* mode = Flat *) (
            let pos = match line_style with
            | Classic -> MiniBuffer.add_string buffer " "; pos + 1
            | Hard -> MiniBuffer.flush_newline buffer; 0
            | Soft -> pos
            in
            process ~pos line_suffices rest
          )
        | Group {should_break; doc} ->
          if should_break || not (fits (width - pos) ((ind, Flat, doc)::rest)) then
            process ~pos line_suffices ((ind, Break, doc)::rest)
          else
            process ~pos line_suffices ((ind, Flat, doc)::rest)
        | CustomLayout docs ->
          let rec find_group_that_fits groups = match groups with
          | [] -> Nil
          | [last_group] -> last_group
          | doc::docs ->
            if (fits (width - pos) ((ind, Flat, doc)::rest)) then
              doc
            else
              find_group_that_fits docs
          in
          let doc = find_group_that_fits docs in
          process ~pos line_suffices ((ind, Flat, doc)::rest)
        end
      | [] ->
        begin match line_suffices with
        | [] -> ()
        | suffices ->
          process ~pos:0 [] (List.rev suffices)
        end
    in
    process ~pos:0 [] [0, Flat, doc];

    let len = MiniBuffer.length buffer in
    if len > 0 && MiniBuffer.unsafe_get buffer (len - 1) != '\n' then
      MiniBuffer.add_char buffer '\n';
    MiniBuffer.contents buffer


  let debug t =
    let rec to_doc = function
      | Nil -> text "nil"
      | BreakParent -> text "breakparent"
      | Text txt -> text ("text(" ^ txt ^ ")")
      | LineSuffix doc -> group(
          concat [
            text "linesuffix(";
            indent (
              concat [line; to_doc doc]
            );
            line;
            text ")"
          ]
        )
      | Concat docs -> group(
          concat [
            text "concat(";
            indent (
              concat [
                line;
                join ~sep:(concat [text ","; line])
                  (List.map to_doc docs) ;
              ]
            );
            line;
            text ")"
          ]
        )
      | CustomLayout docs -> group(
          concat [
            text "customLayout(";
            indent (
              concat [
                line;
                join ~sep:(concat [text ","; line])
                  (List.map to_doc docs) ;
              ]
            );
            line;
            text ")"
          ]
        )
      | Indent doc ->
          concat [
            text "indent(";
            soft_line;
            to_doc doc;
            soft_line;
            text ")";
          ]
      | IfBreaks {yes = true_doc; no = false_doc} ->
        group(
          concat [
            text "ifBreaks(";
            indent (
              concat [
                line;
                to_doc true_doc;
                concat [text ",";  line];
                to_doc false_doc;
              ]
            );
            line;
            text ")"
          ]
        )
      | LineBreak break ->
        let break_txt = match break with
          | Classic -> "Classic"
          | Soft -> "Soft"
          | Hard -> "Hard"
        in
        text ("LineBreak(" ^ break_txt ^ ")")
      | Group {should_break; doc} ->
        group(
          concat [
            text "Group(";
            indent (
              concat [
                line;
                text ("shouldBreak: " ^ (string_of_bool should_break));
                concat [text ",";  line];
                to_doc doc;
              ]
            );
            line;
            text ")"
          ]
        )
    in
    let doc = to_doc t in
    to_string ~width:10 doc |> print_endline
    [@@live]
end

module Sexp: sig
  type t

  val atom: string -> t
  val list: t list -> t
  val to_string: t -> string
end = struct
  type t =
    | Atom of string
    | List of t list

  let atom s = Atom s
  let list l = List l

  let rec to_doc t =
    match t with
    | Atom s -> Doc.text s
    | List [] -> Doc.text "()"
    | List [sexpr] -> Doc.concat [Doc.lparen; to_doc sexpr; Doc.rparen;]
    | List (hd::tail) ->
      Doc.group (
        Doc.concat [
          Doc.lparen;
          to_doc hd;
          Doc.indent (
            Doc.concat [
              Doc.line;
              Doc.join ~sep:Doc.line (List.map to_doc tail);
            ]
          );
          Doc.rparen;
        ]
      )

  let to_string sexpr =
    let doc = to_doc sexpr in
    Doc.to_string ~width:80 doc
end

module SexpAst: sig
  val implementation: Parsetree.structure -> Sexp.t
  val interface: Parsetree.signature -> Sexp.t
end = struct
  open Parsetree

  let map_empty ~f items =
    match items with
    | [] -> [Sexp.list []]
    | items -> List.map f items

  let string txt =
    Sexp.atom ("\"" ^ txt ^ "\"")

  let char c =
    Sexp.atom ("'" ^ (Char.escaped c) ^ "'")

  let opt_char oc =
    match oc with
    | None -> Sexp.atom "None"
    | Some c ->
      Sexp.list [
        Sexp.atom "Some";
        char c
      ]

  let longident l =
    let rec loop l = match l with
    | Longident.Lident ident -> Sexp.list [
        Sexp.atom "Lident";
        string ident;
      ]
    | Longident.Ldot (lident, txt) ->
      Sexp.list [
        Sexp.atom "Ldot";
        loop lident;
        string txt;
      ]
    | Longident.Lapply (l1, l2) ->
      Sexp.list [
        Sexp.atom "Lapply";
        loop l1;
        loop l2;
      ]
    in
    Sexp.list [
      Sexp.atom "longident";
      loop l;
    ]

  let closed_flag flag = match flag with
    | Asttypes.Closed -> Sexp.atom "Closed"
    | Open -> Sexp.atom "Open"

  let direction_flag flag = match flag with
    | Asttypes.Upto -> Sexp.atom "Upto"
    | Downto -> Sexp.atom "Downto"

  let rec_flag flag = match flag with
    | Asttypes.Recursive -> Sexp.atom "Recursive"
    | Nonrecursive -> Sexp.atom "Nonrecursive"

  let override_flag flag = match flag with
    | Asttypes.Override -> Sexp.atom "Override"
    | Fresh -> Sexp.atom "Fresh"

  let private_flag flag = match flag with
    | Asttypes.Public -> Sexp.atom "Public"
    | Private -> Sexp.atom "Private"

  let mutable_flag flag = match flag with
    | Asttypes.Immutable -> Sexp.atom "Immutable"
    | Mutable -> Sexp.atom "Mutable"

   let variance v = match v with
     | Asttypes.Covariant -> Sexp.atom "Covariant"
     | Contravariant -> Sexp.atom "Contravariant"
     | Invariant -> Sexp.atom "Invariant"

  let arg_label lbl = match lbl with
    | Asttypes.Nolabel -> Sexp.atom "Nolabel"
    | Labelled txt -> Sexp.list [
        Sexp.atom "Labelled";
        string txt;
      ]
    | Optional txt -> Sexp.list [
        Sexp.atom "Optional";
        string txt;
      ]

  let constant c =
    let sexpr = match c with
    | Pconst_integer (txt, tag) ->
      Sexp.list [
        Sexp.atom "Pconst_integer";
        string txt;
        opt_char tag;
      ]
    | Pconst_char c ->
      Sexp.list [
        Sexp.atom "Pconst_char";
        Sexp.atom (Char.escaped c);
      ]
    | Pconst_string (txt, tag) ->
      Sexp.list [
        Sexp.atom "Pconst_string";
        string txt;
        match tag with
        | Some txt -> Sexp.list [
            Sexp.atom "Some";
            string txt;
          ]
        | None -> Sexp.atom "None";
      ]
    | Pconst_float (txt, tag)  ->
      Sexp.list [
        Sexp.atom "Pconst_float";
        string txt;
        opt_char tag;
      ]
    in
      Sexp.list [
        Sexp.atom "constant";
        sexpr
      ]

  let rec structure s =
    Sexp.list (
      (Sexp.atom "structure")::(List.map structure_item s)
    )

  and structure_item si =
    let desc = match si.pstr_desc with
    | Pstr_eval (expr, attrs) ->
      Sexp.list [
        Sexp.atom "Pstr_eval";
        expression expr;
        attributes attrs;
      ]
    | Pstr_value (flag, vbs) ->
      Sexp.list [
        Sexp.atom "Pstr_value";
        rec_flag flag;
        Sexp.list (map_empty ~f:value_binding vbs)
      ]
    | Pstr_primitive (vd) ->
      Sexp.list [
        Sexp.atom "Pstr_primitive";
        value_description vd;
      ]
    | Pstr_type (flag, tds) ->
      Sexp.list [
        Sexp.atom "Pstr_type";
        rec_flag flag;
        Sexp.list (map_empty ~f:type_declaration tds)
      ]
    | Pstr_typext typext ->
      Sexp.list [
        Sexp.atom "Pstr_type";
        type_extension typext;
      ]
    | Pstr_exception ec ->
      Sexp.list [
        Sexp.atom "Pstr_exception";
        extension_constructor ec;
      ]
    | Pstr_module mb ->
      Sexp.list [
        Sexp.atom "Pstr_module";
        module_binding mb;
      ]
    | Pstr_recmodule mbs ->
      Sexp.list [
        Sexp.atom "Pstr_recmodule";
        Sexp.list (map_empty ~f:module_binding mbs);
      ]
    | Pstr_modtype mod_typ_decl ->
      Sexp.list [
        Sexp.atom "Pstr_modtype";
        module_type_declaration mod_typ_decl;
      ]
    | Pstr_open open_desc ->
      Sexp.list [
        Sexp.atom "Pstr_open";
        open_description open_desc;
      ]
    | Pstr_class _ -> Sexp.atom "Pstr_class"
    | Pstr_class_type _ -> Sexp.atom "Pstr_class_type"
    | Pstr_include id ->
      Sexp.list [
        Sexp.atom "Pstr_include";
        include_declaration id;
      ]
    | Pstr_attribute attr ->
      Sexp.list [
        Sexp.atom "Pstr_attribute";
        attribute attr;
      ]
    | Pstr_extension (ext, attrs) ->
      Sexp.list [
        Sexp.atom "Pstr_extension";
        extension ext;
        attributes attrs;
      ]
    in
    Sexp.list [
      Sexp.atom "structure_item";
      desc;
    ]

  and include_declaration id =
    Sexp.list [
      Sexp.atom "include_declaration";
      module_expression id.pincl_mod;
      attributes id.pincl_attributes;
    ]

  and open_description od =
    Sexp.list [
      Sexp.atom "open_description";
      longident od.popen_lid.Asttypes.txt;
      attributes od.popen_attributes;
    ]

  and module_type_declaration mtd =
    Sexp.list [
      Sexp.atom "module_type_declaration";
      string mtd.pmtd_name.Asttypes.txt;
      (match mtd.pmtd_type with
      | None -> Sexp.atom "None"
      | Some mod_type -> Sexp.list [
          Sexp.atom "Some";
          module_type mod_type;
      ]);
      attributes mtd.pmtd_attributes;
    ]

  and module_binding mb =
    Sexp.list [
      Sexp.atom "module_binding";
      string mb.pmb_name.Asttypes.txt;
      module_expression mb.pmb_expr;
      attributes mb.pmb_attributes;
    ]

  and module_expression me =
    let desc = match me.pmod_desc with
    | Pmod_ident mod_name ->
      Sexp.list [
        Sexp.atom "Pmod_ident";
        longident mod_name.Asttypes.txt;
      ]
    | Pmod_structure s ->
      Sexp.list [
        Sexp.atom "Pmod_structure";
        structure s;
      ]
    | Pmod_functor (lbl, opt_mod_type, mod_expr) ->
      Sexp.list [
        Sexp.atom "Pmod_functor";
        string lbl.Asttypes.txt;
        (match opt_mod_type with
        | None -> Sexp.atom "None"
        | Some mod_type -> Sexp.list [
            Sexp.atom "Some";
            module_type mod_type;
        ]);
        module_expression mod_expr;
      ]
    | Pmod_apply (call_mod_expr, mod_expr_arg) ->
      Sexp.list [
        Sexp.atom "Pmod_apply";
        module_expression call_mod_expr;
        module_expression mod_expr_arg;
      ]
    | Pmod_constraint (mod_expr, mod_type) ->
      Sexp.list [
        Sexp.atom "Pmod_constraint";
        module_expression mod_expr;
        module_type mod_type;
      ]
    | Pmod_unpack expr ->
      Sexp.list [
        Sexp.atom "Pmod_unpack";
        expression expr;
      ]
    | Pmod_extension ext ->
      Sexp.list [
        Sexp.atom "Pmod_extension";
        extension ext;
      ]
    in
    Sexp.list [
      Sexp.atom "module_expr";
      desc;
      attributes me.pmod_attributes;
    ]

  and module_type mt =
    let desc = match mt.pmty_desc with
    | Pmty_ident longident_loc ->
      Sexp.list [
        Sexp.atom "Pmty_ident";
        longident longident_loc.Asttypes.txt;
      ]
    | Pmty_signature s ->
      Sexp.list [
        Sexp.atom "Pmty_signature";
        signature s;
      ]
    | Pmty_functor (lbl, opt_mod_type, mod_type) ->
      Sexp.list [
        Sexp.atom "Pmty_functor";
        string lbl.Asttypes.txt;
        (match opt_mod_type with
        | None -> Sexp.atom "None"
        | Some mod_type -> Sexp.list [
            Sexp.atom "Some";
            module_type mod_type;
        ]);
        module_type mod_type;
      ]
    | Pmty_alias longident_loc ->
      Sexp.list [
        Sexp.atom "Pmty_alias";
        longident longident_loc.Asttypes.txt;
      ]
    | Pmty_extension ext ->
      Sexp.list [
        Sexp.atom "Pmty_extension";
        extension ext;
      ]
    | Pmty_typeof mod_expr ->
      Sexp.list [
        Sexp.atom "Pmty_typeof";
        module_expression mod_expr;
      ]
    | Pmty_with (mod_type, with_constraints) ->
      Sexp.list [
        Sexp.atom "Pmty_with";
        module_type mod_type;
        Sexp.list (map_empty ~f:with_constraint with_constraints);
      ]
    in
    Sexp.list [
      Sexp.atom "module_type";
      desc;
      attributes mt.pmty_attributes;
    ]

  and with_constraint wc = match wc with
    | Pwith_type (longident_loc, td) ->
      Sexp.list [
        Sexp.atom "Pmty_with";
        longident longident_loc.Asttypes.txt;
        type_declaration td;
      ]
    | Pwith_module (l1, l2) ->
      Sexp.list [
        Sexp.atom "Pwith_module";
        longident l1.Asttypes.txt;
        longident l2.Asttypes.txt;
      ]
    | Pwith_typesubst (longident_loc, td) ->
      Sexp.list [
        Sexp.atom "Pwith_typesubst";
        longident longident_loc.Asttypes.txt;
        type_declaration td;
      ]
    | Pwith_modsubst (l1, l2) ->
      Sexp.list [
        Sexp.atom "Pwith_modsubst";
        longident l1.Asttypes.txt;
        longident l2.Asttypes.txt;
      ]

  and signature s =
    Sexp.list (
      (Sexp.atom "signature")::(List.map signature_item s)
    )

  and signature_item si =
    let descr = match si.psig_desc with
    | Psig_value vd ->
      Sexp.list [
        Sexp.atom "Psig_value";
        value_description vd;
      ]
    | Psig_type (flag, type_declarations) ->
      Sexp.list [
        Sexp.atom "Psig_type";
        rec_flag flag;
        Sexp.list (map_empty ~f:type_declaration type_declarations);
      ]
    | Psig_typext typ_ext ->
      Sexp.list [
        Sexp.atom "Psig_typext";
        type_extension typ_ext;
      ]
    | Psig_exception ext_constr ->
      Sexp.list [
        Sexp.atom "Psig_exception";
        extension_constructor ext_constr;
      ]
    | Psig_module mod_decl ->
      Sexp.list [
        Sexp.atom "Psig_module";
        module_declaration mod_decl;
      ]
    | Psig_recmodule mod_decls ->
      Sexp.list [
        Sexp.atom "Psig_recmodule";
        Sexp.list (map_empty ~f:module_declaration mod_decls);
      ]
    | Psig_modtype mod_typ_decl ->
      Sexp.list [
        Sexp.atom "Psig_modtype";
        module_type_declaration mod_typ_decl;
      ]
    | Psig_open open_desc ->
      Sexp.list [
        Sexp.atom "Psig_open";
        open_description open_desc;
      ]
    | Psig_include incl_decl ->
      Sexp.list [
        Sexp.atom "Psig_include";
        include_description incl_decl
      ]
    | Psig_class _ -> Sexp.list [Sexp.atom "Psig_class";]
    | Psig_class_type _ -> Sexp.list [ Sexp.atom "Psig_class_type"; ]
    | Psig_attribute attr ->
      Sexp.list [
        Sexp.atom "Psig_attribute";
        attribute attr;
      ]
    | Psig_extension (ext, attrs) ->
      Sexp.list [
        Sexp.atom "Psig_extension";
        extension ext;
        attributes attrs;
      ]
    in
    Sexp.list [
      Sexp.atom "signature_item";
      descr;
    ]

  and include_description id =
    Sexp.list [
      Sexp.atom "include_description";
      module_type id.pincl_mod;
      attributes id.pincl_attributes;
    ]

  and module_declaration md =
    Sexp.list [
      Sexp.atom "module_declaration";
      string md.pmd_name.Asttypes.txt;
      module_type md.pmd_type;
      attributes md.pmd_attributes;
    ]

  and value_binding vb =
    Sexp.list [
      Sexp.atom "value_binding";
      pattern vb.pvb_pat;
      expression vb.pvb_expr;
      attributes vb.pvb_attributes;
    ]

  and value_description vd =
    Sexp.list [
      Sexp.atom "value_description";
      string vd.pval_name.Asttypes.txt;
      core_type vd.pval_type;
      Sexp.list (map_empty ~f:string vd.pval_prim);
      attributes vd.pval_attributes;
    ]

  and type_declaration td =
    Sexp.list [
      Sexp.atom "type_declaration";
      string td.ptype_name.Asttypes.txt;
      Sexp.list [
        Sexp.atom "ptype_params";
        Sexp.list (map_empty ~f:(fun (typexpr, var) ->
          Sexp.list [
            core_type typexpr;
            variance var;
          ]) td.ptype_params)
      ];
      Sexp.list [
        Sexp.atom "ptype_cstrs";
        Sexp.list (map_empty ~f:(fun (typ1, typ2, _loc) ->
          Sexp.list [
            core_type typ1;
            core_type typ2;
          ]) td.ptype_cstrs)
      ];
      Sexp.list [
        Sexp.atom "ptype_kind";
        type_kind td.ptype_kind;
      ];
      Sexp.list [
        Sexp.atom "ptype_manifest";
        match td.ptype_manifest with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            core_type typ;
          ]
      ];
      Sexp.list [
        Sexp.atom "ptype_private";
        private_flag td.ptype_private;
      ];
      attributes td.ptype_attributes;
    ]

  and extension_constructor ec =
    Sexp.list [
      Sexp.atom "extension_constructor";
      string ec.pext_name.Asttypes.txt;
      extension_constructor_kind ec.pext_kind;
      attributes ec.pext_attributes;
    ]

  and extension_constructor_kind kind = match kind with
    | Pext_decl (args, opt_typ_expr) ->
      Sexp.list [
        Sexp.atom "Pext_decl";
        constructor_arguments args;
        match opt_typ_expr with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            core_type typ;
          ]
      ]
  | Pext_rebind longident_loc ->
    Sexp.list [
      Sexp.atom "Pext_rebind";
      longident longident_loc.Asttypes.txt;
    ]

  and type_extension te =
    Sexp.list [
      Sexp.atom "type_extension";
      Sexp.list [
        Sexp.atom "ptyext_path";
        longident te.ptyext_path.Asttypes.txt;
      ];
      Sexp.list [
        Sexp.atom "ptyext_parms";
        Sexp.list (map_empty ~f:(fun (typexpr, var) ->
          Sexp.list [
            core_type typexpr;
            variance var;
          ]) te.ptyext_params)
      ];
      Sexp.list [
        Sexp.atom "ptyext_constructors";
        Sexp.list (map_empty ~f:extension_constructor te.ptyext_constructors);
      ];
      Sexp.list [
        Sexp.atom "ptyext_private";
        private_flag te.ptyext_private;
      ];
      attributes te.ptyext_attributes;
    ]

  and type_kind kind = match kind with
    | Ptype_abstract -> Sexp.atom "Ptype_abstract"
    | Ptype_variant constr_decls ->
      Sexp.list [
        Sexp.atom "Ptype_variant";
        Sexp.list (map_empty ~f:constructor_declaration constr_decls);
      ]
    | Ptype_record lbl_decls ->
      Sexp.list [
        Sexp.atom "Ptype_record";
        Sexp.list (map_empty ~f:label_declaration lbl_decls);
      ]
    | Ptype_open -> Sexp.atom "Ptype_open"

  and constructor_declaration cd =
    Sexp.list [
      Sexp.atom "constructor_declaration";
      string cd.pcd_name.Asttypes.txt;
      Sexp.list [
        Sexp.atom "pcd_args";
        constructor_arguments cd.pcd_args;
      ];
      Sexp.list [
        Sexp.atom "pcd_res";
        match cd.pcd_res with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            core_type typ;
          ]
      ];
      attributes cd.pcd_attributes;
    ]

  and constructor_arguments args = match args with
    | Pcstr_tuple types ->
      Sexp.list [
        Sexp.atom "Pcstr_tuple";
        Sexp.list (map_empty ~f:core_type types)
      ]
    | Pcstr_record lds ->
      Sexp.list [
        Sexp.atom "Pcstr_record";
        Sexp.list (map_empty ~f:label_declaration lds)
      ]

  and label_declaration ld =
    Sexp.list [
      Sexp.atom "label_declaration";
      string ld.pld_name.Asttypes.txt;
      mutable_flag ld.pld_mutable;
      core_type ld.pld_type;
      attributes ld.pld_attributes;
    ]

  and expression expr =
    let desc = match expr.pexp_desc with
    | Pexp_ident longident_loc ->
      Sexp.list [
        Sexp.atom "Pexp_ident";
        longident longident_loc.Asttypes.txt;
      ]
    | Pexp_constant c ->
      Sexp.list [
        Sexp.atom "Pexp_constant";
        constant c
      ]
    | Pexp_let (flag, vbs, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_let";
        rec_flag flag;
        Sexp.list (map_empty ~f:value_binding vbs);
        expression expr;
      ]
    | Pexp_function cases ->
      Sexp.list [
        Sexp.atom "Pexp_function";
        Sexp.list (map_empty ~f:case cases);
      ]
    | Pexp_fun (arg_lbl, expr_opt, pat, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_fun";
        arg_label arg_lbl;
        (match expr_opt with
        | None -> Sexp.atom "None"
        | Some expr -> Sexp.list [
            Sexp.atom "Some";
            expression expr;
        ]);
        pattern pat;
        expression expr;
      ]
    | Pexp_apply (expr, args) ->
      Sexp.list [
        Sexp.atom "Pexp_apply";
        expression expr;
        Sexp.list (map_empty ~f:(fun (arg_lbl, expr) -> Sexp.list [
          arg_label arg_lbl;
          expression expr
        ]) args);
      ]
    | Pexp_match (expr, cases) ->
      Sexp.list [
        Sexp.atom "Pexp_match";
        expression expr;
        Sexp.list (map_empty ~f:case cases);
      ]
    | Pexp_try (expr, cases) ->
      Sexp.list [
        Sexp.atom "Pexp_try";
        expression expr;
        Sexp.list (map_empty ~f:case cases);
      ]
    | Pexp_tuple exprs ->
      Sexp.list [
        Sexp.atom "Pexp_tuple";
        Sexp.list (map_empty ~f:expression exprs);
      ]
    | Pexp_construct (longident_loc, expr_opt) ->
      Sexp.list [
        Sexp.atom "Pexp_construct";
        longident longident_loc.Asttypes.txt;
        match expr_opt with
        | None -> Sexp.atom "None"
        | Some expr ->
          Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]
      ]
    | Pexp_variant (lbl, expr_opt) ->
      Sexp.list [
        Sexp.atom "Pexp_variant";
        string lbl;
        match expr_opt with
        | None -> Sexp.atom "None"
        | Some expr ->
          Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]
      ]
    | Pexp_record (rows, opt_expr) ->
      Sexp.list [
        Sexp.atom "Pexp_record";
        Sexp.list (map_empty ~f:(fun (longident_loc, expr) -> Sexp.list [
          longident longident_loc.Asttypes.txt;
          expression expr;
        ]) rows);
        (match opt_expr with
        | None -> Sexp.atom "None"
        | Some expr ->
          Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]);
      ]
    | Pexp_field (expr, longident_loc) ->
      Sexp.list [
        Sexp.atom "Pexp_field";
        expression expr;
        longident longident_loc.Asttypes.txt;
      ]
    | Pexp_setfield (expr1, longident_loc, expr2) ->
      Sexp.list [
        Sexp.atom "Pexp_setfield";
        expression expr1;
        longident longident_loc.Asttypes.txt;
        expression expr2;
      ]
    | Pexp_array exprs ->
      Sexp.list [
        Sexp.atom "Pexp_array";
        Sexp.list (map_empty ~f:expression exprs);
      ]
    | Pexp_ifthenelse (expr1, expr2, opt_expr) ->
      Sexp.list [
        Sexp.atom "Pexp_ifthenelse";
        expression expr1;
        expression expr2;
        (match opt_expr with
        | None -> Sexp.atom "None"
        | Some expr ->
          Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]);
      ]
    | Pexp_sequence (expr1, expr2) ->
      Sexp.list [
        Sexp.atom "Pexp_sequence";
        expression expr1;
        expression expr2;
      ]
    | Pexp_while (expr1, expr2) ->
      Sexp.list [
        Sexp.atom "Pexp_while";
        expression expr1;
        expression expr2;
      ]
    | Pexp_for (pat, e1, e2, flag, e3) ->
      Sexp.list [
        Sexp.atom "Pexp_for";
        pattern pat;
        expression e1;
        expression e2;
        direction_flag flag;
        expression e3;
      ]
    | Pexp_constraint (expr, typexpr) ->
      Sexp.list [
        Sexp.atom "Pexp_constraint";
        expression expr;
        core_type typexpr;
      ]
    | Pexp_coerce (expr, opt_typ, typexpr) ->
      Sexp.list [
        Sexp.atom "Pexp_coerce";
        expression expr;
        (match opt_typ with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            core_type typ;
        ]);
        core_type typexpr;
      ]
    | Pexp_send _ ->
      Sexp.list [
        Sexp.atom "Pexp_send";
      ]
    | Pexp_new _ ->
      Sexp.list [
        Sexp.atom "Pexp_new";
      ]
    | Pexp_setinstvar _ ->
      Sexp.list [
        Sexp.atom "Pexp_setinstvar";
      ]
    | Pexp_override _ ->
      Sexp.list [
        Sexp.atom "Pexp_override";
      ]
    | Pexp_letmodule (mod_name, mod_expr, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_letmodule";
        string mod_name.Asttypes.txt;
        module_expression mod_expr;
        expression expr;
      ]
    | Pexp_letexception (ext_constr, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_letexception";
        extension_constructor ext_constr;
        expression expr;
      ]
    | Pexp_assert expr ->
      Sexp.list [
        Sexp.atom "Pexp_assert";
        expression expr;
      ]
    | Pexp_lazy expr ->
      Sexp.list [
        Sexp.atom "Pexp_lazy";
        expression expr;
      ]
    | Pexp_poly _ ->
      Sexp.list [
        Sexp.atom "Pexp_poly";
      ]
    | Pexp_object _ ->
      Sexp.list [
        Sexp.atom "Pexp_object";
      ]
    | Pexp_newtype (lbl, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_newtype";
        string lbl.Asttypes.txt;
        expression expr;
      ]
    | Pexp_pack mod_expr ->
      Sexp.list [
        Sexp.atom "Pexp_pack";
        module_expression mod_expr;
      ]
    | Pexp_open (flag, longident_loc, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_open";
        override_flag flag;
        longident longident_loc.Asttypes.txt;
        expression expr;
      ]
    | Pexp_extension ext ->
      Sexp.list [
        Sexp.atom "Pexp_extension";
        extension ext;
      ]
    | Pexp_unreachable -> Sexp.atom "Pexp_unreachable"
    in
    Sexp.list [
      Sexp.atom "expression";
      desc;
    ]

  and case c =
    Sexp.list [
      Sexp.atom "case";
      Sexp.list [
        Sexp.atom "pc_lhs";
        pattern c.pc_lhs;
      ];
      Sexp.list [
        Sexp.atom "pc_guard";
        match c.pc_guard with
        | None -> Sexp.atom "None"
        | Some expr -> Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]
      ];
      Sexp.list [
        Sexp.atom "pc_rhs";
        expression c.pc_rhs;
      ]
    ]

  and pattern p =
    let descr = match p.ppat_desc with
    | Ppat_any ->
      Sexp.atom "Ppat_any"
    | Ppat_var var ->
      Sexp.list [
        Sexp.atom "Ppat_var";
        string var.Location.txt;
      ]
    | Ppat_alias (p, alias) ->
      Sexp.list [
        Sexp.atom "Ppat_alias";
        pattern p;
        string alias.txt;
      ]
    | Ppat_constant c ->
      Sexp.list [
        Sexp.atom "Ppat_constant";
        constant c;
      ]
    | Ppat_interval (lo, hi) ->
      Sexp.list [
        Sexp.atom "Ppat_interval";
        constant lo;
        constant hi;
      ]
    | Ppat_tuple (patterns) ->
      Sexp.list [
        Sexp.atom "Ppat_tuple";
        Sexp.list (map_empty ~f:pattern patterns);
      ]
    | Ppat_construct (longident_loc, opt_pattern) ->
      Sexp.list [
        Sexp.atom "Ppat_construct";
        longident longident_loc.Location.txt;
        match opt_pattern with
        | None -> Sexp.atom "None"
        | Some p -> Sexp.list [
            Sexp.atom "some";
            pattern p;
          ]
      ]
    | Ppat_variant (lbl, opt_pattern) ->
      Sexp.list [
        Sexp.atom "Ppat_variant";
        string lbl;
        match opt_pattern with
        | None -> Sexp.atom "None"
        | Some p -> Sexp.list [
            Sexp.atom "Some";
            pattern p;
          ]
      ]
    | Ppat_record (rows, flag) ->
      Sexp.list [
        Sexp.atom "Ppat_record";
        closed_flag flag;
        Sexp.list (map_empty ~f:(fun (longident_loc, p) ->
          Sexp.list [
            longident longident_loc.Location.txt;
            pattern p;
          ]
        ) rows)
      ]
    | Ppat_array patterns ->
      Sexp.list [
        Sexp.atom "Ppat_array";
        Sexp.list (map_empty ~f:pattern patterns);
      ]
    | Ppat_or (p1, p2) ->
      Sexp.list [
        Sexp.atom "Ppat_or";
        pattern p1;
        pattern p2;
      ]
    | Ppat_constraint (p, typexpr) ->
      Sexp.list [
        Sexp.atom "Ppat_constraint";
        pattern p;
        core_type typexpr;
      ]
    | Ppat_type longident_loc ->
      Sexp.list [
        Sexp.atom "Ppat_type";
        longident longident_loc.Location.txt
      ]
    | Ppat_lazy p ->
      Sexp.list [
        Sexp.atom "Ppat_lazy";
        pattern p;
      ]
    | Ppat_unpack string_loc ->
      Sexp.list [
        Sexp.atom "Ppat_unpack";
        string string_loc.Location.txt;
      ]
    | Ppat_exception p ->
      Sexp.list [
        Sexp.atom "Ppat_exception";
        pattern p;
      ]
    | Ppat_extension ext ->
      Sexp.list [
        Sexp.atom "Ppat_extension";
        extension ext;
      ]
    | Ppat_open (longident_loc, p) ->
      Sexp.list [
        Sexp.atom "Ppat_open";
        longident longident_loc.Location.txt;
        pattern p;
      ]
    in
    Sexp.list [
      Sexp.atom "pattern";
      descr;
    ]

  and object_field field = match field with
  | Otag (lbl_loc, attrs, typexpr) ->
    Sexp.list [
      Sexp.atom "Otag";
      string lbl_loc.txt;
      attributes attrs;
      core_type typexpr;
    ]
  | Oinherit typexpr ->
    Sexp.list [
      Sexp.atom "Oinherit";
      core_type typexpr;
    ]

  and row_field field = match field with
    | Rtag (label_loc, attrs, truth, types) ->
      Sexp.list [
        Sexp.atom "Rtag";
        string label_loc.txt;
        attributes attrs;
        Sexp.atom (if truth then "true" else "false");
        Sexp.list (map_empty ~f:core_type types);
      ]
    | Rinherit typexpr ->
      Sexp.list [
        Sexp.atom "Rinherit";
        core_type typexpr;
      ]

  and package_type (mod_name_loc, package_constraints) =
    Sexp.list [
      Sexp.atom "package_type";
      longident mod_name_loc.Asttypes.txt;
      Sexp.list (map_empty ~f:(fun (mod_name_loc, typexpr) ->
        Sexp.list [
          longident mod_name_loc.Asttypes.txt;
          core_type typexpr;
        ]
      ) package_constraints)
    ]

  and core_type typexpr =
    let desc = match typexpr.ptyp_desc with
      | Ptyp_any -> Sexp.atom "Ptyp_any"
      | Ptyp_var var -> Sexp.list [
          Sexp.atom "Ptyp_var";
          string  var
        ]
      | Ptyp_arrow (arg_lbl, typ1, typ2) ->
        Sexp.list [
          Sexp.atom "Ptyp_arrow";
          arg_label arg_lbl;
          core_type typ1;
          core_type typ2;
        ]
      | Ptyp_tuple types ->
        Sexp.list [
          Sexp.atom "Ptyp_tuple";
          Sexp.list (map_empty ~f:core_type types);
        ]
      | Ptyp_constr (longident_loc, types) ->
        Sexp.list [
          Sexp.atom "Ptyp_constr";
          longident longident_loc.txt;
          Sexp.list (map_empty ~f:core_type types);
        ]
      | Ptyp_alias (typexpr, alias) ->
        Sexp.list [
          Sexp.atom "Ptyp_alias";
          core_type typexpr;
          string alias;
        ]
      | Ptyp_object (fields, flag) ->
        Sexp.list [
          Sexp.atom "Ptyp_object";
          closed_flag flag;
          Sexp.list (map_empty ~f:object_field fields)
        ]
      | Ptyp_class (longident_loc, types) ->
        Sexp.list [
          Sexp.atom "Ptyp_class";
          longident longident_loc.Location.txt;
          Sexp.list (map_empty ~f:core_type types)
        ]
      | Ptyp_variant (fields, flag, opt_labels) ->
        Sexp.list [
          Sexp.atom "Ptyp_variant";
          Sexp.list (map_empty ~f:row_field fields);
          closed_flag flag;
          match opt_labels with
          | None -> Sexp.atom "None"
          | Some lbls -> Sexp.list (map_empty ~f:string lbls);
        ]
      | Ptyp_poly (lbls, typexpr) ->
        Sexp.list [
          Sexp.atom "Ptyp_poly";
          Sexp.list (map_empty ~f:(fun lbl -> string lbl.Asttypes.txt) lbls);
          core_type typexpr;
        ]
      | Ptyp_package (package) ->
        Sexp.list [
          Sexp.atom "Ptyp_package";
          package_type package;
        ]
      | Ptyp_extension (ext) ->
        Sexp.list [
          Sexp.atom "Ptyp_extension";
          extension ext;
        ]
    in
    Sexp.list [
      Sexp.atom "core_type";
      desc;
    ]

  and payload p =
    match p with
    | PStr s ->
      Sexp.list (
        (Sexp.atom "PStr")::(map_empty ~f:structure_item s)
      )
    | PSig s ->
      Sexp.list [
        Sexp.atom "PSig";
        signature s;
      ]
    | PTyp ct ->
      Sexp.list [
        Sexp.atom "PTyp";
        core_type ct
      ]
    | PPat (pat, opt_expr) ->
      Sexp.list [
        Sexp.atom "PPat";
        pattern pat;
        match opt_expr with
        | Some expr -> Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]
        | None -> Sexp.atom "None";
      ]

  and attribute (string_loc, p) =
    Sexp.list [
      Sexp.atom "attribute";
      Sexp.atom string_loc.Asttypes.txt;
      payload p;
    ]

  and extension (string_loc, p) =
    Sexp.list [
      Sexp.atom "extension";
      Sexp.atom string_loc.Asttypes.txt;
      payload p;
    ]

  and attributes attrs =
    let sexprs = map_empty ~f:attribute attrs in
    Sexp.list ((Sexp.atom "attributes")::sexprs)

  let implementation = structure
  let interface = signature
end

module IO: sig
  val read_file: string -> string
  val read_stdin: unit -> string
end = struct
  (* random chunk size: 2^15, TODO: why do we guess randomly? *)
  let chunk_size = 32768

  let read_file filename =
    let chan = open_in filename in
    let buffer = Buffer.create chunk_size in
    let chunk = (Bytes.create [@doesNotRaise]) chunk_size in
    let rec loop () =
      let len = try input chan chunk 0 chunk_size with Invalid_argument _ -> 0 in
      if len == 0 then (
        close_in_noerr chan;
        Buffer.contents buffer
      ) else (
        Buffer.add_subbytes buffer chunk 0 len;
        loop ()
      )
    in
    loop ()

  let read_stdin () =
    let buffer = Buffer.create chunk_size in
    let chunk = (Bytes.create [@doesNotRaise]) chunk_size in
    let rec loop () =
      let len = try input stdin chunk 0 chunk_size with Invalid_argument _ -> 0 in
      if len == 0 then (
        close_in_noerr stdin;
        Buffer.contents buffer
      ) else (
        Buffer.add_subbytes buffer chunk 0 len;
        loop ()
      )
    in
    loop ()
end

module CharacterCodes = struct
  let eof = -1

  let space = 0x0020
  let newline = 0x0A (* \n *) [@@live]
  let line_feed = 0x0A (* \n *)
  let carriage_return = 0x0D  (* \r *)
  let line_separator = 0x2028
  let paragraph_separator = 0x2029

  let tab = 0x09

  let bang = 0x21
  let dot = 0x2E
  let colon = 0x3A
  let comma = 0x2C
  let backtick = 0x60
  (* let question = 0x3F *)
  let semicolon = 0x3B
  let underscore = 0x5F
  let single_quote = 0x27
  let double_quote = 0x22
  let equal = 0x3D
  let bar = 0x7C
  let tilde = 0x7E
  let question = 0x3F
  let ampersand = 0x26
  let at = 0x40
  let dollar = 0x24
  let percent = 0x25

  let lparen = 0x28
  let rparen = 0x29
  let lbracket = 0x5B
  let rbracket = 0x5D
  let lbrace = 0x7B
  let rbrace = 0x7D

  let forwardslash = 0x2F (* / *)
  let backslash = 0x5C (* \ *)

  let greater_than = 0x3E
  let hash = 0x23
  let less_than = 0x3C

  let minus = 0x2D
  let plus = 0x2B
  let asterisk = 0x2A

  let _0 = 0x30
  let _1 = 0x31 [@@live]
  let _2 = 0x32 [@@live]
  let _3 = 0x33 [@@live]
  let _4 = 0x34 [@@live]
  let _5 = 0x35 [@@live]
  let _6 = 0x36 [@@live]
  let _7 = 0x37 [@@live]
  let _8 = 0x38 [@@live]
  let _9 = 0x39

  module Lower = struct
    let a = 0x61
    let b = 0x62
    let c = 0x63 [@@live]
    let d = 0x64 [@@live]
    let e = 0x65
    let f = 0x66
    let g = 0x67
    let h = 0x68 [@@live]
    let i = 0x69 [@@live]
    let j = 0x6A [@@live]
    let k = 0x6B [@@live]
    let l = 0x6C [@@live]
    let m = 0x6D [@@live]
    let n = 0x6E
    let o = 0x6F
    let p = 0x70
    let q = 0x71 [@@live]
    let r = 0x72
    let s = 0x73 [@@live]
    let t = 0x74
    let u = 0x75 [@@live]
    let v = 0x76 [@@live]
    let w = 0x77 [@@live]
    let x = 0x78
    let y = 0x79 [@@live]
    let z = 0x7A
  end

  module Upper = struct
    let a = 0x41
    (* let b = 0x42 *)
    let c = 0x43 [@@live]
    let d = 0x44 [@@live]
    let e = 0x45 [@@live]
    let f = 0x46 [@@live]
    let g = 0x47
    let h = 0x48 [@@live]
    let i = 0x49 [@@live]
    let j = 0x4A [@@live]
    let k = 0x4B [@@live]
    let l = 0x4C [@@live]
    let m = 0x4D [@@live]
    let b = 0x4E [@@live]
    let o = 0x4F [@@live]
    let p = 0x50 [@@live]
    let q = 0x51 [@@live]
    let r = 0x52 [@@live]
    let s = 0x53 [@@live]
    let t = 0x54 [@@live]
    let u = 0x55 [@@live]
    let v = 0x56 [@@live]
    let w = 0x57 [@@live]
    let x = 0x58 [@@live]
    let y = 0x59 [@@live]
    let z = 0x5a
  end

  (* returns lower-case ch, ch should be ascii *)
  let lower ch =
    (* if ch >= Lower.a && ch <= Lower.z then ch else ch + 32 *)
    32 lor ch

  let is_letter ch =
    Lower.a <= ch && ch <= Lower.z ||
    Upper.a <= ch && ch <= Upper.z

  let is_upper_case ch =
    Upper.a <= ch && ch <= Upper.z

  let is_digit ch = _0 <= ch && ch <= _9

  let is_hex ch =
    (_0 <= ch && ch <= _9) ||
    (Lower.a <= (lower ch) && (lower ch) <= Lower.f)

    (*
      // ES5 7.3:
      // The ECMAScript line terminator characters are listed in Table 3.
      //     Table 3: Line Terminator Characters
      //     Code Unit Value     Name                    Formal Name
      //     \u000A              Line Feed               <LF>
      //     \u000D              Carriage Return         <CR>
      //     \u2028              Line separator          <LS>
      //     \u2029              Paragraph separator     <PS>
      // Only the characters in Table 3 are treated as line terminators. Other new line or line
      // breaking characters are treated as white space but not as line terminators.
  *)
  let is_line_break ch =
       ch == line_feed
    || ch == carriage_return
    || ch == line_separator
    || ch == paragraph_separator

  let digit_value ch =
    if _0 <= ch && ch <= _9 then
      ch - 48
    else if Lower.a <= (lower ch) && (lower ch) <= Lower.f then
      (lower ch) - Lower.a + 10
    else
      16 (* larger than any legal value *)
end

module Comment: sig
  type t

  val to_string: t -> string

  val loc: t -> Location.t
  val txt: t -> string
  val prev_tok_end_pos: t -> Lexing.position

  val set_prev_tok_end_pos: t -> Lexing.position -> unit

  val is_single_line_comment: t -> bool

  val make_single_line_comment: loc:Location.t -> string -> t
  val make_multi_line_comment: loc:Location.t -> string -> t
  val from_ocaml_comment:
    loc:Location.t -> txt:string -> prev_tok_end_pos:Lexing.position -> t
  val trim_spaces: string -> string
end = struct
  type style =
    | SingleLine
    | MultiLine

  let style_to_string s = match s with
    | SingleLine -> "SingleLine"
    | MultiLine -> "MultiLine"

  type t = {
    txt: string;
    style: style;
    loc: Location.t;
    mutable prev_tok_end_pos: Lexing.position;
  }

  let loc t = t.loc
  let txt t = t.txt
  let prev_tok_end_pos t = t.prev_tok_end_pos

  let set_prev_tok_end_pos t pos =
    t.prev_tok_end_pos <- pos

  let is_single_line_comment t = match t.style with
    | SingleLine -> true
    | MultiLine -> false

  let to_string t =
    Format.sprintf
      "(txt: %s\nstyle: %s\nlines: %d-%d)"
      t.txt
      (style_to_string t.style)
      t.loc.loc_start.pos_lnum
      t.loc.loc_end.pos_lnum

  let make_single_line_comment ~loc txt = {
    txt;
    loc;
    style = SingleLine;
    prev_tok_end_pos = Lexing.dummy_pos;
  }

  let make_multi_line_comment ~loc txt = {
    txt;
    loc;
    style = MultiLine;
    prev_tok_end_pos = Lexing.dummy_pos;
  }

  let from_ocaml_comment ~loc ~txt ~prev_tok_end_pos = {
    txt;
    loc;
    style = MultiLine;
    prev_tok_end_pos = prev_tok_end_pos
  }

  let trim_spaces s =
    let len = String.length s in
    if len = 0 then s
    else if String.unsafe_get s 0 = ' ' || String.unsafe_get s (len - 1) = ' ' then (
      let b = Bytes.of_string s in
      let i = ref 0 in
      while !i < len && (Bytes.unsafe_get b !i) = ' ' do
        incr i
      done;
      let j = ref (len - 1) in
      while !j >= !i && (Bytes.unsafe_get b !j) = ' ' do
        decr j
      done;
      if !j >= !i then
        (Bytes.sub [@doesNotRaise]) b !i (!j - !i + 1) |> Bytes.to_string
      else
        ""
    ) else s
end

module Token = struct
  type t =
    | Open
    | True | False
    | Character of char
    | Int of {i: string; suffix: char option}
    | Float of {f: string; suffix: char option}
    | String of string
    | Lident of string
    | Uident of string
    | As
    | Dot | DotDot | DotDotDot
    | Bang
    | Semicolon
    | Let
    | And
    | Rec
    | Underscore
    | SingleQuote
    | Equal | EqualEqual | EqualEqualEqual
    | Bar
    | Lparen
    | Rparen
    | Lbracket
    | Rbracket
    | Lbrace
    | Rbrace
    | Colon
    | Comma
    | Eof
    | Exception
    | Backslash [@live]
    | Forwardslash | ForwardslashDot
    | Asterisk | AsteriskDot | Exponentiation
    | Minus | MinusDot
    | Plus | PlusDot | PlusPlus | PlusEqual
    | ColonGreaterThan
    | GreaterThan
    | LessThan
    | LessThanSlash
    | Hash | HashEqual | HashHash
    | Assert
    | Lazy
    | Tilde
    | Question
    | If | Else | For | In | To | Downto | While | Switch
    | When
    | EqualGreater | MinusGreater
    | External
    | Typ
    | Private
    | Mutable
    | Constraint
    | Include
    | Module
    | Of
    | With
    | Land | Lor
    | Band (* Bitwise and: & *)
    | BangEqual | BangEqualEqual
    | LessEqual | GreaterEqual
    | ColonEqual
    | At | AtAt
    | Percent | PercentPercent
    | Comment of Comment.t
    | List
    | TemplateTail of string
    | TemplatePart of string
    | Backtick
    | BarGreater
    | Try | Catch
    | Import
    | Export

  let precedence = function
    | HashEqual | ColonEqual -> 1
    | Lor -> 2
    | Land -> 3
    | Equal | EqualEqual | EqualEqualEqual | LessThan | GreaterThan
    | BangEqual | BangEqualEqual | LessEqual | GreaterEqual | BarGreater -> 4
    | Plus | PlusDot | Minus | MinusDot | PlusPlus -> 5
    | Asterisk | AsteriskDot | Forwardslash | ForwardslashDot -> 6
    | Exponentiation -> 7
    | MinusGreater -> 8
    | Dot -> 9
    | _ -> 0

  let to_string = function
    | Open -> "open"
    | True -> "true" | False -> "false"
    | Character c -> "'" ^ (Char.escaped c) ^ "'"
    | String s -> s
    | Lident str -> str
    | Uident str -> str
    | Dot -> "." | DotDot -> ".." | DotDotDot -> "..."
    | Int {i} -> "int " ^ i
    | Float {f} -> "Float: " ^ f
    | Bang -> "!"
    | Semicolon -> ";"
    | Let -> "let"
    | And -> "and"
    | Rec -> "rec"
    | Underscore -> "_"
    | SingleQuote -> "'"
    | Equal -> "=" | EqualEqual -> "==" | EqualEqualEqual -> "==="
    | Eof -> "eof"
    | Bar -> "|"
    | As -> "as"
    | Lparen -> "(" | Rparen -> ")"
    | Lbracket -> "[" | Rbracket -> "]"
    | Lbrace -> "{" | Rbrace -> "}"
    | ColonGreaterThan -> ":>"
    | Colon -> ":"
    | Comma -> ","
    | Minus -> "-" | MinusDot -> "-."
    | Plus -> "+" | PlusDot -> "+." | PlusPlus -> "++" | PlusEqual -> "+="
    | Backslash -> "\\"
    | Forwardslash -> "/" | ForwardslashDot -> "/."
    | Exception -> "exception"
    | Hash -> "#" | HashHash -> "##" | HashEqual -> "#="
    | GreaterThan -> ">"
    | LessThan -> "<"
    | LessThanSlash -> "</"
    | Asterisk -> "*" | AsteriskDot -> "*." | Exponentiation -> "**"
    | Assert -> "assert"
    | Lazy -> "lazy"
    | Tilde -> "tilde"
    | Question -> "?"
    | If -> "if"
    | Else -> "else"
    | For -> "for"
    | In -> "in"
    | To -> "to"
    | Downto -> "downto"
    | While -> "while"
    | Switch -> "switch"
    | When -> "when"
    | EqualGreater -> "=>" | MinusGreater -> "->"
    | External -> "external"
    | Typ -> "type"
    | Private -> "private"
    | Constraint -> "constraint"
    | Mutable -> "mutable"
    | Include -> "include"
    | Module -> "module"
    | Of -> "of"
    | With -> "with"
    | Lor -> "||"
    | Band -> "&" | Land -> "&&"
    | BangEqual -> "!=" | BangEqualEqual -> "!=="
    | GreaterEqual -> ">=" | LessEqual -> "<="
    | ColonEqual -> ":="
    | At -> "@" | AtAt -> "@@"
    | Percent -> "%" | PercentPercent -> "%%"
    | Comment c -> "Comment(" ^ (Comment.to_string c) ^ ")"
    | List -> "list"
    | TemplatePart text -> text ^ "${"
    | TemplateTail text -> "TemplateTail(" ^ text ^ ")"
    | Backtick -> "`"
    | BarGreater -> "|>"
    | Try -> "try" | Catch -> "catch"
    | Import -> "import"
    | Export -> "export"

  let keyword_table = function
  | "true" -> True
  | "false" -> False
  | "open" -> Open
  | "let" -> Let
  | "rec" -> Rec
  | "and" -> And
  | "as" -> As
  | "exception" -> Exception
  | "assert" -> Assert
  | "lazy" -> Lazy
  | "if" -> If
  | "else" -> Else
  | "for" -> For
  | "in" -> In
  | "to" -> To
  | "downto" -> Downto
  | "while" -> While
  | "switch" -> Switch
  | "when" -> When
  | "external" -> External
  | "type" -> Typ
  | "private" -> Private
  | "mutable" -> Mutable
  | "constraint" -> Constraint
  | "include" -> Include
  | "module" -> Module
  | "of" -> Of
  | "list" -> List
  | "with" -> With
  | "try" -> Try
  | "catch" -> Catch
  | "import" -> Import
  | "export" -> Export
  | _ -> raise Not_found
  [@@raises Not_found]

  let is_keyword = function
    | True | False | Open | Let | Rec | And | As
    | Exception | Assert | Lazy | If | Else | For | In | To
    | Downto | While | Switch | When | External | Typ | Private
    | Mutable | Constraint | Include | Module | Of
    | Land | Lor | List | With
    | Try | Catch | Import | Export -> true
    | _ -> false

  let lookup_keyword str =
    try keyword_table str with
    | Not_found ->
      if CharacterCodes.is_upper_case (int_of_char (str.[0] [@doesNotRaise])) then
        Uident str
      else Lident str

  let is_keyword_txt str =
    try let _ = keyword_table str in true with
    | Not_found -> false
end

module Grammar = struct
  type t =
    | OpenDescription (* open Belt *)
    | ModuleLongIdent (* Foo or Foo.Bar *) [@live]
    | Ternary (* condExpr ? trueExpr : falseExpr *)
    | Es6ArrowExpr
    | Jsx
    | JsxAttribute
    | JsxChild [@live]
    | ExprOperand
    | ExprUnary
    | ExprSetField
    | ExprBinaryAfterOp of Token.t
    | ExprBlock
    | ExprCall
    | ExprList
    | ExprArrayAccess
    | ExprArrayMutation
    | ExprIf
    | IfCondition | IfBranch | ElseBranch
    | TypeExpression
    | External
    | PatternMatching
    | PatternMatchCase
    | LetBinding
    | PatternList
    | PatternOcamlList
    | PatternRecord

    | TypeDef
    | TypeConstrName
    | TypeParams
    | TypeParam [@live]
    | PackageConstraint

    | TypeRepresentation

    | RecordDecl
    | ConstructorDeclaration
    | ParameterList
    | StringFieldDeclarations
    | FieldDeclarations
    | TypExprList
    | FunctorArgs
    | ModExprList
    | TypeParameters
    | RecordRows
    | RecordRowsStringKey
    | ArgumentList
    | Signature
    | Specification
    | Structure
    | Implementation
    | Attribute
    | TypeConstraint
    | Primitive
    | AtomicTypExpr
    | ListExpr
    | JsFfiImport

  let to_string = function
    | OpenDescription -> "an open description"
    | ModuleLongIdent -> "a module identifier"
    | Ternary -> "a ternary expression"
    | Es6ArrowExpr -> "an es6 arrow function"
    | Jsx -> "a jsx expression"
    | JsxAttribute -> "a jsx attribute"
    | ExprOperand -> "a basic expression"
    | ExprUnary -> "a unary expression"
    | ExprBinaryAfterOp op -> "an expression after the operator \"" ^ Token.to_string op  ^ "\""
    | ExprIf -> "an if expression"
    | IfCondition -> "the condition of an if expression"
    | IfBranch -> "the true-branch of an if expression"
    | ElseBranch -> "the else-branch of an if expression"
    | TypeExpression -> "a type"
    | External -> "an external"
    | PatternMatching -> "the cases of a pattern match"
    | ExprBlock -> "a block with expressions"
    | ExprSetField -> "a record field mutation"
    | ExprCall -> "a function application"
    | ExprArrayAccess -> "an array access expression"
    | ExprArrayMutation -> "an array mutation"
    | LetBinding -> "a let binding"
    | TypeDef -> "a type definition"
    | TypeParams -> "type parameters"
    | TypeParam -> "a type parameter"
    | TypeConstrName -> "a type-constructor name"
    | TypeRepresentation -> "a type representation"
    | RecordDecl -> "a record declaration"
    | PatternMatchCase -> "a pattern match case"
    | ConstructorDeclaration -> "a constructor declaration"
    | ExprList -> "multiple expressions"
    | PatternList -> "multiple patterns"
    | PatternOcamlList -> "a list pattern"
    | PatternRecord -> "a record pattern"
    | ParameterList -> "parameters"
    | StringFieldDeclarations -> "string field declarations"
    | FieldDeclarations -> "field declarations"
    | TypExprList -> "list of types"
    | FunctorArgs -> "functor arguments"
    | ModExprList -> "list of module expressions"
    | TypeParameters -> "list of type parameters"
    | RecordRows -> "rows of a record"
    | RecordRowsStringKey -> "rows of a record with string keys"
    | ArgumentList -> "arguments"
    | Signature -> "signature"
    | Specification -> "specification"
    | Structure -> "structure"
    | Implementation -> "implementation"
    | Attribute -> "an attribute"
    | TypeConstraint -> "constraints on a type"
    | Primitive -> "an external primitive"
    | AtomicTypExpr -> "a type"
    | ListExpr -> "an ocaml list expr"
    | PackageConstraint -> "a package constraint"
    | JsFfiImport -> "js ffi import"
    | JsxChild -> "jsx child"

  let is_signature_item_start = function
    | Token.At
    | Let
    | Typ
    | External
    | Exception
    | Open
    | Include
    | Module
    | AtAt
    | PercentPercent -> true
    | _ -> false

  let is_atomic_pattern_start = function
    | Token.Int _ | String _ | Character _
    | Lparen | Lbracket | Lbrace
    | Underscore
    | Lident _ | Uident _ | List
    | Exception | Lazy
    | Percent -> true
    | _ -> false

  let is_atomic_expr_start = function
    | Token.True | False
    | Int _ | String _ | Float _ | Character _
    | Backtick
    | Uident _ | Lident _ | Hash
    | Lparen
    | List
    | Lbracket
    | Lbrace
    | LessThan
    | Module
    | Percent -> true
    | _ -> false

  let is_atomic_typ_expr_start = function
    | Token.SingleQuote | Underscore
    | Lparen | Lbrace
    | Uident _ | Lident _ | List
    | Percent -> true
    | _ -> false

  let is_expr_start = function
    | Token.True | False
    | Int _ | String _ | Float _ | Character _ | Backtick
    | Underscore (* _ => doThings() *)
    | Uident _ | Lident _ | Hash
    | Lparen | List | Module | Lbracket | Lbrace
    | LessThan
    | Minus | MinusDot | Plus | PlusDot | Bang
    | Percent | At
    | If | Switch | While | For | Assert | Lazy | Try -> true
    | _ -> false

  let is_jsx_attribute_start = function
    | Token.Lident _ | Question -> true
    | _ -> false

 let is_structure_item_start = function
    | Token.Open
    | Let
    | Typ
    | External | Import | Export
    | Exception
    | Include
    | Module
    | AtAt
    | PercentPercent
    | At -> true
    | t when is_expr_start t -> true
    | _ -> false

  let is_pattern_start = function
    | Token.Int _ | Float _ | String _ | Character _ | True | False | Minus | Plus
    | Lparen | Lbracket | Lbrace | List
    | Underscore
    | Lident _ | Uident _ | Hash | HashHash
    | Exception | Lazy | Percent | Module
    | At -> true
    | _ -> false

  let is_parameter_start = function
    | Token.Typ | Tilde | Dot -> true
    | token when is_pattern_start token -> true
    | _ -> false

  (* TODO: overparse Uident ? *)
  let is_string_field_decl_start = function
    | Token.String _ | At -> true
    | _ -> false

  (* TODO: overparse Uident ? *)
  let is_field_decl_start = function
    | Token.At | Mutable | Lident _ | List  -> true
    (* recovery, TODO: this is not ideal… *)
    | Uident _ -> true
    | t when Token.is_keyword t -> true
    | _ -> false

  let is_record_decl_start = function
    | Token.At
    | Mutable
    | Lident _ | List -> true
    | _ -> false

  let is_typ_expr_start = function
    | Token.At
    | SingleQuote
    | Underscore
    | Lparen | Lbracket
    | Uident _ | Lident _ | List
    | Module
    | Percent
    | Lbrace -> true
    | _ -> false

  let is_type_parameter_start = function
    | Token.Tilde | Dot -> true
    | token when is_typ_expr_start token -> true
    | _ -> false

  let is_type_param_start = function
    | Token.Plus | Minus | SingleQuote | Underscore -> true
    | _ -> false

  let is_functor_arg_start = function
    | Token.At | Uident _ | Underscore
    | Percent
    | Lbrace
    | Lparen -> true
    | _ -> false

  let is_mod_expr_start = function
    | Token.At | Percent
    | Uident _ | Lbrace | Lparen -> true
    | _ -> false

  let is_record_row_start = function
    | Token.DotDotDot -> true
    | Token.Uident _ | Lident _ | List -> true
    (* TODO *)
    | t when Token.is_keyword t -> true
    | _ -> false

  let is_record_row_string_key_start = function
    | Token.String _ -> true
    | _ -> false

  let is_argument_start = function
    | Token.Tilde | Dot | Underscore -> true
    | t when is_expr_start t -> true
    | _ -> false

  let is_pattern_match_start = function
    | Token.Bar -> true
    | t when is_pattern_start t -> true
    | _ -> false

  let is_pattern_ocaml_list_start = function
    | Token.DotDotDot -> true
    | t when is_pattern_start t -> true
    | _ -> false

  let is_pattern_record_item_start = function
    | Token.DotDotDot | Uident _ | Lident _ | List | Underscore -> true
    | _ -> false

  let is_attribute_start = function
    | Token.At -> true
    | _ -> false

  let is_js_ffi_import_start = function
    | Token.Lident _ | At -> true
    | _ -> false

  let is_jsx_child_start = is_atomic_expr_start

  let is_block_expr_start = function
    | Token.At | Hash | Percent | Minus | MinusDot | Plus | PlusDot | Bang
    | True | False | Int _ | String _ | Character _ | Lident _ | Uident _
    | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
    | Lazy | If | For | While | Switch | Open | Module | Exception | Let
    | LessThan | Backtick | Try | Underscore -> true
    | _ -> false

  let is_list_element grammar token =
    match grammar with
    | ExprList -> token = Token.DotDotDot || is_expr_start token
    | ListExpr -> token = DotDotDot || is_expr_start token
    | PatternList -> token = DotDotDot || is_pattern_start token
    | ParameterList -> is_parameter_start token
    | StringFieldDeclarations -> is_string_field_decl_start token
    | FieldDeclarations -> is_field_decl_start token
    | RecordDecl -> is_record_decl_start token
    | TypExprList -> is_typ_expr_start token || token = Token.LessThan
    | TypeParams -> is_type_param_start token
    | FunctorArgs -> is_functor_arg_start token
    | ModExprList -> is_mod_expr_start token
    | TypeParameters -> is_type_parameter_start token
    | RecordRows -> is_record_row_start token
    | RecordRowsStringKey -> is_record_row_string_key_start token
    | ArgumentList -> is_argument_start token
    | Signature | Specification -> is_signature_item_start token
    | Structure | Implementation -> is_structure_item_start token
    | PatternMatching -> is_pattern_match_start token
    | PatternOcamlList -> is_pattern_ocaml_list_start token
    | PatternRecord -> is_pattern_record_item_start token
    | Attribute -> is_attribute_start token
    | TypeConstraint -> token = Constraint
    | PackageConstraint -> token = And
    | ConstructorDeclaration -> token = Bar
    | Primitive -> begin match token with Token.String _ -> true | _ -> false end
    | JsxAttribute -> is_jsx_attribute_start token
    | JsFfiImport -> is_js_ffi_import_start token
    | _ -> false

  let is_list_terminator grammar token =
    match grammar, token with
    | _, Token.Eof
    | ExprList, (Rparen | Forwardslash | Rbracket)
    | ListExpr, Rparen
    | ArgumentList, Rparen
    | TypExprList, (Rparen | Forwardslash | GreaterThan | Equal)
    | ModExprList, Rparen
    | (PatternList | PatternOcamlList | PatternRecord),
      (Forwardslash | Rbracket | Rparen | EqualGreater (* pattern matching => *) | In (* for expressions *) | Equal (* let {x} = foo *))
    | ExprBlock, Rbrace
    | (Structure | Signature), Rbrace
    | TypeParams, Rparen
    | ParameterList, (EqualGreater | Lbrace)
    | JsxAttribute, (Forwardslash | GreaterThan)
    | JsFfiImport, Rbrace
    | StringFieldDeclarations, Rbrace -> true

    | Attribute, token when token <> At -> true
    | TypeConstraint, token when token <> Constraint -> true
    | PackageConstraint, token when token <> And -> true
    | ConstructorDeclaration, token when token <> Bar -> true
    | Primitive, Semicolon -> true
    | Primitive, token when is_structure_item_start token -> true

    | _ -> false

  let is_part_of_list grammar token =
    is_list_element grammar token || is_list_terminator grammar token
end

module Reporting = struct
  module TerminalDoc = struct
    type break =
      | Never
      | Always

    type document =
      | Nil
      | Group of {break: break; doc: document}
      | Text of string
      | Indent of {amount: int; doc: document}
      | Append of {doc1: document; doc2: document}

    let group ~break doc = Group {break; doc}
    let text txt = Text (txt)
    let indent i d = Indent {amount = i; doc = d}
    let append d1 d2 = Append {doc1 = d1; doc2 = d2}
    let nil = Nil

    type stack =
      | Empty
      | Cons of {doc: document; stack: stack}

    let push stack doc = Cons {doc; stack}

    type mode =
      | Flat
      | Break

    let to_string (* ~width *) (doc : document) =
      let buffer = Buffer.create 100 in
      let rec loop stack mode offset =
        match stack with
        | Empty  -> ()
        | Cons {doc; stack = rest} ->
          begin match doc with
           | Nil -> loop rest mode offset
           | Text txt ->
             Buffer.add_string buffer txt;
             loop rest mode (offset + (String.length txt))
           | Indent {amount = i; doc} ->
             let indentation = (String.make [@doesNotRaise]) i ' ' in
             Buffer.add_string buffer indentation;
             loop (push rest doc) mode (offset + i)
           | Append {doc1; doc2} ->
              let rest = push rest doc2 in
              let rest = push rest
                (match mode = Flat with
                | true  -> Nil
                | false  -> text "\n")
              in
              let rest = push rest doc1 in
              loop rest mode offset
           | Group {break; doc} ->
             let rest = push rest doc in
             begin match break with
             | Always  -> loop rest Break offset
             | Never  -> loop rest Flat offset
             end
            end
      in
      loop (push Empty doc) Flat 0;
      Buffer.contents buffer
  end

  type color =
    | NoColor [@live]
    | Red [@live]

  type style = {
    underline: bool; [@live]
    color: color; [@live]
  }

  let highlight ~from ~len txt =
    if from < 0 || (String.length txt) == 0 || (from >= String.length txt) then txt else
    let before = try String.sub txt 0 from with Invalid_argument _ -> "" in
    let content =
      "\027[31m" ^ (try String.sub txt from len with Invalid_argument _ -> "") ^ "\027[0m"
    in
    let after = try String.sub txt (from + len) (String.length txt - (from + len)) with Invalid_argument _ -> "" in
    before ^ content ^ after

  let underline ~from ~len txt =
    let open TerminalDoc in
    let indent = (String.make [@doesNotRaise]) from ' ' in
    let underline = (String.make [@doesNotRaise]) len '^' in
    let line = highlight ~from:0 ~len underline in
    group ~break:Always
      (append (text txt) (text (indent ^ line)))

  let rec drop n l =
    if n == 1 then l
    else drop (n - 1) (match l with | _x::xs -> xs | _ -> l)

  let rec take n l =
    match l with
    | _ when n == 0 -> []
    | [] -> []
    | x::xs -> x::(take (n -1) xs)

  (* TODO: cleanup *)
  let render_code_context ~missing (src : string) start_pos end_pos =
    let open Lexing in
    let start_col = (start_pos.pos_cnum - start_pos.pos_bol) in
    let end_col = end_pos.pos_cnum - start_pos.pos_cnum + start_col in
    let start_line = max 1 (start_pos.pos_lnum - 2) in (* 2 lines before *)
    let lines =  String.split_on_char '\n' src in
    let end_line =
      let len = List.length lines in
      min len (start_pos.pos_lnum + 3) (* 2 lines after *)
    in
    let lines =
      lines
      |> drop start_line
      |> take (end_line - start_line)
      |> Array.of_list
    in

    let render_line x ix =
      let x = if ix = start_pos.pos_lnum then
          begin match missing with
          | Some _len -> x ^ (String.make 10 ' ' [@doesNotRaise])
          | None -> x
          end
        else
          x
      in

      let open TerminalDoc in
      let row_nr =
        let txt = string_of_int ix in
        let len = String.length txt in
        if ix = start_pos.pos_lnum then
          highlight ~from:0 ~len txt
        else txt
      in
      let len =
        let len = if end_col >= 0 then
          end_col - start_col
        else
          1
        in
        if (start_col + len) > String.length x then String.length x - start_col - 1 else len
      in
      let line =
        if ix = start_pos.pos_lnum then
          begin match missing with
          | Some len ->
            underline
              ~from:(
              start_col + String.length (String.length (string_of_int ix) |> string_of_int) + 5
              ) ~len x
          | None ->
              let len = if start_col + len > String.length x then
                (String.length x) - start_col
              else
                len
              in
            text (highlight ~from:start_col ~len x)
          end
        else text x
      in
      group ~break:Never
        (append
          (append (text row_nr) (text " │"))
          (indent 2 line))
    in

    let report_doc = ref TerminalDoc.nil in

    let lines_len = Array.length lines in
    for i = 0 to (lines_len - 1) do
      let line = try (Array.get [@doesNotRaise]) lines i with Invalid_argument _ -> "" in
      report_doc :=
        let open TerminalDoc in
        let ix = start_line + i in
        group ~break:Always (append !report_doc (render_line line ix))
    done;

    TerminalDoc.to_string !report_doc

  type problem =
    | Unexpected of Token.t [@live]
    | Expected of {token: Token.t; pos: Lexing.position; context: Grammar.t option} [@live]
    | Message of string [@live]
    | Uident [@live]
    | Lident [@live]
    | Unbalanced of Token.t [@live]

  type parse_error = Lexing.position * problem
end

module Diagnostics: sig
  type t
  type category
  type report

  type report_style
  val parse_report_style: string -> report_style

  val unexpected: Token.t -> (Grammar.t * Lexing.position) list -> category
  val expected:  ?grammar:Grammar.t -> Lexing.position -> Token.t -> category
  val uident: Token.t -> category
  val lident: Token.t -> category
  val unclosed_string: category
  val unclosed_template: category
  val unclosed_comment: category
  val unknown_uchar: int -> category
  val message: string -> category

  val make:
    filename: string
    -> start_pos: Lexing.position
    -> end_pos: Lexing.position
    -> category
    -> t

  val string_of_report: style:report_style -> t list -> string -> string
end = struct
  type category =
    | Unexpected of {token: Token.t; context: (Grammar.t * Lexing.position) list}
    | Expected of {context: Grammar.t option; pos: Lexing.position (* prev token end*); token: Token.t}
    | Message of string
    | Uident of Token.t
    | Lident of Token.t
    | UnclosedString
    | UnclosedTemplate
    | UnclosedComment
    | UnknownUchar of int

  type t = {
    filename: string;
    start_pos: Lexing.position;
    end_pos: Lexing.position;
    category: category;
  }

  type report = t list

  (* TODO: add json here *)
  type report_style =
    | Pretty
    | Plain

  let parse_report_style txt = match (String.lowercase_ascii txt) with
    | "plain" -> Plain
    | _ -> Pretty

  let default_unexpected token =
    "I'm not sure what to parse here when looking at \"" ^ (Token.to_string token) ^ "\"."

  let explain t =
    match t.category with
    | Uident current_token ->
      begin match current_token with
      | Lident lident ->
        let guess = String.capitalize_ascii lident in
        "Did you mean `" ^ guess ^"` instead of `" ^ lident ^ "`?"
      | t when Token.is_keyword t ->
        let token = Token.to_string t in
        "`" ^ token ^ "` is a reserved keyword."
      | _ ->
        "At this point, I'm looking for an uppercased identifier like `Belt` or `Array`"
      end
    | Lident current_token ->
      begin match current_token with
      | Uident uident ->
        let guess = String.uncapitalize_ascii uident in
        "Did you mean `" ^ guess ^"` instead of `" ^ uident ^ "`?"
      | t when Token.is_keyword t ->
        let token = Token.to_string t in
        "`" ^ token ^ "` is a reserved keyword. Keywords need to be escaped: \\\"" ^ token ^ "\""
      | Underscore ->
        "`_` isn't a valid name."
      | _ ->
        "I'm expecting an lowercased identifier like `name` or `age`"
      end
    | Message txt -> txt
    | UnclosedString ->
      "This string is missing a double quote at the end"
    | UnclosedTemplate ->
      "Did you forget to close this template expression with a backtick?"
    | UnclosedComment ->
      "This comment seems to be missing a closing `*/`"
    | UnknownUchar uchar ->
      begin match uchar with
      | 94 (* ^ *) ->
        "Hmm, not sure what I should do here with this character.\nIf you're trying to deref an expression, use `foo.contents` instead."
      | _ ->
        "Hmm, I have no idea what this character means…"
      end
    | Expected {context; token = t} ->
      let hint = match context with
      | Some grammar -> "It signals the start of " ^ (Grammar.to_string grammar)
      | None -> ""
      in
      "Did you forget a `" ^ (Token.to_string t) ^ "` here? " ^ hint
    | Unexpected {token = t; context = breadcrumbs} ->
      let name = (Token.to_string t) in
      begin match breadcrumbs with
      | (AtomicTypExpr, _)::breadcrumbs ->
          begin match breadcrumbs, t with
          | ((StringFieldDeclarations | FieldDeclarations) , _) :: _, (String _ | At | Rbrace | Comma | Eof) ->
              "I'm missing a type here"
          | _, t when Grammar.is_structure_item_start t || t = Eof ->
              "Missing a type here"
          | _ ->
            default_unexpected t
          end
      | (ExprOperand, _)::breadcrumbs ->
          begin match breadcrumbs, t with
          | (ExprBlock, _) :: _, Rbrace ->
            "It seems that this expression block is empty"
          | (ExprBlock, _) :: _, Bar -> (* Pattern matching *)
            "Looks like there might be an expression missing here"
          | (ExprSetField, _) :: _, _ ->
            "It seems that this record field mutation misses an expression"
          | (ExprArrayMutation, _) :: _, _ ->
            "Seems that an expression is missing, with what do I mutate the array?"
          | ((ExprBinaryAfterOp _ | ExprUnary), _) ::_, _ ->
            "Did you forget to write an expression here?"
          | (Grammar.LetBinding, _)::_, _ ->
            "This let-binding misses an expression"
          | _::_, (Rbracket | Rbrace) ->
            "Missing expression"
          | _ ->
            "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
          end
      | (TypeParam, _)::_ ->
          begin match t with
          | Lident ident ->
            "Did you mean '" ^ ident ^"? A Type parameter starts with a quote."
          | _ ->
            "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
          end
      | _ ->
        (* TODO: match on circumstance to verify Lident needed ? *)
        if Token.is_keyword t then
          "`" ^ name ^ "` is a reserved keyword. Keywords need to be escaped: \\\"" ^ (Token.to_string t) ^ "\""
        else
        "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
      end

  let to_plain_string t buffer =
    Buffer.add_string buffer t.filename;
    Buffer.add_char buffer '(';
    Buffer.add_string buffer (string_of_int t.start_pos.pos_cnum);
    Buffer.add_char buffer ',';
    Buffer.add_string buffer (string_of_int t.end_pos.pos_cnum);
    Buffer.add_char buffer ')';
    Buffer.add_char buffer ':';
    Buffer.add_string buffer (explain t)

  let to_string t src =
    let open Lexing in
    let  startchar = t.start_pos.pos_cnum - t.start_pos.pos_bol in
    let endchar = t.end_pos.pos_cnum - t.start_pos.pos_cnum + startchar in
    let location_info =
      Printf.sprintf (* ReasonLanguageServer requires the following format *)
        "File \"%s\", line %d, characters %d-%d:"
        t.filename
        t.start_pos.pos_lnum
        startchar
        endchar
    in
    let code =
      let missing = match t.category with
      | Expected {token = t} ->
        Some (String.length (Token.to_string t))
      | _ -> None
      in
      Reporting.render_code_context ~missing src t.start_pos t.end_pos
    in
    let explanation = explain t in
    Printf.sprintf "%s\n\n%s\n\n%s\n\n" location_info code explanation

  let make ~filename ~start_pos ~end_pos category = {
    filename;
    start_pos;
    end_pos;
    category
  }

  let string_of_report ~style diagnostics src =
    match style with
    | Pretty ->
      List.fold_left (fun report diagnostic ->
        report ^ (to_string diagnostic src) ^ "\n"
      ) "\n" (List.rev diagnostics)
    | Plain ->
      let buffer = Buffer.create 100 in
      List.iter (fun diagnostic ->
        to_plain_string diagnostic buffer;
        Buffer.add_char buffer '\n';
      ) diagnostics;
      Buffer.contents buffer

  let unexpected token context =
    Unexpected {token; context}

  let expected ?grammar pos token =
    Expected {context = grammar; pos; token}

  let uident current_token = Uident current_token
  let lident current_token = Lident current_token
  let unclosed_string = UnclosedString
  let unclosed_comment = UnclosedComment
  let unclosed_template = UnclosedTemplate
  let unknown_uchar code = UnknownUchar code
  let message txt = Message txt
end

(* Collection of utilities to view the ast in a more a convenient form,
 * allowing for easier processing.
 * Example: given a ptyp_arrow type, what are its arguments and what is the
 * returnType? *)
module ParsetreeViewer : sig
  (* Restructures a nested tree of arrow types into its args & returnType
   * The parsetree contains: a => b => c => d, for printing purposes
   * we restructure the tree into (a, b, c) and its returnType d *)
  val arrow_type: Parsetree.core_type ->
      Parsetree.attributes *
      (Parsetree.attributes * Asttypes.arg_label * Parsetree.core_type) list *
      Parsetree.core_type

  val functor_type: Parsetree.module_type ->
    (Parsetree.attributes * string Asttypes.loc * Parsetree.module_type option) list *
    Parsetree.module_type

  (* filters @bs out of the provided attributes *)
  val process_uncurried_attribute: Parsetree.attributes -> bool * Parsetree.attributes

  (* if ... else if ... else ... is represented as nested expressions: if ... else { if ... }
   * The purpose of this function is to flatten nested ifs into one sequence.
   * Basically compute: ([if, else if, else if, else if], else) *)
  val collect_if_expressions:
    Parsetree.expression ->
      (Parsetree.expression * Parsetree.expression) list * Parsetree.expression option

  val collect_list_expressions:
    Parsetree.expression -> (Parsetree.expression list * Parsetree.expression option)

  type fun_param_kind =
    | Parameter of {
        attrs: Parsetree.attributes;
        lbl: Asttypes.arg_label;
        default_expr: Parsetree.expression option;
        pat: Parsetree.pattern;
      }
    | NewTypes of {attrs: Parsetree.attributes; locs: string Asttypes.loc list}

  val fun_expr:
    Parsetree.expression ->
      Parsetree.attributes *
      fun_param_kind list *
      Parsetree.expression

  (* example:
   *  `makeCoordinate({
   *    x: 1,
   *    y: 2,
   *  })`
   *  Notice howe `({` and `})` "hug" or stick to each other *)
  val is_huggable_expression: Parsetree.expression -> bool

  val is_huggable_pattern: Parsetree.pattern -> bool

  val is_huggable_rhs: Parsetree.expression -> bool

  val operator_precedence: string -> int

  val is_unary_expression: Parsetree.expression -> bool
  val is_binary_operator: string -> bool
  val is_binary_expression: Parsetree.expression -> bool

  val flattenable_operators: string -> string -> bool

  val has_attributes: Parsetree.attributes -> bool

  val is_array_access: Parsetree.expression -> bool
  val is_ternary_expr: Parsetree.expression -> bool

  val collect_ternary_parts: Parsetree.expression -> ((Parsetree.expression * Parsetree.expression) list * Parsetree.expression)

  val parameters_should_hug:
    fun_param_kind list -> bool

  val filter_ternary_attributes: Parsetree.attributes -> Parsetree.attributes

  val is_jsx_expression: Parsetree.expression -> bool
  val has_jsx_attribute: Parsetree.attributes -> bool

  val should_indent_binary_expr: Parsetree.expression -> bool
  val should_inline_rhs_binary_expr: Parsetree.expression -> bool
  val filter_printeable_attributes: Parsetree.attributes -> Parsetree.attributes
  val partition_printeable_attributes: Parsetree.attributes -> (Parsetree.attributes * Parsetree.attributes)

  val requires_special_callback_printing_last_arg: (Asttypes.arg_label * Parsetree.expression) list -> bool
  val requires_special_callback_printing_first_arg: (Asttypes.arg_label * Parsetree.expression) list -> bool

  val mod_expr_apply : Parsetree.module_expr -> (
    Parsetree.module_expr list * Parsetree.module_expr
  )

  val mod_expr_functor : Parsetree.module_expr -> (
    (Parsetree.attributes * string Asttypes.loc * Parsetree.module_type option) list *
    Parsetree.module_expr
  )

  val split_gen_type_attr : Parsetree.attributes -> (bool * Parsetree.attributes)

  val collect_patterns_from_list_construct:
    Parsetree.pattern list -> Parsetree.pattern ->
      (Parsetree.pattern list * Parsetree.pattern)

  val is_block_expr : Parsetree.expression -> bool

  val is_template_literal: Parsetree.expression -> bool

  val collect_or_pattern_chain:
    Parsetree.pattern -> Parsetree.pattern list

  val process_braces_attr : Parsetree.expression -> (Parsetree.attribute option * Parsetree.expression)

  val filter_parsing_attrs : Parsetree.attributes -> Parsetree.attributes

  val is_braced_expr : Parsetree.expression -> bool

  val is_pipe_expr : Parsetree.expression -> bool

  val extract_value_description_from_mod_expr: Parsetree.module_expr -> Parsetree.value_description list

  type js_import_scope =
    | JsGlobalImport (* nothing *)
    | JsModuleImport of string (* from "path" *)
    | JsScopedImport of string list (* window.location *)

  val classify_js_import: Parsetree.value_description -> js_import_scope

  (* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
  val rewrite_underscore_apply: Parsetree.expression -> Parsetree.expression

  (* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
  val is_underscore_apply_sugar: Parsetree.expression -> bool
end = struct
  open Parsetree

  let arrow_type ct =
    let rec process attrs_before acc typ = match typ with
    | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = []} ->
      let arg = ([], lbl, typ1) in
      process attrs_before (arg::acc) typ2
    | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = [({txt ="bs"}, _) ] as attrs} ->
      let arg = (attrs, lbl, typ1) in
      process attrs_before (arg::acc) typ2
    | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = _attrs} as return_type ->
      let args = List.rev acc in
      (attrs_before, args, return_type)
    | {ptyp_desc = Ptyp_arrow ((Labelled _ | Optional _) as lbl, typ1, typ2); ptyp_attributes = attrs} ->
      let arg = (attrs, lbl, typ1) in
      process attrs_before (arg::acc) typ2
    | typ ->
      (attrs_before, List.rev acc, typ)
    in
    begin match ct with
    | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = attrs} as typ ->
      process attrs [] {typ with ptyp_attributes = []}
    | typ -> process [] [] typ
    end

  let functor_type modtype =
    let rec process acc modtype = match modtype with
    | {pmty_desc = Pmty_functor (lbl, arg_type, return_type); pmty_attributes = attrs} ->
      let arg = (attrs, lbl, arg_type) in
      process (arg::acc) return_type
    | mod_type ->
      (List.rev acc, mod_type)
    in
    process [] modtype

  let process_uncurried_attribute attrs =
    let rec process uncurried_spotted acc attrs =
      match attrs with
      | [] -> (uncurried_spotted, List.rev acc)
      | ({Location.txt = "bs"}, _)::rest -> process true acc rest
      | attr::rest -> process uncurried_spotted (attr::acc) rest
    in
    process false [] attrs

  let collect_if_expressions expr =
    let rec collect acc expr = match expr.pexp_desc with
    | Pexp_ifthenelse (if_expr, then_expr, Some else_expr) ->
      collect ((if_expr, then_expr)::acc) else_expr
    | Pexp_ifthenelse (if_expr, then_expr, (None as else_expr)) ->
      let ifs = List.rev ((if_expr, then_expr)::acc) in
      (ifs, else_expr)
    | _ ->
      (List.rev acc, Some expr)
    in
    collect [] expr

  let collect_list_expressions expr =
    let rec collect acc expr = match expr.pexp_desc with
    | Pexp_construct ({txt = Longident.Lident "[]"}, _) ->
      (List.rev acc, None)
    | Pexp_construct (
        {txt = Longident.Lident "::"},
        Some {pexp_desc = Pexp_tuple (hd::[tail])}
      ) ->
        collect (hd::acc) tail
    | _ ->
      (List.rev acc, Some expr)
    in
    collect [] expr

  (* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
  let rewrite_underscore_apply expr =
    match expr.pexp_desc with
    | Pexp_fun (
        Nolabel,
        None,
        {ppat_desc = Ppat_var {txt="__x"}},
        ({pexp_desc = Pexp_apply (call_expr, args)} as e)
      ) ->
        let new_args = List.map (fun arg ->
          match arg with
          | (
              lbl,
              ({pexp_desc = Pexp_ident ({txt = Longident.Lident "__x"} as lid)} as arg_expr)
            ) ->
              (lbl, {arg_expr with pexp_desc = Pexp_ident ({lid with txt = Longident.Lident "_"})})
          | arg ->  arg
        ) args in
        {e with pexp_desc = Pexp_apply (call_expr, new_args)}
    | _ -> expr

  type fun_param_kind =
    | Parameter of {
        attrs: Parsetree.attributes;
        lbl: Asttypes.arg_label;
        default_expr: Parsetree.expression option;
        pat: Parsetree.pattern;
      }
    | NewTypes of {attrs: Parsetree.attributes; locs: string Asttypes.loc list}

  let fun_expr expr =
    (* Turns (type t, type u, type z) into "type t u z" *)
    let rec collect_new_types acc return_expr =
      match return_expr with
      | {pexp_desc = Pexp_newtype (string_loc, return_expr); pexp_attributes = []} ->
        collect_new_types (string_loc::acc) return_expr
      | return_expr ->
        (List.rev acc, return_expr)
    in
    let rec collect attrs_before acc expr = match expr with
    | {pexp_desc = Pexp_fun (
        Nolabel,
        None,
        {ppat_desc = Ppat_var {txt="__x"}},
        {pexp_desc = Pexp_apply _}
      )} ->
      (attrs_before, List.rev acc, rewrite_underscore_apply expr)
    | {pexp_desc = Pexp_fun (lbl, default_expr, pattern, return_expr); pexp_attributes = []} ->
      let parameter = Parameter {
        attrs = [];
        lbl = lbl;
        default_expr = default_expr;
        pat = pattern;
      } in
      collect attrs_before (parameter::acc) return_expr
    | {pexp_desc = Pexp_newtype (string_loc, rest); pexp_attributes = attrs} ->
      let (string_locs, return_expr) = collect_new_types [string_loc] rest in
      let param = NewTypes {attrs; locs = string_locs} in
      collect attrs_before (param::acc) return_expr
    | {pexp_desc = Pexp_fun (lbl, default_expr, pattern, return_expr); pexp_attributes = [({txt = "bs"}, _)] as attrs} ->
      let parameter = Parameter {
        attrs = attrs;
        lbl = lbl;
        default_expr = default_expr;
        pat = pattern;
      } in
      collect attrs_before (parameter::acc) return_expr
    | {
        pexp_desc = Pexp_fun ((Labelled _ | Optional _) as lbl, default_expr, pattern, return_expr);
        pexp_attributes = attrs
      } ->
      let parameter = Parameter {
        attrs = attrs;
        lbl = lbl;
        default_expr = default_expr;
        pat = pattern;
      } in
      collect attrs_before (parameter::acc) return_expr
    | expr ->
      (attrs_before, List.rev acc, expr)
    in
    begin match expr with
    | {pexp_desc = Pexp_fun (Nolabel, _defaultExpr, _pattern, _returnExpr); pexp_attributes = attrs} as expr ->
      collect attrs [] {expr with pexp_attributes = []}
    | expr -> collect [] [] expr
    end

  let process_braces_attr expr =
    match expr.pexp_attributes with
    | (({txt = "res.braces"}, _) as attr)::attrs ->
      (Some attr, {expr with pexp_attributes = attrs})
    | _ ->
      (None, expr)

  let filter_parsing_attrs attrs =
    List.filter (fun attr ->
      match attr with
      | ({Location.txt = ("res.ternary" | "res.braces" | "bs" | "res.namedArgLoc")}, _) -> false
      | _ -> true
    ) attrs

  let is_block_expr expr =
    match expr.pexp_desc with
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_let _
    | Pexp_open _
    | Pexp_sequence _ -> true
    | _ -> false

  let is_braced_expr expr =
    match process_braces_attr expr with
    | (Some _, _) -> true
    | _ -> false

  let is_huggable_expression expr =
    match expr.pexp_desc with
    | Pexp_array _
    | Pexp_tuple _
    | Pexp_construct ({txt = Longident.Lident ("::" | "[]")}, _)
    | Pexp_extension ({txt = "obj"}, _)
    | Pexp_record _ -> true
    | _ when is_block_expr expr -> true
    | _ when is_braced_expr expr -> true
    | _ -> false

  let is_huggable_rhs expr =
    match expr.pexp_desc with
    | Pexp_array _
    | Pexp_tuple _
    | Pexp_construct ({txt = Longident.Lident ("::" | "[]")}, _)
    | Pexp_extension ({txt = "obj"}, _)
    | Pexp_record _ -> true
    | _ when is_braced_expr expr -> true
    | _ -> false

  let is_huggable_pattern pattern =
    match pattern.ppat_desc with
    | Ppat_array _
    | Ppat_tuple _
    | Ppat_record _
    | Ppat_construct _ -> true
    | _ -> false

  let operator_precedence operator = match operator with
    | ":=" -> 1
    | "||" -> 2
    | "&&" -> 3
    | "=" | "==" | "<" | ">" | "!=" | "<>" | "!==" | "<=" | ">=" | "|>" -> 4
    | "+" | "+." | "-" | "-." | "^" -> 5
    | "*" | "*." | "/" | "/." -> 6
    | "**" -> 7
    | "#" | "##" | "|." -> 8
    | _ -> 0

  let is_unary_operator operator = match operator with
    | "~+" | "~+." | "~-" | "~-." | "not" -> true
    | _ -> false

  let is_unary_expression expr = match expr.pexp_desc with
    | Pexp_apply(
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, _arg]
      ) when is_unary_operator operator -> true
    | _ -> false

  let is_binary_operator operator = match operator with
    | ":="
    | "||"
    | "&&"
    | "=" | "==" | "<" | ">" | "!=" | "!==" | "<=" | ">=" | "|>"
    | "+" | "+." | "-" | "-." | "^"
    | "*" | "*." | "/" | "/."
    | "**"
    | "|." | "<>" -> true
    | _ -> false

  let is_binary_expression expr = match expr.pexp_desc with
    | Pexp_apply(
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [(Nolabel, _operand1); (Nolabel, _operand2)]
      ) when is_binary_operator operator -> true
    | _ -> false

  let is_equality_operator operator = match operator with
    | "=" | "==" | "<>" | "!=" -> true
    | _ -> false

  let flattenable_operators parent_operator child_operator =
    let prec_parent = operator_precedence parent_operator in
    let prec_child =  operator_precedence child_operator in
    if prec_parent == prec_child then
      not (
        is_equality_operator parent_operator &&
        is_equality_operator child_operator
      )
    else
      false

  let has_attributes attrs =
    List.exists (fun attr -> match attr with
      | ({Location.txt = "bs" | "res.ternary" | "res.braces"}, _) -> false
      | _ -> true
    ) attrs

  let is_array_access expr = match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "get")}},
        [Nolabel, _parentExpr; Nolabel, _memberExpr]
      ) -> true
    | _ -> false

  let rec has_ternary_attribute attrs =
    match attrs with
    | [] -> false
    | ({Location.txt="res.ternary"},_)::_ -> true
    | _::attrs -> has_ternary_attribute attrs

  let is_ternary_expr expr = match expr with
    | {
        pexp_attributes = attrs;
        pexp_desc = Pexp_ifthenelse _
      } when has_ternary_attribute attrs -> true
    | _ -> false

  let collect_ternary_parts expr =
    let rec collect acc expr = match expr with
    | {
        pexp_attributes = attrs;
        pexp_desc = Pexp_ifthenelse (condition, consequent, Some(alternate))
      } when has_ternary_attribute attrs -> collect ((condition, consequent)::acc) alternate
    | alternate -> (List.rev acc, alternate)
    in
    collect [] expr

  let parameters_should_hug parameters = match parameters with
    | [Parameter {
        attrs = [];
        lbl = Asttypes.Nolabel;
        default_expr = None;
        pat = pat
      }] when is_huggable_pattern pat -> true
    | _ -> false

  let filter_ternary_attributes attrs =
    List.filter (fun attr -> match attr with
      |({Location.txt="res.ternary"},_) -> false
      | _ -> true
    ) attrs

  let is_jsx_expression expr =
    let rec loop attrs =
      match attrs with
      | [] -> false
      | ({Location.txt = "JSX"}, _)::_ -> true
      | _::attrs -> loop attrs
    in
    match expr.pexp_desc with
    | Pexp_apply _ ->
      loop expr.Parsetree.pexp_attributes
    | _ -> false

  let has_jsx_attribute attributes = match attributes with
    | ({Location.txt = "JSX"},_)::_ -> true
    | _ -> false

  let should_indent_binary_expr expr =
    let same_precedence_sub_expression operator sub_expression =
      match sub_expression with
      | {pexp_desc = Pexp_apply (
          {pexp_desc = Pexp_ident {txt = Longident.Lident sub_operator}},
          [Nolabel, _lhs; Nolabel, _rhs]
        )} when is_binary_operator sub_operator ->
        flattenable_operators operator sub_operator
      | _ -> true
    in
    match expr with
    | {pexp_desc = Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, lhs; Nolabel, _rhs]
      )} when is_binary_operator operator ->
      is_equality_operator operator ||
      not (same_precedence_sub_expression operator lhs) ||
      operator = ":="
    | _ -> false

  let should_inline_rhs_binary_expr rhs = match rhs.pexp_desc with
    | Parsetree.Pexp_constant _
    | Pexp_let _
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_sequence _
    | Pexp_open _
    | Pexp_ifthenelse _
    | Pexp_for _
    | Pexp_while _
    | Pexp_try _
    | Pexp_array _
    | Pexp_record _ -> true
    | _ -> false

  let filter_printeable_attributes attrs =
    List.filter (fun attr -> match attr with
      | ({Location.txt="bs" | "res.ternary"}, _) -> false
      | _ -> true
    ) attrs

  let partition_printeable_attributes attrs =
    List.partition (fun attr -> match attr with
      | ({Location.txt="bs" | "res.ternary"}, _) -> false
      | _ -> true
    ) attrs

  let requires_special_callback_printing_last_arg args =
    let rec loop args = match args with
    | [] -> false
    | [(_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})] -> true
    | (_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})::_ -> false
    | _::rest -> loop rest
    in
    loop args

  let requires_special_callback_printing_first_arg args =
    let rec loop args = match args with
      | [] -> true
      | (_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})::_ -> false
      | _::rest -> loop rest
    in
    match args with
    | [(_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})] -> false
    | (_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})::rest -> loop rest
    | _ -> false

  let mod_expr_apply mod_expr =
    let rec loop acc mod_expr = match mod_expr with
    | {pmod_desc = Pmod_apply (next, arg)} ->
      loop (arg::acc) next
    | _ -> (acc, mod_expr)
    in
    loop [] mod_expr

  let mod_expr_functor mod_expr =
    let rec loop acc mod_expr = match mod_expr with
    | {pmod_desc = Pmod_functor (lbl, mod_type, return_mod_expr); pmod_attributes = attrs} ->
      let param = (attrs, lbl, mod_type) in
      loop (param::acc) return_mod_expr
    | return_mod_expr ->
      (List.rev acc, return_mod_expr)
    in
    loop [] mod_expr

  let split_gen_type_attr attrs =
    match attrs with
    | ({Location.txt = "genType"}, _)::attrs -> (true, attrs)
    | attrs -> (false, attrs)

  let rec collect_patterns_from_list_construct acc pattern =
    let open Parsetree in
    match pattern.ppat_desc with
    | Ppat_construct(
        {txt = Longident.Lident "::"},
        Some {ppat_desc=Ppat_tuple (pat::rest::[])}
      ) ->
      collect_patterns_from_list_construct (pat::acc) rest
    | _ -> List.rev acc, pattern

  let rec is_template_literal expr =
    let is_pexp_constant_string expr = match expr.pexp_desc with
    | Pexp_constant (Pconst_string (_, Some _)) -> true
    | _ -> false
    in
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident "^"}},
        [Nolabel, arg1; Nolabel, arg2]
      ) when not (is_pexp_constant_string arg1 && is_pexp_constant_string arg2) ->
      is_template_literal arg1 || is_template_literal arg2
    | Pexp_constant (Pconst_string (_, Some _)) -> true
    | _ -> false

  (* Blue | Red | Green -> [Blue; Red; Green] *)
  let collect_or_pattern_chain pat =
    let rec loop pattern chain =
      match pattern.ppat_desc with
      | Ppat_or (left, right) -> loop left (right::chain)
      | _ -> pattern::chain
    in
    loop pat []

  let is_pipe_expr expr = match expr.pexp_desc with
    | Pexp_apply(
        {pexp_desc = Pexp_ident {txt = Longident.Lident ("|." | "|>") }},
        [(Nolabel, _operand1); (Nolabel, _operand2)]
      ) -> true
    | _ -> false

  let extract_value_description_from_mod_expr mod_expr =
    let rec loop structure acc =
      match structure with
      | [] -> List.rev acc
      | structure_item::structure ->
        begin match structure_item.Parsetree.pstr_desc with
        | Pstr_primitive vd -> loop structure (vd::acc)
        | _ -> loop structure acc
        end
    in
    match mod_expr.pmod_desc with
    | Pmod_structure structure -> loop structure []
    | _ -> []

  type js_import_scope =
    | JsGlobalImport (* nothing *)
    | JsModuleImport of string (* from "path" *)
    | JsScopedImport of string list (* window.location *)

  let classify_js_import value_description =
    let rec loop attrs =
      let open Parsetree in
      match attrs with
      | [] -> JsGlobalImport
      | ({Location.txt = "scope"}, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _))}, _)}])::_ ->
        JsScopedImport [s]
      | ({Location.txt = "genType.import"}, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _))}, _)}])::_ ->
        JsModuleImport s
      | ({Location.txt = "scope"}, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_tuple exprs}, _)}])::_ ->
        let scopes = List.fold_left (fun acc curr ->
          match curr.Parsetree.pexp_desc with
          | Pexp_constant (Pconst_string (s, _)) -> s::acc
          | _ -> acc
        ) [] exprs
        in
        JsScopedImport (List.rev scopes)
      | _::attrs ->
       loop attrs
    in
    loop value_description.pval_attributes

  let is_underscore_apply_sugar expr =
    match expr.pexp_desc with
    | Pexp_fun (
        Nolabel,
        None,
        {ppat_desc = Ppat_var {txt="__x"}},
        {pexp_desc = Pexp_apply _}
      ) -> true
    | _ -> false
end

module Parens: sig
  type kind = Parenthesized | Braced of Location.t | Nothing

  val expr: Parsetree.expression -> kind
  val structure_expr: Parsetree.expression -> kind

  val unary_expr_operand: Parsetree.expression -> kind

  val binary_expr_operand: is_lhs:bool -> Parsetree.expression -> kind
  val sub_binary_expr_operand: string -> string -> bool
  val rhs_binary_expr_operand: string -> Parsetree.expression -> bool
  val flatten_operand_rhs: string -> Parsetree.expression -> bool

  val lazy_or_assert_expr_rhs: Parsetree.expression -> kind

  val field_expr: Parsetree.expression -> kind

  val set_field_expr_rhs: Parsetree.expression -> kind

  val ternary_operand: Parsetree.expression -> kind

  val jsx_prop_expr: Parsetree.expression -> kind
  val jsx_child_expr: Parsetree.expression -> kind

  val binary_expr: Parsetree.expression -> kind
  val mod_type_functor_return: Parsetree.module_type -> bool
  val mod_type_with_operand: Parsetree.module_type -> bool
  val mod_expr_functor_constraint: Parsetree.module_type -> bool

  val braced_expr: Parsetree.expression -> bool
  val call_expr: Parsetree.expression -> kind

  val include_mod_expr : Parsetree.module_expr -> bool
end = struct
  type kind = Parenthesized | Braced of Location.t | Nothing

  let expr expr =
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
    | _ ->
      begin match expr with
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_constraint _ } -> Parenthesized
      |  _ -> Nothing
      end

  let call_expr expr =
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
    | _ ->
      begin match expr with
      | {Parsetree.pexp_attributes = attrs} when
          begin match ParsetreeViewer.filter_parsing_attrs attrs with
          | _::_ -> true
          | [] -> false
          end
          -> Parenthesized
      | _ when ParsetreeViewer.is_unary_expression expr || ParsetreeViewer.is_binary_expression expr -> Parenthesized
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_fun _}
        when ParsetreeViewer.is_underscore_apply_sugar expr -> Nothing
      | {pexp_desc =
            Pexp_lazy _
          | Pexp_assert _
          | Pexp_fun _
          | Pexp_newtype _
          | Pexp_function _
          | Pexp_constraint _
          | Pexp_setfield _
          | Pexp_match _
          | Pexp_try _
          | Pexp_while _
          | Pexp_for _
          | Pexp_ifthenelse _
        } -> Parenthesized
      |  _ -> Nothing
      end

  let structure_expr expr =
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
    | None ->
      begin match expr with
      | _ when ParsetreeViewer.has_attributes expr.pexp_attributes &&
        not (ParsetreeViewer.is_jsx_expression expr) -> Parenthesized
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_constraint _ } -> Parenthesized
      |  _ -> Nothing
      end

  let unary_expr_operand expr =
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_attributes = attrs} when
          begin match ParsetreeViewer.filter_parsing_attrs attrs with
          | _::_ -> true
          | [] -> false
          end
          -> Parenthesized
      | expr when
          ParsetreeViewer.is_unary_expression expr ||
          ParsetreeViewer.is_binary_expression expr
        -> Parenthesized
      | {pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_fun _}
        when ParsetreeViewer.is_underscore_apply_sugar expr -> Nothing
      | {pexp_desc =
            Pexp_lazy _
          | Pexp_assert _
          | Pexp_fun _
          | Pexp_newtype _
          | Pexp_function _
          | Pexp_constraint _
          | Pexp_setfield _
          | Pexp_extension _ (* readability? maybe remove *)
          | Pexp_match _
          | Pexp_try _
          | Pexp_while _
          | Pexp_for _
          | Pexp_ifthenelse _
        } -> Parenthesized
      | _ -> Nothing
      end

  let binary_expr_operand ~is_lhs expr =
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_fun _}
        when ParsetreeViewer.is_underscore_apply_sugar expr -> Nothing
      | {pexp_desc = Pexp_constraint _ | Pexp_fun _ | Pexp_function _ | Pexp_newtype _} -> Parenthesized
      | expr when ParsetreeViewer.is_binary_expression expr -> Parenthesized
      | expr when ParsetreeViewer.is_ternary_expr expr -> Parenthesized
      | {pexp_desc =
            Pexp_lazy _
          | Pexp_assert _
        } when is_lhs -> Parenthesized
      | _ -> Nothing
      end

  let sub_binary_expr_operand parent_operator child_operator =
    let prec_parent = ParsetreeViewer.operator_precedence parent_operator in
    let prec_child =  ParsetreeViewer.operator_precedence child_operator in
    prec_parent > prec_child ||
    (prec_parent == prec_child &&
    not (ParsetreeViewer.flattenable_operators parent_operator child_operator)) ||
    (* a && b || c, add parens to (a && b) for readability, who knows the difference by heart… *)
    (parent_operator = "||" && child_operator = "&&")

  let rhs_binary_expr_operand parent_operator rhs =
    match rhs.Parsetree.pexp_desc with
    | Parsetree.Pexp_apply(
      {pexp_attributes = [];
        pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [_, _left; _, _right]
      ) when ParsetreeViewer.is_binary_operator operator ->
    let prec_parent = ParsetreeViewer.operator_precedence parent_operator in
    let prec_child =  ParsetreeViewer.operator_precedence operator in
    prec_parent == prec_child
    | _ -> false

  let flatten_operand_rhs parent_operator rhs =
    match rhs.Parsetree.pexp_desc with
    | Parsetree.Pexp_apply(
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [_, _left; _, _right]
      ) when ParsetreeViewer.is_binary_operator operator ->
      let prec_parent = ParsetreeViewer.operator_precedence parent_operator in
      let prec_child =  ParsetreeViewer.operator_precedence operator in
      prec_parent >= prec_child || rhs.pexp_attributes <> []
    | Pexp_constraint (
        {pexp_desc = Pexp_pack _},
        {ptyp_desc = Ptyp_package _}
      ) -> false
    | Pexp_fun _ when ParsetreeViewer.is_underscore_apply_sugar rhs -> false
    | Pexp_fun _
    | Pexp_newtype _
    | Pexp_setfield _
    | Pexp_constraint _ -> true
    | _ when ParsetreeViewer.is_ternary_expr rhs -> true
    | _ -> false

  let lazy_or_assert_expr_rhs expr =
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_attributes = attrs} when
          begin match ParsetreeViewer.filter_parsing_attrs attrs with
          | _::_ -> true
          | [] -> false
          end
          -> Parenthesized
      | expr when ParsetreeViewer.is_binary_expression expr -> Parenthesized
      | {pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_fun _}
        when ParsetreeViewer.is_underscore_apply_sugar expr -> Nothing
      | {pexp_desc =
            Pexp_lazy _
          | Pexp_assert _
          | Pexp_fun _
          | Pexp_newtype _
          | Pexp_function _
          | Pexp_constraint _
          | Pexp_setfield _
          | Pexp_match _
          | Pexp_try _
          | Pexp_while _
          | Pexp_for _
          | Pexp_ifthenelse _
        } -> Parenthesized
      | _ -> Nothing
      end

  let is_negative_constant constant =
    let is_neg txt =
      let len = String.length txt in
      len > 0 && (String.get [@doesNotRaise]) txt 0 = '-'
    in
    match constant with
    | Parsetree.Pconst_integer (i, _) | Pconst_float (i, _) when is_neg i -> true
    | _ -> false

  let field_expr expr =
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_attributes = attrs} when
          begin match ParsetreeViewer.filter_parsing_attrs attrs with
          | _::_ -> true
          | [] -> false
          end
          -> Parenthesized
      | expr when
          ParsetreeViewer.is_binary_expression expr ||
          ParsetreeViewer.is_unary_expression expr
        -> Parenthesized
      | {pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_constant c } when is_negative_constant c -> Parenthesized
      | {pexp_desc = Pexp_fun _}
        when ParsetreeViewer.is_underscore_apply_sugar expr -> Nothing
      | {pexp_desc =
            Pexp_lazy _
          | Pexp_assert _
          | Pexp_extension _ (* %extension.x vs (%extension).x *)
          | Pexp_fun _
          | Pexp_newtype _
          | Pexp_function _
          | Pexp_constraint _
          | Pexp_setfield _
          | Pexp_match _
          | Pexp_try _
          | Pexp_while _
          | Pexp_for _
          | Pexp_ifthenelse _
        } -> Parenthesized
      | _ -> Nothing
      end

  let set_field_expr_rhs expr =
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_constraint _ } -> Parenthesized
      | _ -> Nothing
      end

  let ternary_operand expr =
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_constraint _ } -> Parenthesized
      | {pexp_desc = Pexp_fun _ | Pexp_newtype _} ->
        let (_attrsOnArrow, _parameters, return_expr) = ParsetreeViewer.fun_expr expr in
        begin match return_expr.pexp_desc with
        | Pexp_constraint _ -> Parenthesized
        | _ -> Nothing
        end
      | _ -> Nothing
      end

  let starts_with_minus txt =
    let len = String.length txt in
    if len == 0 then
      false
    else
      let s = (String.get [@doesNotRaise]) txt 0 in
      s = '-'

  let jsx_prop_expr expr =
    match expr.Parsetree.pexp_desc with
    | Parsetree.Pexp_let _
    | Pexp_sequence _
    | Pexp_letexception _
    | Pexp_letmodule _
    | Pexp_open _ -> Nothing
    | _ ->
      let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
      begin match opt_braces with
      | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
      | None ->
        begin match expr with
        | {Parsetree.pexp_desc =
            Pexp_constant (Pconst_integer (x, _) | Pconst_float (x, _));
            pexp_attributes = []}
          when starts_with_minus x -> Parenthesized
        | {Parsetree.pexp_desc =
            Pexp_ident _ | Pexp_constant _ | Pexp_field _ | Pexp_construct _ | Pexp_variant _ |
            Pexp_array _ | Pexp_pack _ | Pexp_record _ | Pexp_extension _ |
            Pexp_letmodule _ | Pexp_letexception _ | Pexp_open _ | Pexp_sequence _ |
            Pexp_let _ | Pexp_tuple _;
           pexp_attributes = []
          } -> Nothing
        | {Parsetree.pexp_desc = Pexp_constraint (
            {pexp_desc = Pexp_pack _},
            {ptyp_desc = Ptyp_package _}
          ); pexp_attributes = []} -> Nothing
        | _ -> Parenthesized
        end
      end

  let jsx_child_expr expr =
    match expr.Parsetree.pexp_desc with
    | Parsetree.Pexp_let _
    | Pexp_sequence _
    | Pexp_letexception _
    | Pexp_letmodule _
    | Pexp_open _ -> Nothing
    | _ ->
      let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
      begin match opt_braces with
      | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
      | _ ->
        begin match expr with
        | {Parsetree.pexp_desc = Pexp_constant (Pconst_integer (x, _) | Pconst_float (x, _));
           pexp_attributes = []
          } when starts_with_minus x -> Parenthesized
        | {Parsetree.pexp_desc =
            Pexp_ident _ | Pexp_constant _ | Pexp_field _ | Pexp_construct _ | Pexp_variant _ |
            Pexp_array _ | Pexp_pack _ | Pexp_record _ | Pexp_extension _ |
            Pexp_letmodule _ | Pexp_letexception _ | Pexp_open _ | Pexp_sequence _ |
            Pexp_let _;
            pexp_attributes = []
          } -> Nothing
        | {Parsetree.pexp_desc = Pexp_constraint (
            {pexp_desc = Pexp_pack _},
            {ptyp_desc = Ptyp_package _}
           ); pexp_attributes = []} -> Nothing
        | expr when ParsetreeViewer.is_jsx_expression expr -> Nothing
        | _ -> Parenthesized
        end
      end

  let binary_expr expr =
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced(braces_loc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_attributes = _::_} as expr
        when ParsetreeViewer.is_binary_expression expr -> Parenthesized
      | _ -> Nothing
      end

  let mod_type_functor_return mod_type = match mod_type with
    | {Parsetree.pmty_desc = Pmty_with _} -> true
    | _ -> false

  (* Add parens for readability:
       module type Functor = SetLike => Set with type t = A.t
     This is actually:
       module type Functor = (SetLike => Set) with type t = A.t
  *)
  let mod_type_with_operand mod_type = match mod_type with
    | {Parsetree.pmty_desc = Pmty_functor _} -> true
    | _ -> false

  let mod_expr_functor_constraint mod_type = match mod_type with
    | {Parsetree.pmty_desc = Pmty_functor _ | Pmty_with _} -> true
    | _ -> false

  let braced_expr expr = match expr.Parsetree.pexp_desc with
    | Pexp_constraint (
        {pexp_desc = Pexp_pack _},
        {ptyp_desc = Ptyp_package _}
      ) -> false
    | Pexp_constraint _ -> true
    | _ -> false

  let include_mod_expr mod_expr = match mod_expr.Parsetree.pmod_desc with
  | Parsetree.Pmod_constraint _ -> true
  | _ -> false
end

module CommentTable = struct
  type t = {
    leading: (Location.t, Comment.t list) Hashtbl.t;
    inside: (Location.t, Comment.t list) Hashtbl.t;
    trailing: (Location.t, Comment.t list) Hashtbl.t;
  }

  let make () = {
    leading = Hashtbl.create 100;
    inside = Hashtbl.create 100;
    trailing = Hashtbl.create 100;
  }

  let empty = make ()

  let log t =
    let open Location in
    let leading_stuff = Hashtbl.fold (fun (k : Location.t) (v : Comment.t list) acc ->
      let loc = Doc.concat [
        Doc.lbracket;
        Doc.text (string_of_int k.loc_start.pos_lnum);
        Doc.text ":";
        Doc.text (string_of_int (k.loc_start.pos_cnum  - k.loc_start.pos_bol));
        Doc.text "-";
        Doc.text (string_of_int k.loc_end.pos_lnum);
        Doc.text ":";
        Doc.text (string_of_int (k.loc_end.pos_cnum  - k.loc_end.pos_bol));
        Doc.rbracket;
      ] in
      let doc = Doc.breakable_group ~force_break:true (
        Doc.concat [
          loc;
          Doc.indent (
            Doc.concat [
              Doc.line;
              Doc.join ~sep:Doc.comma (List.map (fun c -> Doc.text (Comment.txt c)) v)
            ]
          );
          Doc.line;
        ]
      ) in
      doc::acc
    ) t.leading []
    in
    let trailing_stuff = Hashtbl.fold (fun (k : Location.t) (v : Comment.t list) acc ->
      let loc = Doc.concat [
        Doc.lbracket;
        Doc.text (string_of_int k.loc_start.pos_lnum);
        Doc.text ":";
        Doc.text (string_of_int (k.loc_start.pos_cnum  - k.loc_start.pos_bol));
        Doc.text "-";
        Doc.text (string_of_int k.loc_end.pos_lnum);
        Doc.text ":";
        Doc.text (string_of_int (k.loc_end.pos_cnum  - k.loc_end.pos_bol));
        Doc.rbracket;
      ] in
      let doc = Doc.breakable_group ~force_break:true (
        Doc.concat [
          loc;
          Doc.indent (
            Doc.concat [
              Doc.line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (List.map (fun c -> Doc.text (Comment.txt c)) v)
            ]
          );
          Doc.line;
        ]
      ) in
      doc::acc
    ) t.trailing []
    in
    Doc.breakable_group ~force_break:true (
      Doc.concat [
        Doc.text "leading comments:";
        Doc.line;
        Doc.indent (Doc.concat leading_stuff);
        Doc.line;
        Doc.line;
        Doc.text "trailing comments:";
        Doc.indent (Doc.concat trailing_stuff);
        Doc.line;
        Doc.line;
      ]
    ) |> Doc.to_string ~width:80 |> print_endline
    [@@live]
  let attach tbl loc comments =
    match comments with
    | [] -> ()
    | comments -> Hashtbl.replace tbl loc comments

  let partition_by_loc comments loc =
    let rec loop (leading, inside, trailing)  comments =
      let open Location in
      match comments with
      | comment::rest ->
        let cmt_loc = Comment.loc comment in
        if cmt_loc.loc_end.pos_cnum <= loc.loc_start.pos_cnum then
          loop (comment::leading, inside, trailing) rest
        else if cmt_loc.loc_start.pos_cnum >= loc.loc_end.pos_cnum then
          loop (leading, inside, comment::trailing) rest
        else
          loop (leading, comment::inside, trailing) rest
      | [] -> (List.rev leading, List.rev inside, List.rev trailing)
    in
    loop ([], [], []) comments

  let partition_leading_trailing comments loc =
    let rec loop (leading, trailing)  comments =
      let open Location in
      match comments with
      | comment::rest ->
        let cmt_loc = Comment.loc comment in
        if cmt_loc.loc_end.pos_cnum <= loc.loc_start.pos_cnum then
          loop (comment::leading, trailing) rest
        else
          loop (leading, comment::trailing) rest
      | [] -> (List.rev leading, List.rev trailing)
    in
    loop ([], []) comments

  let partition_by_on_same_line loc comments =
    let rec loop (on_same_line, on_other_line) comments =
      let open Location in
      match comments with
      | [] -> (List.rev on_same_line, List.rev on_other_line)
      | comment::rest ->
        let cmt_loc = Comment.loc comment in
        if cmt_loc.loc_start.pos_lnum == loc.loc_end.pos_lnum then
          loop (comment::on_same_line, on_other_line) rest
        else
          loop (on_same_line, comment::on_other_line) rest
    in
    loop ([], []) comments

  let partition_adjacent_trailing loc1 comments =
    let open Location in
    let open Lexing in
    let rec loop ~prev_end_pos after_loc1 comments =
      match comments with
      | [] -> (List.rev after_loc1, [])
      | (comment::rest) as comments ->
        let cmt_prev_end_pos = Comment.prev_tok_end_pos comment in
        if prev_end_pos.Lexing.pos_cnum == cmt_prev_end_pos.pos_cnum then
          let comment_end = (Comment.loc comment).loc_end in
          loop ~prev_end_pos:comment_end (comment::after_loc1) rest
        else
          (List.rev after_loc1, comments)
    in
    loop ~prev_end_pos:loc1.loc_end [] comments

  let rec collect_list_patterns acc pattern =
    let open Parsetree in
    match pattern.ppat_desc with
    | Ppat_construct(
        {txt = Longident.Lident "::"},
        Some {ppat_desc=Ppat_tuple (pat::rest::[])}
      ) ->
      collect_list_patterns (pat::acc) rest
    | Ppat_construct ({txt = Longident.Lident "[]"}, None) ->
      List.rev acc
    | _ -> List.rev (pattern::acc)

  let rec collect_list_exprs acc expr =
    let open Parsetree in
    match expr.pexp_desc with
    | Pexp_construct(
        {txt = Longident.Lident "::"},
        Some {pexp_desc=Pexp_tuple (expr::rest::[])}
      ) ->
      collect_list_exprs (expr::acc) rest
    | Pexp_construct ({txt = Longident.Lident "[]"}, _) ->
      List.rev acc
    | _ -> List.rev (expr::acc)

  (* TODO: use ParsetreeViewer *)
  let arrow_type ct =
    let open Parsetree in
    let rec process attrs_before acc typ = match typ with
    | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = []} ->
      let arg = ([], lbl, typ1) in
      process attrs_before (arg::acc) typ2
    | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = [({txt ="bs"}, _) ] as attrs} ->
      let arg = (attrs, lbl, typ1) in
      process attrs_before (arg::acc) typ2
    | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = _attrs} as return_type ->
      let args = List.rev acc in
      (attrs_before, args, return_type)
    | {ptyp_desc = Ptyp_arrow ((Labelled _ | Optional _) as lbl, typ1, typ2); ptyp_attributes = attrs} ->
      let arg = (attrs, lbl, typ1) in
      process attrs_before (arg::acc) typ2
    | typ ->
      (attrs_before, List.rev acc, typ)
    in
    begin match ct with
    | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = attrs} as typ ->
      process attrs [] {typ with ptyp_attributes = []}
    | typ -> process [] [] typ
    end

  (* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? *)
  let mod_expr_apply mod_expr =
    let rec loop acc mod_expr = match mod_expr with
    | {Parsetree.pmod_desc = Pmod_apply (next, arg)} ->
      loop (arg::acc) next
    | _ -> (mod_expr::acc)
    in
    loop [] mod_expr

  (* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? *)
  let mod_expr_functor mod_expr =
    let rec loop acc mod_expr = match mod_expr with
    | {Parsetree.pmod_desc = Pmod_functor (lbl, mod_type, return_mod_expr); pmod_attributes = attrs} ->
      let param = (attrs, lbl, mod_type) in
      loop (param::acc) return_mod_expr
    | return_mod_expr ->
      (List.rev acc, return_mod_expr)
    in
    loop [] mod_expr

  let functor_type modtype =
    let rec process acc modtype = match modtype with
    | {Parsetree.pmty_desc = Pmty_functor (lbl, arg_type, return_type); pmty_attributes = attrs} ->
      let arg = (attrs, lbl, arg_type) in
      process (arg::acc) return_type
    | mod_type ->
      (List.rev acc, mod_type)
    in
    process [] modtype

  let fun_expr expr =
    let open Parsetree in
    (* Turns (type t, type u, type z) into "type t u z" *)
    let rec collect_new_types acc return_expr =
      match return_expr with
      | {pexp_desc = Pexp_newtype (string_loc, return_expr); pexp_attributes = []} ->
        collect_new_types (string_loc::acc) return_expr
      | return_expr ->
        let loc = match (acc, List.rev acc) with
        | (_startLoc::_, end_loc::_) -> { end_loc.loc with loc_end = end_loc.loc.loc_end }
        | _ -> Location.none
        in
        let txt = List.fold_right (fun curr acc -> acc ^ " " ^ curr.Location.txt) acc "type" in
        (Location.mkloc txt loc, return_expr)
    in
    (* For simplicity reason Pexp_newtype gets converted to a Nolabel parameter,
     * otherwise this function would need to return a variant:
     * | NormalParamater(...)
     * | NewType(...)
     * This complicates printing with an extra variant/boxing/allocation for a code-path
     * that is not often used. Lets just keep it simple for now *)
    let rec collect attrs_before acc expr = match expr with
    | {pexp_desc = Pexp_fun (lbl, default_expr, pattern, return_expr); pexp_attributes = []} ->
      let parameter = ([], lbl, default_expr, pattern) in
      collect attrs_before (parameter::acc) return_expr
    | {pexp_desc = Pexp_newtype (string_loc, rest); pexp_attributes = attrs} ->
      let (var, return_expr) = collect_new_types [string_loc] rest in
      let parameter = (
        attrs,
        Asttypes.Nolabel,
        None,
        Ast_helper.Pat.var ~loc:string_loc.loc var
      ) in
      collect attrs_before (parameter::acc) return_expr
    | {pexp_desc = Pexp_fun (lbl, default_expr, pattern, return_expr); pexp_attributes = [({txt = "bs"}, _)] as attrs} ->
      let parameter = (attrs, lbl, default_expr, pattern) in
      collect attrs_before (parameter::acc) return_expr
    | {
        pexp_desc = Pexp_fun ((Labelled _ | Optional _) as lbl, default_expr, pattern, return_expr);
        pexp_attributes = attrs
      } ->
      let parameter = (attrs, lbl, default_expr, pattern) in
      collect attrs_before (parameter::acc) return_expr
    | expr ->
      (attrs_before, List.rev acc, expr)
    in
    begin match expr with
    | {pexp_desc = Pexp_fun (Nolabel, _defaultExpr, _pattern, _returnExpr); pexp_attributes = attrs} as expr ->
      collect attrs [] {expr with pexp_attributes = []}
    | expr -> collect [] [] expr
    end

  let rec is_block_expr expr =
    let open Parsetree in
    match expr.pexp_desc with
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_let _
    | Pexp_open _
    | Pexp_sequence _ -> true
    | Pexp_apply (call_expr, _) when is_block_expr call_expr -> true
    | Pexp_constraint (expr, _) when is_block_expr expr -> true
    | Pexp_field (expr, _) when is_block_expr expr -> true
    | Pexp_setfield (expr, _, _) when is_block_expr expr -> true
    | _ -> false

  let rec walk_structure s t comments =
    match s with
    | _ when comments = [] -> ()
    | [] -> attach t.inside Location.none comments
    | s ->
      walk_list
        ~get_loc:(fun n -> n.Parsetree.pstr_loc)
        ~walk_node:walk_structure_item
        s
        t
        comments

    and walk_structure_item si t comments =
      match si.Parsetree.pstr_desc with
      | _ when comments = [] -> ()
      | Pstr_primitive value_description ->
        walk_value_description value_description t comments
      | Pstr_open open_description ->
        walk_open_description open_description t comments
      | Pstr_value (_, value_bindings) ->
        walk_value_bindings value_bindings t comments
      | Pstr_type (_, type_declarations) ->
        walk_type_declarations type_declarations t comments
      | Pstr_eval (expr, _) ->
        walk_expr expr t comments
      | Pstr_module module_binding ->
        walk_module_binding module_binding t comments
      | Pstr_recmodule module_bindings ->
        walk_list
          ~get_loc:(fun mb -> mb.Parsetree.pmb_loc)
          ~walk_node:walk_module_binding
          module_bindings
          t
          comments
      | Pstr_modtype mod_typ_decl ->
        walk_module_type_declaration mod_typ_decl t comments
      | Pstr_attribute attribute ->
        walk_attribute attribute t comments
      | Pstr_extension (extension, _) ->
        walk_extension extension t comments
      | Pstr_include include_declaration ->
        walk_include_declaration include_declaration t comments
      | Pstr_exception extension_constructor ->
        walk_ext_constr extension_constructor t comments
      | Pstr_typext type_extension ->
        walk_type_extension type_extension t comments
      | Pstr_class_type _  | Pstr_class _ -> ()

    and walk_value_description vd t comments =
      let (leading, trailing) =
        partition_leading_trailing comments vd.pval_name.loc in
      attach t.leading vd.pval_name.loc leading;
      let (after_name, rest) =
        partition_adjacent_trailing vd.pval_name.loc trailing in
      attach t.trailing vd.pval_name.loc after_name;
      let (before, inside, after) =
        partition_by_loc rest vd.pval_type.ptyp_loc
      in
      attach t.leading vd.pval_type.ptyp_loc before;
      walk_typ_expr vd.pval_type t inside;
      attach t.trailing vd.pval_type.ptyp_loc after

    and walk_type_extension te t comments =
      let (leading, trailing) =
        partition_leading_trailing comments te.ptyext_path.loc in
      attach t.leading te.ptyext_path.loc leading;
      let (after_path, rest) =
        partition_adjacent_trailing te.ptyext_path.loc trailing in
      attach t.trailing te.ptyext_path.loc after_path;

      (* type params *)
      let rest = match te.ptyext_params with
      | [] -> rest
      | type_params ->
        visit_list_but_continue_with_remaining_comments
          ~get_loc:(fun (typexpr, _variance) -> typexpr.Parsetree.ptyp_loc)
          ~walk_node:walk_type_param
          ~newline_delimited:false
          type_params
          t
          rest
      in
      walk_list
        ~get_loc:(fun n -> n.Parsetree.pext_loc)
        ~walk_node:walk_ext_constr
        te.ptyext_constructors
        t
        rest

    and walk_include_declaration incl_decl t comments =
      let (before, inside, after) =
        partition_by_loc comments incl_decl.pincl_mod.pmod_loc in
      attach t.leading incl_decl.pincl_mod.pmod_loc before;
      walk_mod_expr incl_decl.pincl_mod t inside;
      attach t.trailing incl_decl.pincl_mod.pmod_loc after

    and walk_module_type_declaration mtd t comments =
      let (leading, trailing) =
        partition_leading_trailing comments mtd.pmtd_name.loc in
      attach t.leading mtd.pmtd_name.loc leading;
      begin match mtd.pmtd_type with
      | None ->
        attach t.trailing mtd.pmtd_name.loc trailing
      | Some mod_type ->
        let (after_name, rest) = partition_adjacent_trailing mtd.pmtd_name.loc trailing in
        attach t.trailing mtd.pmtd_name.loc after_name;
        let (before, inside, after) = partition_by_loc rest mod_type.pmty_loc in
        attach t.leading mod_type.pmty_loc before;
        walk_mod_type mod_type t inside;
        attach t.trailing mod_type.pmty_loc after
      end

    and walk_module_binding mb t comments =
      let (leading, trailing) = partition_leading_trailing comments mb.pmb_name.loc in
      attach t.leading mb.pmb_name.loc leading;
      let (after_name, rest) = partition_adjacent_trailing mb.pmb_name.loc trailing in
      attach t.trailing mb.pmb_name.loc after_name;
      let (leading, inside, trailing) = partition_by_loc rest mb.pmb_expr.pmod_loc in
      begin match mb.pmb_expr.pmod_desc with
      | Pmod_constraint _ ->
        walk_mod_expr mb.pmb_expr t (List.concat [leading; inside]);
      | _ ->
        attach t.leading mb.pmb_expr.pmod_loc leading;
        walk_mod_expr mb.pmb_expr t inside;
      end;
      attach t.trailing mb.pmb_expr.pmod_loc trailing

   and walk_signature signature t comments =
      match signature with
      | _ when comments = [] -> ()
      | [] -> attach t.inside Location.none comments
      | _s ->
        walk_list
          ~get_loc:(fun n -> n.Parsetree.psig_loc)
          ~walk_node:walk_signature_item
          signature
          t
          comments

    and walk_signature_item si t comments =
      match si.psig_desc with
      | _ when comments = [] -> ()
      | Psig_value value_description ->
        walk_value_description value_description t comments
      | Psig_type (_, type_declarations) ->
        walk_type_declarations type_declarations t comments
      | Psig_typext type_extension ->
        walk_type_extension type_extension t comments
      | Psig_exception extension_constructor ->
        walk_ext_constr extension_constructor t comments
      | Psig_module module_declaration ->
        walk_module_declaration module_declaration t comments
      | Psig_recmodule module_declarations ->
        walk_list
          ~get_loc:(fun n -> n.Parsetree.pmd_loc)
          ~walk_node:walk_module_declaration
          module_declarations
          t
          comments
      | Psig_modtype module_type_declaration ->
        walk_module_type_declaration module_type_declaration t comments
      | Psig_open open_description ->
        walk_open_description open_description t comments
      | Psig_include include_description ->
        walk_include_description include_description t comments
      | Psig_attribute attribute ->
        walk_attribute attribute t comments
      | Psig_extension (extension, _) ->
        walk_extension extension t comments
      | Psig_class _ | Psig_class_type _ -> ()

    and walk_include_description id t comments =
      let (before, inside, after) =
        partition_by_loc comments id.pincl_mod.pmty_loc in
      attach t.leading id.pincl_mod.pmty_loc before;
      walk_mod_type id.pincl_mod t inside;
      attach t.trailing id.pincl_mod.pmty_loc after

    and walk_module_declaration md t comments =
      let (leading, trailing) = partition_leading_trailing comments md.pmd_name.loc in
      attach t.leading md.pmd_name.loc leading;
      let (after_name, rest) = partition_adjacent_trailing md.pmd_name.loc trailing in
      attach t.trailing md.pmd_name.loc after_name;
      let (leading, inside, trailing) = partition_by_loc rest md.pmd_type.pmty_loc in
      attach t.leading md.pmd_type.pmty_loc leading;
      walk_mod_type md.pmd_type t inside;
      attach t.trailing md.pmd_type.pmty_loc trailing

    and walk_list:
      'node.
      ?prev_loc:Location.t ->
      get_loc:('node -> Location.t) ->
      walk_node:('node -> t -> Comment.t list -> unit) ->
      'node list -> t -> Comment.t list -> unit
      = fun ?prev_loc ~get_loc ~walk_node l t comments ->
      let open Location in
      match l with
      | _ when comments = [] -> ()
      | [] ->
        begin match prev_loc with
        | Some loc ->
          attach t.trailing loc comments
        | None -> ()
        end
      | node::rest ->
        let curr_loc = get_loc node in
        let (leading, inside, trailing) = partition_by_loc comments curr_loc in
        begin match prev_loc with
        | None -> (* first node, all leading comments attach here *)
          attach t.leading curr_loc leading
        | Some prev_loc ->
          (* Same line *)
          if prev_loc.loc_end.pos_lnum == curr_loc.loc_start.pos_lnum then
            let (after_prev, before_curr) = partition_adjacent_trailing prev_loc leading in
            let () = attach t.trailing prev_loc after_prev in
            attach t.leading curr_loc before_curr
          else
            let (on_same_line_as_prev, after_prev) = partition_by_on_same_line prev_loc leading in
            let () = attach t.trailing prev_loc on_same_line_as_prev in
            let (leading, _inside, _trailing) = partition_by_loc after_prev curr_loc in
            attach t.leading curr_loc leading
        end;
        walk_node node t inside;
        walk_list ~prev_loc:curr_loc ~get_loc ~walk_node rest t trailing

    (* The parsetree doesn't always contain location info about the opening or
     * closing token of a "list-of-things". This routine visits the whole list,
     * but returns any remaining comments that likely fall after the whole list. *)
    and visit_list_but_continue_with_remaining_comments:
      'node.
      ?prev_loc:Location.t ->
      newline_delimited:bool ->
      get_loc:('node -> Location.t) ->
      walk_node:('node -> t -> Comment.t list -> unit) ->
      'node list -> t -> Comment.t list -> Comment.t list
      = fun ?prev_loc ~newline_delimited ~get_loc ~walk_node l t comments ->
      let open Location in
      match l with
      | _ when comments = [] -> []
      | [] ->
        begin match prev_loc with
        | Some loc ->
          let (after_prev, rest) =
            if newline_delimited then
              partition_by_on_same_line loc comments
            else
              partition_adjacent_trailing loc comments
          in
          attach t.trailing loc after_prev;
          rest
        | None -> comments
        end
      | node::rest ->
        let curr_loc = get_loc node in
        let (leading, inside, trailing) = partition_by_loc comments curr_loc in
        let () = match prev_loc with
        | None -> (* first node, all leading comments attach here *)
          attach t.leading curr_loc leading;
          ()
        | Some prev_loc ->
          (* Same line *)
          if prev_loc.loc_end.pos_lnum == curr_loc.loc_start.pos_lnum then
            let (after_prev, before_curr) = partition_adjacent_trailing prev_loc leading in
            let () = attach t.trailing prev_loc after_prev in
            let () = attach t.leading curr_loc before_curr in
            ()
          else
            let (on_same_line_as_prev, after_prev) = partition_by_on_same_line prev_loc leading in
            let () = attach t.trailing prev_loc on_same_line_as_prev in
            let (leading, _inside, _trailing) = partition_by_loc after_prev curr_loc in
            let () = attach t.leading curr_loc leading in
            ()
        in
        walk_node node t inside;
        visit_list_but_continue_with_remaining_comments
          ~prev_loc:curr_loc ~get_loc ~walk_node ~newline_delimited
          rest t trailing

    and walk_value_bindings vbs t comments =
      walk_list
        ~get_loc:(fun n -> n.Parsetree.pvb_loc)
        ~walk_node:walk_value_binding
        vbs
        t
        comments

    and walk_open_description open_description t comments =
      let loc = open_description.popen_lid.loc in
      let (leading, trailing) = partition_leading_trailing comments loc in
      attach t.leading loc leading;
      attach t.trailing loc trailing;

    and walk_type_declarations type_declarations t comments =
      walk_list
        ~get_loc:(fun n -> n.Parsetree.ptype_loc)
        ~walk_node:walk_type_declaration
        type_declarations
        t
        comments

    and walk_type_param (typexpr, _variance) t comments =
      walk_typ_expr typexpr t comments

    and walk_type_declaration td t comments =
      let (before_name, rest) =
        partition_leading_trailing comments td.ptype_name.loc in
      attach t.leading td.ptype_name.loc before_name;

      let (after_name, rest) =
        partition_adjacent_trailing td.ptype_name.loc rest in
      attach t.trailing td.ptype_name.loc after_name;

      (* type params *)
      let rest = match td.ptype_params with
      | [] -> rest
      | type_params ->
        visit_list_but_continue_with_remaining_comments
          ~get_loc:(fun (typexpr, _variance) -> typexpr.Parsetree.ptyp_loc)
          ~walk_node:walk_type_param
          ~newline_delimited:false
          type_params
          t
          rest
      in

      (* manifest:  = typexpr *)
      let rest = match td.ptype_manifest with
      | Some typexpr ->
        let (before_typ, inside_typ, after_typ) =
          partition_by_loc rest typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc before_typ;
        walk_typ_expr typexpr t inside_typ;
        let (after_typ, rest) =
          partition_adjacent_trailing typexpr.Parsetree.ptyp_loc after_typ in
        attach t.trailing typexpr.ptyp_loc after_typ;
        rest
      | None -> rest
      in

      let rest = match td.ptype_kind with
      | Ptype_abstract | Ptype_open -> rest
      | Ptype_record label_declarations ->
        let () = walk_list
          ~get_loc:(fun ld -> ld.Parsetree.pld_loc)
          ~walk_node:walk_label_declaration
          label_declarations
          t
          rest
        in
        []
      | Ptype_variant constructor_declarations ->
        walk_constructor_declarations constructor_declarations t rest
      in
      attach t.trailing td.ptype_loc rest

    and walk_label_declarations lds t comments =
      visit_list_but_continue_with_remaining_comments
        ~get_loc:(fun ld -> ld.Parsetree.pld_loc)
        ~walk_node:walk_label_declaration
        ~newline_delimited:false
        lds
        t
        comments

    and walk_label_declaration ld t comments =
      let (before_name, rest) =
        partition_leading_trailing comments ld.pld_name.loc  in
      attach t.leading ld.pld_name.loc before_name;
      let (after_name, rest) = partition_adjacent_trailing ld.pld_name.loc rest in
      attach t.trailing ld.pld_name.loc after_name;
      let (before_typ, inside_typ, after_typ) =
        partition_by_loc rest ld.pld_type.ptyp_loc in
      attach t.leading ld.pld_type.ptyp_loc before_typ;
      walk_typ_expr ld.pld_type t inside_typ;
      attach t.trailing ld.pld_type.ptyp_loc after_typ

    and walk_constructor_declarations cds t comments =
      visit_list_but_continue_with_remaining_comments
        ~get_loc:(fun cd -> cd.Parsetree.pcd_loc)
        ~walk_node:walk_constructor_declaration
        ~newline_delimited:false
        cds
        t
        comments

    and walk_constructor_declaration cd t comments =
      let (before_name, rest) =
        partition_leading_trailing comments cd.pcd_name.loc  in
      attach t.leading cd.pcd_name.loc before_name;
      let (after_name, rest) =
        partition_adjacent_trailing cd.pcd_name.loc rest in
      attach t.trailing cd.pcd_name.loc after_name;
      let rest = walk_constructor_arguments cd.pcd_args t rest in

      let rest = match cd.pcd_res with
      | Some typexpr ->
        let (before_typ, inside_typ, after_typ) =
          partition_by_loc rest typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc before_typ;
        walk_typ_expr typexpr t inside_typ;
        let (after_typ, rest) =
          partition_adjacent_trailing typexpr.Parsetree.ptyp_loc after_typ in
        attach t.trailing typexpr.ptyp_loc after_typ;
        rest
      | None -> rest
      in
      attach t.trailing cd.pcd_loc rest

    and walk_constructor_arguments args t comments =
      match args with
      | Pcstr_tuple typexprs ->
        visit_list_but_continue_with_remaining_comments
          ~get_loc:(fun n -> n.Parsetree.ptyp_loc)
          ~walk_node:walk_typ_expr
          ~newline_delimited:false
          typexprs
          t
          comments
      | Pcstr_record label_declarations ->
        walk_label_declarations label_declarations t comments

    and walk_value_binding vb t comments =
      let open Location in

      let vb =
        let open Parsetree in
        match (vb.pvb_pat, vb.pvb_expr) with
        | {ppat_desc = Ppat_constraint (pat, {ptyp_desc = Ptyp_poly ([], t)})},
          {pexp_desc = Pexp_constraint (expr, _typ)} ->
          {vb with
            pvb_pat = Ast_helper.Pat.constraint_
              ~loc:{pat.ppat_loc with loc_end = t.Parsetree.ptyp_loc.loc_end} pat t;
            pvb_expr = expr;
          }
        | {ppat_desc = Ppat_constraint (pat, {ptyp_desc = Ptyp_poly (_::_, t)})},
          {pexp_desc = Pexp_fun _} ->
          {vb with
            pvb_pat = {vb.pvb_pat with
              ppat_loc = {pat.ppat_loc with loc_end = t.ptyp_loc.loc_end}}}
        | _ -> vb
      in
      let pattern_loc = vb.Parsetree.pvb_pat.ppat_loc in
      let expr_loc = vb.Parsetree.pvb_expr.pexp_loc in

      let (leading, inside, trailing) =
        partition_by_loc comments pattern_loc in

      (* everything before start of pattern can only be leading on the pattern:
       *   let |* before *| a = 1 *)
      attach t.leading pattern_loc leading;
      walk_pattern vb.Parsetree.pvb_pat t inside;
      (* let pattern = expr     -> pattern and expr on the same line *)
      (* if patternLoc.loc_end.pos_lnum == exprLoc.loc_start.pos_lnum then ( *)
        let (after_pat, surrounding_expr) =
          partition_adjacent_trailing pattern_loc trailing
        in
        attach t.trailing pattern_loc after_pat;
        let (before_expr, inside_expr, after_expr) =
          partition_by_loc surrounding_expr expr_loc in
        if is_block_expr vb.pvb_expr then (
          walk_expr vb.pvb_expr t (List.concat [before_expr; inside_expr; after_expr])
        ) else (
          attach t.leading expr_loc before_expr;
          walk_expr vb.Parsetree.pvb_expr t inside_expr;
          attach t.trailing expr_loc after_expr
        )

    and walk_expr expr t comments =
      let open Location in
      match expr.Parsetree.pexp_desc with
      | _ when comments = [] -> ()
      | Pexp_constant _ ->
        let (leading, trailing) =
          partition_leading_trailing comments expr.pexp_loc in
        attach t.leading expr.pexp_loc leading;
        attach t.trailing expr.pexp_loc trailing;
      | Pexp_ident longident ->
        let (leading, trailing) =
          partition_leading_trailing comments longident.loc in
        attach t.leading longident.loc leading;
        attach t.trailing longident.loc trailing;
      | Pexp_let (_recFlag, value_bindings, expr2) ->
        let comments = visit_list_but_continue_with_remaining_comments
          ~get_loc:(fun n ->
            if n.Parsetree.pvb_pat.ppat_loc.loc_ghost then
              n.pvb_expr.pexp_loc
            else
             n.Parsetree.pvb_loc
          )
          ~walk_node:walk_value_binding
          ~newline_delimited:true
          value_bindings
          t
          comments
        in
        if is_block_expr expr2 then (
          walk_expr expr2 t comments;
        ) else (
          let (leading, inside, trailing) = partition_by_loc comments expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walk_expr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_sequence (expr1, expr2) ->
        let (leading, inside, trailing) = partition_by_loc comments expr1.pexp_loc in
        let comments = if is_block_expr expr1 then (
          let (after_expr, comments) = partition_by_on_same_line expr1.pexp_loc trailing in
          walk_expr expr1 t (List.concat [leading; inside; after_expr]);
          comments
        ) else (
          attach t.leading expr1.pexp_loc leading;
          walk_expr expr1 t inside;
          let (after_expr, comments) = partition_by_on_same_line expr1.pexp_loc trailing in
          attach t.trailing expr1.pexp_loc after_expr;
          comments
        ) in
        if is_block_expr expr2 then (
          walk_expr expr2 t comments
        ) else (
          let (leading, inside, trailing) = partition_by_loc comments expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walk_expr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_open (_override, longident, expr2) ->
        let (leading, comments) =
          partition_leading_trailing comments expr.pexp_loc in
        attach
          t.leading
          {expr.pexp_loc with loc_end = longident.loc.loc_end}
          leading;
        let (leading, trailing) =
          partition_leading_trailing comments longident.loc in
        attach t.leading longident.loc leading;
        let (after_longident, rest) =
          partition_by_on_same_line longident.loc trailing in
        attach t.trailing longident.loc after_longident;
        if is_block_expr expr2 then (
          walk_expr expr2 t rest
        ) else (
          let (leading, inside, trailing) = partition_by_loc rest expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walk_expr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_extension (
          {txt = "obj"},
          PStr [{
            pstr_desc = Pstr_eval({pexp_desc = Pexp_record (rows, _)}, [])
          }]
        ) ->
        walk_list
          ~get_loc:(fun (
              (longident, expr): (Longident.t Asttypes.loc * Parsetree.expression)
            ) -> {
            longident.loc with loc_end = expr.pexp_loc.loc_end
          })
          ~walk_node:walk_expr_record_row
          rows
          t
          comments
      | Pexp_extension extension ->
        walk_extension extension t comments
      | Pexp_letexception (extension_constructor, expr2) ->
        let (leading, comments) =
          partition_leading_trailing comments expr.pexp_loc in
        attach
          t.leading
          {expr.pexp_loc with loc_end = extension_constructor.pext_loc.loc_end}
          leading;
        let (leading, inside, trailing) =
          partition_by_loc comments extension_constructor.pext_loc in
        attach t.leading extension_constructor.pext_loc leading;
        walk_ext_constr extension_constructor t inside;
        let (after_ext_constr, rest) =
          partition_by_on_same_line extension_constructor.pext_loc trailing in
        attach t.trailing extension_constructor.pext_loc after_ext_constr;
        if is_block_expr expr2 then (
          walk_expr expr2 t rest
        ) else (
          let (leading, inside, trailing) = partition_by_loc rest expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walk_expr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_letmodule (string_loc, mod_expr, expr2) ->
        let (leading, comments) =
          partition_leading_trailing comments expr.pexp_loc in
        attach t.leading {expr.pexp_loc with loc_end = mod_expr.pmod_loc.loc_end} leading;
        let (leading, trailing) = partition_leading_trailing comments string_loc.loc in
        attach t.leading string_loc.loc leading;
        let (after_string, rest) =
          partition_adjacent_trailing string_loc.loc trailing in
        attach t.trailing string_loc.loc after_string;
        let (before_mod_expr, inside_mod_expr, after_mod_expr) =
          partition_by_loc rest mod_expr.pmod_loc in
        attach t.leading mod_expr.pmod_loc before_mod_expr;
        walk_mod_expr mod_expr t inside_mod_expr;
        let (after_mod_expr, rest) =
          partition_by_on_same_line mod_expr.pmod_loc after_mod_expr in
        attach t.trailing mod_expr.pmod_loc after_mod_expr;
        if is_block_expr expr2 then (
          walk_expr expr2 t rest;
        ) else (
          let (leading, inside, trailing) = partition_by_loc rest expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walk_expr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_assert expr
      | Pexp_lazy expr ->
        if is_block_expr expr then (
          walk_expr expr t comments
        ) else (
          let (leading, inside, trailing) = partition_by_loc comments expr.pexp_loc in
          attach t.leading expr.pexp_loc leading;
          walk_expr expr t inside;
          attach t.trailing expr.pexp_loc trailing
        )
      | Pexp_coerce (expr, opt_typexpr, typexpr) ->
        let (leading, inside, trailing) = partition_by_loc comments expr.pexp_loc in
        attach t.leading expr.pexp_loc leading;
        walk_expr expr t inside;
        let (after_expr, rest) =
          partition_adjacent_trailing expr.pexp_loc trailing in
        attach t.trailing expr.pexp_loc after_expr;
        let rest = match opt_typexpr with
        | Some typexpr ->
          let (leading, inside, trailing) = partition_by_loc comments typexpr.ptyp_loc in
          attach t.leading typexpr.ptyp_loc leading;
          walk_typ_expr typexpr t inside;
          let (after_typ, rest) =
            partition_adjacent_trailing typexpr.ptyp_loc trailing in
          attach t.trailing typexpr.ptyp_loc after_typ;
          rest
        | None -> rest
        in
        let (leading, inside, trailing) = partition_by_loc rest typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc leading;
        walk_typ_expr typexpr t inside;
        attach t.trailing typexpr.ptyp_loc trailing
      | Pexp_constraint (expr, typexpr) ->
        let (leading, inside, trailing) = partition_by_loc comments expr.pexp_loc in
        attach t.leading expr.pexp_loc leading;
        walk_expr expr t inside;
        let (after_expr, rest) =
          partition_adjacent_trailing expr.pexp_loc trailing in
        attach t.trailing expr.pexp_loc after_expr;
        let (leading, inside, trailing) = partition_by_loc rest typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc leading;
        walk_typ_expr typexpr t inside;
        attach t.trailing typexpr.ptyp_loc trailing
      | Pexp_tuple []
      | Pexp_array []
      | Pexp_construct({txt = Longident.Lident "[]"}, _) ->
        attach t.inside expr.pexp_loc comments
      | Pexp_construct({txt = Longident.Lident "::"}, _) ->
        walk_list
          ~get_loc:(fun n -> n.Parsetree.pexp_loc)
          ~walk_node:walk_expr
          (collect_list_exprs [] expr)
          t
          comments
      | Pexp_construct (longident, args) ->
        let (leading, trailing) =
          partition_leading_trailing comments longident.loc in
        attach t.leading longident.loc leading;
        begin match args with
        | Some expr ->
          let (after_longident, rest) =
            partition_adjacent_trailing longident.loc trailing in
          attach t.trailing longident.loc after_longident;
          walk_expr expr t rest
        | None ->
          attach t.trailing longident.loc trailing
        end
      | Pexp_variant (_label, None) ->
        ()
      | Pexp_variant (_label, Some expr) ->
        walk_expr expr t comments
      | Pexp_array exprs | Pexp_tuple exprs ->
        walk_list
          ~get_loc:(fun n -> n.Parsetree.pexp_loc)
          ~walk_node:walk_expr
          exprs
          t
          comments
      | Pexp_record (rows, spread_expr) ->
        let comments = match spread_expr with
        | None -> comments
        | Some expr ->
          let (leading, inside, trailing) = partition_by_loc comments expr.pexp_loc in
          attach t.leading expr.pexp_loc leading;
          walk_expr expr t inside;
          let (after_expr, rest) = partition_adjacent_trailing expr.pexp_loc trailing in
          attach t.trailing expr.pexp_loc after_expr;
          rest
        in
        walk_list
          ~get_loc:(fun (
              (longident, expr): (Longident.t Asttypes.loc * Parsetree.expression)
            ) -> {
            longident.loc with loc_end = expr.pexp_loc.loc_end
          })
          ~walk_node:walk_expr_record_row
          rows
          t
          comments
      | Pexp_field (expr, longident) ->
        let (leading, inside, trailing) = partition_by_loc comments expr.pexp_loc in
        let trailing = if is_block_expr expr then (
          let (after_expr, rest) =
            partition_adjacent_trailing expr.pexp_loc trailing in
          walk_expr expr t (List.concat [leading; inside; after_expr]);
          rest
        ) else (
          attach t.leading expr.pexp_loc leading;
          walk_expr expr t inside;
          trailing
        ) in
        let (after_expr, rest) = partition_adjacent_trailing expr.pexp_loc trailing in
        attach t.trailing expr.pexp_loc after_expr;
        let (leading, trailing) = partition_leading_trailing rest longident.loc in
        attach t.leading longident.loc leading;
        attach t.trailing longident.loc trailing
      | Pexp_setfield (expr1, longident, expr2) ->
        let (leading, inside, trailing) = partition_by_loc comments expr1.pexp_loc in
        let rest = if is_block_expr expr1 then (
          let (after_expr, rest) =
            partition_adjacent_trailing expr1.pexp_loc trailing in
          walk_expr expr1 t (List.concat [leading; inside; after_expr]);
          rest
        ) else (
          let (after_expr, rest) =
            partition_adjacent_trailing expr1.pexp_loc trailing in
          attach t.leading expr1.pexp_loc leading;
          walk_expr expr1 t inside;
          attach t.trailing expr1.pexp_loc after_expr;
          rest
        ) in
        let (before_longident, after_longident) = partition_leading_trailing rest longident.loc in
        attach t.leading longident.loc before_longident;
        let (after_longident, rest) = partition_adjacent_trailing longident.loc after_longident in
        attach t.trailing longident.loc after_longident;
        if is_block_expr expr2 then
          walk_expr expr2 t rest
        else (
          let (leading, inside, trailing) = partition_by_loc rest expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walk_expr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_ifthenelse (if_expr, then_expr, else_expr) ->
        let (leading, inside, trailing) = partition_by_loc comments if_expr.pexp_loc in
        let comments = if is_block_expr if_expr then (
          let (after_expr, comments) = partition_adjacent_trailing if_expr.pexp_loc trailing in
          walk_expr if_expr t (List.concat [leading; inside; after_expr]);
          comments
        ) else (
          attach t.leading if_expr.pexp_loc leading;
          walk_expr if_expr t inside;
          let (after_expr, comments) = partition_adjacent_trailing if_expr.pexp_loc trailing in
          attach t.trailing if_expr.pexp_loc after_expr;
          comments
        ) in
        let (leading, inside, trailing) = partition_by_loc comments then_expr.pexp_loc in
        let comments = if is_block_expr then_expr then (
          let (after_expr, trailing) = partition_adjacent_trailing then_expr.pexp_loc trailing in
          walk_expr then_expr t (List.concat [leading; inside; after_expr]);
          trailing
        ) else (
          attach t.leading then_expr.pexp_loc leading;
          walk_expr then_expr t inside;
          let (after_expr, comments) = partition_adjacent_trailing then_expr.pexp_loc trailing in
          attach t.trailing then_expr.pexp_loc after_expr;
          comments
        ) in
        begin match else_expr with
        | None -> ()
        | Some expr ->
          if is_block_expr expr then
            walk_expr expr t comments
          else (
            let (leading, inside, trailing) = partition_by_loc comments expr.pexp_loc in
            attach t.leading expr.pexp_loc leading;
            walk_expr expr t inside;
            attach t.trailing expr.pexp_loc trailing
          )
        end
      | Pexp_while (expr1, expr2) ->
        let (leading, inside, trailing) = partition_by_loc comments expr1.pexp_loc in
        let rest = if is_block_expr expr1 then
          let (after_expr, rest) = partition_adjacent_trailing expr1.pexp_loc trailing in
          walk_expr expr1 t (List.concat [leading; inside; after_expr]);
          rest
        else (
          attach t.leading expr1.pexp_loc leading;
          walk_expr expr1 t inside;
          let (after_expr, rest) = partition_adjacent_trailing expr1.pexp_loc trailing in
          attach t.trailing expr1.pexp_loc after_expr;
          rest
        ) in
        if is_block_expr expr2 then (
          walk_expr expr2 t rest
        ) else (
          let (leading, inside, trailing) = partition_by_loc rest expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walk_expr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_for (pat, expr1, expr2, _, expr3) ->
        let (leading, inside, trailing) = partition_by_loc comments pat.ppat_loc in
        attach t.leading pat.ppat_loc leading;
        walk_pattern pat t inside;
        let (after_pat, rest) = partition_adjacent_trailing pat.ppat_loc trailing in
        attach t.trailing pat.ppat_loc after_pat;
        let (leading, inside, trailing) = partition_by_loc rest expr1.pexp_loc in
        attach t.leading expr1.pexp_loc leading;
        walk_expr expr1 t inside;
        let (after_expr, rest) = partition_adjacent_trailing expr1.pexp_loc trailing in
        attach t.trailing expr1.pexp_loc after_expr;
        let (leading, inside, trailing) = partition_by_loc rest expr2.pexp_loc in
        attach t.leading expr2.pexp_loc leading;
        walk_expr expr2 t inside;
        let (after_expr, rest) = partition_adjacent_trailing expr2.pexp_loc trailing in
        attach t.trailing expr2.pexp_loc after_expr;
        if is_block_expr expr3 then (
          walk_expr expr3 t rest
        ) else (
          let (leading, inside, trailing) = partition_by_loc rest expr3.pexp_loc in
          attach t.leading expr3.pexp_loc leading;
          walk_expr expr3 t inside;
          attach t.trailing expr3.pexp_loc trailing
        )
      | Pexp_pack mod_expr ->
        let (before, inside, after) = partition_by_loc comments mod_expr.pmod_loc in
        attach t.leading mod_expr.pmod_loc before;
        walk_mod_expr mod_expr t inside;
        attach t.trailing mod_expr.pmod_loc after
      | Pexp_match (expr, cases) | Pexp_try (expr, cases) ->
        let (before, inside, after) = partition_by_loc comments expr.pexp_loc in
        let after = if is_block_expr expr then (
          let (after_expr, rest) =
            partition_adjacent_trailing expr.pexp_loc after in
          walk_expr expr t (List.concat [before; inside; after_expr]);
          rest
        ) else (
          attach t.leading expr.pexp_loc before;
          walk_expr expr t inside;
          after
        ) in
        let (after_expr, rest) = partition_adjacent_trailing expr.pexp_loc after in
        attach t.trailing expr.pexp_loc after_expr;
        walk_list
          ~get_loc:(fun n -> {n.Parsetree.pc_lhs.ppat_loc with
            loc_end = n.pc_rhs.pexp_loc.loc_end})
          ~walk_node:walk_case
          cases
          t
          rest
        (* unary expression: todo use parsetreeviewer *)
      | Pexp_apply (
          {pexp_desc = Pexp_ident {txt = Longident.Lident
           ("~+" | "~+." | "~-" | "~-." | "not" | "!")
          }},
          [Nolabel, arg_expr]
        ) ->
        let (before, inside, after) = partition_by_loc comments arg_expr.pexp_loc in
        attach t.leading arg_expr.pexp_loc before;
        walk_expr arg_expr t inside;
        attach t.trailing arg_expr.pexp_loc after
      (* binary expression *)
      | Pexp_apply(
          {pexp_desc = Pexp_ident {txt = Longident.Lident
           (":=" | "||" | "&&" | "=" | "==" | "<" | ">"
            | "!=" | "!==" | "<=" | ">=" | "|>" | "+" | "+."
            | "-" | "-." | "++" | "^" | "*" | "*." | "/"
            | "/." | "**" | "|." | "<>") }},
          [(Nolabel, operand1); (Nolabel, operand2)]
        ) ->
        let (before, inside, after) = partition_by_loc comments operand1.pexp_loc in
        attach t.leading operand1.pexp_loc before;
        walk_expr operand1 t inside;
        let (after_operand1, rest) =
          partition_adjacent_trailing operand1.pexp_loc after in
        attach t.trailing operand1.pexp_loc after_operand1;
        let (before, inside, after) = partition_by_loc rest operand2.pexp_loc in
        attach t.leading operand2.pexp_loc before;
        walk_expr operand2 t inside; (* (List.concat [inside; after]); *)
        attach t.trailing operand2.pexp_loc after;
      | Pexp_apply (call_expr, arguments) ->
        let (before, inside, after) = partition_by_loc comments call_expr.pexp_loc in
        let after = if is_block_expr call_expr then (
          let (after_expr, rest) =
            partition_adjacent_trailing call_expr.pexp_loc after in
          walk_expr call_expr t (List.concat [before; inside; after_expr]);
          rest
        ) else (
          attach t.leading call_expr.pexp_loc before;
          walk_expr call_expr t inside;
          after
        ) in
        let (after_expr, rest) = partition_adjacent_trailing call_expr.pexp_loc after in
        attach t.trailing call_expr.pexp_loc after_expr;
        walk_list
          ~get_loc:(fun (_argLabel, expr) ->
            let loc = match expr.Parsetree.pexp_attributes with
            | ({Location.txt = "res.namedArgLoc"; loc}, _)::_attrs ->
                {loc with loc_end = expr.pexp_loc.loc_end}
            | _ ->
               expr.pexp_loc
            in
            loc)
          ~walk_node:walk_expr_argument
          arguments
          t
          rest
    | Pexp_fun (_, _, _, _) | Pexp_newtype _ ->
      let (_, parameters, return_expr) = fun_expr expr in
      let comments = visit_list_but_continue_with_remaining_comments
        ~newline_delimited:false
        ~walk_node:walk_expr_pararameter
        ~get_loc:(fun (_attrs, _argLbl, expr_opt, pattern) ->
          let open Parsetree in
          let start_pos = match pattern.ppat_attributes with
          | ({Location.txt = "res.namedArgLoc"; loc}, _)::_attrs ->
              loc.loc_start
          | _ ->
             pattern.ppat_loc.loc_start
          in
          match expr_opt with
          | None -> {pattern.ppat_loc with loc_start = start_pos}
          | Some expr -> {
            pattern.ppat_loc with
            loc_start = start_pos;
            loc_end = expr.pexp_loc.loc_end
          }
        )
        parameters
        t
        comments
      in
      begin match return_expr.pexp_desc with
      | Pexp_constraint (expr, typ)
        when expr.pexp_loc.loc_start.pos_cnum >= typ.ptyp_loc.loc_end.pos_cnum
        ->
        let (leading, inside, trailing) = partition_by_loc comments typ.ptyp_loc in
        attach t.leading typ.ptyp_loc leading;
        walk_typ_expr typ t inside;
        let (after_typ, comments) =
          partition_adjacent_trailing typ.ptyp_loc trailing in
        attach t.trailing typ.ptyp_loc after_typ;
        if is_block_expr expr then
          walk_expr expr t comments
        else (
          let (leading, inside, trailing) =
            partition_by_loc comments expr.pexp_loc  in
          attach t.leading expr.pexp_loc leading;
          walk_expr expr t inside;
          attach t.trailing expr.pexp_loc trailing
        )
      | _ ->
        if is_block_expr return_expr then
          walk_expr return_expr t comments
        else (
          let (leading, inside, trailing) =
            partition_by_loc comments return_expr.pexp_loc  in
          attach t.leading return_expr.pexp_loc leading;
          walk_expr return_expr t inside;
          attach t.trailing return_expr.pexp_loc trailing
        )
      end
    | _ -> ()

  and walk_expr_pararameter (_attrs, _argLbl, expr_opt, pattern) t comments =
    let (leading, inside, trailing) = partition_by_loc comments pattern.ppat_loc in
    attach t.leading pattern.ppat_loc leading;
    walk_pattern pattern t inside;
    begin match expr_opt with
    | Some expr ->
      let (_afterPat, rest) =
        partition_adjacent_trailing pattern.ppat_loc trailing in
      attach t.trailing pattern.ppat_loc trailing;
      if is_block_expr expr then
        walk_expr expr t rest
      else (
        let (leading, inside, trailing) = partition_by_loc rest expr.pexp_loc in
        attach t.leading expr.pexp_loc leading;
        walk_expr expr t inside;
        attach t.trailing expr.pexp_loc trailing
      )
    | None ->
      attach t.trailing pattern.ppat_loc trailing
    end

  and walk_expr_argument (_argLabel, expr) t comments =
    match expr.Parsetree.pexp_attributes with
    | ({Location.txt = "res.namedArgLoc"; loc}, _)::_attrs ->
      let (leading, trailing) = partition_leading_trailing comments loc in
      attach t.leading loc leading;
      let (after_label, rest) = partition_adjacent_trailing loc trailing in
      attach t.trailing loc after_label;
      let (before, inside, after) = partition_by_loc rest expr.pexp_loc in
      attach t.leading expr.pexp_loc before;
      walk_expr expr t inside;
      attach t.trailing expr.pexp_loc after
    | _ ->
      let (before, inside, after) = partition_by_loc comments expr.pexp_loc in
      attach t.leading expr.pexp_loc before;
      walk_expr expr t inside;
      attach t.trailing expr.pexp_loc after

    and walk_case case t comments =
      let (before, inside, after) = partition_by_loc comments case.pc_lhs.ppat_loc in
      (* cases don't have a location on their own, leading comments should go
       * after the bar on the pattern *)
      walk_pattern case.pc_lhs t (List.concat [before; inside]);
      let (after_pat, rest) = partition_adjacent_trailing case.pc_lhs.ppat_loc after in
      attach t.trailing case.pc_lhs.ppat_loc after_pat;
      let comments = match case.pc_guard with
      | Some expr ->
        let (before, inside, after) = partition_by_loc rest expr.pexp_loc in
        let (after_expr, rest) = partition_adjacent_trailing expr.pexp_loc after in
        if is_block_expr expr then (
          walk_expr expr t (List.concat [before; inside; after_expr])
        ) else (
          attach t.leading expr.pexp_loc before;
          walk_expr expr t inside;
          attach t.trailing expr.pexp_loc after_expr;
        );
        rest
      | None -> rest
      in
      if is_block_expr case.pc_rhs then (
        walk_expr case.pc_rhs t comments
      ) else (
        let (before, inside, after) = partition_by_loc comments case.pc_rhs.pexp_loc in
        attach t.leading case.pc_rhs.pexp_loc before;
        walk_expr case.pc_rhs t inside;
        attach t.trailing case.pc_rhs.pexp_loc after
      )

    and walk_expr_record_row (longident, expr) t comments =
      let (before_longident, after_longident) =
        partition_leading_trailing comments longident.loc
      in
      attach t.leading longident.loc before_longident;
      let (after_longident, rest) =
        partition_adjacent_trailing longident.loc after_longident in
      attach t.trailing longident.loc after_longident;
      let (leading, inside, trailing) = partition_by_loc rest expr.pexp_loc in
      attach t.leading expr.pexp_loc leading;
      walk_expr expr t inside;
      attach t.trailing expr.pexp_loc trailing

    and walk_ext_constr ext_constr t comments =
      let (leading, trailing) =
        partition_leading_trailing comments ext_constr.pext_name.loc in
      attach t.leading ext_constr.pext_name.loc leading;
      let (after_name, rest) =
        partition_adjacent_trailing ext_constr.pext_name.loc trailing in
      attach t.trailing ext_constr.pext_name.loc after_name;
      walk_extension_constructor_kind ext_constr.pext_kind t rest

    and walk_extension_constructor_kind kind t comments =
      match kind with
      | Pext_rebind longident ->
        let (leading, trailing) =
          partition_leading_trailing comments longident.loc in
        attach t.leading longident.loc leading;
        attach t.trailing longident.loc trailing
      | Pext_decl (constructor_arguments, maybe_typ_expr) ->
        let rest = walk_constructor_arguments constructor_arguments t comments in
        begin match maybe_typ_expr with
        | None -> ()
        | Some typexpr ->
          let (before, inside, after) = partition_by_loc rest typexpr.ptyp_loc in
          attach t.leading typexpr.ptyp_loc before;
          walk_typ_expr typexpr t inside;
          attach t.trailing typexpr.ptyp_loc after
        end

    and walk_mod_expr mod_expr t comments =
      match mod_expr.pmod_desc with
      | Pmod_ident longident ->
        let (before, after) = partition_leading_trailing comments longident.loc in
        attach t.leading longident.loc before;
        attach t.trailing longident.loc after
      | Pmod_structure structure ->
        walk_structure structure t comments
      | Pmod_extension extension ->
        walk_extension extension t comments
      | Pmod_unpack expr ->
        let (before, inside, after) = partition_by_loc comments expr.pexp_loc in
        attach t.leading expr.pexp_loc before;
        walk_expr expr t inside;
        attach t.trailing expr.pexp_loc after
      | Pmod_constraint (modexpr, modtype) ->
        if modtype.pmty_loc.loc_start >= modexpr.pmod_loc.loc_end then (
          let (before, inside, after) = partition_by_loc comments modexpr.pmod_loc in
          attach t.leading modexpr.pmod_loc before;
          walk_mod_expr modexpr t inside;
          let (after, rest) = partition_adjacent_trailing modexpr.pmod_loc after in
          attach t.trailing modexpr.pmod_loc after;
          let (before, inside, after) = partition_by_loc rest modtype.pmty_loc in
          attach t.leading modtype.pmty_loc before;
          walk_mod_type modtype t inside;
          attach t.trailing modtype.pmty_loc after
        ) else (
          let (before, inside, after) = partition_by_loc comments modtype.pmty_loc in
          attach t.leading modtype.pmty_loc before;
          walk_mod_type modtype t inside;
          let (after, rest) = partition_adjacent_trailing modtype.pmty_loc after in
          attach t.trailing modtype.pmty_loc after;
          let (before, inside, after) = partition_by_loc rest modexpr.pmod_loc in
          attach t.leading modexpr.pmod_loc before;
          walk_mod_expr modexpr t inside;
          attach t.trailing modexpr.pmod_loc after;
        )
      | Pmod_apply (_callModExpr, _argModExpr) ->
        let mod_exprs = mod_expr_apply mod_expr in
        walk_list
          ~get_loc:(fun n -> n.Parsetree.pmod_loc)
          ~walk_node:walk_mod_expr
          mod_exprs
          t
          comments
      | Pmod_functor _ ->
        let (parameters, return_mod_expr) = mod_expr_functor mod_expr in
        let comments = visit_list_but_continue_with_remaining_comments
          ~get_loc:(fun
            (_, lbl, mod_type_option) -> match mod_type_option with
            | None -> lbl.Asttypes.loc
            | Some mod_type -> {lbl.loc with loc_end = mod_type.Parsetree.pmty_loc.loc_end}
          )
          ~walk_node:walk_mod_expr_parameter
          ~newline_delimited:false
          parameters
          t
          comments
        in
        begin match return_mod_expr.pmod_desc with
        | Pmod_constraint (mod_expr, mod_type)
          when mod_type.pmty_loc.loc_end.pos_cnum <= mod_expr.pmod_loc.loc_start.pos_cnum ->
          let (before, inside, after) = partition_by_loc comments mod_type.pmty_loc in
          attach t.leading mod_type.pmty_loc before;
          walk_mod_type mod_type t inside;
          let (after, rest) = partition_adjacent_trailing mod_type.pmty_loc after in
          attach t.trailing mod_type.pmty_loc after;
          let (before, inside, after) = partition_by_loc rest mod_expr.pmod_loc in
          attach t.leading mod_expr.pmod_loc before;
          walk_mod_expr mod_expr t inside;
          attach t.trailing mod_expr.pmod_loc after
        | _ ->
          let (before, inside, after) = partition_by_loc comments return_mod_expr.pmod_loc in
          attach t.leading return_mod_expr.pmod_loc before;
          walk_mod_expr return_mod_expr t inside;
          attach t.trailing return_mod_expr.pmod_loc after
        end

    and walk_mod_expr_parameter parameter t comments =
      let (_attrs, lbl, mod_type_option) = parameter in
      let (leading, trailing) = partition_leading_trailing comments lbl.loc in
      attach t.leading lbl.loc leading;
      begin match mod_type_option with
      | None -> attach t.trailing lbl.loc trailing
      | Some mod_type ->
        let (after_lbl, rest) = partition_adjacent_trailing lbl.loc trailing in
        attach t.trailing lbl.loc after_lbl;
        let (before, inside, after) = partition_by_loc rest mod_type.pmty_loc in
        attach t.leading mod_type.pmty_loc before;
        walk_mod_type mod_type t inside;
        attach t.trailing mod_type.pmty_loc after;
      end

    and walk_mod_type mod_type t comments =
      match mod_type.pmty_desc with
      | Pmty_ident longident | Pmty_alias longident ->
        let (leading, trailing) = partition_leading_trailing comments longident.loc in
        attach t.leading longident.loc leading;
        attach t.trailing longident.loc trailing;
      | Pmty_signature signature ->
        walk_signature signature t comments
      | Pmty_extension extension ->
        walk_extension extension t comments
      | Pmty_typeof mod_expr ->
        let (before, inside, after) = partition_by_loc comments mod_expr.pmod_loc in
        attach t.leading mod_expr.pmod_loc before;
        walk_mod_expr mod_expr t inside;
        attach t.trailing mod_expr.pmod_loc after;
      | Pmty_with (mod_type, _withConstraints) ->
        let (before, inside, after) = partition_by_loc comments mod_type.pmty_loc in
        attach t.leading mod_type.pmty_loc before;
        walk_mod_type mod_type t inside;
        attach t.trailing mod_type.pmty_loc after
        (* TODO: withConstraints*)
      | Pmty_functor _ ->
        let (parameters, return_mod_type) = functor_type mod_type in
        let comments = visit_list_but_continue_with_remaining_comments
          ~get_loc:(fun
            (_, lbl, mod_type_option) -> match mod_type_option with
            | None -> lbl.Asttypes.loc
            | Some mod_type ->
              if lbl.txt = "_" then mod_type.Parsetree.pmty_loc
              else {lbl.loc with loc_end = mod_type.Parsetree.pmty_loc.loc_end}
          )
          ~walk_node:walk_mod_type_parameter
          ~newline_delimited:false
          parameters
          t
          comments
        in
        let (before, inside, after) = partition_by_loc comments return_mod_type.pmty_loc in
        attach t.leading return_mod_type.pmty_loc before;
        walk_mod_type return_mod_type t inside;
        attach t.trailing return_mod_type.pmty_loc after

    and walk_mod_type_parameter (_, lbl, mod_type_option) t comments =
      let (leading, trailing) = partition_leading_trailing comments lbl.loc in
      attach t.leading lbl.loc leading;
      begin match mod_type_option with
      | None -> attach t.trailing lbl.loc trailing
      | Some mod_type ->
        let (after_lbl, rest) = partition_adjacent_trailing lbl.loc trailing in
        attach t.trailing lbl.loc after_lbl;
        let (before, inside, after) = partition_by_loc rest mod_type.pmty_loc in
        attach t.leading mod_type.pmty_loc before;
        walk_mod_type mod_type t inside;
        attach t.trailing mod_type.pmty_loc after;
      end

    and walk_pattern pat t comments =
      let open Location in
      match pat.Parsetree.ppat_desc with
      | _ when comments = [] -> ()
      | Ppat_alias (pat, alias) ->
        let (leading, inside, trailing) = partition_by_loc comments pat.ppat_loc in
        attach t.leading pat.ppat_loc leading;
        walk_pattern pat t inside;
        let (after_pat, rest) = partition_adjacent_trailing pat.ppat_loc trailing in
        attach t.leading pat.ppat_loc leading;
        attach t.trailing pat.ppat_loc after_pat;
        let (before_alias, after_alias) = partition_leading_trailing rest alias.loc in
        attach t.leading alias.loc before_alias;
        attach t.trailing alias.loc after_alias
      | Ppat_tuple []
      | Ppat_array []
      | Ppat_construct({txt = Longident.Lident "()"}, _)
      | Ppat_construct({txt = Longident.Lident "[]"}, _) ->
        attach t.inside pat.ppat_loc comments;
      | Ppat_array patterns ->
        walk_list
          ~get_loc:(fun n -> n.Parsetree.ppat_loc)
          ~walk_node:walk_pattern
          patterns
          t
          comments
      | Ppat_tuple patterns ->
        walk_list
          ~get_loc:(fun n -> n.Parsetree.ppat_loc)
          ~walk_node:walk_pattern
          patterns
          t
          comments
      | Ppat_construct({txt = Longident.Lident "::"}, _) ->
        walk_list
          ~get_loc:(fun n -> n.Parsetree.ppat_loc)
          ~walk_node:walk_pattern
          (collect_list_patterns [] pat)
          t
          comments
      | Ppat_construct (constr, None) ->
        let (before_constr, after_constr) =
          partition_leading_trailing comments constr.loc
        in
        attach t.leading constr.loc before_constr;
        attach t.trailing constr.loc after_constr
      | Ppat_construct (constr, Some pat) ->
        let (leading, trailing) = partition_leading_trailing comments constr.loc in
        attach t.leading constr.loc leading;
        let (leading, inside, trailing) = partition_by_loc trailing pat.ppat_loc in
        attach t.leading pat.ppat_loc leading;
        walk_pattern pat t inside;
        attach t.trailing pat.ppat_loc trailing
      | Ppat_variant (_label, None) ->
        ()
      | Ppat_variant (_label, Some pat) ->
        walk_pattern pat t comments
      | Ppat_type _ ->
        ()
      | Ppat_record (record_rows, _) ->
        walk_list
          ~get_loc:(fun (
            (longident_loc, pattern): (Longident.t Asttypes.loc * Parsetree.pattern)
          ) -> {
            longident_loc.loc with
            loc_end = pattern.Parsetree.ppat_loc.loc_end
          })
          ~walk_node:walk_pattern_record_row
          record_rows
          t
          comments
      | Ppat_or (pattern1, pattern2) ->
        let (before_pattern1, inside_pattern1, after_pattern1) =
          partition_by_loc comments pattern1.ppat_loc
        in
        attach t.leading pattern1.ppat_loc before_pattern1;
        walk_pattern pattern1 t inside_pattern1;
        let (after_pattern1, rest) =
          partition_adjacent_trailing pattern1.ppat_loc after_pattern1
        in
        attach t.trailing pattern1.ppat_loc after_pattern1;
        let (before_pattern2, inside_pattern2, after_pattern2) =
          partition_by_loc rest pattern2.ppat_loc
        in
        attach t.leading pattern2.ppat_loc before_pattern2;
        walk_pattern pattern2 t inside_pattern2;
        attach t.trailing pattern2.ppat_loc after_pattern2
      | Ppat_constraint (pattern, typ) ->
        let (before_pattern, inside_pattern, after_pattern) =
          partition_by_loc comments pattern.ppat_loc
        in
        attach t.leading pattern.ppat_loc before_pattern;
        walk_pattern pattern t inside_pattern;
        let (after_pattern, rest) =
          partition_adjacent_trailing pattern.ppat_loc after_pattern
        in
        attach t.trailing pattern.ppat_loc after_pattern;
        let (before_typ, inside_typ, after_typ) =
          partition_by_loc rest typ.ptyp_loc
        in
        attach t.leading typ.ptyp_loc before_typ;
        walk_typ_expr typ t inside_typ;
        attach t.trailing typ.ptyp_loc after_typ
      | Ppat_lazy pattern | Ppat_exception pattern ->
        let (leading, inside, trailing) = partition_by_loc comments pattern.ppat_loc in
        attach t.leading pattern.ppat_loc leading;
        walk_pattern pattern t inside;
        attach t.trailing pattern.ppat_loc trailing
      | Ppat_unpack string_loc ->
        let (leading, trailing) = partition_leading_trailing comments string_loc.loc in
        attach t.leading string_loc.loc leading;
        attach t.trailing string_loc.loc trailing
      | Ppat_extension extension ->
        walk_extension extension t comments
      | _ -> ()

    (* name: firstName *)
    and walk_pattern_record_row row t comments =
     match row with
      (* punned {x}*)
      | ({Location.txt=Longident.Lident ident; loc = longident_loc},
         {Parsetree.ppat_desc=Ppat_var {txt;_}}) when ident = txt ->
        let (before_lbl, after_lbl) =
          partition_leading_trailing comments longident_loc
        in
        attach t.leading longident_loc before_lbl;
        attach t.trailing longident_loc after_lbl
      | (longident, pattern) ->
        let (before_lbl, after_lbl) =
          partition_leading_trailing comments longident.loc
        in
        attach t.leading longident.loc before_lbl;
        let (after_lbl, rest) = partition_adjacent_trailing longident.loc after_lbl in
        attach t.trailing longident.loc after_lbl;
        let (leading, inside, trailing) = partition_by_loc rest pattern.ppat_loc in
        attach t.leading pattern.ppat_loc leading;
        walk_pattern pattern t inside;
        attach t.trailing pattern.ppat_loc trailing

    and walk_typ_expr typ t comments =
      match typ.Parsetree.ptyp_desc with
      | _ when comments = [] -> ()
      | Ptyp_tuple typexprs ->
        walk_list
          ~get_loc:(fun n -> n.Parsetree.ptyp_loc)
          ~walk_node:walk_typ_expr
          typexprs
          t
          comments
      | Ptyp_extension extension ->
        walk_extension extension t comments
      | Ptyp_package package_type ->
        walk_package_type package_type t comments
      | Ptyp_alias (typexpr, _alias) ->
        let (before_typ, inside_typ, after_typ) =
          partition_by_loc comments typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc before_typ;
        walk_typ_expr typexpr t inside_typ;
        attach t.trailing typexpr.ptyp_loc after_typ;
      | Ptyp_poly (strings, typexpr) ->
        let comments = visit_list_but_continue_with_remaining_comments
          ~get_loc:(fun n -> n.Asttypes.loc)
          ~walk_node:(fun longident t comments ->
            let (before_longident, after_longident) =
            partition_leading_trailing comments longident.loc in
            attach t.leading longident.loc before_longident;
            attach t.trailing longident.loc after_longident
          )
          ~newline_delimited:false
          strings
          t
          comments
        in
        let (before_typ, inside_typ, after_typ) =
          partition_by_loc comments typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc before_typ;
        walk_typ_expr typexpr t inside_typ;
        attach t.trailing typexpr.ptyp_loc after_typ
      | Ptyp_constr (longident, typexprs) ->
        let (before_longident, _afterLongident) =
          partition_leading_trailing comments longident.loc in
        let (after_longident, rest) =
          partition_adjacent_trailing longident.loc comments in
        attach t.leading longident.loc before_longident;
        attach t.trailing longident.loc after_longident;
        walk_list
          ~get_loc:(fun n -> n.Parsetree.ptyp_loc)
          ~walk_node:walk_typ_expr
          typexprs
          t
          rest
      | Ptyp_arrow _ ->
        let (_, parameters, typexpr) = arrow_type typ in
        let comments = walk_type_parameters parameters t comments in
        let (before_typ, inside_typ, after_typ) =
          partition_by_loc comments typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc before_typ;
        walk_typ_expr typexpr t inside_typ;
        attach t.trailing typexpr.ptyp_loc after_typ
      | Ptyp_object (fields, _) ->
        walk_typ_object_fields fields t comments
      | _ -> ()

    and walk_typ_object_fields fields t comments =
      walk_list
        ~get_loc:(fun field ->
          match field with
          | Parsetree.Otag (lbl, _, typ) ->
            {lbl.loc with loc_end = typ.ptyp_loc.loc_end}
          | _ -> Location.none
        )
        ~walk_node:walk_typ_object_field
        fields
        t
        comments

    and walk_typ_object_field field t comments =
      match field with
      | Otag (lbl, _, typexpr) ->
        let (before_lbl, after_lbl) = partition_leading_trailing comments lbl.loc in
        attach t.leading lbl.loc before_lbl;
        let (after_lbl, rest) = partition_adjacent_trailing lbl.loc after_lbl in
        attach t.trailing lbl.loc after_lbl;
        let (before_typ, inside_typ, after_typ) =
          partition_by_loc rest typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc before_typ;
        walk_typ_expr typexpr t inside_typ;
        attach t.trailing typexpr.ptyp_loc after_typ
      | _ -> ()

    and walk_type_parameters type_parameters t comments =
      visit_list_but_continue_with_remaining_comments
        ~get_loc:(fun (_, _, typexpr) -> typexpr.Parsetree.ptyp_loc)
        ~walk_node:walk_type_parameter
        ~newline_delimited:false
        type_parameters
        t
        comments

    and walk_type_parameter (_attrs, _lbl, typexpr) t comments =
      let (before_typ, inside_typ, after_typ) =
        partition_by_loc comments typexpr.ptyp_loc in
      attach t.leading typexpr.ptyp_loc before_typ;
      walk_typ_expr typexpr t inside_typ;
      attach t.trailing typexpr.ptyp_loc after_typ

    and walk_package_type package_type t comments =
      let (longident, package_constraints) = package_type in
      let (before_longident, after_longident) =
        partition_leading_trailing comments longident.loc in
      attach t.leading longident.loc before_longident;
      let (after_longident, rest) =
        partition_adjacent_trailing longident.loc after_longident in
      attach t.trailing longident.loc after_longident;
      walk_package_constraints package_constraints t rest

    and walk_package_constraints package_constraints t comments =
      walk_list
        ~get_loc:(fun (longident, typexpr) -> {longident.Asttypes.loc with
          loc_end = typexpr.Parsetree.ptyp_loc.loc_end
        })
        ~walk_node:walk_package_constraint
        package_constraints
        t
        comments

    and walk_package_constraint package_constraint t comments =
      let (longident, typexpr) = package_constraint in
      let (before_longident, after_longident) =
        partition_leading_trailing comments longident.loc in
      attach t.leading longident.loc before_longident;
      let (after_longident, rest) =
        partition_adjacent_trailing longident.loc after_longident in
      attach t.trailing longident.loc after_longident;
      let (before_typ, inside_typ, after_typ) =
        partition_by_loc rest typexpr.ptyp_loc in
      attach t.leading typexpr.ptyp_loc before_typ;
      walk_typ_expr typexpr t inside_typ;
      attach t.trailing typexpr.ptyp_loc after_typ;

    and walk_extension extension t comments =
      let (id, payload) = extension in
      let (before_id, after_id) = partition_leading_trailing comments id.loc in
      attach t.leading id.loc before_id;
      let (after_id, rest) = partition_adjacent_trailing id.loc after_id in
      attach t.trailing id.loc after_id;
      walk_payload payload t rest

    and walk_attribute (id, payload) t comments =
      let (before_id, after_id) = partition_leading_trailing comments id.loc in
      attach t.leading id.loc before_id;
      let (after_id, rest) = partition_adjacent_trailing id.loc after_id in
      attach t.trailing id.loc after_id;
      walk_payload payload t rest

    and walk_payload payload t comments =
      match payload with
      | PStr s -> walk_structure s t comments
      | _ -> ()

end

module Printer = struct
  let add_parens doc =
    Doc.group (
      Doc.concat [
        Doc.lparen;
        Doc.indent (
          Doc.concat [
            Doc.soft_line;
            doc
          ]
        );
        Doc.soft_line;
        Doc.rparen;
      ]
    )

  let add_braces doc =
    Doc.group (
      Doc.concat [
        Doc.lbrace;
        doc;
        Doc.rbrace;
      ]
    )

  let get_first_leading_comment tbl loc =
    match Hashtbl.find tbl.CommentTable.leading loc with
    | comment::_ -> Some comment
    | [] -> None
    | exception Not_found -> None

  let print_multiline_comment_content txt =
    (* Turns
     *         |* first line
     *  * second line
     *      * third line *|
     * Into
     * |* first line
     *  * second line
     *  * third line *|
     *
     * What makes a comment suitable for this kind of indentation?
     *  ->  multiple lines + every line starts with a star
     *)
    let rec indent_stars lines acc =
      match lines with
      | [] -> Doc.nil
      | [last_line] ->
        let line = String.trim last_line in
        let doc = Doc.text (" " ^ line) in
        let trailing_space = if String.length line > 0 then Doc.space else Doc.nil in
        List.rev (trailing_space::doc::acc) |> Doc.concat
      | line::lines ->
        let line = String.trim line in
        let len = String.length line in
        if len > 0 && (String.get [@doesNotRaise]) line 0 == '*' then
          let doc = Doc.text (" " ^ (String.trim line)) in
          indent_stars lines (Doc.hard_line::doc::acc)
        else
          let trailing_space =
            let len = String.length txt in
            if len > 0 && (String.unsafe_get txt (len - 1) = ' ') then
              Doc.space
            else Doc.nil
          in
          let content = Comment.trim_spaces txt in
          Doc.concat [Doc.text content; trailing_space]
    in
    let lines = String.split_on_char '\n' txt in
    match lines with
    | [] -> Doc.text "/* */"
    | [line] -> Doc.concat [
        Doc.text "/* ";
        Doc.text (Comment.trim_spaces line);
        Doc.text " */";
      ]
    | first::rest ->
      let first_line = Comment.trim_spaces first in
      Doc.concat [
        Doc.text "/*";
        if String.length first_line > 0 && not (String.equal first_line "*") then
          Doc.space else Doc.nil;
        indent_stars rest [Doc.hard_line; Doc.text first_line];
        Doc.text "*/";
      ]

  let print_trailing_comment (node_loc : Location.t) comment =
    let single_line = Comment.is_single_line_comment comment in
    let content =
      let txt = Comment.txt comment in
      if single_line then
         Doc.text ("// " ^ String.trim txt)
      else
        print_multiline_comment_content txt
    in
    let diff =
      let cmt_start = (Comment.loc comment).loc_start in
      let prev_tok_end_pos = Comment.prev_tok_end_pos comment in
      cmt_start.pos_lnum - prev_tok_end_pos.pos_lnum
    in
    let is_below =
      (Comment.loc comment).loc_start.pos_lnum > node_loc.loc_end.pos_lnum in
    if diff > 0 || is_below then
      Doc.concat [
        Doc.break_parent;
        Doc.line_suffix(
          (Doc.concat [Doc.hard_line; if diff > 1 then Doc.hard_line else Doc.nil; content])
        )
      ]
    else if not single_line then
      Doc.concat [Doc.space; content]
    else
      Doc.line_suffix (Doc.concat [Doc.space; content])

  let print_leading_comment ?next_comment comment =
    let single_line = Comment.is_single_line_comment comment in
    let content =
      let txt = Comment.txt comment in
      if single_line then
         Doc.text ("// " ^ String.trim txt)
      else
        print_multiline_comment_content txt
    in
    let separator = Doc.concat  [
      if single_line then Doc.concat [
        Doc.hard_line;
        Doc.break_parent;
      ] else Doc.nil;
      (match next_comment with
      | Some next ->
        let next_loc = Comment.loc next in
        let curr_loc = Comment.loc comment in
        let diff =
          next_loc.Location.loc_start.pos_lnum -
          curr_loc.Location.loc_end.pos_lnum
        in
        let next_single_line = Comment.is_single_line_comment next in
        if single_line && next_single_line then
          if diff > 1 then Doc.hard_line else Doc.nil
        else if single_line && not next_single_line then
          if diff > 1 then Doc.hard_line else Doc.nil
        else
          if diff > 1 then Doc.concat [Doc.hard_line; Doc.hard_line]
          else if diff == 1 then Doc.hard_line
          else
            Doc.space
      | None -> Doc.nil)
    ]
    in
    Doc.concat [
      content;
      separator;
    ]

  let print_comments_inside cmt_tbl loc =
    let rec loop acc comments =
      match comments with
      | [] -> Doc.nil
      | [comment] ->
        let cmt_doc = print_leading_comment comment in
        let doc = Doc.group (
          Doc.concat [
            Doc.concat (List.rev (cmt_doc::acc));
          ]
        )
        in
        doc
      | comment::((next_comment::_comments) as rest) ->
        let cmt_doc = print_leading_comment ~next_comment comment in
        loop (cmt_doc::acc) rest
    in
    match Hashtbl.find cmt_tbl.CommentTable.inside loc with
    | exception Not_found -> Doc.nil
    | comments ->
      Hashtbl.remove cmt_tbl.inside loc;
      Doc.group (
        loop [] comments
      )

  let print_leading_comments node tbl loc =
    let rec loop acc comments =
      match comments with
      | [] -> node
      | [comment] ->
        let cmt_doc = print_leading_comment comment in
        let diff =
          loc.Location.loc_start.pos_lnum -
          (Comment.loc comment).Location.loc_end.pos_lnum
        in
        let separator =
          if Comment.is_single_line_comment comment then
            if diff > 1 then Doc.hard_line else Doc.nil
          else if diff == 0 then
           Doc.space
          else if diff > 1 then Doc.concat [Doc.hard_line; Doc.hard_line]
          else
           Doc.hard_line
        in
        let doc = Doc.group (
          Doc.concat [
            Doc.concat (List.rev (cmt_doc::acc));
            separator;
            node
          ]
        )
        in
        doc
      | comment::((next_comment::_comments) as rest) ->
        let cmt_doc = print_leading_comment ~next_comment comment in
        loop (cmt_doc::acc) rest
    in
    match Hashtbl.find tbl loc with
    | exception Not_found -> node
    | comments ->
     (* Remove comments from tbl: Some ast nodes have the same location.
      * We only want to print comments once *)
      Hashtbl.remove tbl loc;
      loop [] comments

  let print_trailing_comments node tbl loc =
    let rec loop acc comments =
      match comments with
      | [] -> Doc.concat (List.rev acc)
      | comment::comments ->
        let cmt_doc = print_trailing_comment loc comment in
        loop (cmt_doc::acc) comments
    in
    match Hashtbl.find tbl loc with
    | exception Not_found -> node
    | [] -> node
    | (_first::_) as comments ->
     (* Remove comments from tbl: Some ast nodes have the same location.
      * We only want to print comments once *)
      Hashtbl.remove tbl loc;
      let cmts_doc = loop [] comments in
      Doc.concat [
        node;
        cmts_doc;
      ]

  let print_comments doc (tbl: CommentTable.t) loc =
    let doc_with_leading_comments = print_leading_comments doc tbl.leading loc in
    print_trailing_comments doc_with_leading_comments tbl.trailing loc

  let print_list ~get_loc ~nodes ~print ?(force_break=false) t =
    let rec loop (prev_loc: Location.t) acc nodes =
      match nodes with
      | [] -> (prev_loc, Doc.concat (List.rev acc))
      | node::nodes ->
        let loc = get_loc node in
        let start_pos = match get_first_leading_comment t loc with
        | None -> loc.loc_start
        | Some comment -> (Comment.loc comment).loc_start
        in
        let sep = if start_pos.pos_lnum - prev_loc.loc_end.pos_lnum > 1 then
          Doc.concat [Doc.hard_line; Doc.hard_line]
        else
          Doc.hard_line
        in
        let doc = print_comments (print node t) t loc in
        loop loc (doc::sep::acc) nodes
    in
    match nodes with
    | [] -> Doc.nil
    | node::nodes ->
      let first_loc = get_loc node in
      let doc = print_comments (print node t) t first_loc in
      let (last_loc, docs) = loop first_loc [doc] nodes in
      let force_break =
        force_break ||
        first_loc.loc_start.pos_lnum != last_loc.loc_end.pos_lnum
      in
      Doc.breakable_group ~force_break docs

  let print_listi ~get_loc ~nodes ~print ?(force_break=false) t =
    let rec loop i (prev_loc: Location.t) acc nodes =
      match nodes with
      | [] -> (prev_loc, Doc.concat (List.rev acc))
      | node::nodes ->
        let loc = get_loc node in
        let start_pos = match get_first_leading_comment t loc with
        | None -> loc.loc_start
        | Some comment -> (Comment.loc comment).loc_start
        in
        let sep = if start_pos.pos_lnum - prev_loc.loc_end.pos_lnum > 1 then
          Doc.concat [Doc.hard_line; Doc.hard_line]
        else
          Doc.line
        in
        let doc = print_comments (print node t i) t loc in
        loop (i + 1) loc (doc::sep::acc) nodes
    in
    match nodes with
    | [] -> Doc.nil
    | node::nodes ->
      let first_loc = get_loc node in
      let doc = print_comments (print node t 0) t first_loc in
      let (last_loc, docs) = loop 1 first_loc [doc] nodes in
      let force_break =
        force_break ||
        first_loc.loc_start.pos_lnum != last_loc.loc_end.pos_lnum
      in
      Doc.breakable_group ~force_break docs

  let rec print_longident_aux accu = function
  | Longident.Lident s -> (Doc.text s) :: accu
  | Ldot(lid, s) -> print_longident_aux ((Doc.text s) :: accu) lid
  | Lapply(lid1, lid2) ->
    let d1 = Doc.join ~sep:Doc.dot (print_longident_aux [] lid1) in
    let d2 = Doc.join ~sep:Doc.dot (print_longident_aux [] lid2) in
    (Doc.concat [d1; Doc.lparen; d2; Doc.rparen]) :: accu

  let print_longident = function
  | Longident.Lident txt -> Doc.text txt
  | lid -> Doc.join ~sep:Doc.dot (print_longident_aux [] lid)

  type identifier_style =
    | ExoticIdent
    | NormalIdent

  let classify_ident_content ?(allow_uident=false) txt =
    let len = String.length txt in
    let rec go i =
      if i == len then NormalIdent
      else
        let c = String.unsafe_get txt i in
        if i == 0 && not (
          (allow_uident && (c >= 'A' && c <= 'Z')) ||
          (c >= 'a' && c <= 'z') || c = '_' || (c >= '0' && c <= '9')) then
          ExoticIdent
        else if not (
             (c >= 'a' && c <= 'z')
          || (c >= 'A' && c <= 'Z')
          || c = '\''
          || c = '_'
          || (c >= '0' && c <= '9'))
        then
          ExoticIdent
        else
          go (i + 1)
    in
    if Token.is_keyword_txt txt && txt <> "list" then
      ExoticIdent
    else
      go 0

  let print_ident_like ?allow_uident txt =
    match classify_ident_content ?allow_uident txt with
    | ExoticIdent -> Doc.concat [
        Doc.text "\\\"";
        Doc.text txt;
        Doc.text"\""
      ]
    | NormalIdent -> Doc.text txt

  let print_lident l = match l with
    | Longident.Lident txt -> print_ident_like txt
    | Longident.Ldot (path, txt) ->
      let txts = Longident.flatten path in
      Doc.concat [
        Doc.join ~sep:Doc.dot (List.map Doc.text txts);
        Doc.dot;
        print_ident_like txt;
      ]
    | _ -> Doc.text("printLident: Longident.Lapply is not supported")

  let print_longident_location l cmt_tbl =
    let doc = print_longident l.Location.txt in
    print_comments doc cmt_tbl l.loc

  (* Module.SubModule.x *)
  let print_lident_path path cmt_tbl =
    let doc = print_lident path.Location.txt in
    print_comments doc cmt_tbl path.loc

  (* Module.SubModule.x or Module.SubModule.X *)
  let print_ident_path path cmt_tbl =
    let doc = print_lident path.Location.txt in
    print_comments doc cmt_tbl path.loc

  let print_string_loc sloc cmt_tbl =
    let doc = print_ident_like sloc.Location.txt in
    print_comments doc cmt_tbl sloc.loc

  let print_constant c = match c with
    | Parsetree.Pconst_integer (s, suffix) ->
      begin match suffix with
      | Some c -> Doc.text (s ^ (Char.escaped c))
      | None -> Doc.text s
      end
    | Pconst_string (txt, None) ->
      Doc.text ("\"" ^ txt ^ "\"")
    | Pconst_string (txt, Some prefix) ->
      Doc.concat [
        if prefix = "" then Doc.nil else Doc.text prefix;
        Doc.text ("`" ^ txt ^ "`")
      ]
    | Pconst_float (s, _) -> Doc.text s
    | Pconst_char c -> Doc.text ("'" ^ (Char.escaped c) ^ "'")

  let rec print_structure (s : Parsetree.structure) t =
    match s with
    | [] -> print_comments_inside t Location.none
    | structure ->
      print_list
        ~get_loc:(fun s -> s.Parsetree.pstr_loc)
        ~nodes:structure
        ~print:print_structure_item
        t

  and print_structure_item (si: Parsetree.structure_item) cmt_tbl =
    match si.pstr_desc with
    | Pstr_value(rec_flag, value_bindings) ->
			let rec_flag = match rec_flag with
			| Asttypes.Nonrecursive -> Doc.nil
			| Asttypes.Recursive -> Doc.text "rec "
			in
      print_value_bindings ~rec_flag value_bindings cmt_tbl
    | Pstr_type(rec_flag, type_declarations) ->
      let rec_flag = match rec_flag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
      in
      print_type_declarations ~rec_flag type_declarations cmt_tbl
    | Pstr_primitive value_description ->
      print_value_description value_description cmt_tbl
    | Pstr_eval (expr, attrs) ->
      let expr_doc =
        let doc = print_expression_with_comments expr cmt_tbl in
        match Parens.structure_expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        print_attributes attrs;
        expr_doc;
      ]
    | Pstr_attribute attr -> Doc.concat [
        Doc.text "@";
        print_attribute_with_comments attr cmt_tbl
      ]
    | Pstr_extension (extension, attrs) -> Doc.concat [
        print_attributes attrs;
        Doc.concat [print_extension_with_comments ~at_module_lvl:true extension cmt_tbl];
      ]
    | Pstr_include include_declaration ->
      print_include_declaration include_declaration cmt_tbl
    | Pstr_open open_description ->
      print_open_description open_description cmt_tbl
    | Pstr_modtype mod_type_decl ->
      print_module_type_declaration mod_type_decl cmt_tbl
    | Pstr_module module_binding ->
      print_module_binding ~is_rec:false module_binding cmt_tbl 0
    | Pstr_recmodule module_bindings ->
      print_listi
        ~get_loc:(fun mb -> mb.Parsetree.pmb_loc)
        ~nodes:module_bindings
        ~print:(print_module_binding ~is_rec:true)
        cmt_tbl
    | Pstr_exception extension_constructor ->
      print_exception_def extension_constructor cmt_tbl
    | Pstr_typext type_extension ->
      print_type_extension type_extension cmt_tbl
    | Pstr_class _ | Pstr_class_type _ -> Doc.nil

  and print_type_extension (te : Parsetree.type_extension) cmt_tbl =
    let prefix = Doc.text "type " in
    let name = print_lident_path te.ptyext_path cmt_tbl in
    let type_params = print_type_params te.ptyext_params cmt_tbl in
    let extension_constructors =
      let ecs = te.ptyext_constructors in
      let force_break =
        match (ecs, List.rev ecs) with
        | (first::_, last::_) ->
          first.pext_loc.loc_start.pos_lnum > te.ptyext_path.loc.loc_end.pos_lnum ||
          first.pext_loc.loc_start.pos_lnum < last.pext_loc.loc_end.pos_lnum
        | _ -> false
        in
      let private_flag = match te.ptyext_private with
      | Asttypes.Private -> Doc.concat [
          Doc.text "private";
          Doc.line;
        ]
      | Public -> Doc.nil
      in
      let rows =
        print_listi
         ~get_loc:(fun n -> n.Parsetree.pext_loc)
         ~print:print_extension_constructor
         ~nodes: ecs
         ~force_break
         cmt_tbl
      in
      Doc.breakable_group ~force_break (
        Doc.indent (
          Doc.concat [
            Doc.line;
            private_flag;
            rows;
            (* Doc.join ~sep:Doc.line ( *)
              (* List.mapi printExtensionConstructor ecs *)
            (* ) *)
          ]
        )
      )
    in
    Doc.group (
      Doc.concat [
        print_attributes ~loc: te.ptyext_path.loc te.ptyext_attributes;
        prefix;
        name;
        type_params;
        Doc.text " +=";
        extension_constructors;
      ]
    )

  and print_module_binding ~is_rec module_binding cmt_tbl i =
    let prefix = if i = 0 then
      Doc.concat [
        Doc.text "module ";
        if is_rec then Doc.text "rec " else Doc.nil;
      ]
    else
      Doc.text "and "
    in
    let (mod_expr_doc, mod_constraint_doc) =
      match module_binding.pmb_expr with
      | {pmod_desc = Pmod_constraint (mod_expr, mod_type)} ->
        (
          print_mod_expr mod_expr cmt_tbl,
          Doc.concat [
            Doc.text ": ";
            print_mod_type mod_type cmt_tbl
          ]
        )
      | mod_expr ->
        (print_mod_expr mod_expr cmt_tbl, Doc.nil)
    in
    let mod_name =
      let doc = Doc.text module_binding.pmb_name.Location.txt in
      print_comments doc cmt_tbl module_binding.pmb_name.loc
    in
    let doc = Doc.concat [
      print_attributes ~loc:module_binding.pmb_name.loc module_binding.pmb_attributes;
      prefix;
      mod_name;
      mod_constraint_doc;
      Doc.text " = ";
      mod_expr_doc;
    ] in
    print_comments doc cmt_tbl module_binding.pmb_loc

  and print_module_type_declaration (mod_type_decl : Parsetree.module_type_declaration) cmt_tbl =
    let mod_name =
      let doc = Doc.text mod_type_decl.pmtd_name.txt in
      print_comments doc cmt_tbl mod_type_decl.pmtd_name.loc
    in
    Doc.concat [
      print_attributes mod_type_decl.pmtd_attributes;
      Doc.text "module type ";
      mod_name;
      (match mod_type_decl.pmtd_type with
      | None -> Doc.nil
      | Some mod_type -> Doc.concat [
          Doc.text " = ";
          print_mod_type mod_type cmt_tbl;
        ]);
    ]

  and print_mod_type mod_type cmt_tbl =
    let mod_type_doc = match mod_type.pmty_desc with
    | Parsetree.Pmty_ident longident ->
      Doc.concat [
        print_attributes ~loc:longident.loc mod_type.pmty_attributes;
        print_longident_location longident cmt_tbl
      ]
    | Pmty_signature signature ->
      let signature_doc = Doc.breakable_group ~force_break:true (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.line;
              print_signature signature cmt_tbl;
            ]
          );
          Doc.line;
          Doc.rbrace;
        ]
      ) in
      Doc.concat [
        print_attributes mod_type.pmty_attributes;
        signature_doc
      ]
    | Pmty_functor _ ->
      let (parameters, return_type) = ParsetreeViewer.functor_type mod_type in
      let parameters_doc = match parameters with
      | [] -> Doc.nil
      | [attrs, {Location.txt = "_"; loc}, Some mod_type] ->
        let cmt_loc =
          {loc with loc_end = mod_type.Parsetree.pmty_loc.loc_end}
        in
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map print_attribute attrs);
          Doc.line;
        ] in
        let doc = Doc.concat [
          attrs;
          print_mod_type mod_type cmt_tbl
        ] in
        print_comments doc cmt_tbl cmt_loc
      | params ->
        Doc.group (
          Doc.concat [
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map (fun (attrs, lbl, mod_type) ->
                    let cmt_loc = match mod_type with
                    | None -> lbl.Asttypes.loc
                    | Some mod_type ->
                      {lbl.Asttypes.loc with loc_end = mod_type.Parsetree.pmty_loc.loc_end}
                    in
                    let attrs = match attrs with
                    | [] -> Doc.nil
                    | attrs -> Doc.concat [
                      Doc.join ~sep:Doc.line (List.map print_attribute attrs);
                      Doc.line;
                    ] in
                    let lbl_doc = if lbl.Location.txt = "_" then Doc.nil
                      else
                        let doc = Doc.text lbl.txt in
                        print_comments doc cmt_tbl lbl.loc
                    in
                    let doc = Doc.concat [
                      attrs;
                      lbl_doc;
                      (match mod_type with
                      | None -> Doc.nil
                      | Some mod_type -> Doc.concat [
                        if lbl.txt = "_" then Doc.nil else Doc.text ": ";
                        print_mod_type mod_type cmt_tbl;
                      ]);
                    ] in
                    print_comments doc cmt_tbl cmt_loc
                  ) params
                );
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
            Doc.rparen;
          ]
        )
      in
      let return_doc =
        let doc = print_mod_type return_type cmt_tbl in
        if Parens.mod_type_functor_return return_type then add_parens doc else doc
      in
      Doc.group (
        Doc.concat [
          parameters_doc;
          Doc.group (
            Doc.concat [
            Doc.text " =>";
            Doc.line;
            return_doc;
            ]
          )
        ]
      )
    | Pmty_typeof mod_expr -> Doc.concat [
        Doc.text "module type of ";
        print_mod_expr mod_expr cmt_tbl
      ]
    | Pmty_extension extension -> print_extension_with_comments ~at_module_lvl:false extension cmt_tbl
    | Pmty_alias longident -> Doc.concat [
        Doc.text "module ";
        print_longident_location longident cmt_tbl;
      ]
    | Pmty_with (mod_type, with_constraints) ->
      let operand =
        let doc = print_mod_type mod_type cmt_tbl in
        if Parens.mod_type_with_operand mod_type then add_parens doc else doc
      in
      Doc.group (
        Doc.concat [
          operand;
          Doc.indent (
            Doc.concat [
              Doc.line;
              print_with_constraints with_constraints cmt_tbl;
            ]
          )
        ]
      )
    in
    let attrs_already_printed = match mod_type.pmty_desc with
    | Pmty_functor _ | Pmty_signature _ | Pmty_ident _ -> true
    | _ -> false
    in
    let doc =Doc.concat [
      if attrs_already_printed then Doc.nil else print_attributes mod_type.pmty_attributes;
      mod_type_doc;
    ] in
    print_comments doc cmt_tbl mod_type.pmty_loc

  and print_with_constraints with_constraints cmt_tbl =
    let rows = List.mapi (fun i with_constraint ->
      Doc.group (
        Doc.concat [
          if i == 0 then Doc.text "with " else Doc.text "and ";
          print_with_constraint with_constraint cmt_tbl;
        ]
      )
    ) with_constraints
    in
    Doc.join ~sep:Doc.line rows

  and print_with_constraint (with_constraint : Parsetree.with_constraint) cmt_tbl =
    match with_constraint with
    (* with type X.t = ... *)
    | Pwith_type (longident, type_declaration) ->
      Doc.group (print_type_declaration
        ~name:(print_lident_path longident cmt_tbl)
        ~equal_sign:"="
        ~rec_flag:Doc.nil
        0
        type_declaration
        CommentTable.empty)
    (* with module X.Y = Z *)
    | Pwith_module ({txt = longident1}, {txt = longident2}) ->
        Doc.concat [
          Doc.text "module ";
          print_longident longident1;
          Doc.text " =";
          Doc.indent (
            Doc.concat [
              Doc.line;
              print_longident longident2;
            ]
          )
        ]
    (* with type X.t := ..., same format as [Pwith_type] *)
    | Pwith_typesubst (longident, type_declaration) ->
      Doc.group(print_type_declaration
        ~name:(print_lident_path longident cmt_tbl)
        ~equal_sign:":="
        ~rec_flag:Doc.nil
        0
        type_declaration
        CommentTable.empty)
    | Pwith_modsubst ({txt = longident1}, {txt = longident2}) ->
      Doc.concat [
        Doc.text "module ";
        print_longident longident1;
        Doc.text " :=";
        Doc.indent (
          Doc.concat [
            Doc.line;
            print_longident longident2;
          ]
        )
      ]

  and print_signature signature cmt_tbl =
    match signature with
    | [] -> print_comments_inside cmt_tbl Location.none
    | signature ->
      print_list
        ~get_loc:(fun s -> s.Parsetree.psig_loc)
        ~nodes:signature
        ~print:print_signature_item
        cmt_tbl

  and print_signature_item (si : Parsetree.signature_item) cmt_tbl =
    match si.psig_desc with
    | Parsetree.Psig_value value_description ->
      print_value_description value_description cmt_tbl
    | Psig_type (rec_flag, type_declarations) ->
      let rec_flag = match rec_flag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
      in
      print_type_declarations ~rec_flag type_declarations cmt_tbl
    | Psig_typext type_extension ->
      print_type_extension type_extension cmt_tbl
    | Psig_exception extension_constructor ->
      print_exception_def extension_constructor cmt_tbl
    | Psig_module module_declaration ->
      print_module_declaration module_declaration cmt_tbl
    | Psig_recmodule module_declarations ->
      print_rec_module_declarations module_declarations cmt_tbl
    | Psig_modtype mod_type_decl ->
      print_module_type_declaration mod_type_decl cmt_tbl
    | Psig_open open_description ->
      print_open_description open_description cmt_tbl
    | Psig_include include_description ->
      print_include_description include_description cmt_tbl
    | Psig_attribute attr -> Doc.concat [
        Doc.text "@";
        print_attribute_with_comments attr cmt_tbl
      ]
    | Psig_extension (extension, attrs) -> Doc.concat [
        print_attributes attrs;
        Doc.concat [print_extension_with_comments ~at_module_lvl:true extension cmt_tbl];
      ]
    | Psig_class _ | Psig_class_type _ -> Doc.nil

  and print_rec_module_declarations module_declarations cmt_tbl =
      print_listi
        ~get_loc:(fun n -> n.Parsetree.pmd_loc)
        ~nodes:module_declarations
        ~print:print_rec_module_declaration
        cmt_tbl

 and print_rec_module_declaration md cmt_tbl i =
    let body = match md.pmd_type.pmty_desc with
    | Parsetree.Pmty_alias longident ->
      Doc.concat [Doc.text " = "; print_longident_location longident cmt_tbl]
    | _ ->
      let needs_parens = match md.pmd_type.pmty_desc with
      | Pmty_with _ -> true
      | _ -> false
      in
      let mod_type_doc =
        let doc = print_mod_type md.pmd_type cmt_tbl in
        if needs_parens then add_parens doc else doc
      in
      Doc.concat [Doc.text ": "; mod_type_doc]
    in
    let prefix = if i < 1 then "module rec " else "and " in
    Doc.concat [
      print_attributes ~loc:md.pmd_name.loc md.pmd_attributes;
      Doc.text prefix;
      print_comments (Doc.text md.pmd_name.txt) cmt_tbl md.pmd_name.loc;
      body
    ]

  and print_module_declaration (md: Parsetree.module_declaration) cmt_tbl =
    let body = match md.pmd_type.pmty_desc with
    | Parsetree.Pmty_alias longident ->
      Doc.concat [Doc.text " = "; print_longident_location longident cmt_tbl]
    | _ -> Doc.concat [Doc.text ": "; print_mod_type md.pmd_type cmt_tbl]
    in
    Doc.concat [
      print_attributes ~loc:md.pmd_name.loc md.pmd_attributes;
      Doc.text "module ";
      print_comments (Doc.text md.pmd_name.txt) cmt_tbl md.pmd_name.loc;
      body
    ]

  and print_open_description (open_description : Parsetree.open_description) p =
    Doc.concat [
      print_attributes open_description.popen_attributes;
      Doc.text "open";
      (match open_description.popen_override with
      | Asttypes.Fresh -> Doc.space
      | Asttypes.Override -> Doc.text "! ");
      print_longident_location open_description.popen_lid p
    ]

  and print_include_description (include_description: Parsetree.include_description) cmt_tbl =
    Doc.concat [
      print_attributes include_description.pincl_attributes;
      Doc.text "include ";
      print_mod_type include_description.pincl_mod cmt_tbl;
    ]

  and print_include_declaration (include_declaration : Parsetree.include_declaration)  cmt_tbl =
    let is_js_ffi_import = List.exists (fun attr ->
      match attr with
      | ({Location.txt = "ns.jsFfi"}, _) -> true
      | _ -> false
    ) include_declaration.pincl_attributes
    in
    if is_js_ffi_import then
      print_js_ffi_import_declaration include_declaration cmt_tbl
    else
      Doc.concat [
        print_attributes include_declaration.pincl_attributes;
        Doc.text "include ";
        let include_doc =
          print_mod_expr include_declaration.pincl_mod cmt_tbl
        in
        if Parens.include_mod_expr include_declaration.pincl_mod then
          add_parens include_doc
        else include_doc;
      ]

  and print_js_ffi_import (value_description: Parsetree.value_description) cmt_tbl =
    let attrs = List.filter (fun attr ->
      match attr with
      | ({Location.txt = "val" | "genType.import" | "scope" }, _) -> false
      | _ -> true
    ) value_description.pval_attributes in
    let (ident, alias) = match value_description.pval_prim with
    | primitive::_ ->
      if primitive <> value_description.pval_name.txt then
        (
          print_ident_like primitive,
          Doc.concat [
            Doc.text " as ";
            print_ident_like value_description.pval_name.txt;
          ]
        )
      else
        (print_ident_like primitive, Doc.nil)
    | _ ->
      (print_ident_like value_description.pval_name.txt, Doc.nil)
    in
    Doc.concat [
      print_attributes ~loc:value_description.pval_name.loc attrs;
      ident;
      alias;
      Doc.text ": ";
      print_typ_expr value_description.pval_type cmt_tbl;
    ]

  and print_js_ffi_import_scope (scope: ParsetreeViewer.js_import_scope) =
    match scope with
    | JsGlobalImport -> Doc.nil
    | JsModuleImport mod_name ->
      Doc.concat [
        Doc.text " from ";
        Doc.double_quote;
        Doc.text mod_name;
        Doc.double_quote;
      ]
    | JsScopedImport idents ->
      Doc.concat [
        Doc.text " from ";
        Doc.join ~sep:Doc.dot (List.map Doc.text idents)
      ]

  and print_js_ffi_import_declaration (include_declaration: Parsetree.include_declaration) cmt_tbl =
    let attrs = List.filter (fun attr ->
      match attr with
      | ({Location.txt = "ns.jsFfi"}, _) -> false
      | _ -> true
    ) include_declaration.pincl_attributes
    in
    let imports = ParsetreeViewer.extract_value_description_from_mod_expr include_declaration.pincl_mod in
    let scope = match imports with
    | vd::_ -> ParsetreeViewer.classify_js_import vd
    | [] -> ParsetreeViewer.JsGlobalImport
    in
    let scope_doc = print_js_ffi_import_scope scope in
    Doc.group (
      Doc.concat [
        print_attributes attrs;
        Doc.text "import ";
        Doc.group (
          Doc.concat [
            Doc.lbrace;
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map (fun vd -> print_js_ffi_import vd cmt_tbl) imports
                )
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
            Doc.rbrace;
          ]
        );
        scope_doc;
      ]
    )

  and print_value_bindings ~rec_flag (vbs: Parsetree.value_binding list) cmt_tbl =
    print_listi
      ~get_loc:(fun vb -> vb.Parsetree.pvb_loc)
      ~nodes:vbs
      ~print:(print_value_binding ~rec_flag)
      cmt_tbl

  and print_value_description value_description cmt_tbl =
    let is_external =
      match value_description.pval_prim with | [] -> false | _ -> true
    in
    Doc.group (
      Doc.concat [
        print_attributes value_description.pval_attributes;
        Doc.text (if is_external then "external " else "let ");
        print_comments
          (print_ident_like value_description.pval_name.txt)
          cmt_tbl
          value_description.pval_name.loc;
        Doc.text ": ";
        print_typ_expr value_description.pval_type cmt_tbl;
        if is_external then
          Doc.group (
            Doc.concat [
              Doc.text " =";
              Doc.indent(
                Doc.concat [
                  Doc.line;
                  Doc.join ~sep:Doc.line (
                    List.map(fun s -> Doc.concat [
                      Doc.text "\"";
                      Doc.text s;
                      Doc.text "\"";
                    ])
                    value_description.pval_prim
                  );
                ]
              )
            ]
          )
        else Doc.nil
      ]
    )

  and print_type_declarations ~rec_flag type_declarations cmt_tbl =
    print_listi
      ~get_loc:(fun n -> n.Parsetree.ptype_loc)
      ~nodes:type_declarations
      ~print:(print_type_declaration2 ~rec_flag)
      cmt_tbl

  (*
   * type_declaration = {
   *    ptype_name: string loc;
   *    ptype_params: (core_type * variance) list;
   *          (* ('a1,...'an) t; None represents  _*)
   *    ptype_cstrs: (core_type * core_type * Location.t) list;
   *          (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
   *    ptype_kind: type_kind;
   *    ptype_private: private_flag;   (* = private ... *)
   *    ptype_manifest: core_type option;  (* = T *)
   *    ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
   *    ptype_loc: Location.t;
   * }
   *
   *
   *  type t                     (abstract, no manifest)
   *  type t = T0                (abstract, manifest=T0)
   *  type t = C of T | ...      (variant,  no manifest)
   *  type t = T0 = C of T | ... (variant,  manifest=T0)
   *  type t = {l: T; ...}       (record,   no manifest)
   *  type t = T0 = {l : T; ...} (record,   manifest=T0)
   *  type t = ..                (open,     no manifest)
   *
   *
   * and type_kind =
   *  | Ptype_abstract
   *  | Ptype_variant of constructor_declaration list
   *        (* Invariant: non-empty list *)
   *  | Ptype_record of label_declaration list
   *        (* Invariant: non-empty list *)
   *  | Ptype_open
   *)
  and print_type_declaration ~name ~equal_sign ~rec_flag i (td: Parsetree.type_declaration) cmt_tbl =
    let (has_gen_type, attrs) = ParsetreeViewer.split_gen_type_attr td.ptype_attributes in
    let attrs = print_attributes ~loc:td.ptype_loc attrs in
    let prefix = if i > 0 then
      Doc.concat [
        Doc.text "and ";
        if has_gen_type then Doc.text "export " else Doc.nil
      ]
    else
      Doc.concat [
        Doc.text (if has_gen_type then "export type " else "type ");
        rec_flag
      ]
    in
    let type_name = name in
    let type_params = print_type_params td.ptype_params cmt_tbl in
    let manifest_and_kind = match td.ptype_kind with
    | Ptype_abstract ->
      begin match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) ->
        Doc.concat [
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_private_flag td.ptype_private;
          print_typ_expr typ cmt_tbl;
        ]
      end
    | Ptype_open -> Doc.concat [
        Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
        print_private_flag td.ptype_private;
        Doc.text "..";
      ]
    | Ptype_record(lds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_typ_expr typ cmt_tbl;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
        print_private_flag td.ptype_private;
        print_record_declaration lds cmt_tbl;
      ]
    | Ptype_variant(cds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_typ_expr typ cmt_tbl;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equal_sign];
        print_constructor_declarations ~private_flag:td.ptype_private cds cmt_tbl;
      ]
    in
    let constraints = print_type_definition_constraints td.ptype_cstrs in
    Doc.group (
      Doc.concat [
        attrs;
        prefix;
        type_name;
        type_params;
        manifest_and_kind;
        constraints;
      ]
    )

  and print_type_declaration2 ~rec_flag (td: Parsetree.type_declaration) cmt_tbl i =
    let name =
      let doc = print_ident_like td.Parsetree.ptype_name.txt in
      print_comments doc cmt_tbl td.ptype_name.loc
    in
    let equal_sign = "=" in
    let (has_gen_type, attrs) = ParsetreeViewer.split_gen_type_attr td.ptype_attributes in
    let attrs = print_attributes ~loc:td.ptype_loc attrs in
    let prefix = if i > 0 then
      Doc.concat [
        Doc.text "and ";
        if has_gen_type then Doc.text "export " else Doc.nil
      ]
    else
      Doc.concat [
        Doc.text (if has_gen_type then "export type " else "type ");
        rec_flag
      ]
    in
    let type_name = name in
    let type_params = print_type_params td.ptype_params cmt_tbl in
    let manifest_and_kind = match td.ptype_kind with
    | Ptype_abstract ->
      begin match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) ->
        Doc.concat [
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_private_flag td.ptype_private;
          print_typ_expr typ cmt_tbl;
        ]
      end
    | Ptype_open -> Doc.concat [
        Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
        print_private_flag td.ptype_private;
        Doc.text "..";
      ]
    | Ptype_record(lds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_typ_expr typ cmt_tbl;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
        print_private_flag td.ptype_private;
        print_record_declaration lds cmt_tbl;
      ]
    | Ptype_variant(cds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_typ_expr typ cmt_tbl;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equal_sign];
        print_constructor_declarations ~private_flag:td.ptype_private cds cmt_tbl;
      ]
    in
    let constraints = print_type_definition_constraints td.ptype_cstrs in
    Doc.group (
      Doc.concat [
        attrs;
        prefix;
        type_name;
        type_params;
        manifest_and_kind;
        constraints;
      ]
    )

  and print_type_definition_constraints cstrs =
    match cstrs with
    | [] -> Doc.nil
    | cstrs -> Doc.indent (
        Doc.group (
          Doc.concat [
            Doc.line;
            Doc.group(
              Doc.join ~sep:Doc.line (
                List.map print_type_definition_constraint cstrs
              )
            )
          ]
        )
      )

  and print_type_definition_constraint ((typ1, typ2, _loc ): Parsetree.core_type * Parsetree.core_type * Location.t) =
    Doc.concat [
      Doc.text "constraint ";
      print_typ_expr typ1 CommentTable.empty;
      Doc.text " = ";
      print_typ_expr typ2 CommentTable.empty;
    ]

  and print_private_flag (flag : Asttypes.private_flag) = match flag with
    | Private -> Doc.text "private "
    | Public -> Doc.nil

  and print_type_params type_params cmt_tbl =
    match type_params with
    | [] -> Doc.nil
    | type_params ->
      Doc.group (
        Doc.concat [
          Doc.less_than;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun type_param ->
                  let doc = print_type_param type_param cmt_tbl in
                  print_comments doc cmt_tbl (fst type_param).Parsetree.ptyp_loc
                ) type_params
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.greater_than;
        ]
      )

  and print_type_param (param : (Parsetree.core_type * Asttypes.variance)) cmt_tbl =
    let (typ, variance) = param in
    let printed_variance = match variance with
    | Covariant -> Doc.text "+"
    | Contravariant -> Doc.text "-"
    | Invariant -> Doc.nil
    in
    Doc.concat [
      printed_variance;
      print_typ_expr typ cmt_tbl
    ]

  and print_record_declaration (lds: Parsetree.label_declaration list) cmt_tbl =
    let force_break = match (lds, List.rev lds) with
    | (first::_, last::_) ->
       first.pld_loc.loc_start.pos_lnum < last.pld_loc.loc_end.pos_lnum
    | _ -> false
    in
    Doc.breakable_group ~force_break (
      Doc.concat [
        Doc.lbrace;
        Doc.indent (
          Doc.concat [
            Doc.soft_line;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line])
              (List.map (fun ld ->
                let doc = print_label_declaration ld cmt_tbl in
                print_comments doc cmt_tbl ld.Parsetree.pld_loc
              ) lds)
          ]
        );
        Doc.trailing_comma;
        Doc.soft_line;
        Doc.rbrace;
      ]
    )

  and print_constructor_declarations
    ~private_flag (cds: Parsetree.constructor_declaration list) cmt_tbl
  =
    let force_break = match (cds, List.rev cds) with
    | (first::_, last::_) ->
       first.pcd_loc.loc_start.pos_lnum < last.pcd_loc.loc_end.pos_lnum
    | _ -> false
    in
    let private_flag = match private_flag with
    | Asttypes.Private -> Doc.concat [
        Doc.text "private";
        Doc.line;
      ]
    | Public -> Doc.nil
    in
    let rows =
      print_listi
        ~get_loc:(fun cd -> cd.Parsetree.pcd_loc)
        ~nodes:cds
        ~print:(fun cd cmt_tbl i ->
          let doc = print_constructor_declaration2 i cd cmt_tbl in
          print_comments doc cmt_tbl cd.Parsetree.pcd_loc
        )
        ~force_break
        cmt_tbl
    in
    Doc.breakable_group ~force_break (
      Doc.indent (
        Doc.concat [
          Doc.line;
          private_flag;
          rows;
        ]
      )
    )

  and print_constructor_declaration2 i (cd : Parsetree.constructor_declaration) cmt_tbl =
    let attrs = print_attributes cd.pcd_attributes in
    let bar = if i > 0 then Doc.text "| "
    else Doc.if_breaks (Doc.text "| ") Doc.nil
    in
    let constr_name =
      let doc = Doc.text cd.pcd_name.txt in
      print_comments doc cmt_tbl cd.pcd_name.loc
    in
    let constr_args = print_constructor_arguments ~indent:true cd.pcd_args cmt_tbl in
    let gadt = match cd.pcd_res with
    | None -> Doc.nil
    | Some(typ) -> Doc.indent (
        Doc.concat [
          Doc.text ": ";
          print_typ_expr typ cmt_tbl;
        ]
      )
    in
    Doc.concat [
      bar;
      Doc.group (
        Doc.concat [
          attrs; (* TODO: fix parsing of attributes, so when can print them above the bar? *)
          constr_name;
          constr_args;
          gadt;
        ]
      )
    ]

  and print_constructor_arguments ~indent (cd_args : Parsetree.constructor_arguments) cmt_tbl =
    match cd_args with
    | Pcstr_tuple [] -> Doc.nil
    | Pcstr_tuple types ->
      let args = Doc.concat [
        Doc.lparen;
          Doc.indent (
            Doc.concat [
            Doc.soft_line;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
              List.map (fun typexpr ->
                print_typ_expr typexpr cmt_tbl
              ) types
            )
          ]
        );
        Doc.trailing_comma;
        Doc.soft_line;
        Doc.rparen;
      ] in
      Doc.group (
        if indent then Doc.indent args else args
      )
    | Pcstr_record lds ->
      let args = Doc.concat [
        Doc.lparen;
        (* manually inline the printRecordDeclaration, gives better layout *)
        Doc.lbrace;
        Doc.indent (
          Doc.concat [
            Doc.soft_line;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line])
              (List.map (fun ld ->
                let doc = print_label_declaration ld cmt_tbl in
                print_comments doc cmt_tbl ld.Parsetree.pld_loc
              ) lds)
          ]
        );
        Doc.trailing_comma;
        Doc.soft_line;
        Doc.rbrace;
        Doc.rparen;
      ] in
      if indent then Doc.indent args else args

  and print_label_declaration (ld : Parsetree.label_declaration) cmt_tbl =
    let attrs = print_attributes ~loc:ld.pld_name.loc ld.pld_attributes in
    let mutable_flag = match ld.pld_mutable with
    | Mutable -> Doc.text "mutable "
    | Immutable -> Doc.nil
    in
    let name =
      let doc = print_ident_like ld.pld_name.txt in
      print_comments doc cmt_tbl ld.pld_name.loc
    in
    Doc.group (
      Doc.concat [
        attrs;
        mutable_flag;
        name;
        Doc.text ": ";
        print_typ_expr ld.pld_type cmt_tbl;
      ]
    )

  and print_typ_expr (typ_expr : Parsetree.core_type) cmt_tbl =
    let rendered_type = match typ_expr.ptyp_desc with
    | Ptyp_any -> Doc.text "_"
    | Ptyp_var var -> Doc.concat [
        Doc.text "'";
        print_ident_like var;
      ]
    | Ptyp_extension(extension) ->
      print_extension_with_comments ~at_module_lvl:false extension cmt_tbl
    | Ptyp_alias(typ, alias) ->
      let typ =
        (* Technically type t = (string, float) => unit as 'x, doesn't require
         * parens around the arrow expression. This is very confusing though.
         * Is the "as" part of "unit" or "(string, float) => unit". By printing
         * parens we guide the user towards its meaning.*)
        let needs_parens = match typ.ptyp_desc with
        | Ptyp_arrow _ -> true
        | _ -> false
        in
        let doc = print_typ_expr typ cmt_tbl in
        if needs_parens then
          Doc.concat [Doc.lparen; doc; Doc.rparen]
        else
          doc
      in
      Doc.concat [typ; Doc.text " as "; Doc.concat [Doc.text "'"; print_ident_like alias]]
    | Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")}, [{ptyp_desc = Ptyp_object (_fields, _openFlag)} as typ]) ->
      let bs_object = print_typ_expr typ cmt_tbl in
      begin match typ_expr.ptyp_attributes with
      | [] -> bs_object
      | attrs ->
        Doc.concat [
          Doc.group (
            Doc.join ~sep:Doc.line (List.map print_attribute attrs)
          );
          Doc.space;
          print_typ_expr typ cmt_tbl;
        ]
      end
    | Ptyp_constr(longident_loc, [{ ptyp_desc = Parsetree.Ptyp_tuple tuple }]) ->
      let constr_name = print_lident_path longident_loc cmt_tbl in
      Doc.group(
        Doc.concat([
          constr_name;
          Doc.less_than;
          print_tuple_type ~inline:true tuple cmt_tbl;
          Doc.greater_than;
        ])
      )
    | Ptyp_constr(longident_loc, constr_args) ->
      let constr_name = print_lident_path longident_loc cmt_tbl in
      begin match constr_args with
      | [] -> constr_name
      | [{
          Parsetree.ptyp_desc =
            Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")},
          [{ptyp_desc = Ptyp_object (fields, open_flag)}])
        }] ->
        Doc.concat([
          constr_name;
          Doc.less_than;
          print_bs_object_sugar ~inline:true fields open_flag cmt_tbl;
          Doc.greater_than;
        ])
      | _args -> Doc.group(
        Doc.concat([
          constr_name;
          Doc.less_than;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map
                  (fun typexpr -> print_typ_expr typexpr cmt_tbl)
                  constr_args
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.greater_than;
        ])
      )
      end
    | Ptyp_arrow _ ->
      let (attrs_before, args, return_type) = ParsetreeViewer.arrow_type typ_expr in
      let return_type_needs_parens = match return_type.ptyp_desc with
      | Ptyp_alias _ -> true
      | _ -> false
      in
      let return_doc =
        let doc = print_typ_expr return_type cmt_tbl in
        if return_type_needs_parens then
          Doc.concat [Doc.lparen; doc; Doc.rparen]
        else doc
      in
      let (is_uncurried, attrs) =
        ParsetreeViewer.process_uncurried_attribute attrs_before
      in
      begin match args with
      | [] -> Doc.nil
      | [([], Nolabel, n)] when not is_uncurried ->
          let has_attrs_before = not (attrs = []) in
          let attrs = if has_attrs_before then
            Doc.concat [
              Doc.join ~sep:Doc.line (List.map print_attribute attrs_before);
              Doc.space;
            ]
          else Doc.nil
          in
          let typ_doc =
            let doc = print_typ_expr n cmt_tbl in
            match n.ptyp_desc with
            | Ptyp_arrow _ | Ptyp_tuple _ -> add_parens doc
            | _ -> doc
          in
          Doc.group (
            Doc.concat [
              Doc.group attrs;
              Doc.group (
                if has_attrs_before then
                  Doc.concat [
                    Doc.lparen;
                    Doc.indent (
                      Doc.concat [
                        Doc.soft_line;
                        typ_doc;
                        Doc.text " => ";
                        return_doc;
                      ]
                    );
                    Doc.soft_line;
                    Doc.rparen
                  ]
                else
                Doc.concat [
                  typ_doc;
                  Doc.text " => ";
                  return_doc;
                ]
              )
            ]
          )
      | args ->
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
            Doc.join ~sep:Doc.line (List.map print_attribute attrs);
            Doc.space;
          ]
        in
        let rendered_args = Doc.concat [
          attrs;
          Doc.text "(";
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              if is_uncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun tp -> print_type_parameter tp cmt_tbl) args
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.text ")";
        ] in
        Doc.group (
          Doc.concat [
            rendered_args;
            Doc.text " => ";
            return_doc;
          ]
        )
      end
    | Ptyp_tuple types -> print_tuple_type ~inline:false types cmt_tbl
    | Ptyp_object (fields, open_flag) ->
      print_bs_object_sugar ~inline:false fields open_flag cmt_tbl
    | Ptyp_poly([], typ) ->
      print_typ_expr typ cmt_tbl
    | Ptyp_poly(string_locs, typ) ->
      Doc.concat [
        Doc.join ~sep:Doc.space (List.map (fun {Location.txt; loc} ->
          let doc = Doc.concat [Doc.text "'"; Doc.text txt] in
          print_comments doc cmt_tbl loc
          ) string_locs);
        Doc.dot;
        Doc.space;
        print_typ_expr typ cmt_tbl
      ]
    | Ptyp_package package_type ->
      print_package_type ~print_module_keyword_and_parens:true package_type cmt_tbl
    | Ptyp_class _ ->
      Doc.text "classes are not supported in types"
    | Ptyp_variant (row_fields, closed_flag, labels_opt) ->
      let print_row_field = function
      | Parsetree.Rtag ({txt}, attrs, true, []) ->
        Doc.concat [
          print_attributes attrs;
          Doc.concat [Doc.text "#"; print_ident_like ~allow_uident:true txt]
        ]
      | Rtag ({txt}, attrs, truth, types) ->
        let do_type t = match t.Parsetree.ptyp_desc with
        | Ptyp_tuple _ -> print_typ_expr t cmt_tbl
        | _ -> Doc.concat [ Doc.lparen; print_typ_expr t cmt_tbl; Doc.rparen ]
        in
        let printed_types = List.map do_type types in
        let cases = Doc.join ~sep:(Doc.concat [Doc.line; Doc.text "& "]) printed_types in
        let cases = if truth then Doc.concat [Doc.line; Doc.text "& "; cases] else cases in
        Doc.group (Doc.concat [
          print_attributes attrs;
          Doc.concat [Doc.text "#"; print_ident_like ~allow_uident:true txt];
          cases
        ])
      | Rinherit core_type ->
        print_typ_expr core_type cmt_tbl
      in
      let docs = List.map print_row_field row_fields in
      let cases = Doc.join ~sep:(Doc.concat [Doc.line; Doc.text "| "]) docs in
      let cases = if docs = [] then cases else Doc.concat [Doc.text "| "; cases] in
      let opening_symbol =
        if closed_flag = Open
        then Doc.greater_than
        else if labels_opt = None
        then Doc.nil
        else Doc.less_than in
      let has_labels = labels_opt <> None && labels_opt <> Some [] in
      let labels = match labels_opt with
      | None
      | Some([]) ->
        Doc.nil
      | Some(labels) ->
        Doc.concat (List.map (fun label -> Doc.concat [Doc.line; Doc.text "#" ; print_ident_like ~allow_uident:true label] ) labels)
      in
      let closing_symbol = if has_labels then Doc.text " >" else Doc.nil in
      Doc.group (Doc.concat [Doc.lbracket; opening_symbol; Doc.line; cases; closing_symbol; labels; Doc.line; Doc.rbracket])
    in
    let should_print_its_own_attributes = match typ_expr.ptyp_desc with
    | Ptyp_arrow _ (* es6 arrow types print their own attributes *)
    | Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")}, _) -> true
    | _ -> false
    in
    let doc = begin match typ_expr.ptyp_attributes with
    | _::_ as attrs when not should_print_its_own_attributes ->
      Doc.group (
        Doc.concat [
          print_attributes attrs;
          rendered_type;
        ]
      )
    | _ -> rendered_type
    end
    in
    print_comments doc cmt_tbl typ_expr.ptyp_loc

  and print_bs_object_sugar ~inline fields open_flag cmt_tbl =
    let doc = match fields with
    | [] -> Doc.concat [
        Doc.lbrace;
        (match open_flag with
        | Asttypes.Closed -> Doc.dot
        | Open -> Doc.dotdot);
        Doc.rbrace
      ]
    | fields ->
      Doc.concat [
        Doc.lbrace;
        (match open_flag with
        | Asttypes.Closed -> Doc.nil
        | Open -> Doc.dotdot);
        Doc.indent (
          Doc.concat [
            Doc.soft_line;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
              List.map (fun field -> print_object_field field cmt_tbl) fields
            )
          ]
        );
        Doc.trailing_comma;
        Doc.soft_line;
        Doc.rbrace;
      ]
    in
    if inline then doc else Doc.group doc

  and print_tuple_type ~inline (types: Parsetree.core_type list) cmt_tbl =
    let tuple = Doc.concat([
      Doc.lparen;
      Doc.indent (
        Doc.concat([
          Doc.soft_line;
          Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
            List.map (fun typexpr -> print_typ_expr typexpr cmt_tbl) types
          )
        ])
      );
      Doc.trailing_comma;
      Doc.soft_line;
      Doc.rparen;
    ])
    in
    if inline == false then Doc.group(tuple) else tuple

  and print_object_field (field : Parsetree.object_field) cmt_tbl =
    match field with
    | Otag (label_loc, attrs, typ) ->
      let lbl =
        let doc = Doc.text ("\"" ^ label_loc.txt ^ "\"") in
        print_comments doc cmt_tbl label_loc.loc
      in
      let doc = Doc.concat [
        print_attributes ~loc:label_loc.loc attrs;
        lbl;
        Doc.text ": ";
        print_typ_expr typ cmt_tbl;
      ] in
      let cmt_loc = {label_loc.loc with loc_end = typ.ptyp_loc.loc_end} in
      print_comments doc cmt_tbl cmt_loc
    | _ -> Doc.nil

  (* es6 arrow type arg
   * type t = (~foo: string, ~bar: float=?, unit) => unit
   * i.e. ~foo: string, ~bar: float *)
  and print_type_parameter (attrs, lbl, typ) cmt_tbl =
    let (is_uncurried, attrs) = ParsetreeViewer.process_uncurried_attribute attrs in
    let uncurried = if is_uncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil in
    let attrs = match attrs with
    | [] -> Doc.nil
    | attrs -> Doc.concat [
      Doc.join ~sep:Doc.line (List.map print_attribute attrs);
      Doc.line;
    ] in
    let label = match lbl with
    | Asttypes.Nolabel -> Doc.nil
    | Labelled lbl -> Doc.concat [
        Doc.text "~";
        print_ident_like lbl;
        Doc.text ": ";
      ]
    | Optional lbl -> Doc.concat [
        Doc.text "~";
        print_ident_like lbl;
        Doc.text ": ";
      ]
    in
    let optional_indicator = match lbl with
    | Asttypes.Nolabel
    | Labelled _ -> Doc.nil
    | Optional _lbl -> Doc.text "=?"
    in
    let doc = Doc.group (
      Doc.concat [
        uncurried;
        attrs;
        label;
        print_typ_expr typ cmt_tbl;
        optional_indicator;
      ]
    ) in
    print_comments doc cmt_tbl typ.ptyp_loc

  and print_value_binding ~rec_flag vb cmt_tbl i =
    let (has_gen_type, attrs) = ParsetreeViewer.split_gen_type_attr vb.pvb_attributes in
    let attrs = print_attributes ~loc:vb.pvb_pat.ppat_loc attrs in
		let header =
      if i == 0 then
        Doc.concat [
          if has_gen_type then Doc.text "export " else Doc.text "let ";
          rec_flag
      ] else
        Doc.concat [
          Doc.text "and ";
          if has_gen_type then Doc.text "export " else Doc.nil
        ]
		in
    match vb with
    | {pvb_pat =
        {ppat_desc = Ppat_constraint (pattern, {ptyp_desc = Ptyp_poly _})};
       pvb_expr =
         {pexp_desc = Pexp_newtype _} as expr
      }   ->
      let (_attrs, parameters, return_expr) = ParsetreeViewer.fun_expr expr in
      let abstract_type = match parameters with
      | [NewTypes {locs = vars}] ->
        Doc.concat [
          Doc.text "type ";
          Doc.join ~sep:Doc.space (List.map (fun var -> Doc.text var.Asttypes.txt) vars);
          Doc.dot;
        ]
      | _ -> Doc.nil
      in
      begin match return_expr.pexp_desc with
      | Pexp_constraint (expr, typ) ->
        Doc.group (
          Doc.concat [
            attrs;
            header;
            print_pattern pattern cmt_tbl;
            Doc.text ":";
            Doc.indent (
              Doc.concat [
                Doc.line;
                abstract_type;
                Doc.space;
                print_typ_expr typ cmt_tbl;
                Doc.text " =";
                Doc.concat [
                  Doc.line;
                  print_expression_with_comments expr cmt_tbl;
                ]
              ]
            )
          ]
        )
      | _ -> Doc.nil
      end
    | _ ->
    let (opt_braces, expr) = ParsetreeViewer.process_braces_attr vb.pvb_expr in
    let printed_expr =
      let doc = print_expression_with_comments vb.pvb_expr cmt_tbl in
      match Parens.expr vb.pvb_expr with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces  -> print_braces doc expr braces
      | Nothing -> doc
    in
    if ParsetreeViewer.is_pipe_expr vb.pvb_expr then
      Doc.custom_layout [
        Doc.group (
          Doc.concat [
            attrs;
            header;
            print_pattern vb.pvb_pat cmt_tbl;
            Doc.text " =";
            Doc.space;
            printed_expr;
          ]
        );
        Doc.group (
          Doc.concat [
            attrs;
            header;
            print_pattern vb.pvb_pat cmt_tbl;
            Doc.text " =";
            Doc.indent (
              Doc.concat [
                Doc.line;
                printed_expr;
              ]
            )
          ]
        );
      ]
		else
      let should_indent =
        match opt_braces with
        | Some _ -> false
        | _ ->
          ParsetreeViewer.is_binary_expression expr ||
          (match vb.pvb_expr with
          | {
              pexp_attributes = [({Location.txt="res.ternary"}, _)];
              pexp_desc = Pexp_ifthenelse (if_expr, _, _)
            }  ->
            ParsetreeViewer.is_binary_expression if_expr || ParsetreeViewer.has_attributes if_expr.pexp_attributes
        | { pexp_desc = Pexp_newtype _} -> false
        | e ->
            ParsetreeViewer.has_attributes e.pexp_attributes ||
            ParsetreeViewer.is_array_access e
          )
      in
      Doc.group (
        Doc.concat [
          attrs;
          header;
          print_pattern vb.pvb_pat cmt_tbl;
          Doc.text " =";
          if should_indent then
            Doc.indent (
              Doc.concat [
                Doc.line;
                printed_expr;
              ]
            )
          else
            Doc.concat [
              Doc.space;
              printed_expr;
            ]
        ]
      )

  and print_package_type ~print_module_keyword_and_parens (package_type: Parsetree.package_type) cmt_tbl =
    let doc = match package_type with
    | (longident_loc, []) -> Doc.group(
        Doc.concat [
          print_longident_location longident_loc cmt_tbl;
        ]
      )
    | (longident_loc, package_constraints) -> Doc.group(
        Doc.concat [
          print_longident_location longident_loc cmt_tbl;
          print_package_constraints package_constraints cmt_tbl;
          Doc.soft_line;
        ]
      )
    in
    if print_module_keyword_and_parens then
      Doc.concat[
        Doc.text "module(";
        doc;
        Doc.rparen
      ]
    else
      doc

  and print_package_constraints package_constraints cmt_tbl  =
    Doc.concat [
      Doc.text " with";
      Doc.indent (
        Doc.concat [
          Doc.line;
          Doc.join ~sep:Doc.line (
            List.mapi (fun i pc ->
              let (longident, typexpr) = pc in
              let cmt_loc = {longident.Asttypes.loc with
                loc_end = typexpr.Parsetree.ptyp_loc.loc_end
              } in
              let doc = print_package_constraint i cmt_tbl pc in
              print_comments doc cmt_tbl cmt_loc
            ) package_constraints
          )
        ]
      )
    ]

  and print_package_constraint i cmt_tbl (longident_loc, typ) =
    let prefix = if i == 0 then Doc.text "type " else Doc.text "and type " in
    Doc.concat [
      prefix;
      print_longident_location longident_loc cmt_tbl;
      Doc.text " = ";
      print_typ_expr typ cmt_tbl;
    ]

  and print_extension_with_comments ~at_module_lvl (string_loc, payload) cmt_tbl =
    let ext_name =
      let doc = Doc.concat [
        Doc.text "%";
        if at_module_lvl then Doc.text "%" else Doc.nil;
        Doc.text string_loc.Location.txt;
      ] in
      print_comments doc cmt_tbl string_loc.Location.loc
    in
    match payload with
    | Parsetree.PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
      let expr_doc = print_expression_with_comments expr cmt_tbl in
      let needs_parens = match attrs with | [] -> false | _ -> true in
      Doc.group (
        Doc.concat [
          ext_name;
          add_parens (
            Doc.concat [
              print_attributes attrs;
              if needs_parens then add_parens expr_doc else expr_doc;
            ]
          )
        ]
      )
    | _ -> ext_name

  and print_pattern (p : Parsetree.pattern) cmt_tbl =
    let pattern_without_attributes = match p.ppat_desc with
    | Ppat_any -> Doc.text "_"
    | Ppat_var var -> print_ident_like var.txt
    | Ppat_constant c -> print_constant c
    | Ppat_tuple patterns ->
      Doc.group(
        Doc.concat([
          Doc.lparen;
          Doc.indent (
            Doc.concat([
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map (fun pat ->
                  print_pattern pat cmt_tbl) patterns)
            ])
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen
        ])
      )
    | Ppat_array [] ->
      Doc.concat [
        Doc.lbracket;
        print_comments_inside cmt_tbl p.ppat_loc;
        Doc.rbracket;
      ]
    | Ppat_array patterns ->
      Doc.group(
        Doc.concat([
          Doc.text "[";
          Doc.indent (
            Doc.concat([
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map (fun pat ->
                  print_pattern pat cmt_tbl) patterns)
            ])
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.text "]";
        ])
      )
    | Ppat_construct({txt = Longident.Lident "()"}, _) ->
      Doc.concat [
        Doc.lparen;
        print_comments_inside cmt_tbl p.ppat_loc;
        Doc.rparen;
      ]
    | Ppat_construct({txt = Longident.Lident "[]"}, _) ->
      Doc.concat [
        Doc.text "list[";
        print_comments_inside cmt_tbl p.ppat_loc;
        Doc.rbracket;
      ]
    | Ppat_construct({txt = Longident.Lident "::"}, _) ->
      let (patterns, tail) = ParsetreeViewer.collect_patterns_from_list_construct [] p in
      let should_hug = match (patterns, tail) with
      | ([pat],
        {ppat_desc = Ppat_construct({txt = Longident.Lident "[]"}, _)}) when ParsetreeViewer.is_huggable_pattern pat -> true
      | _ -> false
      in
      let children = Doc.concat([
        if should_hug then Doc.nil else Doc.soft_line;
        Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
          (List.map (fun pat ->
            print_pattern pat cmt_tbl) patterns);
        begin match tail.Parsetree.ppat_desc with
        | Ppat_construct({txt = Longident.Lident "[]"}, _) -> Doc.nil
        | _ ->
          let doc = Doc.concat [Doc.text "..."; print_pattern tail cmt_tbl] in
          let tail = print_comments doc cmt_tbl tail.ppat_loc in
          Doc.concat([Doc.text ","; Doc.line; tail])
        end;
      ]) in
      Doc.group(
        Doc.concat([
          Doc.text "list[";
          if should_hug then children else Doc.concat [
            Doc.indent children;
            Doc.if_breaks (Doc.text ",") Doc.nil;
            Doc.soft_line;
          ];
          Doc.rbracket;
        ])
      )
    | Ppat_construct(constr_name, constructor_args) ->
      let constr_name = print_longident constr_name.txt in
      let args_doc = match constructor_args with
      | None -> Doc.nil
      | Some({ppat_loc; ppat_desc = Ppat_construct ({txt = Longident.Lident "()"}, _)}) ->
        Doc.concat [
          Doc.lparen;
          print_comments_inside cmt_tbl ppat_loc;
          Doc.rparen;
        ]
      | Some({ppat_desc = Ppat_tuple []; ppat_loc = loc}) ->
        Doc.concat [
          Doc.lparen;
          Doc.soft_line;
          print_comments_inside cmt_tbl loc;
          Doc.rparen;
        ]
      (* Some((1, 2) *)
      | Some({ppat_desc = Ppat_tuple [{ppat_desc = Ppat_tuple _} as arg]}) ->
        Doc.concat [
          Doc.lparen;
          print_pattern arg cmt_tbl;
          Doc.rparen;
        ]
      | Some({ppat_desc = Ppat_tuple patterns}) ->
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun pat -> print_pattern pat cmt_tbl) patterns
              );
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ]
      | Some(arg) ->
        let arg_doc = print_pattern arg cmt_tbl in
        let should_hug = ParsetreeViewer.is_huggable_pattern arg in
        Doc.concat [
          Doc.lparen;
          if should_hug then arg_doc
          else Doc.concat [
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                arg_doc;
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
          ];
          Doc.rparen;

        ]
      in
      Doc.group(Doc.concat [constr_name; args_doc])
    | Ppat_variant (label, None) ->
      Doc.concat [Doc.text "#"; print_ident_like ~allow_uident:true label]
    | Ppat_variant (label, variant_args) ->
      let variant_name =
        Doc.concat [Doc.text "#"; print_ident_like ~allow_uident:true label] in
      let args_doc = match variant_args with
      | None -> Doc.nil
      | Some({ppat_desc = Ppat_construct ({txt = Longident.Lident "()"}, _)}) ->
        Doc.text "()"
      | Some({ppat_desc = Ppat_tuple []; ppat_loc = loc}) ->
        Doc.concat [
          Doc.lparen;
          Doc.soft_line;
          print_comments_inside cmt_tbl loc;
          Doc.rparen;
        ]
      (* Some((1, 2) *)
      | Some({ppat_desc = Ppat_tuple [{ppat_desc = Ppat_tuple _} as arg]}) ->
        Doc.concat [
          Doc.lparen;
          print_pattern arg cmt_tbl;
          Doc.rparen;
        ]
      | Some({ppat_desc = Ppat_tuple patterns}) ->
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun pat -> print_pattern pat cmt_tbl) patterns
              );
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ]
      | Some(arg) ->
        let arg_doc = print_pattern arg cmt_tbl in
        let should_hug = ParsetreeViewer.is_huggable_pattern arg in
        Doc.concat [
          Doc.lparen;
          if should_hug then arg_doc
          else Doc.concat [
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                arg_doc;
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
          ];
          Doc.rparen;

        ]
      in
      Doc.group(Doc.concat [variant_name; args_doc])
    | Ppat_type ident ->
      Doc.concat [Doc.text "##"; print_ident_path ident cmt_tbl]
    | Ppat_record(rows, open_flag) ->
        Doc.group(
          Doc.concat([
            Doc.lbrace;
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map (fun row -> print_pattern_record_row row cmt_tbl) rows);
                begin match open_flag with
                | Open -> Doc.concat [Doc.text ","; Doc.line; Doc.text "_"]
                | Closed -> Doc.nil
                end;
              ]
            );
            Doc.if_breaks (Doc.text ",") Doc.nil;
            Doc.soft_line;
            Doc.rbrace;
          ])
        )

    | Ppat_exception p ->
        let needs_parens = match p.ppat_desc with
        | Ppat_or (_, _) | Ppat_alias (_, _) -> true
        | _ -> false
        in
        let pat =
          let p = print_pattern p cmt_tbl in
          if needs_parens then
            Doc.concat [Doc.text "("; p; Doc.text ")"]
          else
            p
        in
        Doc.group (
          Doc.concat [Doc.text "exception"; Doc.line; pat]
        )
    | Ppat_or _ ->
      (* Blue | Red | Green -> [Blue; Red; Green] *)
      let or_chain = ParsetreeViewer.collect_or_pattern_chain p in
      let docs = List.mapi (fun i pat ->
        let pattern_doc = print_pattern pat cmt_tbl in
        Doc.concat [
          if i == 0 then Doc.nil else Doc.concat [Doc.line; Doc.text "| "];
          match pat.ppat_desc with
          (* (Blue | Red) | (Green | Black) | White *)
          | Ppat_or _ -> add_parens pattern_doc
          | _ -> pattern_doc
        ]
      ) or_chain in
      Doc.group (Doc.concat docs)
    | Ppat_extension ext ->
      print_extension_with_comments ~at_module_lvl:false ext cmt_tbl
    | Ppat_lazy p ->
      let needs_parens = match p.ppat_desc with
      | Ppat_or (_, _) | Ppat_alias (_, _) -> true
      | _ -> false
      in
      let pat =
        let p = print_pattern p cmt_tbl in
        if needs_parens then
          Doc.concat [Doc.text "("; p; Doc.text ")"]
        else
          p
      in
      Doc.concat [Doc.text "lazy "; pat]
    | Ppat_alias (p, alias_loc) ->
      let needs_parens = match p.ppat_desc with
      | Ppat_or (_, _) | Ppat_alias (_, _) -> true
      | _ -> false
      in
      let rendered_pattern =
        let p = print_pattern p cmt_tbl in
        if needs_parens then
          Doc.concat [Doc.text "("; p; Doc.text ")"]
        else
          p
      in
      Doc.concat([
        rendered_pattern;
        Doc.text " as ";
        print_string_loc alias_loc cmt_tbl;
      ])

     (* Note: module(P : S) is represented as *)
     (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_constraint ({ppat_desc = Ppat_unpack string_loc}, {ptyp_desc = Ptyp_package package_type; ptyp_loc}) ->
        Doc.concat [
          Doc.text "module(";
          print_comments (Doc.text string_loc.txt) cmt_tbl string_loc.loc;
          Doc.text ": ";
          print_comments
            (print_package_type ~print_module_keyword_and_parens:false package_type cmt_tbl)
            cmt_tbl
            ptyp_loc;
          Doc.rparen;
        ]
    | Ppat_constraint (pattern, typ) ->
      Doc.concat [
        print_pattern pattern cmt_tbl;
        Doc.text ": ";
        print_typ_expr typ cmt_tbl;
      ]

     (* Note: module(P : S) is represented as *)
     (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_unpack string_loc ->
      Doc.concat [
        Doc.text "module(";
        print_comments (Doc.text string_loc.txt) cmt_tbl string_loc.loc;
        Doc.rparen;
      ]
    | Ppat_interval (a, b) ->
      Doc.concat [
        print_constant a;
        Doc.text " .. ";
        print_constant b;
      ]
    | Ppat_open _ -> Doc.nil
    in
    let doc = match p.ppat_attributes with
    | [] -> pattern_without_attributes
    | attrs ->
      Doc.group (
        Doc.concat [
          print_attributes attrs;
          pattern_without_attributes;
        ]
      )
    in
    print_comments doc cmt_tbl p.ppat_loc

  and print_pattern_record_row row cmt_tbl =
    match row with
    (* punned {x}*)
    | ({Location.txt=Longident.Lident ident} as longident,
       {Parsetree.ppat_desc=Ppat_var {txt;_}}) when ident = txt ->
        print_lident_path longident cmt_tbl
    | (longident, pattern) ->
      let loc_for_comments = {
        longident.loc with
        loc_end = pattern.Parsetree.ppat_loc.loc_end
      } in
      let doc = Doc.group (
        Doc.concat([
          print_lident_path longident cmt_tbl;
          Doc.text ": ";
          Doc.indent(
            Doc.concat [
              Doc.soft_line;
              print_pattern pattern cmt_tbl;
            ]
          )
        ])
      ) in
      print_comments doc cmt_tbl loc_for_comments

  and print_expression_with_comments expr cmt_tbl =
    let doc = print_expression expr cmt_tbl in
    print_comments doc cmt_tbl expr.Parsetree.pexp_loc

  and print_expression (e : Parsetree.expression) cmt_tbl =
    let printed_expression = match e.pexp_desc with
    | Parsetree.Pexp_constant c -> print_constant c
    | Pexp_construct _ when ParsetreeViewer.has_jsx_attribute e.pexp_attributes ->
      print_jsx_fragment e cmt_tbl
    | Pexp_construct ({txt = Longident.Lident "()"}, _) -> Doc.text "()"
    | Pexp_construct ({txt = Longident.Lident "[]"}, _) ->
      Doc.concat [
        Doc.text "list[";
        print_comments_inside cmt_tbl e.pexp_loc;
        Doc.rbracket;
      ]
    | Pexp_construct ({txt = Longident.Lident "::"}, _) ->
      let (expressions, spread) = ParsetreeViewer.collect_list_expressions e in
      let spread_doc = match spread with
      | Some(expr) -> Doc.concat [
          Doc.text ",";
          Doc.line;
          Doc.dotdotdot;
          let doc = print_expression_with_comments expr cmt_tbl in
          match Parens.expr expr with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces -> print_braces doc expr braces
          | Nothing -> doc
        ]
      | None -> Doc.nil
      in
      Doc.group(
        Doc.concat([
          Doc.text "list[";
          Doc.indent (
            Doc.concat([
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map
                  (fun expr ->
                    let doc = print_expression_with_comments expr cmt_tbl in
                    match Parens.expr expr with
                    | Parens.Parenthesized -> add_parens doc
                    | Braced braces -> print_braces doc expr braces
                    | Nothing -> doc
                  )
                  expressions);
              spread_doc;
            ])
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rbracket;
        ])
      )
    | Pexp_construct (longident_loc, args) ->
      let constr = print_longident_location longident_loc cmt_tbl in
      let args = match args with
      | None -> Doc.nil
      | Some({pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}) ->
        Doc.text "()"
      (* Some((1, 2)) *)
      | Some({pexp_desc = Pexp_tuple [{pexp_desc = Pexp_tuple _} as arg]}) ->
        Doc.concat [
          Doc.lparen;
          (let doc = print_expression_with_comments arg cmt_tbl in
          match Parens.expr arg with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces -> print_braces doc arg braces
          | Nothing -> doc);
          Doc.rparen;
        ]
      | Some({pexp_desc = Pexp_tuple args }) ->
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map
                  (fun expr ->
                    let doc = print_expression_with_comments expr cmt_tbl in
                    match Parens.expr expr with
                    | Parens.Parenthesized -> add_parens doc
                    | Braced braces -> print_braces doc expr braces
                    | Nothing -> doc)
                  args
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ]
      | Some(arg) ->
        let arg_doc =
          let doc = print_expression_with_comments arg cmt_tbl in
          match Parens.expr arg with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces -> print_braces doc arg braces
          | Nothing -> doc
        in
        let should_hug = ParsetreeViewer.is_huggable_expression arg in
        Doc.concat [
          Doc.lparen;
          if should_hug then arg_doc
          else Doc.concat [
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                arg_doc;
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
          ];
          Doc.rparen;
        ]
      in
      Doc.group(Doc.concat [constr; args])
    | Pexp_ident path ->
      print_lident_path path cmt_tbl
    | Pexp_tuple exprs ->
      Doc.group(
        Doc.concat([
          Doc.lparen;
          Doc.indent (
            Doc.concat([
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map (fun expr ->
                  let doc = print_expression_with_comments expr cmt_tbl in
                  match Parens.expr expr with
                  | Parens.Parenthesized -> add_parens doc
                  | Braced braces -> print_braces doc expr braces
                  | Nothing -> doc)
                 exprs)
            ])
          );
          Doc.if_breaks (Doc.text ",") Doc.nil;
          Doc.soft_line;
          Doc.rparen;
        ])
      )
    | Pexp_array [] ->
      Doc.concat [
        Doc.lbracket;
        print_comments_inside cmt_tbl e.pexp_loc;
        Doc.rbracket;
      ]
    | Pexp_array exprs ->
      Doc.group(
        Doc.concat([
          Doc.lbracket;
          Doc.indent (
            Doc.concat([
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map (fun expr ->
                  let doc = print_expression_with_comments expr cmt_tbl in
                  match Parens.expr expr with
                  | Parens.Parenthesized -> add_parens doc
                  | Braced braces -> print_braces doc expr braces
                  | Nothing -> doc
                  ) exprs)
            ])
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rbracket;
        ])
      )
    | Pexp_variant (label, args) ->
      let variant_name =
        Doc.concat [Doc.text "#"; print_ident_like ~allow_uident:true label] in
      let args = match args with
      | None -> Doc.nil
      | Some({pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}) ->
        Doc.text "()"
      (* #poly((1, 2) *)
      | Some({pexp_desc = Pexp_tuple [{pexp_desc = Pexp_tuple _} as arg]}) ->
        Doc.concat [
          Doc.lparen;
          (let doc = print_expression_with_comments arg cmt_tbl in
          match Parens.expr arg with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces -> print_braces doc arg braces
          | Nothing -> doc);
          Doc.rparen;
        ]
      | Some({pexp_desc = Pexp_tuple args }) ->
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map
                  (fun expr ->
                    let doc = print_expression_with_comments expr cmt_tbl in
                    match Parens.expr expr with
                    | Parens.Parenthesized -> add_parens doc
                    | Braced braces -> print_braces doc expr braces
                    | Nothing -> doc)
                  args
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ]
      | Some(arg) ->
        let arg_doc =
          let doc = print_expression_with_comments arg cmt_tbl in
          match Parens.expr arg with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces -> print_braces doc arg braces
          | Nothing -> doc
        in
        let should_hug = ParsetreeViewer.is_huggable_expression arg in
        Doc.concat [
          Doc.lparen;
          if should_hug then arg_doc
          else Doc.concat [
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                arg_doc;
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
          ];
          Doc.rparen;
        ]
      in
      Doc.group(Doc.concat [variant_name; args])
    | Pexp_record (rows, spread_expr) ->
      let spread = match spread_expr with
      | None -> Doc.nil
      | Some expr -> Doc.concat [
          Doc.dotdotdot;
          (let doc = print_expression_with_comments expr cmt_tbl in
          match Parens.expr expr with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces -> print_braces doc expr braces
          | Nothing -> doc);
          Doc.comma;
          Doc.line;
        ]
      in
      (* If the record is written over multiple lines, break automatically
       * `let x = {a: 1, b: 3}` -> same line, break when line-width exceeded
       * `let x = {
       *   a: 1,
       *   b: 2,
       *  }` -> record is written on multiple lines, break the group *)
      let force_break =
        e.pexp_loc.loc_start.pos_lnum < e.pexp_loc.loc_end.pos_lnum
      in
      Doc.breakable_group ~force_break (
        Doc.concat([
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              spread;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map (fun row -> print_record_row row cmt_tbl) rows)
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rbrace;
        ])
      )
    | Pexp_extension extension ->
      begin match extension with
      | (
          {txt = "obj"},
          PStr [{
            pstr_loc = loc;
            pstr_desc = Pstr_eval({pexp_desc = Pexp_record (rows, _)}, [])
          }]
        ) ->
        (* If the object is written over multiple lines, break automatically
         * `let x = {"a": 1, "b": 3}` -> same line, break when line-width exceeded
         * `let x = {
         *   "a": 1,
         *   "b": 2,
         *  }` -> object is written on multiple lines, break the group *)
        let force_break =
          loc.loc_start.pos_lnum < loc.loc_end.pos_lnum
        in
        Doc.breakable_group ~force_break (
          Doc.concat([
            Doc.lbrace;
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map (fun row -> print_bs_object_row row cmt_tbl) rows)
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
            Doc.rbrace;
          ])
        )
      | extension ->
        print_extension_with_comments ~at_module_lvl:false extension cmt_tbl
      end
    | Pexp_apply _ ->
      if ParsetreeViewer.is_unary_expression e then
        print_unary_expression e cmt_tbl
      else if ParsetreeViewer.is_template_literal e then
        print_template_literal e cmt_tbl
      else if ParsetreeViewer.is_binary_expression e then
        print_binary_expression e cmt_tbl
      else
        print_pexp_apply e cmt_tbl
    | Pexp_unreachable -> Doc.dot
    | Pexp_field (expr, longident_loc) ->
      let lhs =
        let doc = print_expression_with_comments expr cmt_tbl in
        match Parens.field_expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        lhs;
        Doc.dot;
        print_lident_path longident_loc cmt_tbl;
      ]
    | Pexp_setfield (expr1, longident_loc, expr2) ->
      print_set_field_expr e.pexp_attributes expr1 longident_loc expr2 e.pexp_loc cmt_tbl
    | Pexp_ifthenelse (_ifExpr, _thenExpr, _elseExpr) ->
      if ParsetreeViewer.is_ternary_expr e then
        let (parts, alternate) = ParsetreeViewer.collect_ternary_parts e in
        let ternary_doc = match parts with
        | (condition1, consequent1)::rest ->
          Doc.group (Doc.concat [
            print_ternary_operand condition1 cmt_tbl;
            Doc.indent (
              Doc.concat [
                Doc.line;
                Doc.indent (
                  Doc.concat [
                    Doc.text "? ";
                    print_ternary_operand consequent1 cmt_tbl
                  ]
                );
                Doc.concat (
                  List.map (fun (condition, consequent) ->
                    Doc.concat [
                      Doc.line;
                      Doc.text ": ";
                      print_ternary_operand condition cmt_tbl;
                      Doc.line;
                      Doc.text "? ";
                      print_ternary_operand consequent cmt_tbl;
                    ]
                  ) rest
                );
                Doc.line;
                Doc.text ": ";
                Doc.indent (print_ternary_operand alternate cmt_tbl);
              ]
            )
          ])
        | _ -> Doc.nil
        in
        let attrs = ParsetreeViewer.filter_ternary_attributes e.pexp_attributes in
        let needs_parens = match ParsetreeViewer.filter_parsing_attrs attrs with
        | [] -> false | _ -> true
        in
        Doc.concat [
          print_attributes attrs;
          if needs_parens then add_parens ternary_doc else ternary_doc;
        ]
      else
      let (ifs, else_expr) = ParsetreeViewer.collect_if_expressions e in
      let if_docs = Doc.join ~sep:Doc.space (
        List.mapi (fun i (if_expr, then_expr) ->
          let if_txt = if i > 0 then Doc.text "else if " else  Doc.text "if " in
          let condition =
            if ParsetreeViewer.is_block_expr if_expr then
              print_expression_block ~braces:true if_expr cmt_tbl
            else
              let doc = print_expression_with_comments if_expr cmt_tbl in
              match Parens.expr if_expr with
              | Parens.Parenthesized -> add_parens doc
              | Braced braces -> print_braces doc if_expr braces
              | Nothing -> Doc.if_breaks (add_parens doc) doc
          in
          Doc.concat [
            if_txt;
            Doc.group (condition);
            Doc.space;
            let then_expr = match ParsetreeViewer.process_braces_attr then_expr with
            (* This case only happens when coming from Reason, we strip braces *)
            | (Some _, expr) -> expr
            | _ -> then_expr
            in
            print_expression_block ~braces:true then_expr cmt_tbl;
          ]
        ) ifs
      ) in
      let else_doc = match else_expr with
      | None -> Doc.nil
      | Some expr -> Doc.concat [
          Doc.text " else ";
          print_expression_block ~braces:true expr cmt_tbl;
        ]
      in
      Doc.concat [
        print_attributes e.pexp_attributes;
        if_docs;
        else_doc;
      ]
    | Pexp_while (expr1, expr2) ->
      let condition =
        let doc = print_expression_with_comments expr1 cmt_tbl in
        match Parens.expr expr1 with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc expr1 braces
        | Nothing -> doc
      in
      Doc.breakable_group ~force_break:true (
        Doc.concat [
          Doc.text "while ";
          if ParsetreeViewer.is_block_expr expr1 then
            condition
          else
            Doc.group (
              Doc.if_breaks (add_parens condition) condition
            );
          Doc.space;
          print_expression_block ~braces:true expr2 cmt_tbl;
        ]
      )
    | Pexp_for (pattern, from_expr, to_expr, direction_flag, body) ->
      Doc.breakable_group ~force_break:true (
        Doc.concat [
          Doc.text "for ";
          print_pattern pattern cmt_tbl;
          Doc.text " in ";
          (let doc = print_expression_with_comments from_expr cmt_tbl in
          match Parens.expr from_expr with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces -> print_braces doc from_expr braces
          | Nothing -> doc);
          print_direction_flag direction_flag;
          (let doc = print_expression_with_comments to_expr cmt_tbl in
          match Parens.expr to_expr with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces -> print_braces doc to_expr braces
          | Nothing -> doc);
          Doc.space;
          print_expression_block ~braces:true body cmt_tbl;
        ]
      )
    | Pexp_constraint(
        {pexp_desc = Pexp_pack mod_expr},
        {ptyp_desc = Ptyp_package package_type; ptyp_loc}
      ) ->
      Doc.group (
        Doc.concat [
          Doc.text "module(";
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              print_mod_expr mod_expr cmt_tbl;
              Doc.text ": ";
              print_comments
                (print_package_type ~print_module_keyword_and_parens:false package_type cmt_tbl)
                cmt_tbl
                ptyp_loc
            ]
          );
          Doc.soft_line;
          Doc.rparen;
        ]
      )

    | Pexp_constraint (expr, typ) ->
      let expr_doc =
        let doc = print_expression_with_comments expr cmt_tbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        expr_doc;
        Doc.text ": ";
        print_typ_expr typ cmt_tbl;
      ]
    | Pexp_letmodule ({txt = _modName}, _modExpr, _expr) ->
      print_expression_block ~braces:true e cmt_tbl
    | Pexp_letexception (_extensionConstructor, _expr) ->
      print_expression_block ~braces:true e cmt_tbl
    | Pexp_assert expr ->
      let rhs =
        let doc = print_expression_with_comments expr cmt_tbl in
        match Parens.lazy_or_assert_expr_rhs expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        Doc.text "assert ";
        rhs;
      ]
    | Pexp_lazy expr ->
      let rhs =
        let doc = print_expression_with_comments expr cmt_tbl in
        match Parens.lazy_or_assert_expr_rhs expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.group (
        Doc.concat [
          Doc.text "lazy ";
          rhs;
        ]
      )
    | Pexp_open (_overrideFlag, _longidentLoc, _expr) ->
      print_expression_block ~braces:true e cmt_tbl
    | Pexp_pack (mod_expr) ->
      Doc.group (Doc.concat [
        Doc.text "module(";
        Doc.indent (
          Doc.concat [
            Doc.soft_line;
            print_mod_expr mod_expr cmt_tbl;
          ]
        );
        Doc.soft_line;
        Doc.rparen;
      ])
    | Pexp_sequence _ ->
      print_expression_block ~braces:true e cmt_tbl
    | Pexp_let _ ->
      print_expression_block ~braces:true e cmt_tbl
    | Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt="__x"}}, ({pexp_desc = Pexp_apply _})) ->

      (* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
      print_expression_with_comments (ParsetreeViewer.rewrite_underscore_apply e) cmt_tbl
    | Pexp_fun _ | Pexp_newtype _ ->
      let (attrs_on_arrow, parameters, return_expr) = ParsetreeViewer.fun_expr e in
      let (uncurried, attrs) =
        ParsetreeViewer.process_uncurried_attribute attrs_on_arrow
      in
      let (return_expr, typ_constraint) = match return_expr.pexp_desc with
      | Pexp_constraint (expr, typ) -> (
          {expr with pexp_attributes = List.concat [
            expr.pexp_attributes;
            return_expr.pexp_attributes;
          ]},
          Some typ
        )
      | _ -> (return_expr, None)
      in
      let has_constraint = match typ_constraint with | Some _ -> true | None -> false in
      let parameters_doc = print_expr_fun_parameters
        ~in_callback:false
        ~uncurried
        ~has_constraint
        parameters
        cmt_tbl
      in
      let return_expr_doc =
        let (opt_braces, _) = ParsetreeViewer.process_braces_attr return_expr in
        let should_inline = match (return_expr.pexp_desc, opt_braces) with
        | (_, Some _ ) -> true
        | ((Pexp_array _
        | Pexp_tuple _
        | Pexp_construct (_, Some _)
        | Pexp_record _), _) -> true
        | _ -> false
        in
        let should_indent = match return_expr.pexp_desc with
        | Pexp_sequence _
        | Pexp_let _
        | Pexp_letmodule _
        | Pexp_letexception _
        | Pexp_open _ -> false
        | _ -> true
        in
        let return_doc =
          let doc = print_expression_with_comments return_expr cmt_tbl in
          match Parens.expr return_expr with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces  -> print_braces doc return_expr braces
          | Nothing -> doc
        in
        if should_inline then Doc.concat [
          Doc.space;
          return_doc;
        ] else
          Doc.group (
            if should_indent then
              Doc.indent (
                Doc.concat [
                  Doc.line;
                  return_doc;
                ]
              )
            else
              Doc.concat [
                Doc.space;
                return_doc
              ]
          )
      in
      let typ_constraint_doc = match typ_constraint with
      | Some(typ) -> Doc.concat [Doc.text ": "; print_typ_expr typ cmt_tbl]
      | _ -> Doc.nil
      in
      let attrs = print_attributes attrs in
      Doc.group (
        Doc.concat [
          attrs;
          parameters_doc;
          typ_constraint_doc;
          Doc.text " =>";
          return_expr_doc;
        ]
      )
    | Pexp_try (expr, cases) ->
      let expr_doc =
        let doc = print_expression_with_comments expr cmt_tbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        Doc.text "try ";
        expr_doc;
        Doc.text " catch ";
        print_cases cases cmt_tbl;
      ]
    | Pexp_match (expr, cases) ->
      let expr_doc =
        let doc = print_expression_with_comments expr cmt_tbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        Doc.text "switch ";
        expr_doc;
        Doc.space;
        print_cases cases cmt_tbl;
      ]
    | Pexp_function cases ->
      Doc.concat [
        Doc.text "x => switch x ";
        print_cases cases cmt_tbl;
      ]
    | Pexp_coerce (expr, typ_opt, typ) ->
      let doc_expr = print_expression_with_comments expr cmt_tbl in
      let doc_typ = print_typ_expr typ cmt_tbl in
      let of_type = match typ_opt with
      | None -> Doc.nil
      | Some(typ1) ->
        Doc.concat [Doc.text ": "; print_typ_expr typ1 cmt_tbl]
      in
      Doc.concat [Doc.lparen; doc_expr; of_type; Doc.text " :> "; doc_typ; Doc.rparen]
    | Pexp_send _ ->
      Doc.text "Pexp_send not implemented in printer"
    | Pexp_new _ ->
      Doc.text "Pexp_new not implemented in printer"
    | Pexp_setinstvar _ ->
      Doc.text "Pexp_setinstvar not implemented in printer"
    | Pexp_override _ ->
      Doc.text "Pexp_override not implemented in printer"
    | Pexp_poly _ ->
      Doc.text "Pexp_poly not implemented in printer"
    | Pexp_object _ ->
      Doc.text "Pexp_object not implemented in printer"
    in
    let should_print_its_own_attributes = match e.pexp_desc with
    | Pexp_apply _
    | Pexp_fun _
    | Pexp_newtype _
    | Pexp_setfield _
    | Pexp_ifthenelse _ -> true
    | Pexp_construct _ when ParsetreeViewer.has_jsx_attribute e.pexp_attributes -> true
    | _ -> false
    in
    match e.pexp_attributes with
    | [] -> printed_expression
    | attrs when not should_print_its_own_attributes ->
      Doc.group (
        Doc.concat [
          print_attributes attrs;
          printed_expression;
        ]
      )
    | _ -> printed_expression

  and print_pexp_fun ~in_callback e cmt_tbl =
      let (attrs_on_arrow, parameters, return_expr) = ParsetreeViewer.fun_expr e in
      let (uncurried, attrs) =
        ParsetreeViewer.process_uncurried_attribute attrs_on_arrow
      in
      let (return_expr, typ_constraint) = match return_expr.pexp_desc with
      | Pexp_constraint (expr, typ) -> (
          {expr with pexp_attributes = List.concat [
            expr.pexp_attributes;
            return_expr.pexp_attributes;
          ]},
          Some typ
        )
      | _ -> (return_expr, None)
      in
      let parameters_doc = print_expr_fun_parameters
        ~in_callback
        ~uncurried
        ~has_constraint:(match typ_constraint with | Some _ -> true | None -> false)
        parameters cmt_tbl in
      let return_should_indent = match return_expr.pexp_desc with
      | Pexp_sequence _ | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _ | Pexp_open _ -> false
      | _ -> true
      in
      let return_expr_doc =
        let (opt_braces, _) = ParsetreeViewer.process_braces_attr return_expr in
        let should_inline = match (return_expr.pexp_desc, opt_braces) with
        | (_, Some _) -> true
        | ((Pexp_array _
        | Pexp_tuple _
        | Pexp_construct (_, Some _)
        | Pexp_record _), _) -> true
        | _ -> false
        in
        let return_doc =
          let doc = print_expression_with_comments return_expr cmt_tbl in
          match Parens.expr return_expr with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces  -> print_braces doc return_expr braces
          | Nothing -> doc
        in
        if should_inline then Doc.concat [
          Doc.space;
          return_doc;
        ] else
          Doc.group (
            if return_should_indent then
              Doc.concat [
                Doc.indent (
                  Doc.concat [
                    Doc.line;
                    return_doc;
                  ]
                );
                if in_callback then Doc.soft_line else Doc.nil;
              ]
            else
              Doc.concat [
                Doc.space;
                return_doc;
              ]
          )
      in
      let typ_constraint_doc = match typ_constraint with
      | Some(typ) -> Doc.concat [
          Doc.text ": ";
          print_typ_expr typ cmt_tbl
        ]
      | _ -> Doc.nil
      in
      Doc.group (
        Doc.concat [
          print_attributes attrs;
          parameters_doc;
          typ_constraint_doc;
          Doc.text " =>";
          return_expr_doc;
        ]
      )

  and print_ternary_operand expr cmt_tbl =
    let doc = print_expression_with_comments expr cmt_tbl in
    match Parens.ternary_operand expr with
    | Parens.Parenthesized -> add_parens doc
    | Braced braces  -> print_braces doc expr braces
    | Nothing -> doc

  and print_set_field_expr attrs lhs longident_loc rhs loc cmt_tbl =
    let rhs_doc =
      let doc = print_expression_with_comments rhs cmt_tbl in
      match Parens.set_field_expr_rhs rhs with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces  -> print_braces doc rhs braces
      | Nothing -> doc
    in
    let lhs_doc =
      let doc = print_expression_with_comments lhs cmt_tbl in
      match Parens.field_expr lhs with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces  -> print_braces doc lhs braces
      | Nothing -> doc
    in
    let should_indent = ParsetreeViewer.is_binary_expression rhs in
    let doc = Doc.group (Doc.concat [
      lhs_doc;
      Doc.dot;
      print_lident_path longident_loc cmt_tbl;
      Doc.text " =";
      if should_indent then Doc.group (
        Doc.indent (
          (Doc.concat [Doc.line; rhs_doc])
        )
      ) else
        Doc.concat [Doc.space; rhs_doc]
    ]) in
    let doc = match attrs with
    | [] -> doc
    | attrs ->
      Doc.group (
        Doc.concat [
          print_attributes attrs;
          doc
        ]
      )
    in
    print_comments doc cmt_tbl loc

  and print_template_literal expr cmt_tbl =
    let tag = ref "j" in
    let rec walk_expr expr =
      let open Parsetree in
      match expr.pexp_desc with
      | Pexp_apply (
          {pexp_desc = Pexp_ident {txt = Longident.Lident "^"}},
          [Nolabel, arg1; Nolabel, arg2]
        ) ->
          let lhs = walk_expr arg1 in
          let rhs = walk_expr arg2 in
          Doc.concat [lhs; rhs]
      | Pexp_constant (Pconst_string (txt, Some prefix)) ->
        tag := prefix;
        Doc.text txt
      | _ ->
        let doc = print_expression_with_comments expr cmt_tbl in
        Doc.concat [Doc.text "${"; doc; Doc.rbrace]
    in
    let content = walk_expr expr in
    Doc.concat [
      if !tag = "j" then Doc.nil else Doc.text !tag;
      Doc.text "`";
      content;
      Doc.text "`"
    ]

  and print_unary_expression expr cmt_tbl =
    let print_unary_operator op = Doc.text (
      match op with
      | "~+" -> "+"
      | "~+." -> "+."
      | "~-" -> "-"
      | "~-." ->  "-."
      | "not" -> "!"
      | _ -> assert false
    ) in
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, operand]
      ) ->
      let printed_operand =
        let doc = print_expression_with_comments operand cmt_tbl in
        match Parens.unary_expr_operand operand with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc operand braces
        | Nothing -> doc
      in
      let doc = Doc.concat [
        print_unary_operator operator;
        printed_operand;
      ] in
      print_comments doc cmt_tbl expr.pexp_loc
    | _ -> assert false

  and print_binary_expression (expr : Parsetree.expression) cmt_tbl =
    let print_binary_operator ~inline_rhs operator =
      let operator_txt = match operator with
      | "|." -> "->"
      | "^" -> "++"
      | "=" -> "=="
      | "==" -> "==="
      | "<>" -> "!="
      | "!=" -> "!=="
      | txt -> txt
      in
      let spacing_before_operator =
        if operator = "|." then Doc.soft_line
        else if operator = "|>" then Doc.line
        else Doc.space;
      in
      let spacing_after_operator =
        if operator = "|." then Doc.nil
        else if operator = "|>" then Doc.space
        else if inline_rhs then Doc.space else Doc.line
      in
      Doc.concat [
        spacing_before_operator;
        Doc.text operator_txt;
        spacing_after_operator;
      ]
    in
    let print_operand ~is_lhs expr parent_operator =
      let rec flatten ~is_lhs expr parent_operator =
        if ParsetreeViewer.is_binary_expression expr then
          begin match expr with
          | {pexp_desc = Pexp_apply (
              {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
              [_, left; _, right]
            )} ->
            if ParsetreeViewer.flattenable_operators parent_operator operator &&
               not (ParsetreeViewer.has_attributes expr.pexp_attributes)
            then
              let left_printed = flatten ~is_lhs:true left operator in
              let right_printed =
                let (_, right_attrs) =
                  ParsetreeViewer.partition_printeable_attributes right.pexp_attributes
                in
                let doc =
                  print_expression_with_comments
                    {right with pexp_attributes = right_attrs}
                    cmt_tbl
                in
                let doc = if Parens.flatten_operand_rhs parent_operator right then
                  Doc.concat [Doc.lparen; doc; Doc.rparen]
                else
                  doc
                in
                let printeable_attrs =
                  ParsetreeViewer.filter_printeable_attributes right.pexp_attributes
                in
                Doc.concat [print_attributes printeable_attrs; doc]
              in
              let doc = Doc.concat [
                left_printed;
                print_binary_operator ~inline_rhs:false operator;
                right_printed;
              ] in
              let doc =
                if not is_lhs && (Parens.rhs_binary_expr_operand operator expr) then
                  Doc.concat [Doc.lparen; doc; Doc.rparen]
                else doc
              in
              print_comments doc cmt_tbl expr.pexp_loc
            else (
              let doc = print_expression_with_comments {expr with pexp_attributes = []} cmt_tbl in
              let doc = if Parens.sub_binary_expr_operand parent_operator operator ||
                (expr.pexp_attributes <> [] &&
                  (ParsetreeViewer.is_binary_expression expr ||
                ParsetreeViewer.is_ternary_expr expr))
              then
                Doc.concat [Doc.lparen; doc; Doc.rparen]
              else doc
              in Doc.concat [
                print_attributes expr.pexp_attributes;
                doc
              ]
            )
          | _ -> assert false
          end
        else
          begin match expr.pexp_desc with
          | Pexp_setfield (lhs, field, rhs) ->
            let doc = print_set_field_expr expr.pexp_attributes lhs field rhs expr.pexp_loc cmt_tbl  in
            if is_lhs then add_parens doc else doc
          | Pexp_apply(
              {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}},
              [(Nolabel, lhs); (Nolabel, rhs)]
            ) ->
            let rhs_doc = print_expression_with_comments rhs cmt_tbl in
            let lhs_doc = print_expression_with_comments lhs cmt_tbl in
            (* TODO: unify indentation of "=" *)
            let should_indent = ParsetreeViewer.is_binary_expression rhs in
            let doc = Doc.group (
              Doc.concat [
                lhs_doc;
                Doc.text " =";
                if should_indent then Doc.group (
                  Doc.indent (Doc.concat [Doc.line; rhs_doc])
                ) else
                  Doc.concat [Doc.space; rhs_doc]
              ]
            ) in
            let doc = match expr.pexp_attributes with
            | [] -> doc
            | attrs ->
              Doc.group (
                Doc.concat [
                  print_attributes attrs;
                  doc
                ]
              )
            in
            if is_lhs then add_parens doc else doc
          | _ ->
            let doc = print_expression_with_comments expr cmt_tbl in
            begin match Parens.binary_expr_operand ~is_lhs expr with
            | Parens.Parenthesized -> add_parens doc
            | Braced braces  -> print_braces doc expr braces
            | Nothing -> doc
            end
          end
      in
      flatten ~is_lhs expr parent_operator
    in
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident (("|." | "|>") as op)}},
        [Nolabel, lhs; Nolabel, rhs]
      ) when not (
          ParsetreeViewer.is_binary_expression lhs ||
          ParsetreeViewer.is_binary_expression rhs
      ) ->
      let lhs_doc = print_operand ~is_lhs:true lhs op in
      let rhs_doc = print_operand ~is_lhs:false rhs op in
      Doc.group (
        Doc.concat [
          lhs_doc;
          (match op with
          | "|." -> Doc.text "->"
          | "|>" -> Doc.text " |> "
          | _ -> assert false);
          rhs_doc;
        ]
      )
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, lhs; Nolabel, rhs]
      ) ->
      let right =
        let operator_with_rhs =
          let rhs_doc = print_operand ~is_lhs:false rhs operator in
          Doc.concat [
            print_binary_operator
              ~inline_rhs:(ParsetreeViewer.should_inline_rhs_binary_expr rhs) operator;
            rhs_doc;
        ] in
        if ParsetreeViewer.should_indent_binary_expr expr then
          Doc.group (Doc.indent operator_with_rhs)
        else operator_with_rhs
      in
      let doc = Doc.group (
        Doc.concat [
          print_operand ~is_lhs:true lhs operator;
          right
        ]
      ) in
      Doc.group (
        Doc.concat [
          print_attributes expr.pexp_attributes;
          match Parens.binary_expr {expr with
            pexp_attributes = List.filter (fun attr ->
              match attr with
              | ({Location.txt = ("res.braces")}, _) -> false
              | _ -> true
            ) expr.pexp_attributes
          } with
          | Braced(braces_loc) -> print_braces doc expr braces_loc
          | Parenthesized -> add_parens doc
          | Nothing -> doc;
        ]
      )
    | _ -> Doc.nil

  (* callExpr(arg1, arg2) *)
  and print_pexp_apply expr cmt_tbl =
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident "##"}},
        [Nolabel, parent_expr; Nolabel, member_expr]
      ) ->
        let parent_doc =
          let doc = print_expression_with_comments parent_expr cmt_tbl in
          match Parens.unary_expr_operand parent_expr with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces  -> print_braces doc parent_expr braces
          | Nothing -> doc
        in
        let member =
          let member_doc = match member_expr.pexp_desc with
          | Pexp_ident lident ->
            print_comments (print_longident lident.txt) cmt_tbl member_expr.pexp_loc
          | _ -> print_expression_with_comments member_expr cmt_tbl
          in
          Doc.concat [Doc.text "\""; member_doc; Doc.text "\""]
        in
        Doc.group (Doc.concat [
          print_attributes expr.pexp_attributes;
          parent_doc;
          Doc.lbracket;
          member;
          Doc.rbracket;
        ])
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}},
        [Nolabel, lhs; Nolabel, rhs]
      ) ->
        let rhs_doc =
          let doc = print_expression_with_comments rhs cmt_tbl in
          match Parens.expr rhs with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces  -> print_braces doc rhs braces
          | Nothing -> doc
        in
        (* TODO: unify indentation of "=" *)
        let should_indent = not (ParsetreeViewer.is_braced_expr rhs) && ParsetreeViewer.is_binary_expression rhs in
        let doc = Doc.group(
          Doc.concat [
            print_expression_with_comments lhs cmt_tbl;
            Doc.text " =";
            if should_indent then Doc.group (
              Doc.indent (
                (Doc.concat [Doc.line; rhs_doc])
              )
            ) else
              Doc.concat [Doc.space; rhs_doc]
          ]
        ) in
        begin match expr.pexp_attributes with
        | [] -> doc
        | attrs ->
          Doc.group (
            Doc.concat [
              print_attributes attrs;
              doc
            ]
          )
        end
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "get")}},
        [Nolabel, parent_expr; Nolabel, member_expr]
      ) ->
        let member =
          let member_doc =
            let doc = print_expression_with_comments member_expr cmt_tbl in
            match Parens.expr member_expr with
            | Parens.Parenthesized -> add_parens doc
            | Braced braces  -> print_braces doc member_expr braces
            | Nothing -> doc
          in
          let should_inline = match member_expr.pexp_desc with
          | Pexp_constant _ | Pexp_ident _ -> true
          | _ -> false
          in
          if should_inline then member_doc else (
            Doc.concat [
              Doc.indent (
                Doc.concat [
                  Doc.soft_line;
                  member_doc;
                ]
              );
              Doc.soft_line
            ]
          )
        in
        let parent_doc =
          let doc = print_expression_with_comments parent_expr cmt_tbl in
          match Parens.unary_expr_operand parent_expr with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces  -> print_braces doc parent_expr braces
          | Nothing -> doc
        in
        Doc.group (Doc.concat [
          print_attributes expr.pexp_attributes;
          parent_doc;
          Doc.lbracket;
          member;
          Doc.rbracket;
        ])
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "set")}},
        [Nolabel, parent_expr; Nolabel, member_expr; Nolabel, target_expr]
      ) ->
        let member =
          let member_doc =
            let doc = print_expression_with_comments member_expr cmt_tbl in
            match Parens.expr member_expr with
            | Parens.Parenthesized -> add_parens doc
            | Braced braces  -> print_braces doc member_expr braces
            | Nothing -> doc
          in
          let should_inline = match member_expr.pexp_desc with
          | Pexp_constant _ | Pexp_ident _ -> true
          | _ -> false
          in
          if should_inline then member_doc else (
            Doc.concat [
              Doc.indent (
                Doc.concat [
                  Doc.soft_line;
                  member_doc;
                ]
              );
              Doc.soft_line
            ]
          )
        in
        let should_indent_target_expr =
          if ParsetreeViewer.is_braced_expr target_expr then
            false
          else
          ParsetreeViewer.is_binary_expression target_expr ||
          (match target_expr with
          | {
              pexp_attributes = [({Location.txt="res.ternary"}, _)];
              pexp_desc = Pexp_ifthenelse (if_expr, _, _)
            }  ->
            ParsetreeViewer.is_binary_expression if_expr || ParsetreeViewer.has_attributes if_expr.pexp_attributes
        | { pexp_desc = Pexp_newtype _} -> false
        | e ->
            ParsetreeViewer.has_attributes e.pexp_attributes ||
            ParsetreeViewer.is_array_access e
          )
        in
        let target_expr =
          let doc = print_expression_with_comments target_expr cmt_tbl in
          match Parens.expr target_expr with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces  -> print_braces doc target_expr braces
          | Nothing -> doc
        in
        let parent_doc =
          let doc = print_expression_with_comments parent_expr cmt_tbl in
          match Parens.unary_expr_operand parent_expr with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces  -> print_braces doc parent_expr braces
          | Nothing -> doc
        in
        Doc.group (
          Doc.concat [
          print_attributes expr.pexp_attributes;
          parent_doc;
          Doc.lbracket;
          member;
          Doc.rbracket;
          Doc.text " =";
          if should_indent_target_expr then
            Doc.indent (
              Doc.concat [
                Doc.line;
                target_expr;
              ]
            )
          else
            Doc.concat [
              Doc.space;
              target_expr;
            ]
          ]
        )
    (* TODO: cleanup, are those branches even remotely performant? *)
    | Pexp_apply (
        {pexp_desc = Pexp_ident lident},
        args
      ) when ParsetreeViewer.is_jsx_expression expr ->
      print_jsx_expression lident args cmt_tbl
    | Pexp_apply (call_expr, args) ->
      let args = List.map (fun (lbl, arg) ->
        (lbl, ParsetreeViewer.rewrite_underscore_apply arg)
      ) args
      in
      let (uncurried, attrs) =
        ParsetreeViewer.process_uncurried_attribute expr.pexp_attributes
      in
      let call_expr_doc =
        let doc = print_expression_with_comments call_expr cmt_tbl in
        match Parens.call_expr call_expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc call_expr braces
        | Nothing -> doc
      in
      if ParsetreeViewer.requires_special_callback_printing_first_arg args then
        let args_doc =
          print_arguments_with_callback_in_first_position ~uncurried args cmt_tbl
        in
        Doc.concat [
          print_attributes attrs;
          call_expr_doc;
          args_doc;
        ]
      else if ParsetreeViewer.requires_special_callback_printing_last_arg args then
        let args_doc =
          print_arguments_with_callback_in_last_position ~uncurried args cmt_tbl
        in
        Doc.concat [
          print_attributes attrs;
          call_expr_doc;
          args_doc;
        ]
      else
        let args_doc = print_arguments ~uncurried args cmt_tbl in
        Doc.concat [
          print_attributes attrs;
          call_expr_doc;
          args_doc;
        ]
    | _ -> assert false

  and print_jsx_expression lident args cmt_tbl =
    let name = print_jsx_name lident in
    let (formatted_props, children) = print_jsx_props args cmt_tbl in
    (* <div className="test" /> *)
    let is_self_closing = match children with | [] -> true | _ -> false in
    Doc.group (
      Doc.concat [
        Doc.group (
          Doc.concat [
            print_comments (Doc.concat [Doc.less_than; name]) cmt_tbl lident.Asttypes.loc;
            formatted_props;
            if is_self_closing then Doc.concat [Doc.line; Doc.text "/>"] else Doc.nil
          ]
        );
        if is_self_closing then Doc.nil
        else
          Doc.concat [
            Doc.greater_than;
            Doc.indent (
              Doc.concat [
                Doc.line;
                print_jsx_children children cmt_tbl;
              ]
            );
            Doc.line;
            Doc.text "</";
            name;
            Doc.greater_than;
          ]
      ]
    )

  and print_jsx_fragment expr cmt_tbl =
    let opening = Doc.text "<>" in
    let closing = Doc.text "</>" in
    let (children, _) = ParsetreeViewer.collect_list_expressions expr in
    Doc.group (
      Doc.concat [
        opening;
        begin match children with
        | [] -> Doc.nil
        | children ->
          Doc.indent (
            Doc.concat [
              Doc.line;
              print_jsx_children children cmt_tbl;
            ]
          )
        end;
        Doc.line;
        closing;
      ]
    )

  and print_jsx_children (children: Parsetree.expression list) cmt_tbl =
    Doc.group (
      Doc.join ~sep:Doc.line (
        List.map (fun expr ->
          let expr_doc = print_expression_with_comments expr cmt_tbl in
          match Parens.jsx_child_expr expr with
          | Parenthesized | Braced _ ->
            (* {(20: int)} make sure that we also protect the expression inside *)
            add_braces (if Parens.braced_expr expr then add_parens expr_doc else expr_doc)
          | Nothing -> expr_doc
        ) children
      )
    )

  and print_jsx_props args cmt_tbl =
    let rec loop props args =
      match args with
      | [] -> (Doc.nil, [])
      | [
          (Asttypes.Labelled "children", children);
          (
            Asttypes.Nolabel,
            {Parsetree.pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, None)}
          )
        ] ->
        let formatted_props = Doc.indent (
          match props with
          | [] -> Doc.nil
          | props ->
            Doc.concat [
              Doc.line;
              Doc.group (
                Doc.join ~sep:Doc.line (props |> List.rev)
              )
            ]
        ) in
        let (children, _) = ParsetreeViewer.collect_list_expressions children in
        (formatted_props, children)
      | arg::args ->
        let prop_doc = print_jsx_prop arg cmt_tbl in
        loop (prop_doc::props) args
    in
    loop [] args

  and print_jsx_prop arg cmt_tbl =
    match arg with
    | (
        (Asttypes.Labelled lbl_txt | Optional lbl_txt) as lbl,
        {
          Parsetree.pexp_attributes = [({Location.txt = "res.namedArgLoc"; loc = arg_loc}, _)];
          pexp_desc = Pexp_ident {txt = Longident.Lident ident}
        }
      ) when lbl_txt = ident (* jsx punning *) ->
      begin match lbl with
      | Nolabel -> Doc.nil
      | Labelled _lbl ->
        print_comments (print_ident_like ident) cmt_tbl arg_loc
      | Optional _lbl ->
        let doc = Doc.concat [
          Doc.question;
          print_ident_like ident;
        ] in
        print_comments doc cmt_tbl arg_loc
      end
    | (
        (Asttypes.Labelled lbl_txt | Optional lbl_txt) as lbl,
        {
          Parsetree.pexp_attributes = [];
          pexp_desc = Pexp_ident {txt = Longident.Lident ident}
        }
      ) when lbl_txt = ident (* jsx punning when printing from Reason *) ->
      begin match lbl with
      | Nolabel -> Doc.nil
      | Labelled _lbl -> print_ident_like ident
      | Optional _lbl -> Doc.concat [
          Doc.question;
          print_ident_like ident;
        ]
      end
    | (lbl, expr) ->
      let (arg_loc, expr) = match expr.pexp_attributes with
      | ({Location.txt = "res.namedArgLoc"; loc}, _)::attrs ->
          (loc, {expr with pexp_attributes = attrs})
      | _ ->
        Location.none, expr
      in
      let lbl_doc = match lbl with
      | Asttypes.Labelled lbl ->
        let lbl = print_comments (print_ident_like lbl) cmt_tbl arg_loc in
        Doc.concat [lbl; Doc.equal]
      | Asttypes.Optional lbl ->
        let lbl = print_comments (print_ident_like lbl) cmt_tbl arg_loc in
        Doc.concat [lbl; Doc.equal; Doc.question]
      | Nolabel -> Doc.nil
      in
      let expr_doc =
        let doc = print_expression expr cmt_tbl in
        match Parens.jsx_prop_expr expr with
        | Parenthesized | Braced(_) ->
          (* {(20: int)} make sure that we also protect the expression inside *)
          add_braces (if Parens.braced_expr expr then add_parens doc else doc)
        | _ -> doc
      in
      let full_loc = {arg_loc with loc_end = expr.pexp_loc.loc_end} in
      print_comments
        (Doc.concat [
          lbl_doc;
          expr_doc;
        ])
        cmt_tbl
        full_loc

  (* div -> div.
   * Navabar.createElement -> Navbar
   * Staff.Users.createElement -> Staff.Users *)
  and print_jsx_name {txt = lident} =
    let rec flatten acc lident = match lident with
    | Longident.Lident txt -> txt::acc
    | Ldot (lident, txt) ->
      let acc = if txt = "createElement" then acc else txt::acc in
      flatten acc lident
    | _ -> acc
    in
    match lident with
    | Longident.Lident txt -> Doc.text txt
    | _ as lident ->
      let segments = flatten [] lident in
      Doc.join ~sep:Doc.dot (List.map Doc.text segments)

  and print_arguments_with_callback_in_first_position ~uncurried args cmt_tbl =
    let (callback, printed_args) = match args with
    | (lbl, expr)::args ->
      let lbl_doc = match lbl with
      | Asttypes.Nolabel -> Doc.nil
      | Asttypes.Labelled txt ->
        Doc.concat [
          Doc.tilde; print_ident_like txt; Doc.equal;
        ]
      | Asttypes.Optional txt ->
        Doc.concat [
          Doc.tilde; print_ident_like txt; Doc.equal; Doc.question;
        ]
      in
      let callback = Doc.concat [
        lbl_doc;
        print_pexp_fun ~in_callback:true expr cmt_tbl
      ] in
      let printed_args = List.map (fun arg ->
        print_argument arg cmt_tbl
      ) args |> Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line])
      in
      (callback, printed_args)
    | _ -> assert false
    in
    (* Thing.map((arg1, arg2) => MyModuleBlah.toList(argument), foo) *)
    (* Thing.map((arg1, arg2) => {
     *   MyModuleBlah.toList(argument)
     * }, longArgumet, veryLooooongArgument)
     *)
    let fits_on_one_line = Doc.concat [
      if uncurried then Doc.text "(. " else Doc.lparen;
      callback;
      Doc.comma;
      Doc.line;
      printed_args;
      Doc.rparen;
    ] in

    (* Thing.map(
     *   (param1, parm2) => doStuff(param1, parm2),
     *   arg1,
     *   arg2,
     *   arg3,
     * )
     *)
    let break_all_args = print_arguments ~uncurried args cmt_tbl in
    Doc.custom_layout [
      fits_on_one_line;
      break_all_args;
    ]

  and print_arguments_with_callback_in_last_position ~uncurried args cmt_tbl =
    let rec loop acc args = match args with
    | [] -> (Doc.nil, Doc.nil)
    | [lbl, expr] ->
      let lbl_doc = match lbl with
      | Asttypes.Nolabel -> Doc.nil
      | Asttypes.Labelled txt ->
        Doc.concat [
          Doc.tilde; print_ident_like txt; Doc.equal;
        ]
      | Asttypes.Optional txt ->
        Doc.concat [
          Doc.tilde; print_ident_like txt; Doc.equal; Doc.question;
        ]
      in
      let callback = print_pexp_fun ~in_callback:true expr cmt_tbl in
      (Doc.concat (List.rev acc), Doc.concat [lbl_doc; callback])
    | arg::args ->
      let arg_doc = print_argument arg cmt_tbl in
      loop (Doc.line::Doc.comma::arg_doc::acc) args
    in
    let (printed_args, callback) = loop [] args in

    (* Thing.map(foo, (arg1, arg2) => MyModuleBlah.toList(argument)) *)
    let fits_on_one_line = Doc.concat [
      if uncurried then Doc.text "(." else Doc.lparen;
      printed_args;
      callback;
      Doc.rparen;
    ] in

    (* Thing.map(longArgumet, veryLooooongArgument, (arg1, arg2) =>
     *   MyModuleBlah.toList(argument)
     * )
     *)
    let arugments_fit_on_one_line =
      Doc.concat [
        if uncurried then Doc.text "(." else Doc.lparen;
        Doc.soft_line;
        printed_args;
        Doc.breakable_group ~force_break:true callback;
        Doc.soft_line;
        Doc.rparen;
      ]
    in

    (* Thing.map(
     *   arg1,
     *   arg2,
     *   arg3,
     *   (param1, parm2) => doStuff(param1, parm2)
     * )
     *)
    let break_all_args = print_arguments ~uncurried args cmt_tbl in
    Doc.custom_layout [
      fits_on_one_line;
      arugments_fit_on_one_line;
      break_all_args;
    ]

	and print_arguments ~uncurried (args : (Asttypes.arg_label * Parsetree.expression) list) cmt_tbl =
		match args with
		| [Nolabel, {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}] ->
      if uncurried then Doc.text "(.)" else Doc.text "()"
    | [(Nolabel, arg)] when ParsetreeViewer.is_huggable_expression arg ->
      let arg_doc =
        let doc = print_expression_with_comments arg cmt_tbl in
        match Parens.expr arg with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc arg braces
        | Nothing -> doc
      in
      Doc.concat [
        if uncurried then Doc.text "(." else Doc.lparen;
        arg_doc;
        Doc.rparen;
      ]
		| args -> Doc.group (
				Doc.concat [
          if uncurried then Doc.text "(." else Doc.lparen;
					Doc.indent (
						Doc.concat [
              if uncurried then Doc.line else Doc.soft_line;
							Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
								List.map (fun arg -> print_argument arg cmt_tbl) args
							)
						]
					);
					Doc.trailing_comma;
					Doc.soft_line;
					Doc.rparen;
				]
			)

(*
   * argument ::=
   *   | _                            (* syntax sugar *)
   *   | expr
   *   | expr : type
   *   | ~ label-name
   *   | ~ label-name
   *   | ~ label-name ?
   *   | ~ label-name =   expr
   *   | ~ label-name =   _           (* syntax sugar *)
   *   | ~ label-name =   expr : type
   *   | ~ label-name = ? expr
   *   | ~ label-name = ? _           (* syntax sugar *)
   *   | ~ label-name = ? expr : type *)
	and print_argument (arg_lbl, arg) cmt_tbl =
		match (arg_lbl, arg) with
		(* ~a (punned)*)
		| (
				(Asttypes.Labelled lbl),
        ({pexp_desc=Pexp_ident {txt = Longident.Lident name};
          pexp_attributes = ([] | [({Location.txt = "res.namedArgLoc";}, _)])
         } as arg_expr)
			) when lbl = name && not (ParsetreeViewer.is_braced_expr arg_expr) ->
      let loc = match arg.pexp_attributes with
      | ({Location.txt = "res.namedArgLoc"; loc}, _)::_ -> loc
      | _ -> arg.pexp_loc
      in
      let doc = Doc.concat [
        Doc.tilde;
        print_ident_like lbl
      ] in
      print_comments doc cmt_tbl loc

		(* ~a: int (punned)*)
		| (
				(Asttypes.Labelled lbl),
        {pexp_desc = Pexp_constraint (
            {pexp_desc = Pexp_ident {txt = Longident.Lident name}} as arg_expr,
            typ
         );
         pexp_loc;
         pexp_attributes = ([] | [({Location.txt = "res.namedArgLoc";}, _)]) as attrs
        }
			) when lbl = name && not (ParsetreeViewer.is_braced_expr arg_expr) ->
      let loc = match attrs with
      | ({Location.txt = "res.namedArgLoc"; loc}, _)::_ ->
        {loc with loc_end = pexp_loc.loc_end}
      | _ -> arg.pexp_loc
      in
      let doc = Doc.concat [
        Doc.tilde;
        print_ident_like lbl;
        Doc.text ": ";
        print_typ_expr typ cmt_tbl;
      ] in
      print_comments doc cmt_tbl loc
		(* ~a? (optional lbl punned)*)
		| (
				(Asttypes.Optional lbl),
        {pexp_desc=Pexp_ident {txt = Longident.Lident name};
         pexp_attributes = ([] | [({Location.txt = "res.namedArgLoc";}, _)])
        }
			) when lbl = name ->
      let loc = match arg.pexp_attributes with
      | ({Location.txt = "res.namedArgLoc"; loc}, _)::_ -> loc
      | _ -> arg.pexp_loc
      in
      let doc = Doc.concat [
        Doc.tilde;
        print_ident_like lbl;
        Doc.question;
      ] in
      print_comments doc cmt_tbl loc
		| (_lbl, expr) ->
      let (arg_loc, expr) = match expr.pexp_attributes with
      | ({Location.txt = "res.namedArgLoc"; loc}, _)::attrs ->
          (loc, {expr with pexp_attributes = attrs})
      | _ ->
        expr.pexp_loc, expr
      in
			let printed_lbl = match arg_lbl with
			| Asttypes.Nolabel -> Doc.nil
			| Asttypes.Labelled lbl ->
        let doc = Doc.concat [Doc.tilde; print_ident_like lbl; Doc.equal] in
        print_comments doc cmt_tbl arg_loc
			| Asttypes.Optional lbl ->
        let doc = Doc.concat [Doc.tilde; print_ident_like lbl; Doc.equal; Doc.question] in
        print_comments doc cmt_tbl arg_loc
			in
			let printed_expr =
        let doc = print_expression_with_comments expr cmt_tbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc expr braces
        | Nothing -> doc
      in
      let loc = {arg_loc with loc_end = expr.pexp_loc.loc_end} in
      let doc = Doc.concat [
        printed_lbl;
        printed_expr;
      ] in
      print_comments doc cmt_tbl loc

  and print_cases (cases: Parsetree.case list) cmt_tbl =
    Doc.breakable_group ~force_break:true (
      Doc.concat [
        Doc.lbrace;
          Doc.concat [
            Doc.line;
            print_list
              ~get_loc:(fun n -> {n.Parsetree.pc_lhs.ppat_loc with
                loc_end =
                  match ParsetreeViewer.process_braces_attr n.Parsetree.pc_rhs with
                  | (None, _) -> n.pc_rhs.pexp_loc.loc_end
                  | (Some ({loc}, _), _) -> loc.Location.loc_end
              })
              ~print:print_case
              ~nodes:cases
              cmt_tbl
          ];
        Doc.line;
        Doc.rbrace;
      ]
    )

  and print_case (case: Parsetree.case) cmt_tbl =
    let rhs = match case.pc_rhs.pexp_desc with
    | Pexp_let _
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_open _
    | Pexp_sequence _ ->
      print_expression_block ~braces:(ParsetreeViewer.is_braced_expr case.pc_rhs) case.pc_rhs cmt_tbl
    | _ ->
      let doc = print_expression_with_comments case.pc_rhs cmt_tbl in
      begin match Parens.expr case.pc_rhs with
      | Parenthesized -> add_parens doc
      | _ -> doc
      end

    in
    let guard = match case.pc_guard with
    | None -> Doc.nil
    | Some expr -> Doc.group (
        Doc.concat [
          Doc.line;
          Doc.text "when ";
          print_expression_with_comments expr cmt_tbl;
        ]
      )
    in
    let should_inline_rhs = match case.pc_rhs.pexp_desc with
    | Pexp_construct ({txt = Longident.Lident ("()" | "true" | "false")}, _)
    | Pexp_constant _
    | Pexp_ident _ -> true
    | _ when ParsetreeViewer.is_huggable_rhs case.pc_rhs -> true
    | _ -> false
    in
    let should_indent_pattern = match case.pc_lhs.ppat_desc with
    | Ppat_or _ -> false
    | _ -> true
    in
    let pattern_doc =
      let doc = print_pattern case.pc_lhs cmt_tbl in
      match case.pc_lhs.ppat_desc with
      | Ppat_constraint _ -> add_parens doc
      | _ -> doc
    in
    let content = Doc.concat [
      if should_indent_pattern then Doc.indent pattern_doc else pattern_doc;
      Doc.indent guard;
      Doc.text " =>";
      Doc.indent (
        Doc.concat [
          if should_inline_rhs then Doc.space else Doc.line;
          rhs;
        ]
      )
    ] in
    Doc.group (
      Doc.concat [
        Doc.text "| ";
        content;
      ]
    )

  and print_expr_fun_parameters ~in_callback ~uncurried ~has_constraint parameters cmt_tbl =
    match parameters with
    (* let f = _ => () *)
    | [ParsetreeViewer.Parameter {
      attrs = [];
      lbl = Asttypes.Nolabel;
      default_expr = None;
      pat = {Parsetree.ppat_desc = Ppat_any}
      }] when not uncurried ->
      if has_constraint then Doc.text "(_)" else Doc.text "_"
    (* let f = a => () *)
    | [ParsetreeViewer.Parameter {
      attrs = [];
      lbl = Asttypes.Nolabel;
      default_expr = None;
      pat = {Parsetree.ppat_desc = Ppat_var string_loc}
    }] when not uncurried ->
      let txt_doc =
        let var = print_ident_like string_loc.txt in
        if has_constraint then add_parens var else var
      in
      print_comments txt_doc cmt_tbl string_loc.loc
    (* let f = () => () *)
    | [ParsetreeViewer.Parameter {
        attrs = [];
        lbl = Asttypes.Nolabel;
        default_expr = None;
        pat = {ppat_desc = Ppat_construct({txt = Longident.Lident "()"}, None)}
    }] when not uncurried ->
      Doc.text "()"
    (* let f = (~greeting, ~from as hometown, ~x=?) => () *)
    | parameters ->
      let lparen = if uncurried then Doc.text "(. " else Doc.lparen in
      let should_hug = ParsetreeViewer.parameters_should_hug parameters in
      let printed_paramaters = Doc.concat [
        if should_hug || in_callback then Doc.nil else Doc.soft_line;
        Doc.join ~sep:(Doc.concat [Doc.comma; if in_callback then Doc.space else Doc.line])
          (List.map (fun p -> print_exp_fun_parameter p cmt_tbl) parameters)
      ] in
      Doc.group (
        Doc.concat [
          lparen;
          if should_hug || in_callback then
            printed_paramaters
          else Doc.indent printed_paramaters;
          if should_hug || in_callback then
            Doc.nil
          else
            Doc.concat [Doc.trailing_comma; Doc.soft_line];
          Doc.rparen;
        ]
      )

  and print_exp_fun_parameter parameter cmt_tbl =
    match parameter with
    | ParsetreeViewer.NewTypes {attrs; locs = lbls} ->
      Doc.group (
        Doc.concat [
          print_attributes attrs;
          Doc.text "type ";
          Doc.join ~sep:Doc.space (List.map (fun lbl ->
            print_comments (print_ident_like lbl.Asttypes.txt) cmt_tbl lbl.Asttypes.loc
          ) lbls)
        ]
      )
    | Parameter {attrs; lbl; default_expr; pat = pattern} ->
      let (is_uncurried, attrs) = ParsetreeViewer.process_uncurried_attribute attrs in
      let uncurried = if is_uncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil in
      let attrs = print_attributes attrs in
      (* =defaultValue *)
      let default_expr_doc = match default_expr with
      | Some expr -> Doc.concat [
          Doc.text "=";
          print_expression_with_comments expr cmt_tbl
        ]
      | None -> Doc.nil
      in
      (* ~from as hometown
       * ~from                   ->  punning *)
      let label_with_pattern = match (lbl, pattern) with
      | (Asttypes.Nolabel, pattern) -> print_pattern pattern cmt_tbl
      | (
          (Asttypes.Labelled lbl | Optional lbl),
          {ppat_desc = Ppat_var string_loc;
           ppat_attributes = ([] | [({Location.txt = "res.namedArgLoc";}, _)])
          }
        ) when lbl = string_loc.txt ->
          (* ~d *)
          Doc.concat [
            Doc.text "~";
            print_ident_like lbl;
          ]
      | (
          (Asttypes.Labelled lbl | Optional lbl),
           ({ppat_desc = Ppat_constraint ({ ppat_desc = Ppat_var { txt } }, typ);
             ppat_attributes = ([] | [({Location.txt = "res.namedArgLoc";}, _)])
            })
        ) when lbl = txt ->
          (* ~d: e *)
          Doc.concat [
            Doc.text "~";
            print_ident_like lbl;
            Doc.text ": ";
            print_typ_expr typ cmt_tbl;
          ]
      | ((Asttypes.Labelled lbl | Optional lbl), pattern) ->
          (* ~b as c *)
        Doc.concat [
          Doc.text "~";
          print_ident_like lbl;
          Doc.text " as ";
          print_pattern pattern cmt_tbl
        ]
      in
      let optional_label_suffix = match (lbl, default_expr) with
      | (Asttypes.Optional _, None) -> Doc.text "=?"
      | _ -> Doc.nil
      in
      let doc = Doc.group (
        Doc.concat [
          uncurried;
          attrs;
          label_with_pattern;
          default_expr_doc;
          optional_label_suffix;
        ]
      ) in
      let cmt_loc = match default_expr with
      | None ->
        begin match pattern.ppat_attributes with
        | ({Location.txt = "res.namedArgLoc"; loc}, _)::_ ->
          {loc with loc_end = pattern.ppat_loc.loc_end}
        | _ -> pattern.ppat_loc
        end
      | Some expr ->
        let start_pos =  match pattern.ppat_attributes with
        | ({Location.txt = "res.namedArgLoc"; loc}, _)::_ ->
            loc.loc_start
        | _ -> pattern.ppat_loc.loc_start
        in {
          pattern.ppat_loc with
          loc_start = start_pos;
          loc_end = expr.pexp_loc.loc_end
        }
      in
      print_comments doc cmt_tbl cmt_loc

  and print_expression_block ~braces expr cmt_tbl =
    let rec collect_rows acc expr = match expr.Parsetree.pexp_desc with
    | Parsetree.Pexp_letmodule (mod_name, mod_expr, expr2) ->
      let name =
        let doc = Doc.text mod_name.txt in
        print_comments doc cmt_tbl mod_name.loc
      in
      let let_module_doc = Doc.concat [
        Doc.text "module ";
        name;
        Doc.text " = ";
        print_mod_expr mod_expr cmt_tbl;
      ] in
      let loc = {expr.pexp_loc with loc_end = mod_expr.pmod_loc.loc_end} in
      collect_rows ((loc, let_module_doc)::acc) expr2
    | Pexp_letexception (extension_constructor, expr2) ->
      let loc =
        let loc = {expr.pexp_loc with loc_end = extension_constructor.pext_loc.loc_end} in
        match get_first_leading_comment cmt_tbl loc with
        | None -> loc
        | Some comment ->
          let cmt_loc = Comment.loc comment in
          {cmt_loc with loc_end = loc.loc_end}
      in
      let let_exception_doc = print_exception_def extension_constructor cmt_tbl in
      collect_rows ((loc, let_exception_doc)::acc) expr2
    | Pexp_open (override_flag, longident_loc, expr2) ->
      let open_doc = Doc.concat [
        Doc.text "open";
        print_override_flag override_flag;
        Doc.space;
        print_longident_location longident_loc cmt_tbl;
      ] in
      let loc = {expr.pexp_loc with loc_end = longident_loc.loc.loc_end} in
      collect_rows ((loc, open_doc)::acc) expr2
    | Pexp_sequence (expr1, expr2) ->
      let expr_doc =
        let doc = print_expression expr1 cmt_tbl in
        match Parens.expr expr1 with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc expr1 braces
        | Nothing -> doc
      in
      let loc = expr1.pexp_loc in
      collect_rows ((loc, expr_doc)::acc) expr2
    | Pexp_let (rec_flag, value_bindings, expr2) ->
      let loc =
        let loc = match (value_bindings, List.rev value_bindings) with
        | (vb::_, last_vb::_) -> {vb.pvb_loc with loc_end = last_vb.pvb_loc.loc_end}
        | _ -> Location.none
        in
        match get_first_leading_comment cmt_tbl loc with
        | None -> loc
        | Some comment ->
          let cmt_loc = Comment.loc comment in
          {cmt_loc with loc_end = loc.loc_end}
      in
			let rec_flag = match rec_flag with
			| Asttypes.Nonrecursive -> Doc.nil
			| Asttypes.Recursive -> Doc.text "rec "
			in
      let let_doc = print_value_bindings ~rec_flag value_bindings cmt_tbl in
      (* let () = {
       *   let () = foo()
       *   ()
       * }
       * We don't need to print the () on the last line of the block
       *)
      begin match expr2.pexp_desc with
      | Pexp_construct ({txt = Longident.Lident "()"}, _) ->
        List.rev ((loc, let_doc)::acc)
      | _ ->
        collect_rows ((loc, let_doc)::acc) expr2
      end
    | _ ->
      let expr_doc =
        let doc = print_expression expr cmt_tbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces  -> print_braces doc expr braces
        | Nothing -> doc
      in
      List.rev ((expr.pexp_loc, expr_doc)::acc)
    in
    let rows = collect_rows [] expr in
    let block =
      print_list
        ~get_loc:fst
        ~nodes:rows
        ~print:(fun (_, doc) _ -> doc)
        ~force_break:true
        cmt_tbl
    in
    Doc.breakable_group ~force_break:true (
      if braces then
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.line;
              block;
            ]
          );
          Doc.line;
          Doc.rbrace;
        ]
      else block
    )

  (*
   * // user types:
   * let f = (a, b) => { a + b }
   *
   * // printer: everything is on one line
   * let f = (a, b) => { a + b }
   *
   * // user types: over multiple lines
   * let f = (a, b) => {
   *   a + b
   * }
   *
   * // printer: over multiple lines
   * let f = (a, b) => {
   *   a + b
   * }
   *)
  and print_braces doc expr braces_loc =
    let over_multiple_lines =
      let open Location in
      braces_loc.loc_end.pos_lnum > braces_loc.loc_start.pos_lnum
    in
    match expr.Parsetree.pexp_desc with
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_let _
    | Pexp_open _
    | Pexp_sequence _ ->
      (* already has braces *)
      doc
    | _ ->
      Doc.breakable_group ~force_break:over_multiple_lines (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              if Parens.braced_expr expr then add_parens doc else doc;
            ]
          );
          Doc.soft_line;
          Doc.rbrace;
        ]
      )

  and print_override_flag override_flag = match override_flag with
    | Asttypes.Override -> Doc.text "!"
    | Fresh -> Doc.nil

  and print_direction_flag flag = match flag with
    | Asttypes.Downto -> Doc.text " downto "
    | Asttypes.Upto -> Doc.text " to "

  and print_record_row (lbl, expr) cmt_tbl =
    let cmt_loc = {lbl.loc with loc_end = expr.pexp_loc.loc_end} in
    let doc = Doc.group (Doc.concat [
      print_lident_path lbl cmt_tbl;
      Doc.text ": ";
      (let doc = print_expression_with_comments expr cmt_tbl in
      match Parens.expr expr with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc expr braces
      | Nothing -> doc);
    ]) in
    print_comments doc cmt_tbl cmt_loc

  and print_bs_object_row (lbl, expr) cmt_tbl =
    let cmt_loc = {lbl.loc with loc_end = expr.pexp_loc.loc_end} in
    let lbl_doc =
      let doc = Doc.concat [
        Doc.text "\"";
        print_longident lbl.txt;
        Doc.text "\"";
      ] in
      print_comments doc cmt_tbl lbl.loc
    in
    let doc = Doc.concat [
      lbl_doc;
      Doc.text ": ";
      print_expression_with_comments expr cmt_tbl
    ] in
    print_comments doc cmt_tbl cmt_loc

  (* The optional loc indicates whether we need to print the attributes in
   * relation to some location. In practise this means the following:
   *  `@attr type t = string` -> on the same line, print on the same line
   *  `@attr
   *   type t = string` -> attr is on prev line, print the attributes
   *   with a line break between, we respect the users' original layout *)
  and print_attributes ?loc (attrs: Parsetree.attributes) =
    match ParsetreeViewer.filter_parsing_attrs attrs with
    | [] -> Doc.nil
    | attrs ->
      let line_break = match loc with
      | None -> Doc.line
      | Some loc -> begin match List.rev attrs with
        | ({loc = first_loc}, _)::_ when loc.loc_start.pos_lnum > first_loc.loc_end.pos_lnum ->
          Doc.hard_line;
        | _ -> Doc.line
        end
      in
      Doc.concat [
        Doc.group (Doc.join ~sep:Doc.line (List.map print_attribute attrs));
        line_break;
      ]

  and print_attribute ((id, payload) : Parsetree.attribute) =
      let attr_name = Doc.concat [
        Doc.text "@";
        Doc.text id.txt
      ] in
        match payload with
      | PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
        let expr_doc = print_expression expr CommentTable.empty in
        let needs_parens = match attrs with | [] -> false | _ -> true in
        Doc.group (
          Doc.concat [
            attr_name;
            add_parens (
              Doc.concat [
                print_attributes attrs;
                if needs_parens then add_parens expr_doc else expr_doc;
              ]
            )
          ]
        )
      | PTyp typ ->
        Doc.group (
          Doc.concat [
            attr_name;
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.text ": ";
                print_typ_expr typ CommentTable.empty;
              ]
            );
            Doc.soft_line;
            Doc.rparen;
          ]
        )
      | _ -> attr_name

  and print_attribute_with_comments ((id, payload) : Parsetree.attribute) cmt_tbl =
      let attr_name = Doc.text ("@" ^ id.txt) in
      match payload with
      | PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
        let expr_doc = print_expression_with_comments expr cmt_tbl in
        let needs_parens = match attrs with | [] -> false | _ -> true in
        Doc.group (
          Doc.concat [
            attr_name;
            add_parens (
              Doc.concat [
                print_attributes attrs;
                if needs_parens then add_parens expr_doc else expr_doc;
              ]
            )
          ]
        )
      | _ -> attr_name

  and print_mod_expr mod_expr cmt_tbl =
    let doc = match mod_expr.pmod_desc with
    | Pmod_ident longident_loc ->
      print_longident_location longident_loc cmt_tbl
    | Pmod_structure structure ->
      Doc.breakable_group ~force_break:true (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              print_structure structure cmt_tbl;
            ];
          );
          Doc.soft_line;
          Doc.rbrace;
        ]
      )
    | Pmod_unpack expr ->
      let should_hug = match expr.pexp_desc with
      | Pexp_let _ -> true
      | Pexp_constraint (
          {pexp_desc = Pexp_let _ },
          {ptyp_desc = Ptyp_package _packageType}
        ) -> true
      | _ -> false
      in
      let (expr, module_constraint) = match expr.pexp_desc with
      | Pexp_constraint (
          expr,
          {ptyp_desc = Ptyp_package package_type; ptyp_loc}
      ) ->
        let package_doc =
          let doc = print_package_type ~print_module_keyword_and_parens:false package_type cmt_tbl in
          print_comments doc cmt_tbl ptyp_loc
        in
        let type_doc = Doc.group (Doc.concat [
          Doc.text ":";
          Doc.indent (
            Doc.concat [
              Doc.line;
              package_doc
            ]
          )
        ]) in
        (expr, type_doc)
      | _ -> (expr, Doc.nil)
      in
      let unpack_doc = Doc.group(Doc.concat [
        print_expression_with_comments expr cmt_tbl;
        module_constraint;
      ]) in
      Doc.group (
        Doc.concat [
          Doc.text "unpack(";
          if should_hug then unpack_doc
          else
            Doc.concat [
              Doc.indent (
                Doc.concat [
                  Doc.soft_line;
                  unpack_doc;
                ]
              );
             Doc.soft_line;
            ];
          Doc.rparen;
        ]
      )
    | Pmod_extension extension ->
      print_extension_with_comments ~at_module_lvl:false extension cmt_tbl
    | Pmod_apply _ ->
      let (args, call_expr) = ParsetreeViewer.mod_expr_apply mod_expr in
      let is_unit_sugar = match args with
      | [{pmod_desc = Pmod_structure []}] -> true
      | _ -> false
      in
      let should_hug = match args with
      | [{pmod_desc = Pmod_structure _}] -> true
      | _ -> false
      in
      Doc.group (
        Doc.concat [
          print_mod_expr call_expr cmt_tbl;
          if is_unit_sugar then
            print_mod_apply_arg (List.hd args [@doesNotRaise]) cmt_tbl
          else
            Doc.concat [
              Doc.lparen;
              if should_hug then
                print_mod_apply_arg (List.hd args [@doesNotRaise]) cmt_tbl
              else
                Doc.indent (
                  Doc.concat [
                    Doc.soft_line;
                    Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                      List.map (fun mod_arg -> print_mod_apply_arg mod_arg cmt_tbl) args
                    )
                  ]
                );
              if not should_hug then
                Doc.concat [
                  Doc.trailing_comma;
                  Doc.soft_line;
                ]
              else Doc.nil;
              Doc.rparen;
            ]
        ]
      )
    | Pmod_constraint (mod_expr, mod_type) ->
      Doc.concat [
        print_mod_expr mod_expr cmt_tbl;
        Doc.text ": ";
        print_mod_type mod_type cmt_tbl;
      ]
    | Pmod_functor _ ->
      print_mod_functor mod_expr cmt_tbl
    in
    print_comments doc cmt_tbl mod_expr.pmod_loc

  and print_mod_functor mod_expr cmt_tbl =
    let (parameters, return_mod_expr) = ParsetreeViewer.mod_expr_functor mod_expr in
    (* let shouldInline = match returnModExpr.pmod_desc with *)
    (* | Pmod_structure _ | Pmod_ident _ -> true *)
    (* | Pmod_constraint ({pmod_desc = Pmod_structure _}, _) -> true *)
    (* | _ -> false *)
    (* in *)
    let (return_constraint, return_mod_expr) = match return_mod_expr.pmod_desc with
    | Pmod_constraint (mod_expr, mod_type) ->
      let constraint_doc =
        let doc = print_mod_type mod_type cmt_tbl in
        if Parens.mod_expr_functor_constraint mod_type then add_parens doc else doc
      in
      let mod_constraint = Doc.concat [
        Doc.text ": ";
        constraint_doc;
      ] in
      (mod_constraint, print_mod_expr mod_expr cmt_tbl)
    | _ -> (Doc.nil, print_mod_expr return_mod_expr cmt_tbl)
    in
    let parameters_doc = match parameters with
    | [(attrs, {txt = "*"}, None)] ->
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map print_attribute attrs);
          Doc.line;
        ] in
        Doc.group (Doc.concat [
          attrs;
          Doc.text "()"
        ])
    | [([], {txt = lbl}, None)] -> Doc.text lbl
    | parameters ->
      Doc.group (
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun param -> print_mod_functor_param param cmt_tbl) parameters
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ]
      )
    in
    Doc.group (
      Doc.concat [
        parameters_doc;
        return_constraint;
        Doc.text " => ";
        return_mod_expr
      ]
    )

  and print_mod_functor_param (attrs, lbl, opt_mod_type) cmt_tbl =
    let cmt_loc = match opt_mod_type with
    | None -> lbl.Asttypes.loc
    | Some mod_type -> {lbl.loc with loc_end =
        mod_type.Parsetree.pmty_loc.loc_end
      }
    in
    let attrs = match attrs with
    | [] -> Doc.nil
    | attrs -> Doc.concat [
      Doc.join ~sep:Doc.line (List.map print_attribute attrs);
      Doc.line;
    ] in
    let lbl_doc =
      let doc = Doc.text lbl.txt in
      print_comments doc cmt_tbl lbl.loc
    in
    let doc = Doc.group (
      Doc.concat [
        attrs;
        lbl_doc;
        (match opt_mod_type with
        | None -> Doc.nil
        | Some mod_type ->
          Doc.concat [
            Doc.text ": ";
            print_mod_type mod_type cmt_tbl
          ]);
      ]
    ) in
    print_comments doc cmt_tbl cmt_loc

  and print_mod_apply_arg mod_expr cmt_tbl =
    match mod_expr.pmod_desc with
    | Pmod_structure [] -> Doc.text "()"
    | _ -> print_mod_expr mod_expr cmt_tbl


  and print_exception_def (constr : Parsetree.extension_constructor) cmt_tbl =
    let kind = match constr.pext_kind with
    | Pext_rebind longident -> Doc.indent (
        Doc.concat [
          Doc.text " =";
          Doc.line;
          print_longident_location longident cmt_tbl;
        ]
     )
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadt_doc = match gadt with
      | Some typ -> Doc.concat [
          Doc.text ": ";
          print_typ_expr typ cmt_tbl
        ]
      | None -> Doc.nil
      in
      Doc.concat [
        print_constructor_arguments ~indent:false args cmt_tbl;
        gadt_doc
      ]
    in
    let name =
      print_comments
        (Doc.text constr.pext_name.txt)
        cmt_tbl
        constr.pext_name.loc
    in
    let doc = Doc.group (
      Doc.concat [
        print_attributes constr.pext_attributes;
        Doc.text "exception ";
        name;
        kind
      ]
    ) in
    print_comments doc cmt_tbl constr.pext_loc

  and print_extension_constructor (constr : Parsetree.extension_constructor) cmt_tbl i =
    let attrs = print_attributes constr.pext_attributes in
    let bar = if i > 0 then Doc.text "| "
      else Doc.if_breaks (Doc.text "| ") Doc.nil
    in
    let kind = match constr.pext_kind with
    | Pext_rebind longident -> Doc.indent (
        Doc.concat [
          Doc.text " =";
          Doc.line;
          print_longident_location longident cmt_tbl;
        ]
     )
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadt_doc = match gadt with
      | Some typ -> Doc.concat [
          Doc.text ": ";
          print_typ_expr typ cmt_tbl;
        ]
      | None -> Doc.nil
      in
      Doc.concat [
        print_constructor_arguments ~indent:false args cmt_tbl;
        gadt_doc
      ]
    in
    let name =
      print_comments (Doc.text constr.pext_name.txt) cmt_tbl constr.pext_name.loc
    in
    Doc.concat [
      bar;
      Doc.group (
        Doc.concat [
          attrs;
          name;
          kind;
        ]
      )
    ]

  let print_implementation ~width (s: Parsetree.structure) comments =
    let cmt_tbl = CommentTable.make () in
    CommentTable.walk_structure s cmt_tbl comments;
    (* CommentTable.log cmtTbl; *)
    let doc = print_structure s cmt_tbl in
    (* Doc.debug doc; *)
    let string_doc = Doc.to_string ~width doc in
    print_string string_doc

  let print_interface ~width (s: Parsetree.signature) comments =
    let cmt_tbl = CommentTable.make () in
    CommentTable.walk_signature s cmt_tbl comments;
    let string_doc = Doc.to_string ~width (print_signature s cmt_tbl) in
    print_string string_doc

end

module Scanner = struct
  type mode = Template | Jsx | Diamond

  type t = {
    filename: string;
    src: bytes;
    mutable err:
      start_pos: Lexing.position
      -> end_pos: Lexing.position
      -> Diagnostics.category
      -> unit;
    mutable ch: int; (* current character *)
    mutable offset: int; (* character offset *)
    mutable rd_offset: int; (* reading offset (position after current character) *)
    mutable line_offset: int; (* current line offset *)
    mutable lnum: int; (* current line number *)
    mutable mode: mode list;
  }

  let set_diamond_mode scanner =
    scanner.mode <- Diamond::scanner.mode

  let set_template_mode scanner =
    scanner.mode <- Template::scanner.mode

  let set_jsx_mode scanner =
    scanner.mode <- Jsx::scanner.mode

  let pop_mode scanner mode =
    match scanner.mode with
    | m::ms when m = mode ->
      scanner.mode <- ms
    | _ -> ()

  let in_diamond_mode scanner = match scanner.mode with
    | Diamond::_ -> true
    | _ -> false

  let in_jsx_mode scanner = match scanner.mode with
    | Jsx::_ -> true
    | _ -> false

  let in_template_mode scanner = match scanner.mode with
    | Template::_ -> true
    | _ -> false

  let position scanner = Lexing.{
    pos_fname = scanner.filename;
    (* line number *)
    pos_lnum = scanner.lnum;
    (* offset of the beginning of the line (number
       of characters between the beginning of the scanner and the beginning
       of the line) *)
    pos_bol = scanner.line_offset;
    (* [pos_cnum] is the offset of the position (number of
       characters between the beginning of the scanner and the position). *)
    pos_cnum = scanner.offset;
  }

  let next scanner =
    if scanner.rd_offset < (Bytes.length scanner.src) then (
      scanner.offset <- scanner.rd_offset;
      let ch = (Bytes.get [@doesNotRaise]) scanner.src scanner.rd_offset in
      scanner.rd_offset <- scanner.rd_offset + 1;
      scanner.ch <- int_of_char ch
    ) else (
      scanner.offset <- Bytes.length scanner.src;
      scanner.ch <- -1
    )

  let peek scanner =
    if scanner.rd_offset < (Bytes.length scanner.src) then
      int_of_char (Bytes.unsafe_get scanner.src scanner.rd_offset)
    else
      -1

  let make b filename =
    let scanner = {
      filename;
      src = b;
      err = (fun ~start_pos:_ ~end_pos:_ _ -> ());
      ch = CharacterCodes.space;
      offset = 0;
      rd_offset = 0;
      line_offset = 0;
      lnum = 1;
      mode = [];
    } in
    next scanner;
    scanner

  let skip_whitespace scanner =
    let rec scan () =
      if scanner.ch == CharacterCodes.space || scanner.ch == CharacterCodes.tab then (
        next scanner;
        scan()
      ) else if CharacterCodes.is_line_break scanner.ch then (
        scanner.line_offset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
        next scanner;
        scan()
      ) else (
        ()
      )
    in
    scan()

  let scan_identifier scanner =
    let start_off = scanner.offset in
    while (
      CharacterCodes.is_letter scanner.ch ||
      CharacterCodes.is_digit scanner.ch ||
      CharacterCodes.underscore == scanner.ch ||
      CharacterCodes.single_quote == scanner.ch
    ) do
      next scanner
    done;
    let str = Bytes.sub_string scanner.src start_off (scanner.offset - start_off) in
    Token.lookup_keyword str

  let scan_digits scanner ~base =
    if base <= 10 then (
      while CharacterCodes.is_digit scanner.ch || scanner.ch == CharacterCodes.underscore do
        next scanner
      done;
    ) else (
      while CharacterCodes.is_hex scanner.ch || scanner.ch == CharacterCodes.underscore do
        next scanner
      done;
    )

  (* float: (0…9) { 0…9∣ _ } [. { 0…9∣ _ }] [(e∣ E) [+∣ -] (0…9) { 0…9∣ _ }]   *)
  let scan_number scanner =
    let start_off = scanner.offset in

    (* integer part *)
    let base, _prefix = if scanner.ch != CharacterCodes.dot then (
      if scanner.ch == CharacterCodes._0 then (
        next scanner;
        let ch = CharacterCodes.lower scanner.ch in
        if ch == CharacterCodes.Lower.x then (
          next scanner;
          16, 'x'
        ) else if ch == CharacterCodes.Lower.o then (
          next scanner;
          8, 'o'
        ) else if ch == CharacterCodes.Lower.b then (
          next scanner;
          2, 'b'
        ) else (
          8, '0'
        )
      ) else (
        10, ' '
      )
    ) else (10, ' ')
    in
    scan_digits scanner ~base;

    (*  *)
    let is_float = if CharacterCodes.dot == scanner.ch then (
      next scanner;
      scan_digits scanner ~base;
      true
    ) else (
      false
    ) in

    (* exponent part *)
    let is_float =
      if let exp = CharacterCodes.lower scanner.ch in
        exp == CharacterCodes.Lower.e || exp == CharacterCodes.Lower.p
      then (
        next scanner;
        if scanner.ch == CharacterCodes.plus || scanner.ch == CharacterCodes.minus then
          next scanner;
        scan_digits scanner ~base;
        true
      ) else
        is_float
    in
    let literal =
      Bytes.sub_string scanner.src start_off (scanner.offset - start_off)
    in

    (* suffix *)
    let suffix =
      if scanner.ch >= CharacterCodes.Lower.g && scanner.ch <= CharacterCodes.Lower.z
         || scanner.ch >= CharacterCodes.Upper.g && scanner.ch <= CharacterCodes.Upper.z
      then (
        let ch = scanner.ch in
        next scanner;
        Some (Char.unsafe_chr ch)
      ) else
        None
    in
    if is_float then
      Token.Float {f = literal; suffix}
    else
      Token.Int {i = literal; suffix}

  let scan_exotic_identifier scanner =
    next scanner;
    let buffer = Buffer.create 20 in
    let start_pos = position scanner in

    let rec scan () =
      if scanner.ch == CharacterCodes.eof then
        let end_pos = position scanner in
        scanner.err ~start_pos ~end_pos (Diagnostics.message "Did you forget a \" here?")
      else if scanner.ch == CharacterCodes.double_quote then (
        next scanner
      ) else if CharacterCodes.is_line_break scanner.ch then (
        scanner.line_offset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
        let end_pos = position scanner in
        scanner.err ~start_pos ~end_pos (Diagnostics.message "Did you forget a \" here?");
        next scanner
      ) else (
        Buffer.add_char buffer ((Char.chr [@doesNotRaise]) scanner.ch);
        next scanner;
        scan()
      )
    in
    scan();
    Token.Lident (Buffer.contents buffer)

  let scan_string_escape_sequence ~start_pos scanner =
    (* \ already consumed *)
    if CharacterCodes.Lower.n == scanner.ch
      || CharacterCodes.Lower.t == scanner.ch
      || CharacterCodes.Lower.b == scanner.ch
      || CharacterCodes.Lower.r == scanner.ch
      || CharacterCodes.backslash == scanner.ch
      || CharacterCodes.space == scanner.ch
      || CharacterCodes.single_quote == scanner.ch
      || CharacterCodes.double_quote == scanner.ch
    then
      next scanner
    else
      let (n, base, max) =
        if CharacterCodes.is_digit scanner.ch then
          (* decimal *)
          (3, 10, 255)
        else if scanner.ch == CharacterCodes.Lower.o then
          (* octal *)
          let () = next scanner in
          (3, 8, 255)
        else if scanner.ch == CharacterCodes.Lower.x then
          (* hex *)
          let () = next scanner in
          (2, 16, 255)
        else
          (* unknown escape sequence
           * TODO: we should warn the user here. Let's not make it a hard error for now, for reason compat *)
          (* let pos = position scanner in *)
          (* let () = *)
            (* let msg = if scanner.ch == -1 then *)
              (* "unclosed escape sequence" *)
            (* else "unknown escape sequence" *)
            (* in *)
            (* scanner.err ~startPos ~endPos:pos (Diagnostics.message msg) *)
          (* in *)
          (-1, -1, -1)
        in
        if n < 0 then ()
        else
          let rec while_ n x =
            if n == 0 then x
            else
              let d = CharacterCodes.digit_value scanner.ch in
              if d >= base then
                let pos = position scanner in
                let msg = if scanner.ch == -1 then
                  "unclosed escape sequence"
                else "unknown escape sequence"
                in
                scanner.err ~start_pos ~end_pos:pos (Diagnostics.message msg);
                -1
              else
                let () = next scanner in
                while_ (n - 1) (x * base + d)
          in
          let x = while_ n 0 in
          if x > max then
            let pos = position scanner in
            let msg = "invalid escape sequence (value too high)" in
            scanner.err ~start_pos ~end_pos:pos (Diagnostics.message msg);
          ()

  let scan_string scanner =
    let offs = scanner.offset in

    let start_pos = position scanner in
    let rec scan () =
      if scanner.ch == CharacterCodes.eof then
        let end_pos = position scanner in
        scanner.err ~start_pos ~end_pos Diagnostics.unclosed_string
      else if scanner.ch == CharacterCodes.double_quote then (
        next scanner;
      ) else if scanner.ch == CharacterCodes.backslash then (
        let start_pos = position scanner in
        next scanner;
        scan_string_escape_sequence ~start_pos scanner;
        scan ()
      ) else if CharacterCodes.is_line_break scanner.ch then (
        scanner.line_offset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
        next scanner;
        scan ()
      ) else (
        next scanner;
        scan ()
      )
    in
    scan ();
    Token.String (Bytes.sub_string scanner.src offs (scanner.offset - offs - 1))

  (* I wonder if this gets inlined *)
  let convert_number scanner ~n ~base =
    let x = ref 0 in
    for _ = n downto 1 do
      let d = CharacterCodes.digit_value scanner.ch in
      x := (!x * base) + d;
      next scanner
    done;
    !x

  let scan_escape scanner =
    (* let offset = scanner.offset in *)
    let c = match scanner.ch with
    | 98 (* b *)  -> next scanner; '\008'
    | 110 (* n *) -> next scanner; '\010'
    | 114 (* r *) -> next scanner; '\013'
    | 116 (* t *) -> next scanner; '\009'
    | ch when CharacterCodes.is_digit ch ->
      let x = convert_number scanner ~n:3 ~base:10 in
      (Char.chr [@doesNotRaise]) x
    | ch when ch == CharacterCodes.Lower.x ->
      next scanner;
      let x = convert_number scanner ~n:2 ~base:16 in
      (Char.chr [@doesNotRaise]) x
    | ch when ch == CharacterCodes.Lower.o ->
      next scanner;
      let x = convert_number scanner ~n:3 ~base:8 in
      (Char.chr [@doesNotRaise]) x
    | ch ->
      next scanner;
      (Char.chr [@doesNotRaise]) ch
    in
    next scanner; (* Consume \' *)
    Token.Character c

  let scan_single_line_comment scanner =
    let start_off = scanner.offset in
    let start_pos = position scanner in
    while not (CharacterCodes.is_line_break scanner.ch) && scanner.ch >= 0 do
      next scanner
    done;
    let end_pos = position scanner in
    Token.Comment (
      Comment.make_single_line_comment
        ~loc:(Location.{loc_start = start_pos; loc_end = end_pos; loc_ghost = false})
        (Bytes.sub_string scanner.src start_off (scanner.offset - start_off))
    )

  let scan_multi_line_comment scanner =
    let start_off = scanner.offset in
    let start_pos = position scanner in
    let rec scan ~depth () =
      if scanner.ch == CharacterCodes.asterisk &&
         peek scanner == CharacterCodes.forwardslash then (
        next scanner;
        next scanner;
        if depth > 0 then scan ~depth:(depth - 1) () else ()
      ) else if scanner.ch == CharacterCodes.eof then (
        let end_pos = position scanner in
        scanner.err ~start_pos ~end_pos Diagnostics.unclosed_comment
      ) else if scanner.ch == CharacterCodes.forwardslash
        && peek scanner == CharacterCodes. asterisk then (
        next scanner;
        next scanner;
        scan ~depth:(depth + 1) ()
      ) else (
        if CharacterCodes.is_line_break scanner.ch then (
          scanner.line_offset <- scanner.offset + 1;
          scanner.lnum <- scanner.lnum + 1;
        );
        next scanner;
        scan ~depth ()
      )
    in
    scan ~depth:0 ();
    Token.Comment (
      Comment.make_multi_line_comment
        ~loc:(Location.{loc_start = start_pos; loc_end = (position scanner); loc_ghost = false})
        (Bytes.sub_string scanner.src start_off (scanner.offset - 2 - start_off))
    )

  let scan_template scanner =
    let start_off = scanner.offset in
    let start_pos = position scanner in

    let rec scan () =
      if scanner.ch == CharacterCodes.eof then (
        let end_pos = position scanner in
        scanner.err ~start_pos ~end_pos Diagnostics.unclosed_template;
        pop_mode scanner Template;
        Token.TemplateTail(
          Bytes.sub_string scanner.src start_off (scanner.offset - 2 - start_off)
        )
      )
      else if scanner.ch == CharacterCodes.backslash then (
        next scanner;
        if   scanner.ch == CharacterCodes.backtick
          || scanner.ch == CharacterCodes.backslash
          || scanner.ch == CharacterCodes.dollar
        then next scanner;
        scan()
      ) else if scanner.ch == CharacterCodes.backtick then (
        next scanner;
        let contents =
          Bytes.sub_string scanner.src start_off (scanner.offset - 1 - start_off)
        in
        pop_mode scanner Template;
        Token.TemplateTail contents
      ) else if scanner.ch == CharacterCodes.dollar &&
                peek scanner == CharacterCodes.lbrace
        then (
          next scanner; (* consume $ *)
          next scanner; (* consume { *)
          let contents =
            Bytes.sub_string scanner.src start_off (scanner.offset - 2 - start_off)
          in
          pop_mode scanner Template;
          Token.TemplatePart contents
      ) else (
        if CharacterCodes.is_line_break scanner.ch then (
          scanner.line_offset <- scanner.offset + 1;
          scanner.lnum <- scanner.lnum + 1;
        );
        next scanner;
        scan()
      )
    in
    scan()

  let rec scan scanner =
    if not (in_template_mode scanner) then skip_whitespace scanner;
    let start_pos = position scanner in
    let ch = scanner.ch in
    let token = if in_template_mode scanner then
      scan_template scanner
    else if ch == CharacterCodes.underscore then (
      let next_ch = peek scanner in
      if next_ch == CharacterCodes.underscore || CharacterCodes.is_digit next_ch || CharacterCodes.is_letter next_ch then
        scan_identifier scanner
      else (
        next scanner;
        Token.Underscore
      )
    ) else if CharacterCodes.is_letter ch then
      scan_identifier scanner
    else if CharacterCodes.is_digit ch then
      scan_number scanner
    else begin
      next scanner;
      if ch == CharacterCodes.dot then
        if scanner.ch == CharacterCodes.dot then (
          next scanner;
          if scanner.ch == CharacterCodes.dot then (
            next scanner;
            Token.DotDotDot
          ) else (
            Token.DotDot
          )
        ) else (
          Token.Dot
        )
      else if ch == CharacterCodes.double_quote then
        scan_string scanner
      else if ch == CharacterCodes.single_quote then (
        if scanner.ch == CharacterCodes.backslash
          && not ((peek scanner) == CharacterCodes.double_quote) (* start of exotic ident *)
        then (
          next scanner;
          scan_escape scanner
        ) else if (peek scanner) == CharacterCodes.single_quote then (
          let ch = scanner.ch in
          next scanner;
          next scanner;
          Token.Character ((Char.chr [@doesNotRaise]) ch)
        ) else (
          SingleQuote
        )
      ) else if ch == CharacterCodes.bang then
        if scanner.ch == CharacterCodes.equal then (
          next scanner;
          if scanner.ch == CharacterCodes.equal then (
            next scanner;
            Token.BangEqualEqual
          ) else (
            Token.BangEqual
          )
        ) else (
          Token.Bang
        )
      else if ch == CharacterCodes.semicolon then
        Token.Semicolon
      else if ch == CharacterCodes.equal then (
        if scanner.ch == CharacterCodes.greater_than then (
          next scanner;
          Token.EqualGreater
        ) else if scanner.ch == CharacterCodes.equal then (
          next scanner;
          if scanner.ch == CharacterCodes.equal then (
            next scanner;
            Token.EqualEqualEqual
          ) else (
            Token.EqualEqual
          )
        ) else (
          Token.Equal
        )
      ) else if ch == CharacterCodes.bar then
        if scanner.ch == CharacterCodes.bar then (
          next scanner;
          Token.Lor
        ) else if scanner.ch == CharacterCodes.greater_than then (
          next scanner;
          Token.BarGreater
        ) else (
          Token.Bar
        )
      else if ch == CharacterCodes.ampersand then
        if scanner.ch == CharacterCodes.ampersand then (
          next scanner;
          Token.Land
        ) else (
          Token.Band
        )
      else if ch == CharacterCodes.lparen then
        Token.Lparen
      else if ch == CharacterCodes.rparen then
        Token.Rparen
      else if ch == CharacterCodes.lbracket then
        Token.Lbracket
      else if ch == CharacterCodes.rbracket then
        Token.Rbracket
      else if ch == CharacterCodes.lbrace then
        Token.Lbrace
      else if ch == CharacterCodes.rbrace then
        Token.Rbrace
      else if ch == CharacterCodes.comma then
        Token.Comma
      else if ch == CharacterCodes.colon then
       if scanner.ch == CharacterCodes.equal then(
          next scanner;
          Token.ColonEqual
        ) else if (scanner.ch == CharacterCodes.greater_than) then (
          next scanner;
          Token.ColonGreaterThan
        ) else (
          Token.Colon
        )
      else if ch == CharacterCodes.backslash then
        scan_exotic_identifier scanner
      else if ch == CharacterCodes.forwardslash then
        if scanner.ch == CharacterCodes.forwardslash then (
          next scanner;
          scan_single_line_comment scanner
        ) else if (scanner.ch == CharacterCodes.asterisk) then (
          next scanner;
          scan_multi_line_comment scanner
        ) else if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.ForwardslashDot
        ) else (
          Token.Forwardslash
        )
      else if ch == CharacterCodes.minus then
        if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.MinusDot
        ) else if scanner.ch == CharacterCodes.greater_than then (
          next scanner;
          Token.MinusGreater;
        ) else (
          Token.Minus
        )
      else if ch == CharacterCodes.plus then
        if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.PlusDot
        ) else if scanner.ch == CharacterCodes.plus then (
          next scanner;
          Token.PlusPlus
        ) else if scanner.ch == CharacterCodes.equal then (
          next scanner;
          Token.PlusEqual
        ) else (
          Token.Plus
        )
      else if ch == CharacterCodes.greater_than then
        if scanner.ch == CharacterCodes.equal && not (in_diamond_mode scanner) then (
          next scanner;
          Token.GreaterEqual
        ) else (
          Token.GreaterThan
        )
      else if ch == CharacterCodes.less_than then
        (* Imagine the following: <div><
         * < indicates the start of a new jsx-element, the parser expects
         * the name of a new element after the <
         * Example: <div> <div
         * But what if we have a / here: example </ in  <div></div>
         * This signals a closing element. To simulate the two-token lookahead,
         * the </ is emitted as a single new token LessThanSlash *)
        if in_jsx_mode scanner then (
          skip_whitespace scanner;
          if scanner.ch == CharacterCodes.forwardslash then
            let () = next scanner in
            Token.LessThanSlash
          else
            Token.LessThan
        ) else if scanner.ch == CharacterCodes.equal then (
          next scanner;
          Token.LessEqual
        ) else (
          Token.LessThan
        )
      else if ch == CharacterCodes.hash then
        if scanner.ch == CharacterCodes.hash then(
          next scanner;
          Token.HashHash
        ) else if scanner.ch == CharacterCodes.equal then(
          next scanner;
          Token.HashEqual
        ) else (
          Token.Hash
        )
      else if ch == CharacterCodes.asterisk then
        if scanner.ch == CharacterCodes.asterisk then (
          next scanner;
          Token.Exponentiation;
        ) else if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.AsteriskDot
        ) else (
          Token.Asterisk
        )
      else if ch == CharacterCodes.tilde then
        Token.Tilde
      else if ch == CharacterCodes.question then
        Token.Question
      else if ch == CharacterCodes.at then
        if scanner.ch == CharacterCodes.at then (
          next scanner;
          Token.AtAt
        ) else (
          Token.At
        )
    else if ch == CharacterCodes.percent then
      if scanner.ch == CharacterCodes.percent then (
        next scanner;
        Token.PercentPercent
      ) else (
        Token.Percent
      )
      else if ch == CharacterCodes.backtick  then
        Token.Backtick
      else if ch == -1 then
        Token.Eof
      else (
        (* if we arrive here, we're dealing with an unkown character,
         * report the error and continue scanning… *)
        let end_pos = position scanner in
        scanner.err ~start_pos ~end_pos (Diagnostics.unknown_uchar ch);
        let (_, _, token) = scan scanner in
        token
      )
    end in
    let end_pos = position scanner in
    (start_pos, end_pos, token)

  (* Imagine: <div> <Navbar /> <
   * is `<` the start of a jsx-child? <div …
   * or is it the start of a closing tag?  </div>
   * reconsiderLessThan peeks at the next token and
   * determines the correct token to disambiguate *)
  let reconsider_less_than scanner =
    (* < consumed *)
    skip_whitespace scanner;
    if scanner.ch == CharacterCodes.forwardslash then
      let () = next scanner in
      Token.LessThanSlash
    else
      Token.LessThan

  (* If an operator has whitespace around both sides, it's a binary operator *)
  let is_binary_op src start_cnum end_cnum =
    if start_cnum == 0 then false
    else
      let left_ok =
        let c =
          (start_cnum - 1)
          |> (Bytes.get [@doesNotRaise]) src
          |> Char.code
        in
        c == CharacterCodes.space ||
        c == CharacterCodes.tab ||
        CharacterCodes.is_line_break c
      in
      let right_ok =
        let c =
          if end_cnum == Bytes.length src then -1
          else end_cnum |> (Bytes.get [@doesNotRaise]) src |> Char.code
        in
        c == CharacterCodes.space ||
        c == CharacterCodes.tab ||
        CharacterCodes.is_line_break c ||
        c == CharacterCodes.eof
      in
      left_ok && right_ok
end

(* AST for js externals *)
module JsFfi = struct
  type scope =
    | Global
    | Module of string (* module("path") *)
    | Scope of Longident.t (* scope(/"window", "location"/) *)

  type label_declaration = {
    jld_attributes: Parsetree.attributes; [@live]
    jld_name: string;
    jld_alias: string;
    jld_type: Parsetree.core_type;
    jld_loc:  Location.t
  }

  type import_spec =
    | Default of label_declaration
    | Spec of label_declaration list

  type import_description = {
    jid_loc: Location.t;
    jid_spec: import_spec;
    jid_scope: scope;
    jid_attributes:  Parsetree.attributes;
  }

  let decl ~attrs ~loc ~name ~alias ~typ = {
    jld_loc = loc;
    jld_attributes = attrs;
    jld_name = name;
    jld_alias = alias;
    jld_type = typ
  }

  let import_descr ~attrs ~scope ~import_spec ~loc = {
    jid_loc = loc;
    jid_spec = import_spec;
    jid_scope = scope;
    jid_attributes = attrs;
  }

  let to_parsetree import_descr =
    let bs_val = (Location.mknoloc "val", Parsetree.PStr []) in
    let attrs = match import_descr.jid_scope with
    | Global -> [bs_val]
    (* @genType.import("./MyMath"),
     * @genType.import(/"./MyMath", "default"/) *)
    | Module s ->
      let structure = [
        Parsetree.Pconst_string (s, None)
        |> Ast_helper.Exp.constant
        |> Ast_helper.Str.eval
      ] in
      let gen_type = (Location.mknoloc "genType.import", Parsetree.PStr structure) in
      [gen_type]
    | Scope longident ->
      let structure_item =
        let expr = match Longident.flatten longident |> List.map (fun s ->
          Ast_helper.Exp.constant (Parsetree.Pconst_string (s, None))
        ) with
        | [expr] -> expr
        | [] as exprs | (_ as exprs) -> exprs |> Ast_helper.Exp.tuple
        in
        Ast_helper.Str.eval expr
      in
      let bs_scope = (
        Location.mknoloc "scope",
        Parsetree. PStr [structure_item]
      ) in
      [bs_val; bs_scope]
    in
    let value_descrs = match import_descr.jid_spec with
    | Default decl ->
      let prim = [decl.jld_name] in
      let all_attrs =
        List.concat [attrs; import_descr.jid_attributes]
        |> List.map (fun attr -> match attr with
          | (
              {Location.txt = "genType.import"} as id,
              Parsetree.PStr [{pstr_desc = Parsetree.Pstr_eval (module_name, _) }]
            ) ->
            let default =
              Parsetree.Pconst_string ("default", None) |> Ast_helper.Exp.constant
            in
            let structure_item =
              [module_name; default]
              |> Ast_helper.Exp.tuple
              |> Ast_helper.Str.eval
            in
            (id, Parsetree.PStr [structure_item])
          | attr -> attr
        )
      in
      [Ast_helper.Val.mk
        ~loc:import_descr.jid_loc
        ~prim
        ~attrs:all_attrs
        (Location.mknoloc decl.jld_alias)
        decl.jld_type
      |> Ast_helper.Str.primitive]
    | Spec decls ->
      List.map (fun decl ->
        let prim = [decl.jld_name] in
        let all_attrs = List.concat [attrs; decl.jld_attributes] in
        Ast_helper.Val.mk
          ~loc:import_descr.jid_loc
          ~prim
          ~attrs:all_attrs
          (Location.mknoloc decl.jld_alias)
          decl.jld_type
        |> Ast_helper.Str.primitive ~loc:decl.jld_loc
      ) decls
    in
    let js_ffi_attr = (Location.mknoloc "ns.jsFfi", Parsetree.PStr []) in
    Ast_helper.Mod.structure ~loc:import_descr.jid_loc value_descrs
    |> Ast_helper.Incl.mk ~attrs:[js_ffi_attr] ~loc:import_descr.jid_loc
    |> Ast_helper.Str.include_ ~loc:import_descr.jid_loc
end

module ParsetreeCompatibility = struct
  let concat_longidents l1 l2 =
    let parts1 = Longident.flatten l1 in
    let parts2 = Longident.flatten l2 in
    match List.concat [parts1; parts2] |> Longident.unflatten with
    | Some longident -> longident
    | None -> l2

  (* TODO: support nested open's ? *)
  let rec rewrite_ppat_open longident_open pat =
    let open Parsetree in
    match pat.ppat_desc with
    | Ppat_array (first::rest) ->
      (* Color.[Red, Blue, Green] -> [Color.Red, Blue, Green] *)
      {pat with ppat_desc = Ppat_array ((rewrite_ppat_open longident_open first)::rest)}
    | Ppat_tuple (first::rest) ->
      (* Color.(Red, Blue, Green) -> (Color.Red, Blue, Green) *)
      {pat with ppat_desc = Ppat_tuple ((rewrite_ppat_open longident_open first)::rest)}
    | Ppat_construct(
        {txt = Longident.Lident "::"} as list_constructor,
        Some ({ppat_desc=Ppat_tuple (pat::rest)} as element)
      ) ->
      (* Color.(list[Red, Blue, Green]) -> list[Color.Red, Blue, Green] *)
      {pat with ppat_desc =
        Ppat_construct (
          list_constructor,
          Some {element with ppat_desc = Ppat_tuple ((rewrite_ppat_open longident_open pat)::rest)}
        )
      }
    | Ppat_construct ({txt = constructor} as longident_loc, opt_pattern) ->
      (* Foo.(Bar(a)) -> Foo.Bar(a) *)
      {pat with ppat_desc =
        Ppat_construct (
          {longident_loc with txt = concat_longidents longident_open constructor},
          opt_pattern
        )
      }
    | Ppat_record (({txt = lbl} as longident_loc, first_pat)::rest, flag) ->
      (* Foo.{x} -> {Foo.x: x} *)
      let first_row = (
        {longident_loc with txt = concat_longidents longident_open lbl},
        first_pat
      ) in
      {pat with ppat_desc = Ppat_record (first_row::rest, flag)}
    | Ppat_or (pat1, pat2) ->
      {pat with ppat_desc = Ppat_or (
        rewrite_ppat_open longident_open pat1,
        rewrite_ppat_open longident_open pat2
      )}
    | Ppat_constraint (pattern, typ) ->
      {pat with ppat_desc = Ppat_constraint (
        rewrite_ppat_open longident_open pattern,
        typ
      )}
    | Ppat_type ({txt = constructor} as longident_loc) ->
      {pat with ppat_desc = Ppat_type (
        {longident_loc with txt = concat_longidents longident_open constructor}
      )}
    | Ppat_lazy p ->
      {pat with ppat_desc = Ppat_lazy (rewrite_ppat_open longident_open p)}
    | Ppat_exception p ->
      {pat with ppat_desc = Ppat_exception (rewrite_ppat_open longident_open p)}
    | _ -> pat

  let rec rewrite_reason_fast_pipe expr =
    let open Parsetree in
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_apply (
          {pexp_desc = Pexp_ident {txt = Longident.Lident "|."}} as op,
          [Asttypes.Nolabel, lhs; Nolabel, rhs]
        ); pexp_attributes = sub_attrs},
        args
      ) ->
      let rhs_loc = {rhs.pexp_loc with loc_end = expr.pexp_loc.loc_end} in
      let new_lhs =
        let expr = rewrite_reason_fast_pipe lhs in
        {expr with pexp_attributes = sub_attrs}
      in
      let all_args =
        (Asttypes.Nolabel, new_lhs)::[
          Asttypes.Nolabel, Ast_helper.Exp.apply ~loc:rhs_loc rhs args
        ]
      in
      Ast_helper.Exp.apply ~attrs:expr.pexp_attributes ~loc:expr.pexp_loc op all_args
    | _ -> expr

  let make_reason_arity_mapper ~for_printer =
    let open Ast_mapper in
    { default_mapper with
      expr = begin fun mapper expr ->
        match expr with
        (* Don't mind this case, Reason doesn't handle this. *)
        (* | {pexp_desc = Pexp_variant (lbl, args); pexp_loc; pexp_attributes} -> *)
          (* let newArgs = match args with *)
          (* | (Some {pexp_desc = Pexp_tuple [{pexp_desc = Pexp_tuple _ } as sp]}) as args-> *)
            (* if forPrinter then args else Some sp *)
          (* | Some {pexp_desc = Pexp_tuple [sp]} -> Some sp *)
          (* | _ -> args *)
          (* in *)
          (* default_mapper.expr mapper {pexp_desc=Pexp_variant(lbl, newArgs); pexp_loc; pexp_attributes} *)
        | {pexp_desc=Pexp_construct(lid, args); pexp_loc; pexp_attributes} ->
          let new_args = match args with
          | (Some {pexp_desc = Pexp_tuple [{pexp_desc = Pexp_tuple _ } as sp]}) as args ->
            if for_printer then args else Some sp
          | Some {pexp_desc = Pexp_tuple [sp]} -> Some sp
          | _ -> args
          in
          default_mapper.expr mapper { pexp_desc=Pexp_construct(lid, new_args); pexp_loc; pexp_attributes}
        | expr ->
          default_mapper.expr mapper (rewrite_reason_fast_pipe expr)
      end;
      pat = begin fun mapper pattern ->
        match pattern with
        (* Don't mind this case, Reason doesn't handle this. *)
        (* | {ppat_desc = Ppat_variant (lbl, args); ppat_loc; ppat_attributes} -> *)
          (* let newArgs = match args with *)
          (* | (Some {ppat_desc = Ppat_tuple [{ppat_desc = Ppat_tuple _} as sp]}) as args -> *)
            (* if forPrinter then args else Some sp *)
          (* | Some {ppat_desc = Ppat_tuple [sp]} -> Some sp *)
          (* | _ -> args *)
          (* in *)
          (* default_mapper.pat mapper {ppat_desc = Ppat_variant (lbl, newArgs); ppat_loc; ppat_attributes;} *)
        | {ppat_desc=Ppat_construct(lid, args);
           ppat_loc;
           ppat_attributes} ->
           let new_args = match args with
           | (Some {ppat_desc = Ppat_tuple [{ppat_desc = Ppat_tuple _} as sp]}) as args ->
            if for_printer then args else Some sp
           | Some {ppat_desc = Ppat_tuple [sp]} -> Some sp
           | _ -> args in
           default_mapper.pat mapper { ppat_desc=Ppat_construct(lid, new_args); ppat_loc; ppat_attributes;}
          | x -> default_mapper.pat mapper x
      end;
    }

  let escape_template_literal s =
    let len = String.length s in
    let b = Buffer.create len in
    let i = ref 0 in
    while !i < len do
      let c = (String.get [@doesNotRaise]) s !i in
      if c = '`' then (
        Buffer.add_char b '\\';
        Buffer.add_char b '`';
        incr i;
      ) else if c = '$' then (
        if !i + 1 < len then (
          let c2 = (String.get [@doesNotRaise]) s (!i + 1) in
          if c2 = '{' then (
            Buffer.add_char b '\\';
            Buffer.add_char b '$';
            Buffer.add_char b '{';
          ) else (
            Buffer.add_char b c;
            Buffer.add_char b c2;
          );
          i := !i + 2;
        ) else (
          Buffer.add_char b c;
          incr i
        )
      ) else if c = '\\' then (
        Buffer.add_char b '\\';
        Buffer.add_char b '\\';
        incr i;
      ) else (
        Buffer.add_char b c;
        incr i
      )
    done;
    Buffer.contents b

  let escape_string_contents s =
    let len = String.length s in
    let b = Buffer.create len in

    let i = ref 0 in

    while !i < len do
      let c = String.unsafe_get s !i in
      if c = '\\' then (
        incr i;
        Buffer.add_char b c;
        let c = String.unsafe_get s !i in
        if !i < len then
          let () = Buffer.add_char b c in
          incr i
        else
          ()
      ) else if c = '"' then (
        Buffer.add_char b '\\';
        Buffer.add_char b c;
        incr i;
      ) else (
        Buffer.add_char b c;
        incr i;
      )
    done;
    Buffer.contents b

  let looks_like_recursive_type_declaration type_declaration =
    let open Parsetree in
    let name = type_declaration.ptype_name.txt in
    let rec check_kind kind =
      match kind with
      | Ptype_abstract | Ptype_open -> false
      | Ptype_variant constructor_declarations ->
        List.exists check_constructor_declaration constructor_declarations
      | Ptype_record label_declarations ->
        List.exists check_label_declaration label_declarations

    and check_constructor_declaration constr_decl =
      check_constructor_arguments constr_decl.pcd_args
      || (match constr_decl.pcd_res with
      | Some typexpr ->
        check_typ_expr typexpr
      | None -> false
      )

    and check_label_declaration label_declaration =
      check_typ_expr label_declaration.pld_type

    and check_constructor_arguments constr_arg =
      match constr_arg with
      | Pcstr_tuple types ->
        List.exists check_typ_expr types
      | Pcstr_record label_declarations ->
        List.exists check_label_declaration label_declarations

    and check_typ_expr typ =
      match typ.ptyp_desc with
      | Ptyp_any -> false
      | Ptyp_var _ -> false
      | Ptyp_object _ -> false
      | Ptyp_class _ -> false
      | Ptyp_package _ -> false
      | Ptyp_extension _ -> false
      | Ptyp_arrow (_lbl, typ1, typ2) ->
        check_typ_expr typ1 || check_typ_expr typ2
      | Ptyp_tuple types ->
        List.exists check_typ_expr types
      | Ptyp_constr ({txt = longident}, types) ->
        (match longident with
        | Lident ident -> ident = name
        | _ -> false
        ) ||
        List.exists check_typ_expr types
      | Ptyp_alias (typ, _) -> check_typ_expr typ
      | Ptyp_variant (row_fields, _, _) ->
        List.exists check_row_fields row_fields
      | Ptyp_poly (_, typ) ->
        check_typ_expr typ

    and check_row_fields row_field =
      match row_field with
      | Rtag (_, _, _, types) ->
        List.exists check_typ_expr types
      | Rinherit typexpr ->
        check_typ_expr typexpr
    in
    check_kind type_declaration.ptype_kind


  let filter_reason_raw_literal attrs =
    List.filter (fun attr ->
      match attr with
      | ({Location.txt = ("reason.raw_literal")}, _) -> false
      | _ -> true
    ) attrs

  let string_literal_mapper string_data =
    let is_same_location l1 l2 =
      let open Location in
      l1.loc_start.pos_cnum == l2.loc_start.pos_cnum
    in
    let remaining_string_data = string_data in
    let open Ast_mapper in
    { default_mapper with
      expr = (fun mapper expr ->
        match expr.pexp_desc with
        | Pexp_constant (Pconst_string (_txt, None)) ->
          begin match
            List.find_opt (fun (_stringData, string_loc) ->
              is_same_location string_loc expr.pexp_loc
            ) remaining_string_data
          with
          | Some(string_data, _) ->
            let string_data =
              let attr = List.find_opt (fun attr -> match attr with
              | ({Location.txt = ("reason.raw_literal")}, _) -> true
              | _ -> false
              ) expr.pexp_attributes in
              match attr with
              | Some (_, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (raw, _))}, _)}]) ->
                raw
              | _ -> (String.sub [@doesNotRaise]) string_data 1 (String.length string_data - 2)
              in
            {expr with
              pexp_attributes = filter_reason_raw_literal expr.pexp_attributes;
              pexp_desc = Pexp_constant (Pconst_string (string_data, None))
            }
          | None ->
            default_mapper.expr mapper expr
          end
        | _ -> default_mapper.expr mapper expr
      )
    }

  let normalize =
    let open Ast_mapper in
    { default_mapper with
      attributes = (fun mapper attrs ->
        attrs
        |> List.filter (fun attr ->
          match attr with
          | ({Location.txt = (
                "reason.preserve_braces"
              | "explicit_arity"
              | "implicity_arity"
            )}, _) -> false
          | _ ->true
        )
        |> default_mapper.attributes mapper
      );
      pat = begin fun mapper p ->
        match p.ppat_desc with
        | Ppat_open ({txt = longident_open}, pattern) ->
          let p = rewrite_ppat_open longident_open pattern in
          default_mapper.pat mapper p
        | _ ->
          default_mapper.pat mapper p
      end;
      expr = (fun mapper expr ->
        match expr.pexp_desc with
        | Pexp_constant (Pconst_string (txt, None)) ->
          let raw = escape_string_contents txt in
          let s = Parsetree.Pconst_string (raw, None) in
          let expr = Ast_helper.Exp.constant
            ~attrs:expr.pexp_attributes
            ~loc:expr.pexp_loc s
          in
          expr
        | Pexp_constant (Pconst_string (txt, tag)) ->
          let s = Parsetree.Pconst_string ((escape_template_literal txt), tag) in
          Ast_helper.Exp.constant
            ~attrs:(mapper.attributes mapper expr.pexp_attributes)
            ~loc:expr.pexp_loc
            s
        | Pexp_function cases ->
          let loc = match (cases, List.rev cases) with
          | (first::_), (last::_) ->
            {first.pc_lhs.ppat_loc with loc_end = last.pc_rhs.pexp_loc.loc_end}
          | _ -> Location.none
          in
          Ast_helper.Exp.fun_ ~loc
            Asttypes.Nolabel None (Ast_helper.Pat.var (Location.mknoloc "x"))
            (Ast_helper.Exp.match_ ~loc
              (Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident "x")))
              (default_mapper.cases mapper cases)
            )
        | Pexp_apply (
            {pexp_desc = Pexp_ident {txt = Longident.Lident "!"}},
            [Asttypes.Nolabel, operand]
          ) ->
          (* turn `!foo` into `foo.contents` *)
          Ast_helper.Exp.field ~loc:expr.pexp_loc ~attrs:expr.pexp_attributes
            operand
            (Location.mknoloc (Longident.Lident "contents"))
        | Pexp_apply (
            {pexp_desc = Pexp_ident {txt = Longident.Lident "##"}} as op,
            [Asttypes.Nolabel, lhs; Nolabel, ({pexp_desc = Pexp_constant (Pconst_string (txt, None))} as string_expr)]
          ) ->
          let ident = Ast_helper.Exp.ident ~loc:string_expr.pexp_loc
            (Location.mkloc (Longident.Lident txt) string_expr.pexp_loc)
          in
          Ast_helper.Exp.apply ~loc:expr.pexp_loc ~attrs:expr.pexp_attributes
            op [Asttypes.Nolabel, lhs; Nolabel, ident]
        | Pexp_apply (
            {pexp_desc = Pexp_ident {txt = Longident.Lident "@@"}},
            [Asttypes.Nolabel, call_expr; Nolabel, arg_expr]
          ) ->
          Ast_helper.Exp.apply (mapper.expr mapper call_expr) [
            Asttypes.Nolabel, mapper.expr mapper arg_expr
          ]
        | Pexp_apply (
            {pexp_desc = Pexp_ident {txt = Longident.Lident "@"}},
            [Nolabel, arg1; Nolabel, arg2]
          ) ->
          let list_concat = Longident.Ldot (Longident.Lident "List", "append") in
          Ast_helper.Exp.apply
            (Ast_helper.Exp.ident (Location.mknoloc list_concat))
            [Nolabel, mapper.expr mapper arg1; Nolabel, mapper.expr mapper arg2]
        | Pexp_match (
            condition,
            [
              {pc_lhs = {ppat_desc = Ppat_construct ({txt = Longident.Lident "true"}, None)}; pc_rhs = then_expr };
              {pc_lhs = {ppat_desc = Ppat_construct ({txt = Longident.Lident "false"}, None)}; pc_rhs = else_expr };
            ]
          ) ->
          let ternary_marker = (Location.mknoloc "res.ternary", Parsetree.PStr []) in
          Ast_helper.Exp.ifthenelse
            ~loc:expr.pexp_loc
            ~attrs:(ternary_marker::expr.pexp_attributes)
            (default_mapper.expr mapper condition)
            (default_mapper.expr mapper then_expr)
            (Some (default_mapper.expr mapper else_expr))
        | _ -> default_mapper.expr mapper expr
      );
      structure_item = begin fun mapper structure_item ->
        match structure_item.pstr_desc with
        (* heuristic: if we have multiple type declarations, mark them recursive *)
        | Pstr_type (rec_flag, type_declarations) ->
          let flag = match type_declarations with
          | [td] ->
            if looks_like_recursive_type_declaration td then Asttypes.Recursive
            else Asttypes.Nonrecursive
          | _ -> rec_flag
          in
          {structure_item with pstr_desc = Pstr_type (
            flag,
            List.map (fun type_declaration ->
              default_mapper.type_declaration mapper type_declaration
            ) type_declarations
          )}
        | _ -> default_mapper.structure_item mapper structure_item
      end;
      signature_item = begin fun mapper signature_item ->
        match signature_item.psig_desc with
        (* heuristic: if we have multiple type declarations, mark them recursive *)
        | Psig_type (rec_flag, type_declarations) ->
          let flag = match type_declarations with
          | [td] ->
            if looks_like_recursive_type_declaration td then Asttypes.Recursive
            else Asttypes.Nonrecursive
          | _ -> rec_flag
          in
          {signature_item with psig_desc = Psig_type (
            flag,
            List.map (fun type_declaration ->
              default_mapper.type_declaration mapper type_declaration
            ) type_declarations
          )}
        | _ -> default_mapper.signature_item mapper signature_item
      end;
      value_binding = begin fun mapper vb ->
       match vb with
       | {
           pvb_pat = {ppat_desc = Ppat_var _} as pat;
           pvb_expr = {pexp_loc = expr_loc; pexp_desc = Pexp_constraint (expr, typ) }
         } when expr_loc.loc_ghost ->
        (* let t: t = (expr : t) -> let t: t = expr *)
        let typ = default_mapper.typ mapper typ in
        let pat =  default_mapper.pat mapper pat in
        let expr = mapper.expr mapper expr in
        let new_pattern = Ast_helper.Pat.constraint_
          ~loc:{pat.ppat_loc with loc_end = typ.ptyp_loc.loc_end}
          pat typ in
        {vb with
          pvb_pat = new_pattern;
          pvb_expr = expr;
          pvb_attributes = default_mapper.attributes mapper vb.pvb_attributes}
       | {
           pvb_pat = {ppat_desc = Ppat_constraint (pat, {ptyp_desc = Ptyp_poly ([], _)})} ;
           pvb_expr = {pexp_loc = expr_loc; pexp_desc = Pexp_constraint (expr, typ) }
         } when expr_loc.loc_ghost ->
        (* let t: . t = (expr : t) -> let t: t = expr *)
        let typ = default_mapper.typ mapper typ in
        let pat =  default_mapper.pat mapper pat in
        let expr = mapper.expr mapper expr in
        let new_pattern = Ast_helper.Pat.constraint_
          ~loc:{pat.ppat_loc with loc_end = typ.ptyp_loc.loc_end}
          pat typ in
        {vb with
          pvb_pat = new_pattern;
          pvb_expr = expr;
          pvb_attributes = default_mapper.attributes mapper vb.pvb_attributes}
      | _ -> default_mapper.value_binding mapper vb
      end;
    }

  let normalize_reason_arity_structure ~for_printer s =
    let mapper = make_reason_arity_mapper ~for_printer in
    mapper.Ast_mapper.structure mapper s

  let normalize_reason_arity_signature ~for_printer s =
    let mapper = make_reason_arity_mapper ~for_printer in
    mapper.Ast_mapper.signature mapper s

  let structure s = normalize.Ast_mapper.structure normalize s
  let signature s = normalize.Ast_mapper.signature normalize s

  let replace_string_literal_structure string_data structure =
    let mapper = string_literal_mapper string_data in
    mapper.Ast_mapper.structure mapper structure

  let replace_string_literal_signature string_data signature =
    let mapper = string_literal_mapper string_data in
    mapper.Ast_mapper.signature mapper signature
end

module OcamlParser = Parser

module Parser = struct
  type mode = ParseForTypeChecker | Default

  type region_status = Report | Silent

  type t = {
    mode: mode;
    mutable scanner: Scanner.t;
    mutable token: Token.t;
    mutable start_pos: Lexing.position;
    mutable end_pos: Lexing.position;
    mutable prev_end_pos: Lexing.position;
    mutable breadcrumbs: (Grammar.t * Lexing.position) list;
    mutable errors: Reporting.parse_error list;
    mutable diagnostics: Diagnostics.t list;
    mutable comments: Comment.t list;
    mutable regions: region_status ref list;
  }

  let err ?start_pos ?end_pos p error =
    let d = Diagnostics.make
      ~filename:p.scanner.filename
      ~start_pos:(match start_pos with | Some pos -> pos | None -> p.start_pos)
      ~end_pos:(match end_pos with | Some pos -> pos | None -> p.end_pos)
      error
    in
    try
      if (!(List.hd p.regions) = Report) then (
        p.diagnostics <- d::p.diagnostics;
        List.hd p.regions := Silent
      )
    with Failure _ -> ()

  let begin_region p =
    p.regions <- ref Report :: p.regions
  let end_region p =
    try p.regions <- List.tl p.regions with Failure _ -> ()

   (* Advance to the next non-comment token and store any encountered comment
    * in the parser's state. Every comment contains the end position of it's
    * previous token to facilite comment interleaving *)
   let rec next ?prev_end_pos p =
     let prev_end_pos = match prev_end_pos with Some pos -> pos | None -> p.end_pos in
     let (start_pos, end_pos, token) = Scanner.scan p.scanner in
     match token with
     | Comment c ->
       Comment.set_prev_tok_end_pos c p.end_pos;
       p.comments <- c::p.comments;
       p.prev_end_pos <- p.end_pos;
       p.end_pos <- end_pos;
       next ~prev_end_pos p
     | _ ->
       p.token <- token;
       (* p.prevEndPos <- prevEndPos; *)
       p.prev_end_pos <- prev_end_pos;
       p.start_pos <- start_pos;
       p.end_pos <- end_pos

  let check_progress ~prev_end_pos ~result p =
    if p.end_pos == prev_end_pos
    then None
    else Some result

  let make ?(mode=ParseForTypeChecker) src filename =
    let scanner = Scanner.make (Bytes.of_string src) filename in
    let parser_state = {
      mode;
      scanner;
      token = Token.Eof;
      start_pos = Lexing.dummy_pos;
      prev_end_pos = Lexing.dummy_pos;
      end_pos = Lexing.dummy_pos;
      breadcrumbs = [];
      errors = [];
      diagnostics = [];
      comments = [];
      regions = [ref Report];
    } in
    parser_state.scanner.err <- (fun ~start_pos ~end_pos error ->
      let diagnostic = Diagnostics.make
        ~filename
        ~start_pos
        ~end_pos
        error
      in
      parser_state.diagnostics <- diagnostic::parser_state.diagnostics
    );
    next parser_state;
    parser_state

  let leave_breadcrumb p circumstance =
    let crumb = (circumstance, p.start_pos) in
    p.breadcrumbs <- crumb::p.breadcrumbs

  let eat_breadcrumb p =
    match p.breadcrumbs with
    | [] -> ()
    | _::crumbs -> p.breadcrumbs <- crumbs

  let optional p token =
    if p.token = token then
      let () = next p in true
    else
      false

  let expect ?grammar token p =
    if p.token = token then
      next p
    else
      let error = Diagnostics.expected ?grammar p.prev_end_pos token in
      err ~start_pos:p.prev_end_pos p error

  (* Don't use immutable copies here, it trashes certain heuristics
   * in the ocaml compiler, resulting in massive slowdowns of the parser *)
  let lookahead p callback =
    let err = p.scanner.err in
    let ch = p.scanner.ch in
    let offset = p.scanner.offset in
    let rd_offset = p.scanner.rd_offset in
    let line_offset = p.scanner.line_offset in
    let lnum = p.scanner.lnum in
    let mode = p.scanner.mode in
    let token = p.token in
    let start_pos = p.start_pos in
    let end_pos = p.end_pos in
    let prev_end_pos = p.prev_end_pos in
    let breadcrumbs = p.breadcrumbs in
    let errors = p.errors in
    let diagnostics = p.diagnostics in
    let comments = p.comments in

    let res = callback p in

    p.scanner.err <- err;
    p.scanner.ch <- ch;
    p.scanner.offset <- offset;
    p.scanner.rd_offset <- rd_offset;
    p.scanner.line_offset <- line_offset;
    p.scanner.lnum <- lnum;
    p.scanner.mode <- mode;
    p.token <- token;
    p.start_pos <- start_pos;
    p.end_pos <- end_pos;
    p.prev_end_pos <- prev_end_pos;
    p.breadcrumbs <- breadcrumbs;
    p.errors <- errors;
    p.diagnostics <- diagnostics;
    p.comments <- comments;

    res
end

module NapkinScript = struct
  let mk_loc start_loc end_loc = Location.{
    loc_start = start_loc;
    loc_end = end_loc;
    loc_ghost = false;
  }


  module Recover = struct
    type action = unit option (* None is abort, Some () is retry *)

    let default_expr () =
      let id = Location.mknoloc "napkinscript.exprhole" in
      Ast_helper.Exp.mk (Pexp_extension (id, PStr []))

    let default_type () =
      let id = Location.mknoloc "napkinscript.typehole" in
      Ast_helper.Typ.extension (id, PStr [])

    let default_pattern () =
      let id = Location.mknoloc "napkinscript.patternhole" in
      Ast_helper.Pat.extension (id, PStr [])
      (* Ast_helper.Pat.any  () *)

    let default_module_expr () = Ast_helper.Mod.structure []
    let default_module_type () = Ast_helper.Mty.signature []

    let recover_equal_greater p =
      Parser.expect EqualGreater p;
      match p.Parser.token with
      | MinusGreater -> Parser.next p
      | _ -> ()

    let should_abort_list_parse p =
      let rec check breadcrumbs =
        match breadcrumbs with
        | [] -> false
        | (grammar, _)::rest ->
          if Grammar.is_part_of_list grammar p.Parser.token then
            true
          else
            check rest
      in
      check p.breadcrumbs
  end

  module ErrorMessages = struct
    let list_pattern_spread = "List pattern matches only supports one `...` spread, at the end.
Explanation: a list spread at the tail is efficient, but a spread in the middle would create new list[s]; out of performance concern, our pattern matching currently guarantees to never create new intermediate data."

    let record_pattern_spread = "Record's `...` spread is not supported in pattern matches.
Explanation: you can't collect a subset of a record's field into its own record, since a record needs an explicit declaration and that subset wouldn't have one.
Solution: you need to pull out each field you want explicitly."

    let record_pattern_underscore = "Record patterns only support one `_`, at the end."
    [@@live]

    let array_pattern_spread = "Array's `...` spread is not supported in pattern matches.
Explanation: such spread would create a subarray; out of performance concern, our pattern matching currently guarantees to never create new intermediate data.
Solution: if it's to validate the first few elements, use a `when` clause + Array size check + `get` checks on the current pattern. If it's to obtain a subarray, use `Array.sub` or `Belt.Array.slice`."

    let array_expr_spread = "Arrays can't use the `...` spread currently. Please use `concat` or other Array helpers."

    let record_expr_spread = "Records can only have one `...` spread, at the beginning.
Explanation: since records have a known, fixed shape, a spread like `{a, ...b}` wouldn't make sense, as `b` would override every field of `a` anyway."

    let list_expr_spread =  "Lists can only have one `...` spread, and at the end.
Explanation: lists are singly-linked list, where a node contains a value and points to the next node. `list[a, ...bc]` efficiently creates a new item and links `bc` as its next nodes. `[...bc, a]` would be expensive, as it'd need to traverse `bc` and prepend each item to `a` one by one. We therefore disallow such syntax sugar.
Solution: directly use `concat`."

    let variant_ident = "A polymorphic variant (e.g. #id) must start with an alphabetical letter."
end


  let jsx_attr = (Location.mknoloc "JSX", Parsetree.PStr [])
  let uncurry_attr = (Location.mknoloc "bs", Parsetree.PStr [])
  let ternary_attr = (Location.mknoloc "res.ternary", Parsetree.PStr [])
  let make_braces_attr loc = (Location.mkloc "res.braces" loc, Parsetree.PStr [])

  type typ_def_or_ext =
    | TypeDef of {rec_flag: Asttypes.rec_flag; types: Parsetree.type_declaration list}
    | TypeExt of Parsetree.type_extension

  type labelled_parameter =
    | TermParameter of
        {uncurried: bool; attrs: Parsetree.attributes; label: Asttypes.arg_label; expr: Parsetree.expression option;
        pat: Parsetree.pattern; pos: Lexing.position}
    | TypeParameter of {uncurried: bool; attrs: Parsetree.attributes; locs: string Location.loc list; pos: Lexing.position}

  type record_pattern_item =
    | PatUnderscore
    | PatField of (Ast_helper.lid * Parsetree.pattern)

  type context =
    | OrdinaryExpr
    | TernaryTrueBranchExpr
    | WhenExpr

  let get_closing_token = function
    | Token.Lparen -> Token.Rparen
    | Lbrace -> Rbrace
    | Lbracket -> Rbracket
    | _ -> assert false

  let rec go_to_closing closing_token state =
    match (state.Parser.token, closing_token) with
    | (Rparen, Token.Rparen) | (Rbrace, Rbrace) | (Rbracket, Rbracket) ->
      Parser.next state;
      ()
    | (Token.Lbracket | Lparen | Lbrace) as t, _ ->
      Parser.next state;
      go_to_closing (get_closing_token t) state;
      go_to_closing closing_token state
    | ((Rparen | Token.Rbrace | Rbracket | Eof), _)  ->
      () (* TODO: how do report errors here? *)
    | _ ->
      Parser.next state;
      go_to_closing closing_token state

  (* Madness *)
  let is_es6_arrow_expression ~in_ternary p =
    Parser.lookahead p (fun state ->
      match state.Parser.token with
      | Lident _ | List | Underscore ->
        Parser.next state;
        begin match state.Parser.token with
        (* Don't think that this valid
         * Imagine: let x = (a: int)
         * This is a parenthesized expression with a type constraint, wait for
         * the arrow *)
        (* | Colon when not inTernary -> true *)
        | EqualGreater -> true
        | _ -> false
        end
      | Lparen ->
        let prev_end_pos = state.prev_end_pos in
        Parser.next state;
        begin match state.token with
        | Rparen ->
          Parser.next state;
          begin match state.Parser.token with
          | Colon when not in_ternary -> true
          | EqualGreater -> true
          | _ -> false
          end
        | Dot (* uncurried *) -> true
        | Tilde -> true
        | Backtick -> false (* (` always indicates the start of an expr, can't be es6 parameter *)
        | _ ->
          go_to_closing Rparen state;
          begin match state.Parser.token with
          | EqualGreater -> true
          (* | Lbrace TODO: detect missing =>, is this possible? *)
          | Colon when not in_ternary -> true
          | Rparen ->
            (* imagine having something as :
             * switch colour {
             * | Red
             *    when l == l'
             *    || (&Clflags.classic && (l == Nolabel && !is_optional(l'))) => (t1, t2)
             * We'll arrive at the outer rparen just before the =>.
             * This is not an es6 arrow.
             * *)
            false
          | _ ->
            Parser.next state;
            (* error recovery, peek at the next token,
             * (elements, providerId] => {
             *  in the example above, we have an unbalanced ] here
             *)
            begin match state.Parser.token with
            | EqualGreater when state.start_pos.pos_lnum == prev_end_pos.pos_lnum -> true
            | _ -> false
            end
          end
        end
      | _ -> false)


  let is_es6_arrow_functor p =
    Parser.lookahead p (fun state ->
      match state.Parser.token with
      (* | Uident _ | Underscore -> *)
        (* Parser.next state; *)
        (* begin match state.Parser.token with *)
        (* | EqualGreater -> true *)
        (* | _ -> false *)
        (* end *)
      | Lparen ->
        Parser.next state;
        begin match state.token with
        | Rparen ->
          Parser.next state;
          begin match state.token with
          | Colon | EqualGreater -> true
          | _ -> false
          end
        | _ ->
          go_to_closing Rparen state;
          begin match state.Parser.token with
          | EqualGreater | Lbrace -> true
          | Colon -> true
          | _ -> false
          end
        end
      | _ -> false
    )

  let is_es6_arrow_type p =
    Parser.lookahead p (fun state ->
      match state.Parser.token with
      | Lparen ->
        Parser.next state;
        begin match state.Parser.token with
        | Rparen ->
          Parser.next state;
          begin match state.Parser.token with
          | EqualGreater -> true
          | _ -> false
          end
        | Tilde | Dot -> true
        | _ ->
          go_to_closing Rparen state;
          begin match state.Parser.token with
          | EqualGreater  -> true
          | _ -> false
          end
        end
      | Tilde -> true
      | _ -> false
    )

  let build_longident words = match List.rev words with
    | [] -> assert false
    | hd::tl -> List.fold_left (fun p s -> Longident.Ldot (p, s)) (Lident hd) tl

  let make_infix_operator p token start_pos end_pos =
    let stringified_token =
      if token = Token.MinusGreater then "|."
      else if token = Token.PlusPlus then "^"
      else if token = Token.BangEqual then "<>"
      else if token = Token.BangEqualEqual then "!="
      else if token = Token.Equal then (
        (* TODO: could have a totally different meaning like x->fooSet(y)*)
        Parser.err ~start_pos ~end_pos p (
          Diagnostics.message "Did you mean `==` here?"
        );
        "="
      ) else if token = Token.EqualEqual then "="
      else if token = Token.EqualEqualEqual then "=="
      else Token.to_string token
    in
    let loc = mk_loc start_pos end_pos in
    let operator = Location.mkloc
      (Longident.Lident stringified_token) loc
    in
    Ast_helper.Exp.ident ~loc operator

  let negate_string s =
    if String.length s > 0 && (s.[0] [@doesNotRaise]) = '-'
    then (String.sub [@doesNotRaise]) s 1 (String.length s - 1)
    else "-" ^ s

  let make_unary_expr start_pos token_end token operand =
    match token, operand.Parsetree.pexp_desc with
    | (Token.Plus | PlusDot), Pexp_constant((Pconst_integer _ | Pconst_float _)) ->
      operand
    | Minus, Pexp_constant(Pconst_integer (n,m)) ->
      {operand with pexp_desc = Pexp_constant(Pconst_integer (negate_string n,m))}
    | (Minus | MinusDot), Pexp_constant(Pconst_float (n,m)) ->
      {operand with pexp_desc = Pexp_constant(Pconst_float (negate_string n,m))}
    | (Token.Plus | PlusDot | Minus | MinusDot ), _ ->
      let token_loc = mk_loc start_pos token_end in
      let operator = "~" ^ Token.to_string token in
      Ast_helper.Exp.apply
        ~loc:(mk_loc start_pos operand.Parsetree.pexp_loc.loc_end)
        (Ast_helper.Exp.ident ~loc:token_loc
          (Location.mkloc (Longident.Lident operator) token_loc))
        [Nolabel, operand]
    | Token.Bang, _ ->
      let token_loc = mk_loc start_pos token_end in
      Ast_helper.Exp.apply
        ~loc:(mk_loc start_pos operand.Parsetree.pexp_loc.loc_end)
        (Ast_helper.Exp.ident ~loc:token_loc
          (Location.mkloc (Longident.Lident "not") token_loc))
        [Nolabel, operand]
    | _ ->
      operand

  let make_list_expression loc seq ext_opt =
    let rec handle_seq = function
      | [] ->
        begin match ext_opt with
        | Some ext -> ext
        | None ->
          let loc = {loc with Location.loc_ghost = true} in
          let nil = Location.mkloc (Longident.Lident "[]") loc in
          Ast_helper.Exp.construct ~loc nil None
        end
      | e1 :: el ->
        let exp_el = handle_seq el in
        let loc = mk_loc
          e1.Parsetree.pexp_loc.Location.loc_start
          exp_el.pexp_loc.loc_end
        in
        let arg = Ast_helper.Exp.tuple ~loc [e1; exp_el] in
        Ast_helper.Exp.construct ~loc
          (Location.mkloc (Longident.Lident "::") loc)
          (Some arg)
    in
    let expr = handle_seq seq in
    {expr with pexp_loc = loc}

  let make_list_pattern loc seq ext_opt =
    let rec handle_seq = function
      [] ->
        let base_case = match ext_opt with
          | Some ext ->
            ext
          | None ->
            let loc = { loc with Location.loc_ghost = true} in
            let nil = { Location.txt = Longident.Lident "[]"; loc } in
            Ast_helper.Pat.construct ~loc nil None
        in
        base_case
    | p1 :: pl ->
        let pat_pl = handle_seq pl in
        let loc =
          mk_loc p1.Parsetree.ppat_loc.loc_start pat_pl.ppat_loc.loc_end in
        let arg = Ast_helper.Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
        Ast_helper.Pat.mk ~loc (Ppat_construct(Location.mkloc (Longident.Lident "::") loc, Some arg))
    in
    handle_seq seq


  (* {"foo": bar} -> Js.t({. foo: bar})
   * {.. "foo": bar} -> Js.t({.. foo: bar})
   * {..} -> Js.t({..}) *)
  let make_bs_obj_type ~attrs ~loc ~closed rows =
    let obj = Ast_helper.Typ.object_ ~loc rows closed in
    let js_dot_t_ctor =
      Location.mkloc (Longident.Ldot (Longident.Lident "Js", "t")) loc
    in
    Ast_helper.Typ.constr ~loc ~attrs js_dot_t_ctor [obj]

  (* TODO: diagnostic reporting *)
  let lident_of_path longident =
    match Longident.flatten longident |> List.rev with
    | [] -> ""
    | ident::_ -> ident

  let make_newtypes ~attrs ~loc newtypes exp =
    let expr = List.fold_right (fun newtype exp ->
      Ast_helper.Exp.mk ~loc (Pexp_newtype (newtype, exp))
    ) newtypes exp
    in {expr with pexp_attributes = attrs}

  (* locally abstract types syntax sugar
   * Transforms
   *  let f: type t u v. = (foo : list</t, u, v/>) => ...
   * into
   *  let f = (type t u v. foo : list</t, u, v/>) => ...
   *)
  let wrap_type_annotation ~loc newtypes core_type body =
    let exp = make_newtypes ~attrs:[] ~loc newtypes
      (Ast_helper.Exp.constraint_ ~loc body core_type)
    in
    let typ = Ast_helper.Typ.poly ~loc newtypes
      (Ast_helper.Typ.varify_constructors newtypes core_type)
    in
    (exp, typ)

  (**
    * process the occurrence of _ in the arguments of a function application
    * replace _ with a new variable, currently __x, in the arguments
    * return a wrapping function that wraps ((__x) => ...) around an expression
    * e.g. foo(_, 3) becomes (__x) => foo(__x, 3)
    *)
  let process_underscore_application args =
    let open Parsetree in
    let exp_question = ref None in
    let hidden_var = "__x" in
    let check_arg ((lab, exp) as arg) =
      match exp.pexp_desc with
      | Pexp_ident ({ txt = Lident "_"} as id) ->
        let new_id = Location.mkloc (Longident.Lident hidden_var) id.loc in
        let new_exp = Ast_helper.Exp.mk (Pexp_ident new_id) ~loc:exp.pexp_loc in
        exp_question := Some new_exp;
        (lab, new_exp)
      | _ ->
        arg
    in
    let args = List.map check_arg args in
    let wrap exp_apply =
      match !exp_question with
      | Some {pexp_loc=loc} ->
        let pattern = Ast_helper.Pat.mk (Ppat_var (Location.mkloc hidden_var loc)) ~loc in
        Ast_helper.Exp.mk (Pexp_fun (Nolabel, None, pattern, exp_apply)) ~loc
      | None ->
        exp_apply
    in
    (args, wrap)

  let rec parse_lident p =
    let recover_lident p =
      if (
        Token.is_keyword p.Parser.token &&
        p.Parser.prev_end_pos.pos_lnum == p.start_pos.pos_lnum
      )
      then (
        Parser.err p (Diagnostics.lident p.Parser.token);
        Parser.next p;
        None
      ) else (
        let rec loop p =
          if not (Recover.should_abort_list_parse p)
          then begin
            Parser.next p;
            loop p
          end
        in
        Parser.next p;
        loop p;
        match p.Parser.token with
        | Lident _ -> Some ()
        | _ -> None
      )
    in
    let start_pos = p.Parser.start_pos in
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      let loc = mk_loc start_pos p.prev_end_pos in
      (ident, loc)
    | List ->
      Parser.next p;
      let loc = mk_loc start_pos p.prev_end_pos in
      ("list", loc)
    | _ ->
      begin match recover_lident p with
      | Some () ->
        parse_lident p
      | None ->
        ("_", mk_loc start_pos p.prev_end_pos)
      end

  let parse_ident ~msg ~start_pos p =
    match p.Parser.token with
    | Lident ident
    | Uident ident ->
      Parser.next p;
      let loc = mk_loc start_pos p.prev_end_pos in
      (ident, loc)
    | List ->
      Parser.next p;
      let loc = mk_loc start_pos p.prev_end_pos in
      ("list", loc)
    | _token ->
      Parser.err p (Diagnostics.message msg);
      Parser.next p;
      ("_", mk_loc start_pos p.prev_end_pos)

  let parse_hash_ident ~start_pos p =
    Parser.expect Hash p;
    parse_ident ~start_pos ~msg:ErrorMessages.variant_ident p

  (* Ldot (Ldot (Lident "Foo", "Bar"), "baz") *)
  let parse_value_path p =
    let start_pos = p.Parser.start_pos in
    let rec aux p path =
      match p.Parser.token with
      | List -> Longident.Ldot(path, "list")
      | Lident ident -> Longident.Ldot(path, ident)
      | Uident uident ->
        Parser.next p;
        Parser.expect Dot p;
        aux p (Ldot (path, uident))
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        Longident.Lident "_"
    in
    let ident = match p.Parser.token with
    | List -> Longident.Lident "list"
    | Lident ident -> Longident.Lident ident
    | Uident ident ->
      Parser.next p;
      Parser.expect Dot p;
      aux p (Lident ident)
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Longident.Lident "_"
    in
    Parser.next p;
    Location.mkloc ident (mk_loc start_pos p.prev_end_pos)

 let parse_value_path_tail p start_pos ident =
    let rec loop p path =
      match p.Parser.token with
      | Lident ident ->
        Parser.next p;
        Location.mkloc (Longident.Ldot(path, ident)) (mk_loc start_pos p.prev_end_pos)
      | List ->
        Parser.next p;
        Location.mkloc (Longident.Ldot(path, "list")) (mk_loc start_pos p.prev_end_pos)
      | Uident ident ->
        Parser.next p;
        Parser.expect Dot p;
        loop p (Longident.Ldot (path, ident))
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        Location.mknoloc path
    in
    loop p ident

  let parse_module_long_ident_tail ~lowercase p start_pos ident =
    let rec loop p acc =
      match p.Parser.token with
      | List when lowercase ->
        Parser.next p;
        let lident = (Longident.Ldot (acc, "list")) in
        Location.mkloc lident (mk_loc start_pos p.prev_end_pos)
      | Lident ident when lowercase ->
        Parser.next p;
        let lident = (Longident.Ldot (acc, ident)) in
        Location.mkloc lident (mk_loc start_pos p.prev_end_pos)
      | Uident ident ->
        Parser.next p;
        let end_pos = p.prev_end_pos in
        let lident = (Longident.Ldot (acc, ident)) in
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          loop p lident
        | _ -> Location.mkloc lident (mk_loc start_pos end_pos)
        end
      | t ->
        Parser.err p (Diagnostics.uident t);
        Location.mkloc acc (mk_loc start_pos p.prev_end_pos)
    in
    loop p ident

  (* Parses module identifiers:
       Foo
       Foo.Bar *)
  let parse_module_long_ident ~lowercase p =
    (* Parser.leaveBreadcrumb p Reporting.ModuleLongIdent; *)
    let start_pos = p.Parser.start_pos in
    let module_ident = match p.Parser.token with
    | List when lowercase ->
      let loc = mk_loc start_pos p.end_pos in
      Parser.next p;
      Location.mkloc (Longident.Lident "list") loc
    | Lident ident when lowercase ->
      let loc = mk_loc start_pos p.end_pos in
      let lident = Longident.Lident ident in
      Parser.next p;
      Location.mkloc lident loc
    | Uident ident ->
      let lident = Longident.Lident ident in
      let end_pos = p.end_pos in
      Parser.next p;
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        parse_module_long_ident_tail ~lowercase p start_pos lident
      | _ -> Location.mkloc lident (mk_loc start_pos end_pos)
      end
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mkloc (Longident.Lident "_") (mk_loc start_pos p.prev_end_pos)
    in
    (* Parser.eatBreadcrumb p; *)
    module_ident

  (* `window.location` or `Math` or `Foo.Bar` *)
  let parse_ident_path p =
    let rec loop p acc =
      match p.Parser.token with
      | Uident ident | Lident ident ->
        Parser.next p;
        let lident = (Longident.Ldot (acc, ident)) in
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          loop p lident
        | _ -> lident
        end
      | _t -> acc
    in
    match p.Parser.token with
    | Lident ident | Uident ident ->
      Parser.next p;
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        loop p (Longident.Lident ident)
      | _ -> Longident.Lident ident
      end
    | _ ->
      Longident.Lident "_"

  let verify_jsx_opening_closing_name p name_expr =
    let closing = match p.Parser.token with
    | Lident lident -> Parser.next p; Longident.Lident lident
    | Uident _ ->
      (parse_module_long_ident ~lowercase:false p).txt
    | _ -> Longident.Lident ""
    in
    match name_expr.Parsetree.pexp_desc with
    | Pexp_ident opening_ident ->
      let opening =
        let without_create_element =
          Longident.flatten opening_ident.txt
          |> List.filter (fun s -> s <> "createElement")
        in
        match (Longident.unflatten without_create_element) with
        | Some li -> li
        | None -> Longident.Lident ""
      in
      opening = closing
    | _ -> assert false

  let string_of_pexp_ident name_expr =
    match name_expr.Parsetree.pexp_desc with
    | Pexp_ident opening_ident ->
      Longident.flatten opening_ident.txt
      |> List.filter (fun s -> s <> "createElement")
      |> String.concat "."
    | _ -> ""

  (* open-def ::=
   *   | open module-path
   *   | open! module-path *)
  let parse_open_description ~attrs p =
    Parser.leave_breadcrumb p Grammar.OpenDescription;
    let start_pos = p.Parser.start_pos in
    Parser.expect Open p;
    let override = if Parser.optional p Token.Bang then
      Asttypes.Override
    else
      Asttypes.Fresh
    in
    let modident = parse_module_long_ident ~lowercase:false p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Parser.eat_breadcrumb p;
    Ast_helper.Opn.mk ~loc ~attrs ~override modident

  let hex_value x =
    match x with
    | '0' .. '9' ->
      (Char.code x) - 48
    | 'A' .. 'Z' ->
      (Char.code x) - 55
    | 'a' .. 'z' ->
      (Char.code x) - 97
    | _ -> 16

  let parse_string_literal s =
    let len = String.length s in
    let b = Buffer.create (String.length s) in

    let rec loop i =
      if i = len then
        ()
      else
        let c = String.unsafe_get s i in
        match c with
        | '\\' as c ->
          let next_ix = i + 1 in
          if next_ix < len then
            let next_char = String.unsafe_get s next_ix in
            begin match next_char with
            | 'n' ->
              Buffer.add_char b '\010';
              loop (next_ix + 1)
            | 'r' ->
              Buffer.add_char b '\013';
              loop (next_ix + 1)
            | 'b' ->
              Buffer.add_char b '\008';
              loop (next_ix + 1)
            | 't' ->
              Buffer.add_char b '\009';
              loop (next_ix + 1)
            | '\\' as c ->
              Buffer.add_char b c;
              loop (next_ix + 1)
            | ' ' as c ->
                Buffer.add_char b c;
              loop (next_ix + 1)
            | '\'' as c ->
                Buffer.add_char b c;
              loop (next_ix + 1)
            | '\"' as c ->
              Buffer.add_char b c;
              loop (next_ix + 1)
            | '0' .. '9' ->
              if next_ix + 2 < len then
                let c0 = next_char in
                let c1 = (String.unsafe_get s (next_ix + 1)) in
                let c2 = (String.unsafe_get s (next_ix + 2)) in
                let c =
                  100 * (Char.code c0 - 48) +
                  10 * (Char.code c1  - 48) +
                  (Char.code c2 - 48)
                in
                if (c < 0 || c > 255) then (
                  Buffer.add_char b '\\';
                  Buffer.add_char b c0;
                  Buffer.add_char b c1;
                  Buffer.add_char b c2;
                  loop (next_ix + 3)
                ) else (
                  Buffer.add_char b (Char.unsafe_chr c);
                  loop (next_ix + 3)
                )
              else (
                Buffer.add_char b '\\';
                Buffer.add_char b next_char;
                loop (next_ix + 1)
              )
            | 'o' ->
              if next_ix + 3 < len then
                let c0 = (String.unsafe_get s (next_ix + 1)) in
                let c1 = (String.unsafe_get s (next_ix + 2)) in
                let c2 = (String.unsafe_get s (next_ix + 3)) in
                let c =
                  64 * (Char.code c0 - 48) +
                  8 * (Char.code c1  - 48) +
                  (Char.code c2 - 48)
                in
                if (c < 0 || c > 255) then (
                  Buffer.add_char b '\\';
                  Buffer.add_char b '0';
                  Buffer.add_char b c0;
                  Buffer.add_char b c1;
                  Buffer.add_char b c2;
                  loop (next_ix + 4)
                ) else (
                  Buffer.add_char b (Char.unsafe_chr c);
                  loop (next_ix + 4)
                )
              else (
                Buffer.add_char b '\\';
                Buffer.add_char b next_char;
                loop (next_ix + 1)
              )
            | 'x' as c ->
              if next_ix + 2 < len then
                let c0 = (String.unsafe_get s (next_ix + 1)) in
                let c1 = (String.unsafe_get s (next_ix + 2)) in
                let c = (16 * (hex_value c0)) + (hex_value c1) in
                if (c < 0 || c > 255) then (
                  Buffer.add_char b '\\';
                  Buffer.add_char b 'x';
                  Buffer.add_char b c0;
                  Buffer.add_char b c1;
                  loop (next_ix + 3)
                ) else (
                  Buffer.add_char b (Char.unsafe_chr c);
                  loop (next_ix + 3)
                )
              else (
                Buffer.add_char b '\\';
                Buffer.add_char b c;
                loop (next_ix + 2)
              )
            | _ ->
              Buffer.add_char b c;
              Buffer.add_char b next_char;
              loop (next_ix + 1)
            end
          else (
            Buffer.add_char b c;
            ()
          )
        | c ->
          Buffer.add_char b c;
          loop (i + 1)
      in
      loop 0;
      Buffer.contents b

  let parse_template_string_literal s =
    let len = String.length s in
    let b = Buffer.create len in

    let rec loop i =
      if i < len then
        let c = String.unsafe_get s i in
        match c with
        | '\\' as c ->
          if i + 1 < len then
            let next_char = String.unsafe_get s (i + 1) in
            begin match next_char with
            | '\\' as c ->
              Buffer.add_char b c;
              loop (i + 2)
            | '$' as c ->
              Buffer.add_char b c;
              loop (i + 2)
            | '`' as c ->
              Buffer.add_char b c;
              loop (i + 2)
            | c ->
              Buffer.add_char b '\\';
              Buffer.add_char b c;
              loop (i + 2)
            end
          else (
            Buffer.add_char b c
          )

        | c ->
          Buffer.add_char b c;
          loop (i + 1)

      else
        ()
    in
    loop 0;
    Buffer.contents b

  (* constant	::=	integer-literal   *)
   (* ∣	 float-literal   *)
   (* ∣	 string-literal   *)
  let parse_constant p =
    let is_negative = match p.Parser.token with
    | Token.Minus -> Parser.next p; true
    | Plus -> Parser.next p; false
    | _ -> false
    in
    let constant = match p.Parser.token with
    | Int {i; suffix} ->
      let int_txt = if is_negative then "-" ^ i else i in
      Parsetree.Pconst_integer (int_txt, suffix)
    | Float {f; suffix} ->
      let float_txt = if is_negative then "-" ^ f else f in
      Parsetree.Pconst_float (float_txt, suffix)
    | String s ->
      let txt = if p.mode = ParseForTypeChecker then
        parse_string_literal s
      else
        s
      in
      Pconst_string(txt, None)
    | Character c -> Pconst_char c
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Pconst_string("", None)
    in
    Parser.next p;
    constant

  let parse_comma_delimited_region p ~grammar ~closing ~f =
    Parser.leave_breadcrumb p grammar;
    let rec loop nodes =
      match f p with
      | Some node ->
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop (node::nodes)
        | token when token = closing || token = Eof ->
          List.rev (node::nodes)
        | _ ->
          if not (p.token = Eof || p.token = closing || Recover.should_abort_list_parse p) then
            Parser.expect Comma p;
          if p.token = Semicolon then Parser.next p;
          loop (node::nodes)
        end
      | None ->
        if p.token = Eof || p.token = closing || Recover.should_abort_list_parse p then
          List.rev nodes
        else (
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        );
    in
    let nodes = loop [] in
    Parser.eat_breadcrumb p;
    nodes

  let parse_comma_delimited_reversed_list p ~grammar ~closing ~f =
    Parser.leave_breadcrumb p grammar;
    let rec loop nodes =
      match f p with
      | Some node ->
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop (node::nodes)
        | token when token = closing || token = Eof ->
          (node::nodes)
        | _ ->
          if not (p.token = Eof || p.token = closing || Recover.should_abort_list_parse p) then
            Parser.expect Comma p;
          if p.token = Semicolon then Parser.next p;
          loop (node::nodes)
        end
      | None ->
        if p.token = Eof || p.token = closing || Recover.should_abort_list_parse p then
          nodes
        else (
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        );
    in
    let nodes = loop [] in
    Parser.eat_breadcrumb p;
    nodes

  let parse_delimited_region p ~grammar ~closing ~f =
    Parser.leave_breadcrumb p grammar;
    let rec loop nodes =
      match f p with
      | Some node ->
        loop (node::nodes)
      | None ->
        if (
          p.Parser.token = Token.Eof ||
          p.token = closing ||
          Recover.should_abort_list_parse p
        ) then
          List.rev nodes
        else (
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        )
      in
      let nodes = loop [] in
      Parser.eat_breadcrumb p;
      nodes

  let parse_region p ~grammar ~f =
    Parser.leave_breadcrumb p grammar;
    let rec loop nodes =
      match f p with
      | Some node ->
        loop (node::nodes)
      | None ->
        if p.Parser.token = Token.Eof || Recover.should_abort_list_parse p then
          List.rev nodes
        else (
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        )
    in
    let nodes = loop [] in
    Parser.eat_breadcrumb p;
    nodes

  (* let-binding	::=	pattern =  expr   *)
     (* ∣	 value-name  { parameter }  [: typexpr]  [:> typexpr] =  expr   *)
     (* ∣	 value-name :  poly-typexpr =  expr   *)

   (* pattern	::=	value-name   *)
     (* ∣	 _   *)
     (* ∣	 constant   *)
     (* ∣	 pattern as  value-name   *)
     (* ∣	 ( pattern )   *)
     (* ∣	 ( pattern :  typexpr )   *)
     (* ∣	 pattern |  pattern   *)
     (* ∣	 constr  pattern   *)
     (* ∣	 #variant variant-pattern *)
     (* ∣	 ##type  *)
     (* ∣	 / pattern  { , pattern }+  /   *)
     (* ∣	 { field  [: typexpr]  [= pattern] { ; field  [: typexpr]  [= pattern] }  [; _ ] [ ; ] }   *)
     (* ∣	 [ pattern  { ; pattern }  [ ; ] ]   *)
     (* ∣	 pattern ::  pattern   *)
     (* ∣	 [| pattern  { ; pattern }  [ ; ] |]   *)
     (* ∣	 char-literal ..  char-literal *)
     (*	∣	 exception pattern  *)
  let rec parse_pattern ?(alias=true) ?(or_=true) p =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes p in
    let pat = match p.Parser.token with
    | (True | False) as token ->
      let end_pos = p.end_pos in
      Parser.next p;
      let loc = mk_loc start_pos end_pos in
      Ast_helper.Pat.construct ~loc
        (Location.mkloc (Longident.Lident (Token.to_string token)) loc) None
    | Int _ | String _ | Float _ | Character _ | Minus | Plus ->
      let c = parse_constant p in
       begin match p.token with
        | DotDot ->
          Parser.next p;
          let c2 = parse_constant p in
          Ast_helper.Pat.interval ~loc:(mk_loc start_pos p.prev_end_pos) c c2
        | _ ->
          Ast_helper.Pat.constant ~loc:(mk_loc start_pos p.prev_end_pos) c
      end
    | Lparen ->
      Parser.next p;
      begin match p.token with
      | Rparen ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let lid = Location.mkloc (Longident.Lident "()") loc in
        Ast_helper.Pat.construct ~loc lid None
      | _ ->
        let pat = parse_constrained_pattern p in
        begin match p.token with
        | Comma ->
          Parser.next p;
          parse_tuple_pattern ~attrs ~first:pat ~start_pos p
        | _ ->
          Parser.expect Rparen p;
          let loc = mk_loc start_pos p.prev_end_pos in
          {pat with ppat_loc = loc}
        end
      end
    | Lbracket ->
      parse_array_pattern ~attrs p
    | Lbrace ->
      parse_record_pattern ~attrs p
    | Underscore ->
      let end_pos = p.end_pos in
      let loc = mk_loc start_pos end_pos in
      Parser.next p;
      Ast_helper.Pat.any ~loc ~attrs ()
    | Lident ident ->
      let end_pos = p.end_pos in
      let loc = mk_loc start_pos end_pos in
      Parser.next p;
      Ast_helper.Pat.var ~loc ~attrs (Location.mkloc ident loc)
    | Uident _ ->
      let constr = parse_module_long_ident ~lowercase:false p in
      begin match p.Parser.token with
      | Lparen ->
        parse_constructor_pattern_args p constr start_pos attrs
      | _ ->
        Ast_helper.Pat.construct ~loc:constr.loc ~attrs constr None
      end
    | Hash ->
      let (ident, loc) = parse_hash_ident ~start_pos p in
      begin match p.Parser.token with
      | Lparen ->
        parse_variant_pattern_args p ident start_pos attrs
      | _ ->
        Ast_helper.Pat.variant ~loc ~attrs ident None
      end
    | HashHash ->
      Parser.next p;
      let ident = parse_value_path p in
      let loc = mk_loc start_pos ident.loc.loc_end in
      Ast_helper.Pat.type_ ~loc ~attrs ident
    | Exception ->
      Parser.next p;
      let pat = parse_pattern ~alias:false ~or_:false p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Pat.exception_ ~loc ~attrs pat
    | Lazy ->
      Parser.next p;
      let pat = parse_pattern ~alias:false ~or_:false p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Pat.lazy_ ~loc ~attrs pat
    | List ->
      Parser.next p;
      begin match p.token with
      | Lbracket ->
        parse_list_pattern ~start_pos ~attrs p
      | _ ->
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Pat.var ~loc ~attrs (Location.mkloc "list" loc)
      end
    | Module ->
      parse_module_pattern ~attrs p
    | Percent ->
      let extension = parse_extension p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Pat.extension ~loc ~attrs extension
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      begin match skip_tokens_and_maybe_retry p ~is_start_of_grammar:Grammar.is_atomic_pattern_start with
      | None ->
        Recover.default_pattern()
      | Some () ->
        parse_pattern p
      end
    in
    let pat = if alias then parse_alias_pattern ~attrs pat p else pat in
    if or_ then parse_or_pattern pat p else pat

  and skip_tokens_and_maybe_retry p ~is_start_of_grammar =
    if Token.is_keyword p.Parser.token
        && p.Parser.prev_end_pos.pos_lnum == p.start_pos.pos_lnum
    then (
      Parser.next p;
      None
    ) else (
      if Recover.should_abort_list_parse p then
        begin
          if is_start_of_grammar p.Parser.token then
            begin
              Parser.next p;
              Some ()
            end
          else
            None
        end
      else
        begin
          Parser.next p;
          let rec loop p =
            if not (Recover.should_abort_list_parse p)
            then begin
              Parser.next p;
              loop p
            end in
          loop p;
          if is_start_of_grammar p.Parser.token then
            Some ()
          else
            None
        end
    )

  (* alias ::= pattern as lident *)
  and parse_alias_pattern ~attrs pattern p =
    match p.Parser.token with
    | As ->
      Parser.next p;
      let (name, loc) = parse_lident p in
      let name = Location.mkloc name loc in
      Ast_helper.Pat.alias
        ~loc:({pattern.ppat_loc with loc_end = p.prev_end_pos})
        ~attrs
         pattern
         name
    | _ -> pattern

  (* or ::= pattern | pattern
   * precedence: Red | Blue | Green is interpreted as (Red | Blue) | Green *)
  and parse_or_pattern pattern1 p =
    let rec loop pattern1 =
      match p.Parser.token with
      | Bar ->
        Parser.next p;
        let pattern2 = parse_pattern ~or_:false p in
        let loc = { pattern1.Parsetree.ppat_loc with
          loc_end = pattern2.ppat_loc.loc_end
        } in
        loop (Ast_helper.Pat.or_ ~loc pattern1 pattern2)
      | _ -> pattern1
    in
    loop pattern1

  and parse_non_spread_pattern ~msg p =
    let () = match p.Parser.token with
    | DotDotDot ->
      Parser.err p (Diagnostics.message msg);
      Parser.next p;
    | _ -> ()
    in
    match p.Parser.token with
    | token when Grammar.is_pattern_start token ->
      let pat = parse_pattern p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let typ = parse_typ_expr p in
        let loc = mk_loc pat.ppat_loc.loc_start typ.Parsetree.ptyp_loc.loc_end in
        Some (Ast_helper.Pat.constraint_ ~loc pat typ)
      | _ -> Some pat
      end
    | _ -> None

  and parse_constrained_pattern p =
    let pat = parse_pattern p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let typ = parse_typ_expr p in
      let loc = mk_loc pat.ppat_loc.loc_start typ.Parsetree.ptyp_loc.loc_end in
      Ast_helper.Pat.constraint_ ~loc pat typ
    | _ -> pat

  and parse_constrained_pattern_region p =
    match p.Parser.token with
    | token when Grammar.is_pattern_start token ->
      Some (parse_constrained_pattern p)
    | _ -> None

	(* field ::=
	 *   | longident
	 *   | longident : pattern
	 *   | longident as lident
   *
	 *  row ::=
	 *	 | field ,
	 *	 | field , _
	 *	 | field , _,
	 *)
  and parse_record_pattern_field p =
    let start_pos = p.Parser.start_pos in
    let label = parse_value_path p in
    let pattern = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parse_pattern p
    | _ ->
      Ast_helper.Pat.var
        ~loc:label.loc
        (Location.mkloc (Longident.last label.txt) label.loc)
    in
		match p.token with
		| As ->
			Parser.next p;
      let (name, loc) = parse_lident p in
      let name = Location.mkloc name loc in
      let alias_pattern = Ast_helper.Pat.alias
        ~loc:(mk_loc start_pos p.prev_end_pos)
        pattern
        name
      in
      (Location.mkloc label.txt (mk_loc start_pos alias_pattern.ppat_loc.loc_end), alias_pattern)
		| _ ->
      (label, pattern)

   (* TODO: there are better representations than PatField|Underscore ? *)
  and parse_record_pattern_item p =
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      Some (true, PatField (parse_record_pattern_field p))
    | Uident _ | Lident _ ->
      Some (false, PatField (parse_record_pattern_field p))
    | Underscore ->
      Parser.next p;
      Some (false, PatUnderscore)
    | _ ->
      None

  and parse_record_pattern ~attrs p =
    let start_pos = p.start_pos in
    Parser.expect Lbrace p;
    let raw_fields =
      parse_comma_delimited_reversed_list p
       ~grammar:PatternRecord
       ~closing:Rbrace
       ~f:parse_record_pattern_item
    in
    Parser.expect Rbrace p;
    let (fields, closed_flag) =
      let (raw_fields, flag) = match raw_fields with
      | (_hasSpread, PatUnderscore)::rest ->
        (rest, Asttypes.Open)
      | raw_fields ->
        (raw_fields, Asttypes.Closed)
      in
      List.fold_left (fun (fields, flag) curr ->
        let (has_spread, field) = curr in
        match field with
        | PatField field ->
          if has_spread then (
            let (_, pattern) = field in
            Parser.err ~start_pos:pattern.Parsetree.ppat_loc.loc_start p (Diagnostics.message ErrorMessages.record_pattern_spread)
          );
          (field::fields, flag)
        | PatUnderscore ->
          (fields, flag)
      ) ([], flag) raw_fields
    in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Pat.record ~loc ~attrs fields closed_flag

  and parse_tuple_pattern ~attrs ~first ~start_pos p =
    let patterns =
      parse_comma_delimited_region p
        ~grammar:Grammar.PatternList
        ~closing:Rparen
        ~f:parse_constrained_pattern_region
    in
    Parser.expect Rparen p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Pat.tuple ~loc ~attrs (first::patterns)

  and parse_pattern_region p =
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      Some (true, parse_constrained_pattern p)
    | token when Grammar.is_pattern_start token ->
      Some (false, parse_constrained_pattern p)
    | _ -> None

  and parse_module_pattern ~attrs p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Module p;
    Parser.expect Lparen p;
    let uident = match p.token with
    | Uident uident ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc uident loc
    | _ -> (* TODO: error recovery *)
      Location.mknoloc "_"
    in
    begin match p.token with
    | Colon ->
      let colon_start = p.Parser.start_pos in
      Parser.next p;
      let package_typ_attrs = parse_attributes p in
      let package_type = parse_package_type ~start_pos:colon_start ~attrs:package_typ_attrs p in
      Parser.expect Rparen p;
      let loc = mk_loc start_pos p.prev_end_pos in
      let unpack = Ast_helper.Pat.unpack ~loc:uident.loc uident in
      Ast_helper.Pat.constraint_
        ~loc
        ~attrs
        unpack
        package_type
    | _ ->
      Parser.expect Rparen p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Pat.unpack ~loc ~attrs uident
    end

  and parse_list_pattern ~start_pos ~attrs p =
    Parser.expect Lbracket p;
    let list_patterns =
      parse_comma_delimited_reversed_list p
        ~grammar:Grammar.PatternOcamlList
        ~closing:Rbracket
        ~f:parse_pattern_region
    in
    Parser.expect Rbracket p;
    let loc = mk_loc start_pos p.prev_end_pos in
    let filter_spread (has_spread, pattern) =
      if has_spread then (
        Parser.err
          ~start_pos:pattern.Parsetree.ppat_loc.loc_start
          p
          (Diagnostics.message ErrorMessages.list_pattern_spread);
        pattern
      ) else
        pattern
    in
    match list_patterns with
    | (true, pattern)::patterns ->
      let patterns = patterns |> List.map filter_spread |> List.rev in
      let pat = make_list_pattern loc patterns (Some pattern) in
      {pat with ppat_loc = loc; ppat_attributes = attrs;}
    | patterns ->
      let patterns = patterns |> List.map filter_spread |> List.rev in
      let pat = make_list_pattern loc patterns None in
      {pat with ppat_loc = loc; ppat_attributes = attrs;}

  and parse_array_pattern ~attrs p =
    let start_pos = p.start_pos in
    Parser.expect Lbracket p;
    let patterns =
      parse_comma_delimited_region
        p
        ~grammar:Grammar.PatternList
        ~closing:Rbracket
        ~f:(parse_non_spread_pattern ~msg:ErrorMessages.array_pattern_spread)
    in
    Parser.expect Rbracket p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Pat.array ~loc ~attrs patterns

  and parse_constructor_pattern_args p constr start_pos attrs =
    let lparen = p.start_pos in
    Parser.expect Lparen p;
    let args = parse_comma_delimited_region
      p ~grammar:Grammar.PatternList ~closing:Rparen ~f:parse_constrained_pattern_region
    in
    Parser.expect Rparen p;
    let args = match args with
    | [] ->
      let loc = mk_loc lparen p.prev_end_pos in
      Some (
        Ast_helper.Pat.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
      )
    | [{ppat_desc = Ppat_tuple _} as pat] as patterns ->
      if p.mode = ParseForTypeChecker then
        (* Some(1, 2) for type-checker *)
        Some pat
      else
      (* Some((1, 2)) for printer *)
        Some (Ast_helper.Pat.tuple ~loc:(mk_loc lparen p.end_pos) patterns)
    | [pattern] -> Some pattern
    | patterns ->
      Some (Ast_helper.Pat.tuple ~loc:(mk_loc lparen p.end_pos) patterns)
    in
    Ast_helper.Pat.construct ~loc:(mk_loc start_pos p.prev_end_pos) ~attrs constr args

  and parse_variant_pattern_args p ident start_pos attrs =
    let lparen = p.start_pos in
    Parser.expect Lparen p;
    let patterns =
      parse_comma_delimited_region
        p ~grammar:Grammar.PatternList ~closing:Rparen ~f:parse_constrained_pattern_region in
    let args =
      match patterns with
      | [{ppat_desc = Ppat_tuple _} as pat] as patterns ->
        if p.mode = ParseForTypeChecker then
          (* #ident(1, 2) for type-checker *)
          Some pat
        else
          (* #ident((1, 2)) for printer *)
          Some (Ast_helper.Pat.tuple ~loc:(mk_loc lparen p.end_pos) patterns)
      | [pattern] -> Some pattern
      | patterns ->
        Some (Ast_helper.Pat.tuple ~loc:(mk_loc lparen p.end_pos) patterns)
    in
    Parser.expect Rparen p;
    Ast_helper.Pat.variant ~loc:(mk_loc start_pos p.prev_end_pos) ~attrs ident args

  and parse_expr ?(context=OrdinaryExpr) p =
    let expr = parse_operand_expr ~context p in
    let expr = parse_binary_expr ~context ~a:expr p 1 in
    parse_ternary_expr expr p

  (* expr ? expr : expr *)
  and parse_ternary_expr left_operand p =
    match p.Parser.token with
    | Question ->
      Parser.leave_breadcrumb p Grammar.Ternary;
      Parser.next p;
      let true_branch = parse_expr ~context:TernaryTrueBranchExpr p in
      Parser.expect Colon p;
      let false_branch = parse_expr p in
      Parser.eat_breadcrumb p;
      let loc = {left_operand.Parsetree.pexp_loc with
        loc_start = left_operand.pexp_loc.loc_start;
        loc_end = false_branch.Parsetree.pexp_loc.loc_end;
      } in
      Ast_helper.Exp.ifthenelse
        ~attrs:[ternary_attr] ~loc
        left_operand true_branch (Some false_branch)
    | _ ->
      left_operand

  and parse_es6_arrow_expression ?parameters p =
    let start_pos = p.Parser.start_pos in
    Parser.leave_breadcrumb p Grammar.Es6ArrowExpr;
    let parameters = match parameters with
    | Some params -> params
    | None -> parse_parameters p
    in
    let return_type = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parse_typ_expr ~es6_arrow:false p)
    | _ ->
      None
    in
    Parser.expect EqualGreater p;
    let body =
      let expr = parse_expr p in
      match return_type with
      | Some typ ->
        Ast_helper.Exp.constraint_
          ~loc:(mk_loc expr.pexp_loc.loc_start typ.Parsetree.ptyp_loc.loc_end) expr typ
      | None -> expr
    in
    Parser.eat_breadcrumb p;
    let end_pos = p.prev_end_pos in
    let arrow_expr =
      List.fold_right (fun parameter expr ->
        match parameter with
        | TermParameter {uncurried; attrs; label = lbl; expr = default_expr; pat; pos = start_pos} ->
          let attrs = if uncurried then uncurry_attr::attrs else attrs in
          Ast_helper.Exp.fun_ ~loc:(mk_loc start_pos end_pos) ~attrs lbl default_expr pat expr
        | TypeParameter {uncurried; attrs; locs = newtypes; pos = start_pos} ->
          let attrs = if uncurried then uncurry_attr::attrs else attrs in
          make_newtypes ~attrs ~loc:(mk_loc start_pos end_pos) newtypes expr
      ) parameters body
    in
    {arrow_expr with pexp_loc = {arrow_expr.pexp_loc with loc_start = start_pos}}

	(*
   * uncurried_parameter ::=
   *   | . parameter
   *
	 * parameter ::=
	 *   | pattern
   *   | pattern : type
	 *   | ~ labelName
	 *   | ~ labelName as pattern
	 *   | ~ labelName as pattern : type
	 *   | ~ labelName = expr
	 *   | ~ labelName as pattern = expr
	 *   | ~ labelName as pattern : type = expr
	 *   | ~ labelName = ?
	 *   | ~ labelName as pattern = ?
	 *   | ~ labelName as pattern : type = ?
   *
	 * labelName ::= lident
   *)
  and parse_parameter p =
    if (
      p.Parser.token = Token.Typ ||
      p.token = Tilde ||
      p.token = Dot ||
      Grammar.is_pattern_start p.token
    ) then (
      let start_pos = p.Parser.start_pos in
      let uncurried = Parser.optional p Token.Dot in
      (* two scenarios:
       *   attrs ~lbl ...
       *   attrs pattern
       * Attributes before a labelled arg, indicate that it's on the whole arrow expr
       * Otherwise it's part of the pattern
       *  *)
      let attrs = parse_attributes p in
      if p.Parser.token = Typ then (
        Parser.next p;
        let lidents = parse_lident_list p in
        Some (TypeParameter {uncurried; attrs; locs = lidents; pos = start_pos})
      ) else (
      let (attrs, lbl, pat) = match p.Parser.token with
      | Tilde ->
        Parser.next p;
        let (lbl_name, loc) = parse_lident p in
        let prop_loc_attr = (Location.mkloc "res.namedArgLoc" loc, Parsetree.PStr []) in
        begin match p.Parser.token with
        | Comma | Equal | Rparen ->
          let loc = mk_loc start_pos p.prev_end_pos in
          (
            attrs,
            Asttypes.Labelled lbl_name,
            Ast_helper.Pat.var ~attrs:[prop_loc_attr] ~loc (Location.mkloc lbl_name loc)
          )
        | Colon ->
          let lbl_end = p.prev_end_pos in
          Parser.next p;
          let typ = parse_typ_expr p in
          let loc = mk_loc start_pos lbl_end in
          let pat =
            let pat = Ast_helper.Pat.var ~loc (Location.mkloc lbl_name loc) in
            let loc = mk_loc start_pos p.prev_end_pos in
            Ast_helper.Pat.constraint_ ~attrs:[prop_loc_attr] ~loc pat typ in
          (attrs, Asttypes.Labelled lbl_name, pat)
        | As ->
          Parser.next p;
          let pat =
            let pat = parse_constrained_pattern p in
            {pat with ppat_attributes = prop_loc_attr::pat.ppat_attributes}
          in
          (attrs, Asttypes.Labelled lbl_name, pat)
        | t ->
          Parser.err p (Diagnostics.unexpected t p.breadcrumbs);
          let loc = mk_loc start_pos p.prev_end_pos in
          (
            attrs,
            Asttypes.Labelled lbl_name,
            Ast_helper.Pat.var ~loc (Location.mkloc lbl_name loc)
          )
        end
      | _ ->
        let pattern = parse_constrained_pattern p in
        let attrs = List.concat [attrs; pattern.ppat_attributes] in
        ([], Asttypes.Nolabel, {pattern with ppat_attributes = attrs})
      in
      match p.Parser.token with
      | Equal ->
        Parser.next p;
        let lbl = match lbl with
        | Asttypes.Labelled lbl_name -> Asttypes.Optional lbl_name
        | Asttypes.Optional _ as lbl -> lbl
        | Asttypes.Nolabel -> Asttypes.Nolabel
        in
        begin match p.Parser.token with
        | Question ->
          Parser.next p;
          Some (TermParameter {uncurried; attrs; label = lbl; expr = None; pat; pos = start_pos})
        | _ ->
          let expr = parse_constrained_or_coerced_expr p in
          Some (TermParameter {uncurried; attrs; label = lbl; expr = Some expr; pat; pos = start_pos})
        end
      | _ ->
        Some (TermParameter {uncurried; attrs; label = lbl; expr = None; pat; pos = start_pos})
    )
    ) else None

  and parse_parameter_list p =
    let parameters =
      parse_comma_delimited_region
        ~grammar:Grammar.ParameterList
        ~f:parse_parameter
        ~closing:Rparen
        p
    in
    Parser.expect Rparen p;
    parameters

  (* parameters ::=
   *   | _
   *   | lident
   *   | ()
   *   | (.)
   *   | ( parameter {, parameter} [,] )
   *)
  and parse_parameters p =
    let start_pos = p.Parser.start_pos in
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      let loc = mk_loc start_pos p.Parser.prev_end_pos in
      [TermParameter {
        uncurried = false;
        attrs = [];
        label = Asttypes.Nolabel;
        expr = None;
        pat = Ast_helper.Pat.var ~loc (Location.mkloc ident loc);
        pos = start_pos;
      }]
    | List ->
      Parser.next p;
      let loc = mk_loc start_pos p.Parser.prev_end_pos in
      [TermParameter {
        uncurried = false;
        attrs = [];
        label = Asttypes.Nolabel;
        expr = None;
        pat = Ast_helper.Pat.var ~loc (Location.mkloc "list" loc);
        pos = start_pos;
      }]
    | Underscore ->
      Parser.next p;
      let loc = mk_loc start_pos p.Parser.prev_end_pos in
      [TermParameter {uncurried = false; attrs = []; label = Asttypes.Nolabel; expr = None; pat = Ast_helper.Pat.any ~loc (); pos = start_pos}]
    | Lparen ->
      Parser.next p;
      begin match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let loc = mk_loc start_pos p.Parser.prev_end_pos in
        let unit_pattern = Ast_helper.Pat.construct
          ~loc (Location.mkloc (Longident.Lident "()") loc) None
        in
        [TermParameter {uncurried = false; attrs = []; label = Asttypes.Nolabel; expr = None; pat = unit_pattern; pos = start_pos}]
      | Dot ->
        Parser.next p;
        begin match p.token with
        | Rparen ->
          Parser.next p;
          let loc = mk_loc start_pos p.Parser.prev_end_pos in
          let unit_pattern = Ast_helper.Pat.construct
            ~loc (Location.mkloc (Longident.Lident "()") loc) None
          in
          [TermParameter {uncurried = true; attrs = []; label = Asttypes.Nolabel; expr = None; pat = unit_pattern; pos = start_pos}]
        | _ ->
          begin match parse_parameter_list p with
          | (TermParameter {attrs; label = lbl; expr = default_expr; pat = pattern; pos = start_pos})::rest ->
            (TermParameter {uncurried = true; attrs; label = lbl; expr = default_expr; pat = pattern; pos = start_pos})::rest
          | parameters -> parameters
          end
        end
      | _ -> parse_parameter_list p
      end
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      []

  and parse_coerced_expr ~(expr: Parsetree.expression) p =
    Parser.expect ColonGreaterThan p;
    let typ = parse_typ_expr p in
    let loc = mk_loc expr.pexp_loc.loc_start p.prev_end_pos in
    Ast_helper.Exp.coerce ~loc expr None typ

  and parse_constrained_or_coerced_expr p =
    let expr = parse_expr p in
    match p.Parser.token with
    | ColonGreaterThan ->
      parse_coerced_expr ~expr p
    | Colon ->
      Parser.next p;
      begin match p.token with
      | _ ->
        let typ = parse_typ_expr p in
        let loc = mk_loc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
        let expr = Ast_helper.Exp.constraint_ ~loc expr typ in
        begin match p.token with
          | ColonGreaterThan ->
            parse_coerced_expr ~expr p
          | _ ->
            expr
        end
      end
    | _ -> expr


  and parse_constrained_expr_region p =
    match p.Parser.token with
    | token when Grammar.is_expr_start token ->
      let expr = parse_expr p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let typ = parse_typ_expr p in
        let loc = mk_loc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
        Some (Ast_helper.Exp.constraint_ ~loc expr typ)
      | _ -> Some expr
      end
    | _ -> None

  (* Atomic expressions represent unambiguous expressions.
   * This means that regardless of the context, these expressions
   * are always interpreted correctly. *)
  and parse_atomic_expr p =
    Parser.leave_breadcrumb p Grammar.ExprOperand;
    let start_pos = p.Parser.start_pos in
    let expr = match p.Parser.token with
      | (True | False) as token ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Exp.construct ~loc
          (Location.mkloc (Longident.Lident (Token.to_string token)) loc) None
      | Int _ | String _ | Float _ | Character _ ->
        let c = parse_constant p in
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Exp.constant ~loc c
      | Backtick ->
        let expr = parse_template_expr p in
        {expr with pexp_loc = mk_loc start_pos p.prev_end_pos}
      | Uident _ | Lident _ ->
        parse_value_or_constructor p
      | Hash ->
        parse_poly_variant_expr p
      | Lparen ->
        Parser.next p;
        begin match p.Parser.token with
        | Rparen ->
          Parser.next p;
          let loc = mk_loc start_pos p.prev_end_pos in
          Ast_helper.Exp.construct
            ~loc (Location.mkloc (Longident.Lident "()") loc) None
        | _t ->
          let expr = parse_constrained_or_coerced_expr p in
          begin match p.token with
          | Comma ->
            Parser.next p;
            parse_tuple_expr ~start_pos ~first:expr p
          | _ ->
            Parser.expect Rparen p;
            expr
            (* {expr with pexp_loc = mkLoc startPos p.prevEndPos}
             * What does this location mean here? It means that when there's
             * a parenthesized we keep the location here for whitespace interleaving.
             * Without the closing paren in the location there will always be an extra
             * line. For now we don't include it, because it does weird things
             * with for comments. *)
          end
        end
      | List ->
        Parser.next p;
        begin match p.token with
        | Lbracket ->
          parse_list_expr ~start_pos  p
        | _ ->
          let loc = mk_loc start_pos p.prev_end_pos in
          Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident "list") loc)
        end
      | Module ->
        Parser.next p;
        parse_first_class_module_expr ~start_pos p
      | Lbracket ->
        parse_array_exp p
      | Lbrace ->
        parse_braced_or_record_expr  p
      | LessThan ->
        parse_jsx p
      | Percent ->
        let extension = parse_extension p in
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Exp.extension ~loc extension
      | Underscore as token ->
        (* This case is for error recovery. Not sure if it's the correct place *)
        Parser.err p (Diagnostics.lident token);
        Parser.next p;
        Recover.default_expr ()
      | token ->
        let err_pos = p.prev_end_pos in
        begin match skip_tokens_and_maybe_retry p ~is_start_of_grammar:Grammar.is_atomic_expr_start with
        | None ->
          Parser.err ~start_pos:err_pos p (Diagnostics.unexpected token p.breadcrumbs);
          Recover.default_expr ()
        | Some () -> parse_atomic_expr p
        end
    in
    Parser.eat_breadcrumb p;
    expr

  (* module(module-expr)
   * module(module-expr : package-type) *)
  and parse_first_class_module_expr ~start_pos p =
    Parser.expect Lparen p;

    let mod_expr = parse_module_expr p in
    let mod_end_loc = p.prev_end_pos in
    begin match p.Parser.token with
    | Colon ->
      let colon_start = p.Parser.start_pos in
      Parser.next p;
      let attrs = parse_attributes p in
      let package_type = parse_package_type ~start_pos:colon_start ~attrs p in
      Parser.expect Rparen p;
      let loc = mk_loc start_pos mod_end_loc in
      let first_class_module = Ast_helper.Exp.pack ~loc mod_expr in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.constraint_ ~loc first_class_module package_type
    | _ ->
      Parser.expect Rparen p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.pack ~loc mod_expr
    end

  and parse_bracket_access p expr start_pos =
    Parser.leave_breadcrumb p Grammar.ExprArrayAccess;
    let lbracket = p.start_pos in
    Parser.next p;
    let string_start = p.start_pos in
    match p.Parser.token with
    | String s ->
      Parser.next p;
      let string_end = p.prev_end_pos in
      Parser.expect Rbracket p;
      let rbracket = p.prev_end_pos in
      let e =
        let ident_loc = mk_loc string_start string_end in
        let loc = mk_loc lbracket rbracket in
        Ast_helper.Exp.apply ~loc
        (Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident "##") loc))
        [Nolabel, expr; Nolabel, (Ast_helper.Exp.ident ~loc:ident_loc (Location.mkloc (Longident.Lident s) ident_loc))]
      in
      let e = parse_primary_expr ~operand:e p in
      let equal_start = p.start_pos in
      begin match p.token with
      | Equal ->
        Parser.next p;
        let equal_end = p.prev_end_pos in
        let rhs_expr = parse_expr p in
        let loc = mk_loc start_pos rhs_expr.pexp_loc.loc_end in
        let operator_loc = mk_loc equal_start equal_end in
        Ast_helper.Exp.apply ~loc
          (Ast_helper.Exp.ident ~loc:operator_loc (Location.mkloc (Longident.Lident "#=") operator_loc))
          [Nolabel, e; Nolabel, rhs_expr]
      | _ -> e
      end
    | _ ->
      let access_expr = parse_constrained_or_coerced_expr p in
      Parser.expect Rbracket p;
      let rbracket = p.prev_end_pos in
      let array_loc = mk_loc lbracket rbracket in
      begin match p.token with
      | Equal ->
        Parser.leave_breadcrumb p ExprArrayMutation;
        Parser.next p;
        let rhs_expr = parse_expr p in
        let array_set = Location.mkloc
          (Longident.Ldot(Lident "Array", "set"))
          array_loc
        in
        let end_pos = p.prev_end_pos in
        let array_set = Ast_helper.Exp.apply
          ~loc:(mk_loc start_pos end_pos)
          (Ast_helper.Exp.ident ~loc:array_loc array_set)
          [Nolabel, expr; Nolabel, access_expr; Nolabel, rhs_expr]
        in
        Parser.eat_breadcrumb p;
        array_set
      | _ ->
        let end_pos = p.prev_end_pos in
        let e =
          Ast_helper.Exp.apply
            ~loc:(mk_loc start_pos end_pos)
            (Ast_helper.Exp.ident
              ~loc:array_loc
              (Location.mkloc (Longident.Ldot(Lident "Array", "get")) array_loc)
              )
            [Nolabel, expr; Nolabel, access_expr]
        in
        Parser.eat_breadcrumb p;
        parse_primary_expr ~operand:e p
      end

  (* * A primary expression represents
   *  - atomic-expr
   *  - john.age
   *  - array[0]
   *  - applyFunctionTo(arg1, arg2)
   *
   *  The "operand" represents the expression that is operated on
   *)
  and parse_primary_expr ~operand ?(no_call=false) p =
    let start_pos = operand.pexp_loc.loc_start in
    let rec loop p expr =
      match p.Parser.token with
      | Dot ->
        Parser.next p;
        let lident = parse_value_path p in
        begin match p.Parser.token with
        | Equal when no_call = false ->
          Parser.leave_breadcrumb p Grammar.ExprSetField;
          Parser.next p;
          let target_expr = parse_expr p in
          let loc = mk_loc start_pos p.prev_end_pos in
          let setfield = Ast_helper.Exp.setfield ~loc expr lident target_expr  in
          Parser.eat_breadcrumb p;
          setfield
        | _ ->
          let end_pos = p.prev_end_pos in
          let loc = mk_loc start_pos end_pos in
          loop p (Ast_helper.Exp.field ~loc expr lident)
        end
      | Lbracket when no_call = false && p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
        parse_bracket_access p expr start_pos
      | Lparen when no_call = false && p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
        loop p (parse_call_expr p expr)
      | Backtick when no_call = false && p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
        begin match expr.pexp_desc with
        | Pexp_ident {txt = Longident.Lident ident} ->
          parse_template_expr ~prefix:ident p
        | _ ->
          Parser.err
            ~start_pos:expr.pexp_loc.loc_start
            ~end_pos:expr.pexp_loc.loc_end
            p
            (Diagnostics.message "Tagged template literals are currently restricted to identifiers like: json`null`.");
          parse_template_expr p
        end
      | _ -> expr
    in
    loop p operand

  (* a unary expression is an expression with only one operand and
   * unary operator. Examples:
   *   -1
   *   !condition
   *   -. 1.6
   *)
  and parse_unary_expr p =
    let start_pos = p.Parser.start_pos in
    match p.Parser.token with
    | (Minus | MinusDot | Plus | PlusDot | Bang) as token ->
      Parser.leave_breadcrumb p Grammar.ExprUnary;
      let token_end = p.end_pos in
      Parser.next p;
      let operand = parse_unary_expr p in
      let unary_expr = make_unary_expr start_pos token_end token operand  in
      Parser.eat_breadcrumb p;
      unary_expr
    | _ ->
      parse_primary_expr ~operand:(parse_atomic_expr p) p

  (* Represents an "operand" in a binary expression.
   * If you have `a + b`, `a` and `b` both represent
   * the operands of the binary expression with opeartor `+` *)
  and parse_operand_expr ~context p =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes p in
    let expr = match p.Parser.token with
    | Assert ->
      Parser.next p;
      let expr = parse_unary_expr p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.assert_ ~loc expr
    | Lazy ->
      Parser.next p;
      let expr = parse_unary_expr p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.lazy_ ~loc expr
    | Try ->
      parse_try_expression p
    | If ->
      parse_if_expression p
    | For ->
      parse_for_expression p
    | While ->
      parse_while_expression p
    | Switch ->
      parse_switch_expression p
    | _ ->
      if (context != WhenExpr) &&
         is_es6_arrow_expression ~in_ternary:(context=TernaryTrueBranchExpr) p
      then
        parse_es6_arrow_expression p
      else
        parse_unary_expr p
    in
    (* let endPos = p.Parser.prevEndPos in *)
    {expr with
      pexp_attributes = List.concat[expr.Parsetree.pexp_attributes; attrs];
      (* pexp_loc = mkLoc startPos endPos *)
    }

  (* a binary expression is an expression that combines two expressions with an
   * operator. Examples:
   *    a + b
   *    f(x) |> g(y)
   *)
  and parse_binary_expr ?(context=OrdinaryExpr) ?a p prec =
    let a = match a with
    | Some e -> e
    | None -> parse_operand_expr ~context p
    in
    let rec loop a =
      let token = p.Parser.token in
      let token_prec =
        match token with
        (* Can the minus be interpreted as a binary operator? Or is it a unary?
         * let w = {
         *   x
         *   -10
         * }
         * vs
         * let w = {
         *   width
         *   - gap
         * }
         *
         * First case is unary, second is a binary operator.
         * See Scanner.isBinaryOp *)
        | Minus | MinusDot | LessThan when not (
            Scanner.is_binary_op p.scanner.src p.start_pos.pos_cnum p.end_pos.pos_cnum
          ) && p.start_pos.pos_lnum > p.prev_end_pos.pos_lnum -> -1
        | token -> Token.precedence token
      in
      if token_prec < prec then a
      else begin
        Parser.leave_breadcrumb p (Grammar.ExprBinaryAfterOp token);
        let start_pos = p.start_pos in
        Parser.next p;
        let end_pos = p.prev_end_pos in
        let b = parse_binary_expr ~context p (token_prec + 1) in
        let loc = mk_loc a.Parsetree.pexp_loc.loc_start b.pexp_loc.loc_end in
        let expr = Ast_helper.Exp.apply
          ~loc
          (make_infix_operator p token start_pos end_pos)
          [Nolabel, a; Nolabel, b]
        in
        loop expr
      end
    in
    loop a

  (* If we even need this, determines if < might be the start of jsx. Not 100% complete *)
  (* and isStartOfJsx p = *)
    (* Parser.lookahead p (fun p -> *)
      (* match p.Parser.token with *)
      (* | LessThan -> *)
        (* Parser.next p; *)
        (* begin match p.token with *)
        (* | GreaterThan (* <> *) -> true *)
        (* | Lident _ | Uident _ | List -> *)
          (* ignore (parseJsxName p); *)
          (* begin match p.token with *)
          (* | GreaterThan (* <div> *) -> true *)
          (* | Question (*<Component ? *) -> true *)
          (* | Lident _ | List -> *)
            (* Parser.next p; *)
            (* begin match p.token with *)
            (* | Equal (* <Component handleClick= *) -> true *)
            (* | _ -> false (* TODO *) *)
            (* end *)
          (* | Forwardslash (* <Component / *)-> *)
            (* Parser.next p; *)
            (* begin match p.token with *)
            (* | GreaterThan (* <Component /> *) -> true *)
            (* | _ -> false *)
            (* end *)
          (* | _ -> *)
            (* false *)
          (* end *)
        (* | _ -> false *)
        (* end *)
      (* | _ -> false *)
    (* ) *)

  and parse_template_expr ?(prefix="") p =
    let hidden_operator =
      let op = Location.mknoloc (Longident.Lident "^") in
      Ast_helper.Exp.ident op
    in
    let rec loop acc p =
      let start_pos = p.Parser.start_pos in
      match p.Parser.token with
      | TemplateTail txt ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        if String.length txt > 0 then
          let txt = if p.mode = ParseForTypeChecker then parse_template_string_literal txt else txt in
          let str = Ast_helper.Exp.constant ~loc (Pconst_string(txt, Some prefix)) in
          Ast_helper.Exp.apply ~loc hidden_operator
            [Nolabel, acc; Nolabel, str]
        else
          acc
      | TemplatePart txt ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let expr = parse_expr_block p in
        let full_loc = mk_loc start_pos p.prev_end_pos in
        Scanner.set_template_mode p.scanner;
        Parser.expect Rbrace p;
        let txt = if p.mode = ParseForTypeChecker then parse_template_string_literal txt else txt in
        let str = Ast_helper.Exp.constant ~loc (Pconst_string(txt, Some prefix)) in
        let next =
          let a = if String.length txt > 0 then
              Ast_helper.Exp.apply ~loc:full_loc hidden_operator [Nolabel, acc; Nolabel, str]
            else acc
          in
          Ast_helper.Exp.apply ~loc:full_loc hidden_operator
            [Nolabel, a; Nolabel, expr]
        in
        loop next p
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        acc
    in
    Scanner.set_template_mode p.scanner;
    Parser.expect Backtick p;
    let start_pos = p.Parser.start_pos in
    match p.Parser.token with
    | TemplateTail txt ->
      let loc = mk_loc start_pos p.end_pos in
      Parser.next p;
      let txt = if p.mode = ParseForTypeChecker then parse_template_string_literal txt else txt in
      Ast_helper.Exp.constant ~loc (Pconst_string(txt, Some prefix))
    | TemplatePart txt ->
      let constant_loc = mk_loc start_pos p.end_pos in
      Parser.next p;
      let expr = parse_expr_block p in
      let full_loc = mk_loc start_pos p.prev_end_pos in
      Scanner.set_template_mode p.scanner;
      Parser.expect Rbrace p;
      let txt = if p.mode = ParseForTypeChecker then parse_template_string_literal txt else txt in
      let str = Ast_helper.Exp.constant ~loc:constant_loc (Pconst_string(txt, Some prefix)) in
      let next =
        if String.length txt > 0 then
          Ast_helper.Exp.apply ~loc:full_loc hidden_operator [Nolabel, str; Nolabel, expr]
        else
          expr
      in
      loop next p
   | token ->
     Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
     Ast_helper.Exp.constant (Pconst_string("", None))

  (* Overparse: let f = a : int => a + 1, is it (a : int) => or (a): int =>
   * Also overparse constraints:
   *  let x = {
   *    let a = 1
   *    a + pi: int
   *  }
   *
   *  We want to give a nice error message in these cases
   *  *)
  and over_parse_constrained_or_coerced_or_arrow_expression p expr =
    match p.Parser.token with
    | ColonGreaterThan ->
      parse_coerced_expr ~expr p
    | Colon ->
      Parser.next p;
      let typ = parse_typ_expr ~es6_arrow:false p in
      begin match p.Parser.token with
      | EqualGreater ->
        Parser.next p;
        let body = parse_expr p in
        let pat = match expr.pexp_desc with
        | Pexp_ident longident ->
          Ast_helper.Pat.var ~loc:expr.pexp_loc
            (Location.mkloc
              (Longident.flatten longident.txt |> String.concat ".")
              longident.loc)
        (* TODO: can we convert more expressions to patterns?*)
        | _ ->
          Ast_helper.Pat.var ~loc:expr.pexp_loc (Location.mkloc "pattern" expr.pexp_loc)
        in
        let arrow1 = Ast_helper.Exp.fun_
          ~loc:(mk_loc expr.pexp_loc.loc_start body.pexp_loc.loc_end)
          Asttypes.Nolabel
          None
          pat
          (Ast_helper.Exp.constraint_ body typ)
        in
        let arrow2 = Ast_helper.Exp.fun_
          ~loc:(mk_loc expr.pexp_loc.loc_start body.pexp_loc.loc_end)
          Asttypes.Nolabel
          None
          (Ast_helper.Pat.constraint_ pat typ)
          body
        in
        let msg =
          Doc.breakable_group ~force_break:true (
            Doc.concat [
              Doc.text "Did you mean to annotate the parameter type or the return type?";
              Doc.indent (
                Doc.concat [
                  Doc.line;
                  Doc.text "1) ";
                  Printer.print_expression arrow1 CommentTable.empty;
                  Doc.line;
                  Doc.text "2) ";
                  Printer.print_expression arrow2 CommentTable.empty;
                ]
              )
            ]
          ) |> Doc.to_string ~width:80
        in
        Parser.err
          ~start_pos:expr.pexp_loc.loc_start
          ~end_pos:body.pexp_loc.loc_end
          p
          (Diagnostics.message msg);
        arrow1
      | _ ->
        let open Parsetree in
        let loc = mk_loc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
        let expr = Ast_helper.Exp.constraint_ ~loc expr typ in
        let () = Parser.err
          ~start_pos:expr.pexp_loc.loc_start
          ~end_pos:typ.ptyp_loc.loc_end
          p
          (Diagnostics.message
            (Doc.breakable_group ~force_break:true (Doc.concat [
              Doc.text "Expressions with type constraints need to be wrapped in parens:";
              Doc.indent (
                Doc.concat [
                Doc.line;
                Printer.add_parens (Printer.print_expression expr CommentTable.empty);
                ]
              )
            ]) |> Doc.to_string ~width:80
          ))
        in
        expr
      end
    | _ -> expr

  and parse_let_binding_body ~start_pos ~attrs p =
    Parser.begin_region p;
    Parser.leave_breadcrumb p Grammar.LetBinding;
    let pat, exp =
      let pat = parse_pattern p in
      match p.Parser.token with
      | Colon ->
        Parser.next p;
        begin match p.token with
        | Typ -> (* locally abstract types *)
          Parser.next p;
          let newtypes = parse_lident_list p in
          Parser.expect Dot p;
          let typ = parse_typ_expr p in
          Parser.expect Equal p;
          let expr = parse_expr p in
          let loc = mk_loc start_pos p.prev_end_pos in
          let exp, poly = wrap_type_annotation ~loc newtypes typ expr in
          let pat = Ast_helper.Pat.constraint_ ~loc pat poly in
          (pat, exp)
        | _ ->
          let poly_type = parse_poly_type_expr p in
          let loc = {pat.ppat_loc with loc_end = poly_type.Parsetree.ptyp_loc.loc_end} in
          let pat = Ast_helper.Pat.constraint_ ~loc pat poly_type in
          Parser.expect Token.Equal p;
          let exp = parse_expr p in
          let exp = over_parse_constrained_or_coerced_or_arrow_expression p exp in
          (pat, exp)
        end
      | _ ->
        Parser.expect Token.Equal p;
        let exp = over_parse_constrained_or_coerced_or_arrow_expression p (parse_expr p) in
        (pat, exp)
    in
    let loc = mk_loc start_pos p.prev_end_pos in
    let vb = Ast_helper.Vb.mk ~loc ~attrs pat exp in
    Parser.eat_breadcrumb p;
    Parser.end_region p;
    vb

  (* TODO: find a better way? Is it possible?
   * let a = 1
   * @attr
   * and b = 2
   *
   * The problem is that without semi we need a lookahead to determine
   * if the attr is on the letbinding or the start of a new thing
   *
   * let a = 1
   * @attr
   * let b = 1
   *
   * Here @attr should attach to something "new": `let b = 1`
   * The parser state is forked, which is quite expensive…
   *)
  and parse_attributes_and_binding (p : Parser.t) =
    let err = p.scanner.err in
    let ch = p.scanner.ch in
    let offset = p.scanner.offset in
    let rd_offset = p.scanner.rd_offset in
    let line_offset = p.scanner.line_offset in
    let lnum = p.scanner.lnum in
    let mode = p.scanner.mode in
    let token = p.token in
    let start_pos = p.start_pos in
    let end_pos = p.end_pos in
    let prev_end_pos = p.prev_end_pos in
    let breadcrumbs = p.breadcrumbs in
    let errors = p.errors in
    let diagnostics = p.diagnostics in
    let comments = p.comments in

    match p.Parser.token with
    | At ->
      let attrs = parse_attributes p in
      begin match p.Parser.token with
      | And ->
        attrs
      | _ ->
        p.scanner.err <- err;
        p.scanner.ch <- ch;
        p.scanner.offset <- offset;
        p.scanner.rd_offset <- rd_offset;
        p.scanner.line_offset <- line_offset;
        p.scanner.lnum <- lnum;
        p.scanner.mode <- mode;
        p.token <- token;
        p.start_pos <- start_pos;
        p.end_pos <- end_pos;
        p.prev_end_pos <- prev_end_pos;
        p.breadcrumbs <- breadcrumbs;
        p.errors <- errors;
        p.diagnostics <- diagnostics;
        p.comments <- comments;
        []
      end
    | _ -> []

  (* definition	::=	let [rec] let-binding  { and let-binding }   *)
  and parse_let_bindings ~attrs p =
    let start_pos = p.Parser.start_pos in
    Parser.optional p Let |> ignore;
    let rec_flag = if Parser.optional p Token.Rec then
      Asttypes.Recursive
    else
      Asttypes.Nonrecursive
    in
    let first = parse_let_binding_body ~start_pos ~attrs p in

    let rec loop p bindings =
      let start_pos = p.Parser.start_pos in
      let attrs = parse_attributes_and_binding p in
      match p.Parser.token with
      | And ->
        Parser.next p;
        let attrs = match p.token with
        | Export ->
          let export_loc = mk_loc p.start_pos p.end_pos in
          Parser.next p;
          let gen_type_attr = (Location.mkloc "genType" export_loc, Parsetree.PStr []) in
          gen_type_attr::attrs
        | _ -> attrs
        in
        ignore(Parser.optional p Let); (* overparse for fault tolerance *)
        let let_binding = parse_let_binding_body ~start_pos ~attrs p in
        loop p (let_binding::bindings)
      | _ ->
        List.rev bindings
    in
    (rec_flag, loop p [first])

  (*
   * div -> div
   * Foo -> Foo.createElement
   * Foo.Bar -> Foo.Bar.createElement
   *)
  and parse_jsx_name p =
    let longident = match p.Parser.token with
    | Lident ident ->
      let ident_start = p.start_pos in
      let ident_end = p.end_pos in
      Parser.next p;
      let loc = mk_loc ident_start ident_end in
      Location.mkloc (Longident.Lident ident) loc
    | Uident _ ->
      let longident = parse_module_long_ident ~lowercase:false p in
      Location.mkloc (Longident.Ldot (longident.txt, "createElement")) longident.loc
    | _ ->
      let msg = "A jsx name should start with a lowercase or uppercase identifier, like: div in <div /> or Navbar in <Navbar />"
      in
      Parser.err p (Diagnostics.message msg);
      Location.mknoloc (Longident.Lident "_")
    in
    Ast_helper.Exp.ident ~loc:longident.loc longident

  and parse_jsx_opening_or_self_closing_element ~start_pos p =
    let jsx_start_pos = p.Parser.start_pos in
    let name = parse_jsx_name p in
    let jsx_props = parse_jsx_props p in
    let children = match p.Parser.token with
    | Forwardslash -> (* <foo a=b /> *)
      let children_start_pos = p.Parser.start_pos in
      Parser.next p;
      let children_end_pos = p.Parser.start_pos in
      Parser.expect GreaterThan p;
      let loc = mk_loc children_start_pos children_end_pos in
      make_list_expression loc [] None (* no children *)
    | GreaterThan -> (* <foo a=b> bar </foo> *)
      let children_start_pos = p.Parser.start_pos in
      Scanner.set_jsx_mode p.scanner;
      Parser.next p;
      let (spread, children) = parse_jsx_children p in
      let children_end_pos = p.Parser.start_pos in
      let () = match p.token with
      | LessThanSlash -> Parser.next p
      | LessThan -> Parser.next p; Parser.expect Forwardslash p
      | token when Grammar.is_structure_item_start token -> ()
      | _ -> Parser.expect LessThanSlash p
      in
      begin match p.Parser.token with
      | Lident _ | Uident _ when verify_jsx_opening_closing_name p name ->
        Parser.expect GreaterThan p;
        let loc = mk_loc children_start_pos children_end_pos in
        ( match spread, children with
          | true, child :: _ ->
            child
          | _ ->
            make_list_expression loc children None
        )
      | token ->
        let () = if Grammar.is_structure_item_start token then (
          let closing = "</" ^ (string_of_pexp_ident name) ^ ">" in
          let msg = Diagnostics.message ("Missing " ^ closing) in
          Parser.err ~start_pos ~end_pos:p.prev_end_pos p msg;
        ) else (
          let opening = "</" ^ (string_of_pexp_ident name) ^ ">" in
          let msg = "Closing jsx name should be the same as the opening name. Did you mean " ^ opening ^ " ?" in
          Parser.err ~start_pos ~end_pos:p.prev_end_pos p (Diagnostics.message msg);
          Parser.expect GreaterThan p
        )
        in
        let loc = mk_loc children_start_pos children_end_pos in
        ( match spread, children with
          | true, child :: _ ->
            child
          | _ ->
            make_list_expression loc children None
        )
      end
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      make_list_expression Location.none [] None
    in
    let jsx_end_pos = p.prev_end_pos in
    let loc = mk_loc jsx_start_pos jsx_end_pos in
    Ast_helper.Exp.apply
      ~loc
      name
      (List.concat [jsx_props; [
        (Asttypes.Labelled "children", children);
        (Asttypes.Nolabel, Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None)
      ]])

  (*
   *  jsx ::=
   *    | <> jsx-children </>
   *    | <element-name {jsx-prop} />
   *    | <element-name {jsx-prop}> jsx-children </element-name>
   *
   *  jsx-children ::= primary-expr*          * => 0 or more
   *)
  and parse_jsx p =
    Parser.leave_breadcrumb p Grammar.Jsx;
    let start_pos = p.Parser.start_pos in
    Parser.expect LessThan p;
    let jsx_expr = match p.Parser.token with
    | Lident _ | Uident _ ->
      parse_jsx_opening_or_self_closing_element ~start_pos p
    | GreaterThan -> (* fragment: <> foo </> *)
      parse_jsx_fragment p
    | _ ->
      parse_jsx_name p
    in
    {jsx_expr with pexp_attributes = [jsx_attr]}

  (*
   * jsx-fragment ::=
   *  | <> </>
   *  | <> jsx-children </>
   *)
  and parse_jsx_fragment p =
    let children_start_pos = p.Parser.start_pos in
    Scanner.set_jsx_mode p.scanner;
    Parser.expect GreaterThan p;
    let (_spread, children) = parse_jsx_children p in
    let children_end_pos = p.Parser.start_pos in
    Parser.expect LessThanSlash p;
    Parser.expect GreaterThan p;
    let loc = mk_loc children_start_pos children_end_pos in
    make_list_expression loc children None


  (*
   * jsx-prop ::=
   *   |  lident
   *   | ?lident
   *   |  lident =  jsx_expr
   *   |  lident = ?jsx_expr
   *)
  and parse_jsx_prop p =
    Parser.leave_breadcrumb p Grammar.JsxAttribute;
    match p.Parser.token with
    | Question | Lident _ ->
      let optional = Parser.optional p Question in
      let (name, loc) = parse_lident p in
      let prop_loc_attr = (Location.mkloc "res.namedArgLoc" loc, Parsetree.PStr []) in
      (* optional punning: <foo ?a /> *)
      if optional then
        Some (
          Asttypes.Optional name,
          Ast_helper.Exp.ident ~attrs:[prop_loc_attr]
            ~loc (Location.mkloc (Longident.Lident name) loc)
        )
      else begin
        match p.Parser.token with
        | Equal ->
          Parser.next p;
          (* no punning *)
          let optional = Parser.optional p Question in
          let attr_expr =
            let e = parse_primary_expr ~operand:(parse_atomic_expr p) p in
            {e with pexp_attributes = prop_loc_attr::e.pexp_attributes}
          in
          let label =
            if optional then Asttypes.Optional name else Asttypes.Labelled name
          in
          Some (label, attr_expr)
        | _ ->
          let attr_expr =
            Ast_helper.Exp.ident ~loc ~attrs:[prop_loc_attr]
              (Location.mknoloc (Longident.Lident name)) in
          let label =
            if optional then Asttypes.Optional name else Asttypes.Labelled name
          in
          Some (label, attr_expr)
      end
    | _ ->
      None

  and parse_jsx_props p =
    parse_region
      ~grammar:Grammar.JsxAttribute
      ~f:parse_jsx_prop
      p

  and parse_jsx_children p =
    let rec loop p children =
      match p.Parser.token  with
      | Token.Eof | LessThanSlash ->
        Scanner.pop_mode p.scanner Jsx;
        List.rev children
      | LessThan ->
        (* Imagine: <div> <Navbar /> <
         * is `<` the start of a jsx-child? <div …
         * or is it the start of a closing tag?  </div>
         * reconsiderLessThan peeks at the next token and
         * determines the correct token to disambiguate *)
        let token = Scanner.reconsider_less_than p.scanner in
        if token = LessThan then
          let child = parse_primary_expr ~operand:(parse_atomic_expr p) ~no_call:true p in
          loop p (child::children)
        else (* LessThanSlash *)
          let () = p.token <- token in
          let () = Scanner.pop_mode p.scanner Jsx in
          List.rev children
      | token when Grammar.is_jsx_child_start token ->
        let () = Scanner.pop_mode p.scanner Jsx in
        let child = parse_primary_expr ~operand:(parse_atomic_expr p) ~no_call:true p in
        loop p (child::children)
      | _ ->
        Scanner.pop_mode p.scanner Jsx;
        List.rev children
    in
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      (true, [parse_primary_expr ~operand:(parse_atomic_expr p) ~no_call:true p])
    | _ -> (false, loop p [])

  and parse_braced_or_record_expr  p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Lbrace p;
    match p.Parser.token with
    | Rbrace ->
      Parser.err p (Diagnostics.unexpected Rbrace p.breadcrumbs);
      Parser.next p;
      let loc = mk_loc start_pos p.prev_end_pos in
      let braces = make_braces_attr loc in
      Ast_helper.Exp.construct ~attrs:[braces] ~loc
        (Location.mkloc (Longident.Lident "()") loc) None
    | DotDotDot ->
      (* beginning of record spread, parse record *)
      Parser.next p;
      let spread_expr = parse_constrained_or_coerced_expr p in
      Parser.expect Comma p;
      let expr = parse_record_expr ~start_pos ~spread:(Some spread_expr) [] p in
      Parser.expect Rbrace p;
      expr
    | String s ->
      let field =
        let loc = mk_loc p.start_pos p.end_pos in
        Parser.next p;
        Location.mkloc (Longident.Lident s) loc
      in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let field_expr = parse_expr p in
        Parser.optional p Comma |> ignore;
        let expr = parse_record_expr_with_string_keys ~start_pos (field, field_expr) p in
        Parser.expect Rbrace p;
        expr
      | _ ->
        let constant = Ast_helper.Exp.constant ~loc:field.loc (Parsetree.Pconst_string(s, None)) in
        let a = parse_primary_expr ~operand:constant p in
        let e = parse_binary_expr ~a p 1 in
        let e = parse_ternary_expr e p in
        begin match p.Parser.token with
        | Semicolon ->
          Parser.next p;
          let expr = parse_expr_block ~first:e p in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        | Rbrace ->
          Parser.next p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {e with pexp_attributes = braces::e.pexp_attributes}
        | _ ->
          let expr = parse_expr_block ~first:e p in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        end
      end
    | Uident _ | Lident _ ->
      let value_or_constructor = parse_value_or_constructor p in
      begin match value_or_constructor.pexp_desc with
      | Pexp_ident path_ident ->
        let ident_end_pos = p.prev_end_pos in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          let expr = parse_record_expr ~start_pos [(path_ident, value_or_constructor)] p in
          Parser.expect Rbrace p;
          expr
        | Colon ->
          Parser.next p;
          let field_expr = parse_expr p in
          begin match p.token with
          | Rbrace ->
            Parser.next p;
            let loc = mk_loc start_pos p.prev_end_pos in
            Ast_helper.Exp.record ~loc [(path_ident, field_expr)] None
          | _ ->
            Parser.expect Comma p;
            let expr = parse_record_expr ~start_pos [(path_ident, field_expr)] p in
            Parser.expect Rbrace p;
            expr
          end
        (* error case *)
        | Lident _ ->
          if p.prev_end_pos.pos_lnum < p.start_pos.pos_lnum then (
            Parser.expect Comma p;
            let expr = parse_record_expr ~start_pos [(path_ident, value_or_constructor)] p in
            Parser.expect Rbrace p;
            expr
          ) else (
            Parser.expect Colon p;
            let expr = parse_record_expr ~start_pos [(path_ident, value_or_constructor)] p in
            Parser.expect Rbrace p;
            expr
          )
        | Semicolon ->
          Parser.next p;
          let expr = parse_expr_block ~first:(Ast_helper.Exp.ident path_ident) p in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        | Rbrace ->
          Parser.next p;
          let expr = Ast_helper.Exp.ident ~loc:path_ident.loc path_ident in
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        | EqualGreater ->
          let loc = mk_loc start_pos ident_end_pos in
          let ident = Location.mkloc (Longident.last path_ident.txt) loc in
          let a = parse_es6_arrow_expression
            ~parameters:[TermParameter {
              uncurried = false;
              attrs = [];
              label = Asttypes.Nolabel;
              expr = None;
              pat = Ast_helper.Pat.var ident;
              pos = start_pos;
            }]
            p
          in
          let e = parse_binary_expr ~a p 1 in
          let e = parse_ternary_expr e p in
          begin match p.Parser.token with
          | Semicolon ->
            Parser.next p;
            let expr = parse_expr_block ~first:e p in
            Parser.expect Rbrace p;
            let loc = mk_loc start_pos p.prev_end_pos in
            let braces = make_braces_attr loc in
            {expr with pexp_attributes = braces::expr.pexp_attributes}
          | Rbrace ->
            Parser.next p;
            let loc = mk_loc start_pos p.prev_end_pos in
            let braces = make_braces_attr loc in
            {e with pexp_attributes = braces::e.pexp_attributes}
          | _ ->
            let expr = parse_expr_block ~first:e p in
            Parser.expect Rbrace p;
            let loc = mk_loc start_pos p.prev_end_pos in
            let braces = make_braces_attr loc in
            {expr with pexp_attributes = braces::expr.pexp_attributes}
          end
        | _ ->
          Parser.leave_breadcrumb p Grammar.ExprBlock;
          let a = parse_primary_expr ~operand:(Ast_helper.Exp.ident ~loc:path_ident.loc path_ident) p in
          let e = parse_binary_expr ~a p 1 in
          let e = parse_ternary_expr e p in
          Parser.eat_breadcrumb p;
          begin match p.Parser.token with
          | Semicolon ->
            Parser.next p;
            let expr = parse_expr_block ~first:e p in
            Parser.expect Rbrace p;
            let loc = mk_loc start_pos p.prev_end_pos in
            let braces = make_braces_attr loc in
            {expr with pexp_attributes = braces::expr.pexp_attributes}
          | Rbrace ->
            Parser.next p;
            let loc = mk_loc start_pos p.prev_end_pos in
            let braces = make_braces_attr loc in
            {e with pexp_attributes = braces::e.pexp_attributes}
          | _ ->
            let expr = parse_expr_block ~first:e p in
            Parser.expect Rbrace p;
            let loc = mk_loc start_pos p.prev_end_pos in
            let braces = make_braces_attr loc in
            {expr with pexp_attributes = braces::expr.pexp_attributes}
          end
         end
      | _ ->
        Parser.leave_breadcrumb p Grammar.ExprBlock;
        let a = parse_primary_expr ~operand:value_or_constructor p in
        let e = parse_binary_expr ~a p 1 in
        let e = parse_ternary_expr e p in
        Parser.eat_breadcrumb p;
        begin match p.Parser.token with
        | Semicolon ->
          Parser.next p;
          let expr = parse_expr_block ~first:e p in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        | Rbrace ->
          Parser.next p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {e with pexp_attributes = braces::e.pexp_attributes}
        | _ ->
          let expr = parse_expr_block ~first:e p in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        end
         end
    | _ ->
      let expr = parse_expr_block p in
      Parser.expect Rbrace p;
      let loc = mk_loc start_pos p.prev_end_pos in
      let braces = make_braces_attr loc in
      {expr with pexp_attributes = braces::expr.pexp_attributes}

  and parse_record_row_with_string_key p =
    match p.Parser.token with
    | String s ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      let field = Location.mkloc (Longident.Lident s) loc in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let field_expr = parse_expr p in
        Some (field, field_expr)
      | _ ->
        Some (field, Ast_helper.Exp.ident ~loc:field.loc field)
      end
    | _ -> None

  and parse_record_row p =
    let () = match p.Parser.token with
    | Token.DotDotDot ->
      Parser.err p (Diagnostics.message ErrorMessages.record_expr_spread);
      Parser.next p;
    | _ -> ()
    in
    match p.Parser.token with
    | Lident _ | Uident _ | List ->
      let field = parse_value_path p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let field_expr = parse_expr p in
        Some (field, field_expr)
      | _ ->
        Some (field, Ast_helper.Exp.ident ~loc:field.loc  field)
      end
    | _ -> None

  and parse_record_expr_with_string_keys ~start_pos first_row p =
    let rows = first_row::(
      parse_comma_delimited_region ~grammar:Grammar.RecordRowsStringKey ~closing:Rbrace ~f:parse_record_row_with_string_key p
    ) in
    let loc = mk_loc start_pos p.end_pos in
    let record_str_expr = Ast_helper.Str.eval ~loc (
      Ast_helper.Exp.record ~loc rows None
    ) in
    Ast_helper.Exp.extension ~loc
      (Location.mkloc "obj" loc, Parsetree.PStr [record_str_expr])

  and parse_record_expr ~start_pos ?(spread=None) rows p =
    let exprs =
      parse_comma_delimited_region
        ~grammar:Grammar.RecordRows
        ~closing:Rbrace
        ~f:parse_record_row p
    in
    let rows = List.concat [rows; exprs] in
    let () = match rows with
    | [] ->
      let msg = "Record spread needs at least one field that's updated" in
      Parser.err p (Diagnostics.message msg);
    | _rows -> ()
    in
    let loc = mk_loc start_pos p.end_pos in
    Ast_helper.Exp.record ~loc rows spread

  and parse_expr_block_item p =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes p in
    match p.Parser.token with
    | Module ->
      Parser.next p;
      begin match p.token with
      | Lparen ->
        parse_first_class_module_expr ~start_pos p
      | _ ->
        let name = match p.Parser.token with
        | Uident ident ->
          let loc = mk_loc p.start_pos p.end_pos in
          Parser.next p;
          Location.mkloc ident loc
        | t ->
          Parser.err p (Diagnostics.uident t);
          Location.mknoloc "_"
        in
        let body = parse_module_binding_body p in
        Parser.optional p Semicolon |> ignore;
        let expr = parse_expr_block p in
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Exp.letmodule ~loc name body expr
      end
    | Exception ->
      let extension_constructor = parse_exception_def ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let block_expr = parse_expr_block  p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.letexception ~loc extension_constructor block_expr
    | Open ->
      let od = parse_open_description ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let block_expr = parse_expr_block p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.open_ ~loc od.popen_override od.popen_lid block_expr
    | Let ->
      let (rec_flag, let_bindings) = parse_let_bindings ~attrs p in
      let next = match p.Parser.token with
      | Semicolon ->
        Parser.next p;
        if Grammar.is_block_expr_start p.Parser.token then
          parse_expr_block p
        else
          let loc = mk_loc p.start_pos p.end_pos in
          Ast_helper.Exp.construct ~loc
            (Location.mkloc (Longident.Lident "()") loc) None
      | token when Grammar.is_block_expr_start token ->
        parse_expr_block p
      | _ ->
        let loc = mk_loc p.start_pos p.end_pos in
        Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
      in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.let_ ~loc rec_flag let_bindings next
    | _ ->
      let e1 =
        let expr = parse_expr p in
        {expr with pexp_attributes = List.concat [attrs; expr.pexp_attributes]}
      in
      ignore (Parser.optional p Semicolon);
      if Grammar.is_block_expr_start p.Parser.token then
        let e2 = parse_expr_block p in
        let loc = {e1.pexp_loc with loc_end = e2.pexp_loc.loc_end} in
        Ast_helper.Exp.sequence ~loc e1 e2
      else e1

  (* blockExpr ::= expr
   *            |  expr          ;
   *            |  expr          ; blockExpr
   *            |  module    ... ; blockExpr
   *            |  open      ... ; blockExpr
   *            |  exception ... ; blockExpr
   *            |  let       ...
   *            |  let       ... ;
   *            |  let       ... ; blockExpr
   *
   *  note: semi should be made optional
   *  a block of expression is always
   *)
  and parse_expr_block ?first p =
    Parser.leave_breadcrumb p Grammar.ExprBlock;
    let item = match first with
    | Some e -> e
    | None -> parse_expr_block_item p
    in
    let block_expr = match p.Parser.token with
    | Semicolon ->
      Parser.next p;
      if Grammar.is_block_expr_start p.Parser.token then
        let next = parse_expr_block_item p in
        ignore(Parser.optional p Semicolon);
        let loc = {item.pexp_loc with loc_end = next.pexp_loc.loc_end} in
        Ast_helper.Exp.sequence ~loc item next
      else
        item
    | token when Grammar.is_block_expr_start token ->
      let next = parse_expr_block_item p in
      ignore(Parser.optional p Semicolon);
      let loc = {item.pexp_loc with loc_end = next.pexp_loc.loc_end} in
      Ast_helper.Exp.sequence ~loc item next
    | _ ->
      item
    in
    Parser.eat_breadcrumb p;
    over_parse_constrained_or_coerced_or_arrow_expression p block_expr

  and parse_try_expression p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Try p;
    let expr = parse_expr ~context:WhenExpr p in
    Parser.expect Catch p;
    Parser.expect Lbrace p;
    let cases = parse_pattern_matching p in
    Parser.expect Rbrace p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.try_ ~loc expr cases

  and parse_if_expression p =
    Parser.begin_region p;
    Parser.leave_breadcrumb p Grammar.ExprIf;
    let start_pos = p.Parser.start_pos in
    Parser.expect If p;
    Parser.leave_breadcrumb p Grammar.IfCondition;
    (* doesn't make sense to try es6 arrow here? *)
    let condition_expr = parse_expr ~context:WhenExpr p in
    Parser.eat_breadcrumb p;
    Parser.leave_breadcrumb p IfBranch;
    Parser.expect Lbrace p;
    let then_expr = parse_expr_block p in
    Parser.expect Rbrace p;
    Parser.eat_breadcrumb p;
    let else_expr = match p.Parser.token with
    | Else ->
      Parser.end_region p;
      Parser.leave_breadcrumb p Grammar.ElseBranch;
      Parser.next p;
      Parser.begin_region p;
      let else_expr = match p.token with
      | If ->
        parse_if_expression p
      | _ ->
        Parser.expect  Lbrace p;
        let block_expr = parse_expr_block p in
        Parser.expect Rbrace p;
        block_expr
      in
      Parser.eat_breadcrumb p;
      Parser.end_region p;
      Some else_expr
    | _ ->
      Parser.end_region p;
      None
    in
    let loc = mk_loc start_pos p.prev_end_pos in
    Parser.eat_breadcrumb p;
    Ast_helper.Exp.ifthenelse ~loc condition_expr then_expr else_expr

  and parse_for_rest has_opening_paren pattern start_pos p =
    Parser.expect In p;
    let e1 = parse_expr p in
    let direction = match p.Parser.token with
    | To -> Asttypes.Upto
    | Downto -> Asttypes.Downto
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Asttypes.Upto
    in
    Parser.next p;
    let e2 = parse_expr ~context:WhenExpr p in
    if has_opening_paren then Parser.expect Rparen p;
    Parser.expect Lbrace p;
    let body_expr = parse_expr_block p in
    Parser.expect Rbrace p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.for_ ~loc pattern e1 e2 direction body_expr

  and parse_for_expression p =
    let start_pos = p.Parser.start_pos in
    Parser.expect For p;
		match p.token with
		| Lparen ->
			let lparen = p.start_pos in
			Parser.next p;
			begin match p.token with
			| Rparen ->
				Parser.next p;
				let unit_pattern =
					let loc = mk_loc lparen p.prev_end_pos in
					let lid = Location.mkloc (Longident.Lident "()") loc in
					Ast_helper.Pat.construct lid None
				in
        parse_for_rest false (parse_alias_pattern ~attrs:[] unit_pattern p) start_pos p
			| _ ->
        let pat = parse_pattern p in
        begin match p.token with
        | Comma ->
          Parser.next p;
          let tuple_pattern =
            parse_tuple_pattern ~attrs:[] ~start_pos:lparen ~first:pat p
          in
          let pattern = parse_alias_pattern ~attrs:[] tuple_pattern p in
          parse_for_rest false pattern start_pos p
        | _ ->
          parse_for_rest true pat start_pos p
        end
			end
		| _ ->
      parse_for_rest false (parse_pattern p) start_pos p

  and parse_while_expression p =
    let start_pos = p.Parser.start_pos in
    Parser.expect While p;
    let expr1 = parse_expr ~context:WhenExpr p in
    Parser.expect Lbrace p;
    let expr2 = parse_expr_block p in
    Parser.expect Rbrace p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.while_ ~loc expr1 expr2

  and parse_pattern_match_case p =
    Parser.begin_region p;
    Parser.leave_breadcrumb p Grammar.PatternMatchCase;
    match p.Parser.token with
    | Token.Bar ->
      Parser.next p;
      let lhs = parse_pattern p in
      let guard = match p.Parser.token with
      | When ->
        Parser.next p;
        Some (parse_expr ~context:WhenExpr p)
      | _ ->
        None
      in
      let () = match p.token with
      | EqualGreater -> Parser.next p
      | _ -> Recover.recover_equal_greater p
      in
      let rhs = parse_expr_block p in
      Parser.end_region p;
      Parser.eat_breadcrumb p;
      Some (Ast_helper.Exp.case lhs ?guard rhs)
    | _ ->
      Parser.end_region p;
      None

  and parse_pattern_matching p =
    Parser.leave_breadcrumb p Grammar.PatternMatching;
    let cases =
      parse_delimited_region
        ~grammar:Grammar.PatternMatching
        ~closing:Rbrace
        ~f:parse_pattern_match_case
        p
    in
    let () = match cases with
    | [] -> Parser.err ~start_pos:p.prev_end_pos p (
        Diagnostics.message "Pattern matching needs at least one case"
      )
    | _ -> ()
    in
    cases

  and parse_switch_expression p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Switch p;
    let switch_expr = parse_expr ~context:WhenExpr p in
    Parser.expect Lbrace p;
    let cases = parse_pattern_matching p in
    Parser.expect Rbrace p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.match_ ~loc switch_expr cases

  (*
   * argument ::=
   *   | _                            (* syntax sugar *)
   *   | expr
   *   | expr : type
   *   | ~ label-name
   *   | ~ label-name
   *   | ~ label-name ?
   *   | ~ label-name =   expr
   *   | ~ label-name =   _           (* syntax sugar *)
   *   | ~ label-name =   expr : type
   *   | ~ label-name = ? expr
   *   | ~ label-name = ? _           (* syntax sugar *)
   *   | ~ label-name = ? expr : type
   *
   *  uncurried_argument ::=
   *   | . argument
   *)
  and parse_argument p =
    if (
      p.Parser.token = Token.Tilde ||
      p.token = Dot ||
      p.token = Underscore ||
      Grammar.is_expr_start p.token
    ) then (
      match p.Parser.token with
      | Dot ->
        let uncurried = true in
        let start_pos = p.Parser.start_pos in
        Parser.next(p);
        begin match p.token with
          (* apply(.) *)
          | Rparen ->
            let loc = mk_loc start_pos p.prev_end_pos in
            let unit_expr = Ast_helper.Exp.construct ~loc
              (Location.mkloc (Longident.Lident "()") loc) None
            in
            Some (uncurried, Asttypes.Nolabel, unit_expr)
          | _ ->
            parse_argument2 p ~uncurried
        end
      | _ ->
        parse_argument2 p ~uncurried:false
    ) else
      None

  and parse_argument2 p ~uncurried =
    match p.Parser.token with
    (* foo(_), do not confuse with foo(_ => x), TODO: performance *)
    | Underscore when not (is_es6_arrow_expression ~in_ternary:false p) ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      let exp = Ast_helper.Exp.ident ~loc (
        Location.mkloc (Longident.Lident "_") loc
      ) in
      Some (uncurried, Asttypes.Nolabel, exp)
    | Tilde ->
      Parser.next p;
      (* TODO: nesting of pattern matches not intuitive for error recovery *)
      begin match p.Parser.token with
      | Lident ident ->
        let start_pos = p.start_pos in
        Parser.next p;
        let end_pos = p.prev_end_pos in
        let loc = mk_loc start_pos end_pos in
        let prop_loc_attr = (Location.mkloc "res.namedArgLoc" loc, Parsetree.PStr []) in
        let ident_expr = Ast_helper.Exp.ident ~attrs:[prop_loc_attr] ~loc (
          Location.mkloc (Longident.Lident ident) loc
        ) in
        begin match p.Parser.token with
        | Question ->
          Parser.next p;
          Some (uncurried, Asttypes.Optional ident, ident_expr)
        | Equal ->
          Parser.next p;
          let label = match p.Parser.token with
          | Question ->
            Parser.next p;
            Asttypes.Optional ident
          | _ ->
            Labelled ident
          in
          let expr = match p.Parser.token with
          | Underscore when not (is_es6_arrow_expression ~in_ternary:false p) ->
            let loc = mk_loc p.start_pos p.end_pos in
            Parser.next p;
            Ast_helper.Exp.ident ~loc (
              Location.mkloc (Longident.Lident "_") loc
            )
          | _ ->
           let expr = parse_constrained_or_coerced_expr p in
           {expr with pexp_attributes = prop_loc_attr::expr.pexp_attributes}
          in
          Some (uncurried, label, expr)
        | Colon ->
          Parser.next p;
          let typ = parse_typ_expr p in
          let loc = mk_loc start_pos p.prev_end_pos in
          let expr = Ast_helper.Exp.constraint_ ~attrs:[prop_loc_attr] ~loc ident_expr typ in
          Some (uncurried, Labelled ident, expr)
        | _ ->
          Some (uncurried, Labelled ident, ident_expr)
        end
      | t ->
        Parser.err p (Diagnostics.lident t);
        Some (uncurried, Nolabel, Recover.default_expr ())
      end
    | _ -> Some (uncurried, Nolabel, parse_constrained_or_coerced_expr p)

  and parse_call_expr p fun_expr =
    Parser.expect Lparen p;
    let start_pos = p.Parser.start_pos in
    Parser.leave_breadcrumb p Grammar.ExprCall;
    let args =
      parse_comma_delimited_region
        ~grammar:Grammar.ArgumentList
        ~closing:Rparen
        ~f:parse_argument p
    in
    Parser.expect Rparen p;
    let args = match args with
    | [] ->
      let loc = mk_loc start_pos p.prev_end_pos in
     (* No args -> unit sugar: `foo()` *)
      [ false,
        Asttypes.Nolabel,
        Ast_helper.Exp.construct
          ~loc (Location.mkloc (Longident.Lident "()") loc) None
      ]
    | args -> args
    in
    let loc = {fun_expr.pexp_loc with loc_end = p.prev_end_pos} in
    let args = match args with
    | (u, lbl, expr)::args ->
      let group (grp, acc) (uncurried, lbl, expr) =
        let (_u, grp) = grp in
        if uncurried == true then
          ((true, [lbl, expr]), ((_u, (List.rev grp))::acc))
        else
          ((_u, ((lbl, expr)::grp)), acc)
      in
      let ((_u, grp), acc) = List.fold_left group((u, [lbl, expr]), []) args in
      List.rev ((_u, (List.rev grp))::acc)
    | [] -> []
    in
    let apply = List.fold_left (fun call_body group ->
      let (uncurried, args) = group in
      let (args, wrap) = process_underscore_application args in
      let exp = if uncurried then
        let attrs = [uncurry_attr] in
        Ast_helper.Exp.apply ~loc ~attrs call_body args
      else
        Ast_helper.Exp.apply ~loc call_body args
      in
      wrap exp
    ) fun_expr args
    in
    Parser.eat_breadcrumb p;
    apply

  and parse_value_or_constructor p =
    let start_pos = p.Parser.start_pos in
    let rec aux p acc =
      match p.Parser.token with
      | Uident ident ->
        let end_pos_lident = p.end_pos in
        Parser.next p;
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          aux p (ident::acc)
        | Lparen when p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
          let lparen = p.start_pos in
          let args = parse_constructor_args p in
          let rparen = p.prev_end_pos in
          let lident = build_longident (ident::acc) in
          let tail = match args with
          | [] -> None
          | [{Parsetree.pexp_desc = Pexp_tuple _} as arg] as args ->
            let loc = mk_loc lparen rparen in
            if p.mode = ParseForTypeChecker then
              (* Some(1, 2) for type-checker *)
              Some arg
            else
              (* Some((1, 2)) for printer *)
              Some (Ast_helper.Exp.tuple ~loc args)
          | [arg] ->
            Some arg
          | args ->
            let loc = mk_loc lparen rparen in
            Some (Ast_helper.Exp.tuple ~loc args)
          in
          let loc = mk_loc start_pos p.prev_end_pos in
          let ident_loc = mk_loc start_pos end_pos_lident in
          Ast_helper.Exp.construct ~loc (Location.mkloc lident ident_loc) tail
        | _ ->
          let loc = mk_loc start_pos p.prev_end_pos in
          let lident = build_longident (ident::acc) in
          Ast_helper.Exp.construct ~loc (Location.mkloc lident loc) None
        end
      | Lident ident ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let lident = build_longident (ident::acc) in
        Ast_helper.Exp.ident ~loc (Location.mkloc lident loc)
      | List ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let lident = build_longident ("list"::acc) in
        Ast_helper.Exp.ident ~loc (Location.mkloc lident loc)
      | token ->
        Parser.next p;
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        Recover.default_expr()
    in
    aux p []

  and parse_poly_variant_expr p =
    let start_pos = p.start_pos in
    let (ident, _loc) = parse_hash_ident ~start_pos p in
    begin match p.Parser.token with
    | Lparen when p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
      let lparen = p.start_pos in
      let args = parse_constructor_args p in
      let rparen = p.prev_end_pos in
      let loc_paren = mk_loc lparen rparen in
      let tail = match args with
      | [] -> None
      | [{Parsetree.pexp_desc = Pexp_tuple _} as expr ] as args ->
        if p.mode = ParseForTypeChecker then
          (* #a(1, 2) for type-checker *)
          Some expr
        else
          (* #a((1, 2)) for type-checker *)
          Some (Ast_helper.Exp.tuple ~loc:loc_paren args)
      | [arg] -> Some arg
      | args ->
        (* #a((1, 2)) for printer *)
        Some (Ast_helper.Exp.tuple ~loc:loc_paren args)
      in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.variant ~loc ident tail
    | _ ->
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.variant ~loc ident None
    end

  and parse_constructor_args p =
    let lparen = p.Parser.start_pos in
    Parser.expect Lparen p;
    let args =
      parse_comma_delimited_region
        ~grammar:Grammar.ExprList ~f:parse_constrained_expr_region ~closing:Rparen p
    in
    Parser.expect Rparen p;
    match args with
    | [] ->
      let loc = mk_loc lparen p.prev_end_pos in
      [Ast_helper.Exp.construct
        ~loc (Location.mkloc (Longident.Lident "()") loc) None]
    | args -> args

  and parse_tuple_expr ~first ~start_pos p =
    let exprs =
      parse_comma_delimited_region
        p ~grammar:Grammar.ExprList ~closing:Rparen ~f:parse_constrained_expr_region
    in
    Parser.expect Rparen p;
    Ast_helper.Exp.tuple ~loc:(mk_loc start_pos p.prev_end_pos) (first::exprs)

  and parse_spread_expr_region p =
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      let expr = parse_constrained_or_coerced_expr p in
      Some (true, expr)
    | token when Grammar.is_expr_start token ->
      Some (false, parse_constrained_or_coerced_expr p)
    | _ -> None

  and parse_list_expr ~start_pos p =
    Parser.expect Lbracket p;
    let list_exprs =
      parse_comma_delimited_reversed_list
      p ~grammar:Grammar.ListExpr ~closing:Rbracket ~f:parse_spread_expr_region
    in
    Parser.expect Rbracket p;
    let loc = mk_loc start_pos p.prev_end_pos in
    match list_exprs with
    | (true, expr)::exprs ->
      let exprs = exprs |> List.map snd |> List.rev in
      make_list_expression loc exprs (Some expr)
    | exprs ->
     let exprs =
        exprs
        |> List.map (fun (spread, expr) ->
            if spread then
              Parser.err p (Diagnostics.message ErrorMessages.list_expr_spread);
            expr)
        |> List.rev
      in
      make_list_expression loc exprs None

  (* Overparse ... and give a nice error message *)
  and parse_non_spread_exp ~msg p =
    let () = match p.Parser.token with
    | DotDotDot ->
      Parser.err p (Diagnostics.message msg);
      Parser.next p;
    | _ -> ()
    in
    match p.Parser.token with
    | token when Grammar.is_expr_start token ->
      let expr = parse_expr p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let typ = parse_typ_expr p in
        let loc = mk_loc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
        Some (Ast_helper.Exp.constraint_ ~loc expr typ)
      | _ -> Some expr
      end
    | _ -> None

  and parse_array_exp p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Lbracket p;
    let exprs =
      parse_comma_delimited_region
        p
        ~grammar:Grammar.ExprList
        ~closing:Rbracket
        ~f:(parse_non_spread_exp ~msg:ErrorMessages.array_expr_spread)
    in
    Parser.expect Rbracket p;
    Ast_helper.Exp.array ~loc:(mk_loc start_pos p.prev_end_pos) exprs

  (* TODO: check attributes in the case of poly type vars,
   * might be context dependend: parseFieldDeclaration (see ocaml) *)
  and parse_poly_type_expr p =
    let start_pos = p.Parser.start_pos in
    match p.Parser.token with
    | SingleQuote ->
      let vars = parse_type_var_list p in
      begin match vars with
      | _v1::_v2::_ ->
        Parser.expect Dot p;
        let typ = parse_typ_expr p in
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Typ.poly ~loc vars typ
      | [var] ->
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          let typ = parse_typ_expr p in
          let loc = mk_loc start_pos p.prev_end_pos in
          Ast_helper.Typ.poly ~loc vars typ
        | EqualGreater ->
          Parser.next p;
          let typ = Ast_helper.Typ.var ~loc:var.loc var.txt in
          let return_type = parse_typ_expr ~alias:false p in
          let loc = mk_loc typ.Parsetree.ptyp_loc.loc_start p.prev_end_pos in
          Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ return_type
        | _ ->
          Ast_helper.Typ.var ~loc:var.loc var.txt
        end
      | _ -> assert false
      end
    | _ ->
      parse_typ_expr p

  (* 'a 'b 'c *)
  and parse_type_var_list p =
    let rec loop p vars =
      match p.Parser.token with
      | SingleQuote ->
        Parser.next p;
        let (lident, loc) = parse_lident p in
        let var = Location.mkloc lident loc in
        loop p (var::vars)
      | _ ->
        List.rev vars
    in
    loop p []

  and parse_lident_list p =
    let rec loop p ls =
      match p.Parser.token with
      | Lident lident ->
        let loc = mk_loc p.start_pos p.end_pos in
        Parser.next p;
        loop p ((Location.mkloc lident loc)::ls)
      | _ ->
        List.rev ls
    in
    loop p []

  and parse_atomic_typ_expr ~attrs p =
    Parser.leave_breadcrumb p Grammar.AtomicTypExpr;
    let start_pos = p.Parser.start_pos in
    let typ = match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      let (ident, loc) = parse_lident p in
      Ast_helper.Typ.var ~loc ~attrs ident
    | Underscore ->
      let end_pos = p.end_pos in
      Parser.next p;
      Ast_helper.Typ.any ~loc:(mk_loc start_pos end_pos) ~attrs ()
    | Lparen ->
      Parser.next p;
      begin match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let unit_constr = Location.mkloc (Longident.Lident "unit") loc in
        Ast_helper.Typ.constr ~attrs unit_constr []
      | _ ->
        let t = parse_typ_expr p in
        begin match p.token with
        | Comma ->
          Parser.next p;
          parse_tuple_type ~attrs ~first:t ~start_pos p
        | _ ->
          Parser.expect Rparen p;
          {t with
            ptyp_loc = mk_loc start_pos p.prev_end_pos;
            ptyp_attributes = List.concat [attrs; t.ptyp_attributes]}
        end
      end
    | Lbracket ->
      parse_polymorphic_variant_type ~attrs p
    | Uident _ | Lident _ | List ->
      let constr = parse_value_path p in
      let args =  parse_type_constructor_args ~constr_name:constr p in
      Ast_helper.Typ.constr ~loc:(mk_loc start_pos p.prev_end_pos) ~attrs constr args
    | Module ->
      Parser.next p;
      Parser.expect Lparen p;
      let package_type = parse_package_type ~start_pos ~attrs p in
      Parser.expect Rparen p;
      {package_type with ptyp_loc = mk_loc start_pos p.prev_end_pos}
    | Percent ->
      let extension = parse_extension p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Typ.extension ~attrs ~loc extension
    | Lbrace ->
      parse_bs_object_type ~attrs p
    | token ->
      begin match skip_tokens_and_maybe_retry p ~is_start_of_grammar:Grammar.is_atomic_typ_expr_start with
      | Some () ->
        parse_atomic_typ_expr ~attrs p
      | None ->
        Parser.err ~start_pos:p.prev_end_pos p (Diagnostics.unexpected token p.breadcrumbs);
        Recover.default_type()
      end
    in
    Parser.eat_breadcrumb p;
    typ

  (* package-type	::=
      | modtype-path
      ∣ modtype-path with package-constraint  { and package-constraint }
   *)
  and parse_package_type ~start_pos ~attrs p =
    let mod_type_path = parse_module_long_ident ~lowercase:true p in
    begin match p.Parser.token with
    | With ->
      Parser.next p;
      let constraints = parse_package_constraints p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Typ.package ~loc ~attrs mod_type_path constraints
    | _ ->
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Typ.package ~loc ~attrs mod_type_path []
    end

  (* package-constraint  { and package-constraint } *)
  and parse_package_constraints p =
    let first =
      Parser.expect Typ p;
      let type_constr = parse_value_path p in
      Parser.expect Equal p;
      let typ = parse_typ_expr p in
      (type_constr, typ)
    in
    let rest = parse_region
      ~grammar:Grammar.PackageConstraint
      ~f:parse_package_constraint
      p
    in
    first::rest

  (* and type typeconstr = typexpr *)
  and parse_package_constraint p =
    match p.Parser.token with
    | And ->
      Parser.next p;
      Parser.expect Typ p;
      let type_constr = parse_value_path p in
      Parser.expect Equal p;
      let typ = parse_typ_expr p in
      Some (type_constr, typ)
    | _ -> None

  and parse_bs_object_type ~attrs p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Lbrace p;
    let closed_flag = match p.token with
    | DotDot -> Parser.next p; Asttypes.Open
    | Dot -> Parser.next p; Asttypes.Closed
    | _ -> Asttypes.Closed
    in
    let fields =
      parse_comma_delimited_region
        ~grammar:Grammar.StringFieldDeclarations
        ~closing:Rbrace
        ~f:parse_string_field_declaration
        p
    in
    Parser.expect Rbrace p;
    let loc = mk_loc start_pos p.prev_end_pos in
    make_bs_obj_type ~attrs ~loc ~closed:closed_flag fields

  (* TODO: check associativity in combination with attributes *)
  and parse_type_alias p typ =
    match p.Parser.token with
    | As ->
      Parser.next p;
      Parser.expect SingleQuote p;
      let (ident, _loc) = parse_lident p in
      (* TODO: how do we parse attributes here? *)
      Ast_helper.Typ.alias ~loc:(mk_loc typ.Parsetree.ptyp_loc.loc_start p.prev_end_pos) typ ident
    | _ -> typ


  (* type_parameter ::=
    *  | type_expr
    *  | ~ident: type_expr
    *  | ~ident: type_expr=?
    *
    * note:
    *  | attrs ~ident: type_expr    -> attrs are on the arrow
    *  | attrs type_expr            -> attrs are here part of the type_expr
    *
    * uncurried_type_parameter ::=
    *  | . type_parameter
    *)
  and parse_type_parameter p =
    if (
      p.Parser.token = Token.Tilde ||
      p.token = Dot ||
      Grammar.is_typ_expr_start p.token
    ) then (
      let start_pos = p.Parser.start_pos in
      let uncurried = Parser.optional p Dot in
      let attrs = parse_attributes p in
      match p.Parser.token with
      | Tilde ->
        Parser.next p;
        let (name, _loc) = parse_lident p in
        Parser.expect ~grammar:Grammar.TypeExpression Colon p;
        let typ = parse_typ_expr p in
        begin match p.Parser.token with
        | Equal ->
          Parser.next p;
          Parser.expect Question p;
          Some (uncurried, attrs, Asttypes.Optional name, typ, start_pos)
        | _ ->
          Some (uncurried, attrs, Asttypes.Labelled name, typ, start_pos)
        end
      | Lident _ | List ->
        let (name, loc) = parse_lident p in
        begin match p.token with
        | Colon ->
          let () =
            let error = Diagnostics.message
              ("Parameter names start with a `~`, like: ~" ^ name)
            in
            Parser.err ~start_pos:loc.loc_start ~end_pos:loc.loc_end p error
          in
          Parser.next p;
          let typ = parse_typ_expr p in
          begin match p.Parser.token with
          | Equal ->
            Parser.next p;
            Parser.expect Question p;
            Some (uncurried, attrs, Asttypes.Optional name, typ, start_pos)
          | _ ->
            Some (uncurried, attrs, Asttypes.Labelled name, typ, start_pos)
          end
        | _ ->
          let constr = Location.mkloc (Longident.Lident name) loc in
          let args =  parse_type_constructor_args ~constr_name:constr p in
          let typ = Ast_helper.Typ.constr ~loc:(mk_loc start_pos p.prev_end_pos) ~attrs constr args
          in

          let typ = parse_arrow_type_rest ~es6_arrow:true ~start_pos typ p in
          let typ = parse_type_alias p typ in
          Some (uncurried, [], Asttypes.Nolabel, typ, start_pos)
        end
      | _ ->
        let typ = parse_typ_expr p in
        let typ_with_attributes = {typ with ptyp_attributes = List.concat[attrs; typ.ptyp_attributes]} in
        Some (uncurried, [], Asttypes.Nolabel, typ_with_attributes, start_pos)
    ) else
      None

  (* (int, ~x:string, float) *)
  and parse_type_parameters p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Lparen p;
    match p.Parser.token with
    | Rparen ->
      Parser.next p;
      let loc = mk_loc start_pos p.prev_end_pos in
      let unit_constr = Location.mkloc (Longident.Lident "unit") loc in
      let typ = Ast_helper.Typ.constr unit_constr [] in
      [(false, [], Asttypes.Nolabel, typ, start_pos)]
    | _ ->
      let params =
        parse_comma_delimited_region ~grammar:Grammar.TypeParameters ~closing:Rparen ~f:parse_type_parameter p
      in
      Parser.expect Rparen p;
      params

  and parse_es6_arrow_type ~attrs p =
    let start_pos = p.Parser.start_pos in
    match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let (name, _loc) = parse_lident p in
      Parser.expect ~grammar:Grammar.TypeExpression Colon p;
      let typ = parse_typ_expr ~alias:false ~es6_arrow:false p in
      let arg = match p.Parser.token with
      | Equal ->
        Parser.next p;
        Parser.expect Question p;
        Asttypes.Optional name
      | _ ->
        Asttypes.Labelled name
      in
      Parser.expect EqualGreater p;
      let return_type = parse_typ_expr ~alias:false p in
      Ast_helper.Typ.arrow ~attrs arg typ return_type
    | _ ->
      let parameters = parse_type_parameters p in
      Parser.expect EqualGreater p;
      let return_type = parse_typ_expr ~alias:false p in
      let end_pos = p.prev_end_pos in
      let typ = List.fold_right (fun (uncurried, attrs, arg_lbl, typ, start_pos) t ->
        let attrs = if uncurried then uncurry_attr::attrs else attrs in
        Ast_helper.Typ.arrow ~loc:(mk_loc start_pos end_pos) ~attrs arg_lbl typ t
      ) parameters return_type
      in
      {typ with
        ptyp_attributes = List.concat [typ.ptyp_attributes; attrs];
        ptyp_loc = mk_loc start_pos p.prev_end_pos}

  (*
   * typexpr ::=
   *  | 'ident
   *  | _
   *  | (typexpr)
   *  | typexpr => typexpr            --> es6 arrow
   *  | (typexpr, typexpr) => typexpr --> es6 arrow
   *  | /typexpr, typexpr, typexpr/  --> tuple
   *  | typeconstr
   *  | typeconstr<typexpr>
   *  | typeconstr<typexpr, typexpr,>
   *  | typexpr as 'ident
   *  | %attr-id                      --> extension
   *  | %attr-id(payload)             --> extension
   *
   * typeconstr ::=
   *  | lident
   *  | uident.lident
   *  | uident.uident.lident     --> long module path
   *)
  and parse_typ_expr ?attrs ?(es6_arrow=true) ?(alias=true) p =
    (* Parser.leaveBreadcrumb p Grammar.TypeExpression; *)
    let start_pos = p.Parser.start_pos in
    let attrs = match attrs with
      | Some attrs ->
        attrs
      | None ->
        parse_attributes p in
    let typ = if es6_arrow && is_es6_arrow_type p then
      parse_es6_arrow_type ~attrs p
    else
      let typ = parse_atomic_typ_expr ~attrs p in
      parse_arrow_type_rest ~es6_arrow ~start_pos typ p
    in
    let typ = if alias then parse_type_alias p typ else typ in
    (* Parser.eatBreadcrumb p; *)
    typ

  and parse_arrow_type_rest ~es6_arrow ~start_pos typ p =
    match p.Parser.token with
    | (EqualGreater | MinusGreater) as token when es6_arrow == true ->
      (* error recovery *)
      if token = MinusGreater then (
        Parser.expect EqualGreater p;
      );
      Parser.next p;
      let return_type = parse_typ_expr ~alias:false p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ return_type
    | _ -> typ

  and parse_typ_expr_region p =
    if Grammar.is_typ_expr_start p.Parser.token then
      Some (parse_typ_expr p)
    else
      None

  and parse_tuple_type ~attrs ~first ~start_pos p =
    let typexprs =
      parse_comma_delimited_region
        ~grammar:Grammar.TypExprList
        ~closing:Rparen
        ~f:parse_typ_expr_region
        p
    in
    Parser.expect Rparen p;
    let tuple_loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Typ.tuple ~attrs ~loc:tuple_loc (first::typexprs)

  and parse_type_constructor_arg_region p =
    if Grammar.is_typ_expr_start p.Parser.token then
      Some (parse_typ_expr p)
    else if p.token = LessThan then (
      Parser.next p;
      parse_type_constructor_arg_region p
    ) else
      None

  (* Js.Nullable.value<'a> *)
  and parse_type_constructor_args ~constr_name p =
    let opening = p.Parser.token in
    let opening_start_pos = p.start_pos in
    match opening with
    | LessThan | Lparen ->
      Scanner.set_diamond_mode p.scanner;
      Parser.next p;
      let type_args =
        (* TODO: change Grammar.TypExprList to TypArgList!!! Why did I wrote this? *)
        parse_comma_delimited_region
          ~grammar:Grammar.TypExprList
          ~closing:GreaterThan
          ~f:parse_type_constructor_arg_region
          p
      in
      let () = match p.token with
      | Rparen when opening = Token.Lparen ->
        let typ = Ast_helper.Typ.constr constr_name type_args in
        let msg =
          Doc.breakable_group ~force_break:true (
            Doc.concat [
              Doc.text "Type parameters require angle brackets:";
              Doc.indent (
                Doc.concat [
                  Doc.line;
                  Printer.print_typ_expr typ CommentTable.empty;
                ]
              )
            ]
          ) |> Doc.to_string ~width:80
        in
        Parser.err ~start_pos:opening_start_pos p (Diagnostics.message msg);
        Parser.next p
      | _ ->
        Parser.expect GreaterThan p
      in
      Scanner.pop_mode p.scanner Diamond;
      type_args
    | _ -> []

  (* string-field-decl ::=
   *  | string: poly-typexpr
   *  | attributes string-field-decl *)
  and parse_string_field_declaration p =
    let attrs = parse_attributes p in
    match p.Parser.token with
    | String name ->
      let name_start_pos = p.start_pos in
      let name_end_pos = p.end_pos in
      Parser.next p;
      let field_name = Location.mkloc name (mk_loc name_start_pos name_end_pos) in
      Parser.expect ~grammar:Grammar.TypeExpression Colon p;
      let typ = parse_poly_type_expr p in
      Some(Parsetree.Otag (field_name, attrs, typ))
    | _token ->
      None

  (* field-decl	::=
   *  | [mutable] field-name : poly-typexpr
   *  | attributes field-decl *)
  and parse_field_declaration p =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes p in
    let mut = if Parser.optional p Token.Mutable then
      Asttypes.Mutable
    else
      Asttypes.Immutable
    in
    let (lident, loc) = match p.token with
    | List ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      ("list", loc)
    | _ -> parse_lident p
    in
    let name = Location.mkloc lident loc in
    let typ = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parse_poly_type_expr p
    | _ ->
      Ast_helper.Typ.constr ~loc:name.loc {name with txt = Lident name.txt} []
    in
    let loc = mk_loc start_pos typ.ptyp_loc.loc_end in
    Ast_helper.Type.field ~attrs ~loc ~mut name typ


  and parse_field_declaration_region p =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes p in
    let mut = if Parser.optional p Token.Mutable then
      Asttypes.Mutable
    else
      Asttypes.Immutable
    in
    match p.token with
    | Lident _ | List ->
      let (lident, loc) =  match p.token with
      | List ->
        let loc = mk_loc p.start_pos p.end_pos in
        Parser.next p;
        ("list", loc)
      | _ -> parse_lident p
      in
      let name = Location.mkloc lident loc in
      let typ = match p.Parser.token with
      | Colon ->
        Parser.next p;
        parse_poly_type_expr p
      | _ ->
        Ast_helper.Typ.constr ~loc:name.loc {name with txt = Lident name.txt} []
      in
      let loc = mk_loc start_pos typ.ptyp_loc.loc_end in
      Some(Ast_helper.Type.field ~attrs ~loc ~mut name typ)
    | _ ->
      None

  (* record-decl ::=
   *  | { field-decl }
   *  | { field-decl, field-decl }
   *  | { field-decl, field-decl, field-decl, }
   *)
  and parse_record_declaration p =
    Parser.leave_breadcrumb p Grammar.RecordDecl;
    Parser.expect Lbrace p;
    let rows =
      parse_comma_delimited_region
        ~grammar:Grammar.RecordDecl
        ~closing:Rbrace
        ~f:parse_field_declaration_region
        p
    in
    Parser.expect Rbrace p;
    Parser.eat_breadcrumb p;
    rows

  (* constr-args ::=
   *  | (typexpr)
   *  | (typexpr, typexpr)
   *  | (typexpr, typexpr, typexpr,)
   *  | (record-decl)
   *
   * TODO: should we overparse inline-records in every position?
   * Give a good error message afterwards?
   *)
  and parse_constr_decl_args p =
    let constr_args = match p.Parser.token with
    | Lparen ->
      Parser.next p;
      (* TODO: this could use some cleanup/stratification *)
      begin match p.Parser.token with
      | Lbrace ->
        let lbrace = p.start_pos in
        Parser.next p;
        let start_pos = p.Parser.start_pos in
        begin match p.Parser.token with
        | DotDot | Dot ->
          let closed_flag = match p.token with
          | DotDot -> Parser.next p; Asttypes.Open
          | Dot -> Parser.next p; Asttypes.Closed
          | _ -> Asttypes.Closed
          in
          let fields =
            parse_comma_delimited_region
              ~grammar:Grammar.StringFieldDeclarations
              ~closing:Rbrace
              ~f:parse_string_field_declaration
              p
          in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let typ = make_bs_obj_type ~attrs:[] ~loc ~closed:closed_flag fields in
          Parser.optional p Comma |> ignore;
          let more_args =
            parse_comma_delimited_region
            ~grammar:Grammar.TypExprList
            ~closing:Rparen
            ~f:parse_typ_expr_region
            p
          in
          Parser.expect Rparen p;
          Parsetree.Pcstr_tuple (typ::more_args)
        | _ ->
          let attrs = parse_attributes p in
          begin match p.Parser.token with
          | String _  ->
            let closed_flag = Asttypes.Closed in
            let fields = match attrs with
            | [] ->
              parse_comma_delimited_region
                ~grammar:Grammar.StringFieldDeclarations
                ~closing:Rbrace
                ~f:parse_string_field_declaration
                p
            | attrs ->
              let first =
                Parser.leave_breadcrumb p Grammar.StringFieldDeclarations;
                let field = match parse_string_field_declaration p with
                | Some field -> field
                | None -> assert false
                in
                (* parse comma after first *)
                let () = match p.Parser.token with
                | Rbrace | Eof -> ()
                | Comma -> Parser.next p
                | _ -> Parser.expect Comma p
                in
                Parser.eat_breadcrumb p;
                begin match field with
                | Parsetree.Otag (label, _, ct) -> Parsetree.Otag (label, attrs, ct)
                | Oinherit ct -> Oinherit ct
                end
              in
              first::(
                parse_comma_delimited_region
                  ~grammar:Grammar.StringFieldDeclarations
                  ~closing:Rbrace
                  ~f:parse_string_field_declaration
                  p
              ) in
              Parser.expect Rbrace p;
              let loc = mk_loc start_pos p.prev_end_pos in
              let typ = make_bs_obj_type ~attrs:[]  ~loc ~closed:closed_flag fields in
              Parser.optional p Comma |> ignore;
              let more_args =
                parse_comma_delimited_region
                  ~grammar:Grammar.TypExprList
                  ~closing:Rparen
                  ~f:parse_typ_expr_region p
              in
              Parser.expect Rparen p;
              Parsetree.Pcstr_tuple (typ::more_args)
            | _ ->
              let fields = match attrs with
              | [] ->
                parse_comma_delimited_region
                  ~grammar:Grammar.FieldDeclarations
                  ~closing:Rbrace
                  ~f:parse_field_declaration_region
                  p
              | attrs ->
                let first =
                  let field = parse_field_declaration p in
                  Parser.expect Comma p;
                  {field with Parsetree.pld_attributes = attrs}
                in
                first::(
                  parse_comma_delimited_region
                    ~grammar:Grammar.FieldDeclarations
                    ~closing:Rbrace
                    ~f:parse_field_declaration_region
                    p
                )
              in
              let () = match fields with
              | [] -> Parser.err ~start_pos:lbrace p (
                  Diagnostics.message "An inline record declaration needs at least one field"
                )
              | _ -> ()
              in
              Parser.expect Rbrace p;
              Parser.optional p Comma |> ignore;
              Parser.expect Rparen p;
              Parsetree.Pcstr_record fields
            end
        end
        | _ ->
          let args =
            parse_comma_delimited_region
              ~grammar:Grammar.TypExprList
              ~closing:Rparen
              ~f:parse_typ_expr_region
              p
          in
          Parser.expect Rparen p;
          Parsetree.Pcstr_tuple args
       end
    | _ -> Pcstr_tuple []
    in
    let res = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parse_typ_expr p)
    | _ -> None
    in
    (constr_args, res)

  (* constr-decl ::=
   *  | constr-name
   *  | attrs constr-name
   *  | constr-name const-args
   *  | attrs constr-name const-args *)
   and parse_type_constructor_declaration_with_bar p =
    match p.Parser.token with
    | Bar ->
      let start_pos = p.Parser.start_pos in
      Parser.next p;
      Some (parse_type_constructor_declaration ~start_pos p)
    | _ -> None

   and parse_type_constructor_declaration ~start_pos p =
     Parser.leave_breadcrumb p Grammar.ConstructorDeclaration;
     let attrs = parse_attributes p in
     match p.Parser.token with
     | Uident uident ->
       let uident_loc = mk_loc p.start_pos p.end_pos in
       Parser.next p;
       let (args, res) = parse_constr_decl_args p in
       Parser.eat_breadcrumb p;
       let loc = mk_loc start_pos p.prev_end_pos in
       Ast_helper.Type.constructor ~loc ~attrs ?res ~args (Location.mkloc uident uident_loc)
     | t ->
      Parser.err p (Diagnostics.uident t);
      Ast_helper.Type.constructor (Location.mknoloc "_")

   (* [|] constr-decl  { | constr-decl }   *)
   and parse_type_constructor_declarations ?first p =
    let first_constr_decl = match first with
    | None ->
      let start_pos = p.Parser.start_pos in
      ignore (Parser.optional p Token.Bar);
      parse_type_constructor_declaration ~start_pos p
    | Some first_constr_decl ->
      first_constr_decl
    in
    first_constr_decl::(
      parse_region
        ~grammar:Grammar.ConstructorDeclaration
        ~f:parse_type_constructor_declaration_with_bar
        p
    )

  (*
   * type-representation ::=
   *  ∣	 = [ | ] constr-decl  { | constr-decl }
   *  ∣	 = private [ | ] constr-decl  { | constr-decl }
   *  |  = |
   *  ∣	 = private |
   *  ∣	 = record-decl
   *  ∣	 = private record-decl
   *  |  = ..
   *)
  and parse_type_representation p =
    Parser.leave_breadcrumb p Grammar.TypeRepresentation;
    (* = consumed *)
    let private_flag =
      if Parser.optional p Token.Private
      then Asttypes.Private
      else Asttypes.Public
    in
    let kind = match p.Parser.token with
    | Bar | Uident _ ->
      Parsetree.Ptype_variant (parse_type_constructor_declarations p)
    | Lbrace ->
      Parsetree.Ptype_record (parse_record_declaration p)
    | DotDot ->
      Parser.next p;
      Ptype_open
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      (* TODO: I have no idea if this is even remotely a good idea *)
      Parsetree.Ptype_variant []
    in
    Parser.eat_breadcrumb p;
    (private_flag, kind)

  (* type-param	::=
   *  | variance 'lident
   *  | variance _
   *
   * variance ::=
   *   | +
   *   | -
   *   | (* empty *)
   *)
  and parse_type_param p =
    let variance = match p.Parser.token with
    | Plus -> Parser.next p; Asttypes.Covariant
    | Minus -> Parser.next p; Contravariant
    | _ -> Invariant
    in
    match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      let (ident, loc) = parse_lident p in
      Some (Ast_helper.Typ.var ~loc ident, variance)
    | Underscore ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Some (Ast_helper.Typ.any ~loc (), variance)
    (* TODO: should we try parsing lident as 'ident ? *)
    | _token ->
      None

  (* type-params	::=
   *  | <type-param>
 	 *  ∣	<type-param, type-param>
 	 *  ∣	<type-param, type-param, type-param>
 	 *  ∣	<type-param, type-param, type-param,>
   *
   *  TODO: when we have pretty-printer show an error
   *  with the actual code corrected. *)
  and parse_type_params ~parent p =
    let opening = p.Parser.token in
    match opening with
    | LessThan | Lparen when p.start_pos.pos_lnum == p.prev_end_pos.pos_lnum ->
      Scanner.set_diamond_mode p.scanner;
      let opening_start_pos = p.start_pos in
      Parser.leave_breadcrumb p Grammar.TypeParams;
      Parser.next p;
      let params =
        parse_comma_delimited_region
          ~grammar:Grammar.TypeParams
          ~closing:GreaterThan
          ~f:parse_type_param
          p
      in
      let () = match p.token with
      | Rparen when opening = Token.Lparen ->
        let msg =
          Doc.breakable_group ~force_break:true (
            Doc.concat [
              Doc.text "Type parameters require angle brackets:";
              Doc.indent (
                Doc.concat [
                  Doc.line;
                  Doc.concat [
                    Printer.print_longident parent.Location.txt;
                    Printer.print_type_params params CommentTable.empty;
                  ]
                ]
              )
            ]
          ) |> Doc.to_string ~width:80
        in
        Parser.err ~start_pos:opening_start_pos p (Diagnostics.message msg);
        Parser.next p
      | _ ->
        Parser.expect GreaterThan p
      in
      Scanner.pop_mode p.scanner Diamond;
      Parser.eat_breadcrumb p;
      params
    | _ -> []

  (* type-constraint	::=	constraint ' ident =  typexpr *)
  and parse_type_constraint p =
    let start_pos = p.Parser.start_pos in
    match p.Parser.token with
    | Token.Constraint ->
      Parser.next p;
      Parser.expect SingleQuote p;
      begin match p.Parser.token with
      | Lident ident ->
        let ident_loc = mk_loc start_pos p.end_pos in
        Parser.next p;
        Parser.expect Equal p;
        let typ = parse_typ_expr p in
        let loc = mk_loc start_pos p.prev_end_pos in
        Some (Ast_helper.Typ.var ~loc:ident_loc ident, typ, loc)
      | t ->
        Parser.err p (Diagnostics.lident t);
        let loc = mk_loc start_pos p.prev_end_pos in
        Some (Ast_helper.Typ.any (), parse_typ_expr p, loc)
      end
    | _ -> None

  (* type-constraints ::=
   *  | (* empty *)
   *  | type-constraint
   *  | type-constraint type-constraint
   *  | type-constraint type-constraint type-constraint (* 0 or more *)
   *)
  and parse_type_constraints p =
    parse_region
      ~grammar:Grammar.TypeConstraint
      ~f:parse_type_constraint
      p

  and parse_type_equation_or_constr_decl p =
    let uident_start_pos = p.Parser.start_pos in
    match p.Parser.token with
    | Uident uident ->
      Parser.next p;
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        let type_constr =
          parse_value_path_tail p uident_start_pos (Longident.Lident uident)
        in
        let loc = mk_loc uident_start_pos p.prev_end_pos in
        let typ = parse_type_alias p (
          Ast_helper.Typ.constr ~loc type_constr (parse_type_constructor_args ~constr_name:type_constr p)
        ) in
        begin match p.token with
        | Equal ->
          Parser.next p;
          let (priv, kind) = parse_type_representation p in
          (Some typ, priv, kind)
        | EqualGreater ->
          Parser.next p;
          let return_type = parse_typ_expr ~alias:false p in
          let loc = mk_loc uident_start_pos p.prev_end_pos in
          let arrow_type = Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ return_type in
          let typ = parse_type_alias p arrow_type in
          (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
        | _ -> (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
        end
      | _ ->
        let uident_end_pos = p.end_pos in
        let (args, res) = parse_constr_decl_args p in
        let first = Some (
          let uident_loc = mk_loc uident_start_pos uident_end_pos in
          Ast_helper.Type.constructor
            ~loc:(mk_loc uident_start_pos p.prev_end_pos)
            ?res
            ~args
            (Location.mkloc uident uident_loc)
        ) in
        (None, Asttypes.Public, Parsetree.Ptype_variant (parse_type_constructor_declarations p ?first))
      end
    | t ->
      Parser.err p (Diagnostics.uident t);
      (* TODO: is this a good idea? *)
      (None, Asttypes.Public, Parsetree.Ptype_abstract)

  and parse_record_or_bs_object_decl p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Lbrace p;
    match p.Parser.token with
    | DotDot | Dot ->
      let closed_flag = match p.token with
      | DotDot -> Parser.next p; Asttypes.Open
      | Dot -> Parser.next p; Asttypes.Closed
      | _ -> Asttypes.Closed
      in
      let fields =
        parse_comma_delimited_region
          ~grammar:Grammar.StringFieldDeclarations
          ~closing:Rbrace
          ~f:parse_string_field_declaration
          p
      in
      Parser.expect Rbrace p;
      let loc = mk_loc start_pos p.prev_end_pos in
      let typ =
        make_bs_obj_type ~attrs:[] ~loc ~closed:closed_flag fields
        |> parse_type_alias p
      in
      let typ = parse_arrow_type_rest ~es6_arrow:true ~start_pos typ p in
      (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
    | _ ->
      let attrs = parse_attributes p in
      begin match p.Parser.token with
      | String _  ->
        let closed_flag = Asttypes.Closed in
        let fields = match attrs with
        | [] ->
          parse_comma_delimited_region
            ~grammar:Grammar.StringFieldDeclarations
            ~closing:Rbrace
            ~f:parse_string_field_declaration
            p
        | attrs ->
          let first =
            Parser.leave_breadcrumb p Grammar.StringFieldDeclarations;
            let field = match parse_string_field_declaration p with
            | Some field -> field
            | None -> assert false
            in
            (* parse comma after first *)
            let () = match p.Parser.token with
            | Rbrace | Eof -> ()
            | Comma -> Parser.next p
            | _ -> Parser.expect Comma p
            in
            Parser.eat_breadcrumb p;
            begin match field with
            | Parsetree.Otag (label, _, ct) -> Parsetree.Otag (label, attrs, ct)
            | Oinherit ct -> Oinherit ct
            end
          in
          first::(
            parse_comma_delimited_region
              ~grammar:Grammar.StringFieldDeclarations
              ~closing:Rbrace
              ~f:parse_string_field_declaration
              p
          )
          in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let typ =
            make_bs_obj_type ~attrs:[] ~loc ~closed:closed_flag fields |> parse_type_alias p
          in
          let typ = parse_arrow_type_rest ~es6_arrow:true ~start_pos typ p in
          (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
      | _ ->
        Parser.leave_breadcrumb p Grammar.RecordDecl;
        let fields = match attrs with
        | [] ->
          parse_comma_delimited_region
            ~grammar:Grammar.FieldDeclarations
            ~closing:Rbrace
            ~f:parse_field_declaration_region
            p
        | attr::_ as attrs ->
          let first =
            let field = parse_field_declaration p in
            Parser.optional p Comma |> ignore;
            {field with
              Parsetree.pld_attributes = attrs;
              pld_loc = {
                field.Parsetree.pld_loc with loc_start =
                  (attr |> fst).loc.loc_start
              }
            }
          in
          first::(
            parse_comma_delimited_region
              ~grammar:Grammar.FieldDeclarations
              ~closing:Rbrace
              ~f:parse_field_declaration_region
              p
          )
        in
        let () = match fields with
        | [] -> Parser.err ~start_pos p (
            Diagnostics.message "A record needs at least one field"
          )
        | _ -> ()
        in
        Parser.expect Rbrace p;
        Parser.eat_breadcrumb p;
        (None, Asttypes.Public, Parsetree.Ptype_record fields)
      end

  and parse_private_eq_or_repr p =
    Parser.expect Private p;
    match p.Parser.token with
    | Lbrace ->
      let (manifest, _ ,kind) = parse_record_or_bs_object_decl p in
      (manifest, Asttypes.Private, kind)
    | Uident _ ->
      let (manifest, _, kind) = parse_type_equation_or_constr_decl p in
      (manifest, Asttypes.Private, kind)
    | Bar | DotDot ->
      let (_, kind) = parse_type_representation p in
      (None, Asttypes.Private, kind)
    | t when Grammar.is_typ_expr_start t ->
      (Some (parse_typ_expr p), Asttypes.Private, Parsetree.Ptype_abstract)
    | _ ->
      let (_, kind) = parse_type_representation p in
      (None, Asttypes.Private, kind)

  (*
    polymorphic-variant-type	::=
                              | [ tag-spec-first  { | tag-spec } ]
                              | [> [ tag-spec ]  { | tag-spec } ]
                              | [< [|] tag-spec-full  { | tag-spec-full }  [ > { `tag-name }+ ] ]

              tag-spec-first	::=	`tag-name  [ of typexpr ]
 	                            |	[ typexpr ] |  tag-spec

                    tag-spec	::=	`tag-name  [ of typexpr ]
 	                            |	typexpr

                tag-spec-full	::=	`tag-name  [ of [&] typexpr  { & typexpr } ]
                               |	typexpr
  *)
  and parse_polymorphic_variant_type ~attrs p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Lbracket p;
    match p.token with
    | GreaterThan ->
      Parser.next p;
      let row_fields =
        begin match p.token with
        | Rbracket ->
          []
        | Bar ->
          parse_tag_specs p
        | _ ->
          let row_field = parse_tag_spec p in
          row_field :: parse_tag_specs p
        end
      in
      let variant =
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Typ.variant ~attrs ~loc row_fields Open None in
      Parser.expect Rbracket p;
      variant
    | LessThan ->
      Parser.next p;
      Parser.optional p Bar |> ignore;
      let row_field = parse_tag_spec_full p in
      let row_fields = parse_tag_spec_fulls p in
      let tag_names =
        if p.token == GreaterThan
        then begin
          Parser.next p;
          let rec loop p = match p.Parser.token with
            | Rbracket -> []
            | _ ->
              let (ident, _loc) = parse_hash_ident ~start_pos:p.start_pos p in
              ident :: loop p
          in
          loop p
        end
        else [] in
      let variant =
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Typ.variant ~attrs ~loc (row_field :: row_fields) Closed (Some tag_names) in
      Parser.expect Rbracket p;
      variant
    | _ ->
      let row_fields1 = parse_tag_spec_first p in
      let row_fields2 = parse_tag_specs p in
      let variant =
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Typ.variant ~attrs ~loc (row_fields1 @ row_fields2) Closed None in
      Parser.expect Rbracket p;
      variant

  and parse_tag_spec_fulls p =
    match p.Parser.token with
    | Rbracket ->
      []
    | GreaterThan ->
      []
    | Bar ->
      Parser.next p;
      let row_field = parse_tag_spec_full p in
      row_field ::parse_tag_spec_fulls p
    | _ ->
      []

  and parse_tag_spec_full p =
    let attrs = parse_attributes p in
    match p.Parser.token with
    | Hash ->
      parse_polymorphic_variant_type_spec_hash ~attrs ~full:true p
    | _ ->
      let typ = parse_typ_expr ~attrs p in
      Parsetree.Rinherit typ

  and parse_tag_specs p =
    match p.Parser.token with
    | Bar ->
      Parser.next p;
      let row_field = parse_tag_spec p in
      row_field :: parse_tag_specs p
    | _ ->
      []

  and parse_tag_spec p =
    let attrs = parse_attributes p in
    match p.Parser.token with
    | Hash ->
      parse_polymorphic_variant_type_spec_hash ~attrs ~full:false p
    | _ ->
      let typ = parse_typ_expr ~attrs p in
      Parsetree.Rinherit typ

  and parse_tag_spec_first p =
    let attrs = parse_attributes p in
    match p.Parser.token with
    | Bar ->
      Parser.next p;
      [parse_tag_spec p]
    | Hash ->
      [parse_polymorphic_variant_type_spec_hash ~attrs ~full:false p]
    | _ ->
      let typ = parse_typ_expr ~attrs p in
      Parser.expect Bar p;
      [Parsetree.Rinherit typ; parse_tag_spec p]

  and parse_polymorphic_variant_type_spec_hash ~attrs ~full p : Parsetree.row_field =
    let start_pos = p.Parser.start_pos in
    let (ident, loc) = parse_hash_ident ~start_pos p in
    let rec loop p =
      match p.Parser.token with
      | Band when full ->
        Parser.next p;
        let row_field = parse_polymorphic_variant_type_args p in
        row_field :: loop p
      | _ ->
        []
    in
    let first_tuple, tag_contains_a_constant_empty_constructor =
      match p.Parser.token with
      | Band when full ->
        Parser.next p;
        [parse_polymorphic_variant_type_args p], true
      | Lparen ->
        [parse_polymorphic_variant_type_args p], false
      | _ ->
        [], true
    in
    let tuples = first_tuple @ loop p in
    Parsetree.Rtag (
      Location.mkloc ident loc,
      attrs,
      tag_contains_a_constant_empty_constructor,
      tuples
    )

  and parse_polymorphic_variant_type_args p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Lparen p;
    let args = parse_comma_delimited_region
      ~grammar:Grammar.TypExprList
      ~closing:Rparen
      ~f:parse_typ_expr_region
      p
    in
    Parser.expect Rparen p;
    let attrs = [] in
    let loc = mk_loc start_pos p.prev_end_pos in
    match args with
    | [{ptyp_desc = Ptyp_tuple _} as typ] as types ->
      if p.mode = ParseForTypeChecker then
        typ
      else
        Ast_helper.Typ.tuple ~loc ~attrs types
    | [typ] -> typ
    | types -> Ast_helper.Typ.tuple ~loc ~attrs types

  and parse_type_equation_and_representation p =
    match p.Parser.token with
    | Equal | Bar as token ->
      if token = Bar then Parser.expect Equal p;
      Parser.next p;
      begin match p.Parser.token with
      | Uident _ ->
        parse_type_equation_or_constr_decl p
      | Lbrace ->
        parse_record_or_bs_object_decl p
      | Private ->
        parse_private_eq_or_repr p
      | Bar | DotDot ->
        let (priv, kind) = parse_type_representation p in
        (None, priv, kind)
      | _ ->
        let manifest = Some (parse_typ_expr p) in
        begin match p.Parser.token with
        | Equal ->
          Parser.next p;
          let (priv, kind) = parse_type_representation p in
          (manifest, priv, kind)
        | _ ->
          (manifest, Public, Parsetree.Ptype_abstract)
        end
      end
    | _ -> (None, Public, Parsetree.Ptype_abstract)

  (* type-definition	::=	type [rec] typedef  { and typedef }
   * typedef	::=	typeconstr-name [type-params] type-information
   * type-information	::=	[type-equation]  [type-representation]  { type-constraint }
   * type-equation	::=	= typexpr *)
  and parse_type_def ~attrs ~start_pos p =
    Parser.leave_breadcrumb p Grammar.TypeDef;
    (* let attrs = match attrs with | Some attrs -> attrs | None -> parseAttributes p in *)
    Parser.leave_breadcrumb p Grammar.TypeConstrName;
    let (name, loc) = parse_lident p in
    let type_constr_name = Location.mkloc name loc in
    Parser.eat_breadcrumb p;
    let params =
      let constr_name = Location.mkloc (Longident.Lident name) loc in
      parse_type_params ~parent:constr_name p in
    let type_def =
      let (manifest, priv, kind) = parse_type_equation_and_representation p in
      let cstrs = parse_type_constraints p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Type.mk
        ~loc ~attrs ~priv ~kind ~params ~cstrs ?manifest type_constr_name
    in
    Parser.eat_breadcrumb p;
    type_def

  and parse_type_extension ~params ~attrs ~name p =
    Parser.expect PlusEqual p;
    let priv =
      if Parser.optional p Token.Private
      then Asttypes.Private
      else Asttypes.Public
    in
    let constr_start = p.Parser.start_pos in
    Parser.optional p Bar |> ignore;
    let first =
      let (attrs, name, kind) = match p.Parser.token with
      | Bar ->
        Parser.next p;
        parse_constr_def ~parse_attrs:true p
      | _ ->
        parse_constr_def ~parse_attrs:true p
      in
      let loc = mk_loc constr_start p.prev_end_pos in
      Ast_helper.Te.constructor ~loc ~attrs name kind
    in
    let rec loop p cs =
      match p.Parser.token with
      | Bar ->
        let start_pos = p.Parser.start_pos in
        Parser.next p;
        let (attrs, name, kind) = parse_constr_def ~parse_attrs:true p in
        let ext_constr =
          Ast_helper.Te.constructor ~attrs ~loc:(mk_loc start_pos p.prev_end_pos) name kind
        in
        loop p (ext_constr::cs)
      | _ ->
        List.rev cs
    in
    let constructors = loop p [first] in
    Ast_helper.Te.mk ~attrs ~params ~priv name constructors

  and parse_type_definitions ~attrs ~name ~params ~start_pos p =
      let type_def =
        let (manifest, priv, kind) = parse_type_equation_and_representation p in
        let cstrs = parse_type_constraints p in
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Type.mk
          ~loc ~attrs ~priv ~kind ~params ~cstrs ?manifest
          {name with txt = lident_of_path name.Location.txt}
      in
      let rec loop p defs =
        let start_pos = p.Parser.start_pos in
        let attrs = parse_attributes_and_binding p in
        match p.Parser.token with
        | And ->
          Parser.next p;
          let attrs = match p.token with
          | Export ->
            let export_loc = mk_loc p.start_pos p.end_pos in
            Parser.next p;
            let gen_type_attr = (Location.mkloc "genType" export_loc, Parsetree.PStr []) in
            gen_type_attr::attrs
          | _ -> attrs
          in
          let type_def = parse_type_def ~attrs ~start_pos p in
          loop p (type_def::defs)
        | _ ->
          List.rev defs
      in
      loop p [type_def]

  (* TODO: decide if we really want type extensions (eg. type x += Blue)
   * It adds quite a bit of complexity that can be avoided,
   * implemented for now. Needed to get a feel for the complexities of
   * this territory of the grammar *)
  and parse_type_definition_or_extension ~attrs p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Token.Typ p;
    let rec_flag = match p.token with
      | Rec -> Parser.next p; Asttypes.Recursive
      | Lident "nonrec" ->
        Parser.next p;
        Asttypes.Nonrecursive
      | _ -> Asttypes.Nonrecursive
    in
    let name = parse_value_path p in
    let params = parse_type_params ~parent:name p in
    match p.Parser.token with
    | PlusEqual ->
      TypeExt(parse_type_extension ~params ~attrs ~name p)
    | _ ->
      let type_defs = parse_type_definitions ~attrs ~name ~params ~start_pos p in
      TypeDef {rec_flag; types = type_defs}

  and parse_primitive p =
    match p.Parser.token with
    | String s -> Parser.next p; Some s
    | _ -> None

  and parse_primitives p =
    match (parse_region ~grammar:Grammar.Primitive ~f:parse_primitive p) with
    | [] ->
      let msg = "An external definition should have at least one primitive. Example: \"setTimeout\"" in
      Parser.err p (Diagnostics.message msg);
      []
    | primitives -> primitives

  (* external value-name : typexp = external-declaration *)
  and parse_external_def ~attrs p =
    let start_pos = p.Parser.start_pos in
    Parser.leave_breadcrumb p Grammar.External;
    Parser.expect Token.External p;
    let (name, loc) = parse_lident p in
    let name = Location.mkloc name loc in
    Parser.expect ~grammar:(Grammar.TypeExpression) Colon p;
    let typ_expr = parse_typ_expr p in
    Parser.expect Equal p;
    let prim = parse_primitives p in
    let loc = mk_loc start_pos p.prev_end_pos in
    let vb = Ast_helper.Val.mk ~loc ~attrs ~prim name typ_expr in
    Parser.eat_breadcrumb p;
    vb

  (* constr-def ::=
   *  | constr-decl
   *  | constr-name = constr
   *
   *  constr-decl ::= constr-name constr-args
   *  constr-name ::= uident
   *  constr      ::= path-uident *)
  and parse_constr_def ~parse_attrs p =
    let attrs = if parse_attrs then parse_attributes p else [] in
    let name = match p.Parser.token with
    | Uident name ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc name loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    let kind = match p.Parser.token with
    | Lparen ->
      let (args, res) = parse_constr_decl_args p in
      Parsetree.Pext_decl (args, res)
    | Equal ->
      Parser.next p;
      let longident = parse_module_long_ident ~lowercase:false p in
      Parsetree.Pext_rebind longident
    | _ ->
      Parsetree.Pext_decl (Pcstr_tuple [], None)
    in
    (attrs, name, kind)

  (*
   * exception-definition	::=
   *  | exception constr-decl
   *  ∣	exception constr-name = constr
   *
   *  constr-name ::= uident
   *  constr ::= long_uident *)
  and parse_exception_def ~attrs p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Token.Exception p;
    let (_, name, kind) = parse_constr_def ~parse_attrs:false p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Te.constructor ~loc ~attrs name kind

  (* module structure on the file level *)
  and parse_implementation p : Parsetree.structure =
    parse_region p ~grammar:Grammar.Implementation ~f:parse_structure_item_region
    [@@progress (Parser.next, Parser.expect, Parser.check_progress)]

  and parse_structure_item_region p =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes p in
    match p.Parser.token with
    | Open ->
      let open_description = parse_open_description ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Str.open_ ~loc open_description)
    | Let ->
      let (rec_flag, let_bindings) = parse_let_bindings ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Str.value ~loc rec_flag let_bindings)
    | Typ ->
      Parser.begin_region p;
      begin match parse_type_definition_or_extension ~attrs p with
      | TypeDef {rec_flag; types} ->
        Parser.optional p Semicolon |> ignore;
        let loc = mk_loc start_pos p.prev_end_pos in
        Parser.end_region p;
        Some (Ast_helper.Str.type_ ~loc rec_flag types)
      | TypeExt(ext) ->
        Parser.optional p Semicolon |> ignore;
        let loc = mk_loc start_pos p.prev_end_pos in
        Parser.end_region p;
        Some (Ast_helper.Str.type_extension ~loc ext)
      end
    | External ->
      let external_def = parse_external_def ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Str.primitive ~loc external_def)
    | Import ->
      let import_descr = parse_js_import ~start_pos ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      let structure_item = JsFfi.to_parsetree import_descr in
      Some {structure_item with pstr_loc = loc}
    | Exception ->
      let exception_def = parse_exception_def ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Str.exception_ ~loc exception_def)
    | Include ->
      let include_statement = parse_include_statement ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Str.include_ ~loc include_statement)
    | Export ->
      let structure_item = parse_js_export ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some {structure_item with pstr_loc = loc}
    | Module ->
      let structure_item = parse_module_or_module_type_impl_or_pack_expr ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some {structure_item with pstr_loc = loc}
    | AtAt ->
      let attr = parse_standalone_attribute p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Str.attribute ~loc attr)
    | PercentPercent ->
      let extension = parse_extension ~module_language:true p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Str.extension ~attrs ~loc extension)
    | token when Grammar.is_expr_start token ->
      let prev_end_pos = p.Parser.end_pos in
      let exp = parse_expr p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Parser.check_progress ~prev_end_pos ~result:(Ast_helper.Str.eval ~loc ~attrs exp) p
    | _ -> None

  and parse_js_import ~start_pos ~attrs p =
    Parser.expect Token.Import p;
    let import_spec = match p.Parser.token with
    | Token.Lident _ | Token.At ->
      let decl = match parse_js_ffi_declaration p with
      | Some decl -> decl
      | None -> assert false
      in
      JsFfi.Default decl
    | _ -> JsFfi.Spec(parse_js_ffi_declarations p)
    in
    let scope = parse_js_ffi_scope p in
    let loc = mk_loc start_pos p.prev_end_pos in
    JsFfi.import_descr ~attrs ~import_spec ~scope ~loc

  and parse_js_export ~attrs p =
    let export_start = p.Parser.start_pos in
    Parser.expect Token.Export p;
    let export_loc = mk_loc export_start p.prev_end_pos in
    let gen_type_attr = (Location.mkloc "genType" export_loc, Parsetree.PStr []) in
    let attrs = gen_type_attr::attrs in
    match p.Parser.token with
    | Typ ->
      begin match parse_type_definition_or_extension ~attrs p with
      | TypeDef {rec_flag; types} ->
        Ast_helper.Str.type_ rec_flag types
      | TypeExt(ext) ->
        Ast_helper.Str.type_extension ext
      end
    | (* Let *) _ ->
      let (rec_flag, let_bindings) = parse_let_bindings ~attrs p in
      Ast_helper.Str.value rec_flag let_bindings

  and parse_js_ffi_scope p =
    match p.Parser.token with
    | Token.Lident "from" ->
      Parser.next p;
      begin match p.token with
      | String s -> Parser.next p; JsFfi.Module s
      | Uident _ | Lident _ ->
        let value = parse_ident_path p in
        JsFfi.Scope value
      | _ -> JsFfi.Global
      end
    | _ -> JsFfi.Global

  and parse_js_ffi_declarations p =
    Parser.expect Token.Lbrace p;
    let decls = parse_comma_delimited_region
      ~grammar:Grammar.JsFfiImport
      ~closing:Rbrace
      ~f:parse_js_ffi_declaration
      p
    in
    Parser.expect Rbrace p;
    decls

  and parse_js_ffi_declaration p =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes p in
    match p.Parser.token with
    | Lident _ ->
      let (ident, _) = parse_lident p in
      let alias = match p.token with
      | As ->
        Parser.next p;
        let (ident, _) = parse_lident p in
        ident
      | _ ->
        ident
      in
      Parser.expect Token.Colon p;
      let typ = parse_typ_expr p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (JsFfi.decl ~loc ~alias ~attrs ~name:ident ~typ)
    | _ -> None

  (* include-statement ::= include module-expr *)
  and parse_include_statement ~attrs p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Token.Include p;
    let mod_expr = parse_module_expr p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Incl.mk ~loc ~attrs mod_expr

  and parse_atomic_module_expr p =
    let start_pos = p.Parser.start_pos in
    match p.Parser.token with
    | Uident _ident ->
      let longident = parse_module_long_ident ~lowercase:false p in
      Ast_helper.Mod.ident ~loc:longident.loc longident
    | Lbrace ->
      Parser.next p;
      let structure = Ast_helper.Mod.structure (
        parse_delimited_region
          ~grammar:Grammar.Structure
          ~closing:Rbrace
          ~f:parse_structure_item_region
          p
      ) in
      Parser.expect Rbrace p;
      let end_pos = p.prev_end_pos in
      {structure with pmod_loc = mk_loc start_pos end_pos}
    | Lparen ->
      Parser.next p;
      let mod_expr = match p.token with
      | Rparen ->
        Ast_helper.Mod.structure ~loc:(mk_loc start_pos p.prev_end_pos) []
      | _ ->
        parse_constrained_mod_expr p
      in
      Parser.expect Rparen p;
      mod_expr
    | Lident "unpack" -> (* TODO: should this be made a keyword?? *)
      Parser.next p;
      Parser.expect Lparen p;
      let expr = parse_expr p in
      begin match p.Parser.token with
      | Colon ->
        let colon_start = p.Parser.start_pos in
        Parser.next p;
        let attrs = parse_attributes p in
        let package_type = parse_package_type ~start_pos:colon_start ~attrs p in
        Parser.expect Rparen p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let constraint_expr = Ast_helper.Exp.constraint_
          ~loc
          expr package_type
        in
        Ast_helper.Mod.unpack ~loc constraint_expr
      | _ ->
        Parser.expect Rparen p;
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Mod.unpack ~loc expr
      end
    | Percent ->
      let extension = parse_extension p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Mod.extension ~loc extension
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.default_module_expr()

  and parse_primary_mod_expr p =
    let start_pos = p.Parser.start_pos in
    let mod_expr = parse_atomic_module_expr p in
    let rec loop p mod_expr =
      match p.Parser.token with
      | Lparen when p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
        loop p (parse_module_application p mod_expr)
      | _ -> mod_expr
    in
    let mod_expr = loop p mod_expr in
    {mod_expr with pmod_loc = mk_loc start_pos p.prev_end_pos}

  (*
   * functor-arg ::=
   *  | uident : modtype
   *  | _ : modtype
   *  | modtype           --> "punning" for _ : modtype
   *  | attributes functor-arg
   *)
  and parse_functor_arg p =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes p in
    match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      let uident_end_pos = p.prev_end_pos in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let module_type = parse_module_type p in
        let loc = mk_loc start_pos uident_end_pos in
        let arg_name = Location.mkloc ident loc in
        Some (attrs, arg_name, Some module_type, start_pos)
      | Dot ->
        Parser.next p;
        let module_type =
          let module_long_ident =
            parse_module_long_ident_tail ~lowercase:false p start_pos (Longident.Lident ident) in
          Ast_helper.Mty.ident ~loc:module_long_ident.loc module_long_ident
        in
        let arg_name = Location.mknoloc "_" in
        Some (attrs, arg_name, Some module_type, start_pos)
      | _ ->
        let loc = mk_loc start_pos uident_end_pos in
        let mod_ident = Location.mkloc (Longident.Lident ident) loc in
        let module_type = Ast_helper.Mty.ident ~loc mod_ident in
        let arg_name = Location.mknoloc "_" in
        Some (attrs, arg_name, Some module_type, start_pos)
      end
    | Underscore ->
      Parser.next p;
      let arg_name = Location.mkloc "_" (mk_loc start_pos p.prev_end_pos) in
      Parser.expect Colon p;
      let module_type = parse_module_type p in
      Some (attrs, arg_name, Some module_type, start_pos)
    | _ ->
      None

  and parse_functor_args p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Lparen p;
    let args =
      parse_comma_delimited_region
        ~grammar:Grammar.FunctorArgs
        ~closing:Rparen
        ~f:parse_functor_arg
        p
    in
    Parser.expect Rparen p;
    match args with
    | [] ->
      [[], Location.mkloc "*" (mk_loc start_pos p.prev_end_pos), None, start_pos]
    | args -> args

  and parse_functor_module_expr p =
    let start_pos = p.Parser.start_pos in
    let args = parse_functor_args p in
    let return_type = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parse_module_type ~es6_arrow:false p)
    | _ -> None
    in
    Parser.expect EqualGreater p;
    let rhs_module_expr =
      let mod_expr = parse_module_expr p in
      match return_type with
      | Some mod_type ->
        Ast_helper.Mod.constraint_
          ~loc:(mk_loc mod_expr.pmod_loc.loc_start mod_type.Parsetree.pmty_loc.loc_end)
          mod_expr mod_type
      | None -> mod_expr
    in
    let end_pos = p.prev_end_pos in
    let mod_expr = List.fold_right (fun (attrs, name, module_type, start_pos) acc ->
      Ast_helper.Mod.functor_
        ~loc:(mk_loc start_pos end_pos)
        ~attrs
        name module_type acc
    ) args rhs_module_expr
    in
    {mod_expr with pmod_loc = mk_loc start_pos end_pos}

  (* module-expr	::=
   *  | module-path
   *  ∣	{ structure-items }
   *  ∣	functorArgs =>  module-expr
   *  ∣	module-expr(module-expr)
   *  ∣	( module-expr )
   *  ∣	( module-expr : module-type )
   *  | extension
   *  | attributes module-expr *)
  and parse_module_expr p =
    let attrs = parse_attributes p in
    let mod_expr = if is_es6_arrow_functor p then
        parse_functor_module_expr p
      else
        parse_primary_mod_expr p
    in
    {mod_expr with pmod_attributes = List.concat [mod_expr.pmod_attributes; attrs]}

  and parse_constrained_mod_expr p =
    let mod_expr = parse_module_expr p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let mod_type = parse_module_type p in
      let loc = mk_loc mod_expr.pmod_loc.loc_start mod_type.pmty_loc.loc_end in
      Ast_helper.Mod.constraint_ ~loc mod_expr mod_type
    | _ -> mod_expr

  and parse_constrained_mod_expr_region p =
    if Grammar.is_mod_expr_start p.Parser.token then
      Some (parse_constrained_mod_expr p)
    else
      None

  and parse_module_application p mod_expr =
    let start_pos = p.Parser.start_pos in
    Parser.expect Lparen p;
    let args =
      parse_comma_delimited_region
        ~grammar:Grammar.ModExprList
        ~closing:Rparen
        ~f:parse_constrained_mod_expr_region
        p
    in
    Parser.expect Rparen p;
    let args = match args with
    | [] ->
      let loc = mk_loc start_pos p.prev_end_pos in
      [Ast_helper.Mod.structure ~loc []]
    | args -> args
    in
    List.fold_left (fun mod_expr arg ->
      Ast_helper.Mod.apply
        ~loc:(mk_loc mod_expr.Parsetree.pmod_loc.loc_start arg.Parsetree.pmod_loc.loc_end)
        mod_expr arg
    ) mod_expr args

  and parse_module_or_module_type_impl_or_pack_expr ~attrs p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Module p;
    match p.Parser.token with
    | Typ -> parse_module_type_impl ~attrs start_pos p
    | Lparen ->
      let expr = parse_first_class_module_expr ~start_pos p in
      Ast_helper.Str.eval ~attrs expr
    | _ -> parse_maybe_rec_module_binding ~attrs ~start_pos p

  and parse_module_type_impl ~attrs start_pos p =
    Parser.expect Typ p;
    let name_start = p.Parser.start_pos in
    let name = match p.Parser.token with
    | List ->
      Parser.next p;
      let loc = mk_loc name_start p.prev_end_pos in
      Location.mkloc "list" loc
    | Lident ident ->
      Parser.next p;
      let loc = mk_loc name_start p.prev_end_pos in
      Location.mkloc ident loc
    | Uident ident ->
      Parser.next p;
      let loc = mk_loc name_start p.prev_end_pos in
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    Parser.expect Equal p;
    let module_type = parse_module_type p in
    let module_type_declaration =
      Ast_helper.Mtd.mk
        ~attrs
        ~loc:(mk_loc name_start p.prev_end_pos)
        ~typ:module_type
        name
    in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Str.modtype ~loc module_type_declaration

  (* definition	::=
    ∣	 module rec module-name :  module-type =  module-expr   { and module-name
    :  module-type =  module-expr } *)
  and parse_maybe_rec_module_binding ~attrs ~start_pos p =
    match p.Parser.token with
    | Token.Rec ->
      Parser.next p;
      Ast_helper.Str.rec_module (parse_module_bindings ~start_pos ~attrs p)
    | _ ->
      Ast_helper.Str.module_ (parse_module_binding ~attrs ~start_pos:p.Parser.start_pos p)

  and parse_module_binding ~attrs ~start_pos p =
    let name = match p.Parser.token with
    | Uident ident ->
      let start_pos = p.Parser.start_pos in
      Parser.next p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    let body = parse_module_binding_body p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Mb.mk ~attrs ~loc name body

  and parse_module_binding_body p =
    (* TODO: make required with good error message when rec module binding *)
    let return_mod_type = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parse_module_type p)
    | _ -> None
    in
    Parser.expect Equal p;
    let mod_expr = parse_module_expr p in
    match return_mod_type with
    | Some mod_type ->
      Ast_helper.Mod.constraint_
        ~loc:(mk_loc mod_type.pmty_loc.loc_start mod_expr.pmod_loc.loc_end)
        mod_expr mod_type
    | None -> mod_expr


  (* module-name :  module-type =  module-expr
   * { and module-name :  module-type =  module-expr } *)
  and parse_module_bindings ~attrs ~start_pos p =
    let rec loop p acc =
      let start_pos = p.Parser.start_pos in
      let attrs = parse_attributes_and_binding p in
      match p.Parser.token with
      | And ->
        Parser.next p;
        ignore(Parser.optional p Module); (* over-parse for fault-tolerance *)
        let mod_binding = parse_module_binding ~attrs ~start_pos p in
        loop p (mod_binding::acc)
      | _ -> List.rev acc
    in
    let first = parse_module_binding ~attrs ~start_pos p in
    loop p [first]

  and parse_atomic_module_type p =
    let start_pos = p.Parser.start_pos in
    let module_type = match p.Parser.token with
    | Uident _ | Lident _ | List ->
      (* Ocaml allows module types to end with lowercase: module Foo : bar = { ... }
       * lets go with uppercase terminal for now *)
      let module_long_ident = parse_module_long_ident ~lowercase:true p in
      Ast_helper.Mty.ident ~loc:module_long_ident.loc module_long_ident
    | Lparen ->
      Parser.next p;
      let mty = parse_module_type p in
      Parser.expect Rparen p;
      {mty with pmty_loc = mk_loc start_pos p.prev_end_pos}
    | Lbrace ->
      Parser.next p;
      let spec =
        parse_delimited_region
          ~grammar:Grammar.Signature
          ~closing:Rbrace
          ~f:parse_signature_item_region
          p
      in
      Parser.expect Rbrace p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Mty.signature ~loc spec
    | Module -> (* TODO: check if this is still atomic when implementing first class modules*)
      parse_module_type_of p
    | Percent ->
      let extension = parse_extension p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Mty.extension ~loc extension
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.default_module_type()
    in
    let module_type_loc = mk_loc start_pos p.prev_end_pos in
    {module_type with pmty_loc = module_type_loc}

  and parse_functor_module_type p =
    let start_pos = p.Parser.start_pos in
    let args = parse_functor_args p in
    Parser.expect EqualGreater p;
    let rhs = parse_module_type p in
    let end_pos = p.prev_end_pos in
    let mod_type = List.fold_right (fun (attrs, name, module_type, start_pos) acc ->
      Ast_helper.Mty.functor_
        ~loc:(mk_loc start_pos end_pos)
        ~attrs
        name module_type acc
    ) args rhs
    in
    {mod_type with pmty_loc = mk_loc start_pos end_pos}

  (* Module types are the module-level equivalent of type expressions: they
   * specify the general shape and type properties of modules.
   *
   * module-type ::=
   *  | modtype-path
   *  | { signature }
   *  | ( module-type )               --> parenthesized module-type
   *  | functor-args => module-type   --> functor
   *  | module-type => module-type    --> functor
   *  | module type of module-expr
   *  | attributes module-type
   *  | module-type with-mod-constraints
   *  | extension
   *)
   and parse_module_type ?(es6_arrow=true) ?(with_=true) p =
    let attrs = parse_attributes p in
    let modty = if es6_arrow && is_es6_arrow_functor p then
      parse_functor_module_type p
    else
      let modty = parse_atomic_module_type p in
      match p.Parser.token with
      | EqualGreater when es6_arrow == true ->
        Parser.next p;
        let rhs = parse_module_type ~with_:false p in
        let str = Location.mknoloc "_" in
        let loc = mk_loc modty.pmty_loc.loc_start p.prev_end_pos in
        Ast_helper.Mty.functor_ ~loc str (Some modty) rhs
      | _ -> modty
    in
    let module_type = { modty with
      pmty_attributes = List.concat [modty.pmty_attributes; attrs]
    } in
    if with_ then
      parse_with_constraints module_type p
    else module_type


  and parse_with_constraints module_type p =
    match p.Parser.token with
    | With ->
      Parser.next p;
      let first = parse_with_constraint p in
      let rec loop p acc =
        match p.Parser.token with
        | And ->
          Parser.next p;
          loop p ((parse_with_constraint p)::acc)
        | _ ->
          List.rev acc
      in
      let constraints = loop p [first] in
      let loc = mk_loc module_type.pmty_loc.loc_start p.prev_end_pos in
      Ast_helper.Mty.with_ ~loc module_type constraints
    | _ ->
      module_type

  (* mod-constraint	::=
   *  |  type typeconstr<type-params> type-equation type-constraints?
   *  ∣	 type typeconstr-name<type-params> := typexpr
   *  ∣	 module module-path = extended-module-path
   *  ∣	 module module-path :=  extended-module-path
   *
   *  TODO: split this up into multiple functions, better errors *)
  and parse_with_constraint p =
    match p.Parser.token with
    | Module ->
      Parser.next p;
      let module_path = parse_module_long_ident ~lowercase:false p in
      begin match p.Parser.token with
      | ColonEqual ->
        Parser.next p;
        let lident = parse_module_long_ident ~lowercase:false p in
        Parsetree.Pwith_modsubst (module_path, lident)
      | Equal ->
        Parser.next p;
        let lident = parse_module_long_ident ~lowercase:false p in
        Parsetree.Pwith_module (module_path, lident)
      | token ->
        (* TODO: revisit *)
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        let lident = parse_module_long_ident ~lowercase:false p in
        Parsetree.Pwith_modsubst (module_path, lident)
      end
    | Typ ->
      Parser.next p;
      let type_constr = parse_value_path p in
      let params = parse_type_params ~parent:type_constr p in
      begin match p.Parser.token with
      | ColonEqual ->
        Parser.next p;
        let typ_expr = parse_typ_expr p in
        Parsetree.Pwith_typesubst (
          type_constr,
          Ast_helper.Type.mk
            ~loc:type_constr.loc
            ~params
            ~manifest:typ_expr
            (Location.mkloc (Longident.last type_constr.txt) type_constr.loc)
        )
      | Equal ->
        Parser.next p;
        let typ_expr = parse_typ_expr p in
        let type_constraints = parse_type_constraints p in
        Parsetree.Pwith_type (
          type_constr,
          Ast_helper.Type.mk
            ~loc:type_constr.loc
            ~params
            ~manifest:typ_expr
            ~cstrs:type_constraints
            (Location.mkloc (Longident.last type_constr.txt) type_constr.loc)
        )
      | token ->
        (* TODO: revisit *)
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        let typ_expr = parse_typ_expr p in
        let type_constraints = parse_type_constraints p in
        Parsetree.Pwith_type (
          type_constr,
          Ast_helper.Type.mk
            ~loc:type_constr.loc
            ~params
            ~manifest:typ_expr
            ~cstrs:type_constraints
            (Location.mkloc (Longident.last type_constr.txt) type_constr.loc)
        )
      end
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      exit (-1) (* TODO: handle this case *)

  and parse_module_type_of p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Module p;
    Parser.expect Typ p;
    Parser.expect Of p;
    let module_expr = parse_module_expr p in
    Ast_helper.Mty.typeof_ ~loc:(mk_loc start_pos p.prev_end_pos) module_expr

  (* module signature on the file level *)
  and parse_specification p =
    parse_region ~grammar:Grammar.Specification ~f:parse_signature_item_region p
    [@@progress (Parser.next, Parser.expect, Parser.check_progress)]

  and parse_signature_item_region p =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes p in
    match p.Parser.token with
    | Let ->
      Parser.begin_region p;
      let value_desc = parse_sign_let_desc ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Parser.end_region p;
      Some (Ast_helper.Sig.value ~loc value_desc)
    | Typ ->
      Parser.begin_region p;
      begin match parse_type_definition_or_extension ~attrs p with
      | TypeDef {rec_flag; types} ->
        Parser.optional p Semicolon |> ignore;
        let loc = mk_loc start_pos p.prev_end_pos in
        Parser.end_region p;
        Some (Ast_helper.Sig.type_ ~loc rec_flag types)
      | TypeExt(ext) ->
        Parser.optional p Semicolon |> ignore;
        let loc = mk_loc start_pos p.prev_end_pos in
        Parser.end_region p;
        Some (Ast_helper.Sig.type_extension ~loc ext)
      end
    | External ->
      let external_def = parse_external_def ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Sig.value ~loc external_def)
    | Exception ->
      let exception_def = parse_exception_def ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Sig.exception_ ~loc exception_def)
    | Open ->
      let open_description = parse_open_description ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Sig.open_ ~loc open_description)
    | Include ->
      Parser.next p;
      let module_type = parse_module_type p in
      let include_description = Ast_helper.Incl.mk
        ~loc:(mk_loc start_pos p.prev_end_pos)
        ~attrs
        module_type
      in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Sig.include_ ~loc include_description)
    | Module ->
      Parser.next p;
      begin match p.Parser.token with
      | Uident _ ->
        let mod_decl = parse_module_declaration_or_alias ~attrs p in
        Parser.optional p Semicolon |> ignore;
        let loc = mk_loc start_pos p.prev_end_pos in
        Some (Ast_helper.Sig.module_ ~loc mod_decl)
      | Rec ->
        let rec_module = parse_rec_module_spec ~attrs ~start_pos p in
        Parser.optional p Semicolon |> ignore;
        let loc = mk_loc start_pos p.prev_end_pos in
        Some (Ast_helper.Sig.rec_module ~loc rec_module)
      | Typ ->
        Some (parse_module_type_declaration ~attrs ~start_pos p)
      | _t ->
        let mod_decl = parse_module_declaration_or_alias ~attrs p in
        Parser.optional p Semicolon |> ignore;
        let loc = mk_loc start_pos p.prev_end_pos in
        Some (Ast_helper.Sig.module_ ~loc mod_decl)
      end
    | AtAt ->
      let attr = parse_standalone_attribute p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Sig.attribute ~loc attr)
    | PercentPercent ->
      let extension = parse_extension ~module_language:true p in
      Parser.optional p Semicolon |> ignore;
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Sig.extension ~attrs ~loc extension)
    | Import ->
      Parser.next p;
      parse_signature_item_region p
    | _ ->
      None

  (* module rec module-name :  module-type  { and module-name:  module-type } *)
  and parse_rec_module_spec ~attrs ~start_pos p =
    Parser.expect Rec p;
    let rec loop p spec =
      let start_pos = p.Parser.start_pos in
      let attrs = parse_attributes_and_binding p in
      match p.Parser.token with
      | And ->
        (* TODO: give a good error message when with constraint, no parens
         * and ASet: (Set.S with type elt = A.t)
         * and BTree: (Btree.S with type elt = A.t)
         * Without parens, the `and` signals the start of another
         * `with-constraint`
         *)
        Parser.expect And p;
        let decl = parse_rec_module_declaration ~attrs ~start_pos p in
        loop p (decl::spec)
      | _ ->
        List.rev spec
    in
    let first = parse_rec_module_declaration ~attrs ~start_pos p in
    loop p [first]

  (* module-name : module-type *)
  and parse_rec_module_declaration ~attrs ~start_pos p =
    let name = match p.Parser.token with
    | Uident mod_name ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc mod_name loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    Parser.expect Colon p;
    let mod_type = parse_module_type p in
    Ast_helper.Md.mk ~loc:(mk_loc start_pos p.prev_end_pos) ~attrs name mod_type

  and parse_module_declaration_or_alias ~attrs p =
    let start_pos = p.Parser.start_pos in
    let module_name = match p.Parser.token with
    | Uident ident ->
      let loc = mk_loc p.Parser.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    let body = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parse_module_type p
    | Equal ->
      Parser.next p;
      let lident = parse_module_long_ident ~lowercase:false p in
      Ast_helper.Mty.alias lident
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.default_module_type()
    in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Md.mk ~loc ~attrs module_name body

  and parse_module_type_declaration ~attrs ~start_pos p =
    Parser.expect Typ p;
    let module_name = match p.Parser.token with
    | Uident ident ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc ident loc
    | Lident ident ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    let typ = match p.Parser.token with
    | Equal ->
      Parser.next p;
      Some (parse_module_type p)
    | _ -> None
    in
    let module_decl = Ast_helper.Mtd.mk ~attrs ?typ module_name in
    Ast_helper.Sig.modtype ~loc:(mk_loc start_pos p.prev_end_pos) module_decl

  and parse_sign_let_desc ~attrs p =
    let start_pos = p.Parser.start_pos in
    Parser.expect Let p;
    let (name, loc) = parse_lident p in
    let name = Location.mkloc name loc in
    Parser.expect Colon p;
    let typ_expr = parse_poly_type_expr p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Val.mk ~loc ~attrs name typ_expr

(*    attr-id	::=	lowercase-ident
 	∣	  capitalized-ident
 	∣	  attr-id .  attr-id   *)
  and parse_attribute_id p =
    let start_pos = p.Parser.start_pos in
    let rec loop p acc =
      match p.Parser.token with
      | Lident ident | Uident ident ->
        Parser.next p;
        let id = acc ^ ident in
        begin match p.Parser.token with
        | Dot -> Parser.next p; loop p (id ^ ".")
        | _ -> id
        end
      | token when Token.is_keyword token ->
        Parser.next p;
        let id = acc ^ (Token.to_string token) in
        begin match p.Parser.token with
        | Dot -> Parser.next p; loop p (id ^ ".")
        | _ -> id
        end
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        acc
    in
    let id = loop p "" in
    let end_pos = p.prev_end_pos in
    Location.mkloc id (mk_loc start_pos end_pos)

  (*
   * payload ::=  empty
   *          |  ( structure-item )
   *
   * TODO: what about multiple structure items?
   * @attr({let x = 1; let x = 2})
   *
   * Also what about type-expressions and specifications?
   * @attr(:myType) ???
   *)
  and parse_payload p =
    match p.Parser.token with
    | Lparen when p.start_pos.pos_cnum = p.prev_end_pos.pos_cnum  ->
      Parser.next p;
      begin match p.token with
      | Colon ->
        Parser.next p;
        let typ = parse_typ_expr p in
        Parser.expect Rparen p;
        Parsetree.PTyp typ
      | _ ->
        let items = parse_delimited_region
          ~grammar:Grammar.Structure
          ~closing:Rparen
          ~f:parse_structure_item_region
          p
        in
        Parser.expect Rparen p;
        Parsetree.PStr items
      end
    | _ -> Parsetree.PStr []

  (* type attribute = string loc * payload *)
  and parse_attribute p =
    match p.Parser.token with
    | At ->
      Parser.next p;
      let attr_id = parse_attribute_id p in
      let payload = parse_payload p in
      Some(attr_id, payload)
    | _ -> None

  and parse_attributes p =
    parse_region p
      ~grammar:Grammar.Attribute
      ~f:parse_attribute

  (*
   * standalone-attribute ::=
   *  | @@ atribute-id
   *  | @@ attribute-id ( structure-item )
   *)
  and parse_standalone_attribute p =
    Parser.expect AtAt p;
    let attr_id = parse_attribute_id p in
    let payload = parse_payload p in
    (attr_id, payload)

  (* extension	::=	% attr-id  attr-payload
   *              | %% attr-id(
   *  expr	::=	 ...
   *    ∣	 extension
   *
   *  typexpr	::=	 ...
   *    ∣	 extension
   *
   *  pattern	::=	 ...
   *    ∣	 extension
   *
   *  module-expr	::=	 ...
   *    ∣	 extension
   *
   *  module-type	::=	 ...
   *    ∣	 extension
   *
   *  class-expr	::=	 ...
   *    ∣	 extension
   *
   *  class-type	::=	 ...
   *    ∣	 extension
   *
   *
   * item extension nodes usable in structures and signature
   *
   * item-extension ::= %% attr-id
   *                  | %% attr-id(structure-item)
   *
   *  attr-payload ::= structure-item
   *
   *  ~moduleLanguage represents whether we're on the module level or not
   *)
  and parse_extension ?(module_language=false) p =
    if module_language then
      Parser.expect PercentPercent p
    else
      Parser.expect Percent p;
    let attr_id = parse_attribute_id p in
    let payload = parse_payload p in
    (attr_id, payload)
end

module OutcomePrinter: sig
  open Format
  open Outcometree

  val out_value : (formatter -> out_value -> unit) ref [@@live]
  val out_type : (formatter -> out_type -> unit) ref [@@live]
  val out_class_type : (formatter -> out_class_type -> unit) ref [@@live]
  val out_module_type : (formatter -> out_module_type -> unit) ref [@@live]
  val out_sig_item : (formatter -> out_sig_item -> unit) ref [@@live]
  val out_signature : (formatter -> out_sig_item list -> unit) ref [@@live]
  val out_type_extension : (formatter -> out_type_extension -> unit) ref [@@live]
  val out_phrase : (formatter -> out_phrase -> unit) ref [@@live]

  val parenthesized_ident : string -> bool [@@live]
end = struct
  (* Napkin doesn't have parenthesized identifiers.
   * We don't support custom operators. *)
  let parenthesized_ident _name = true

  (* TODO: better allocation strategy for the buffer *)
  let escape_string_contents s =
    let len = String.length s in
    let b = Buffer.create len in
    for i = 0 to len - 1 do
      let c = (String.get [@doesNotRaise]) s i in
      if c = '\008'  then (
        Buffer.add_char b '\\';
        Buffer.add_char b 'b';
      ) else if c = '\009'  then (
        Buffer.add_char b '\\';
        Buffer.add_char b 't';
      ) else if c = '\010' then (
        Buffer.add_char b '\\';
        Buffer.add_char b 'n';
      ) else if c = '\013' then (
        Buffer.add_char b '\\';
        Buffer.add_char b 'r';
      ) else if c = '\034' then (
        Buffer.add_char b '\\';
        Buffer.add_char b '"';
      ) else if c = '\092' then (
        Buffer.add_char b '\\';
        Buffer.add_char b '\\';
      ) else (
        Buffer.add_char b c;
      );
    done;
    Buffer.contents b

  (* let rec print_ident fmt ident = match ident with
    | Outcometree.Oide_ident s -> Format.pp_print_string fmt s
    | Oide_dot (id, s) ->
      print_ident fmt id;
      Format.pp_print_char fmt '.';
      Format.pp_print_string fmt s
    | Oide_apply (id1, id2) ->
      print_ident fmt id1;
      Format.pp_print_char fmt '(';
      print_ident fmt id2;
      Format.pp_print_char fmt ')' *)

    let rec print_out_ident_doc (ident : Outcometree.out_ident) =
      match ident with
      | Oide_ident s -> Doc.text s
      | Oide_dot (ident, s) -> Doc.concat [
          print_out_ident_doc ident;
          Doc.dot;
          Doc.text s;
        ]
      | Oide_apply (call, arg) ->Doc.concat [
          print_out_ident_doc call;
          Doc.lparen;
          print_out_ident_doc arg;
          Doc.rparen;
        ]

  let print_out_attribute_doc (out_attribute: Outcometree.out_attribute) =
    Doc.concat [
      Doc.text "@";
      Doc.text out_attribute.oattr_name;
    ]

  let print_out_attributes_doc (attrs: Outcometree.out_attribute list) =
    match attrs with
    | [] -> Doc.nil
    | attrs ->
      Doc.concat [
        Doc.group (
          Doc.join ~sep:Doc.line (List.map print_out_attribute_doc attrs)
        );
        Doc.line;
      ]

  let rec collect_arrow_args (out_type: Outcometree.out_type) args =
    match out_type with
    | Otyp_arrow (label, arg_type, return_type) ->
      let arg = (label, arg_type) in
      collect_arrow_args return_type (arg::args)
    | _ as return_type ->
      (List.rev args, return_type)

  let rec collect_functor_args (out_module_type: Outcometree.out_module_type) args =
    match out_module_type with
    | Omty_functor (lbl, opt_mod_type, return_mod_type) ->
      let arg = (lbl, opt_mod_type) in
      collect_functor_args return_mod_type (arg::args)
    | _ ->
      (List.rev args, out_module_type)

  let rec print_out_type_doc (out_type: Outcometree.out_type) =
    match out_type with
    | Otyp_abstract | Otyp_variant _ (* don't support poly-variants atm *) | Otyp_open -> Doc.nil
    | Otyp_alias (typ, alias_txt) ->
      Doc.concat [
        print_out_type_doc typ;
        Doc.text " as '";
        Doc.text alias_txt
      ]
    | Otyp_constr (out_ident, []) ->
      print_out_ident_doc out_ident
    | Otyp_manifest (typ1, typ2) ->
        Doc.concat [
          print_out_type_doc typ1;
          Doc.text " = ";
          print_out_type_doc typ2;
        ]
    | Otyp_record record ->
      print_record_declaration_doc ~inline:true record
    | Otyp_stuff txt -> Doc.text txt
    | Otyp_var (ng, s) -> Doc.concat [
        Doc.text ("'" ^ (if ng then "_" else ""));
        Doc.text s
      ]
    | Otyp_object (fields, rest) -> print_object_fields fields rest
    | Otyp_class _ -> Doc.nil
    | Otyp_attribute (typ, attribute) ->
      Doc.group (
        Doc.concat [
          print_out_attribute_doc attribute;
          Doc.line;
          print_out_type_doc typ;
        ]
      )
    (* example: Red | Blue | Green | CustomColour(float, float, float) *)
    | Otyp_sum constructors ->
      print_out_constructors_doc constructors

    (* example: {"name": string, "age": int} *)
    | Otyp_constr (
        (Oide_dot ((Oide_ident "Js"), "t")),
        [Otyp_object (fields, rest)]
      ) -> print_object_fields fields rest

    (* example: node<root, 'value> *)
    | Otyp_constr (out_ident, args) ->
      let args_doc = match args with
      | [] -> Doc.nil
      | args ->
        Doc.concat [
          Doc.less_than;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_out_type_doc args
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.greater_than;
        ]
      in
      Doc.group (
        Doc.concat [
          print_out_ident_doc out_ident;
          args_doc;
        ]
      )
    | Otyp_tuple tuple_args ->
      Doc.group (
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_out_type_doc tuple_args
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ]
      )
    | Otyp_poly (vars, out_type) ->
      Doc.group (
        Doc.concat [
          Doc.join ~sep:Doc.space (
            List.map (fun var -> Doc.text ("'" ^ var)) vars
          );
          print_out_type_doc out_type;
        ]
      )
    | Otyp_arrow _ as typ ->
      let (typ_args, typ) = collect_arrow_args typ [] in
      let args = Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
        List.map (fun (lbl, typ) ->
          if lbl = "" then
            print_out_type_doc typ
          else
            Doc.group (
              Doc.concat [
                Doc.text ("~" ^ lbl ^ ": ");
                print_out_type_doc typ
              ]
            )
        ) typ_args
      ) in
      let args_doc =
        let needs_parens = match typ_args with
        | [_, (Otyp_tuple _ | Otyp_arrow _)] -> true
        (* single argument should not be wrapped *)
        | ["", _] -> false
        | _ -> true
        in
        if needs_parens then
          Doc.group (
            Doc.concat [
              Doc.lparen;
              Doc.indent (
                Doc.concat [
                  Doc.soft_line;
                  args;
                ]
              );
              Doc.trailing_comma;
              Doc.soft_line;
              Doc.rparen;
            ]
          )
        else args
      in
      Doc.concat [
        args_doc;
        Doc.text " => ";
        print_out_type_doc typ;
      ]
    | Otyp_module (_modName, _stringList, _outTypes) ->
        Doc.nil

  and print_object_fields fields rest =
    let dots = match rest with
    | Some non_gen -> Doc.text ((if non_gen then "_" else "") ^ "..")
    | None -> Doc.nil
    in
    Doc.group (
      Doc.concat [
        Doc.lbrace;
        dots;
        Doc.indent (
          Doc.concat [
            Doc.soft_line;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
              List.map (fun (lbl, out_type) -> Doc.group (
                Doc.concat [
                  Doc.text ("\"" ^ lbl ^ "\": ");
                  print_out_type_doc out_type;
                ]
              )) fields
            )
          ]
        );
        Doc.soft_line;
        Doc.trailing_comma;
        Doc.rbrace;
      ]
    )


  and print_out_constructors_doc constructors =
    Doc.group (
      Doc.indent (
        Doc.concat [
          Doc.line;
          Doc.join ~sep:Doc.line (
            List.mapi (fun i constructor ->
              Doc.concat [
                if i > 0 then Doc.text "| " else Doc.if_breaks (Doc.text "| ") Doc.nil;
                print_out_constructor_doc constructor;
              ]
            ) constructors
          )
        ]
      )
    )

  and print_out_constructor_doc (name, args, gadt) =
      let gadt_doc = match gadt with
      | Some out_type ->
        Doc.concat [
          Doc.text ": ";
          print_out_type_doc out_type
        ]
      | None -> Doc.nil
      in
      let args_doc = match args with
      | [] -> Doc.nil
      | [Otyp_record record] ->
        (* inline records
         *   | Root({
         *      mutable value: 'value,
         *      mutable updatedTime: float,
         *    })
         *)
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            print_record_declaration_doc ~inline:true record;
          );
          Doc.rparen;
        ]
      | _types ->
        Doc.indent (
          Doc.concat [
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map print_out_type_doc args
                )
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
            Doc.rparen;
          ]
        )
      in
      Doc.group (
        Doc.concat [
          Doc.text name;
          args_doc;
          gadt_doc
        ]
      )

  and print_record_decl_row_doc (name, mut, arg) =
    Doc.group (
      Doc.concat [
        if mut then Doc.text "mutable " else Doc.nil;
        Doc.text name;
        Doc.text ": ";
        print_out_type_doc arg;
      ]
    )

  and print_record_declaration_doc ~inline rows =
    let content = Doc.concat [
      Doc.lbrace;
      Doc.indent (
        Doc.concat [
          Doc.soft_line;
          Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
            List.map print_record_decl_row_doc rows
          )
        ]
      );
      Doc.trailing_comma;
      Doc.soft_line;
      Doc.rbrace;
    ] in
    if not inline then
      Doc.group content
    else content

  let print_out_type fmt out_type =
    Format.pp_print_string fmt
      (Doc.to_string ~width:80 (print_out_type_doc out_type))

  let print_type_parameter_doc (typ, (co, cn)) =
    Doc.concat [
      if not cn then Doc.text "+" else if not co then Doc.text "-" else Doc.nil;
      if typ = "_" then Doc.text "_" else Doc.text ("'" ^ typ)
    ]


  let rec print_out_sig_item_doc (out_sig_item : Outcometree.out_sig_item) =
    match out_sig_item with
    | Osig_class _ | Osig_class_type _ -> Doc.nil
    | Osig_ellipsis -> Doc.dotdotdot
    | Osig_value value_decl ->
      Doc.group (
        Doc.concat [
          print_out_attributes_doc value_decl.oval_attributes;
          Doc.text (
            match value_decl.oval_prims with | [] -> "let " | _ -> "external "
          );
          Doc.text value_decl.oval_name;
          Doc.text ":";
          Doc.space;
          print_out_type_doc value_decl.oval_type;
          match value_decl.oval_prims with
          | [] -> Doc.nil
          | primitives -> Doc.indent (
              Doc.concat [
                Doc.text " =";
                Doc.line;
                Doc.group (
                  Doc.join ~sep:Doc.line (List.map (fun prim -> Doc.text ("\"" ^ prim ^ "\"")) primitives)
                )
              ]
            )
        ]
      )
  | Osig_typext (out_extension_constructor, _outExtStatus) ->
    print_out_extension_constructor_doc out_extension_constructor
  | Osig_modtype (mod_name, Omty_signature []) ->
    Doc.concat [
      Doc.text "module type ";
      Doc.text mod_name;
    ]
  | Osig_modtype (mod_name, out_module_type) ->
    Doc.group (
      Doc.concat [
        Doc.text "module type ";
        Doc.text mod_name;
        Doc.text " = ";
        print_out_module_type_doc out_module_type;
      ]
    )
  | Osig_module (mod_name, Omty_alias ident, _) ->
    Doc.group (
      Doc.concat [
        Doc.text "module ";
        Doc.text mod_name;
        Doc.text " =";
        Doc.line;
        print_out_ident_doc ident;
      ]
    )
  | Osig_module (mod_name, out_mod_type, out_rec_status) ->
     Doc.group (
      Doc.concat [
        Doc.text (
          match out_rec_status with
          | Orec_not -> "module "
          | Orec_first -> "module rec "
          | Orec_next -> "and"
        );
        Doc.text mod_name;
        Doc.text " = ";
        print_out_module_type_doc out_mod_type;
      ]
    )
  | Osig_type (out_type_decl, out_rec_status) ->
    (* TODO: manifest ? *)
    let attrs = match out_type_decl.otype_immediate, out_type_decl.otype_unboxed with
    | false, false -> Doc.nil
    | true, false ->
      Doc.concat [Doc.text "@immediate"; Doc.line]
    | false, true ->
      Doc.concat [Doc.text "@unboxed"; Doc.line]
    | true, true ->
      Doc.concat [Doc.text "@immediate @unboxed"; Doc.line]
    in
    let kw = Doc.text (
      match out_rec_status with
      | Orec_not -> "type "
      | Orec_first -> "type rec "
      | Orec_next -> "and "
    ) in
    let type_params = match out_type_decl.otype_params with
    | [] -> Doc.nil
    | _params -> Doc.group (
        Doc.concat [
          Doc.less_than;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_type_parameter_doc out_type_decl.otype_params
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.greater_than;
        ]
      )
    in
    let private_doc = match out_type_decl.otype_private with
    | Asttypes.Private -> Doc.text "private "
    | Public -> Doc.nil
    in
    let kind = match out_type_decl.otype_type with
    | Otyp_open -> Doc.concat [
        Doc.text " = ";
        private_doc;
        Doc.text "..";
      ]
    | Otyp_abstract -> Doc.nil
    | Otyp_record record -> Doc.concat [
        Doc.text " = ";
        private_doc;
        print_record_declaration_doc ~inline:false record;
      ]
    | typ -> Doc.concat [
        Doc.text " = ";
        print_out_type_doc typ
      ]
    in
    let constraints =  match out_type_decl.otype_cstrs with
    | [] -> Doc.nil
    | _ -> Doc.group (
      Doc.concat [
        Doc.line;
        Doc.indent (
          Doc.concat [
            Doc.hard_line;
            Doc.join ~sep:Doc.line (List.map (fun (typ1, typ2) ->
              Doc.group (
                Doc.concat [
                  Doc.text "constraint ";
                  print_out_type_doc typ1;
                  Doc.text " =";
                  Doc.indent (
                    Doc.concat [
                      Doc.line;
                      print_out_type_doc typ2;
                    ]
                  )
                ]
              )
            ) out_type_decl.otype_cstrs)
          ]
        )
      ]
    ) in
    Doc.group (
      Doc.concat [
        attrs;
        Doc.group (
          Doc.concat [
            attrs;
            kw;
            Doc.text out_type_decl.otype_name;
            type_params;
            kind
          ]
        );
        constraints
      ]
    )

  and print_out_module_type_doc (out_mod_type : Outcometree.out_module_type) =
    match out_mod_type with
    | Omty_abstract -> Doc.nil
    | Omty_ident ident -> print_out_ident_doc ident
    (* example: module Increment = (M: X_int) => X_int *)
    | Omty_functor _ ->
      let (args, return_mod_type) = collect_functor_args out_mod_type [] in
      let args_doc = match args with
      | [_, None] -> Doc.text "()"
      | args ->
        Doc.group (
          Doc.concat [
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map (fun (lbl, opt_mod_type) -> Doc.group (
                    Doc.concat [
                      Doc.text lbl;
                      match opt_mod_type with
                      | None -> Doc.nil
                      | Some mod_type -> Doc.concat [
                          Doc.text ": ";
                          print_out_module_type_doc mod_type;
                        ]
                    ]
                  )) args
                )
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
            Doc.rparen;
          ]
        )
      in
      Doc.group (
        Doc.concat [
          args_doc;
          Doc.text " => ";
          print_out_module_type_doc return_mod_type
        ]
      )
    | Omty_signature [] -> Doc.nil
    | Omty_signature signature ->
      Doc.breakable_group ~force_break:true (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.line;
              print_out_signature_doc signature;
            ]
          );
          Doc.soft_line;
          Doc.rbrace;
        ]
      )
    | Omty_alias _ident -> Doc.nil

  and print_out_signature_doc (signature : Outcometree.out_sig_item list) =
    let rec loop signature acc =
      match signature with
      | [] -> List.rev acc
      | Outcometree.Osig_typext(ext, Oext_first) :: items ->
        (* Gather together the extension constructors *)
        let rec gather_extensions acc items =
          match items with
              Outcometree.Osig_typext(ext, Oext_next) :: items ->
                gather_extensions
                  ((ext.oext_name, ext.oext_args, ext.oext_ret_type) :: acc)
                  items
            | _ -> (List.rev acc, items)
        in
        let exts, items =
          gather_extensions
            [(ext.oext_name, ext.oext_args, ext.oext_ret_type)]
            items
        in
        let te =
          { Outcometree.otyext_name = ext.oext_type_name;
            otyext_params = ext.oext_type_params;
            otyext_constructors = exts;
            otyext_private = ext.oext_private }
        in
        let doc = print_out_type_extension_doc te in
        loop items (doc::acc)
      | item::items ->
        let doc = print_out_sig_item_doc item in
        loop items (doc::acc)
    in
    match loop signature [] with
    | [doc] -> doc
    | docs ->
      Doc.breakable_group ~force_break:true (
        Doc.join ~sep:Doc.line docs
      )

  and print_out_extension_constructor_doc (out_ext : Outcometree.out_extension_constructor) =
    let type_params = match out_ext.oext_type_params with
    | [] -> Doc.nil
    | params ->
      Doc.group(
        Doc.concat [
          Doc.less_than;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (List.map
                (fun ty -> Doc.text (if ty = "_" then ty else "'" ^ ty))
                params

              )
            ]
          );
          Doc.soft_line;
          Doc.greater_than;
        ]
      )

    in
    Doc.group (
      Doc.concat [
        Doc.text "type ";
        Doc.text out_ext.oext_type_name;
        type_params;
        Doc.text " +=";
        Doc.line;
        if out_ext.oext_private = Asttypes.Private then
          Doc.text "private "
        else
          Doc.nil;
        print_out_constructor_doc
          (out_ext.oext_name, out_ext.oext_args, out_ext.oext_ret_type)
      ]
    )

  and print_out_type_extension_doc (type_extension : Outcometree.out_type_extension) =
    let type_params = match type_extension.otyext_params with
    | [] -> Doc.nil
    | params ->
      Doc.group(
        Doc.concat [
          Doc.less_than;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (List.map
                (fun ty -> Doc.text (if ty = "_" then ty else "'" ^ ty))
                params

              )
            ]
          );
          Doc.soft_line;
          Doc.greater_than;
        ]
      )

    in
    Doc.group (
      Doc.concat [
        Doc.text "type ";
        Doc.text type_extension.otyext_name;
        type_params;
        Doc.text " +=";
        if type_extension.otyext_private = Asttypes.Private then
          Doc.text "private "
        else
          Doc.nil;
        print_out_constructors_doc type_extension.otyext_constructors;
      ]
    )

  let print_out_sig_item fmt out_sig_item =
    Format.pp_print_string fmt
      (Doc.to_string ~width:80 (print_out_sig_item_doc out_sig_item))

  let print_out_signature fmt signature =
    Format.pp_print_string fmt
      (Doc.to_string ~width:80 (print_out_signature_doc signature))

  let valid_float_lexeme s =
    let l = String.length s in
    let rec loop i =
      if i >= l then s ^ "." else
      match (s.[i] [@doesNotRaise]) with
      | '0' .. '9' | '-' -> loop (i+1)
      | _ -> s
    in loop 0

  let float_repres f =
    match classify_float f with
    | FP_nan -> "nan"
    | FP_infinite ->
      if f < 0.0 then "neg_infinity" else "infinity"
    | _ ->
      let float_val =
        let s1 = Printf.sprintf "%.12g" f in
        if f = (float_of_string [@doesNotRaise]) s1 then s1 else
        let s2 = Printf.sprintf "%.15g" f in
        if f = (float_of_string [@doesNotRaise]) s2 then s2 else
        Printf.sprintf "%.18g" f
      in valid_float_lexeme float_val

  let rec print_out_value_doc (out_value : Outcometree.out_value) =
    match out_value with
    | Oval_array out_values ->
      Doc.group (
        Doc.concat [
          Doc.lbracket;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_out_value_doc out_values
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rbracket;
        ]
      )
    | Oval_char c -> Doc.text ("'" ^ (Char.escaped c) ^ "'")
    | Oval_constr (out_ident, out_values) ->
      Doc.group (
        Doc.concat [
          print_out_ident_doc out_ident;
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_out_value_doc out_values
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ]
      )
    | Oval_ellipsis -> Doc.text "..."
    | Oval_int i -> Doc.text (Format.sprintf "%i" i)
    | Oval_int32 i -> Doc.text (Format.sprintf "%lil" i)
    | Oval_int64 i -> Doc.text (Format.sprintf "%LiL" i)
    | Oval_nativeint i -> Doc.text (Format.sprintf "%nin" i)
    | Oval_float f -> Doc.text (float_repres f)
    | Oval_list out_values ->
      Doc.group (
        Doc.concat [
          Doc.text "list[";
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_out_value_doc out_values
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rbracket;
        ]
      )
    | Oval_printer fn ->
      let fmt = Format.str_formatter in
      fn fmt;
      let str = Format.flush_str_formatter () in
      Doc.text str
    | Oval_record rows ->
      Doc.group (
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun (out_ident, out_value) -> Doc.group (
                    Doc.concat [
                      print_out_ident_doc out_ident;
                      Doc.text ": ";
                      print_out_value_doc out_value;
                    ]
                  )
                ) rows
              );
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ]
      )
    | Oval_string (txt, _sizeToPrint, _kind) ->
      Doc.text (escape_string_contents txt)
    | Oval_stuff txt -> Doc.text txt
    | Oval_tuple out_values ->
      Doc.group (
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_out_value_doc out_values
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ]
      )
    (* Not supported by NapkinScript *)
    | Oval_variant _ -> Doc.nil

  let print_out_exception_doc exc out_value =
    match exc with
    | Sys.Break -> Doc.text "Interrupted."
    | Out_of_memory -> Doc.text "Out of memory during evaluation."
    | Stack_overflow ->
      Doc.text "Stack overflow during evaluation (looping recursion?)."
    | _ ->
      Doc.group (
        Doc.indent(
          Doc.concat [
            Doc.text "Exception:";
            Doc.line;
            print_out_value_doc out_value;
          ]
        )
      )

  let print_out_phrase_signature signature =
    let rec loop signature acc =
     match signature with
     | [] -> List.rev acc
     | (Outcometree.Osig_typext(ext, Oext_first), None)::signature ->
        (* Gather together extension constructors *)
        let rec gather_extensions acc items =
          match items with
          |  (Outcometree.Osig_typext(ext, Oext_next), None)::items ->
              gather_extensions
                ((ext.oext_name, ext.oext_args, ext.oext_ret_type)::acc)
                items
          | _ -> (List.rev acc, items)
        in
        let exts, signature =
          gather_extensions
            [(ext.oext_name, ext.oext_args, ext.oext_ret_type)]
            signature
        in
        let te =
          { Outcometree.otyext_name = ext.oext_type_name;
            otyext_params = ext.oext_type_params;
            otyext_constructors = exts;
            otyext_private = ext.oext_private }
        in
        let doc = print_out_type_extension_doc te in
        loop signature (doc::acc)
     | (sig_item, opt_out_value)::signature ->
       let doc = match opt_out_value with
        | None ->
          print_out_sig_item_doc sig_item
        | Some out_value ->
          Doc.group (
            Doc.concat [
              print_out_sig_item_doc sig_item;
              Doc.text " = ";
              print_out_value_doc out_value;
            ]
          )
       in
       loop signature (doc::acc)
     in
     Doc.breakable_group ~force_break:true (
       Doc.join ~sep:Doc.line (loop signature [])
     )

  let print_out_phrase_doc (out_phrase : Outcometree.out_phrase) =
    match out_phrase with
    | Ophr_eval (out_value, out_type) ->
      Doc.group (
        Doc.concat [
          Doc.text "- : ";
          print_out_type_doc out_type;
          Doc.text " =";
          Doc.indent (
            Doc.concat [
              Doc.line;
              print_out_value_doc out_value;
            ]
          )
        ]
      )
    | Ophr_signature [] -> Doc.nil
    | Ophr_signature signature -> print_out_phrase_signature signature
    | Ophr_exception (exc, out_value) ->
      print_out_exception_doc exc out_value

  let print_out_phase fmt out_phrase =
    Format.pp_print_string fmt
      (Doc.to_string ~width:80 (print_out_phrase_doc out_phrase))

  let print_out_module_type fmt out_module_type =
    Format.pp_print_string fmt
      (Doc.to_string ~width:80 (print_out_module_type_doc out_module_type))

  let print_out_type_extension fmt type_extension =
    Format.pp_print_string fmt
      (Doc.to_string ~width:80 (print_out_type_extension_doc type_extension))

  let print_out_value fmt out_value =
    Format.pp_print_string fmt
      (Doc.to_string ~width:80 (print_out_value_doc out_value))

  (* Not supported in Napkin *)
  let print_out_class_type _fmt _ = ()

  let out_value = ref print_out_value
  let out_type = ref print_out_type
  let out_module_type = ref print_out_module_type
  let out_sig_item = ref print_out_sig_item
  let out_signature = ref print_out_signature
  let out_type_extension = ref print_out_type_extension
  let out_phrase = ref print_out_phase [@live]
  let out_class_type =  ref print_out_class_type
end

module Repl = struct
  let parse_toplevel_phrase filename =
    let src = IO.read_file filename in
    let p = Parser.make src filename in
    Parsetree.Ptop_def (NapkinScript.parse_implementation p)

  let type_and_print_outcome filename =
    Compmisc.init_path false;
    let env = Compmisc.initial_env () in
    try
      let sstr = match parse_toplevel_phrase filename with
      | Parsetree.Ptop_def sstr -> sstr
      | _ -> assert false
      in
      let (_str, signature, _newenv) = Typemod.type_toplevel_phrase env sstr in
      let out_sig_items = Printtyp.tree_of_signature signature in
      let fmt = Format.str_formatter in
      !OutcomePrinter.out_signature fmt out_sig_items;
      let result = Format.flush_str_formatter () in
      print_string result
    with
    | Typetexp.Error (_, _, err) ->
      let fmt = Format.str_formatter in
      Typetexp.report_error env fmt err;
      let result = Format.flush_str_formatter () in
      let () = print_endline result in
      ()
    | _ -> print_endline "catch all"
end

(* command line flags *)
module Clflags: sig
  val recover: bool ref
  val print: string ref
  val width: int ref
  val origin: string ref
  val files: string list ref
  val interface: bool ref
  val report: string ref

  val parse: unit -> unit
  val outcome: bool ref
end = struct
  let recover = ref false
  let width = ref 100

  let files = ref []
  let add_filename filename = files := filename::(!files)

  let print = ref ""
  let outcome = ref false
  let origin = ref ""
  let interface = ref false
  let report = ref "pretty"

  let usage = "Usage: napkinscript <options> <file>\nOptions are:"

  let spec = [
    ("-recover", Arg.Unit (fun () -> recover := true), "Emit partial ast");
    ("-print", Arg.String (fun txt -> print := txt), "Print either binary, ocaml or ast");
    ("-parse", Arg.String (fun txt -> origin := txt), "Parse ocaml or napkinscript");
    ("-outcome", Arg.Bool (fun print_outcome_tree -> outcome := print_outcome_tree), "print outcometree");
    ("-width", Arg.Int (fun w -> width := w), "Specify the line length that the printer will wrap on" );
    ("-interface", Arg.Unit (fun () -> interface := true), "Parse as interface");
    ("-report", Arg.String (fun txt -> report := txt), "Stylize errors and messages using color and context. Accepts `Pretty` and `Plain`. Default `Plain`")
  ]

  let parse () = Arg.parse spec add_filename usage
end

module Driver: sig
  val process_file:
       is_interface: bool
    -> width: int
    -> recover: bool
    -> origin:string
    -> target:string
    -> report:string
    -> string
    -> unit
end = struct
  type 'a file_kind =
    | Structure: Parsetree.structure file_kind
    | Signature: Parsetree.signature file_kind

  let parse_napkin (type a) (kind : a file_kind) p : a =
    match kind with
    | Structure -> NapkinScript.parse_implementation p
    | Signature -> NapkinScript.parse_specification p

  let extract_ocaml_string_data filename =
    let lexbuf = if String.length filename > 0 then
      IO.read_file filename |> Lexing.from_string
    else
      Lexing.from_channel stdin
    in
    let string_locs = ref [] in
    let rec next () =
      let token = Lexer.token_with_comments lexbuf in
      match token with
      | OcamlParser.STRING (_txt, None) ->
        let open Location in
        let loc = {
          loc_start = lexbuf.lex_start_p;
          loc_end = lexbuf.Lexing.lex_curr_p;
          loc_ghost = false;
        } in
        let len = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
        let txt = Bytes.to_string (
          (Bytes.sub [@doesNotRaise]) lexbuf.Lexing.lex_buffer loc.loc_start.pos_cnum len
        ) in
        string_locs := (txt, loc)::(!string_locs);
        next();
      | OcamlParser.EOF -> ()
      | _ -> next()
    in
    next();
    List.rev !string_locs

  let parse_ocaml (type a) (kind : a file_kind) filename : a =
    let lexbuf = if String.length filename > 0 then
      IO.read_file filename |> Lexing.from_string
    else
      Lexing.from_channel stdin
    in
    let string_data = extract_ocaml_string_data filename in
    match kind with
    | Structure ->
      Parse.implementation lexbuf
      |> ParsetreeCompatibility.replace_string_literal_structure string_data
      |> ParsetreeCompatibility.structure
    | Signature ->
      Parse.interface lexbuf
      |> ParsetreeCompatibility.replace_string_literal_signature string_data
      |> ParsetreeCompatibility.signature

  let parse_napkin_file ~destination kind filename =
    let src = if String.length filename > 0 then
      IO.read_file filename
    else
      IO.read_stdin ()
    in
    let p =
      let mode = match destination with
      | "napkinscript" | "ns" | "sexp" -> Parser.Default
      | _ -> Parser.ParseForTypeChecker
      in
      Parser.make ~mode src filename in
    let ast = parse_napkin kind p in
    let report = match p.diagnostics with
    | [] -> None
    | diagnostics -> Some(diagnostics)
    in
    (ast, report, p)

  let parse_ocaml_file kind filename =
    let ast = parse_ocaml kind filename in
    let lexbuf2 = if String.length filename > 0 then
      IO.read_file filename |> Lexing.from_string
    else
      Lexing.from_channel stdin
    in
    let comments =
      let rec next (prev_tok_end_pos : Lexing.position) comments lb =
        let token = Lexer.token_with_comments lb in
        match token with
        | OcamlParser.EOF -> comments
        | OcamlParser.COMMENT (txt, loc) ->
          let comment = Comment.from_ocaml_comment
            ~loc
            ~prev_tok_end_pos
            ~txt
          in
          next loc.Location.loc_end (comment::comments) lb
        | _ ->
          next lb.Lexing.lex_curr_p comments lb
      in
      let cmts = next lexbuf2.Lexing.lex_start_p [] lexbuf2 in
      cmts
    in
    let p = Parser.make "" filename in
    p.comments <- comments;
    (ast, None, p)

  let reason_filename = ref ""
  let comment_data = ref []
  let string_data = ref []

  let parse_reason_binary_from_stdin (type a) (kind : a file_kind) filename  :a  =
    let chan, close =
      match String.length filename == 0 with
      | true -> stdin, (fun _ -> ())
      | false ->
          let file_chan = open_in_bin filename in
          seek_in file_chan 0;
          file_chan, close_in_noerr
    in
    let ic = chan in
    let magic = match kind with
    | Structure -> Config.ast_impl_magic_number
    | Signature -> Config.ast_intf_magic_number
    in
    let buffer = (really_input_string [@doesNotRaise]) ic (String.length magic) in
    assert(buffer = magic);
    let filename = input_value ic in
    reason_filename := filename;
    let ast = input_value ic in
    close chan;

    let src =
      if String.length filename > 0 then IO.read_file filename
      else IO.read_stdin ()
    in

    let scanner = Scanner.make (Bytes.of_string src) filename in

    let rec next prev_end_pos scanner =
      let (start_pos, end_pos, token) = Scanner.scan scanner in
      match token with
      | Eof -> ()
      | Comment c ->
        Comment.set_prev_tok_end_pos c prev_end_pos;
        comment_data := c::(!comment_data);
        next end_pos scanner
      | String _ ->
        let loc = {Location.loc_start = start_pos; loc_end = end_pos; loc_ghost = false} in
        let len = end_pos.pos_cnum - start_pos.pos_cnum in
        let txt = (String.sub [@doesNotRaise]) src start_pos.pos_cnum len in
        string_data := (txt, loc)::(!string_data);
        next end_pos scanner
      | _ ->
        next end_pos scanner
    in

    next Lexing.dummy_pos scanner;

    match kind with
    | Structure ->
      ast
      |> ParsetreeCompatibility.replace_string_literal_structure !string_data
      |> ParsetreeCompatibility.normalize_reason_arity_structure ~for_printer:true
      |> ParsetreeCompatibility.structure
    | Signature ->
      ast
      |> ParsetreeCompatibility.replace_string_literal_signature !string_data
      |> ParsetreeCompatibility.normalize_reason_arity_signature ~for_printer:true
      |> ParsetreeCompatibility.signature

  let is_reason_doc_comment (comment: Comment.t) =
    let content = Comment.txt comment in
    let len = String.length content in
    if len = 0 then true
    else if len >= 2 && (String.unsafe_get content 0 = '*' && String.unsafe_get content 1 = '*') then false
    else if len >= 1 && (String.unsafe_get content 0 = '*') then true
    else false


  let parse_reason_binary kind filename =
    let ast = parse_reason_binary_from_stdin kind filename in
    let p = Parser.make "" !reason_filename in
    p.comments <- List.filter (fun c -> not (is_reason_doc_comment c)) !comment_data;
    (ast, None, p)

  let parse_implementation ~origin ~destination filename =
    match origin with
    | "ml" | "ocaml" ->
      parse_ocaml_file Structure filename
    | "reasonBinary" ->
      parse_reason_binary Structure filename
    | _ ->
      parse_napkin_file ~destination Structure filename

  let parse_interface ~destination ~origin filename =
    match origin with
    | "ml" | "ocaml" ->
      parse_ocaml_file Signature filename
    | "reasonBinary" ->
      parse_reason_binary Signature filename
    | _ ->
      parse_napkin_file ~destination Signature filename

  let process ~report_style parse_fn print_fn recover filename =
    let (ast, report, parser_state) = parse_fn filename in
    match report with
    | Some report when recover = true ->
      print_fn ast parser_state;
      prerr_string (
        Diagnostics.string_of_report
          ~style:(Diagnostics.parse_report_style report_style)
          report (Bytes.to_string parser_state.Parser.scanner.src)
      );
    | Some report ->
      prerr_string (
        Diagnostics.string_of_report
          ~style:(Diagnostics.parse_report_style report_style)
          report (Bytes.to_string parser_state.Parser.scanner.src)
      );
      exit 1
    | None ->
      print_fn ast parser_state

  type action =
    | ProcessImplementation
    | ProcessInterface

  let print_implementation ~target ~width filename ast _parserState =
    match target with
    | "ml" | "ocaml" ->
      Pprintast.structure Format.std_formatter ast
    | "ns" | "napkinscript" ->
      Printer.print_implementation ~width ast (List.rev _parserState.Parser.comments)
    | "ast" ->
      Printast.implementation Format.std_formatter ast
    | "sexp" ->
      ast |> SexpAst.implementation |> Sexp.to_string |> print_string
    | _ -> (* default binary *)
      output_string stdout Config.ast_impl_magic_number;
      output_value stdout filename;
      output_value stdout ast

  let print_interface ~target ~width filename ast _parserState =
    match target with
    | "ml" | "ocaml" -> Pprintast.signature Format.std_formatter ast
    | "ns" | "napkinscript" ->
      Printer.print_interface ~width ast (List.rev _parserState.Parser.comments)
    | "ast" -> Printast.interface Format.std_formatter ast
    | "sexp" ->
      ast |> SexpAst.interface |> Sexp.to_string |> print_string
    | _ -> (* default binary *)
      output_string stdout Config.ast_intf_magic_number;
      output_value stdout filename;
      output_value stdout ast

  let process_file ~is_interface ~width ~recover ~origin ~target ~report filename =
    try
      let len = String.length filename in
      let action =
        if is_interface || len > 0 && (String.get [@doesNotRaise]) filename (len - 1) = 'i' then
          ProcessInterface
        else ProcessImplementation
      in
      match action with
      | ProcessImplementation ->
        process
          ~report_style:report
          (parse_implementation ~origin ~destination:target)
          (print_implementation ~target ~width filename) recover filename
      | ProcessInterface ->
        process
          ~report_style:report
          (parse_interface ~origin ~destination:target)
          (print_interface ~target ~width filename) recover filename
    with
    | Failure txt ->
      prerr_string txt;
      prerr_newline();
      exit 1
    | _ -> exit 1
end

let () =
  Clflags.parse ();
  if !Clflags.outcome then (
    Repl.type_and_print_outcome (List.hd !Clflags.files)
  ) else (
    let () = match !Clflags.files with
    | (_file::_) as files ->
      List.iter (fun filename ->
        Driver.process_file
          ~is_interface:!Clflags.interface
          ~width:!Clflags.width
          ~recover:!Clflags.recover
          ~target:!Clflags.print
          ~origin:!Clflags.origin
          ~report:!Clflags.report
          filename
      ) files;
    | [] ->
      Driver.process_file
        ~is_interface:!Clflags.interface
        ~width:!Clflags.width
        ~recover:!Clflags.recover
        ~target:!Clflags.print
        ~origin:!Clflags.origin
        ~report:!Clflags.report
        ""
    in
    exit 0
  )
