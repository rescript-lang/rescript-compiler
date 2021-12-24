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

  type lineStyle =
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
    | LineBreak of lineStyle
    | Group of {shouldBreak: bool; doc: t}
    | CustomLayout of t list
    | BreakParent
    (* | Cursor *)

  let nil = Nil
  let line = LineBreak Classic
  let hardLine = LineBreak Hard
  let softLine = LineBreak Soft
  let text s = Text s
  let concat l = Concat l
  let indent d = Indent d
  let ifBreaks t f = IfBreaks {yes = t; no = f}
  let lineSuffix d = LineSuffix d
  let group d = Group {shouldBreak = false; doc = d}
  let breakableGroup ~forceBreak d = Group {shouldBreak = forceBreak; doc = d}
  let customLayout gs = CustomLayout gs
  let breakParent = BreakParent
  (* let cursor = Cursor *)

  let space = Text " "
  let comma = Text ","
  let dot = Text "."
  let dotdot = Text ".."
  let dotdotdot = Text "..."
  let lessThan = Text "<"
  let greaterThan = Text ">"
  let lbrace = Text "{"
  let rbrace = Text "}"
  let lparen = Text "("
  let rparen = Text ")"
  let lbracket = Text "["
  let rbracket = Text "]"
  let question = Text "?"
  let tilde = Text "~"
  let equal = Text "="
  let trailingComma = IfBreaks {yes = comma; no = nil}
  let doubleQuote = Text "\""

  let propagateForcedBreaks doc =
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
      let (childForcesBreak, newChildren) = walk children in
      (childForcesBreak, Indent newChildren)
    | IfBreaks {yes = trueDoc; no = falseDoc} ->
      let (falseForceBreak, falseDoc) = walk falseDoc in
      if falseForceBreak then
        let (_, trueDoc) = walk trueDoc in
        (true, trueDoc)
      else
        let forceBreak, trueDoc = walk trueDoc in
        (forceBreak, IfBreaks {yes = trueDoc; no = falseDoc})
    | Group {shouldBreak = forceBreak; doc = children} ->
      let (childForcesBreak, newChildren) = walk children in
      let shouldBreak = forceBreak || childForcesBreak in
      (shouldBreak, Group {shouldBreak; doc = newChildren})
    | Concat children ->
      let (forceBreak, newChildren) = List.fold_left (fun (forceBreak, newChildren) child ->
        let (childForcesBreak, newChild) = walk child in
        (forceBreak || childForcesBreak, newChild::newChildren)
      ) (false, []) children
      in
      (forceBreak, Concat (List.rev newChildren))
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
    let (_, processedDoc) = walk doc in
    processedDoc

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
    | (ind, mode, Group {shouldBreak = forceBreak; doc})::rest ->
      let mode = if forceBreak then Break else mode in
      fits w ((ind, mode, doc)::rest)
    | (ind, mode, IfBreaks {yes = breakDoc; no = flatDoc})::rest ->
        if mode = Break then
          fits w ((ind, mode, breakDoc)::rest)
        else
          fits w ((ind, mode, flatDoc)::rest)
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

  let toString ~width doc =
    let doc = propagateForcedBreaks doc in
    let buffer = MiniBuffer.create 1000 in

    let rec process ~pos lineSuffices stack =
      match stack with
      | ((ind, mode, doc) as cmd)::rest ->
        begin match doc with
        | Nil | BreakParent ->
          process ~pos lineSuffices rest
        | Text txt ->
          MiniBuffer.add_string buffer txt;
          process ~pos:(String.length txt + pos) lineSuffices rest
        | LineSuffix doc ->
          process ~pos ((ind, mode, doc)::lineSuffices) rest
        | Concat docs ->
          let ops = List.map (fun doc -> (ind, mode, doc)) docs in
          process ~pos lineSuffices (List.append ops rest)
        | Indent doc ->
          process ~pos lineSuffices ((ind + 2, mode, doc)::rest)
        | IfBreaks {yes = breakDoc; no = flatDoc} ->
          if mode = Break then
            process ~pos lineSuffices ((ind, mode, breakDoc)::rest)
          else
            process ~pos lineSuffices ((ind, mode, flatDoc)::rest)
        | LineBreak lineStyle  ->
          if mode = Break then (
            begin match lineSuffices with
            | [] ->
              MiniBuffer.flush_newline buffer;
              MiniBuffer.add_string buffer (String.make ind ' ' [@doesNotRaise]);
              process ~pos:ind [] rest
            | _docs ->
              process ~pos:ind [] (List.concat [List.rev lineSuffices; cmd::rest])
            end
          ) else (* mode = Flat *) (
            let pos = match lineStyle with
            | Classic -> MiniBuffer.add_string buffer " "; pos + 1
            | Hard -> MiniBuffer.flush_newline buffer; 0
            | Soft -> pos
            in
            process ~pos lineSuffices rest
          )
        | Group {shouldBreak; doc} ->
          if shouldBreak || not (fits (width - pos) ((ind, Flat, doc)::rest)) then
            process ~pos lineSuffices ((ind, Break, doc)::rest)
          else
            process ~pos lineSuffices ((ind, Flat, doc)::rest)
        | CustomLayout docs ->
          let rec findGroupThatFits groups = match groups with
          | [] -> Nil
          | [lastGroup] -> lastGroup
          | doc::docs ->
            if (fits (width - pos) ((ind, Flat, doc)::rest)) then
              doc
            else
              findGroupThatFits docs
          in
          let doc = findGroupThatFits docs in
          process ~pos lineSuffices ((ind, Flat, doc)::rest)
        end
      | [] ->
        begin match lineSuffices with
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
    let rec toDoc = function
      | Nil -> text "nil"
      | BreakParent -> text "breakparent"
      | Text txt -> text ("text(" ^ txt ^ ")")
      | LineSuffix doc -> group(
          concat [
            text "linesuffix(";
            indent (
              concat [line; toDoc doc]
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
                  (List.map toDoc docs) ;
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
                  (List.map toDoc docs) ;
              ]
            );
            line;
            text ")"
          ]
        )
      | Indent doc ->
          concat [
            text "indent(";
            softLine;
            toDoc doc;
            softLine;
            text ")";
          ]
      | IfBreaks {yes = trueDoc; no = falseDoc} ->
        group(
          concat [
            text "ifBreaks(";
            indent (
              concat [
                line;
                toDoc trueDoc;
                concat [text ",";  line];
                toDoc falseDoc;
              ]
            );
            line;
            text ")"
          ]
        )
      | LineBreak break ->
        let breakTxt = match break with
          | Classic -> "Classic"
          | Soft -> "Soft"
          | Hard -> "Hard"
        in
        text ("LineBreak(" ^ breakTxt ^ ")")
      | Group {shouldBreak; doc} ->
        group(
          concat [
            text "Group(";
            indent (
              concat [
                line;
                text ("shouldBreak: " ^ (string_of_bool shouldBreak));
                concat [text ",";  line];
                toDoc doc;
              ]
            );
            line;
            text ")"
          ]
        )
    in
    let doc = toDoc t in
    toString ~width:10 doc |> print_endline
    [@@live]
end

module Sexp: sig
  type t

  val atom: string -> t
  val list: t list -> t
  val toString: t -> string
end = struct
  type t =
    | Atom of string
    | List of t list

  let atom s = Atom s
  let list l = List l

  let rec toDoc t =
    match t with
    | Atom s -> Doc.text s
    | List [] -> Doc.text "()"
    | List [sexpr] -> Doc.concat [Doc.lparen; toDoc sexpr; Doc.rparen;]
    | List (hd::tail) ->
      Doc.group (
        Doc.concat [
          Doc.lparen;
          toDoc hd;
          Doc.indent (
            Doc.concat [
              Doc.line;
              Doc.join ~sep:Doc.line (List.map toDoc tail);
            ]
          );
          Doc.rparen;
        ]
      )

  let toString sexpr =
    let doc = toDoc sexpr in
    Doc.toString ~width:80 doc
end

module SexpAst: sig
  val implementation: Parsetree.structure -> Sexp.t
  val interface: Parsetree.signature -> Sexp.t
end = struct
  open Parsetree

  let mapEmpty ~f items =
    match items with
    | [] -> [Sexp.list []]
    | items -> List.map f items

  let string txt =
    Sexp.atom ("\"" ^ txt ^ "\"")

  let char c =
    Sexp.atom ("'" ^ (Char.escaped c) ^ "'")

  let optChar oc =
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

  let closedFlag flag = match flag with
    | Asttypes.Closed -> Sexp.atom "Closed"
    | Open -> Sexp.atom "Open"

  let directionFlag flag = match flag with
    | Asttypes.Upto -> Sexp.atom "Upto"
    | Downto -> Sexp.atom "Downto"

  let recFlag flag = match flag with
    | Asttypes.Recursive -> Sexp.atom "Recursive"
    | Nonrecursive -> Sexp.atom "Nonrecursive"

  let overrideFlag flag = match flag with
    | Asttypes.Override -> Sexp.atom "Override"
    | Fresh -> Sexp.atom "Fresh"

  let privateFlag flag = match flag with
    | Asttypes.Public -> Sexp.atom "Public"
    | Private -> Sexp.atom "Private"

  let mutableFlag flag = match flag with
    | Asttypes.Immutable -> Sexp.atom "Immutable"
    | Mutable -> Sexp.atom "Mutable"

   let variance v = match v with
     | Asttypes.Covariant -> Sexp.atom "Covariant"
     | Contravariant -> Sexp.atom "Contravariant"
     | Invariant -> Sexp.atom "Invariant"

  let argLabel lbl = match lbl with
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
        optChar tag;
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
        optChar tag;
      ]
    in
      Sexp.list [
        Sexp.atom "constant";
        sexpr
      ]

  let rec structure s =
    Sexp.list (
      (Sexp.atom "structure")::(List.map structureItem s)
    )

  and structureItem si =
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
        recFlag flag;
        Sexp.list (mapEmpty ~f:valueBinding vbs)
      ]
    | Pstr_primitive (vd) ->
      Sexp.list [
        Sexp.atom "Pstr_primitive";
        valueDescription vd;
      ]
    | Pstr_type (flag, tds) ->
      Sexp.list [
        Sexp.atom "Pstr_type";
        recFlag flag;
        Sexp.list (mapEmpty ~f:typeDeclaration tds)
      ]
    | Pstr_typext typext ->
      Sexp.list [
        Sexp.atom "Pstr_type";
        typeExtension typext;
      ]
    | Pstr_exception ec ->
      Sexp.list [
        Sexp.atom "Pstr_exception";
        extensionConstructor ec;
      ]
    | Pstr_module mb ->
      Sexp.list [
        Sexp.atom "Pstr_module";
        moduleBinding mb;
      ]
    | Pstr_recmodule mbs ->
      Sexp.list [
        Sexp.atom "Pstr_recmodule";
        Sexp.list (mapEmpty ~f:moduleBinding mbs);
      ]
    | Pstr_modtype modTypDecl ->
      Sexp.list [
        Sexp.atom "Pstr_modtype";
        moduleTypeDeclaration modTypDecl;
      ]
    | Pstr_open openDesc ->
      Sexp.list [
        Sexp.atom "Pstr_open";
        openDescription openDesc;
      ]
    | Pstr_class _ -> Sexp.atom "Pstr_class"
    | Pstr_class_type _ -> Sexp.atom "Pstr_class_type"
    | Pstr_include id ->
      Sexp.list [
        Sexp.atom "Pstr_include";
        includeDeclaration id;
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

  and includeDeclaration id =
    Sexp.list [
      Sexp.atom "include_declaration";
      moduleExpression id.pincl_mod;
      attributes id.pincl_attributes;
    ]

  and openDescription od =
    Sexp.list [
      Sexp.atom "open_description";
      longident od.popen_lid.Asttypes.txt;
      attributes od.popen_attributes;
    ]

  and moduleTypeDeclaration mtd =
    Sexp.list [
      Sexp.atom "module_type_declaration";
      string mtd.pmtd_name.Asttypes.txt;
      (match mtd.pmtd_type with
      | None -> Sexp.atom "None"
      | Some modType -> Sexp.list [
          Sexp.atom "Some";
          moduleType modType;
      ]);
      attributes mtd.pmtd_attributes;
    ]

  and moduleBinding mb =
    Sexp.list [
      Sexp.atom "module_binding";
      string mb.pmb_name.Asttypes.txt;
      moduleExpression mb.pmb_expr;
      attributes mb.pmb_attributes;
    ]

  and moduleExpression me =
    let desc = match me.pmod_desc with
    | Pmod_ident modName ->
      Sexp.list [
        Sexp.atom "Pmod_ident";
        longident modName.Asttypes.txt;
      ]
    | Pmod_structure s ->
      Sexp.list [
        Sexp.atom "Pmod_structure";
        structure s;
      ]
    | Pmod_functor (lbl, optModType, modExpr) ->
      Sexp.list [
        Sexp.atom "Pmod_functor";
        string lbl.Asttypes.txt;
        (match optModType with
        | None -> Sexp.atom "None"
        | Some modType -> Sexp.list [
            Sexp.atom "Some";
            moduleType modType;
        ]);
        moduleExpression modExpr;
      ]
    | Pmod_apply (callModExpr, modExprArg) ->
      Sexp.list [
        Sexp.atom "Pmod_apply";
        moduleExpression callModExpr;
        moduleExpression modExprArg;
      ]
    | Pmod_constraint (modExpr, modType) ->
      Sexp.list [
        Sexp.atom "Pmod_constraint";
        moduleExpression modExpr;
        moduleType modType;
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

  and moduleType mt =
    let desc = match mt.pmty_desc with
    | Pmty_ident longidentLoc ->
      Sexp.list [
        Sexp.atom "Pmty_ident";
        longident longidentLoc.Asttypes.txt;
      ]
    | Pmty_signature s ->
      Sexp.list [
        Sexp.atom "Pmty_signature";
        signature s;
      ]
    | Pmty_functor (lbl, optModType, modType) ->
      Sexp.list [
        Sexp.atom "Pmty_functor";
        string lbl.Asttypes.txt;
        (match optModType with
        | None -> Sexp.atom "None"
        | Some modType -> Sexp.list [
            Sexp.atom "Some";
            moduleType modType;
        ]);
        moduleType modType;
      ]
    | Pmty_alias longidentLoc ->
      Sexp.list [
        Sexp.atom "Pmty_alias";
        longident longidentLoc.Asttypes.txt;
      ]
    | Pmty_extension ext ->
      Sexp.list [
        Sexp.atom "Pmty_extension";
        extension ext;
      ]
    | Pmty_typeof modExpr ->
      Sexp.list [
        Sexp.atom "Pmty_typeof";
        moduleExpression modExpr;
      ]
    | Pmty_with (modType, withConstraints) ->
      Sexp.list [
        Sexp.atom "Pmty_with";
        moduleType modType;
        Sexp.list (mapEmpty ~f:withConstraint withConstraints);
      ]
    in
    Sexp.list [
      Sexp.atom "module_type";
      desc;
      attributes mt.pmty_attributes;
    ]

  and withConstraint wc = match wc with
    | Pwith_type (longidentLoc, td) ->
      Sexp.list [
        Sexp.atom "Pmty_with";
        longident longidentLoc.Asttypes.txt;
        typeDeclaration td;
      ]
    | Pwith_module (l1, l2) ->
      Sexp.list [
        Sexp.atom "Pwith_module";
        longident l1.Asttypes.txt;
        longident l2.Asttypes.txt;
      ]
    | Pwith_typesubst (longidentLoc, td) ->
      Sexp.list [
        Sexp.atom "Pwith_typesubst";
        longident longidentLoc.Asttypes.txt;
        typeDeclaration td;
      ]
    | Pwith_modsubst (l1, l2) ->
      Sexp.list [
        Sexp.atom "Pwith_modsubst";
        longident l1.Asttypes.txt;
        longident l2.Asttypes.txt;
      ]

  and signature s =
    Sexp.list (
      (Sexp.atom "signature")::(List.map signatureItem s)
    )

  and signatureItem si =
    let descr = match si.psig_desc with
    | Psig_value vd ->
      Sexp.list [
        Sexp.atom "Psig_value";
        valueDescription vd;
      ]
    | Psig_type (flag, typeDeclarations) ->
      Sexp.list [
        Sexp.atom "Psig_type";
        recFlag flag;
        Sexp.list (mapEmpty ~f:typeDeclaration typeDeclarations);
      ]
    | Psig_typext typExt ->
      Sexp.list [
        Sexp.atom "Psig_typext";
        typeExtension typExt;
      ]
    | Psig_exception extConstr ->
      Sexp.list [
        Sexp.atom "Psig_exception";
        extensionConstructor extConstr;
      ]
    | Psig_module modDecl ->
      Sexp.list [
        Sexp.atom "Psig_module";
        moduleDeclaration modDecl;
      ]
    | Psig_recmodule modDecls ->
      Sexp.list [
        Sexp.atom "Psig_recmodule";
        Sexp.list (mapEmpty ~f:moduleDeclaration modDecls);
      ]
    | Psig_modtype modTypDecl ->
      Sexp.list [
        Sexp.atom "Psig_modtype";
        moduleTypeDeclaration modTypDecl;
      ]
    | Psig_open openDesc ->
      Sexp.list [
        Sexp.atom "Psig_open";
        openDescription openDesc;
      ]
    | Psig_include inclDecl ->
      Sexp.list [
        Sexp.atom "Psig_include";
        includeDescription inclDecl
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

  and includeDescription id =
    Sexp.list [
      Sexp.atom "include_description";
      moduleType id.pincl_mod;
      attributes id.pincl_attributes;
    ]

  and moduleDeclaration md =
    Sexp.list [
      Sexp.atom "module_declaration";
      string md.pmd_name.Asttypes.txt;
      moduleType md.pmd_type;
      attributes md.pmd_attributes;
    ]

  and valueBinding vb =
    Sexp.list [
      Sexp.atom "value_binding";
      pattern vb.pvb_pat;
      expression vb.pvb_expr;
      attributes vb.pvb_attributes;
    ]

  and valueDescription vd =
    Sexp.list [
      Sexp.atom "value_description";
      string vd.pval_name.Asttypes.txt;
      coreType vd.pval_type;
      Sexp.list (mapEmpty ~f:string vd.pval_prim);
      attributes vd.pval_attributes;
    ]

  and typeDeclaration td =
    Sexp.list [
      Sexp.atom "type_declaration";
      string td.ptype_name.Asttypes.txt;
      Sexp.list [
        Sexp.atom "ptype_params";
        Sexp.list (mapEmpty ~f:(fun (typexpr, var) ->
          Sexp.list [
            coreType typexpr;
            variance var;
          ]) td.ptype_params)
      ];
      Sexp.list [
        Sexp.atom "ptype_cstrs";
        Sexp.list (mapEmpty ~f:(fun (typ1, typ2, _loc) ->
          Sexp.list [
            coreType typ1;
            coreType typ2;
          ]) td.ptype_cstrs)
      ];
      Sexp.list [
        Sexp.atom "ptype_kind";
        typeKind td.ptype_kind;
      ];
      Sexp.list [
        Sexp.atom "ptype_manifest";
        match td.ptype_manifest with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            coreType typ;
          ]
      ];
      Sexp.list [
        Sexp.atom "ptype_private";
        privateFlag td.ptype_private;
      ];
      attributes td.ptype_attributes;
    ]

  and extensionConstructor ec =
    Sexp.list [
      Sexp.atom "extension_constructor";
      string ec.pext_name.Asttypes.txt;
      extensionConstructorKind ec.pext_kind;
      attributes ec.pext_attributes;
    ]

  and extensionConstructorKind kind = match kind with
    | Pext_decl (args, optTypExpr) ->
      Sexp.list [
        Sexp.atom "Pext_decl";
        constructorArguments args;
        match optTypExpr with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            coreType typ;
          ]
      ]
  | Pext_rebind longidentLoc ->
    Sexp.list [
      Sexp.atom "Pext_rebind";
      longident longidentLoc.Asttypes.txt;
    ]

  and typeExtension te =
    Sexp.list [
      Sexp.atom "type_extension";
      Sexp.list [
        Sexp.atom "ptyext_path";
        longident te.ptyext_path.Asttypes.txt;
      ];
      Sexp.list [
        Sexp.atom "ptyext_parms";
        Sexp.list (mapEmpty ~f:(fun (typexpr, var) ->
          Sexp.list [
            coreType typexpr;
            variance var;
          ]) te.ptyext_params)
      ];
      Sexp.list [
        Sexp.atom "ptyext_constructors";
        Sexp.list (mapEmpty ~f:extensionConstructor te.ptyext_constructors);
      ];
      Sexp.list [
        Sexp.atom "ptyext_private";
        privateFlag te.ptyext_private;
      ];
      attributes te.ptyext_attributes;
    ]

  and typeKind kind = match kind with
    | Ptype_abstract -> Sexp.atom "Ptype_abstract"
    | Ptype_variant constrDecls ->
      Sexp.list [
        Sexp.atom "Ptype_variant";
        Sexp.list (mapEmpty ~f:constructorDeclaration constrDecls);
      ]
    | Ptype_record lblDecls ->
      Sexp.list [
        Sexp.atom "Ptype_record";
        Sexp.list (mapEmpty ~f:labelDeclaration lblDecls);
      ]
    | Ptype_open -> Sexp.atom "Ptype_open"

  and constructorDeclaration cd =
    Sexp.list [
      Sexp.atom "constructor_declaration";
      string cd.pcd_name.Asttypes.txt;
      Sexp.list [
        Sexp.atom "pcd_args";
        constructorArguments cd.pcd_args;
      ];
      Sexp.list [
        Sexp.atom "pcd_res";
        match cd.pcd_res with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            coreType typ;
          ]
      ];
      attributes cd.pcd_attributes;
    ]

  and constructorArguments args = match args with
    | Pcstr_tuple types ->
      Sexp.list [
        Sexp.atom "Pcstr_tuple";
        Sexp.list (mapEmpty ~f:coreType types)
      ]
    | Pcstr_record lds ->
      Sexp.list [
        Sexp.atom "Pcstr_record";
        Sexp.list (mapEmpty ~f:labelDeclaration lds)
      ]

  and labelDeclaration ld =
    Sexp.list [
      Sexp.atom "label_declaration";
      string ld.pld_name.Asttypes.txt;
      mutableFlag ld.pld_mutable;
      coreType ld.pld_type;
      attributes ld.pld_attributes;
    ]

  and expression expr =
    let desc = match expr.pexp_desc with
    | Pexp_ident longidentLoc ->
      Sexp.list [
        Sexp.atom "Pexp_ident";
        longident longidentLoc.Asttypes.txt;
      ]
    | Pexp_constant c ->
      Sexp.list [
        Sexp.atom "Pexp_constant";
        constant c
      ]
    | Pexp_let (flag, vbs, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_let";
        recFlag flag;
        Sexp.list (mapEmpty ~f:valueBinding vbs);
        expression expr;
      ]
    | Pexp_function cases ->
      Sexp.list [
        Sexp.atom "Pexp_function";
        Sexp.list (mapEmpty ~f:case cases);
      ]
    | Pexp_fun (argLbl, exprOpt, pat, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_fun";
        argLabel argLbl;
        (match exprOpt with
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
        Sexp.list (mapEmpty ~f:(fun (argLbl, expr) -> Sexp.list [
          argLabel argLbl;
          expression expr
        ]) args);
      ]
    | Pexp_match (expr, cases) ->
      Sexp.list [
        Sexp.atom "Pexp_match";
        expression expr;
        Sexp.list (mapEmpty ~f:case cases);
      ]
    | Pexp_try (expr, cases) ->
      Sexp.list [
        Sexp.atom "Pexp_try";
        expression expr;
        Sexp.list (mapEmpty ~f:case cases);
      ]
    | Pexp_tuple exprs ->
      Sexp.list [
        Sexp.atom "Pexp_tuple";
        Sexp.list (mapEmpty ~f:expression exprs);
      ]
    | Pexp_construct (longidentLoc, exprOpt) ->
      Sexp.list [
        Sexp.atom "Pexp_construct";
        longident longidentLoc.Asttypes.txt;
        match exprOpt with
        | None -> Sexp.atom "None"
        | Some expr ->
          Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]
      ]
    | Pexp_variant (lbl, exprOpt) ->
      Sexp.list [
        Sexp.atom "Pexp_variant";
        string lbl;
        match exprOpt with
        | None -> Sexp.atom "None"
        | Some expr ->
          Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]
      ]
    | Pexp_record (rows, optExpr) ->
      Sexp.list [
        Sexp.atom "Pexp_record";
        Sexp.list (mapEmpty ~f:(fun (longidentLoc, expr) -> Sexp.list [
          longident longidentLoc.Asttypes.txt;
          expression expr;
        ]) rows);
        (match optExpr with
        | None -> Sexp.atom "None"
        | Some expr ->
          Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]);
      ]
    | Pexp_field (expr, longidentLoc) ->
      Sexp.list [
        Sexp.atom "Pexp_field";
        expression expr;
        longident longidentLoc.Asttypes.txt;
      ]
    | Pexp_setfield (expr1, longidentLoc, expr2) ->
      Sexp.list [
        Sexp.atom "Pexp_setfield";
        expression expr1;
        longident longidentLoc.Asttypes.txt;
        expression expr2;
      ]
    | Pexp_array exprs ->
      Sexp.list [
        Sexp.atom "Pexp_array";
        Sexp.list (mapEmpty ~f:expression exprs);
      ]
    | Pexp_ifthenelse (expr1, expr2, optExpr) ->
      Sexp.list [
        Sexp.atom "Pexp_ifthenelse";
        expression expr1;
        expression expr2;
        (match optExpr with
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
        directionFlag flag;
        expression e3;
      ]
    | Pexp_constraint (expr, typexpr) ->
      Sexp.list [
        Sexp.atom "Pexp_constraint";
        expression expr;
        coreType typexpr;
      ]
    | Pexp_coerce (expr, optTyp, typexpr) ->
      Sexp.list [
        Sexp.atom "Pexp_coerce";
        expression expr;
        (match optTyp with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            coreType typ;
        ]);
        coreType typexpr;
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
    | Pexp_letmodule (modName, modExpr, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_letmodule";
        string modName.Asttypes.txt;
        moduleExpression modExpr;
        expression expr;
      ]
    | Pexp_letexception (extConstr, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_letexception";
        extensionConstructor extConstr;
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
    | Pexp_pack modExpr ->
      Sexp.list [
        Sexp.atom "Pexp_pack";
        moduleExpression modExpr;
      ]
    | Pexp_open (flag, longidentLoc, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_open";
        overrideFlag flag;
        longident longidentLoc.Asttypes.txt;
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
        Sexp.list (mapEmpty ~f:pattern patterns);
      ]
    | Ppat_construct (longidentLoc, optPattern) ->
      Sexp.list [
        Sexp.atom "Ppat_construct";
        longident longidentLoc.Location.txt;
        match optPattern with
        | None -> Sexp.atom "None"
        | Some p -> Sexp.list [
            Sexp.atom "some";
            pattern p;
          ]
      ]
    | Ppat_variant (lbl, optPattern) ->
      Sexp.list [
        Sexp.atom "Ppat_variant";
        string lbl;
        match optPattern with
        | None -> Sexp.atom "None"
        | Some p -> Sexp.list [
            Sexp.atom "Some";
            pattern p;
          ]
      ]
    | Ppat_record (rows, flag) ->
      Sexp.list [
        Sexp.atom "Ppat_record";
        closedFlag flag;
        Sexp.list (mapEmpty ~f:(fun (longidentLoc, p) ->
          Sexp.list [
            longident longidentLoc.Location.txt;
            pattern p;
          ]
        ) rows)
      ]
    | Ppat_array patterns ->
      Sexp.list [
        Sexp.atom "Ppat_array";
        Sexp.list (mapEmpty ~f:pattern patterns);
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
        coreType typexpr;
      ]
    | Ppat_type longidentLoc ->
      Sexp.list [
        Sexp.atom "Ppat_type";
        longident longidentLoc.Location.txt
      ]
    | Ppat_lazy p ->
      Sexp.list [
        Sexp.atom "Ppat_lazy";
        pattern p;
      ]
    | Ppat_unpack stringLoc ->
      Sexp.list [
        Sexp.atom "Ppat_unpack";
        string stringLoc.Location.txt;
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
    | Ppat_open (longidentLoc, p) ->
      Sexp.list [
        Sexp.atom "Ppat_open";
        longident longidentLoc.Location.txt;
        pattern p;
      ]
    in
    Sexp.list [
      Sexp.atom "pattern";
      descr;
    ]

  and objectField field = match field with
  | Otag (lblLoc, attrs, typexpr) ->
    Sexp.list [
      Sexp.atom "Otag";
      string lblLoc.txt;
      attributes attrs;
      coreType typexpr;
    ]
  | Oinherit typexpr ->
    Sexp.list [
      Sexp.atom "Oinherit";
      coreType typexpr;
    ]

  and rowField field = match field with
    | Rtag (labelLoc, attrs, truth, types) ->
      Sexp.list [
        Sexp.atom "Rtag";
        string labelLoc.txt;
        attributes attrs;
        Sexp.atom (if truth then "true" else "false");
        Sexp.list (mapEmpty ~f:coreType types);
      ]
    | Rinherit typexpr ->
      Sexp.list [
        Sexp.atom "Rinherit";
        coreType typexpr;
      ]

  and packageType (modNameLoc, packageConstraints) =
    Sexp.list [
      Sexp.atom "package_type";
      longident modNameLoc.Asttypes.txt;
      Sexp.list (mapEmpty ~f:(fun (modNameLoc, typexpr) ->
        Sexp.list [
          longident modNameLoc.Asttypes.txt;
          coreType typexpr;
        ]
      ) packageConstraints)
    ]

  and coreType typexpr =
    let desc = match typexpr.ptyp_desc with
      | Ptyp_any -> Sexp.atom "Ptyp_any"
      | Ptyp_var var -> Sexp.list [
          Sexp.atom "Ptyp_var";
          string  var
        ]
      | Ptyp_arrow (argLbl, typ1, typ2) ->
        Sexp.list [
          Sexp.atom "Ptyp_arrow";
          argLabel argLbl;
          coreType typ1;
          coreType typ2;
        ]
      | Ptyp_tuple types ->
        Sexp.list [
          Sexp.atom "Ptyp_tuple";
          Sexp.list (mapEmpty ~f:coreType types);
        ]
      | Ptyp_constr (longidentLoc, types) ->
        Sexp.list [
          Sexp.atom "Ptyp_constr";
          longident longidentLoc.txt;
          Sexp.list (mapEmpty ~f:coreType types);
        ]
      | Ptyp_alias (typexpr, alias) ->
        Sexp.list [
          Sexp.atom "Ptyp_alias";
          coreType typexpr;
          string alias;
        ]
      | Ptyp_object (fields, flag) ->
        Sexp.list [
          Sexp.atom "Ptyp_object";
          closedFlag flag;
          Sexp.list (mapEmpty ~f:objectField fields)
        ]
      | Ptyp_class (longidentLoc, types) ->
        Sexp.list [
          Sexp.atom "Ptyp_class";
          longident longidentLoc.Location.txt;
          Sexp.list (mapEmpty ~f:coreType types)
        ]
      | Ptyp_variant (fields, flag, optLabels) ->
        Sexp.list [
          Sexp.atom "Ptyp_variant";
          Sexp.list (mapEmpty ~f:rowField fields);
          closedFlag flag;
          match optLabels with
          | None -> Sexp.atom "None"
          | Some lbls -> Sexp.list (mapEmpty ~f:string lbls);
        ]
      | Ptyp_poly (lbls, typexpr) ->
        Sexp.list [
          Sexp.atom "Ptyp_poly";
          Sexp.list (mapEmpty ~f:(fun lbl -> string lbl.Asttypes.txt) lbls);
          coreType typexpr;
        ]
      | Ptyp_package (package) ->
        Sexp.list [
          Sexp.atom "Ptyp_package";
          packageType package;
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
        (Sexp.atom "PStr")::(mapEmpty ~f:structureItem s)
      )
    | PSig s ->
      Sexp.list [
        Sexp.atom "PSig";
        signature s;
      ]
    | PTyp ct ->
      Sexp.list [
        Sexp.atom "PTyp";
        coreType ct
      ]
    | PPat (pat, optExpr) ->
      Sexp.list [
        Sexp.atom "PPat";
        pattern pat;
        match optExpr with
        | Some expr -> Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]
        | None -> Sexp.atom "None";
      ]

  and attribute (stringLoc, p) =
    Sexp.list [
      Sexp.atom "attribute";
      Sexp.atom stringLoc.Asttypes.txt;
      payload p;
    ]

  and extension (stringLoc, p) =
    Sexp.list [
      Sexp.atom "extension";
      Sexp.atom stringLoc.Asttypes.txt;
      payload p;
    ]

  and attributes attrs =
    let sexprs = mapEmpty ~f:attribute attrs in
    Sexp.list ((Sexp.atom "attributes")::sexprs)

  let implementation = structure
  let interface = signature
end

module IO: sig
  val readFile: string -> string
  val readStdin: unit -> string
end = struct
  (* random chunk size: 2^15, TODO: why do we guess randomly? *)
  let chunkSize = 32768

  let readFile filename =
    let chan = open_in filename in
    let buffer = Buffer.create chunkSize in
    let chunk = (Bytes.create [@doesNotRaise]) chunkSize in
    let rec loop () =
      let len = try input chan chunk 0 chunkSize with Invalid_argument _ -> 0 in
      if len == 0 then (
        close_in_noerr chan;
        Buffer.contents buffer
      ) else (
        Buffer.add_subbytes buffer chunk 0 len;
        loop ()
      )
    in
    loop ()

  let readStdin () =
    let buffer = Buffer.create chunkSize in
    let chunk = (Bytes.create [@doesNotRaise]) chunkSize in
    let rec loop () =
      let len = try input stdin chunk 0 chunkSize with Invalid_argument _ -> 0 in
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
  let lineFeed = 0x0A (* \n *)
  let carriageReturn = 0x0D  (* \r *)
  let lineSeparator = 0x2028
  let paragraphSeparator = 0x2029

  let tab = 0x09

  let bang = 0x21
  let dot = 0x2E
  let colon = 0x3A
  let comma = 0x2C
  let backtick = 0x60
  (* let question = 0x3F *)
  let semicolon = 0x3B
  let underscore = 0x5F
  let singleQuote = 0x27
  let doubleQuote = 0x22
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

  let greaterThan = 0x3E
  let hash = 0x23
  let lessThan = 0x3C

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

  let isLetter ch =
    Lower.a <= ch && ch <= Lower.z ||
    Upper.a <= ch && ch <= Upper.z

  let isUpperCase ch =
    Upper.a <= ch && ch <= Upper.z

  let isDigit ch = _0 <= ch && ch <= _9

  let isHex ch =
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
  let isLineBreak ch =
       ch == lineFeed
    || ch == carriageReturn
    || ch == lineSeparator
    || ch == paragraphSeparator

  let digitValue ch =
    if _0 <= ch && ch <= _9 then
      ch - 48
    else if Lower.a <= (lower ch) && (lower ch) <= Lower.f then
      (lower ch) - Lower.a + 10
    else
      16 (* larger than any legal value *)
end

module Comment: sig
  type t

  val toString: t -> string

  val loc: t -> Location.t
  val txt: t -> string
  val prevTokEndPos: t -> Lexing.position

  val setPrevTokEndPos: t -> Lexing.position -> unit

  val isSingleLineComment: t -> bool

  val makeSingleLineComment: loc:Location.t -> string -> t
  val makeMultiLineComment: loc:Location.t -> string -> t
  val fromOcamlComment:
    loc:Location.t -> txt:string -> prevTokEndPos:Lexing.position -> t
  val trimSpaces: string -> string
end = struct
  type style =
    | SingleLine
    | MultiLine

  let styleToString s = match s with
    | SingleLine -> "SingleLine"
    | MultiLine -> "MultiLine"

  type t = {
    txt: string;
    style: style;
    loc: Location.t;
    mutable prevTokEndPos: Lexing.position;
  }

  let loc t = t.loc
  let txt t = t.txt
  let prevTokEndPos t = t.prevTokEndPos

  let setPrevTokEndPos t pos =
    t.prevTokEndPos <- pos

  let isSingleLineComment t = match t.style with
    | SingleLine -> true
    | MultiLine -> false

  let toString t =
    Format.sprintf
      "(txt: %s\nstyle: %s\nlines: %d-%d)"
      t.txt
      (styleToString t.style)
      t.loc.loc_start.pos_lnum
      t.loc.loc_end.pos_lnum

  let makeSingleLineComment ~loc txt = {
    txt;
    loc;
    style = SingleLine;
    prevTokEndPos = Lexing.dummy_pos;
  }

  let makeMultiLineComment ~loc txt = {
    txt;
    loc;
    style = MultiLine;
    prevTokEndPos = Lexing.dummy_pos;
  }

  let fromOcamlComment ~loc ~txt ~prevTokEndPos = {
    txt;
    loc;
    style = MultiLine;
    prevTokEndPos = prevTokEndPos
  }

  let trimSpaces s =
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

  let toString = function
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
    | Comment c -> "Comment(" ^ (Comment.toString c) ^ ")"
    | List -> "list"
    | TemplatePart text -> text ^ "${"
    | TemplateTail text -> "TemplateTail(" ^ text ^ ")"
    | Backtick -> "`"
    | BarGreater -> "|>"
    | Try -> "try" | Catch -> "catch"
    | Import -> "import"
    | Export -> "export"

  let keywordTable = function
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

  let isKeyword = function
    | True | False | Open | Let | Rec | And | As
    | Exception | Assert | Lazy | If | Else | For | In | To
    | Downto | While | Switch | When | External | Typ | Private
    | Mutable | Constraint | Include | Module | Of
    | Land | Lor | List | With
    | Try | Catch | Import | Export -> true
    | _ -> false

  let lookupKeyword str =
    try keywordTable str with
    | Not_found ->
      if CharacterCodes.isUpperCase (int_of_char (str.[0] [@doesNotRaise])) then
        Uident str
      else Lident str

  let isKeywordTxt str =
    try let _ = keywordTable str in true with
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

  let toString = function
    | OpenDescription -> "an open description"
    | ModuleLongIdent -> "a module identifier"
    | Ternary -> "a ternary expression"
    | Es6ArrowExpr -> "an es6 arrow function"
    | Jsx -> "a jsx expression"
    | JsxAttribute -> "a jsx attribute"
    | ExprOperand -> "a basic expression"
    | ExprUnary -> "a unary expression"
    | ExprBinaryAfterOp op -> "an expression after the operator \"" ^ Token.toString op  ^ "\""
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

  let isSignatureItemStart = function
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

  let isAtomicPatternStart = function
    | Token.Int _ | String _ | Character _
    | Lparen | Lbracket | Lbrace
    | Underscore
    | Lident _ | Uident _ | List
    | Exception | Lazy
    | Percent -> true
    | _ -> false

  let isAtomicExprStart = function
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

  let isAtomicTypExprStart = function
    | Token.SingleQuote | Underscore
    | Lparen | Lbrace
    | Uident _ | Lident _ | List
    | Percent -> true
    | _ -> false

  let isExprStart = function
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

  let isJsxAttributeStart = function
    | Token.Lident _ | Question -> true
    | _ -> false

 let isStructureItemStart = function
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
    | t when isExprStart t -> true
    | _ -> false

  let isPatternStart = function
    | Token.Int _ | Float _ | String _ | Character _ | True | False | Minus | Plus
    | Lparen | Lbracket | Lbrace | List
    | Underscore
    | Lident _ | Uident _ | Hash | HashHash
    | Exception | Lazy | Percent | Module
    | At -> true
    | _ -> false

  let isParameterStart = function
    | Token.Typ | Tilde | Dot -> true
    | token when isPatternStart token -> true
    | _ -> false

  (* TODO: overparse Uident ? *)
  let isStringFieldDeclStart = function
    | Token.String _ | At -> true
    | _ -> false

  (* TODO: overparse Uident ? *)
  let isFieldDeclStart = function
    | Token.At | Mutable | Lident _ | List  -> true
    (* recovery, TODO: this is not ideal… *)
    | Uident _ -> true
    | t when Token.isKeyword t -> true
    | _ -> false

  let isRecordDeclStart = function
    | Token.At
    | Mutable
    | Lident _ | List -> true
    | _ -> false

  let isTypExprStart = function
    | Token.At
    | SingleQuote
    | Underscore
    | Lparen | Lbracket
    | Uident _ | Lident _ | List
    | Module
    | Percent
    | Lbrace -> true
    | _ -> false

  let isTypeParameterStart = function
    | Token.Tilde | Dot -> true
    | token when isTypExprStart token -> true
    | _ -> false

  let isTypeParamStart = function
    | Token.Plus | Minus | SingleQuote | Underscore -> true
    | _ -> false

  let isFunctorArgStart = function
    | Token.At | Uident _ | Underscore
    | Percent
    | Lbrace
    | Lparen -> true
    | _ -> false

  let isModExprStart = function
    | Token.At | Percent
    | Uident _ | Lbrace | Lparen -> true
    | _ -> false

  let isRecordRowStart = function
    | Token.DotDotDot -> true
    | Token.Uident _ | Lident _ | List -> true
    (* TODO *)
    | t when Token.isKeyword t -> true
    | _ -> false

  let isRecordRowStringKeyStart = function
    | Token.String _ -> true
    | _ -> false

  let isArgumentStart = function
    | Token.Tilde | Dot | Underscore -> true
    | t when isExprStart t -> true
    | _ -> false

  let isPatternMatchStart = function
    | Token.Bar -> true
    | t when isPatternStart t -> true
    | _ -> false

  let isPatternOcamlListStart = function
    | Token.DotDotDot -> true
    | t when isPatternStart t -> true
    | _ -> false

  let isPatternRecordItemStart = function
    | Token.DotDotDot | Uident _ | Lident _ | List | Underscore -> true
    | _ -> false

  let isAttributeStart = function
    | Token.At -> true
    | _ -> false

  let isJsFfiImportStart = function
    | Token.Lident _ | At -> true
    | _ -> false

  let isJsxChildStart = isAtomicExprStart

  let isBlockExprStart = function
    | Token.At | Hash | Percent | Minus | MinusDot | Plus | PlusDot | Bang
    | True | False | Int _ | String _ | Character _ | Lident _ | Uident _
    | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
    | Lazy | If | For | While | Switch | Open | Module | Exception | Let
    | LessThan | Backtick | Try | Underscore -> true
    | _ -> false

  let isListElement grammar token =
    match grammar with
    | ExprList -> token = Token.DotDotDot || isExprStart token
    | ListExpr -> token = DotDotDot || isExprStart token
    | PatternList -> token = DotDotDot || isPatternStart token
    | ParameterList -> isParameterStart token
    | StringFieldDeclarations -> isStringFieldDeclStart token
    | FieldDeclarations -> isFieldDeclStart token
    | RecordDecl -> isRecordDeclStart token
    | TypExprList -> isTypExprStart token || token = Token.LessThan
    | TypeParams -> isTypeParamStart token
    | FunctorArgs -> isFunctorArgStart token
    | ModExprList -> isModExprStart token
    | TypeParameters -> isTypeParameterStart token
    | RecordRows -> isRecordRowStart token
    | RecordRowsStringKey -> isRecordRowStringKeyStart token
    | ArgumentList -> isArgumentStart token
    | Signature | Specification -> isSignatureItemStart token
    | Structure | Implementation -> isStructureItemStart token
    | PatternMatching -> isPatternMatchStart token
    | PatternOcamlList -> isPatternOcamlListStart token
    | PatternRecord -> isPatternRecordItemStart token
    | Attribute -> isAttributeStart token
    | TypeConstraint -> token = Constraint
    | PackageConstraint -> token = And
    | ConstructorDeclaration -> token = Bar
    | Primitive -> begin match token with Token.String _ -> true | _ -> false end
    | JsxAttribute -> isJsxAttributeStart token
    | JsFfiImport -> isJsFfiImportStart token
    | _ -> false

  let isListTerminator grammar token =
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
    | Primitive, token when isStructureItemStart token -> true

    | _ -> false

  let isPartOfList grammar token =
    isListElement grammar token || isListTerminator grammar token
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

    let toString (* ~width *) (doc : document) =
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
  let renderCodeContext ~missing (src : string) startPos endPos =
    let open Lexing in
    let startCol = (startPos.pos_cnum - startPos.pos_bol) in
    let endCol = endPos.pos_cnum - startPos.pos_cnum + startCol in
    let startLine = max 1 (startPos.pos_lnum - 2) in (* 2 lines before *)
    let lines =  String.split_on_char '\n' src in
    let endLine =
      let len = List.length lines in
      min len (startPos.pos_lnum + 3) (* 2 lines after *)
    in
    let lines =
      lines
      |> drop startLine
      |> take (endLine - startLine)
      |> Array.of_list
    in

    let renderLine x ix =
      let x = if ix = startPos.pos_lnum then
          begin match missing with
          | Some _len -> x ^ (String.make 10 ' ' [@doesNotRaise])
          | None -> x
          end
        else
          x
      in

      let open TerminalDoc in
      let rowNr =
        let txt = string_of_int ix in
        let len = String.length txt in
        if ix = startPos.pos_lnum then
          highlight ~from:0 ~len txt
        else txt
      in
      let len =
        let len = if endCol >= 0 then
          endCol - startCol
        else
          1
        in
        if (startCol + len) > String.length x then String.length x - startCol - 1 else len
      in
      let line =
        if ix = startPos.pos_lnum then
          begin match missing with
          | Some len ->
            underline
              ~from:(
              startCol + String.length (String.length (string_of_int ix) |> string_of_int) + 5
              ) ~len x
          | None ->
              let len = if startCol + len > String.length x then
                (String.length x) - startCol
              else
                len
              in
            text (highlight ~from:startCol ~len x)
          end
        else text x
      in
      group ~break:Never
        (append
          (append (text rowNr) (text " │"))
          (indent 2 line))
    in

    let reportDoc = ref TerminalDoc.nil in

    let linesLen = Array.length lines in
    for i = 0 to (linesLen - 1) do
      let line = try (Array.get [@doesNotRaise]) lines i with Invalid_argument _ -> "" in
      reportDoc :=
        let open TerminalDoc in
        let ix = startLine + i in
        group ~break:Always (append !reportDoc (renderLine line ix))
    done;

    TerminalDoc.toString !reportDoc

  type problem =
    | Unexpected of Token.t [@live]
    | Expected of {token: Token.t; pos: Lexing.position; context: Grammar.t option} [@live]
    | Message of string [@live]
    | Uident [@live]
    | Lident [@live]
    | Unbalanced of Token.t [@live]

  type parseError = Lexing.position * problem
end

module Diagnostics: sig
  type t
  type category
  type report

  type reportStyle
  val parseReportStyle: string -> reportStyle

  val unexpected: Token.t -> (Grammar.t * Lexing.position) list -> category
  val expected:  ?grammar:Grammar.t -> Lexing.position -> Token.t -> category
  val uident: Token.t -> category
  val lident: Token.t -> category
  val unclosedString: category
  val unclosedTemplate: category
  val unclosedComment: category
  val unknownUchar: int -> category
  val message: string -> category

  val make:
    filename: string
    -> startPos: Lexing.position
    -> endPos: Lexing.position
    -> category
    -> t

  val stringOfReport: style:reportStyle -> t list -> string -> string
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
    startPos: Lexing.position;
    endPos: Lexing.position;
    category: category;
  }

  type report = t list

  (* TODO: add json here *)
  type reportStyle =
    | Pretty
    | Plain

  let parseReportStyle txt = match (String.lowercase_ascii txt) with
    | "plain" -> Plain
    | _ -> Pretty

  let defaultUnexpected token =
    "I'm not sure what to parse here when looking at \"" ^ (Token.toString token) ^ "\"."

  let explain t =
    match t.category with
    | Uident currentToken ->
      begin match currentToken with
      | Lident lident ->
        let guess = String.capitalize_ascii lident in
        "Did you mean `" ^ guess ^"` instead of `" ^ lident ^ "`?"
      | t when Token.isKeyword t ->
        let token = Token.toString t in
        "`" ^ token ^ "` is a reserved keyword."
      | _ ->
        "At this point, I'm looking for an uppercased identifier like `Belt` or `Array`"
      end
    | Lident currentToken ->
      begin match currentToken with
      | Uident uident ->
        let guess = String.uncapitalize_ascii uident in
        "Did you mean `" ^ guess ^"` instead of `" ^ uident ^ "`?"
      | t when Token.isKeyword t ->
        let token = Token.toString t in
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
      | Some grammar -> "It signals the start of " ^ (Grammar.toString grammar)
      | None -> ""
      in
      "Did you forget a `" ^ (Token.toString t) ^ "` here? " ^ hint
    | Unexpected {token = t; context = breadcrumbs} ->
      let name = (Token.toString t) in
      begin match breadcrumbs with
      | (AtomicTypExpr, _)::breadcrumbs ->
          begin match breadcrumbs, t with
          | ((StringFieldDeclarations | FieldDeclarations) , _) :: _, (String _ | At | Rbrace | Comma | Eof) ->
              "I'm missing a type here"
          | _, t when Grammar.isStructureItemStart t || t = Eof ->
              "Missing a type here"
          | _ ->
            defaultUnexpected t
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
        if Token.isKeyword t then
          "`" ^ name ^ "` is a reserved keyword. Keywords need to be escaped: \\\"" ^ (Token.toString t) ^ "\""
        else
        "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
      end

  let toPlainString t buffer =
    Buffer.add_string buffer t.filename;
    Buffer.add_char buffer '(';
    Buffer.add_string buffer (string_of_int t.startPos.pos_cnum);
    Buffer.add_char buffer ',';
    Buffer.add_string buffer (string_of_int t.endPos.pos_cnum);
    Buffer.add_char buffer ')';
    Buffer.add_char buffer ':';
    Buffer.add_string buffer (explain t)

  let toString t src =
    let open Lexing in
    let  startchar = t.startPos.pos_cnum - t.startPos.pos_bol in
    let endchar = t.endPos.pos_cnum - t.startPos.pos_cnum + startchar in
    let locationInfo =
      Printf.sprintf (* ReasonLanguageServer requires the following format *)
        "File \"%s\", line %d, characters %d-%d:"
        t.filename
        t.startPos.pos_lnum
        startchar
        endchar
    in
    let code =
      let missing = match t.category with
      | Expected {token = t} ->
        Some (String.length (Token.toString t))
      | _ -> None
      in
      Reporting.renderCodeContext ~missing src t.startPos t.endPos
    in
    let explanation = explain t in
    Printf.sprintf "%s\n\n%s\n\n%s\n\n" locationInfo code explanation

  let make ~filename ~startPos ~endPos category = {
    filename;
    startPos;
    endPos;
    category
  }

  let stringOfReport ~style diagnostics src =
    match style with
    | Pretty ->
      List.fold_left (fun report diagnostic ->
        report ^ (toString diagnostic src) ^ "\n"
      ) "\n" (List.rev diagnostics)
    | Plain ->
      let buffer = Buffer.create 100 in
      List.iter (fun diagnostic ->
        toPlainString diagnostic buffer;
        Buffer.add_char buffer '\n';
      ) diagnostics;
      Buffer.contents buffer

  let unexpected token context =
    Unexpected {token; context}

  let expected ?grammar pos token =
    Expected {context = grammar; pos; token}

  let uident currentToken = Uident currentToken
  let lident currentToken = Lident currentToken
  let unclosedString = UnclosedString
  let unclosedComment = UnclosedComment
  let unclosedTemplate = UnclosedTemplate
  let unknownUchar code = UnknownUchar code
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
  val arrowType: Parsetree.core_type ->
      Parsetree.attributes *
      (Parsetree.attributes * Asttypes.arg_label * Parsetree.core_type) list *
      Parsetree.core_type

  val functorType: Parsetree.module_type ->
    (Parsetree.attributes * string Asttypes.loc * Parsetree.module_type option) list *
    Parsetree.module_type

  (* filters @bs out of the provided attributes *)
  val processUncurriedAttribute: Parsetree.attributes -> bool * Parsetree.attributes

  (* if ... else if ... else ... is represented as nested expressions: if ... else { if ... }
   * The purpose of this function is to flatten nested ifs into one sequence.
   * Basically compute: ([if, else if, else if, else if], else) *)
  val collectIfExpressions:
    Parsetree.expression ->
      (Parsetree.expression * Parsetree.expression) list * Parsetree.expression option

  val collectListExpressions:
    Parsetree.expression -> (Parsetree.expression list * Parsetree.expression option)

  type funParamKind =
    | Parameter of {
        attrs: Parsetree.attributes;
        lbl: Asttypes.arg_label;
        defaultExpr: Parsetree.expression option;
        pat: Parsetree.pattern;
      }
    | NewTypes of {attrs: Parsetree.attributes; locs: string Asttypes.loc list}

  val funExpr:
    Parsetree.expression ->
      Parsetree.attributes *
      funParamKind list *
      Parsetree.expression

  (* example:
   *  `makeCoordinate({
   *    x: 1,
   *    y: 2,
   *  })`
   *  Notice howe `({` and `})` "hug" or stick to each other *)
  val isHuggableExpression: Parsetree.expression -> bool

  val isHuggablePattern: Parsetree.pattern -> bool

  val isHuggableRhs: Parsetree.expression -> bool

  val operatorPrecedence: string -> int

  val isUnaryExpression: Parsetree.expression -> bool
  val isBinaryOperator: string -> bool
  val isBinaryExpression: Parsetree.expression -> bool

  val flattenableOperators: string -> string -> bool

  val hasAttributes: Parsetree.attributes -> bool

  val isArrayAccess: Parsetree.expression -> bool
  val isTernaryExpr: Parsetree.expression -> bool

  val collectTernaryParts: Parsetree.expression -> ((Parsetree.expression * Parsetree.expression) list * Parsetree.expression)

  val parametersShouldHug:
    funParamKind list -> bool

  val filterTernaryAttributes: Parsetree.attributes -> Parsetree.attributes

  val isJsxExpression: Parsetree.expression -> bool
  val hasJsxAttribute: Parsetree.attributes -> bool

  val shouldIndentBinaryExpr: Parsetree.expression -> bool
  val shouldInlineRhsBinaryExpr: Parsetree.expression -> bool
  val filterPrinteableAttributes: Parsetree.attributes -> Parsetree.attributes
  val partitionPrinteableAttributes: Parsetree.attributes -> (Parsetree.attributes * Parsetree.attributes)

  val requiresSpecialCallbackPrintingLastArg: (Asttypes.arg_label * Parsetree.expression) list -> bool
  val requiresSpecialCallbackPrintingFirstArg: (Asttypes.arg_label * Parsetree.expression) list -> bool

  val modExprApply : Parsetree.module_expr -> (
    Parsetree.module_expr list * Parsetree.module_expr
  )

  val modExprFunctor : Parsetree.module_expr -> (
    (Parsetree.attributes * string Asttypes.loc * Parsetree.module_type option) list *
    Parsetree.module_expr
  )

  val splitGenTypeAttr : Parsetree.attributes -> (bool * Parsetree.attributes)

  val collectPatternsFromListConstruct:
    Parsetree.pattern list -> Parsetree.pattern ->
      (Parsetree.pattern list * Parsetree.pattern)

  val isBlockExpr : Parsetree.expression -> bool

  val isTemplateLiteral: Parsetree.expression -> bool

  val collectOrPatternChain:
    Parsetree.pattern -> Parsetree.pattern list

  val processBracesAttr : Parsetree.expression -> (Parsetree.attribute option * Parsetree.expression)

  val filterParsingAttrs : Parsetree.attributes -> Parsetree.attributes

  val isBracedExpr : Parsetree.expression -> bool

  val isPipeExpr : Parsetree.expression -> bool

  val extractValueDescriptionFromModExpr: Parsetree.module_expr -> Parsetree.value_description list

  type jsImportScope =
    | JsGlobalImport (* nothing *)
    | JsModuleImport of string (* from "path" *)
    | JsScopedImport of string list (* window.location *)

  val classifyJsImport: Parsetree.value_description -> jsImportScope

  (* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
  val rewriteUnderscoreApply: Parsetree.expression -> Parsetree.expression

  (* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
  val isUnderscoreApplySugar: Parsetree.expression -> bool
end = struct
  open Parsetree

  let arrowType ct =
    let rec process attrsBefore acc typ = match typ with
    | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = []} ->
      let arg = ([], lbl, typ1) in
      process attrsBefore (arg::acc) typ2
    | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = [({txt ="bs"}, _) ] as attrs} ->
      let arg = (attrs, lbl, typ1) in
      process attrsBefore (arg::acc) typ2
    | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = _attrs} as returnType ->
      let args = List.rev acc in
      (attrsBefore, args, returnType)
    | {ptyp_desc = Ptyp_arrow ((Labelled _ | Optional _) as lbl, typ1, typ2); ptyp_attributes = attrs} ->
      let arg = (attrs, lbl, typ1) in
      process attrsBefore (arg::acc) typ2
    | typ ->
      (attrsBefore, List.rev acc, typ)
    in
    begin match ct with
    | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = attrs} as typ ->
      process attrs [] {typ with ptyp_attributes = []}
    | typ -> process [] [] typ
    end

  let functorType modtype =
    let rec process acc modtype = match modtype with
    | {pmty_desc = Pmty_functor (lbl, argType, returnType); pmty_attributes = attrs} ->
      let arg = (attrs, lbl, argType) in
      process (arg::acc) returnType
    | modType ->
      (List.rev acc, modType)
    in
    process [] modtype

  let processUncurriedAttribute attrs =
    let rec process uncurriedSpotted acc attrs =
      match attrs with
      | [] -> (uncurriedSpotted, List.rev acc)
      | ({Location.txt = "bs"}, _)::rest -> process true acc rest
      | attr::rest -> process uncurriedSpotted (attr::acc) rest
    in
    process false [] attrs

  let collectIfExpressions expr =
    let rec collect acc expr = match expr.pexp_desc with
    | Pexp_ifthenelse (ifExpr, thenExpr, Some elseExpr) ->
      collect ((ifExpr, thenExpr)::acc) elseExpr
    | Pexp_ifthenelse (ifExpr, thenExpr, (None as elseExpr)) ->
      let ifs = List.rev ((ifExpr, thenExpr)::acc) in
      (ifs, elseExpr)
    | _ ->
      (List.rev acc, Some expr)
    in
    collect [] expr

  let collectListExpressions expr =
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
  let rewriteUnderscoreApply expr =
    match expr.pexp_desc with
    | Pexp_fun (
        Nolabel,
        None,
        {ppat_desc = Ppat_var {txt="__x"}},
        ({pexp_desc = Pexp_apply (callExpr, args)} as e)
      ) ->
        let newArgs = List.map (fun arg ->
          match arg with
          | (
              lbl,
              ({pexp_desc = Pexp_ident ({txt = Longident.Lident "__x"} as lid)} as argExpr)
            ) ->
              (lbl, {argExpr with pexp_desc = Pexp_ident ({lid with txt = Longident.Lident "_"})})
          | arg ->  arg
        ) args in
        {e with pexp_desc = Pexp_apply (callExpr, newArgs)}
    | _ -> expr

  type funParamKind =
    | Parameter of {
        attrs: Parsetree.attributes;
        lbl: Asttypes.arg_label;
        defaultExpr: Parsetree.expression option;
        pat: Parsetree.pattern;
      }
    | NewTypes of {attrs: Parsetree.attributes; locs: string Asttypes.loc list}

  let funExpr expr =
    (* Turns (type t, type u, type z) into "type t u z" *)
    let rec collectNewTypes acc returnExpr =
      match returnExpr with
      | {pexp_desc = Pexp_newtype (stringLoc, returnExpr); pexp_attributes = []} ->
        collectNewTypes (stringLoc::acc) returnExpr
      | returnExpr ->
        (List.rev acc, returnExpr)
    in
    let rec collect attrsBefore acc expr = match expr with
    | {pexp_desc = Pexp_fun (
        Nolabel,
        None,
        {ppat_desc = Ppat_var {txt="__x"}},
        {pexp_desc = Pexp_apply _}
      )} ->
      (attrsBefore, List.rev acc, rewriteUnderscoreApply expr)
    | {pexp_desc = Pexp_fun (lbl, defaultExpr, pattern, returnExpr); pexp_attributes = []} ->
      let parameter = Parameter {
        attrs = [];
        lbl = lbl;
        defaultExpr = defaultExpr;
        pat = pattern;
      } in
      collect attrsBefore (parameter::acc) returnExpr
    | {pexp_desc = Pexp_newtype (stringLoc, rest); pexp_attributes = attrs} ->
      let (stringLocs, returnExpr) = collectNewTypes [stringLoc] rest in
      let param = NewTypes {attrs; locs = stringLocs} in
      collect attrsBefore (param::acc) returnExpr
    | {pexp_desc = Pexp_fun (lbl, defaultExpr, pattern, returnExpr); pexp_attributes = [({txt = "bs"}, _)] as attrs} ->
      let parameter = Parameter {
        attrs = attrs;
        lbl = lbl;
        defaultExpr = defaultExpr;
        pat = pattern;
      } in
      collect attrsBefore (parameter::acc) returnExpr
    | {
        pexp_desc = Pexp_fun ((Labelled _ | Optional _) as lbl, defaultExpr, pattern, returnExpr);
        pexp_attributes = attrs
      } ->
      let parameter = Parameter {
        attrs = attrs;
        lbl = lbl;
        defaultExpr = defaultExpr;
        pat = pattern;
      } in
      collect attrsBefore (parameter::acc) returnExpr
    | expr ->
      (attrsBefore, List.rev acc, expr)
    in
    begin match expr with
    | {pexp_desc = Pexp_fun (Nolabel, _defaultExpr, _pattern, _returnExpr); pexp_attributes = attrs} as expr ->
      collect attrs [] {expr with pexp_attributes = []}
    | expr -> collect [] [] expr
    end

  let processBracesAttr expr =
    match expr.pexp_attributes with
    | (({txt = "ns.braces"}, _) as attr)::attrs ->
      (Some attr, {expr with pexp_attributes = attrs})
    | _ ->
      (None, expr)

  let filterParsingAttrs attrs =
    List.filter (fun attr ->
      match attr with
      | ({Location.txt = ("ns.ternary" | "ns.braces" | "bs" | "ns.namedArgLoc")}, _) -> false
      | _ -> true
    ) attrs

  let isBlockExpr expr =
    match expr.pexp_desc with
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_let _
    | Pexp_open _
    | Pexp_sequence _ -> true
    | _ -> false

  let isBracedExpr expr =
    match processBracesAttr expr with
    | (Some _, _) -> true
    | _ -> false

  let isHuggableExpression expr =
    match expr.pexp_desc with
    | Pexp_array _
    | Pexp_tuple _
    | Pexp_construct ({txt = Longident.Lident ("::" | "[]")}, _)
    | Pexp_extension ({txt = "bs.obj"}, _)
    | Pexp_record _ -> true
    | _ when isBlockExpr expr -> true
    | _ when isBracedExpr expr -> true
    | _ -> false

  let isHuggableRhs expr =
    match expr.pexp_desc with
    | Pexp_array _
    | Pexp_tuple _
    | Pexp_construct ({txt = Longident.Lident ("::" | "[]")}, _)
    | Pexp_extension ({txt = "bs.obj"}, _)
    | Pexp_record _ -> true
    | _ when isBracedExpr expr -> true
    | _ -> false

  let isHuggablePattern pattern =
    match pattern.ppat_desc with
    | Ppat_array _
    | Ppat_tuple _
    | Ppat_record _
    | Ppat_construct _ -> true
    | _ -> false

  let operatorPrecedence operator = match operator with
    | ":=" -> 1
    | "||" -> 2
    | "&&" -> 3
    | "=" | "==" | "<" | ">" | "!=" | "<>" | "!==" | "<=" | ">=" | "|>" -> 4
    | "+" | "+." | "-" | "-." | "^" -> 5
    | "*" | "*." | "/" | "/." -> 6
    | "**" -> 7
    | "#" | "##" | "|." -> 8
    | _ -> 0

  let isUnaryOperator operator = match operator with
    | "~+" | "~+." | "~-" | "~-." | "not" -> true
    | _ -> false

  let isUnaryExpression expr = match expr.pexp_desc with
    | Pexp_apply(
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, _arg]
      ) when isUnaryOperator operator -> true
    | _ -> false

  let isBinaryOperator operator = match operator with
    | ":="
    | "||"
    | "&&"
    | "=" | "==" | "<" | ">" | "!=" | "!==" | "<=" | ">=" | "|>"
    | "+" | "+." | "-" | "-." | "^"
    | "*" | "*." | "/" | "/."
    | "**"
    | "|." | "<>" -> true
    | _ -> false

  let isBinaryExpression expr = match expr.pexp_desc with
    | Pexp_apply(
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [(Nolabel, _operand1); (Nolabel, _operand2)]
      ) when isBinaryOperator operator -> true
    | _ -> false

  let isEqualityOperator operator = match operator with
    | "=" | "==" | "<>" | "!=" -> true
    | _ -> false

  let flattenableOperators parentOperator childOperator =
    let precParent = operatorPrecedence parentOperator in
    let precChild =  operatorPrecedence childOperator in
    if precParent == precChild then
      not (
        isEqualityOperator parentOperator &&
        isEqualityOperator childOperator
      )
    else
      false

  let hasAttributes attrs =
    List.exists (fun attr -> match attr with
      | ({Location.txt = "bs" | "ns.ternary" | "ns.braces"}, _) -> false
      | _ -> true
    ) attrs

  let isArrayAccess expr = match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "get")}},
        [Nolabel, _parentExpr; Nolabel, _memberExpr]
      ) -> true
    | _ -> false

  let rec hasTernaryAttribute attrs =
    match attrs with
    | [] -> false
    | ({Location.txt="ns.ternary"},_)::_ -> true
    | _::attrs -> hasTernaryAttribute attrs

  let isTernaryExpr expr = match expr with
    | {
        pexp_attributes = attrs;
        pexp_desc = Pexp_ifthenelse _
      } when hasTernaryAttribute attrs -> true
    | _ -> false

  let collectTernaryParts expr =
    let rec collect acc expr = match expr with
    | {
        pexp_attributes = attrs;
        pexp_desc = Pexp_ifthenelse (condition, consequent, Some(alternate))
      } when hasTernaryAttribute attrs -> collect ((condition, consequent)::acc) alternate
    | alternate -> (List.rev acc, alternate)
    in
    collect [] expr

  let parametersShouldHug parameters = match parameters with
    | [Parameter {
        attrs = [];
        lbl = Asttypes.Nolabel;
        defaultExpr = None;
        pat = pat
      }] when isHuggablePattern pat -> true
    | _ -> false

  let filterTernaryAttributes attrs =
    List.filter (fun attr -> match attr with
      |({Location.txt="ns.ternary"},_) -> false
      | _ -> true
    ) attrs

  let isJsxExpression expr =
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

  let hasJsxAttribute attributes = match attributes with
    | ({Location.txt = "JSX"},_)::_ -> true
    | _ -> false

  let shouldIndentBinaryExpr expr =
    let samePrecedenceSubExpression operator subExpression =
      match subExpression with
      | {pexp_desc = Pexp_apply (
          {pexp_desc = Pexp_ident {txt = Longident.Lident subOperator}},
          [Nolabel, _lhs; Nolabel, _rhs]
        )} when isBinaryOperator subOperator ->
        flattenableOperators operator subOperator
      | _ -> true
    in
    match expr with
    | {pexp_desc = Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, lhs; Nolabel, _rhs]
      )} when isBinaryOperator operator ->
      isEqualityOperator operator ||
      not (samePrecedenceSubExpression operator lhs) ||
      operator = ":="
    | _ -> false

  let shouldInlineRhsBinaryExpr rhs = match rhs.pexp_desc with
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

  let filterPrinteableAttributes attrs =
    List.filter (fun attr -> match attr with
      | ({Location.txt="bs" | "ns.ternary"}, _) -> false
      | _ -> true
    ) attrs

  let partitionPrinteableAttributes attrs =
    List.partition (fun attr -> match attr with
      | ({Location.txt="bs" | "ns.ternary"}, _) -> false
      | _ -> true
    ) attrs

  let requiresSpecialCallbackPrintingLastArg args =
    let rec loop args = match args with
    | [] -> false
    | [(_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})] -> true
    | (_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})::_ -> false
    | _::rest -> loop rest
    in
    loop args

  let requiresSpecialCallbackPrintingFirstArg args =
    let rec loop args = match args with
      | [] -> true
      | (_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})::_ -> false
      | _::rest -> loop rest
    in
    match args with
    | [(_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})] -> false
    | (_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})::rest -> loop rest
    | _ -> false

  let modExprApply modExpr =
    let rec loop acc modExpr = match modExpr with
    | {pmod_desc = Pmod_apply (next, arg)} ->
      loop (arg::acc) next
    | _ -> (acc, modExpr)
    in
    loop [] modExpr

  let modExprFunctor modExpr =
    let rec loop acc modExpr = match modExpr with
    | {pmod_desc = Pmod_functor (lbl, modType, returnModExpr); pmod_attributes = attrs} ->
      let param = (attrs, lbl, modType) in
      loop (param::acc) returnModExpr
    | returnModExpr ->
      (List.rev acc, returnModExpr)
    in
    loop [] modExpr

  let splitGenTypeAttr attrs =
    match attrs with
    | ({Location.txt = "genType"}, _)::attrs -> (true, attrs)
    | attrs -> (false, attrs)

  let rec collectPatternsFromListConstruct acc pattern =
    let open Parsetree in
    match pattern.ppat_desc with
    | Ppat_construct(
        {txt = Longident.Lident "::"},
        Some {ppat_desc=Ppat_tuple (pat::rest::[])}
      ) ->
      collectPatternsFromListConstruct (pat::acc) rest
    | _ -> List.rev acc, pattern

  let rec isTemplateLiteral expr =
    let isPexpConstantString expr = match expr.pexp_desc with
    | Pexp_constant (Pconst_string (_, Some _)) -> true
    | _ -> false
    in
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident "^"}},
        [Nolabel, arg1; Nolabel, arg2]
      ) when not (isPexpConstantString arg1 && isPexpConstantString arg2) ->
      isTemplateLiteral arg1 || isTemplateLiteral arg2
    | Pexp_constant (Pconst_string (_, Some _)) -> true
    | _ -> false

  (* Blue | Red | Green -> [Blue; Red; Green] *)
  let collectOrPatternChain pat =
    let rec loop pattern chain =
      match pattern.ppat_desc with
      | Ppat_or (left, right) -> loop left (right::chain)
      | _ -> pattern::chain
    in
    loop pat []

  let isPipeExpr expr = match expr.pexp_desc with
    | Pexp_apply(
        {pexp_desc = Pexp_ident {txt = Longident.Lident ("|." | "|>") }},
        [(Nolabel, _operand1); (Nolabel, _operand2)]
      ) -> true
    | _ -> false

  let extractValueDescriptionFromModExpr modExpr =
    let rec loop structure acc =
      match structure with
      | [] -> List.rev acc
      | structureItem::structure ->
        begin match structureItem.Parsetree.pstr_desc with
        | Pstr_primitive vd -> loop structure (vd::acc)
        | _ -> loop structure acc
        end
    in
    match modExpr.pmod_desc with
    | Pmod_structure structure -> loop structure []
    | _ -> []

  type jsImportScope =
    | JsGlobalImport (* nothing *)
    | JsModuleImport of string (* from "path" *)
    | JsScopedImport of string list (* window.location *)

  let classifyJsImport valueDescription =
    let rec loop attrs =
      let open Parsetree in
      match attrs with
      | [] -> JsGlobalImport
      | ({Location.txt = "bs.scope"}, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _))}, _)}])::_ ->
        JsScopedImport [s]
      | ({Location.txt = "genType.import"}, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _))}, _)}])::_ ->
        JsModuleImport s
      | ({Location.txt = "bs.scope"}, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_tuple exprs}, _)}])::_ ->
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
    loop valueDescription.pval_attributes

  let isUnderscoreApplySugar expr =
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
  val structureExpr: Parsetree.expression -> kind

  val unaryExprOperand: Parsetree.expression -> kind

  val binaryExprOperand: isLhs:bool -> Parsetree.expression -> kind
  val subBinaryExprOperand: string -> string -> bool
  val rhsBinaryExprOperand: string -> Parsetree.expression -> bool
  val flattenOperandRhs: string -> Parsetree.expression -> bool

  val lazyOrAssertExprRhs: Parsetree.expression -> kind

  val fieldExpr: Parsetree.expression -> kind

  val setFieldExprRhs: Parsetree.expression -> kind

  val ternaryOperand: Parsetree.expression -> kind

  val jsxPropExpr: Parsetree.expression -> kind
  val jsxChildExpr: Parsetree.expression -> kind

  val binaryExpr: Parsetree.expression -> kind
  val modTypeFunctorReturn: Parsetree.module_type -> bool
  val modTypeWithOperand: Parsetree.module_type -> bool
  val modExprFunctorConstraint: Parsetree.module_type -> bool

  val bracedExpr: Parsetree.expression -> bool
  val callExpr: Parsetree.expression -> kind

  val includeModExpr : Parsetree.module_expr -> bool
end = struct
  type kind = Parenthesized | Braced of Location.t | Nothing

  let expr expr =
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
    | _ ->
      begin match expr with
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_constraint _ } -> Parenthesized
      |  _ -> Nothing
      end

  let callExpr expr =
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
    | _ ->
      begin match expr with
      | {Parsetree.pexp_attributes = attrs} when
          begin match ParsetreeViewer.filterParsingAttrs attrs with
          | _::_ -> true
          | [] -> false
          end
          -> Parenthesized
      | _ when ParsetreeViewer.isUnaryExpression expr || ParsetreeViewer.isBinaryExpression expr -> Parenthesized
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_fun _}
        when ParsetreeViewer.isUnderscoreApplySugar expr -> Nothing
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

  let structureExpr expr =
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
    | None ->
      begin match expr with
      | _ when ParsetreeViewer.hasAttributes expr.pexp_attributes &&
        not (ParsetreeViewer.isJsxExpression expr) -> Parenthesized
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_constraint _ } -> Parenthesized
      |  _ -> Nothing
      end

  let unaryExprOperand expr =
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_attributes = attrs} when
          begin match ParsetreeViewer.filterParsingAttrs attrs with
          | _::_ -> true
          | [] -> false
          end
          -> Parenthesized
      | expr when
          ParsetreeViewer.isUnaryExpression expr ||
          ParsetreeViewer.isBinaryExpression expr
        -> Parenthesized
      | {pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_fun _}
        when ParsetreeViewer.isUnderscoreApplySugar expr -> Nothing
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

  let binaryExprOperand ~isLhs expr =
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_fun _}
        when ParsetreeViewer.isUnderscoreApplySugar expr -> Nothing
      | {pexp_desc = Pexp_constraint _ | Pexp_fun _ | Pexp_function _ | Pexp_newtype _} -> Parenthesized
      | expr when ParsetreeViewer.isBinaryExpression expr -> Parenthesized
      | expr when ParsetreeViewer.isTernaryExpr expr -> Parenthesized
      | {pexp_desc =
            Pexp_lazy _
          | Pexp_assert _
        } when isLhs -> Parenthesized
      | _ -> Nothing
      end

  let subBinaryExprOperand parentOperator childOperator =
    let precParent = ParsetreeViewer.operatorPrecedence parentOperator in
    let precChild =  ParsetreeViewer.operatorPrecedence childOperator in
    precParent > precChild ||
    (precParent == precChild &&
    not (ParsetreeViewer.flattenableOperators parentOperator childOperator)) ||
    (* a && b || c, add parens to (a && b) for readability, who knows the difference by heart… *)
    (parentOperator = "||" && childOperator = "&&")

  let rhsBinaryExprOperand parentOperator rhs =
    match rhs.Parsetree.pexp_desc with
    | Parsetree.Pexp_apply(
      {pexp_attributes = [];
        pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [_, _left; _, _right]
      ) when ParsetreeViewer.isBinaryOperator operator ->
    let precParent = ParsetreeViewer.operatorPrecedence parentOperator in
    let precChild =  ParsetreeViewer.operatorPrecedence operator in
    precParent == precChild
    | _ -> false

  let flattenOperandRhs parentOperator rhs =
    match rhs.Parsetree.pexp_desc with
    | Parsetree.Pexp_apply(
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [_, _left; _, _right]
      ) when ParsetreeViewer.isBinaryOperator operator ->
      let precParent = ParsetreeViewer.operatorPrecedence parentOperator in
      let precChild =  ParsetreeViewer.operatorPrecedence operator in
      precParent >= precChild || rhs.pexp_attributes <> []
    | Pexp_constraint (
        {pexp_desc = Pexp_pack _},
        {ptyp_desc = Ptyp_package _}
      ) -> false
    | Pexp_fun _ when ParsetreeViewer.isUnderscoreApplySugar rhs -> false
    | Pexp_fun _
    | Pexp_newtype _
    | Pexp_setfield _
    | Pexp_constraint _ -> true
    | _ when ParsetreeViewer.isTernaryExpr rhs -> true
    | _ -> false

  let lazyOrAssertExprRhs expr =
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_attributes = attrs} when
          begin match ParsetreeViewer.filterParsingAttrs attrs with
          | _::_ -> true
          | [] -> false
          end
          -> Parenthesized
      | expr when ParsetreeViewer.isBinaryExpression expr -> Parenthesized
      | {pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_fun _}
        when ParsetreeViewer.isUnderscoreApplySugar expr -> Nothing
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

  let isNegativeConstant constant =
    let isNeg txt =
      let len = String.length txt in
      len > 0 && (String.get [@doesNotRaise]) txt 0 = '-'
    in
    match constant with
    | Parsetree.Pconst_integer (i, _) | Pconst_float (i, _) when isNeg i -> true
    | _ -> false

  let fieldExpr expr =
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_attributes = attrs} when
          begin match ParsetreeViewer.filterParsingAttrs attrs with
          | _::_ -> true
          | [] -> false
          end
          -> Parenthesized
      | expr when
          ParsetreeViewer.isBinaryExpression expr ||
          ParsetreeViewer.isUnaryExpression expr
        -> Parenthesized
      | {pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_constant c } when isNegativeConstant c -> Parenthesized
      | {pexp_desc = Pexp_fun _}
        when ParsetreeViewer.isUnderscoreApplySugar expr -> Nothing
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

  let setFieldExprRhs expr =
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_constraint _ } -> Parenthesized
      | _ -> Nothing
      end

  let ternaryOperand expr =
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_desc = Pexp_constraint (
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        )} -> Nothing
      | {pexp_desc = Pexp_constraint _ } -> Parenthesized
      | {pexp_desc = Pexp_fun _ | Pexp_newtype _} ->
        let (_attrsOnArrow, _parameters, returnExpr) = ParsetreeViewer.funExpr expr in
        begin match returnExpr.pexp_desc with
        | Pexp_constraint _ -> Parenthesized
        | _ -> Nothing
        end
      | _ -> Nothing
      end

  let startsWithMinus txt =
    let len = String.length txt in
    if len == 0 then
      false
    else
      let s = (String.get [@doesNotRaise]) txt 0 in
      s = '-'

  let jsxPropExpr expr =
    match expr.Parsetree.pexp_desc with
    | Parsetree.Pexp_let _
    | Pexp_sequence _
    | Pexp_letexception _
    | Pexp_letmodule _
    | Pexp_open _ -> Nothing
    | _ ->
      let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
      begin match optBraces with
      | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
      | None ->
        begin match expr with
        | {Parsetree.pexp_desc =
            Pexp_constant (Pconst_integer (x, _) | Pconst_float (x, _));
            pexp_attributes = []}
          when startsWithMinus x -> Parenthesized
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

  let jsxChildExpr expr =
    match expr.Parsetree.pexp_desc with
    | Parsetree.Pexp_let _
    | Pexp_sequence _
    | Pexp_letexception _
    | Pexp_letmodule _
    | Pexp_open _ -> Nothing
    | _ ->
      let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
      begin match optBraces with
      | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
      | _ ->
        begin match expr with
        | {Parsetree.pexp_desc = Pexp_constant (Pconst_integer (x, _) | Pconst_float (x, _));
           pexp_attributes = []
          } when startsWithMinus x -> Parenthesized
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
        | expr when ParsetreeViewer.isJsxExpression expr -> Nothing
        | _ -> Parenthesized
        end
      end

  let binaryExpr expr =
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced(bracesLoc)
    | None ->
      begin match expr with
      | {Parsetree.pexp_attributes = _::_} as expr
        when ParsetreeViewer.isBinaryExpression expr -> Parenthesized
      | _ -> Nothing
      end

  let modTypeFunctorReturn modType = match modType with
    | {Parsetree.pmty_desc = Pmty_with _} -> true
    | _ -> false

  (* Add parens for readability:
       module type Functor = SetLike => Set with type t = A.t
     This is actually:
       module type Functor = (SetLike => Set) with type t = A.t
  *)
  let modTypeWithOperand modType = match modType with
    | {Parsetree.pmty_desc = Pmty_functor _} -> true
    | _ -> false

  let modExprFunctorConstraint modType = match modType with
    | {Parsetree.pmty_desc = Pmty_functor _ | Pmty_with _} -> true
    | _ -> false

  let bracedExpr expr = match expr.Parsetree.pexp_desc with
    | Pexp_constraint (
        {pexp_desc = Pexp_pack _},
        {ptyp_desc = Ptyp_package _}
      ) -> false
    | Pexp_constraint _ -> true
    | _ -> false

  let includeModExpr modExpr = match modExpr.Parsetree.pmod_desc with
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
    let leadingStuff = Hashtbl.fold (fun (k : Location.t) (v : Comment.t list) acc ->
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
      let doc = Doc.breakableGroup ~forceBreak:true (
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
    let trailingStuff = Hashtbl.fold (fun (k : Location.t) (v : Comment.t list) acc ->
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
      let doc = Doc.breakableGroup ~forceBreak:true (
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
    Doc.breakableGroup ~forceBreak:true (
      Doc.concat [
        Doc.text "leading comments:";
        Doc.line;
        Doc.indent (Doc.concat leadingStuff);
        Doc.line;
        Doc.line;
        Doc.text "trailing comments:";
        Doc.indent (Doc.concat trailingStuff);
        Doc.line;
        Doc.line;
      ]
    ) |> Doc.toString ~width:80 |> print_endline
    [@@live]
  let attach tbl loc comments =
    match comments with
    | [] -> ()
    | comments -> Hashtbl.replace tbl loc comments

  let partitionByLoc comments loc =
    let rec loop (leading, inside, trailing)  comments =
      let open Location in
      match comments with
      | comment::rest ->
        let cmtLoc = Comment.loc comment in
        if cmtLoc.loc_end.pos_cnum <= loc.loc_start.pos_cnum then
          loop (comment::leading, inside, trailing) rest
        else if cmtLoc.loc_start.pos_cnum >= loc.loc_end.pos_cnum then
          loop (leading, inside, comment::trailing) rest
        else
          loop (leading, comment::inside, trailing) rest
      | [] -> (List.rev leading, List.rev inside, List.rev trailing)
    in
    loop ([], [], []) comments

  let partitionLeadingTrailing comments loc =
    let rec loop (leading, trailing)  comments =
      let open Location in
      match comments with
      | comment::rest ->
        let cmtLoc = Comment.loc comment in
        if cmtLoc.loc_end.pos_cnum <= loc.loc_start.pos_cnum then
          loop (comment::leading, trailing) rest
        else
          loop (leading, comment::trailing) rest
      | [] -> (List.rev leading, List.rev trailing)
    in
    loop ([], []) comments

  let partitionByOnSameLine loc comments =
    let rec loop (onSameLine, onOtherLine) comments =
      let open Location in
      match comments with
      | [] -> (List.rev onSameLine, List.rev onOtherLine)
      | comment::rest ->
        let cmtLoc = Comment.loc comment in
        if cmtLoc.loc_start.pos_lnum == loc.loc_end.pos_lnum then
          loop (comment::onSameLine, onOtherLine) rest
        else
          loop (onSameLine, comment::onOtherLine) rest
    in
    loop ([], []) comments

  let partitionAdjacentTrailing loc1 comments =
    let open Location in
    let open Lexing in
    let rec loop ~prevEndPos afterLoc1 comments =
      match comments with
      | [] -> (List.rev afterLoc1, [])
      | (comment::rest) as comments ->
        let cmtPrevEndPos = Comment.prevTokEndPos comment in
        if prevEndPos.Lexing.pos_cnum == cmtPrevEndPos.pos_cnum then
          let commentEnd = (Comment.loc comment).loc_end in
          loop ~prevEndPos:commentEnd (comment::afterLoc1) rest
        else
          (List.rev afterLoc1, comments)
    in
    loop ~prevEndPos:loc1.loc_end [] comments

  let rec collectListPatterns acc pattern =
    let open Parsetree in
    match pattern.ppat_desc with
    | Ppat_construct(
        {txt = Longident.Lident "::"},
        Some {ppat_desc=Ppat_tuple (pat::rest::[])}
      ) ->
      collectListPatterns (pat::acc) rest
    | Ppat_construct ({txt = Longident.Lident "[]"}, None) ->
      List.rev acc
    | _ -> List.rev (pattern::acc)

  let rec collectListExprs acc expr =
    let open Parsetree in
    match expr.pexp_desc with
    | Pexp_construct(
        {txt = Longident.Lident "::"},
        Some {pexp_desc=Pexp_tuple (expr::rest::[])}
      ) ->
      collectListExprs (expr::acc) rest
    | Pexp_construct ({txt = Longident.Lident "[]"}, _) ->
      List.rev acc
    | _ -> List.rev (expr::acc)

  (* TODO: use ParsetreeViewer *)
  let arrowType ct =
    let open Parsetree in
    let rec process attrsBefore acc typ = match typ with
    | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = []} ->
      let arg = ([], lbl, typ1) in
      process attrsBefore (arg::acc) typ2
    | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = [({txt ="bs"}, _) ] as attrs} ->
      let arg = (attrs, lbl, typ1) in
      process attrsBefore (arg::acc) typ2
    | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = _attrs} as returnType ->
      let args = List.rev acc in
      (attrsBefore, args, returnType)
    | {ptyp_desc = Ptyp_arrow ((Labelled _ | Optional _) as lbl, typ1, typ2); ptyp_attributes = attrs} ->
      let arg = (attrs, lbl, typ1) in
      process attrsBefore (arg::acc) typ2
    | typ ->
      (attrsBefore, List.rev acc, typ)
    in
    begin match ct with
    | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = attrs} as typ ->
      process attrs [] {typ with ptyp_attributes = []}
    | typ -> process [] [] typ
    end

  (* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? *)
  let modExprApply modExpr =
    let rec loop acc modExpr = match modExpr with
    | {Parsetree.pmod_desc = Pmod_apply (next, arg)} ->
      loop (arg::acc) next
    | _ -> (modExpr::acc)
    in
    loop [] modExpr

  (* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? *)
  let modExprFunctor modExpr =
    let rec loop acc modExpr = match modExpr with
    | {Parsetree.pmod_desc = Pmod_functor (lbl, modType, returnModExpr); pmod_attributes = attrs} ->
      let param = (attrs, lbl, modType) in
      loop (param::acc) returnModExpr
    | returnModExpr ->
      (List.rev acc, returnModExpr)
    in
    loop [] modExpr

  let functorType modtype =
    let rec process acc modtype = match modtype with
    | {Parsetree.pmty_desc = Pmty_functor (lbl, argType, returnType); pmty_attributes = attrs} ->
      let arg = (attrs, lbl, argType) in
      process (arg::acc) returnType
    | modType ->
      (List.rev acc, modType)
    in
    process [] modtype

  let funExpr expr =
    let open Parsetree in
    (* Turns (type t, type u, type z) into "type t u z" *)
    let rec collectNewTypes acc returnExpr =
      match returnExpr with
      | {pexp_desc = Pexp_newtype (stringLoc, returnExpr); pexp_attributes = []} ->
        collectNewTypes (stringLoc::acc) returnExpr
      | returnExpr ->
        let loc = match (acc, List.rev acc) with
        | (_startLoc::_, endLoc::_) -> { endLoc.loc with loc_end = endLoc.loc.loc_end }
        | _ -> Location.none
        in
        let txt = List.fold_right (fun curr acc -> acc ^ " " ^ curr.Location.txt) acc "type" in
        (Location.mkloc txt loc, returnExpr)
    in
    (* For simplicity reason Pexp_newtype gets converted to a Nolabel parameter,
     * otherwise this function would need to return a variant:
     * | NormalParamater(...)
     * | NewType(...)
     * This complicates printing with an extra variant/boxing/allocation for a code-path
     * that is not often used. Lets just keep it simple for now *)
    let rec collect attrsBefore acc expr = match expr with
    | {pexp_desc = Pexp_fun (lbl, defaultExpr, pattern, returnExpr); pexp_attributes = []} ->
      let parameter = ([], lbl, defaultExpr, pattern) in
      collect attrsBefore (parameter::acc) returnExpr
    | {pexp_desc = Pexp_newtype (stringLoc, rest); pexp_attributes = attrs} ->
      let (var, returnExpr) = collectNewTypes [stringLoc] rest in
      let parameter = (
        attrs,
        Asttypes.Nolabel,
        None,
        Ast_helper.Pat.var ~loc:stringLoc.loc var
      ) in
      collect attrsBefore (parameter::acc) returnExpr
    | {pexp_desc = Pexp_fun (lbl, defaultExpr, pattern, returnExpr); pexp_attributes = [({txt = "bs"}, _)] as attrs} ->
      let parameter = (attrs, lbl, defaultExpr, pattern) in
      collect attrsBefore (parameter::acc) returnExpr
    | {
        pexp_desc = Pexp_fun ((Labelled _ | Optional _) as lbl, defaultExpr, pattern, returnExpr);
        pexp_attributes = attrs
      } ->
      let parameter = (attrs, lbl, defaultExpr, pattern) in
      collect attrsBefore (parameter::acc) returnExpr
    | expr ->
      (attrsBefore, List.rev acc, expr)
    in
    begin match expr with
    | {pexp_desc = Pexp_fun (Nolabel, _defaultExpr, _pattern, _returnExpr); pexp_attributes = attrs} as expr ->
      collect attrs [] {expr with pexp_attributes = []}
    | expr -> collect [] [] expr
    end

  let rec isBlockExpr expr =
    let open Parsetree in
    match expr.pexp_desc with
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_let _
    | Pexp_open _
    | Pexp_sequence _ -> true
    | Pexp_apply (callExpr, _) when isBlockExpr callExpr -> true
    | Pexp_constraint (expr, _) when isBlockExpr expr -> true
    | Pexp_field (expr, _) when isBlockExpr expr -> true
    | Pexp_setfield (expr, _, _) when isBlockExpr expr -> true
    | _ -> false

  let rec walkStructure s t comments =
    match s with
    | _ when comments = [] -> ()
    | [] -> attach t.inside Location.none comments
    | s ->
      walkList
        ~getLoc:(fun n -> n.Parsetree.pstr_loc)
        ~walkNode:walkStructureItem
        s
        t
        comments

    and walkStructureItem si t comments =
      match si.Parsetree.pstr_desc with
      | _ when comments = [] -> ()
      | Pstr_primitive valueDescription ->
        walkValueDescription valueDescription t comments
      | Pstr_open openDescription ->
        walkOpenDescription openDescription t comments
      | Pstr_value (_, valueBindings) ->
        walkValueBindings valueBindings t comments
      | Pstr_type (_, typeDeclarations) ->
        walkTypeDeclarations typeDeclarations t comments
      | Pstr_eval (expr, _) ->
        walkExpr expr t comments
      | Pstr_module moduleBinding ->
        walkModuleBinding moduleBinding t comments
      | Pstr_recmodule moduleBindings ->
        walkList
          ~getLoc:(fun mb -> mb.Parsetree.pmb_loc)
          ~walkNode:walkModuleBinding
          moduleBindings
          t
          comments
      | Pstr_modtype modTypDecl ->
        walkModuleTypeDeclaration modTypDecl t comments
      | Pstr_attribute attribute ->
        walkAttribute attribute t comments
      | Pstr_extension (extension, _) ->
        walkExtension extension t comments
      | Pstr_include includeDeclaration ->
        walkIncludeDeclaration includeDeclaration t comments
      | Pstr_exception extensionConstructor ->
        walkExtConstr extensionConstructor t comments
      | Pstr_typext typeExtension ->
        walkTypeExtension typeExtension t comments
      | Pstr_class_type _  | Pstr_class _ -> ()

    and walkValueDescription vd t comments =
      let (leading, trailing) =
        partitionLeadingTrailing comments vd.pval_name.loc in
      attach t.leading vd.pval_name.loc leading;
      let (afterName, rest) =
        partitionAdjacentTrailing vd.pval_name.loc trailing in
      attach t.trailing vd.pval_name.loc afterName;
      let (before, inside, after) =
        partitionByLoc rest vd.pval_type.ptyp_loc
      in
      attach t.leading vd.pval_type.ptyp_loc before;
      walkTypExpr vd.pval_type t inside;
      attach t.trailing vd.pval_type.ptyp_loc after

    and walkTypeExtension te t comments =
      let (leading, trailing) =
        partitionLeadingTrailing comments te.ptyext_path.loc in
      attach t.leading te.ptyext_path.loc leading;
      let (afterPath, rest) =
        partitionAdjacentTrailing te.ptyext_path.loc trailing in
      attach t.trailing te.ptyext_path.loc afterPath;

      (* type params *)
      let rest = match te.ptyext_params with
      | [] -> rest
      | typeParams ->
        visitListButContinueWithRemainingComments
          ~getLoc:(fun (typexpr, _variance) -> typexpr.Parsetree.ptyp_loc)
          ~walkNode:walkTypeParam
          ~newlineDelimited:false
          typeParams
          t
          rest
      in
      walkList
        ~getLoc:(fun n -> n.Parsetree.pext_loc)
        ~walkNode:walkExtConstr
        te.ptyext_constructors
        t
        rest

    and walkIncludeDeclaration inclDecl t comments =
      let (before, inside, after) =
        partitionByLoc comments inclDecl.pincl_mod.pmod_loc in
      attach t.leading inclDecl.pincl_mod.pmod_loc before;
      walkModExpr inclDecl.pincl_mod t inside;
      attach t.trailing inclDecl.pincl_mod.pmod_loc after

    and walkModuleTypeDeclaration mtd t comments =
      let (leading, trailing) =
        partitionLeadingTrailing comments mtd.pmtd_name.loc in
      attach t.leading mtd.pmtd_name.loc leading;
      begin match mtd.pmtd_type with
      | None ->
        attach t.trailing mtd.pmtd_name.loc trailing
      | Some modType ->
        let (afterName, rest) = partitionAdjacentTrailing mtd.pmtd_name.loc trailing in
        attach t.trailing mtd.pmtd_name.loc afterName;
        let (before, inside, after) = partitionByLoc rest modType.pmty_loc in
        attach t.leading modType.pmty_loc before;
        walkModType modType t inside;
        attach t.trailing modType.pmty_loc after
      end

    and walkModuleBinding mb t comments =
      let (leading, trailing) = partitionLeadingTrailing comments mb.pmb_name.loc in
      attach t.leading mb.pmb_name.loc leading;
      let (afterName, rest) = partitionAdjacentTrailing mb.pmb_name.loc trailing in
      attach t.trailing mb.pmb_name.loc afterName;
      let (leading, inside, trailing) = partitionByLoc rest mb.pmb_expr.pmod_loc in
      begin match mb.pmb_expr.pmod_desc with
      | Pmod_constraint _ ->
        walkModExpr mb.pmb_expr t (List.concat [leading; inside]);
      | _ ->
        attach t.leading mb.pmb_expr.pmod_loc leading;
        walkModExpr mb.pmb_expr t inside;
      end;
      attach t.trailing mb.pmb_expr.pmod_loc trailing

   and walkSignature signature t comments =
      match signature with
      | _ when comments = [] -> ()
      | [] -> attach t.inside Location.none comments
      | _s ->
        walkList
          ~getLoc:(fun n -> n.Parsetree.psig_loc)
          ~walkNode:walkSignatureItem
          signature
          t
          comments

    and walkSignatureItem si t comments =
      match si.psig_desc with
      | _ when comments = [] -> ()
      | Psig_value valueDescription ->
        walkValueDescription valueDescription t comments
      | Psig_type (_, typeDeclarations) ->
        walkTypeDeclarations typeDeclarations t comments
      | Psig_typext typeExtension ->
        walkTypeExtension typeExtension t comments
      | Psig_exception extensionConstructor ->
        walkExtConstr extensionConstructor t comments
      | Psig_module moduleDeclaration ->
        walkModuleDeclaration moduleDeclaration t comments
      | Psig_recmodule moduleDeclarations ->
        walkList
          ~getLoc:(fun n -> n.Parsetree.pmd_loc)
          ~walkNode:walkModuleDeclaration
          moduleDeclarations
          t
          comments
      | Psig_modtype moduleTypeDeclaration ->
        walkModuleTypeDeclaration moduleTypeDeclaration t comments
      | Psig_open openDescription ->
        walkOpenDescription openDescription t comments
      | Psig_include includeDescription ->
        walkIncludeDescription includeDescription t comments
      | Psig_attribute attribute ->
        walkAttribute attribute t comments
      | Psig_extension (extension, _) ->
        walkExtension extension t comments
      | Psig_class _ | Psig_class_type _ -> ()

    and walkIncludeDescription id t comments =
      let (before, inside, after) =
        partitionByLoc comments id.pincl_mod.pmty_loc in
      attach t.leading id.pincl_mod.pmty_loc before;
      walkModType id.pincl_mod t inside;
      attach t.trailing id.pincl_mod.pmty_loc after

    and walkModuleDeclaration md t comments =
      let (leading, trailing) = partitionLeadingTrailing comments md.pmd_name.loc in
      attach t.leading md.pmd_name.loc leading;
      let (afterName, rest) = partitionAdjacentTrailing md.pmd_name.loc trailing in
      attach t.trailing md.pmd_name.loc afterName;
      let (leading, inside, trailing) = partitionByLoc rest md.pmd_type.pmty_loc in
      attach t.leading md.pmd_type.pmty_loc leading;
      walkModType md.pmd_type t inside;
      attach t.trailing md.pmd_type.pmty_loc trailing

    and walkList:
      'node.
      ?prevLoc:Location.t ->
      getLoc:('node -> Location.t) ->
      walkNode:('node -> t -> Comment.t list -> unit) ->
      'node list -> t -> Comment.t list -> unit
      = fun ?prevLoc ~getLoc ~walkNode l t comments ->
      let open Location in
      match l with
      | _ when comments = [] -> ()
      | [] ->
        begin match prevLoc with
        | Some loc ->
          attach t.trailing loc comments
        | None -> ()
        end
      | node::rest ->
        let currLoc = getLoc node in
        let (leading, inside, trailing) = partitionByLoc comments currLoc in
        begin match prevLoc with
        | None -> (* first node, all leading comments attach here *)
          attach t.leading currLoc leading
        | Some prevLoc ->
          (* Same line *)
          if prevLoc.loc_end.pos_lnum == currLoc.loc_start.pos_lnum then
            let (afterPrev, beforeCurr) = partitionAdjacentTrailing prevLoc leading in
            let () = attach t.trailing prevLoc afterPrev in
            attach t.leading currLoc beforeCurr
          else
            let (onSameLineAsPrev, afterPrev) = partitionByOnSameLine prevLoc leading in
            let () = attach t.trailing prevLoc onSameLineAsPrev in
            let (leading, _inside, _trailing) = partitionByLoc afterPrev currLoc in
            attach t.leading currLoc leading
        end;
        walkNode node t inside;
        walkList ~prevLoc:currLoc ~getLoc ~walkNode rest t trailing

    (* The parsetree doesn't always contain location info about the opening or
     * closing token of a "list-of-things". This routine visits the whole list,
     * but returns any remaining comments that likely fall after the whole list. *)
    and visitListButContinueWithRemainingComments:
      'node.
      ?prevLoc:Location.t ->
      newlineDelimited:bool ->
      getLoc:('node -> Location.t) ->
      walkNode:('node -> t -> Comment.t list -> unit) ->
      'node list -> t -> Comment.t list -> Comment.t list
      = fun ?prevLoc ~newlineDelimited ~getLoc ~walkNode l t comments ->
      let open Location in
      match l with
      | _ when comments = [] -> []
      | [] ->
        begin match prevLoc with
        | Some loc ->
          let (afterPrev, rest) =
            if newlineDelimited then
              partitionByOnSameLine loc comments
            else
              partitionAdjacentTrailing loc comments
          in
          attach t.trailing loc afterPrev;
          rest
        | None -> comments
        end
      | node::rest ->
        let currLoc = getLoc node in
        let (leading, inside, trailing) = partitionByLoc comments currLoc in
        let () = match prevLoc with
        | None -> (* first node, all leading comments attach here *)
          attach t.leading currLoc leading;
          ()
        | Some prevLoc ->
          (* Same line *)
          if prevLoc.loc_end.pos_lnum == currLoc.loc_start.pos_lnum then
            let (afterPrev, beforeCurr) = partitionAdjacentTrailing prevLoc leading in
            let () = attach t.trailing prevLoc afterPrev in
            let () = attach t.leading currLoc beforeCurr in
            ()
          else
            let (onSameLineAsPrev, afterPrev) = partitionByOnSameLine prevLoc leading in
            let () = attach t.trailing prevLoc onSameLineAsPrev in
            let (leading, _inside, _trailing) = partitionByLoc afterPrev currLoc in
            let () = attach t.leading currLoc leading in
            ()
        in
        walkNode node t inside;
        visitListButContinueWithRemainingComments
          ~prevLoc:currLoc ~getLoc ~walkNode ~newlineDelimited
          rest t trailing

    and walkValueBindings vbs t comments =
      walkList
        ~getLoc:(fun n -> n.Parsetree.pvb_loc)
        ~walkNode:walkValueBinding
        vbs
        t
        comments

    and walkOpenDescription openDescription t comments =
      let loc = openDescription.popen_lid.loc in
      let (leading, trailing) = partitionLeadingTrailing comments loc in
      attach t.leading loc leading;
      attach t.trailing loc trailing;

    and walkTypeDeclarations typeDeclarations t comments =
      walkList
        ~getLoc:(fun n -> n.Parsetree.ptype_loc)
        ~walkNode:walkTypeDeclaration
        typeDeclarations
        t
        comments

    and walkTypeParam (typexpr, _variance) t comments =
      walkTypExpr typexpr t comments

    and walkTypeDeclaration td t comments =
      let (beforeName, rest) =
        partitionLeadingTrailing comments td.ptype_name.loc in
      attach t.leading td.ptype_name.loc beforeName;

      let (afterName, rest) =
        partitionAdjacentTrailing td.ptype_name.loc rest in
      attach t.trailing td.ptype_name.loc afterName;

      (* type params *)
      let rest = match td.ptype_params with
      | [] -> rest
      | typeParams ->
        visitListButContinueWithRemainingComments
          ~getLoc:(fun (typexpr, _variance) -> typexpr.Parsetree.ptyp_loc)
          ~walkNode:walkTypeParam
          ~newlineDelimited:false
          typeParams
          t
          rest
      in

      (* manifest:  = typexpr *)
      let rest = match td.ptype_manifest with
      | Some typexpr ->
        let (beforeTyp, insideTyp, afterTyp) =
          partitionByLoc rest typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc beforeTyp;
        walkTypExpr typexpr t insideTyp;
        let (afterTyp, rest) =
          partitionAdjacentTrailing typexpr.Parsetree.ptyp_loc afterTyp in
        attach t.trailing typexpr.ptyp_loc afterTyp;
        rest
      | None -> rest
      in

      let rest = match td.ptype_kind with
      | Ptype_abstract | Ptype_open -> rest
      | Ptype_record labelDeclarations ->
        let () = walkList
          ~getLoc:(fun ld -> ld.Parsetree.pld_loc)
          ~walkNode:walkLabelDeclaration
          labelDeclarations
          t
          rest
        in
        []
      | Ptype_variant constructorDeclarations ->
        walkConstructorDeclarations constructorDeclarations t rest
      in
      attach t.trailing td.ptype_loc rest

    and walkLabelDeclarations lds t comments =
      visitListButContinueWithRemainingComments
        ~getLoc:(fun ld -> ld.Parsetree.pld_loc)
        ~walkNode:walkLabelDeclaration
        ~newlineDelimited:false
        lds
        t
        comments

    and walkLabelDeclaration ld t comments =
      let (beforeName, rest) =
        partitionLeadingTrailing comments ld.pld_name.loc  in
      attach t.leading ld.pld_name.loc beforeName;
      let (afterName, rest) = partitionAdjacentTrailing ld.pld_name.loc rest in
      attach t.trailing ld.pld_name.loc afterName;
      let (beforeTyp, insideTyp, afterTyp) =
        partitionByLoc rest ld.pld_type.ptyp_loc in
      attach t.leading ld.pld_type.ptyp_loc beforeTyp;
      walkTypExpr ld.pld_type t insideTyp;
      attach t.trailing ld.pld_type.ptyp_loc afterTyp

    and walkConstructorDeclarations cds t comments =
      visitListButContinueWithRemainingComments
        ~getLoc:(fun cd -> cd.Parsetree.pcd_loc)
        ~walkNode:walkConstructorDeclaration
        ~newlineDelimited:false
        cds
        t
        comments

    and walkConstructorDeclaration cd t comments =
      let (beforeName, rest) =
        partitionLeadingTrailing comments cd.pcd_name.loc  in
      attach t.leading cd.pcd_name.loc beforeName;
      let (afterName, rest) =
        partitionAdjacentTrailing cd.pcd_name.loc rest in
      attach t.trailing cd.pcd_name.loc afterName;
      let rest = walkConstructorArguments cd.pcd_args t rest in

      let rest = match cd.pcd_res with
      | Some typexpr ->
        let (beforeTyp, insideTyp, afterTyp) =
          partitionByLoc rest typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc beforeTyp;
        walkTypExpr typexpr t insideTyp;
        let (afterTyp, rest) =
          partitionAdjacentTrailing typexpr.Parsetree.ptyp_loc afterTyp in
        attach t.trailing typexpr.ptyp_loc afterTyp;
        rest
      | None -> rest
      in
      attach t.trailing cd.pcd_loc rest

    and walkConstructorArguments args t comments =
      match args with
      | Pcstr_tuple typexprs ->
        visitListButContinueWithRemainingComments
          ~getLoc:(fun n -> n.Parsetree.ptyp_loc)
          ~walkNode:walkTypExpr
          ~newlineDelimited:false
          typexprs
          t
          comments
      | Pcstr_record labelDeclarations ->
        walkLabelDeclarations labelDeclarations t comments

    and walkValueBinding vb t comments =
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
      let patternLoc = vb.Parsetree.pvb_pat.ppat_loc in
      let exprLoc = vb.Parsetree.pvb_expr.pexp_loc in

      let (leading, inside, trailing) =
        partitionByLoc comments patternLoc in

      (* everything before start of pattern can only be leading on the pattern:
       *   let |* before *| a = 1 *)
      attach t.leading patternLoc leading;
      walkPattern vb.Parsetree.pvb_pat t inside;
      (* let pattern = expr     -> pattern and expr on the same line *)
      (* if patternLoc.loc_end.pos_lnum == exprLoc.loc_start.pos_lnum then ( *)
        let (afterPat, surroundingExpr) =
          partitionAdjacentTrailing patternLoc trailing
        in
        attach t.trailing patternLoc afterPat;
        let (beforeExpr, insideExpr, afterExpr) =
          partitionByLoc surroundingExpr exprLoc in
        if isBlockExpr vb.pvb_expr then (
          walkExpr vb.pvb_expr t (List.concat [beforeExpr; insideExpr; afterExpr])
        ) else (
          attach t.leading exprLoc beforeExpr;
          walkExpr vb.Parsetree.pvb_expr t insideExpr;
          attach t.trailing exprLoc afterExpr
        )

    and walkExpr expr t comments =
      let open Location in
      match expr.Parsetree.pexp_desc with
      | _ when comments = [] -> ()
      | Pexp_constant _ ->
        let (leading, trailing) =
          partitionLeadingTrailing comments expr.pexp_loc in
        attach t.leading expr.pexp_loc leading;
        attach t.trailing expr.pexp_loc trailing;
      | Pexp_ident longident ->
        let (leading, trailing) =
          partitionLeadingTrailing comments longident.loc in
        attach t.leading longident.loc leading;
        attach t.trailing longident.loc trailing;
      | Pexp_let (_recFlag, valueBindings, expr2) ->
        let comments = visitListButContinueWithRemainingComments
          ~getLoc:(fun n ->
            if n.Parsetree.pvb_pat.ppat_loc.loc_ghost then
              n.pvb_expr.pexp_loc
            else
             n.Parsetree.pvb_loc
          )
          ~walkNode:walkValueBinding
          ~newlineDelimited:true
          valueBindings
          t
          comments
        in
        if isBlockExpr expr2 then (
          walkExpr expr2 t comments;
        ) else (
          let (leading, inside, trailing) = partitionByLoc comments expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walkExpr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_sequence (expr1, expr2) ->
        let (leading, inside, trailing) = partitionByLoc comments expr1.pexp_loc in
        let comments = if isBlockExpr expr1 then (
          let (afterExpr, comments) = partitionByOnSameLine expr1.pexp_loc trailing in
          walkExpr expr1 t (List.concat [leading; inside; afterExpr]);
          comments
        ) else (
          attach t.leading expr1.pexp_loc leading;
          walkExpr expr1 t inside;
          let (afterExpr, comments) = partitionByOnSameLine expr1.pexp_loc trailing in
          attach t.trailing expr1.pexp_loc afterExpr;
          comments
        ) in
        if isBlockExpr expr2 then (
          walkExpr expr2 t comments
        ) else (
          let (leading, inside, trailing) = partitionByLoc comments expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walkExpr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_open (_override, longident, expr2) ->
        let (leading, comments) =
          partitionLeadingTrailing comments expr.pexp_loc in
        attach
          t.leading
          {expr.pexp_loc with loc_end = longident.loc.loc_end}
          leading;
        let (leading, trailing) =
          partitionLeadingTrailing comments longident.loc in
        attach t.leading longident.loc leading;
        let (afterLongident, rest) =
          partitionByOnSameLine longident.loc trailing in
        attach t.trailing longident.loc afterLongident;
        if isBlockExpr expr2 then (
          walkExpr expr2 t rest
        ) else (
          let (leading, inside, trailing) = partitionByLoc rest expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walkExpr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_extension (
          {txt = "bs.obj"},
          PStr [{
            pstr_desc = Pstr_eval({pexp_desc = Pexp_record (rows, _)}, [])
          }]
        ) ->
        walkList
          ~getLoc:(fun (
              (longident, expr): (Longident.t Asttypes.loc * Parsetree.expression)
            ) -> {
            longident.loc with loc_end = expr.pexp_loc.loc_end
          })
          ~walkNode:walkExprRecordRow
          rows
          t
          comments
      | Pexp_extension extension ->
        walkExtension extension t comments
      | Pexp_letexception (extensionConstructor, expr2) ->
        let (leading, comments) =
          partitionLeadingTrailing comments expr.pexp_loc in
        attach
          t.leading
          {expr.pexp_loc with loc_end = extensionConstructor.pext_loc.loc_end}
          leading;
        let (leading, inside, trailing) =
          partitionByLoc comments extensionConstructor.pext_loc in
        attach t.leading extensionConstructor.pext_loc leading;
        walkExtConstr extensionConstructor t inside;
        let (afterExtConstr, rest) =
          partitionByOnSameLine extensionConstructor.pext_loc trailing in
        attach t.trailing extensionConstructor.pext_loc afterExtConstr;
        if isBlockExpr expr2 then (
          walkExpr expr2 t rest
        ) else (
          let (leading, inside, trailing) = partitionByLoc rest expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walkExpr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_letmodule (stringLoc, modExpr, expr2) ->
        let (leading, comments) =
          partitionLeadingTrailing comments expr.pexp_loc in
        attach t.leading {expr.pexp_loc with loc_end = modExpr.pmod_loc.loc_end} leading;
        let (leading, trailing) = partitionLeadingTrailing comments stringLoc.loc in
        attach t.leading stringLoc.loc leading;
        let (afterString, rest) =
          partitionAdjacentTrailing stringLoc.loc trailing in
        attach t.trailing stringLoc.loc afterString;
        let (beforeModExpr, insideModExpr, afterModExpr) =
          partitionByLoc rest modExpr.pmod_loc in
        attach t.leading modExpr.pmod_loc beforeModExpr;
        walkModExpr modExpr t insideModExpr;
        let (afterModExpr, rest) =
          partitionByOnSameLine modExpr.pmod_loc afterModExpr in
        attach t.trailing modExpr.pmod_loc afterModExpr;
        if isBlockExpr expr2 then (
          walkExpr expr2 t rest;
        ) else (
          let (leading, inside, trailing) = partitionByLoc rest expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walkExpr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_assert expr
      | Pexp_lazy expr ->
        if isBlockExpr expr then (
          walkExpr expr t comments
        ) else (
          let (leading, inside, trailing) = partitionByLoc comments expr.pexp_loc in
          attach t.leading expr.pexp_loc leading;
          walkExpr expr t inside;
          attach t.trailing expr.pexp_loc trailing
        )
      | Pexp_coerce (expr, optTypexpr, typexpr) ->
        let (leading, inside, trailing) = partitionByLoc comments expr.pexp_loc in
        attach t.leading expr.pexp_loc leading;
        walkExpr expr t inside;
        let (afterExpr, rest) =
          partitionAdjacentTrailing expr.pexp_loc trailing in
        attach t.trailing expr.pexp_loc afterExpr;
        let rest = match optTypexpr with
        | Some typexpr ->
          let (leading, inside, trailing) = partitionByLoc comments typexpr.ptyp_loc in
          attach t.leading typexpr.ptyp_loc leading;
          walkTypExpr typexpr t inside;
          let (afterTyp, rest) =
            partitionAdjacentTrailing typexpr.ptyp_loc trailing in
          attach t.trailing typexpr.ptyp_loc afterTyp;
          rest
        | None -> rest
        in
        let (leading, inside, trailing) = partitionByLoc rest typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc leading;
        walkTypExpr typexpr t inside;
        attach t.trailing typexpr.ptyp_loc trailing
      | Pexp_constraint (expr, typexpr) ->
        let (leading, inside, trailing) = partitionByLoc comments expr.pexp_loc in
        attach t.leading expr.pexp_loc leading;
        walkExpr expr t inside;
        let (afterExpr, rest) =
          partitionAdjacentTrailing expr.pexp_loc trailing in
        attach t.trailing expr.pexp_loc afterExpr;
        let (leading, inside, trailing) = partitionByLoc rest typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc leading;
        walkTypExpr typexpr t inside;
        attach t.trailing typexpr.ptyp_loc trailing
      | Pexp_tuple []
      | Pexp_array []
      | Pexp_construct({txt = Longident.Lident "[]"}, _) ->
        attach t.inside expr.pexp_loc comments
      | Pexp_construct({txt = Longident.Lident "::"}, _) ->
        walkList
          ~getLoc:(fun n -> n.Parsetree.pexp_loc)
          ~walkNode:walkExpr
          (collectListExprs [] expr)
          t
          comments
      | Pexp_construct (longident, args) ->
        let (leading, trailing) =
          partitionLeadingTrailing comments longident.loc in
        attach t.leading longident.loc leading;
        begin match args with
        | Some expr ->
          let (afterLongident, rest) =
            partitionAdjacentTrailing longident.loc trailing in
          attach t.trailing longident.loc afterLongident;
          walkExpr expr t rest
        | None ->
          attach t.trailing longident.loc trailing
        end
      | Pexp_variant (_label, None) ->
        ()
      | Pexp_variant (_label, Some expr) ->
        walkExpr expr t comments
      | Pexp_array exprs | Pexp_tuple exprs ->
        walkList
          ~getLoc:(fun n -> n.Parsetree.pexp_loc)
          ~walkNode:walkExpr
          exprs
          t
          comments
      | Pexp_record (rows, spreadExpr) ->
        let comments = match spreadExpr with
        | None -> comments
        | Some expr ->
          let (leading, inside, trailing) = partitionByLoc comments expr.pexp_loc in
          attach t.leading expr.pexp_loc leading;
          walkExpr expr t inside;
          let (afterExpr, rest) = partitionAdjacentTrailing expr.pexp_loc trailing in
          attach t.trailing expr.pexp_loc afterExpr;
          rest
        in
        walkList
          ~getLoc:(fun (
              (longident, expr): (Longident.t Asttypes.loc * Parsetree.expression)
            ) -> {
            longident.loc with loc_end = expr.pexp_loc.loc_end
          })
          ~walkNode:walkExprRecordRow
          rows
          t
          comments
      | Pexp_field (expr, longident) ->
        let (leading, inside, trailing) = partitionByLoc comments expr.pexp_loc in
        let trailing = if isBlockExpr expr then (
          let (afterExpr, rest) =
            partitionAdjacentTrailing expr.pexp_loc trailing in
          walkExpr expr t (List.concat [leading; inside; afterExpr]);
          rest
        ) else (
          attach t.leading expr.pexp_loc leading;
          walkExpr expr t inside;
          trailing
        ) in
        let (afterExpr, rest) = partitionAdjacentTrailing expr.pexp_loc trailing in
        attach t.trailing expr.pexp_loc afterExpr;
        let (leading, trailing) = partitionLeadingTrailing rest longident.loc in
        attach t.leading longident.loc leading;
        attach t.trailing longident.loc trailing
      | Pexp_setfield (expr1, longident, expr2) ->
        let (leading, inside, trailing) = partitionByLoc comments expr1.pexp_loc in
        let rest = if isBlockExpr expr1 then (
          let (afterExpr, rest) =
            partitionAdjacentTrailing expr1.pexp_loc trailing in
          walkExpr expr1 t (List.concat [leading; inside; afterExpr]);
          rest
        ) else (
          let (afterExpr, rest) =
            partitionAdjacentTrailing expr1.pexp_loc trailing in
          attach t.leading expr1.pexp_loc leading;
          walkExpr expr1 t inside;
          attach t.trailing expr1.pexp_loc afterExpr;
          rest
        ) in
        let (beforeLongident, afterLongident) = partitionLeadingTrailing rest longident.loc in
        attach t.leading longident.loc beforeLongident;
        let (afterLongident, rest) = partitionAdjacentTrailing longident.loc afterLongident in
        attach t.trailing longident.loc afterLongident;
        if isBlockExpr expr2 then
          walkExpr expr2 t rest
        else (
          let (leading, inside, trailing) = partitionByLoc rest expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walkExpr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_ifthenelse (ifExpr, thenExpr, elseExpr) ->
        let (leading, inside, trailing) = partitionByLoc comments ifExpr.pexp_loc in
        let comments = if isBlockExpr ifExpr then (
          let (afterExpr, comments) = partitionAdjacentTrailing ifExpr.pexp_loc trailing in
          walkExpr ifExpr t (List.concat [leading; inside; afterExpr]);
          comments
        ) else (
          attach t.leading ifExpr.pexp_loc leading;
          walkExpr ifExpr t inside;
          let (afterExpr, comments) = partitionAdjacentTrailing ifExpr.pexp_loc trailing in
          attach t.trailing ifExpr.pexp_loc afterExpr;
          comments
        ) in
        let (leading, inside, trailing) = partitionByLoc comments thenExpr.pexp_loc in
        let comments = if isBlockExpr thenExpr then (
          let (afterExpr, trailing) = partitionAdjacentTrailing thenExpr.pexp_loc trailing in
          walkExpr thenExpr t (List.concat [leading; inside; afterExpr]);
          trailing
        ) else (
          attach t.leading thenExpr.pexp_loc leading;
          walkExpr thenExpr t inside;
          let (afterExpr, comments) = partitionAdjacentTrailing thenExpr.pexp_loc trailing in
          attach t.trailing thenExpr.pexp_loc afterExpr;
          comments
        ) in
        begin match elseExpr with
        | None -> ()
        | Some expr ->
          if isBlockExpr expr then
            walkExpr expr t comments
          else (
            let (leading, inside, trailing) = partitionByLoc comments expr.pexp_loc in
            attach t.leading expr.pexp_loc leading;
            walkExpr expr t inside;
            attach t.trailing expr.pexp_loc trailing
          )
        end
      | Pexp_while (expr1, expr2) ->
        let (leading, inside, trailing) = partitionByLoc comments expr1.pexp_loc in
        let rest = if isBlockExpr expr1 then
          let (afterExpr, rest) = partitionAdjacentTrailing expr1.pexp_loc trailing in
          walkExpr expr1 t (List.concat [leading; inside; afterExpr]);
          rest
        else (
          attach t.leading expr1.pexp_loc leading;
          walkExpr expr1 t inside;
          let (afterExpr, rest) = partitionAdjacentTrailing expr1.pexp_loc trailing in
          attach t.trailing expr1.pexp_loc afterExpr;
          rest
        ) in
        if isBlockExpr expr2 then (
          walkExpr expr2 t rest
        ) else (
          let (leading, inside, trailing) = partitionByLoc rest expr2.pexp_loc in
          attach t.leading expr2.pexp_loc leading;
          walkExpr expr2 t inside;
          attach t.trailing expr2.pexp_loc trailing
        )
      | Pexp_for (pat, expr1, expr2, _, expr3) ->
        let (leading, inside, trailing) = partitionByLoc comments pat.ppat_loc in
        attach t.leading pat.ppat_loc leading;
        walkPattern pat t inside;
        let (afterPat, rest) = partitionAdjacentTrailing pat.ppat_loc trailing in
        attach t.trailing pat.ppat_loc afterPat;
        let (leading, inside, trailing) = partitionByLoc rest expr1.pexp_loc in
        attach t.leading expr1.pexp_loc leading;
        walkExpr expr1 t inside;
        let (afterExpr, rest) = partitionAdjacentTrailing expr1.pexp_loc trailing in
        attach t.trailing expr1.pexp_loc afterExpr;
        let (leading, inside, trailing) = partitionByLoc rest expr2.pexp_loc in
        attach t.leading expr2.pexp_loc leading;
        walkExpr expr2 t inside;
        let (afterExpr, rest) = partitionAdjacentTrailing expr2.pexp_loc trailing in
        attach t.trailing expr2.pexp_loc afterExpr;
        if isBlockExpr expr3 then (
          walkExpr expr3 t rest
        ) else (
          let (leading, inside, trailing) = partitionByLoc rest expr3.pexp_loc in
          attach t.leading expr3.pexp_loc leading;
          walkExpr expr3 t inside;
          attach t.trailing expr3.pexp_loc trailing
        )
      | Pexp_pack modExpr ->
        let (before, inside, after) = partitionByLoc comments modExpr.pmod_loc in
        attach t.leading modExpr.pmod_loc before;
        walkModExpr modExpr t inside;
        attach t.trailing modExpr.pmod_loc after
      | Pexp_match (expr, cases) | Pexp_try (expr, cases) ->
        let (before, inside, after) = partitionByLoc comments expr.pexp_loc in
        let after = if isBlockExpr expr then (
          let (afterExpr, rest) =
            partitionAdjacentTrailing expr.pexp_loc after in
          walkExpr expr t (List.concat [before; inside; afterExpr]);
          rest
        ) else (
          attach t.leading expr.pexp_loc before;
          walkExpr expr t inside;
          after
        ) in
        let (afterExpr, rest) = partitionAdjacentTrailing expr.pexp_loc after in
        attach t.trailing expr.pexp_loc afterExpr;
        walkList
          ~getLoc:(fun n -> {n.Parsetree.pc_lhs.ppat_loc with
            loc_end = n.pc_rhs.pexp_loc.loc_end})
          ~walkNode:walkCase
          cases
          t
          rest
        (* unary expression: todo use parsetreeviewer *)
      | Pexp_apply (
          {pexp_desc = Pexp_ident {txt = Longident.Lident
           ("~+" | "~+." | "~-" | "~-." | "not" | "!")
          }},
          [Nolabel, argExpr]
        ) ->
        let (before, inside, after) = partitionByLoc comments argExpr.pexp_loc in
        attach t.leading argExpr.pexp_loc before;
        walkExpr argExpr t inside;
        attach t.trailing argExpr.pexp_loc after
      (* binary expression *)
      | Pexp_apply(
          {pexp_desc = Pexp_ident {txt = Longident.Lident
           (":=" | "||" | "&&" | "=" | "==" | "<" | ">"
            | "!=" | "!==" | "<=" | ">=" | "|>" | "+" | "+."
            | "-" | "-." | "++" | "^" | "*" | "*." | "/"
            | "/." | "**" | "|." | "<>") }},
          [(Nolabel, operand1); (Nolabel, operand2)]
        ) ->
        let (before, inside, after) = partitionByLoc comments operand1.pexp_loc in
        attach t.leading operand1.pexp_loc before;
        walkExpr operand1 t inside;
        let (afterOperand1, rest) =
          partitionAdjacentTrailing operand1.pexp_loc after in
        attach t.trailing operand1.pexp_loc afterOperand1;
        let (before, inside, after) = partitionByLoc rest operand2.pexp_loc in
        attach t.leading operand2.pexp_loc before;
        walkExpr operand2 t inside; (* (List.concat [inside; after]); *)
        attach t.trailing operand2.pexp_loc after;
      | Pexp_apply (callExpr, arguments) ->
        let (before, inside, after) = partitionByLoc comments callExpr.pexp_loc in
        let after = if isBlockExpr callExpr then (
          let (afterExpr, rest) =
            partitionAdjacentTrailing callExpr.pexp_loc after in
          walkExpr callExpr t (List.concat [before; inside; afterExpr]);
          rest
        ) else (
          attach t.leading callExpr.pexp_loc before;
          walkExpr callExpr t inside;
          after
        ) in
        let (afterExpr, rest) = partitionAdjacentTrailing callExpr.pexp_loc after in
        attach t.trailing callExpr.pexp_loc afterExpr;
        walkList
          ~getLoc:(fun (_argLabel, expr) ->
            let loc = match expr.Parsetree.pexp_attributes with
            | ({Location.txt = "ns.namedArgLoc"; loc}, _)::_attrs ->
                {loc with loc_end = expr.pexp_loc.loc_end}
            | _ ->
               expr.pexp_loc
            in
            loc)
          ~walkNode:walkExprArgument
          arguments
          t
          rest
    | Pexp_fun (_, _, _, _) | Pexp_newtype _ ->
      let (_, parameters, returnExpr) = funExpr expr in
      let comments = visitListButContinueWithRemainingComments
        ~newlineDelimited:false
        ~walkNode:walkExprPararameter
        ~getLoc:(fun (_attrs, _argLbl, exprOpt, pattern) ->
          let open Parsetree in
          let startPos = match pattern.ppat_attributes with
          | ({Location.txt = "ns.namedArgLoc"; loc}, _)::_attrs ->
              loc.loc_start
          | _ ->
             pattern.ppat_loc.loc_start
          in
          match exprOpt with
          | None -> {pattern.ppat_loc with loc_start = startPos}
          | Some expr -> {
            pattern.ppat_loc with
            loc_start = startPos;
            loc_end = expr.pexp_loc.loc_end
          }
        )
        parameters
        t
        comments
      in
      begin match returnExpr.pexp_desc with
      | Pexp_constraint (expr, typ)
        when expr.pexp_loc.loc_start.pos_cnum >= typ.ptyp_loc.loc_end.pos_cnum
        ->
        let (leading, inside, trailing) = partitionByLoc comments typ.ptyp_loc in
        attach t.leading typ.ptyp_loc leading;
        walkTypExpr typ t inside;
        let (afterTyp, comments) =
          partitionAdjacentTrailing typ.ptyp_loc trailing in
        attach t.trailing typ.ptyp_loc afterTyp;
        if isBlockExpr expr then
          walkExpr expr t comments
        else (
          let (leading, inside, trailing) =
            partitionByLoc comments expr.pexp_loc  in
          attach t.leading expr.pexp_loc leading;
          walkExpr expr t inside;
          attach t.trailing expr.pexp_loc trailing
        )
      | _ ->
        if isBlockExpr returnExpr then
          walkExpr returnExpr t comments
        else (
          let (leading, inside, trailing) =
            partitionByLoc comments returnExpr.pexp_loc  in
          attach t.leading returnExpr.pexp_loc leading;
          walkExpr returnExpr t inside;
          attach t.trailing returnExpr.pexp_loc trailing
        )
      end
    | _ -> ()

  and walkExprPararameter (_attrs, _argLbl, exprOpt, pattern) t comments =
    let (leading, inside, trailing) = partitionByLoc comments pattern.ppat_loc in
    attach t.leading pattern.ppat_loc leading;
    walkPattern pattern t inside;
    begin match exprOpt with
    | Some expr ->
      let (_afterPat, rest) =
        partitionAdjacentTrailing pattern.ppat_loc trailing in
      attach t.trailing pattern.ppat_loc trailing;
      if isBlockExpr expr then
        walkExpr expr t rest
      else (
        let (leading, inside, trailing) = partitionByLoc rest expr.pexp_loc in
        attach t.leading expr.pexp_loc leading;
        walkExpr expr t inside;
        attach t.trailing expr.pexp_loc trailing
      )
    | None ->
      attach t.trailing pattern.ppat_loc trailing
    end

  and walkExprArgument (_argLabel, expr) t comments =
    match expr.Parsetree.pexp_attributes with
    | ({Location.txt = "ns.namedArgLoc"; loc}, _)::_attrs ->
      let (leading, trailing) = partitionLeadingTrailing comments loc in
      attach t.leading loc leading;
      let (afterLabel, rest) = partitionAdjacentTrailing loc trailing in
      attach t.trailing loc afterLabel;
      let (before, inside, after) = partitionByLoc rest expr.pexp_loc in
      attach t.leading expr.pexp_loc before;
      walkExpr expr t inside;
      attach t.trailing expr.pexp_loc after
    | _ ->
      let (before, inside, after) = partitionByLoc comments expr.pexp_loc in
      attach t.leading expr.pexp_loc before;
      walkExpr expr t inside;
      attach t.trailing expr.pexp_loc after

    and walkCase case t comments =
      let (before, inside, after) = partitionByLoc comments case.pc_lhs.ppat_loc in
      (* cases don't have a location on their own, leading comments should go
       * after the bar on the pattern *)
      walkPattern case.pc_lhs t (List.concat [before; inside]);
      let (afterPat, rest) = partitionAdjacentTrailing case.pc_lhs.ppat_loc after in
      attach t.trailing case.pc_lhs.ppat_loc afterPat;
      let comments = match case.pc_guard with
      | Some expr ->
        let (before, inside, after) = partitionByLoc rest expr.pexp_loc in
        let (afterExpr, rest) = partitionAdjacentTrailing expr.pexp_loc after in
        if isBlockExpr expr then (
          walkExpr expr t (List.concat [before; inside; afterExpr])
        ) else (
          attach t.leading expr.pexp_loc before;
          walkExpr expr t inside;
          attach t.trailing expr.pexp_loc afterExpr;
        );
        rest
      | None -> rest
      in
      if isBlockExpr case.pc_rhs then (
        walkExpr case.pc_rhs t comments
      ) else (
        let (before, inside, after) = partitionByLoc comments case.pc_rhs.pexp_loc in
        attach t.leading case.pc_rhs.pexp_loc before;
        walkExpr case.pc_rhs t inside;
        attach t.trailing case.pc_rhs.pexp_loc after
      )

    and walkExprRecordRow (longident, expr) t comments =
      let (beforeLongident, afterLongident) =
        partitionLeadingTrailing comments longident.loc
      in
      attach t.leading longident.loc beforeLongident;
      let (afterLongident, rest) =
        partitionAdjacentTrailing longident.loc afterLongident in
      attach t.trailing longident.loc afterLongident;
      let (leading, inside, trailing) = partitionByLoc rest expr.pexp_loc in
      attach t.leading expr.pexp_loc leading;
      walkExpr expr t inside;
      attach t.trailing expr.pexp_loc trailing

    and walkExtConstr extConstr t comments =
      let (leading, trailing) =
        partitionLeadingTrailing comments extConstr.pext_name.loc in
      attach t.leading extConstr.pext_name.loc leading;
      let (afterName, rest) =
        partitionAdjacentTrailing extConstr.pext_name.loc trailing in
      attach t.trailing extConstr.pext_name.loc afterName;
      walkExtensionConstructorKind extConstr.pext_kind t rest

    and walkExtensionConstructorKind kind t comments =
      match kind with
      | Pext_rebind longident ->
        let (leading, trailing) =
          partitionLeadingTrailing comments longident.loc in
        attach t.leading longident.loc leading;
        attach t.trailing longident.loc trailing
      | Pext_decl (constructorArguments, maybeTypExpr) ->
        let rest = walkConstructorArguments constructorArguments t comments in
        begin match maybeTypExpr with
        | None -> ()
        | Some typexpr ->
          let (before, inside, after) = partitionByLoc rest typexpr.ptyp_loc in
          attach t.leading typexpr.ptyp_loc before;
          walkTypExpr typexpr t inside;
          attach t.trailing typexpr.ptyp_loc after
        end

    and walkModExpr modExpr t comments =
      match modExpr.pmod_desc with
      | Pmod_ident longident ->
        let (before, after) = partitionLeadingTrailing comments longident.loc in
        attach t.leading longident.loc before;
        attach t.trailing longident.loc after
      | Pmod_structure structure ->
        walkStructure structure t comments
      | Pmod_extension extension ->
        walkExtension extension t comments
      | Pmod_unpack expr ->
        let (before, inside, after) = partitionByLoc comments expr.pexp_loc in
        attach t.leading expr.pexp_loc before;
        walkExpr expr t inside;
        attach t.trailing expr.pexp_loc after
      | Pmod_constraint (modexpr, modtype) ->
        if modtype.pmty_loc.loc_start >= modexpr.pmod_loc.loc_end then (
          let (before, inside, after) = partitionByLoc comments modexpr.pmod_loc in
          attach t.leading modexpr.pmod_loc before;
          walkModExpr modexpr t inside;
          let (after, rest) = partitionAdjacentTrailing modexpr.pmod_loc after in
          attach t.trailing modexpr.pmod_loc after;
          let (before, inside, after) = partitionByLoc rest modtype.pmty_loc in
          attach t.leading modtype.pmty_loc before;
          walkModType modtype t inside;
          attach t.trailing modtype.pmty_loc after
        ) else (
          let (before, inside, after) = partitionByLoc comments modtype.pmty_loc in
          attach t.leading modtype.pmty_loc before;
          walkModType modtype t inside;
          let (after, rest) = partitionAdjacentTrailing modtype.pmty_loc after in
          attach t.trailing modtype.pmty_loc after;
          let (before, inside, after) = partitionByLoc rest modexpr.pmod_loc in
          attach t.leading modexpr.pmod_loc before;
          walkModExpr modexpr t inside;
          attach t.trailing modexpr.pmod_loc after;
        )
      | Pmod_apply (_callModExpr, _argModExpr) ->
        let modExprs = modExprApply modExpr in
        walkList
          ~getLoc:(fun n -> n.Parsetree.pmod_loc)
          ~walkNode:walkModExpr
          modExprs
          t
          comments
      | Pmod_functor _ ->
        let (parameters, returnModExpr) = modExprFunctor modExpr in
        let comments = visitListButContinueWithRemainingComments
          ~getLoc:(fun
            (_, lbl, modTypeOption) -> match modTypeOption with
            | None -> lbl.Asttypes.loc
            | Some modType -> {lbl.loc with loc_end = modType.Parsetree.pmty_loc.loc_end}
          )
          ~walkNode:walkModExprParameter
          ~newlineDelimited:false
          parameters
          t
          comments
        in
        begin match returnModExpr.pmod_desc with
        | Pmod_constraint (modExpr, modType)
          when modType.pmty_loc.loc_end.pos_cnum <= modExpr.pmod_loc.loc_start.pos_cnum ->
          let (before, inside, after) = partitionByLoc comments modType.pmty_loc in
          attach t.leading modType.pmty_loc before;
          walkModType modType t inside;
          let (after, rest) = partitionAdjacentTrailing modType.pmty_loc after in
          attach t.trailing modType.pmty_loc after;
          let (before, inside, after) = partitionByLoc rest modExpr.pmod_loc in
          attach t.leading modExpr.pmod_loc before;
          walkModExpr modExpr t inside;
          attach t.trailing modExpr.pmod_loc after
        | _ ->
          let (before, inside, after) = partitionByLoc comments returnModExpr.pmod_loc in
          attach t.leading returnModExpr.pmod_loc before;
          walkModExpr returnModExpr t inside;
          attach t.trailing returnModExpr.pmod_loc after
        end

    and walkModExprParameter parameter t comments =
      let (_attrs, lbl, modTypeOption) = parameter in
      let (leading, trailing) = partitionLeadingTrailing comments lbl.loc in
      attach t.leading lbl.loc leading;
      begin match modTypeOption with
      | None -> attach t.trailing lbl.loc trailing
      | Some modType ->
        let (afterLbl, rest) = partitionAdjacentTrailing lbl.loc trailing in
        attach t.trailing lbl.loc afterLbl;
        let (before, inside, after) = partitionByLoc rest modType.pmty_loc in
        attach t.leading modType.pmty_loc before;
        walkModType modType t inside;
        attach t.trailing modType.pmty_loc after;
      end

    and walkModType modType t comments =
      match modType.pmty_desc with
      | Pmty_ident longident | Pmty_alias longident ->
        let (leading, trailing) = partitionLeadingTrailing comments longident.loc in
        attach t.leading longident.loc leading;
        attach t.trailing longident.loc trailing;
      | Pmty_signature signature ->
        walkSignature signature t comments
      | Pmty_extension extension ->
        walkExtension extension t comments
      | Pmty_typeof modExpr ->
        let (before, inside, after) = partitionByLoc comments modExpr.pmod_loc in
        attach t.leading modExpr.pmod_loc before;
        walkModExpr modExpr t inside;
        attach t.trailing modExpr.pmod_loc after;
      | Pmty_with (modType, _withConstraints) ->
        let (before, inside, after) = partitionByLoc comments modType.pmty_loc in
        attach t.leading modType.pmty_loc before;
        walkModType modType t inside;
        attach t.trailing modType.pmty_loc after
        (* TODO: withConstraints*)
      | Pmty_functor _ ->
        let (parameters, returnModType) = functorType modType in
        let comments = visitListButContinueWithRemainingComments
          ~getLoc:(fun
            (_, lbl, modTypeOption) -> match modTypeOption with
            | None -> lbl.Asttypes.loc
            | Some modType ->
              if lbl.txt = "_" then modType.Parsetree.pmty_loc
              else {lbl.loc with loc_end = modType.Parsetree.pmty_loc.loc_end}
          )
          ~walkNode:walkModTypeParameter
          ~newlineDelimited:false
          parameters
          t
          comments
        in
        let (before, inside, after) = partitionByLoc comments returnModType.pmty_loc in
        attach t.leading returnModType.pmty_loc before;
        walkModType returnModType t inside;
        attach t.trailing returnModType.pmty_loc after

    and walkModTypeParameter (_, lbl, modTypeOption) t comments =
      let (leading, trailing) = partitionLeadingTrailing comments lbl.loc in
      attach t.leading lbl.loc leading;
      begin match modTypeOption with
      | None -> attach t.trailing lbl.loc trailing
      | Some modType ->
        let (afterLbl, rest) = partitionAdjacentTrailing lbl.loc trailing in
        attach t.trailing lbl.loc afterLbl;
        let (before, inside, after) = partitionByLoc rest modType.pmty_loc in
        attach t.leading modType.pmty_loc before;
        walkModType modType t inside;
        attach t.trailing modType.pmty_loc after;
      end

    and walkPattern pat t comments =
      let open Location in
      match pat.Parsetree.ppat_desc with
      | _ when comments = [] -> ()
      | Ppat_alias (pat, alias) ->
        let (leading, inside, trailing) = partitionByLoc comments pat.ppat_loc in
        attach t.leading pat.ppat_loc leading;
        walkPattern pat t inside;
        let (afterPat, rest) = partitionAdjacentTrailing pat.ppat_loc trailing in
        attach t.leading pat.ppat_loc leading;
        attach t.trailing pat.ppat_loc afterPat;
        let (beforeAlias, afterAlias) = partitionLeadingTrailing rest alias.loc in
        attach t.leading alias.loc beforeAlias;
        attach t.trailing alias.loc afterAlias
      | Ppat_tuple []
      | Ppat_array []
      | Ppat_construct({txt = Longident.Lident "()"}, _)
      | Ppat_construct({txt = Longident.Lident "[]"}, _) ->
        attach t.inside pat.ppat_loc comments;
      | Ppat_array patterns ->
        walkList
          ~getLoc:(fun n -> n.Parsetree.ppat_loc)
          ~walkNode:walkPattern
          patterns
          t
          comments
      | Ppat_tuple patterns ->
        walkList
          ~getLoc:(fun n -> n.Parsetree.ppat_loc)
          ~walkNode:walkPattern
          patterns
          t
          comments
      | Ppat_construct({txt = Longident.Lident "::"}, _) ->
        walkList
          ~getLoc:(fun n -> n.Parsetree.ppat_loc)
          ~walkNode:walkPattern
          (collectListPatterns [] pat)
          t
          comments
      | Ppat_construct (constr, None) ->
        let (beforeConstr, afterConstr) =
          partitionLeadingTrailing comments constr.loc
        in
        attach t.leading constr.loc beforeConstr;
        attach t.trailing constr.loc afterConstr
      | Ppat_construct (constr, Some pat) ->
        let (leading, trailing) = partitionLeadingTrailing comments constr.loc in
        attach t.leading constr.loc leading;
        let (leading, inside, trailing) = partitionByLoc trailing pat.ppat_loc in
        attach t.leading pat.ppat_loc leading;
        walkPattern pat t inside;
        attach t.trailing pat.ppat_loc trailing
      | Ppat_variant (_label, None) ->
        ()
      | Ppat_variant (_label, Some pat) ->
        walkPattern pat t comments
      | Ppat_type _ ->
        ()
      | Ppat_record (recordRows, _) ->
        walkList
          ~getLoc:(fun (
            (longidentLoc, pattern): (Longident.t Asttypes.loc * Parsetree.pattern)
          ) -> {
            longidentLoc.loc with
            loc_end = pattern.Parsetree.ppat_loc.loc_end
          })
          ~walkNode:walkPatternRecordRow
          recordRows
          t
          comments
      | Ppat_or (pattern1, pattern2) ->
        let (beforePattern1, insidePattern1, afterPattern1) =
          partitionByLoc comments pattern1.ppat_loc
        in
        attach t.leading pattern1.ppat_loc beforePattern1;
        walkPattern pattern1 t insidePattern1;
        let (afterPattern1, rest) =
          partitionAdjacentTrailing pattern1.ppat_loc afterPattern1
        in
        attach t.trailing pattern1.ppat_loc afterPattern1;
        let (beforePattern2, insidePattern2, afterPattern2) =
          partitionByLoc rest pattern2.ppat_loc
        in
        attach t.leading pattern2.ppat_loc beforePattern2;
        walkPattern pattern2 t insidePattern2;
        attach t.trailing pattern2.ppat_loc afterPattern2
      | Ppat_constraint (pattern, typ) ->
        let (beforePattern, insidePattern, afterPattern) =
          partitionByLoc comments pattern.ppat_loc
        in
        attach t.leading pattern.ppat_loc beforePattern;
        walkPattern pattern t insidePattern;
        let (afterPattern, rest) =
          partitionAdjacentTrailing pattern.ppat_loc afterPattern
        in
        attach t.trailing pattern.ppat_loc afterPattern;
        let (beforeTyp, insideTyp, afterTyp) =
          partitionByLoc rest typ.ptyp_loc
        in
        attach t.leading typ.ptyp_loc beforeTyp;
        walkTypExpr typ t insideTyp;
        attach t.trailing typ.ptyp_loc afterTyp
      | Ppat_lazy pattern | Ppat_exception pattern ->
        let (leading, inside, trailing) = partitionByLoc comments pattern.ppat_loc in
        attach t.leading pattern.ppat_loc leading;
        walkPattern pattern t inside;
        attach t.trailing pattern.ppat_loc trailing
      | Ppat_unpack stringLoc ->
        let (leading, trailing) = partitionLeadingTrailing comments stringLoc.loc in
        attach t.leading stringLoc.loc leading;
        attach t.trailing stringLoc.loc trailing
      | Ppat_extension extension ->
        walkExtension extension t comments
      | _ -> ()

    (* name: firstName *)
    and walkPatternRecordRow row t comments =
     match row with
      (* punned {x}*)
      | ({Location.txt=Longident.Lident ident; loc = longidentLoc},
         {Parsetree.ppat_desc=Ppat_var {txt;_}}) when ident = txt ->
        let (beforeLbl, afterLbl) =
          partitionLeadingTrailing comments longidentLoc
        in
        attach t.leading longidentLoc beforeLbl;
        attach t.trailing longidentLoc afterLbl
      | (longident, pattern) ->
        let (beforeLbl, afterLbl) =
          partitionLeadingTrailing comments longident.loc
        in
        attach t.leading longident.loc beforeLbl;
        let (afterLbl, rest) = partitionAdjacentTrailing longident.loc afterLbl in
        attach t.trailing longident.loc afterLbl;
        let (leading, inside, trailing) = partitionByLoc rest pattern.ppat_loc in
        attach t.leading pattern.ppat_loc leading;
        walkPattern pattern t inside;
        attach t.trailing pattern.ppat_loc trailing

    and walkTypExpr typ t comments =
      match typ.Parsetree.ptyp_desc with
      | _ when comments = [] -> ()
      | Ptyp_tuple typexprs ->
        walkList
          ~getLoc:(fun n -> n.Parsetree.ptyp_loc)
          ~walkNode:walkTypExpr
          typexprs
          t
          comments
      | Ptyp_extension extension ->
        walkExtension extension t comments
      | Ptyp_package packageType ->
        walkPackageType packageType t comments
      | Ptyp_alias (typexpr, _alias) ->
        let (beforeTyp, insideTyp, afterTyp) =
          partitionByLoc comments typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc beforeTyp;
        walkTypExpr typexpr t insideTyp;
        attach t.trailing typexpr.ptyp_loc afterTyp;
      | Ptyp_poly (strings, typexpr) ->
        let comments = visitListButContinueWithRemainingComments
          ~getLoc:(fun n -> n.Asttypes.loc)
          ~walkNode:(fun longident t comments ->
            let (beforeLongident, afterLongident) =
            partitionLeadingTrailing comments longident.loc in
            attach t.leading longident.loc beforeLongident;
            attach t.trailing longident.loc afterLongident
          )
          ~newlineDelimited:false
          strings
          t
          comments
        in
        let (beforeTyp, insideTyp, afterTyp) =
          partitionByLoc comments typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc beforeTyp;
        walkTypExpr typexpr t insideTyp;
        attach t.trailing typexpr.ptyp_loc afterTyp
      | Ptyp_constr (longident, typexprs) ->
        let (beforeLongident, _afterLongident) =
          partitionLeadingTrailing comments longident.loc in
        let (afterLongident, rest) =
          partitionAdjacentTrailing longident.loc comments in
        attach t.leading longident.loc beforeLongident;
        attach t.trailing longident.loc afterLongident;
        walkList
          ~getLoc:(fun n -> n.Parsetree.ptyp_loc)
          ~walkNode:walkTypExpr
          typexprs
          t
          rest
      | Ptyp_arrow _ ->
        let (_, parameters, typexpr) = arrowType typ in
        let comments = walkTypeParameters parameters t comments in
        let (beforeTyp, insideTyp, afterTyp) =
          partitionByLoc comments typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc beforeTyp;
        walkTypExpr typexpr t insideTyp;
        attach t.trailing typexpr.ptyp_loc afterTyp
      | Ptyp_object (fields, _) ->
        walkTypObjectFields fields t comments
      | _ -> ()

    and walkTypObjectFields fields t comments =
      walkList
        ~getLoc:(fun field ->
          match field with
          | Parsetree.Otag (lbl, _, typ) ->
            {lbl.loc with loc_end = typ.ptyp_loc.loc_end}
          | _ -> Location.none
        )
        ~walkNode:walkTypObjectField
        fields
        t
        comments

    and walkTypObjectField field t comments =
      match field with
      | Otag (lbl, _, typexpr) ->
        let (beforeLbl, afterLbl) = partitionLeadingTrailing comments lbl.loc in
        attach t.leading lbl.loc beforeLbl;
        let (afterLbl, rest) = partitionAdjacentTrailing lbl.loc afterLbl in
        attach t.trailing lbl.loc afterLbl;
        let (beforeTyp, insideTyp, afterTyp) =
          partitionByLoc rest typexpr.ptyp_loc in
        attach t.leading typexpr.ptyp_loc beforeTyp;
        walkTypExpr typexpr t insideTyp;
        attach t.trailing typexpr.ptyp_loc afterTyp
      | _ -> ()

    and walkTypeParameters typeParameters t comments =
      visitListButContinueWithRemainingComments
        ~getLoc:(fun (_, _, typexpr) -> typexpr.Parsetree.ptyp_loc)
        ~walkNode:walkTypeParameter
        ~newlineDelimited:false
        typeParameters
        t
        comments

    and walkTypeParameter (_attrs, _lbl, typexpr) t comments =
      let (beforeTyp, insideTyp, afterTyp) =
        partitionByLoc comments typexpr.ptyp_loc in
      attach t.leading typexpr.ptyp_loc beforeTyp;
      walkTypExpr typexpr t insideTyp;
      attach t.trailing typexpr.ptyp_loc afterTyp

    and walkPackageType packageType t comments =
      let (longident, packageConstraints) = packageType in
      let (beforeLongident, afterLongident) =
        partitionLeadingTrailing comments longident.loc in
      attach t.leading longident.loc beforeLongident;
      let (afterLongident, rest) =
        partitionAdjacentTrailing longident.loc afterLongident in
      attach t.trailing longident.loc afterLongident;
      walkPackageConstraints packageConstraints t rest

    and walkPackageConstraints packageConstraints t comments =
      walkList
        ~getLoc:(fun (longident, typexpr) -> {longident.Asttypes.loc with
          loc_end = typexpr.Parsetree.ptyp_loc.loc_end
        })
        ~walkNode:walkPackageConstraint
        packageConstraints
        t
        comments

    and walkPackageConstraint packageConstraint t comments =
      let (longident, typexpr) = packageConstraint in
      let (beforeLongident, afterLongident) =
        partitionLeadingTrailing comments longident.loc in
      attach t.leading longident.loc beforeLongident;
      let (afterLongident, rest) =
        partitionAdjacentTrailing longident.loc afterLongident in
      attach t.trailing longident.loc afterLongident;
      let (beforeTyp, insideTyp, afterTyp) =
        partitionByLoc rest typexpr.ptyp_loc in
      attach t.leading typexpr.ptyp_loc beforeTyp;
      walkTypExpr typexpr t insideTyp;
      attach t.trailing typexpr.ptyp_loc afterTyp;

    and walkExtension extension t comments =
      let (id, payload) = extension in
      let (beforeId, afterId) = partitionLeadingTrailing comments id.loc in
      attach t.leading id.loc beforeId;
      let (afterId, rest) = partitionAdjacentTrailing id.loc afterId in
      attach t.trailing id.loc afterId;
      walkPayload payload t rest

    and walkAttribute (id, payload) t comments =
      let (beforeId, afterId) = partitionLeadingTrailing comments id.loc in
      attach t.leading id.loc beforeId;
      let (afterId, rest) = partitionAdjacentTrailing id.loc afterId in
      attach t.trailing id.loc afterId;
      walkPayload payload t rest

    and walkPayload payload t comments =
      match payload with
      | PStr s -> walkStructure s t comments
      | _ -> ()

end

module Printer = struct
  let addParens doc =
    Doc.group (
      Doc.concat [
        Doc.lparen;
        Doc.indent (
          Doc.concat [
            Doc.softLine;
            doc
          ]
        );
        Doc.softLine;
        Doc.rparen;
      ]
    )

  let addBraces doc =
    Doc.group (
      Doc.concat [
        Doc.lbrace;
        doc;
        Doc.rbrace;
      ]
    )

  let getFirstLeadingComment tbl loc =
    match Hashtbl.find tbl.CommentTable.leading loc with
    | comment::_ -> Some comment
    | [] -> None
    | exception Not_found -> None

  let printMultilineCommentContent txt =
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
    let rec indentStars lines acc =
      match lines with
      | [] -> Doc.nil
      | [lastLine] ->
        let line = String.trim lastLine in
        let doc = Doc.text (" " ^ line) in
        let trailingSpace = if String.length line > 0 then Doc.space else Doc.nil in
        List.rev (trailingSpace::doc::acc) |> Doc.concat
      | line::lines ->
        let line = String.trim line in
        let len = String.length line in
        if len > 0 && (String.get [@doesNotRaise]) line 0 == '*' then
          let doc = Doc.text (" " ^ (String.trim line)) in
          indentStars lines (Doc.hardLine::doc::acc)
        else
          let trailingSpace =
            let len = String.length txt in
            if len > 0 && (String.unsafe_get txt (len - 1) = ' ') then
              Doc.space
            else Doc.nil
          in
          let content = Comment.trimSpaces txt in
          Doc.concat [Doc.text content; trailingSpace]
    in
    let lines = String.split_on_char '\n' txt in
    match lines with
    | [] -> Doc.text "/* */"
    | [line] -> Doc.concat [
        Doc.text "/* ";
        Doc.text (Comment.trimSpaces line);
        Doc.text " */";
      ]
    | first::rest ->
      let firstLine = Comment.trimSpaces first in
      Doc.concat [
        Doc.text "/*";
        if String.length firstLine > 0 && not (String.equal firstLine "*") then
          Doc.space else Doc.nil;
        indentStars rest [Doc.hardLine; Doc.text firstLine];
        Doc.text "*/";
      ]

  let printTrailingComment (nodeLoc : Location.t) comment =
    let singleLine = Comment.isSingleLineComment comment in
    let content =
      let txt = Comment.txt comment in
      if singleLine then
         Doc.text ("// " ^ String.trim txt)
      else
        printMultilineCommentContent txt
    in
    let diff =
      let cmtStart = (Comment.loc comment).loc_start in
      let prevTokEndPos = Comment.prevTokEndPos comment in
      cmtStart.pos_lnum - prevTokEndPos.pos_lnum
    in
    let isBelow =
      (Comment.loc comment).loc_start.pos_lnum > nodeLoc.loc_end.pos_lnum in
    if diff > 0 || isBelow then
      Doc.concat [
        Doc.breakParent;
        Doc.lineSuffix(
          (Doc.concat [Doc.hardLine; if diff > 1 then Doc.hardLine else Doc.nil; content])
        )
      ]
    else if not singleLine then
      Doc.concat [Doc.space; content]
    else
      Doc.lineSuffix (Doc.concat [Doc.space; content])

  let printLeadingComment ?nextComment comment =
    let singleLine = Comment.isSingleLineComment comment in
    let content =
      let txt = Comment.txt comment in
      if singleLine then
         Doc.text ("// " ^ String.trim txt)
      else
        printMultilineCommentContent txt
    in
    let separator = Doc.concat  [
      if singleLine then Doc.concat [
        Doc.hardLine;
        Doc.breakParent;
      ] else Doc.nil;
      (match nextComment with
      | Some next ->
        let nextLoc = Comment.loc next in
        let currLoc = Comment.loc comment in
        let diff =
          nextLoc.Location.loc_start.pos_lnum -
          currLoc.Location.loc_end.pos_lnum
        in
        let nextSingleLine = Comment.isSingleLineComment next in
        if singleLine && nextSingleLine then
          if diff > 1 then Doc.hardLine else Doc.nil
        else if singleLine && not nextSingleLine then
          if diff > 1 then Doc.hardLine else Doc.nil
        else
          if diff > 1 then Doc.concat [Doc.hardLine; Doc.hardLine]
          else if diff == 1 then Doc.hardLine
          else
            Doc.space
      | None -> Doc.nil)
    ]
    in
    Doc.concat [
      content;
      separator;
    ]

  let printCommentsInside cmtTbl loc =
    let rec loop acc comments =
      match comments with
      | [] -> Doc.nil
      | [comment] ->
        let cmtDoc = printLeadingComment comment in
        let doc = Doc.group (
          Doc.concat [
            Doc.concat (List.rev (cmtDoc::acc));
          ]
        )
        in
        doc
      | comment::((nextComment::_comments) as rest) ->
        let cmtDoc = printLeadingComment ~nextComment comment in
        loop (cmtDoc::acc) rest
    in
    match Hashtbl.find cmtTbl.CommentTable.inside loc with
    | exception Not_found -> Doc.nil
    | comments ->
      Hashtbl.remove cmtTbl.inside loc;
      Doc.group (
        loop [] comments
      )

  let printLeadingComments node tbl loc =
    let rec loop acc comments =
      match comments with
      | [] -> node
      | [comment] ->
        let cmtDoc = printLeadingComment comment in
        let diff =
          loc.Location.loc_start.pos_lnum -
          (Comment.loc comment).Location.loc_end.pos_lnum
        in
        let separator =
          if Comment.isSingleLineComment comment then
            if diff > 1 then Doc.hardLine else Doc.nil
          else if diff == 0 then
           Doc.space
          else if diff > 1 then Doc.concat [Doc.hardLine; Doc.hardLine]
          else
           Doc.hardLine
        in
        let doc = Doc.group (
          Doc.concat [
            Doc.concat (List.rev (cmtDoc::acc));
            separator;
            node
          ]
        )
        in
        doc
      | comment::((nextComment::_comments) as rest) ->
        let cmtDoc = printLeadingComment ~nextComment comment in
        loop (cmtDoc::acc) rest
    in
    match Hashtbl.find tbl loc with
    | exception Not_found -> node
    | comments ->
     (* Remove comments from tbl: Some ast nodes have the same location.
      * We only want to print comments once *)
      Hashtbl.remove tbl loc;
      loop [] comments

  let printTrailingComments node tbl loc =
    let rec loop acc comments =
      match comments with
      | [] -> Doc.concat (List.rev acc)
      | comment::comments ->
        let cmtDoc = printTrailingComment loc comment in
        loop (cmtDoc::acc) comments
    in
    match Hashtbl.find tbl loc with
    | exception Not_found -> node
    | [] -> node
    | (_first::_) as comments ->
     (* Remove comments from tbl: Some ast nodes have the same location.
      * We only want to print comments once *)
      Hashtbl.remove tbl loc;
      let cmtsDoc = loop [] comments in
      Doc.concat [
        node;
        cmtsDoc;
      ]

  let printComments doc (tbl: CommentTable.t) loc =
    let docWithLeadingComments = printLeadingComments doc tbl.leading loc in
    printTrailingComments docWithLeadingComments tbl.trailing loc

  let printList ~getLoc ~nodes ~print ?(forceBreak=false) t =
    let rec loop (prevLoc: Location.t) acc nodes =
      match nodes with
      | [] -> (prevLoc, Doc.concat (List.rev acc))
      | node::nodes ->
        let loc = getLoc node in
        let startPos = match getFirstLeadingComment t loc with
        | None -> loc.loc_start
        | Some comment -> (Comment.loc comment).loc_start
        in
        let sep = if startPos.pos_lnum - prevLoc.loc_end.pos_lnum > 1 then
          Doc.concat [Doc.hardLine; Doc.hardLine]
        else
          Doc.hardLine
        in
        let doc = printComments (print node t) t loc in
        loop loc (doc::sep::acc) nodes
    in
    match nodes with
    | [] -> Doc.nil
    | node::nodes ->
      let firstLoc = getLoc node in
      let doc = printComments (print node t) t firstLoc in
      let (lastLoc, docs) = loop firstLoc [doc] nodes in
      let forceBreak =
        forceBreak ||
        firstLoc.loc_start.pos_lnum != lastLoc.loc_end.pos_lnum
      in
      Doc.breakableGroup ~forceBreak docs

  let printListi ~getLoc ~nodes ~print ?(forceBreak=false) t =
    let rec loop i (prevLoc: Location.t) acc nodes =
      match nodes with
      | [] -> (prevLoc, Doc.concat (List.rev acc))
      | node::nodes ->
        let loc = getLoc node in
        let startPos = match getFirstLeadingComment t loc with
        | None -> loc.loc_start
        | Some comment -> (Comment.loc comment).loc_start
        in
        let sep = if startPos.pos_lnum - prevLoc.loc_end.pos_lnum > 1 then
          Doc.concat [Doc.hardLine; Doc.hardLine]
        else
          Doc.line
        in
        let doc = printComments (print node t i) t loc in
        loop (i + 1) loc (doc::sep::acc) nodes
    in
    match nodes with
    | [] -> Doc.nil
    | node::nodes ->
      let firstLoc = getLoc node in
      let doc = printComments (print node t 0) t firstLoc in
      let (lastLoc, docs) = loop 1 firstLoc [doc] nodes in
      let forceBreak =
        forceBreak ||
        firstLoc.loc_start.pos_lnum != lastLoc.loc_end.pos_lnum
      in
      Doc.breakableGroup ~forceBreak docs

  let rec printLongidentAux accu = function
  | Longident.Lident s -> (Doc.text s) :: accu
  | Ldot(lid, s) -> printLongidentAux ((Doc.text s) :: accu) lid
  | Lapply(lid1, lid2) ->
    let d1 = Doc.join ~sep:Doc.dot (printLongidentAux [] lid1) in
    let d2 = Doc.join ~sep:Doc.dot (printLongidentAux [] lid2) in
    (Doc.concat [d1; Doc.lparen; d2; Doc.rparen]) :: accu

  let printLongident = function
  | Longident.Lident txt -> Doc.text txt
  | lid -> Doc.join ~sep:Doc.dot (printLongidentAux [] lid)

  type identifierStyle =
    | ExoticIdent
    | NormalIdent

  let classifyIdentContent ?(allowUident=false) txt =
    let len = String.length txt in
    let rec go i =
      if i == len then NormalIdent
      else
        let c = String.unsafe_get txt i in
        if i == 0 && not (
          (allowUident && (c >= 'A' && c <= 'Z')) ||
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
    if Token.isKeywordTxt txt && txt <> "list" then
      ExoticIdent
    else
      go 0

  let printIdentLike ?allowUident txt =
    match classifyIdentContent ?allowUident txt with
    | ExoticIdent -> Doc.concat [
        Doc.text "\\\"";
        Doc.text txt;
        Doc.text"\""
      ]
    | NormalIdent -> Doc.text txt

  let printLident l = match l with
    | Longident.Lident txt -> printIdentLike txt
    | Longident.Ldot (path, txt) ->
      let txts = Longident.flatten path in
      Doc.concat [
        Doc.join ~sep:Doc.dot (List.map Doc.text txts);
        Doc.dot;
        printIdentLike txt;
      ]
    | _ -> Doc.text("printLident: Longident.Lapply is not supported")

  let printLongidentLocation l cmtTbl =
    let doc = printLongident l.Location.txt in
    printComments doc cmtTbl l.loc

  (* Module.SubModule.x *)
  let printLidentPath path cmtTbl =
    let doc = printLident path.Location.txt in
    printComments doc cmtTbl path.loc

  (* Module.SubModule.x or Module.SubModule.X *)
  let printIdentPath path cmtTbl =
    let doc = printLident path.Location.txt in
    printComments doc cmtTbl path.loc

  let printStringLoc sloc cmtTbl =
    let doc = printIdentLike sloc.Location.txt in
    printComments doc cmtTbl sloc.loc

  let printConstant c = match c with
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

  let rec printStructure (s : Parsetree.structure) t =
    match s with
    | [] -> printCommentsInside t Location.none
    | structure ->
      printList
        ~getLoc:(fun s -> s.Parsetree.pstr_loc)
        ~nodes:structure
        ~print:printStructureItem
        t

  and printStructureItem (si: Parsetree.structure_item) cmtTbl =
    match si.pstr_desc with
    | Pstr_value(rec_flag, valueBindings) ->
			let recFlag = match rec_flag with
			| Asttypes.Nonrecursive -> Doc.nil
			| Asttypes.Recursive -> Doc.text "rec "
			in
      printValueBindings ~recFlag valueBindings cmtTbl
    | Pstr_type(recFlag, typeDeclarations) ->
      let recFlag = match recFlag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
      in
      printTypeDeclarations ~recFlag typeDeclarations cmtTbl
    | Pstr_primitive valueDescription ->
      printValueDescription valueDescription cmtTbl
    | Pstr_eval (expr, attrs) ->
      let exprDoc =
        let doc = printExpressionWithComments expr cmtTbl in
        match Parens.structureExpr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        printAttributes attrs;
        exprDoc;
      ]
    | Pstr_attribute attr -> Doc.concat [
        Doc.text "@";
        printAttributeWithComments attr cmtTbl
      ]
    | Pstr_extension (extension, attrs) -> Doc.concat [
        printAttributes attrs;
        Doc.concat [printExtensionWithComments ~atModuleLvl:true extension cmtTbl];
      ]
    | Pstr_include includeDeclaration ->
      printIncludeDeclaration includeDeclaration cmtTbl
    | Pstr_open openDescription ->
      printOpenDescription openDescription cmtTbl
    | Pstr_modtype modTypeDecl ->
      printModuleTypeDeclaration modTypeDecl cmtTbl
    | Pstr_module moduleBinding ->
      printModuleBinding ~isRec:false moduleBinding cmtTbl 0
    | Pstr_recmodule moduleBindings ->
      printListi
        ~getLoc:(fun mb -> mb.Parsetree.pmb_loc)
        ~nodes:moduleBindings
        ~print:(printModuleBinding ~isRec:true)
        cmtTbl
    | Pstr_exception extensionConstructor ->
      printExceptionDef extensionConstructor cmtTbl
    | Pstr_typext typeExtension ->
      printTypeExtension typeExtension cmtTbl
    | Pstr_class _ | Pstr_class_type _ -> Doc.nil

  and printTypeExtension (te : Parsetree.type_extension) cmtTbl =
    let prefix = Doc.text "type " in
    let name = printLidentPath te.ptyext_path cmtTbl in
    let typeParams = printTypeParams te.ptyext_params cmtTbl in
    let extensionConstructors =
      let ecs = te.ptyext_constructors in
      let forceBreak =
        match (ecs, List.rev ecs) with
        | (first::_, last::_) ->
          first.pext_loc.loc_start.pos_lnum > te.ptyext_path.loc.loc_end.pos_lnum ||
          first.pext_loc.loc_start.pos_lnum < last.pext_loc.loc_end.pos_lnum
        | _ -> false
        in
      let privateFlag = match te.ptyext_private with
      | Asttypes.Private -> Doc.concat [
          Doc.text "private";
          Doc.line;
        ]
      | Public -> Doc.nil
      in
      let rows =
        printListi
         ~getLoc:(fun n -> n.Parsetree.pext_loc)
         ~print:printExtensionConstructor
         ~nodes: ecs
         ~forceBreak
         cmtTbl
      in
      Doc.breakableGroup ~forceBreak (
        Doc.indent (
          Doc.concat [
            Doc.line;
            privateFlag;
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
        printAttributes ~loc: te.ptyext_path.loc te.ptyext_attributes;
        prefix;
        name;
        typeParams;
        Doc.text " +=";
        extensionConstructors;
      ]
    )

  and printModuleBinding ~isRec moduleBinding cmtTbl i =
    let prefix = if i = 0 then
      Doc.concat [
        Doc.text "module ";
        if isRec then Doc.text "rec " else Doc.nil;
      ]
    else
      Doc.text "and "
    in
    let (modExprDoc, modConstraintDoc) =
      match moduleBinding.pmb_expr with
      | {pmod_desc = Pmod_constraint (modExpr, modType)} ->
        (
          printModExpr modExpr cmtTbl,
          Doc.concat [
            Doc.text ": ";
            printModType modType cmtTbl
          ]
        )
      | modExpr ->
        (printModExpr modExpr cmtTbl, Doc.nil)
    in
    let modName =
      let doc = Doc.text moduleBinding.pmb_name.Location.txt in
      printComments doc cmtTbl moduleBinding.pmb_name.loc
    in
    let doc = Doc.concat [
      printAttributes ~loc:moduleBinding.pmb_name.loc moduleBinding.pmb_attributes;
      prefix;
      modName;
      modConstraintDoc;
      Doc.text " = ";
      modExprDoc;
    ] in
    printComments doc cmtTbl moduleBinding.pmb_loc

  and printModuleTypeDeclaration (modTypeDecl : Parsetree.module_type_declaration) cmtTbl =
    let modName =
      let doc = Doc.text modTypeDecl.pmtd_name.txt in
      printComments doc cmtTbl modTypeDecl.pmtd_name.loc
    in
    Doc.concat [
      printAttributes modTypeDecl.pmtd_attributes;
      Doc.text "module type ";
      modName;
      (match modTypeDecl.pmtd_type with
      | None -> Doc.nil
      | Some modType -> Doc.concat [
          Doc.text " = ";
          printModType modType cmtTbl;
        ]);
    ]

  and printModType modType cmtTbl =
    let modTypeDoc = match modType.pmty_desc with
    | Parsetree.Pmty_ident longident ->
      Doc.concat [
        printAttributes ~loc:longident.loc modType.pmty_attributes;
        printLongidentLocation longident cmtTbl
      ]
    | Pmty_signature signature ->
      let signatureDoc = Doc.breakableGroup ~forceBreak:true (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.line;
              printSignature signature cmtTbl;
            ]
          );
          Doc.line;
          Doc.rbrace;
        ]
      ) in
      Doc.concat [
        printAttributes modType.pmty_attributes;
        signatureDoc
      ]
    | Pmty_functor _ ->
      let (parameters, returnType) = ParsetreeViewer.functorType modType in
      let parametersDoc = match parameters with
      | [] -> Doc.nil
      | [attrs, {Location.txt = "_"; loc}, Some modType] ->
        let cmtLoc =
          {loc with loc_end = modType.Parsetree.pmty_loc.loc_end}
        in
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map printAttribute attrs);
          Doc.line;
        ] in
        let doc = Doc.concat [
          attrs;
          printModType modType cmtTbl
        ] in
        printComments doc cmtTbl cmtLoc
      | params ->
        Doc.group (
          Doc.concat [
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map (fun (attrs, lbl, modType) ->
                    let cmtLoc = match modType with
                    | None -> lbl.Asttypes.loc
                    | Some modType ->
                      {lbl.Asttypes.loc with loc_end = modType.Parsetree.pmty_loc.loc_end}
                    in
                    let attrs = match attrs with
                    | [] -> Doc.nil
                    | attrs -> Doc.concat [
                      Doc.join ~sep:Doc.line (List.map printAttribute attrs);
                      Doc.line;
                    ] in
                    let lblDoc = if lbl.Location.txt = "_" then Doc.nil
                      else
                        let doc = Doc.text lbl.txt in
                        printComments doc cmtTbl lbl.loc
                    in
                    let doc = Doc.concat [
                      attrs;
                      lblDoc;
                      (match modType with
                      | None -> Doc.nil
                      | Some modType -> Doc.concat [
                        if lbl.txt = "_" then Doc.nil else Doc.text ": ";
                        printModType modType cmtTbl;
                      ]);
                    ] in
                    printComments doc cmtTbl cmtLoc
                  ) params
                );
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
            Doc.rparen;
          ]
        )
      in
      let returnDoc =
        let doc = printModType returnType cmtTbl in
        if Parens.modTypeFunctorReturn returnType then addParens doc else doc
      in
      Doc.group (
        Doc.concat [
          parametersDoc;
          Doc.group (
            Doc.concat [
            Doc.text " =>";
            Doc.line;
            returnDoc;
            ]
          )
        ]
      )
    | Pmty_typeof modExpr -> Doc.concat [
        Doc.text "module type of ";
        printModExpr modExpr cmtTbl
      ]
    | Pmty_extension extension -> printExtensionWithComments ~atModuleLvl:false extension cmtTbl
    | Pmty_alias longident -> Doc.concat [
        Doc.text "module ";
        printLongidentLocation longident cmtTbl;
      ]
    | Pmty_with (modType, withConstraints) ->
      let operand =
        let doc = printModType modType cmtTbl in
        if Parens.modTypeWithOperand modType then addParens doc else doc
      in
      Doc.group (
        Doc.concat [
          operand;
          Doc.indent (
            Doc.concat [
              Doc.line;
              printWithConstraints withConstraints cmtTbl;
            ]
          )
        ]
      )
    in
    let attrsAlreadyPrinted = match modType.pmty_desc with
    | Pmty_functor _ | Pmty_signature _ | Pmty_ident _ -> true
    | _ -> false
    in
    let doc =Doc.concat [
      if attrsAlreadyPrinted then Doc.nil else printAttributes modType.pmty_attributes;
      modTypeDoc;
    ] in
    printComments doc cmtTbl modType.pmty_loc

  and printWithConstraints withConstraints cmtTbl =
    let rows = List.mapi (fun i withConstraint ->
      Doc.group (
        Doc.concat [
          if i == 0 then Doc.text "with " else Doc.text "and ";
          printWithConstraint withConstraint cmtTbl;
        ]
      )
    ) withConstraints
    in
    Doc.join ~sep:Doc.line rows

  and printWithConstraint (withConstraint : Parsetree.with_constraint) cmtTbl =
    match withConstraint with
    (* with type X.t = ... *)
    | Pwith_type (longident, typeDeclaration) ->
      Doc.group (printTypeDeclaration
        ~name:(printLidentPath longident cmtTbl)
        ~equalSign:"="
        ~recFlag:Doc.nil
        0
        typeDeclaration
        CommentTable.empty)
    (* with module X.Y = Z *)
    | Pwith_module ({txt = longident1}, {txt = longident2}) ->
        Doc.concat [
          Doc.text "module ";
          printLongident longident1;
          Doc.text " =";
          Doc.indent (
            Doc.concat [
              Doc.line;
              printLongident longident2;
            ]
          )
        ]
    (* with type X.t := ..., same format as [Pwith_type] *)
    | Pwith_typesubst (longident, typeDeclaration) ->
      Doc.group(printTypeDeclaration
        ~name:(printLidentPath longident cmtTbl)
        ~equalSign:":="
        ~recFlag:Doc.nil
        0
        typeDeclaration
        CommentTable.empty)
    | Pwith_modsubst ({txt = longident1}, {txt = longident2}) ->
      Doc.concat [
        Doc.text "module ";
        printLongident longident1;
        Doc.text " :=";
        Doc.indent (
          Doc.concat [
            Doc.line;
            printLongident longident2;
          ]
        )
      ]

  and printSignature signature cmtTbl =
    match signature with
    | [] -> printCommentsInside cmtTbl Location.none
    | signature ->
      printList
        ~getLoc:(fun s -> s.Parsetree.psig_loc)
        ~nodes:signature
        ~print:printSignatureItem
        cmtTbl

  and printSignatureItem (si : Parsetree.signature_item) cmtTbl =
    match si.psig_desc with
    | Parsetree.Psig_value valueDescription ->
      printValueDescription valueDescription cmtTbl
    | Psig_type (recFlag, typeDeclarations) ->
      let recFlag = match recFlag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
      in
      printTypeDeclarations ~recFlag typeDeclarations cmtTbl
    | Psig_typext typeExtension ->
      printTypeExtension typeExtension cmtTbl
    | Psig_exception extensionConstructor ->
      printExceptionDef extensionConstructor cmtTbl
    | Psig_module moduleDeclaration ->
      printModuleDeclaration moduleDeclaration cmtTbl
    | Psig_recmodule moduleDeclarations ->
      printRecModuleDeclarations moduleDeclarations cmtTbl
    | Psig_modtype modTypeDecl ->
      printModuleTypeDeclaration modTypeDecl cmtTbl
    | Psig_open openDescription ->
      printOpenDescription openDescription cmtTbl
    | Psig_include includeDescription ->
      printIncludeDescription includeDescription cmtTbl
    | Psig_attribute attr -> Doc.concat [
        Doc.text "@";
        printAttributeWithComments attr cmtTbl
      ]
    | Psig_extension (extension, attrs) -> Doc.concat [
        printAttributes attrs;
        Doc.concat [printExtensionWithComments ~atModuleLvl:true extension cmtTbl];
      ]
    | Psig_class _ | Psig_class_type _ -> Doc.nil

  and printRecModuleDeclarations moduleDeclarations cmtTbl =
      printListi
        ~getLoc:(fun n -> n.Parsetree.pmd_loc)
        ~nodes:moduleDeclarations
        ~print:printRecModuleDeclaration
        cmtTbl

 and printRecModuleDeclaration md cmtTbl i =
    let body = match md.pmd_type.pmty_desc with
    | Parsetree.Pmty_alias longident ->
      Doc.concat [Doc.text " = "; printLongidentLocation longident cmtTbl]
    | _ ->
      let needsParens = match md.pmd_type.pmty_desc with
      | Pmty_with _ -> true
      | _ -> false
      in
      let modTypeDoc =
        let doc = printModType md.pmd_type cmtTbl in
        if needsParens then addParens doc else doc
      in
      Doc.concat [Doc.text ": "; modTypeDoc]
    in
    let prefix = if i < 1 then "module rec " else "and " in
    Doc.concat [
      printAttributes ~loc:md.pmd_name.loc md.pmd_attributes;
      Doc.text prefix;
      printComments (Doc.text md.pmd_name.txt) cmtTbl md.pmd_name.loc;
      body
    ]

  and printModuleDeclaration (md: Parsetree.module_declaration) cmtTbl =
    let body = match md.pmd_type.pmty_desc with
    | Parsetree.Pmty_alias longident ->
      Doc.concat [Doc.text " = "; printLongidentLocation longident cmtTbl]
    | _ -> Doc.concat [Doc.text ": "; printModType md.pmd_type cmtTbl]
    in
    Doc.concat [
      printAttributes ~loc:md.pmd_name.loc md.pmd_attributes;
      Doc.text "module ";
      printComments (Doc.text md.pmd_name.txt) cmtTbl md.pmd_name.loc;
      body
    ]

  and printOpenDescription (openDescription : Parsetree.open_description) p =
    Doc.concat [
      printAttributes openDescription.popen_attributes;
      Doc.text "open";
      (match openDescription.popen_override with
      | Asttypes.Fresh -> Doc.space
      | Asttypes.Override -> Doc.text "! ");
      printLongidentLocation openDescription.popen_lid p
    ]

  and printIncludeDescription (includeDescription: Parsetree.include_description) cmtTbl =
    Doc.concat [
      printAttributes includeDescription.pincl_attributes;
      Doc.text "include ";
      printModType includeDescription.pincl_mod cmtTbl;
    ]

  and printIncludeDeclaration (includeDeclaration : Parsetree.include_declaration)  cmtTbl =
    let isJsFfiImport = List.exists (fun attr ->
      match attr with
      | ({Location.txt = "ns.jsFfi"}, _) -> true
      | _ -> false
    ) includeDeclaration.pincl_attributes
    in
    if isJsFfiImport then
      printJsFfiImportDeclaration includeDeclaration cmtTbl
    else
      Doc.concat [
        printAttributes includeDeclaration.pincl_attributes;
        Doc.text "include ";
        let includeDoc =
          printModExpr includeDeclaration.pincl_mod cmtTbl
        in
        if Parens.includeModExpr includeDeclaration.pincl_mod then
          addParens includeDoc
        else includeDoc;
      ]

  and printJsFfiImport (valueDescription: Parsetree.value_description) cmtTbl =
    let attrs = List.filter (fun attr ->
      match attr with
      | ({Location.txt = "bs.val" | "genType.import" | "bs.scope" }, _) -> false
      | _ -> true
    ) valueDescription.pval_attributes in
    let (ident, alias) = match valueDescription.pval_prim with
    | primitive::_ ->
      if primitive <> valueDescription.pval_name.txt then
        (
          printIdentLike primitive,
          Doc.concat [
            Doc.text " as ";
            printIdentLike valueDescription.pval_name.txt;
          ]
        )
      else
        (printIdentLike primitive, Doc.nil)
    | _ ->
      (printIdentLike valueDescription.pval_name.txt, Doc.nil)
    in
    Doc.concat [
      printAttributes ~loc:valueDescription.pval_name.loc attrs;
      ident;
      alias;
      Doc.text ": ";
      printTypExpr valueDescription.pval_type cmtTbl;
    ]

  and printJsFfiImportScope (scope: ParsetreeViewer.jsImportScope) =
    match scope with
    | JsGlobalImport -> Doc.nil
    | JsModuleImport modName ->
      Doc.concat [
        Doc.text " from ";
        Doc.doubleQuote;
        Doc.text modName;
        Doc.doubleQuote;
      ]
    | JsScopedImport idents ->
      Doc.concat [
        Doc.text " from ";
        Doc.join ~sep:Doc.dot (List.map Doc.text idents)
      ]

  and printJsFfiImportDeclaration (includeDeclaration: Parsetree.include_declaration) cmtTbl =
    let attrs = List.filter (fun attr ->
      match attr with
      | ({Location.txt = "ns.jsFfi"}, _) -> false
      | _ -> true
    ) includeDeclaration.pincl_attributes
    in
    let imports = ParsetreeViewer.extractValueDescriptionFromModExpr includeDeclaration.pincl_mod in
    let scope = match imports with
    | vd::_ -> ParsetreeViewer.classifyJsImport vd
    | [] -> ParsetreeViewer.JsGlobalImport
    in
    let scopeDoc = printJsFfiImportScope scope in
    Doc.group (
      Doc.concat [
        printAttributes attrs;
        Doc.text "import ";
        Doc.group (
          Doc.concat [
            Doc.lbrace;
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map (fun vd -> printJsFfiImport vd cmtTbl) imports
                )
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
            Doc.rbrace;
          ]
        );
        scopeDoc;
      ]
    )

  and printValueBindings ~recFlag (vbs: Parsetree.value_binding list) cmtTbl =
    printListi
      ~getLoc:(fun vb -> vb.Parsetree.pvb_loc)
      ~nodes:vbs
      ~print:(printValueBinding ~recFlag)
      cmtTbl

  and printValueDescription valueDescription cmtTbl =
    let isExternal =
      match valueDescription.pval_prim with | [] -> false | _ -> true
    in
    Doc.group (
      Doc.concat [
        printAttributes valueDescription.pval_attributes;
        Doc.text (if isExternal then "external " else "let ");
        printComments
          (printIdentLike valueDescription.pval_name.txt)
          cmtTbl
          valueDescription.pval_name.loc;
        Doc.text ": ";
        printTypExpr valueDescription.pval_type cmtTbl;
        if isExternal then
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
                    valueDescription.pval_prim
                  );
                ]
              )
            ]
          )
        else Doc.nil
      ]
    )

  and printTypeDeclarations ~recFlag typeDeclarations cmtTbl =
    printListi
      ~getLoc:(fun n -> n.Parsetree.ptype_loc)
      ~nodes:typeDeclarations
      ~print:(printTypeDeclaration2 ~recFlag)
      cmtTbl

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
  and printTypeDeclaration ~name ~equalSign ~recFlag i (td: Parsetree.type_declaration) cmtTbl =
    let (hasGenType, attrs) = ParsetreeViewer.splitGenTypeAttr td.ptype_attributes in
    let attrs = printAttributes ~loc:td.ptype_loc attrs in
    let prefix = if i > 0 then
      Doc.concat [
        Doc.text "and ";
        if hasGenType then Doc.text "export " else Doc.nil
      ]
    else
      Doc.concat [
        Doc.text (if hasGenType then "export type " else "type ");
        recFlag
      ]
    in
    let typeName = name in
    let typeParams = printTypeParams td.ptype_params cmtTbl in
    let manifestAndKind = match td.ptype_kind with
    | Ptype_abstract ->
      begin match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) ->
        Doc.concat [
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printPrivateFlag td.ptype_private;
          printTypExpr typ cmtTbl;
        ]
      end
    | Ptype_open -> Doc.concat [
        Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
        printPrivateFlag td.ptype_private;
        Doc.text "..";
      ]
    | Ptype_record(lds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printTypExpr typ cmtTbl;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
        printPrivateFlag td.ptype_private;
        printRecordDeclaration lds cmtTbl;
      ]
    | Ptype_variant(cds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printTypExpr typ cmtTbl;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equalSign];
        printConstructorDeclarations ~privateFlag:td.ptype_private cds cmtTbl;
      ]
    in
    let constraints = printTypeDefinitionConstraints td.ptype_cstrs in
    Doc.group (
      Doc.concat [
        attrs;
        prefix;
        typeName;
        typeParams;
        manifestAndKind;
        constraints;
      ]
    )

  and printTypeDeclaration2 ~recFlag (td: Parsetree.type_declaration) cmtTbl i =
    let name =
      let doc = printIdentLike td.Parsetree.ptype_name.txt in
      printComments doc cmtTbl td.ptype_name.loc
    in
    let equalSign = "=" in
    let (hasGenType, attrs) = ParsetreeViewer.splitGenTypeAttr td.ptype_attributes in
    let attrs = printAttributes ~loc:td.ptype_loc attrs in
    let prefix = if i > 0 then
      Doc.concat [
        Doc.text "and ";
        if hasGenType then Doc.text "export " else Doc.nil
      ]
    else
      Doc.concat [
        Doc.text (if hasGenType then "export type " else "type ");
        recFlag
      ]
    in
    let typeName = name in
    let typeParams = printTypeParams td.ptype_params cmtTbl in
    let manifestAndKind = match td.ptype_kind with
    | Ptype_abstract ->
      begin match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) ->
        Doc.concat [
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printPrivateFlag td.ptype_private;
          printTypExpr typ cmtTbl;
        ]
      end
    | Ptype_open -> Doc.concat [
        Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
        printPrivateFlag td.ptype_private;
        Doc.text "..";
      ]
    | Ptype_record(lds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printTypExpr typ cmtTbl;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
        printPrivateFlag td.ptype_private;
        printRecordDeclaration lds cmtTbl;
      ]
    | Ptype_variant(cds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printTypExpr typ cmtTbl;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equalSign];
        printConstructorDeclarations ~privateFlag:td.ptype_private cds cmtTbl;
      ]
    in
    let constraints = printTypeDefinitionConstraints td.ptype_cstrs in
    Doc.group (
      Doc.concat [
        attrs;
        prefix;
        typeName;
        typeParams;
        manifestAndKind;
        constraints;
      ]
    )

  and printTypeDefinitionConstraints cstrs =
    match cstrs with
    | [] -> Doc.nil
    | cstrs -> Doc.indent (
        Doc.group (
          Doc.concat [
            Doc.line;
            Doc.group(
              Doc.join ~sep:Doc.line (
                List.map printTypeDefinitionConstraint cstrs
              )
            )
          ]
        )
      )

  and printTypeDefinitionConstraint ((typ1, typ2, _loc ): Parsetree.core_type * Parsetree.core_type * Location.t) =
    Doc.concat [
      Doc.text "constraint ";
      printTypExpr typ1 CommentTable.empty;
      Doc.text " = ";
      printTypExpr typ2 CommentTable.empty;
    ]

  and printPrivateFlag (flag : Asttypes.private_flag) = match flag with
    | Private -> Doc.text "private "
    | Public -> Doc.nil

  and printTypeParams typeParams cmtTbl =
    match typeParams with
    | [] -> Doc.nil
    | typeParams ->
      Doc.group (
        Doc.concat [
          Doc.lessThan;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun typeParam ->
                  let doc = printTypeParam typeParam cmtTbl in
                  printComments doc cmtTbl (fst typeParam).Parsetree.ptyp_loc
                ) typeParams
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.greaterThan;
        ]
      )

  and printTypeParam (param : (Parsetree.core_type * Asttypes.variance)) cmtTbl =
    let (typ, variance) = param in
    let printedVariance = match variance with
    | Covariant -> Doc.text "+"
    | Contravariant -> Doc.text "-"
    | Invariant -> Doc.nil
    in
    Doc.concat [
      printedVariance;
      printTypExpr typ cmtTbl
    ]

  and printRecordDeclaration (lds: Parsetree.label_declaration list) cmtTbl =
    let forceBreak = match (lds, List.rev lds) with
    | (first::_, last::_) ->
       first.pld_loc.loc_start.pos_lnum < last.pld_loc.loc_end.pos_lnum
    | _ -> false
    in
    Doc.breakableGroup ~forceBreak (
      Doc.concat [
        Doc.lbrace;
        Doc.indent (
          Doc.concat [
            Doc.softLine;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line])
              (List.map (fun ld ->
                let doc = printLabelDeclaration ld cmtTbl in
                printComments doc cmtTbl ld.Parsetree.pld_loc
              ) lds)
          ]
        );
        Doc.trailingComma;
        Doc.softLine;
        Doc.rbrace;
      ]
    )

  and printConstructorDeclarations
    ~privateFlag (cds: Parsetree.constructor_declaration list) cmtTbl
  =
    let forceBreak = match (cds, List.rev cds) with
    | (first::_, last::_) ->
       first.pcd_loc.loc_start.pos_lnum < last.pcd_loc.loc_end.pos_lnum
    | _ -> false
    in
    let privateFlag = match privateFlag with
    | Asttypes.Private -> Doc.concat [
        Doc.text "private";
        Doc.line;
      ]
    | Public -> Doc.nil
    in
    let rows =
      printListi
        ~getLoc:(fun cd -> cd.Parsetree.pcd_loc)
        ~nodes:cds
        ~print:(fun cd cmtTbl i ->
          let doc = printConstructorDeclaration2 i cd cmtTbl in
          printComments doc cmtTbl cd.Parsetree.pcd_loc
        )
        ~forceBreak
        cmtTbl
    in
    Doc.breakableGroup ~forceBreak (
      Doc.indent (
        Doc.concat [
          Doc.line;
          privateFlag;
          rows;
        ]
      )
    )

  and printConstructorDeclaration2 i (cd : Parsetree.constructor_declaration) cmtTbl =
    let attrs = printAttributes cd.pcd_attributes in
    let bar = if i > 0 then Doc.text "| "
    else Doc.ifBreaks (Doc.text "| ") Doc.nil
    in
    let constrName =
      let doc = Doc.text cd.pcd_name.txt in
      printComments doc cmtTbl cd.pcd_name.loc
    in
    let constrArgs = printConstructorArguments ~indent:true cd.pcd_args cmtTbl in
    let gadt = match cd.pcd_res with
    | None -> Doc.nil
    | Some(typ) -> Doc.indent (
        Doc.concat [
          Doc.text ": ";
          printTypExpr typ cmtTbl;
        ]
      )
    in
    Doc.concat [
      bar;
      Doc.group (
        Doc.concat [
          attrs; (* TODO: fix parsing of attributes, so when can print them above the bar? *)
          constrName;
          constrArgs;
          gadt;
        ]
      )
    ]

  and printConstructorArguments ~indent (cdArgs : Parsetree.constructor_arguments) cmtTbl =
    match cdArgs with
    | Pcstr_tuple [] -> Doc.nil
    | Pcstr_tuple types ->
      let args = Doc.concat [
        Doc.lparen;
          Doc.indent (
            Doc.concat [
            Doc.softLine;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
              List.map (fun typexpr ->
                printTypExpr typexpr cmtTbl
              ) types
            )
          ]
        );
        Doc.trailingComma;
        Doc.softLine;
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
            Doc.softLine;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line])
              (List.map (fun ld ->
                let doc = printLabelDeclaration ld cmtTbl in
                printComments doc cmtTbl ld.Parsetree.pld_loc
              ) lds)
          ]
        );
        Doc.trailingComma;
        Doc.softLine;
        Doc.rbrace;
        Doc.rparen;
      ] in
      if indent then Doc.indent args else args

  and printLabelDeclaration (ld : Parsetree.label_declaration) cmtTbl =
    let attrs = printAttributes ~loc:ld.pld_name.loc ld.pld_attributes in
    let mutableFlag = match ld.pld_mutable with
    | Mutable -> Doc.text "mutable "
    | Immutable -> Doc.nil
    in
    let name =
      let doc = printIdentLike ld.pld_name.txt in
      printComments doc cmtTbl ld.pld_name.loc
    in
    Doc.group (
      Doc.concat [
        attrs;
        mutableFlag;
        name;
        Doc.text ": ";
        printTypExpr ld.pld_type cmtTbl;
      ]
    )

  and printTypExpr (typExpr : Parsetree.core_type) cmtTbl =
    let renderedType = match typExpr.ptyp_desc with
    | Ptyp_any -> Doc.text "_"
    | Ptyp_var var -> Doc.concat [
        Doc.text "'";
        printIdentLike var;
      ]
    | Ptyp_extension(extension) ->
      printExtensionWithComments ~atModuleLvl:false extension cmtTbl
    | Ptyp_alias(typ, alias) ->
      let typ =
        (* Technically type t = (string, float) => unit as 'x, doesn't require
         * parens around the arrow expression. This is very confusing though.
         * Is the "as" part of "unit" or "(string, float) => unit". By printing
         * parens we guide the user towards its meaning.*)
        let needsParens = match typ.ptyp_desc with
        | Ptyp_arrow _ -> true
        | _ -> false
        in
        let doc = printTypExpr typ cmtTbl in
        if needsParens then
          Doc.concat [Doc.lparen; doc; Doc.rparen]
        else
          doc
      in
      Doc.concat [typ; Doc.text " as "; Doc.concat [Doc.text "'"; printIdentLike alias]]
    | Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")}, [{ptyp_desc = Ptyp_object (_fields, _openFlag)} as typ]) ->
      let bsObject = printTypExpr typ cmtTbl in
      begin match typExpr.ptyp_attributes with
      | [] -> bsObject
      | attrs ->
        Doc.concat [
          Doc.group (
            Doc.join ~sep:Doc.line (List.map printAttribute attrs)
          );
          Doc.space;
          printTypExpr typ cmtTbl;
        ]
      end
    | Ptyp_constr(longidentLoc, [{ ptyp_desc = Parsetree.Ptyp_tuple tuple }]) ->
      let constrName = printLidentPath longidentLoc cmtTbl in
      Doc.group(
        Doc.concat([
          constrName;
          Doc.lessThan;
          printTupleType ~inline:true tuple cmtTbl;
          Doc.greaterThan;
        ])
      )
    | Ptyp_constr(longidentLoc, constrArgs) ->
      let constrName = printLidentPath longidentLoc cmtTbl in
      begin match constrArgs with
      | [] -> constrName
      | [{
          Parsetree.ptyp_desc =
            Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")},
          [{ptyp_desc = Ptyp_object (fields, openFlag)}])
        }] ->
        Doc.concat([
          constrName;
          Doc.lessThan;
          printBsObjectSugar ~inline:true fields openFlag cmtTbl;
          Doc.greaterThan;
        ])
      | _args -> Doc.group(
        Doc.concat([
          constrName;
          Doc.lessThan;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map
                  (fun typexpr -> printTypExpr typexpr cmtTbl)
                  constrArgs
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.greaterThan;
        ])
      )
      end
    | Ptyp_arrow _ ->
      let (attrsBefore, args, returnType) = ParsetreeViewer.arrowType typExpr in
      let returnTypeNeedsParens = match returnType.ptyp_desc with
      | Ptyp_alias _ -> true
      | _ -> false
      in
      let returnDoc =
        let doc = printTypExpr returnType cmtTbl in
        if returnTypeNeedsParens then
          Doc.concat [Doc.lparen; doc; Doc.rparen]
        else doc
      in
      let (isUncurried, attrs) =
        ParsetreeViewer.processUncurriedAttribute attrsBefore
      in
      begin match args with
      | [] -> Doc.nil
      | [([], Nolabel, n)] when not isUncurried ->
          let hasAttrsBefore = not (attrs = []) in
          let attrs = if hasAttrsBefore then
            Doc.concat [
              Doc.join ~sep:Doc.line (List.map printAttribute attrsBefore);
              Doc.space;
            ]
          else Doc.nil
          in
          let typDoc =
            let doc = printTypExpr n cmtTbl in
            match n.ptyp_desc with
            | Ptyp_arrow _ | Ptyp_tuple _ -> addParens doc
            | _ -> doc
          in
          Doc.group (
            Doc.concat [
              Doc.group attrs;
              Doc.group (
                if hasAttrsBefore then
                  Doc.concat [
                    Doc.lparen;
                    Doc.indent (
                      Doc.concat [
                        Doc.softLine;
                        typDoc;
                        Doc.text " => ";
                        returnDoc;
                      ]
                    );
                    Doc.softLine;
                    Doc.rparen
                  ]
                else
                Doc.concat [
                  typDoc;
                  Doc.text " => ";
                  returnDoc;
                ]
              )
            ]
          )
      | args ->
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
            Doc.join ~sep:Doc.line (List.map printAttribute attrs);
            Doc.space;
          ]
        in
        let renderedArgs = Doc.concat [
          attrs;
          Doc.text "(";
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              if isUncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun tp -> printTypeParameter tp cmtTbl) args
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.text ")";
        ] in
        Doc.group (
          Doc.concat [
            renderedArgs;
            Doc.text " => ";
            returnDoc;
          ]
        )
      end
    | Ptyp_tuple types -> printTupleType ~inline:false types cmtTbl
    | Ptyp_object (fields, openFlag) ->
      printBsObjectSugar ~inline:false fields openFlag cmtTbl
    | Ptyp_poly([], typ) ->
      printTypExpr typ cmtTbl
    | Ptyp_poly(stringLocs, typ) ->
      Doc.concat [
        Doc.join ~sep:Doc.space (List.map (fun {Location.txt; loc} ->
          let doc = Doc.concat [Doc.text "'"; Doc.text txt] in
          printComments doc cmtTbl loc
          ) stringLocs);
        Doc.dot;
        Doc.space;
        printTypExpr typ cmtTbl
      ]
    | Ptyp_package packageType ->
      printPackageType ~printModuleKeywordAndParens:true packageType cmtTbl
    | Ptyp_class _ ->
      Doc.text "classes are not supported in types"
    | Ptyp_variant (rowFields, closedFlag, labelsOpt) ->
      let printRowField = function
      | Parsetree.Rtag ({txt}, attrs, true, []) ->
        Doc.concat [
          printAttributes attrs;
          Doc.concat [Doc.text "#"; printIdentLike ~allowUident:true txt]
        ]
      | Rtag ({txt}, attrs, truth, types) ->
        let doType t = match t.Parsetree.ptyp_desc with
        | Ptyp_tuple _ -> printTypExpr t cmtTbl
        | _ -> Doc.concat [ Doc.lparen; printTypExpr t cmtTbl; Doc.rparen ]
        in
        let printedTypes = List.map doType types in
        let cases = Doc.join ~sep:(Doc.concat [Doc.line; Doc.text "& "]) printedTypes in
        let cases = if truth then Doc.concat [Doc.line; Doc.text "& "; cases] else cases in
        Doc.group (Doc.concat [
          printAttributes attrs;
          Doc.concat [Doc.text "#"; printIdentLike ~allowUident:true txt];
          cases
        ])
      | Rinherit coreType ->
        printTypExpr coreType cmtTbl
      in
      let docs = List.map printRowField rowFields in
      let cases = Doc.join ~sep:(Doc.concat [Doc.line; Doc.text "| "]) docs in
      let cases = if docs = [] then cases else Doc.concat [Doc.text "| "; cases] in
      let openingSymbol =
        if closedFlag = Open
        then Doc.greaterThan
        else if labelsOpt = None
        then Doc.nil
        else Doc.lessThan in
      let hasLabels = labelsOpt <> None && labelsOpt <> Some [] in
      let labels = match labelsOpt with
      | None
      | Some([]) ->
        Doc.nil
      | Some(labels) ->
        Doc.concat (List.map (fun label -> Doc.concat [Doc.line; Doc.text "#" ; printIdentLike ~allowUident:true label] ) labels)
      in
      let closingSymbol = if hasLabels then Doc.text " >" else Doc.nil in
      Doc.group (Doc.concat [Doc.lbracket; openingSymbol; Doc.line; cases; closingSymbol; labels; Doc.line; Doc.rbracket])
    in
    let shouldPrintItsOwnAttributes = match typExpr.ptyp_desc with
    | Ptyp_arrow _ (* es6 arrow types print their own attributes *)
    | Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")}, _) -> true
    | _ -> false
    in
    let doc = begin match typExpr.ptyp_attributes with
    | _::_ as attrs when not shouldPrintItsOwnAttributes ->
      Doc.group (
        Doc.concat [
          printAttributes attrs;
          renderedType;
        ]
      )
    | _ -> renderedType
    end
    in
    printComments doc cmtTbl typExpr.ptyp_loc

  and printBsObjectSugar ~inline fields openFlag cmtTbl =
    let doc = match fields with
    | [] -> Doc.concat [
        Doc.lbrace;
        (match openFlag with
        | Asttypes.Closed -> Doc.dot
        | Open -> Doc.dotdot);
        Doc.rbrace
      ]
    | fields ->
      Doc.concat [
        Doc.lbrace;
        (match openFlag with
        | Asttypes.Closed -> Doc.nil
        | Open -> Doc.dotdot);
        Doc.indent (
          Doc.concat [
            Doc.softLine;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
              List.map (fun field -> printObjectField field cmtTbl) fields
            )
          ]
        );
        Doc.trailingComma;
        Doc.softLine;
        Doc.rbrace;
      ]
    in
    if inline then doc else Doc.group doc

  and printTupleType ~inline (types: Parsetree.core_type list) cmtTbl =
    let tuple = Doc.concat([
      Doc.lparen;
      Doc.indent (
        Doc.concat([
          Doc.softLine;
          Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
            List.map (fun typexpr -> printTypExpr typexpr cmtTbl) types
          )
        ])
      );
      Doc.trailingComma;
      Doc.softLine;
      Doc.rparen;
    ])
    in
    if inline == false then Doc.group(tuple) else tuple

  and printObjectField (field : Parsetree.object_field) cmtTbl =
    match field with
    | Otag (labelLoc, attrs, typ) ->
      let lbl =
        let doc = Doc.text ("\"" ^ labelLoc.txt ^ "\"") in
        printComments doc cmtTbl labelLoc.loc
      in
      let doc = Doc.concat [
        printAttributes ~loc:labelLoc.loc attrs;
        lbl;
        Doc.text ": ";
        printTypExpr typ cmtTbl;
      ] in
      let cmtLoc = {labelLoc.loc with loc_end = typ.ptyp_loc.loc_end} in
      printComments doc cmtTbl cmtLoc
    | _ -> Doc.nil

  (* es6 arrow type arg
   * type t = (~foo: string, ~bar: float=?, unit) => unit
   * i.e. ~foo: string, ~bar: float *)
  and printTypeParameter (attrs, lbl, typ) cmtTbl =
    let (isUncurried, attrs) = ParsetreeViewer.processUncurriedAttribute attrs in
    let uncurried = if isUncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil in
    let attrs = match attrs with
    | [] -> Doc.nil
    | attrs -> Doc.concat [
      Doc.join ~sep:Doc.line (List.map printAttribute attrs);
      Doc.line;
    ] in
    let label = match lbl with
    | Asttypes.Nolabel -> Doc.nil
    | Labelled lbl -> Doc.concat [
        Doc.text "~";
        printIdentLike lbl;
        Doc.text ": ";
      ]
    | Optional lbl -> Doc.concat [
        Doc.text "~";
        printIdentLike lbl;
        Doc.text ": ";
      ]
    in
    let optionalIndicator = match lbl with
    | Asttypes.Nolabel
    | Labelled _ -> Doc.nil
    | Optional _lbl -> Doc.text "=?"
    in
    let doc = Doc.group (
      Doc.concat [
        uncurried;
        attrs;
        label;
        printTypExpr typ cmtTbl;
        optionalIndicator;
      ]
    ) in
    printComments doc cmtTbl typ.ptyp_loc

  and printValueBinding ~recFlag vb cmtTbl i =
    let (hasGenType, attrs) = ParsetreeViewer.splitGenTypeAttr vb.pvb_attributes in
    let attrs = printAttributes ~loc:vb.pvb_pat.ppat_loc attrs in
		let header =
      if i == 0 then
        Doc.concat [
          if hasGenType then Doc.text "export " else Doc.text "let ";
          recFlag
      ] else
        Doc.concat [
          Doc.text "and ";
          if hasGenType then Doc.text "export " else Doc.nil
        ]
		in
    match vb with
    | {pvb_pat =
        {ppat_desc = Ppat_constraint (pattern, {ptyp_desc = Ptyp_poly _})};
       pvb_expr =
         {pexp_desc = Pexp_newtype _} as expr
      }   ->
      let (_attrs, parameters, returnExpr) = ParsetreeViewer.funExpr expr in
      let abstractType = match parameters with
      | [NewTypes {locs = vars}] ->
        Doc.concat [
          Doc.text "type ";
          Doc.join ~sep:Doc.space (List.map (fun var -> Doc.text var.Asttypes.txt) vars);
          Doc.dot;
        ]
      | _ -> Doc.nil
      in
      begin match returnExpr.pexp_desc with
      | Pexp_constraint (expr, typ) ->
        Doc.group (
          Doc.concat [
            attrs;
            header;
            printPattern pattern cmtTbl;
            Doc.text ":";
            Doc.indent (
              Doc.concat [
                Doc.line;
                abstractType;
                Doc.space;
                printTypExpr typ cmtTbl;
                Doc.text " =";
                Doc.concat [
                  Doc.line;
                  printExpressionWithComments expr cmtTbl;
                ]
              ]
            )
          ]
        )
      | _ -> Doc.nil
      end
    | _ ->
    let (optBraces, expr) = ParsetreeViewer.processBracesAttr vb.pvb_expr in
    let printedExpr =
      let doc = printExpressionWithComments vb.pvb_expr cmtTbl in
      match Parens.expr vb.pvb_expr with
      | Parens.Parenthesized -> addParens doc
      | Braced braces  -> printBraces doc expr braces
      | Nothing -> doc
    in
    if ParsetreeViewer.isPipeExpr vb.pvb_expr then
      Doc.customLayout [
        Doc.group (
          Doc.concat [
            attrs;
            header;
            printPattern vb.pvb_pat cmtTbl;
            Doc.text " =";
            Doc.space;
            printedExpr;
          ]
        );
        Doc.group (
          Doc.concat [
            attrs;
            header;
            printPattern vb.pvb_pat cmtTbl;
            Doc.text " =";
            Doc.indent (
              Doc.concat [
                Doc.line;
                printedExpr;
              ]
            )
          ]
        );
      ]
		else
      let shouldIndent =
        match optBraces with
        | Some _ -> false
        | _ ->
          ParsetreeViewer.isBinaryExpression expr ||
          (match vb.pvb_expr with
          | {
              pexp_attributes = [({Location.txt="ns.ternary"}, _)];
              pexp_desc = Pexp_ifthenelse (ifExpr, _, _)
            }  ->
            ParsetreeViewer.isBinaryExpression ifExpr || ParsetreeViewer.hasAttributes ifExpr.pexp_attributes
        | { pexp_desc = Pexp_newtype _} -> false
        | e ->
            ParsetreeViewer.hasAttributes e.pexp_attributes ||
            ParsetreeViewer.isArrayAccess e
          )
      in
      Doc.group (
        Doc.concat [
          attrs;
          header;
          printPattern vb.pvb_pat cmtTbl;
          Doc.text " =";
          if shouldIndent then
            Doc.indent (
              Doc.concat [
                Doc.line;
                printedExpr;
              ]
            )
          else
            Doc.concat [
              Doc.space;
              printedExpr;
            ]
        ]
      )

  and printPackageType ~printModuleKeywordAndParens (packageType: Parsetree.package_type) cmtTbl =
    let doc = match packageType with
    | (longidentLoc, []) -> Doc.group(
        Doc.concat [
          printLongidentLocation longidentLoc cmtTbl;
        ]
      )
    | (longidentLoc, packageConstraints) -> Doc.group(
        Doc.concat [
          printLongidentLocation longidentLoc cmtTbl;
          printPackageConstraints packageConstraints cmtTbl;
          Doc.softLine;
        ]
      )
    in
    if printModuleKeywordAndParens then
      Doc.concat[
        Doc.text "module(";
        doc;
        Doc.rparen
      ]
    else
      doc

  and printPackageConstraints packageConstraints cmtTbl  =
    Doc.concat [
      Doc.text " with";
      Doc.indent (
        Doc.concat [
          Doc.line;
          Doc.join ~sep:Doc.line (
            List.mapi (fun i pc ->
              let (longident, typexpr) = pc in
              let cmtLoc = {longident.Asttypes.loc with
                loc_end = typexpr.Parsetree.ptyp_loc.loc_end
              } in
              let doc = printPackageConstraint i cmtTbl pc in
              printComments doc cmtTbl cmtLoc
            ) packageConstraints
          )
        ]
      )
    ]

  and printPackageConstraint i cmtTbl (longidentLoc, typ) =
    let prefix = if i == 0 then Doc.text "type " else Doc.text "and type " in
    Doc.concat [
      prefix;
      printLongidentLocation longidentLoc cmtTbl;
      Doc.text " = ";
      printTypExpr typ cmtTbl;
    ]

  and printExtensionWithComments ~atModuleLvl (stringLoc, payload) cmtTbl =
    let extName =
      let doc = Doc.concat [
        Doc.text "%";
        if atModuleLvl then Doc.text "%" else Doc.nil;
        Doc.text stringLoc.Location.txt;
      ] in
      printComments doc cmtTbl stringLoc.Location.loc
    in
    match payload with
    | Parsetree.PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
      let exprDoc = printExpressionWithComments expr cmtTbl in
      let needsParens = match attrs with | [] -> false | _ -> true in
      Doc.group (
        Doc.concat [
          extName;
          addParens (
            Doc.concat [
              printAttributes attrs;
              if needsParens then addParens exprDoc else exprDoc;
            ]
          )
        ]
      )
    | _ -> extName

  and printPattern (p : Parsetree.pattern) cmtTbl =
    let patternWithoutAttributes = match p.ppat_desc with
    | Ppat_any -> Doc.text "_"
    | Ppat_var var -> printIdentLike var.txt
    | Ppat_constant c -> printConstant c
    | Ppat_tuple patterns ->
      Doc.group(
        Doc.concat([
          Doc.lparen;
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map (fun pat ->
                  printPattern pat cmtTbl) patterns)
            ])
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen
        ])
      )
    | Ppat_array [] ->
      Doc.concat [
        Doc.lbracket;
        printCommentsInside cmtTbl p.ppat_loc;
        Doc.rbracket;
      ]
    | Ppat_array patterns ->
      Doc.group(
        Doc.concat([
          Doc.text "[";
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map (fun pat ->
                  printPattern pat cmtTbl) patterns)
            ])
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.text "]";
        ])
      )
    | Ppat_construct({txt = Longident.Lident "()"}, _) ->
      Doc.concat [
        Doc.lparen;
        printCommentsInside cmtTbl p.ppat_loc;
        Doc.rparen;
      ]
    | Ppat_construct({txt = Longident.Lident "[]"}, _) ->
      Doc.concat [
        Doc.text "list[";
        printCommentsInside cmtTbl p.ppat_loc;
        Doc.rbracket;
      ]
    | Ppat_construct({txt = Longident.Lident "::"}, _) ->
      let (patterns, tail) = ParsetreeViewer.collectPatternsFromListConstruct [] p in
      let shouldHug = match (patterns, tail) with
      | ([pat],
        {ppat_desc = Ppat_construct({txt = Longident.Lident "[]"}, _)}) when ParsetreeViewer.isHuggablePattern pat -> true
      | _ -> false
      in
      let children = Doc.concat([
        if shouldHug then Doc.nil else Doc.softLine;
        Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
          (List.map (fun pat ->
            printPattern pat cmtTbl) patterns);
        begin match tail.Parsetree.ppat_desc with
        | Ppat_construct({txt = Longident.Lident "[]"}, _) -> Doc.nil
        | _ ->
          let doc = Doc.concat [Doc.text "..."; printPattern tail cmtTbl] in
          let tail = printComments doc cmtTbl tail.ppat_loc in
          Doc.concat([Doc.text ","; Doc.line; tail])
        end;
      ]) in
      Doc.group(
        Doc.concat([
          Doc.text "list[";
          if shouldHug then children else Doc.concat [
            Doc.indent children;
            Doc.ifBreaks (Doc.text ",") Doc.nil;
            Doc.softLine;
          ];
          Doc.rbracket;
        ])
      )
    | Ppat_construct(constrName, constructorArgs) ->
      let constrName = printLongident constrName.txt in
      let argsDoc = match constructorArgs with
      | None -> Doc.nil
      | Some({ppat_loc; ppat_desc = Ppat_construct ({txt = Longident.Lident "()"}, _)}) ->
        Doc.concat [
          Doc.lparen;
          printCommentsInside cmtTbl ppat_loc;
          Doc.rparen;
        ]
      | Some({ppat_desc = Ppat_tuple []; ppat_loc = loc}) ->
        Doc.concat [
          Doc.lparen;
          Doc.softLine;
          printCommentsInside cmtTbl loc;
          Doc.rparen;
        ]
      (* Some((1, 2) *)
      | Some({ppat_desc = Ppat_tuple [{ppat_desc = Ppat_tuple _} as arg]}) ->
        Doc.concat [
          Doc.lparen;
          printPattern arg cmtTbl;
          Doc.rparen;
        ]
      | Some({ppat_desc = Ppat_tuple patterns}) ->
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun pat -> printPattern pat cmtTbl) patterns
              );
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ]
      | Some(arg) ->
        let argDoc = printPattern arg cmtTbl in
        let shouldHug = ParsetreeViewer.isHuggablePattern arg in
        Doc.concat [
          Doc.lparen;
          if shouldHug then argDoc
          else Doc.concat [
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                argDoc;
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
          ];
          Doc.rparen;

        ]
      in
      Doc.group(Doc.concat [constrName; argsDoc])
    | Ppat_variant (label, None) ->
      Doc.concat [Doc.text "#"; printIdentLike ~allowUident:true label]
    | Ppat_variant (label, variantArgs) ->
      let variantName =
        Doc.concat [Doc.text "#"; printIdentLike ~allowUident:true label] in
      let argsDoc = match variantArgs with
      | None -> Doc.nil
      | Some({ppat_desc = Ppat_construct ({txt = Longident.Lident "()"}, _)}) ->
        Doc.text "()"
      | Some({ppat_desc = Ppat_tuple []; ppat_loc = loc}) ->
        Doc.concat [
          Doc.lparen;
          Doc.softLine;
          printCommentsInside cmtTbl loc;
          Doc.rparen;
        ]
      (* Some((1, 2) *)
      | Some({ppat_desc = Ppat_tuple [{ppat_desc = Ppat_tuple _} as arg]}) ->
        Doc.concat [
          Doc.lparen;
          printPattern arg cmtTbl;
          Doc.rparen;
        ]
      | Some({ppat_desc = Ppat_tuple patterns}) ->
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun pat -> printPattern pat cmtTbl) patterns
              );
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ]
      | Some(arg) ->
        let argDoc = printPattern arg cmtTbl in
        let shouldHug = ParsetreeViewer.isHuggablePattern arg in
        Doc.concat [
          Doc.lparen;
          if shouldHug then argDoc
          else Doc.concat [
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                argDoc;
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
          ];
          Doc.rparen;

        ]
      in
      Doc.group(Doc.concat [variantName; argsDoc])
    | Ppat_type ident ->
      Doc.concat [Doc.text "##"; printIdentPath ident cmtTbl]
    | Ppat_record(rows, openFlag) ->
        Doc.group(
          Doc.concat([
            Doc.lbrace;
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map (fun row -> printPatternRecordRow row cmtTbl) rows);
                begin match openFlag with
                | Open -> Doc.concat [Doc.text ","; Doc.line; Doc.text "_"]
                | Closed -> Doc.nil
                end;
              ]
            );
            Doc.ifBreaks (Doc.text ",") Doc.nil;
            Doc.softLine;
            Doc.rbrace;
          ])
        )

    | Ppat_exception p ->
        let needsParens = match p.ppat_desc with
        | Ppat_or (_, _) | Ppat_alias (_, _) -> true
        | _ -> false
        in
        let pat =
          let p = printPattern p cmtTbl in
          if needsParens then
            Doc.concat [Doc.text "("; p; Doc.text ")"]
          else
            p
        in
        Doc.group (
          Doc.concat [Doc.text "exception"; Doc.line; pat]
        )
    | Ppat_or _ ->
      (* Blue | Red | Green -> [Blue; Red; Green] *)
      let orChain = ParsetreeViewer.collectOrPatternChain p in
      let docs = List.mapi (fun i pat ->
        let patternDoc = printPattern pat cmtTbl in
        Doc.concat [
          if i == 0 then Doc.nil else Doc.concat [Doc.line; Doc.text "| "];
          match pat.ppat_desc with
          (* (Blue | Red) | (Green | Black) | White *)
          | Ppat_or _ -> addParens patternDoc
          | _ -> patternDoc
        ]
      ) orChain in
      Doc.group (Doc.concat docs)
    | Ppat_extension ext ->
      printExtensionWithComments ~atModuleLvl:false ext cmtTbl
    | Ppat_lazy p ->
      let needsParens = match p.ppat_desc with
      | Ppat_or (_, _) | Ppat_alias (_, _) -> true
      | _ -> false
      in
      let pat =
        let p = printPattern p cmtTbl in
        if needsParens then
          Doc.concat [Doc.text "("; p; Doc.text ")"]
        else
          p
      in
      Doc.concat [Doc.text "lazy "; pat]
    | Ppat_alias (p, aliasLoc) ->
      let needsParens = match p.ppat_desc with
      | Ppat_or (_, _) | Ppat_alias (_, _) -> true
      | _ -> false
      in
      let renderedPattern =
        let p = printPattern p cmtTbl in
        if needsParens then
          Doc.concat [Doc.text "("; p; Doc.text ")"]
        else
          p
      in
      Doc.concat([
        renderedPattern;
        Doc.text " as ";
        printStringLoc aliasLoc cmtTbl;
      ])

     (* Note: module(P : S) is represented as *)
     (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_constraint ({ppat_desc = Ppat_unpack stringLoc}, {ptyp_desc = Ptyp_package packageType; ptyp_loc}) ->
        Doc.concat [
          Doc.text "module(";
          printComments (Doc.text stringLoc.txt) cmtTbl stringLoc.loc;
          Doc.text ": ";
          printComments
            (printPackageType ~printModuleKeywordAndParens:false packageType cmtTbl)
            cmtTbl
            ptyp_loc;
          Doc.rparen;
        ]
    | Ppat_constraint (pattern, typ) ->
      Doc.concat [
        printPattern pattern cmtTbl;
        Doc.text ": ";
        printTypExpr typ cmtTbl;
      ]

     (* Note: module(P : S) is represented as *)
     (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_unpack stringLoc ->
      Doc.concat [
        Doc.text "module(";
        printComments (Doc.text stringLoc.txt) cmtTbl stringLoc.loc;
        Doc.rparen;
      ]
    | Ppat_interval (a, b) ->
      Doc.concat [
        printConstant a;
        Doc.text " .. ";
        printConstant b;
      ]
    | Ppat_open _ -> Doc.nil
    in
    let doc = match p.ppat_attributes with
    | [] -> patternWithoutAttributes
    | attrs ->
      Doc.group (
        Doc.concat [
          printAttributes attrs;
          patternWithoutAttributes;
        ]
      )
    in
    printComments doc cmtTbl p.ppat_loc

  and printPatternRecordRow row cmtTbl =
    match row with
    (* punned {x}*)
    | ({Location.txt=Longident.Lident ident} as longident,
       {Parsetree.ppat_desc=Ppat_var {txt;_}}) when ident = txt ->
        printLidentPath longident cmtTbl
    | (longident, pattern) ->
      let locForComments = {
        longident.loc with
        loc_end = pattern.Parsetree.ppat_loc.loc_end
      } in
      let doc = Doc.group (
        Doc.concat([
          printLidentPath longident cmtTbl;
          Doc.text ": ";
          Doc.indent(
            Doc.concat [
              Doc.softLine;
              printPattern pattern cmtTbl;
            ]
          )
        ])
      ) in
      printComments doc cmtTbl locForComments

  and printExpressionWithComments expr cmtTbl =
    let doc = printExpression expr cmtTbl in
    printComments doc cmtTbl expr.Parsetree.pexp_loc

  and printExpression (e : Parsetree.expression) cmtTbl =
    let printedExpression = match e.pexp_desc with
    | Parsetree.Pexp_constant c -> printConstant c
    | Pexp_construct _ when ParsetreeViewer.hasJsxAttribute e.pexp_attributes ->
      printJsxFragment e cmtTbl
    | Pexp_construct ({txt = Longident.Lident "()"}, _) -> Doc.text "()"
    | Pexp_construct ({txt = Longident.Lident "[]"}, _) ->
      Doc.concat [
        Doc.text "list[";
        printCommentsInside cmtTbl e.pexp_loc;
        Doc.rbracket;
      ]
    | Pexp_construct ({txt = Longident.Lident "::"}, _) ->
      let (expressions, spread) = ParsetreeViewer.collectListExpressions e in
      let spreadDoc = match spread with
      | Some(expr) -> Doc.concat [
          Doc.text ",";
          Doc.line;
          Doc.dotdotdot;
          let doc = printExpressionWithComments expr cmtTbl in
          match Parens.expr expr with
          | Parens.Parenthesized -> addParens doc
          | Braced braces -> printBraces doc expr braces
          | Nothing -> doc
        ]
      | None -> Doc.nil
      in
      Doc.group(
        Doc.concat([
          Doc.text "list[";
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map
                  (fun expr ->
                    let doc = printExpressionWithComments expr cmtTbl in
                    match Parens.expr expr with
                    | Parens.Parenthesized -> addParens doc
                    | Braced braces -> printBraces doc expr braces
                    | Nothing -> doc
                  )
                  expressions);
              spreadDoc;
            ])
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rbracket;
        ])
      )
    | Pexp_construct (longidentLoc, args) ->
      let constr = printLongidentLocation longidentLoc cmtTbl in
      let args = match args with
      | None -> Doc.nil
      | Some({pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}) ->
        Doc.text "()"
      (* Some((1, 2)) *)
      | Some({pexp_desc = Pexp_tuple [{pexp_desc = Pexp_tuple _} as arg]}) ->
        Doc.concat [
          Doc.lparen;
          (let doc = printExpressionWithComments arg cmtTbl in
          match Parens.expr arg with
          | Parens.Parenthesized -> addParens doc
          | Braced braces -> printBraces doc arg braces
          | Nothing -> doc);
          Doc.rparen;
        ]
      | Some({pexp_desc = Pexp_tuple args }) ->
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map
                  (fun expr ->
                    let doc = printExpressionWithComments expr cmtTbl in
                    match Parens.expr expr with
                    | Parens.Parenthesized -> addParens doc
                    | Braced braces -> printBraces doc expr braces
                    | Nothing -> doc)
                  args
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ]
      | Some(arg) ->
        let argDoc =
          let doc = printExpressionWithComments arg cmtTbl in
          match Parens.expr arg with
          | Parens.Parenthesized -> addParens doc
          | Braced braces -> printBraces doc arg braces
          | Nothing -> doc
        in
        let shouldHug = ParsetreeViewer.isHuggableExpression arg in
        Doc.concat [
          Doc.lparen;
          if shouldHug then argDoc
          else Doc.concat [
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                argDoc;
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
          ];
          Doc.rparen;
        ]
      in
      Doc.group(Doc.concat [constr; args])
    | Pexp_ident path ->
      printLidentPath path cmtTbl
    | Pexp_tuple exprs ->
      Doc.group(
        Doc.concat([
          Doc.lparen;
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map (fun expr ->
                  let doc = printExpressionWithComments expr cmtTbl in
                  match Parens.expr expr with
                  | Parens.Parenthesized -> addParens doc
                  | Braced braces -> printBraces doc expr braces
                  | Nothing -> doc)
                 exprs)
            ])
          );
          Doc.ifBreaks (Doc.text ",") Doc.nil;
          Doc.softLine;
          Doc.rparen;
        ])
      )
    | Pexp_array [] ->
      Doc.concat [
        Doc.lbracket;
        printCommentsInside cmtTbl e.pexp_loc;
        Doc.rbracket;
      ]
    | Pexp_array exprs ->
      Doc.group(
        Doc.concat([
          Doc.lbracket;
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map (fun expr ->
                  let doc = printExpressionWithComments expr cmtTbl in
                  match Parens.expr expr with
                  | Parens.Parenthesized -> addParens doc
                  | Braced braces -> printBraces doc expr braces
                  | Nothing -> doc
                  ) exprs)
            ])
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rbracket;
        ])
      )
    | Pexp_variant (label, args) ->
      let variantName =
        Doc.concat [Doc.text "#"; printIdentLike ~allowUident:true label] in
      let args = match args with
      | None -> Doc.nil
      | Some({pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}) ->
        Doc.text "()"
      (* #poly((1, 2) *)
      | Some({pexp_desc = Pexp_tuple [{pexp_desc = Pexp_tuple _} as arg]}) ->
        Doc.concat [
          Doc.lparen;
          (let doc = printExpressionWithComments arg cmtTbl in
          match Parens.expr arg with
          | Parens.Parenthesized -> addParens doc
          | Braced braces -> printBraces doc arg braces
          | Nothing -> doc);
          Doc.rparen;
        ]
      | Some({pexp_desc = Pexp_tuple args }) ->
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map
                  (fun expr ->
                    let doc = printExpressionWithComments expr cmtTbl in
                    match Parens.expr expr with
                    | Parens.Parenthesized -> addParens doc
                    | Braced braces -> printBraces doc expr braces
                    | Nothing -> doc)
                  args
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ]
      | Some(arg) ->
        let argDoc =
          let doc = printExpressionWithComments arg cmtTbl in
          match Parens.expr arg with
          | Parens.Parenthesized -> addParens doc
          | Braced braces -> printBraces doc arg braces
          | Nothing -> doc
        in
        let shouldHug = ParsetreeViewer.isHuggableExpression arg in
        Doc.concat [
          Doc.lparen;
          if shouldHug then argDoc
          else Doc.concat [
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                argDoc;
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
          ];
          Doc.rparen;
        ]
      in
      Doc.group(Doc.concat [variantName; args])
    | Pexp_record (rows, spreadExpr) ->
      let spread = match spreadExpr with
      | None -> Doc.nil
      | Some expr -> Doc.concat [
          Doc.dotdotdot;
          (let doc = printExpressionWithComments expr cmtTbl in
          match Parens.expr expr with
          | Parens.Parenthesized -> addParens doc
          | Braced braces -> printBraces doc expr braces
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
      let forceBreak =
        e.pexp_loc.loc_start.pos_lnum < e.pexp_loc.loc_end.pos_lnum
      in
      Doc.breakableGroup ~forceBreak (
        Doc.concat([
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              spread;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map (fun row -> printRecordRow row cmtTbl) rows)
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rbrace;
        ])
      )
    | Pexp_extension extension ->
      begin match extension with
      | (
          {txt = "bs.obj"},
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
        let forceBreak =
          loc.loc_start.pos_lnum < loc.loc_end.pos_lnum
        in
        Doc.breakableGroup ~forceBreak (
          Doc.concat([
            Doc.lbrace;
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map (fun row -> printBsObjectRow row cmtTbl) rows)
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
            Doc.rbrace;
          ])
        )
      | extension ->
        printExtensionWithComments ~atModuleLvl:false extension cmtTbl
      end
    | Pexp_apply _ ->
      if ParsetreeViewer.isUnaryExpression e then
        printUnaryExpression e cmtTbl
      else if ParsetreeViewer.isTemplateLiteral e then
        printTemplateLiteral e cmtTbl
      else if ParsetreeViewer.isBinaryExpression e then
        printBinaryExpression e cmtTbl
      else
        printPexpApply e cmtTbl
    | Pexp_unreachable -> Doc.dot
    | Pexp_field (expr, longidentLoc) ->
      let lhs =
        let doc = printExpressionWithComments expr cmtTbl in
        match Parens.fieldExpr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        lhs;
        Doc.dot;
        printLidentPath longidentLoc cmtTbl;
      ]
    | Pexp_setfield (expr1, longidentLoc, expr2) ->
      printSetFieldExpr e.pexp_attributes expr1 longidentLoc expr2 e.pexp_loc cmtTbl
    | Pexp_ifthenelse (_ifExpr, _thenExpr, _elseExpr) ->
      if ParsetreeViewer.isTernaryExpr e then
        let (parts, alternate) = ParsetreeViewer.collectTernaryParts e in
        let ternaryDoc = match parts with
        | (condition1, consequent1)::rest ->
          Doc.group (Doc.concat [
            printTernaryOperand condition1 cmtTbl;
            Doc.indent (
              Doc.concat [
                Doc.line;
                Doc.indent (
                  Doc.concat [
                    Doc.text "? ";
                    printTernaryOperand consequent1 cmtTbl
                  ]
                );
                Doc.concat (
                  List.map (fun (condition, consequent) ->
                    Doc.concat [
                      Doc.line;
                      Doc.text ": ";
                      printTernaryOperand condition cmtTbl;
                      Doc.line;
                      Doc.text "? ";
                      printTernaryOperand consequent cmtTbl;
                    ]
                  ) rest
                );
                Doc.line;
                Doc.text ": ";
                Doc.indent (printTernaryOperand alternate cmtTbl);
              ]
            )
          ])
        | _ -> Doc.nil
        in
        let attrs = ParsetreeViewer.filterTernaryAttributes e.pexp_attributes in
        let needsParens = match ParsetreeViewer.filterParsingAttrs attrs with
        | [] -> false | _ -> true
        in
        Doc.concat [
          printAttributes attrs;
          if needsParens then addParens ternaryDoc else ternaryDoc;
        ]
      else
      let (ifs, elseExpr) = ParsetreeViewer.collectIfExpressions e in
      let ifDocs = Doc.join ~sep:Doc.space (
        List.mapi (fun i (ifExpr, thenExpr) ->
          let ifTxt = if i > 0 then Doc.text "else if " else  Doc.text "if " in
          let condition =
            if ParsetreeViewer.isBlockExpr ifExpr then
              printExpressionBlock ~braces:true ifExpr cmtTbl
            else
              let doc = printExpressionWithComments ifExpr cmtTbl in
              match Parens.expr ifExpr with
              | Parens.Parenthesized -> addParens doc
              | Braced braces -> printBraces doc ifExpr braces
              | Nothing -> Doc.ifBreaks (addParens doc) doc
          in
          Doc.concat [
            ifTxt;
            Doc.group (condition);
            Doc.space;
            let thenExpr = match ParsetreeViewer.processBracesAttr thenExpr with
            (* This case only happens when coming from Reason, we strip braces *)
            | (Some _, expr) -> expr
            | _ -> thenExpr
            in
            printExpressionBlock ~braces:true thenExpr cmtTbl;
          ]
        ) ifs
      ) in
      let elseDoc = match elseExpr with
      | None -> Doc.nil
      | Some expr -> Doc.concat [
          Doc.text " else ";
          printExpressionBlock ~braces:true expr cmtTbl;
        ]
      in
      Doc.concat [
        printAttributes e.pexp_attributes;
        ifDocs;
        elseDoc;
      ]
    | Pexp_while (expr1, expr2) ->
      let condition =
        let doc = printExpressionWithComments expr1 cmtTbl in
        match Parens.expr expr1 with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc expr1 braces
        | Nothing -> doc
      in
      Doc.breakableGroup ~forceBreak:true (
        Doc.concat [
          Doc.text "while ";
          if ParsetreeViewer.isBlockExpr expr1 then
            condition
          else
            Doc.group (
              Doc.ifBreaks (addParens condition) condition
            );
          Doc.space;
          printExpressionBlock ~braces:true expr2 cmtTbl;
        ]
      )
    | Pexp_for (pattern, fromExpr, toExpr, directionFlag, body) ->
      Doc.breakableGroup ~forceBreak:true (
        Doc.concat [
          Doc.text "for ";
          printPattern pattern cmtTbl;
          Doc.text " in ";
          (let doc = printExpressionWithComments fromExpr cmtTbl in
          match Parens.expr fromExpr with
          | Parens.Parenthesized -> addParens doc
          | Braced braces -> printBraces doc fromExpr braces
          | Nothing -> doc);
          printDirectionFlag directionFlag;
          (let doc = printExpressionWithComments toExpr cmtTbl in
          match Parens.expr toExpr with
          | Parens.Parenthesized -> addParens doc
          | Braced braces -> printBraces doc toExpr braces
          | Nothing -> doc);
          Doc.space;
          printExpressionBlock ~braces:true body cmtTbl;
        ]
      )
    | Pexp_constraint(
        {pexp_desc = Pexp_pack modExpr},
        {ptyp_desc = Ptyp_package packageType; ptyp_loc}
      ) ->
      Doc.group (
        Doc.concat [
          Doc.text "module(";
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              printModExpr modExpr cmtTbl;
              Doc.text ": ";
              printComments
                (printPackageType ~printModuleKeywordAndParens:false packageType cmtTbl)
                cmtTbl
                ptyp_loc
            ]
          );
          Doc.softLine;
          Doc.rparen;
        ]
      )

    | Pexp_constraint (expr, typ) ->
      let exprDoc =
        let doc = printExpressionWithComments expr cmtTbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        exprDoc;
        Doc.text ": ";
        printTypExpr typ cmtTbl;
      ]
    | Pexp_letmodule ({txt = _modName}, _modExpr, _expr) ->
      printExpressionBlock ~braces:true e cmtTbl
    | Pexp_letexception (_extensionConstructor, _expr) ->
      printExpressionBlock ~braces:true e cmtTbl
    | Pexp_assert expr ->
      let rhs =
        let doc = printExpressionWithComments expr cmtTbl in
        match Parens.lazyOrAssertExprRhs expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        Doc.text "assert ";
        rhs;
      ]
    | Pexp_lazy expr ->
      let rhs =
        let doc = printExpressionWithComments expr cmtTbl in
        match Parens.lazyOrAssertExprRhs expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.group (
        Doc.concat [
          Doc.text "lazy ";
          rhs;
        ]
      )
    | Pexp_open (_overrideFlag, _longidentLoc, _expr) ->
      printExpressionBlock ~braces:true e cmtTbl
    | Pexp_pack (modExpr) ->
      Doc.group (Doc.concat [
        Doc.text "module(";
        Doc.indent (
          Doc.concat [
            Doc.softLine;
            printModExpr modExpr cmtTbl;
          ]
        );
        Doc.softLine;
        Doc.rparen;
      ])
    | Pexp_sequence _ ->
      printExpressionBlock ~braces:true e cmtTbl
    | Pexp_let _ ->
      printExpressionBlock ~braces:true e cmtTbl
    | Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt="__x"}}, ({pexp_desc = Pexp_apply _})) ->

      (* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
      printExpressionWithComments (ParsetreeViewer.rewriteUnderscoreApply e) cmtTbl
    | Pexp_fun _ | Pexp_newtype _ ->
      let (attrsOnArrow, parameters, returnExpr) = ParsetreeViewer.funExpr e in
      let (uncurried, attrs) =
        ParsetreeViewer.processUncurriedAttribute attrsOnArrow
      in
      let (returnExpr, typConstraint) = match returnExpr.pexp_desc with
      | Pexp_constraint (expr, typ) -> (
          {expr with pexp_attributes = List.concat [
            expr.pexp_attributes;
            returnExpr.pexp_attributes;
          ]},
          Some typ
        )
      | _ -> (returnExpr, None)
      in
      let hasConstraint = match typConstraint with | Some _ -> true | None -> false in
      let parametersDoc = printExprFunParameters
        ~inCallback:false
        ~uncurried
        ~hasConstraint
        parameters
        cmtTbl
      in
      let returnExprDoc =
        let (optBraces, _) = ParsetreeViewer.processBracesAttr returnExpr in
        let shouldInline = match (returnExpr.pexp_desc, optBraces) with
        | (_, Some _ ) -> true
        | ((Pexp_array _
        | Pexp_tuple _
        | Pexp_construct (_, Some _)
        | Pexp_record _), _) -> true
        | _ -> false
        in
        let shouldIndent = match returnExpr.pexp_desc with
        | Pexp_sequence _
        | Pexp_let _
        | Pexp_letmodule _
        | Pexp_letexception _
        | Pexp_open _ -> false
        | _ -> true
        in
        let returnDoc =
          let doc = printExpressionWithComments returnExpr cmtTbl in
          match Parens.expr returnExpr with
          | Parens.Parenthesized -> addParens doc
          | Braced braces  -> printBraces doc returnExpr braces
          | Nothing -> doc
        in
        if shouldInline then Doc.concat [
          Doc.space;
          returnDoc;
        ] else
          Doc.group (
            if shouldIndent then
              Doc.indent (
                Doc.concat [
                  Doc.line;
                  returnDoc;
                ]
              )
            else
              Doc.concat [
                Doc.space;
                returnDoc
              ]
          )
      in
      let typConstraintDoc = match typConstraint with
      | Some(typ) -> Doc.concat [Doc.text ": "; printTypExpr typ cmtTbl]
      | _ -> Doc.nil
      in
      let attrs = printAttributes attrs in
      Doc.group (
        Doc.concat [
          attrs;
          parametersDoc;
          typConstraintDoc;
          Doc.text " =>";
          returnExprDoc;
        ]
      )
    | Pexp_try (expr, cases) ->
      let exprDoc =
        let doc = printExpressionWithComments expr cmtTbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        Doc.text "try ";
        exprDoc;
        Doc.text " catch ";
        printCases cases cmtTbl;
      ]
    | Pexp_match (expr, cases) ->
      let exprDoc =
        let doc = printExpressionWithComments expr cmtTbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [
        Doc.text "switch ";
        exprDoc;
        Doc.space;
        printCases cases cmtTbl;
      ]
    | Pexp_function cases ->
      Doc.concat [
        Doc.text "x => switch x ";
        printCases cases cmtTbl;
      ]
    | Pexp_coerce (expr, typOpt, typ) ->
      let docExpr = printExpressionWithComments expr cmtTbl in
      let docTyp = printTypExpr typ cmtTbl in
      let ofType = match typOpt with
      | None -> Doc.nil
      | Some(typ1) ->
        Doc.concat [Doc.text ": "; printTypExpr typ1 cmtTbl]
      in
      Doc.concat [Doc.lparen; docExpr; ofType; Doc.text " :> "; docTyp; Doc.rparen]
    | Pexp_send _ ->
      Doc.text "Pexp_send not impemented in printer"
    | Pexp_new _ ->
      Doc.text "Pexp_new not impemented in printer"
    | Pexp_setinstvar _ ->
      Doc.text "Pexp_setinstvar not impemented in printer"
    | Pexp_override _ ->
      Doc.text "Pexp_override not impemented in printer"
    | Pexp_poly _ ->
      Doc.text "Pexp_poly not impemented in printer"
    | Pexp_object _ ->
      Doc.text "Pexp_object not impemented in printer"
    in
    let shouldPrintItsOwnAttributes = match e.pexp_desc with
    | Pexp_apply _
    | Pexp_fun _
    | Pexp_newtype _
    | Pexp_setfield _
    | Pexp_ifthenelse _ -> true
    | Pexp_construct _ when ParsetreeViewer.hasJsxAttribute e.pexp_attributes -> true
    | _ -> false
    in
    match e.pexp_attributes with
    | [] -> printedExpression
    | attrs when not shouldPrintItsOwnAttributes ->
      Doc.group (
        Doc.concat [
          printAttributes attrs;
          printedExpression;
        ]
      )
    | _ -> printedExpression

  and printPexpFun ~inCallback e cmtTbl =
      let (attrsOnArrow, parameters, returnExpr) = ParsetreeViewer.funExpr e in
      let (uncurried, attrs) =
        ParsetreeViewer.processUncurriedAttribute attrsOnArrow
      in
      let (returnExpr, typConstraint) = match returnExpr.pexp_desc with
      | Pexp_constraint (expr, typ) -> (
          {expr with pexp_attributes = List.concat [
            expr.pexp_attributes;
            returnExpr.pexp_attributes;
          ]},
          Some typ
        )
      | _ -> (returnExpr, None)
      in
      let parametersDoc = printExprFunParameters
        ~inCallback
        ~uncurried
        ~hasConstraint:(match typConstraint with | Some _ -> true | None -> false)
        parameters cmtTbl in
      let returnShouldIndent = match returnExpr.pexp_desc with
      | Pexp_sequence _ | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _ | Pexp_open _ -> false
      | _ -> true
      in
      let returnExprDoc =
        let (optBraces, _) = ParsetreeViewer.processBracesAttr returnExpr in
        let shouldInline = match (returnExpr.pexp_desc, optBraces) with
        | (_, Some _) -> true
        | ((Pexp_array _
        | Pexp_tuple _
        | Pexp_construct (_, Some _)
        | Pexp_record _), _) -> true
        | _ -> false
        in
        let returnDoc =
          let doc = printExpressionWithComments returnExpr cmtTbl in
          match Parens.expr returnExpr with
          | Parens.Parenthesized -> addParens doc
          | Braced braces  -> printBraces doc returnExpr braces
          | Nothing -> doc
        in
        if shouldInline then Doc.concat [
          Doc.space;
          returnDoc;
        ] else
          Doc.group (
            if returnShouldIndent then
              Doc.concat [
                Doc.indent (
                  Doc.concat [
                    Doc.line;
                    returnDoc;
                  ]
                );
                if inCallback then Doc.softLine else Doc.nil;
              ]
            else
              Doc.concat [
                Doc.space;
                returnDoc;
              ]
          )
      in
      let typConstraintDoc = match typConstraint with
      | Some(typ) -> Doc.concat [
          Doc.text ": ";
          printTypExpr typ cmtTbl
        ]
      | _ -> Doc.nil
      in
      Doc.group (
        Doc.concat [
          printAttributes attrs;
          parametersDoc;
          typConstraintDoc;
          Doc.text " =>";
          returnExprDoc;
        ]
      )

  and printTernaryOperand expr cmtTbl =
    let doc = printExpressionWithComments expr cmtTbl in
    match Parens.ternaryOperand expr with
    | Parens.Parenthesized -> addParens doc
    | Braced braces  -> printBraces doc expr braces
    | Nothing -> doc

  and printSetFieldExpr attrs lhs longidentLoc rhs loc cmtTbl =
    let rhsDoc =
      let doc = printExpressionWithComments rhs cmtTbl in
      match Parens.setFieldExprRhs rhs with
      | Parens.Parenthesized -> addParens doc
      | Braced braces  -> printBraces doc rhs braces
      | Nothing -> doc
    in
    let lhsDoc =
      let doc = printExpressionWithComments lhs cmtTbl in
      match Parens.fieldExpr lhs with
      | Parens.Parenthesized -> addParens doc
      | Braced braces  -> printBraces doc lhs braces
      | Nothing -> doc
    in
    let shouldIndent = ParsetreeViewer.isBinaryExpression rhs in
    let doc = Doc.group (Doc.concat [
      lhsDoc;
      Doc.dot;
      printLidentPath longidentLoc cmtTbl;
      Doc.text " =";
      if shouldIndent then Doc.group (
        Doc.indent (
          (Doc.concat [Doc.line; rhsDoc])
        )
      ) else
        Doc.concat [Doc.space; rhsDoc]
    ]) in
    let doc = match attrs with
    | [] -> doc
    | attrs ->
      Doc.group (
        Doc.concat [
          printAttributes attrs;
          doc
        ]
      )
    in
    printComments doc cmtTbl loc

  and printTemplateLiteral expr cmtTbl =
    let tag = ref "j" in
    let rec walkExpr expr =
      let open Parsetree in
      match expr.pexp_desc with
      | Pexp_apply (
          {pexp_desc = Pexp_ident {txt = Longident.Lident "^"}},
          [Nolabel, arg1; Nolabel, arg2]
        ) ->
          let lhs = walkExpr arg1 in
          let rhs = walkExpr arg2 in
          Doc.concat [lhs; rhs]
      | Pexp_constant (Pconst_string (txt, Some prefix)) ->
        tag := prefix;
        Doc.text txt
      | _ ->
        let doc = printExpressionWithComments expr cmtTbl in
        Doc.concat [Doc.text "${"; doc; Doc.rbrace]
    in
    let content = walkExpr expr in
    Doc.concat [
      if !tag = "j" then Doc.nil else Doc.text !tag;
      Doc.text "`";
      content;
      Doc.text "`"
    ]

  and printUnaryExpression expr cmtTbl =
    let printUnaryOperator op = Doc.text (
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
      let printedOperand =
        let doc = printExpressionWithComments operand cmtTbl in
        match Parens.unaryExprOperand operand with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc operand braces
        | Nothing -> doc
      in
      let doc = Doc.concat [
        printUnaryOperator operator;
        printedOperand;
      ] in
      printComments doc cmtTbl expr.pexp_loc
    | _ -> assert false

  and printBinaryExpression (expr : Parsetree.expression) cmtTbl =
    let printBinaryOperator ~inlineRhs operator =
      let operatorTxt = match operator with
      | "|." -> "->"
      | "^" -> "++"
      | "=" -> "=="
      | "==" -> "==="
      | "<>" -> "!="
      | "!=" -> "!=="
      | txt -> txt
      in
      let spacingBeforeOperator =
        if operator = "|." then Doc.softLine
        else if operator = "|>" then Doc.line
        else Doc.space;
      in
      let spacingAfterOperator =
        if operator = "|." then Doc.nil
        else if operator = "|>" then Doc.space
        else if inlineRhs then Doc.space else Doc.line
      in
      Doc.concat [
        spacingBeforeOperator;
        Doc.text operatorTxt;
        spacingAfterOperator;
      ]
    in
    let printOperand ~isLhs expr parentOperator =
      let rec flatten ~isLhs expr parentOperator =
        if ParsetreeViewer.isBinaryExpression expr then
          begin match expr with
          | {pexp_desc = Pexp_apply (
              {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
              [_, left; _, right]
            )} ->
            if ParsetreeViewer.flattenableOperators parentOperator operator &&
               not (ParsetreeViewer.hasAttributes expr.pexp_attributes)
            then
              let leftPrinted = flatten ~isLhs:true left operator in
              let rightPrinted =
                let (_, rightAttrs) =
                  ParsetreeViewer.partitionPrinteableAttributes right.pexp_attributes
                in
                let doc =
                  printExpressionWithComments
                    {right with pexp_attributes = rightAttrs}
                    cmtTbl
                in
                let doc = if Parens.flattenOperandRhs parentOperator right then
                  Doc.concat [Doc.lparen; doc; Doc.rparen]
                else
                  doc
                in
                let printeableAttrs =
                  ParsetreeViewer.filterPrinteableAttributes right.pexp_attributes
                in
                Doc.concat [printAttributes printeableAttrs; doc]
              in
              let doc = Doc.concat [
                leftPrinted;
                printBinaryOperator ~inlineRhs:false operator;
                rightPrinted;
              ] in
              let doc =
                if not isLhs && (Parens.rhsBinaryExprOperand operator expr) then
                  Doc.concat [Doc.lparen; doc; Doc.rparen]
                else doc
              in
              printComments doc cmtTbl expr.pexp_loc
            else (
              let doc = printExpressionWithComments {expr with pexp_attributes = []} cmtTbl in
              let doc = if Parens.subBinaryExprOperand parentOperator operator ||
                (expr.pexp_attributes <> [] &&
                  (ParsetreeViewer.isBinaryExpression expr ||
                ParsetreeViewer.isTernaryExpr expr))
              then
                Doc.concat [Doc.lparen; doc; Doc.rparen]
              else doc
              in Doc.concat [
                printAttributes expr.pexp_attributes;
                doc
              ]
            )
          | _ -> assert false
          end
        else
          begin match expr.pexp_desc with
          | Pexp_setfield (lhs, field, rhs) ->
            let doc = printSetFieldExpr expr.pexp_attributes lhs field rhs expr.pexp_loc cmtTbl  in
            if isLhs then addParens doc else doc
          | Pexp_apply(
              {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}},
              [(Nolabel, lhs); (Nolabel, rhs)]
            ) ->
            let rhsDoc = printExpressionWithComments rhs cmtTbl in
            let lhsDoc = printExpressionWithComments lhs cmtTbl in
            (* TODO: unify indentation of "=" *)
            let shouldIndent = ParsetreeViewer.isBinaryExpression rhs in
            let doc = Doc.group (
              Doc.concat [
                lhsDoc;
                Doc.text " =";
                if shouldIndent then Doc.group (
                  Doc.indent (Doc.concat [Doc.line; rhsDoc])
                ) else
                  Doc.concat [Doc.space; rhsDoc]
              ]
            ) in
            let doc = match expr.pexp_attributes with
            | [] -> doc
            | attrs ->
              Doc.group (
                Doc.concat [
                  printAttributes attrs;
                  doc
                ]
              )
            in
            if isLhs then addParens doc else doc
          | _ ->
            let doc = printExpressionWithComments expr cmtTbl in
            begin match Parens.binaryExprOperand ~isLhs expr with
            | Parens.Parenthesized -> addParens doc
            | Braced braces  -> printBraces doc expr braces
            | Nothing -> doc
            end
          end
      in
      flatten ~isLhs expr parentOperator
    in
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident (("|." | "|>") as op)}},
        [Nolabel, lhs; Nolabel, rhs]
      ) when not (
          ParsetreeViewer.isBinaryExpression lhs ||
          ParsetreeViewer.isBinaryExpression rhs
      ) ->
      let lhsDoc = printOperand ~isLhs:true lhs op in
      let rhsDoc = printOperand ~isLhs:false rhs op in
      Doc.group (
        Doc.concat [
          lhsDoc;
          (match op with
          | "|." -> Doc.text "->"
          | "|>" -> Doc.text " |> "
          | _ -> assert false);
          rhsDoc;
        ]
      )
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, lhs; Nolabel, rhs]
      ) ->
      let right =
        let operatorWithRhs =
          let rhsDoc = printOperand ~isLhs:false rhs operator in
          Doc.concat [
            printBinaryOperator
              ~inlineRhs:(ParsetreeViewer.shouldInlineRhsBinaryExpr rhs) operator;
            rhsDoc;
        ] in
        if ParsetreeViewer.shouldIndentBinaryExpr expr then
          Doc.group (Doc.indent operatorWithRhs)
        else operatorWithRhs
      in
      let doc = Doc.group (
        Doc.concat [
          printOperand ~isLhs:true lhs operator;
          right
        ]
      ) in
      Doc.group (
        Doc.concat [
          printAttributes expr.pexp_attributes;
          match Parens.binaryExpr {expr with
            pexp_attributes = List.filter (fun attr ->
              match attr with
              | ({Location.txt = ("ns.braces")}, _) -> false
              | _ -> true
            ) expr.pexp_attributes
          } with
          | Braced(bracesLoc) -> printBraces doc expr bracesLoc
          | Parenthesized -> addParens doc
          | Nothing -> doc;
        ]
      )
    | _ -> Doc.nil

  (* callExpr(arg1, arg2) *)
  and printPexpApply expr cmtTbl =
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident "##"}},
        [Nolabel, parentExpr; Nolabel, memberExpr]
      ) ->
        let parentDoc =
          let doc = printExpressionWithComments parentExpr cmtTbl in
          match Parens.unaryExprOperand parentExpr with
          | Parens.Parenthesized -> addParens doc
          | Braced braces  -> printBraces doc parentExpr braces
          | Nothing -> doc
        in
        let member =
          let memberDoc = match memberExpr.pexp_desc with
          | Pexp_ident lident ->
            printComments (printLongident lident.txt) cmtTbl memberExpr.pexp_loc
          | _ -> printExpressionWithComments memberExpr cmtTbl
          in
          Doc.concat [Doc.text "\""; memberDoc; Doc.text "\""]
        in
        Doc.group (Doc.concat [
          printAttributes expr.pexp_attributes;
          parentDoc;
          Doc.lbracket;
          member;
          Doc.rbracket;
        ])
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}},
        [Nolabel, lhs; Nolabel, rhs]
      ) ->
        let rhsDoc =
          let doc = printExpressionWithComments rhs cmtTbl in
          match Parens.expr rhs with
          | Parens.Parenthesized -> addParens doc
          | Braced braces  -> printBraces doc rhs braces
          | Nothing -> doc
        in
        (* TODO: unify indentation of "=" *)
        let shouldIndent = not (ParsetreeViewer.isBracedExpr rhs) && ParsetreeViewer.isBinaryExpression rhs in
        let doc = Doc.group(
          Doc.concat [
            printExpressionWithComments lhs cmtTbl;
            Doc.text " =";
            if shouldIndent then Doc.group (
              Doc.indent (
                (Doc.concat [Doc.line; rhsDoc])
              )
            ) else
              Doc.concat [Doc.space; rhsDoc]
          ]
        ) in
        begin match expr.pexp_attributes with
        | [] -> doc
        | attrs ->
          Doc.group (
            Doc.concat [
              printAttributes attrs;
              doc
            ]
          )
        end
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "get")}},
        [Nolabel, parentExpr; Nolabel, memberExpr]
      ) ->
        let member =
          let memberDoc =
            let doc = printExpressionWithComments memberExpr cmtTbl in
            match Parens.expr memberExpr with
            | Parens.Parenthesized -> addParens doc
            | Braced braces  -> printBraces doc memberExpr braces
            | Nothing -> doc
          in
          let shouldInline = match memberExpr.pexp_desc with
          | Pexp_constant _ | Pexp_ident _ -> true
          | _ -> false
          in
          if shouldInline then memberDoc else (
            Doc.concat [
              Doc.indent (
                Doc.concat [
                  Doc.softLine;
                  memberDoc;
                ]
              );
              Doc.softLine
            ]
          )
        in
        let parentDoc =
          let doc = printExpressionWithComments parentExpr cmtTbl in
          match Parens.unaryExprOperand parentExpr with
          | Parens.Parenthesized -> addParens doc
          | Braced braces  -> printBraces doc parentExpr braces
          | Nothing -> doc
        in
        Doc.group (Doc.concat [
          printAttributes expr.pexp_attributes;
          parentDoc;
          Doc.lbracket;
          member;
          Doc.rbracket;
        ])
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "set")}},
        [Nolabel, parentExpr; Nolabel, memberExpr; Nolabel, targetExpr]
      ) ->
        let member =
          let memberDoc =
            let doc = printExpressionWithComments memberExpr cmtTbl in
            match Parens.expr memberExpr with
            | Parens.Parenthesized -> addParens doc
            | Braced braces  -> printBraces doc memberExpr braces
            | Nothing -> doc
          in
          let shouldInline = match memberExpr.pexp_desc with
          | Pexp_constant _ | Pexp_ident _ -> true
          | _ -> false
          in
          if shouldInline then memberDoc else (
            Doc.concat [
              Doc.indent (
                Doc.concat [
                  Doc.softLine;
                  memberDoc;
                ]
              );
              Doc.softLine
            ]
          )
        in
        let shouldIndentTargetExpr =
          if ParsetreeViewer.isBracedExpr targetExpr then
            false
          else
          ParsetreeViewer.isBinaryExpression targetExpr ||
          (match targetExpr with
          | {
              pexp_attributes = [({Location.txt="ns.ternary"}, _)];
              pexp_desc = Pexp_ifthenelse (ifExpr, _, _)
            }  ->
            ParsetreeViewer.isBinaryExpression ifExpr || ParsetreeViewer.hasAttributes ifExpr.pexp_attributes
        | { pexp_desc = Pexp_newtype _} -> false
        | e ->
            ParsetreeViewer.hasAttributes e.pexp_attributes ||
            ParsetreeViewer.isArrayAccess e
          )
        in
        let targetExpr =
          let doc = printExpressionWithComments targetExpr cmtTbl in
          match Parens.expr targetExpr with
          | Parens.Parenthesized -> addParens doc
          | Braced braces  -> printBraces doc targetExpr braces
          | Nothing -> doc
        in
        let parentDoc =
          let doc = printExpressionWithComments parentExpr cmtTbl in
          match Parens.unaryExprOperand parentExpr with
          | Parens.Parenthesized -> addParens doc
          | Braced braces  -> printBraces doc parentExpr braces
          | Nothing -> doc
        in
        Doc.group (
          Doc.concat [
          printAttributes expr.pexp_attributes;
          parentDoc;
          Doc.lbracket;
          member;
          Doc.rbracket;
          Doc.text " =";
          if shouldIndentTargetExpr then
            Doc.indent (
              Doc.concat [
                Doc.line;
                targetExpr;
              ]
            )
          else
            Doc.concat [
              Doc.space;
              targetExpr;
            ]
          ]
        )
    (* TODO: cleanup, are those branches even remotely performant? *)
    | Pexp_apply (
        {pexp_desc = Pexp_ident lident},
        args
      ) when ParsetreeViewer.isJsxExpression expr ->
      printJsxExpression lident args cmtTbl
    | Pexp_apply (callExpr, args) ->
      let args = List.map (fun (lbl, arg) ->
        (lbl, ParsetreeViewer.rewriteUnderscoreApply arg)
      ) args
      in
      let (uncurried, attrs) =
        ParsetreeViewer.processUncurriedAttribute expr.pexp_attributes
      in
      let callExprDoc =
        let doc = printExpressionWithComments callExpr cmtTbl in
        match Parens.callExpr callExpr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc callExpr braces
        | Nothing -> doc
      in
      if ParsetreeViewer.requiresSpecialCallbackPrintingFirstArg args then
        let argsDoc =
          printArgumentsWithCallbackInFirstPosition ~uncurried args cmtTbl
        in
        Doc.concat [
          printAttributes attrs;
          callExprDoc;
          argsDoc;
        ]
      else if ParsetreeViewer.requiresSpecialCallbackPrintingLastArg args then
        let argsDoc =
          printArgumentsWithCallbackInLastPosition ~uncurried args cmtTbl
        in
        Doc.concat [
          printAttributes attrs;
          callExprDoc;
          argsDoc;
        ]
      else
        let argsDoc = printArguments ~uncurried args cmtTbl in
        Doc.concat [
          printAttributes attrs;
          callExprDoc;
          argsDoc;
        ]
    | _ -> assert false

  and printJsxExpression lident args cmtTbl =
    let name = printJsxName lident in
    let (formattedProps, children) = printJsxProps args cmtTbl in
    (* <div className="test" /> *)
    let isSelfClosing = match children with | [] -> true | _ -> false in
    Doc.group (
      Doc.concat [
        Doc.group (
          Doc.concat [
            printComments (Doc.concat [Doc.lessThan; name]) cmtTbl lident.Asttypes.loc;
            formattedProps;
            if isSelfClosing then Doc.concat [Doc.line; Doc.text "/>"] else Doc.nil
          ]
        );
        if isSelfClosing then Doc.nil
        else
          Doc.concat [
            Doc.greaterThan;
            Doc.indent (
              Doc.concat [
                Doc.line;
                printJsxChildren children cmtTbl;
              ]
            );
            Doc.line;
            Doc.text "</";
            name;
            Doc.greaterThan;
          ]
      ]
    )

  and printJsxFragment expr cmtTbl =
    let opening = Doc.text "<>" in
    let closing = Doc.text "</>" in
    let (children, _) = ParsetreeViewer.collectListExpressions expr in
    Doc.group (
      Doc.concat [
        opening;
        begin match children with
        | [] -> Doc.nil
        | children ->
          Doc.indent (
            Doc.concat [
              Doc.line;
              printJsxChildren children cmtTbl;
            ]
          )
        end;
        Doc.line;
        closing;
      ]
    )

  and printJsxChildren (children: Parsetree.expression list) cmtTbl =
    Doc.group (
      Doc.join ~sep:Doc.line (
        List.map (fun expr ->
          let exprDoc = printExpressionWithComments expr cmtTbl in
          match Parens.jsxChildExpr expr with
          | Parenthesized | Braced _ ->
            (* {(20: int)} make sure that we also protect the expression inside *)
            addBraces (if Parens.bracedExpr expr then addParens exprDoc else exprDoc)
          | Nothing -> exprDoc
        ) children
      )
    )

  and printJsxProps args cmtTbl =
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
        let formattedProps = Doc.indent (
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
        let (children, _) = ParsetreeViewer.collectListExpressions children in
        (formattedProps, children)
      | arg::args ->
        let propDoc = printJsxProp arg cmtTbl in
        loop (propDoc::props) args
    in
    loop [] args

  and printJsxProp arg cmtTbl =
    match arg with
    | (
        (Asttypes.Labelled lblTxt | Optional lblTxt) as lbl,
        {
          Parsetree.pexp_attributes = [({Location.txt = "ns.namedArgLoc"; loc = argLoc}, _)];
          pexp_desc = Pexp_ident {txt = Longident.Lident ident}
        }
      ) when lblTxt = ident (* jsx punning *) ->
      begin match lbl with
      | Nolabel -> Doc.nil
      | Labelled _lbl ->
        printComments (printIdentLike ident) cmtTbl argLoc
      | Optional _lbl ->
        let doc = Doc.concat [
          Doc.question;
          printIdentLike ident;
        ] in
        printComments doc cmtTbl argLoc
      end
    | (
        (Asttypes.Labelled lblTxt | Optional lblTxt) as lbl,
        {
          Parsetree.pexp_attributes = [];
          pexp_desc = Pexp_ident {txt = Longident.Lident ident}
        }
      ) when lblTxt = ident (* jsx punning when printing from Reason *) ->
      begin match lbl with
      | Nolabel -> Doc.nil
      | Labelled _lbl -> printIdentLike ident
      | Optional _lbl -> Doc.concat [
          Doc.question;
          printIdentLike ident;
        ]
      end
    | (lbl, expr) ->
      let (argLoc, expr) = match expr.pexp_attributes with
      | ({Location.txt = "ns.namedArgLoc"; loc}, _)::attrs ->
          (loc, {expr with pexp_attributes = attrs})
      | _ ->
        Location.none, expr
      in
      let lblDoc = match lbl with
      | Asttypes.Labelled lbl ->
        let lbl = printComments (printIdentLike lbl) cmtTbl argLoc in
        Doc.concat [lbl; Doc.equal]
      | Asttypes.Optional lbl ->
        let lbl = printComments (printIdentLike lbl) cmtTbl argLoc in
        Doc.concat [lbl; Doc.equal; Doc.question]
      | Nolabel -> Doc.nil
      in
      let exprDoc =
        let doc = printExpression expr cmtTbl in
        match Parens.jsxPropExpr expr with
        | Parenthesized | Braced(_) ->
          (* {(20: int)} make sure that we also protect the expression inside *)
          addBraces (if Parens.bracedExpr expr then addParens doc else doc)
        | _ -> doc
      in
      let fullLoc = {argLoc with loc_end = expr.pexp_loc.loc_end} in
      printComments
        (Doc.concat [
          lblDoc;
          exprDoc;
        ])
        cmtTbl
        fullLoc

  (* div -> div.
   * Navabar.createElement -> Navbar
   * Staff.Users.createElement -> Staff.Users *)
  and printJsxName {txt = lident} =
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

  and printArgumentsWithCallbackInFirstPosition ~uncurried args cmtTbl =
    let (callback, printedArgs) = match args with
    | (lbl, expr)::args ->
      let lblDoc = match lbl with
      | Asttypes.Nolabel -> Doc.nil
      | Asttypes.Labelled txt ->
        Doc.concat [
          Doc.tilde; printIdentLike txt; Doc.equal;
        ]
      | Asttypes.Optional txt ->
        Doc.concat [
          Doc.tilde; printIdentLike txt; Doc.equal; Doc.question;
        ]
      in
      let callback = Doc.concat [
        lblDoc;
        printPexpFun ~inCallback:true expr cmtTbl
      ] in
      let printedArgs = List.map (fun arg ->
        printArgument arg cmtTbl
      ) args |> Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line])
      in
      (callback, printedArgs)
    | _ -> assert false
    in
    (* Thing.map((arg1, arg2) => MyModuleBlah.toList(argument), foo) *)
    (* Thing.map((arg1, arg2) => {
     *   MyModuleBlah.toList(argument)
     * }, longArgumet, veryLooooongArgument)
     *)
    let fitsOnOneLine = Doc.concat [
      if uncurried then Doc.text "(. " else Doc.lparen;
      callback;
      Doc.comma;
      Doc.line;
      printedArgs;
      Doc.rparen;
    ] in

    (* Thing.map(
     *   (param1, parm2) => doStuff(param1, parm2),
     *   arg1,
     *   arg2,
     *   arg3,
     * )
     *)
    let breakAllArgs = printArguments ~uncurried args cmtTbl in
    Doc.customLayout [
      fitsOnOneLine;
      breakAllArgs;
    ]

  and printArgumentsWithCallbackInLastPosition ~uncurried args cmtTbl =
    let rec loop acc args = match args with
    | [] -> (Doc.nil, Doc.nil)
    | [lbl, expr] ->
      let lblDoc = match lbl with
      | Asttypes.Nolabel -> Doc.nil
      | Asttypes.Labelled txt ->
        Doc.concat [
          Doc.tilde; printIdentLike txt; Doc.equal;
        ]
      | Asttypes.Optional txt ->
        Doc.concat [
          Doc.tilde; printIdentLike txt; Doc.equal; Doc.question;
        ]
      in
      let callback = printPexpFun ~inCallback:true expr cmtTbl in
      (Doc.concat (List.rev acc), Doc.concat [lblDoc; callback])
    | arg::args ->
      let argDoc = printArgument arg cmtTbl in
      loop (Doc.line::Doc.comma::argDoc::acc) args
    in
    let (printedArgs, callback) = loop [] args in

    (* Thing.map(foo, (arg1, arg2) => MyModuleBlah.toList(argument)) *)
    let fitsOnOneLine = Doc.concat [
      if uncurried then Doc.text "(." else Doc.lparen;
      printedArgs;
      callback;
      Doc.rparen;
    ] in

    (* Thing.map(longArgumet, veryLooooongArgument, (arg1, arg2) =>
     *   MyModuleBlah.toList(argument)
     * )
     *)
    let arugmentsFitOnOneLine =
      Doc.concat [
        if uncurried then Doc.text "(." else Doc.lparen;
        Doc.softLine;
        printedArgs;
        Doc.breakableGroup ~forceBreak:true callback;
        Doc.softLine;
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
    let breakAllArgs = printArguments ~uncurried args cmtTbl in
    Doc.customLayout [
      fitsOnOneLine;
      arugmentsFitOnOneLine;
      breakAllArgs;
    ]

	and printArguments ~uncurried (args : (Asttypes.arg_label * Parsetree.expression) list) cmtTbl =
		match args with
		| [Nolabel, {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}] ->
      if uncurried then Doc.text "(.)" else Doc.text "()"
    | [(Nolabel, arg)] when ParsetreeViewer.isHuggableExpression arg ->
      let argDoc =
        let doc = printExpressionWithComments arg cmtTbl in
        match Parens.expr arg with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc arg braces
        | Nothing -> doc
      in
      Doc.concat [
        if uncurried then Doc.text "(." else Doc.lparen;
        argDoc;
        Doc.rparen;
      ]
		| args -> Doc.group (
				Doc.concat [
          if uncurried then Doc.text "(." else Doc.lparen;
					Doc.indent (
						Doc.concat [
              if uncurried then Doc.line else Doc.softLine;
							Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
								List.map (fun arg -> printArgument arg cmtTbl) args
							)
						]
					);
					Doc.trailingComma;
					Doc.softLine;
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
	and printArgument (argLbl, arg) cmtTbl =
		match (argLbl, arg) with
		(* ~a (punned)*)
		| (
				(Asttypes.Labelled lbl),
        ({pexp_desc=Pexp_ident {txt = Longident.Lident name};
          pexp_attributes = ([] | [({Location.txt = "ns.namedArgLoc";}, _)])
         } as argExpr)
			) when lbl = name && not (ParsetreeViewer.isBracedExpr argExpr) ->
      let loc = match arg.pexp_attributes with
      | ({Location.txt = "ns.namedArgLoc"; loc}, _)::_ -> loc
      | _ -> arg.pexp_loc
      in
      let doc = Doc.concat [
        Doc.tilde;
        printIdentLike lbl
      ] in
      printComments doc cmtTbl loc

		(* ~a: int (punned)*)
		| (
				(Asttypes.Labelled lbl),
        {pexp_desc = Pexp_constraint (
            {pexp_desc = Pexp_ident {txt = Longident.Lident name}} as argExpr,
            typ
         );
         pexp_loc;
         pexp_attributes = ([] | [({Location.txt = "ns.namedArgLoc";}, _)]) as attrs
        }
			) when lbl = name && not (ParsetreeViewer.isBracedExpr argExpr) ->
      let loc = match attrs with
      | ({Location.txt = "ns.namedArgLoc"; loc}, _)::_ ->
        {loc with loc_end = pexp_loc.loc_end}
      | _ -> arg.pexp_loc
      in
      let doc = Doc.concat [
        Doc.tilde;
        printIdentLike lbl;
        Doc.text ": ";
        printTypExpr typ cmtTbl;
      ] in
      printComments doc cmtTbl loc
		(* ~a? (optional lbl punned)*)
		| (
				(Asttypes.Optional lbl),
        {pexp_desc=Pexp_ident {txt = Longident.Lident name};
         pexp_attributes = ([] | [({Location.txt = "ns.namedArgLoc";}, _)])
        }
			) when lbl = name ->
      let loc = match arg.pexp_attributes with
      | ({Location.txt = "ns.namedArgLoc"; loc}, _)::_ -> loc
      | _ -> arg.pexp_loc
      in
      let doc = Doc.concat [
        Doc.tilde;
        printIdentLike lbl;
        Doc.question;
      ] in
      printComments doc cmtTbl loc
		| (_lbl, expr) ->
      let (argLoc, expr) = match expr.pexp_attributes with
      | ({Location.txt = "ns.namedArgLoc"; loc}, _)::attrs ->
          (loc, {expr with pexp_attributes = attrs})
      | _ ->
        expr.pexp_loc, expr
      in
			let printedLbl = match argLbl with
			| Asttypes.Nolabel -> Doc.nil
			| Asttypes.Labelled lbl ->
        let doc = Doc.concat [Doc.tilde; printIdentLike lbl; Doc.equal] in
        printComments doc cmtTbl argLoc
			| Asttypes.Optional lbl ->
        let doc = Doc.concat [Doc.tilde; printIdentLike lbl; Doc.equal; Doc.question] in
        printComments doc cmtTbl argLoc
			in
			let printedExpr =
        let doc = printExpressionWithComments expr cmtTbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc expr braces
        | Nothing -> doc
      in
      let loc = {argLoc with loc_end = expr.pexp_loc.loc_end} in
      let doc = Doc.concat [
        printedLbl;
        printedExpr;
      ] in
      printComments doc cmtTbl loc

  and printCases (cases: Parsetree.case list) cmtTbl =
    Doc.breakableGroup ~forceBreak:true (
      Doc.concat [
        Doc.lbrace;
          Doc.concat [
            Doc.line;
            printList
              ~getLoc:(fun n -> {n.Parsetree.pc_lhs.ppat_loc with
                loc_end =
                  match ParsetreeViewer.processBracesAttr n.Parsetree.pc_rhs with
                  | (None, _) -> n.pc_rhs.pexp_loc.loc_end
                  | (Some ({loc}, _), _) -> loc.Location.loc_end
              })
              ~print:printCase
              ~nodes:cases
              cmtTbl
          ];
        Doc.line;
        Doc.rbrace;
      ]
    )

  and printCase (case: Parsetree.case) cmtTbl =
    let rhs = match case.pc_rhs.pexp_desc with
    | Pexp_let _
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_open _
    | Pexp_sequence _ ->
      printExpressionBlock ~braces:(ParsetreeViewer.isBracedExpr case.pc_rhs) case.pc_rhs cmtTbl
    | _ ->
      let doc = printExpressionWithComments case.pc_rhs cmtTbl in
      begin match Parens.expr case.pc_rhs with
      | Parenthesized -> addParens doc
      | _ -> doc
      end

    in
    let guard = match case.pc_guard with
    | None -> Doc.nil
    | Some expr -> Doc.group (
        Doc.concat [
          Doc.line;
          Doc.text "when ";
          printExpressionWithComments expr cmtTbl;
        ]
      )
    in
    let shouldInlineRhs = match case.pc_rhs.pexp_desc with
    | Pexp_construct ({txt = Longident.Lident ("()" | "true" | "false")}, _)
    | Pexp_constant _
    | Pexp_ident _ -> true
    | _ when ParsetreeViewer.isHuggableRhs case.pc_rhs -> true
    | _ -> false
    in
    let shouldIndentPattern = match case.pc_lhs.ppat_desc with
    | Ppat_or _ -> false
    | _ -> true
    in
    let patternDoc =
      let doc = printPattern case.pc_lhs cmtTbl in
      match case.pc_lhs.ppat_desc with
      | Ppat_constraint _ -> addParens doc
      | _ -> doc
    in
    let content = Doc.concat [
      if shouldIndentPattern then Doc.indent patternDoc else patternDoc;
      Doc.indent guard;
      Doc.text " =>";
      Doc.indent (
        Doc.concat [
          if shouldInlineRhs then Doc.space else Doc.line;
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

  and printExprFunParameters ~inCallback ~uncurried ~hasConstraint parameters cmtTbl =
    match parameters with
    (* let f = _ => () *)
    | [ParsetreeViewer.Parameter {
      attrs = [];
      lbl = Asttypes.Nolabel;
      defaultExpr = None;
      pat = {Parsetree.ppat_desc = Ppat_any}
      }] when not uncurried ->
      if hasConstraint then Doc.text "(_)" else Doc.text "_"
    (* let f = a => () *)
    | [ParsetreeViewer.Parameter {
      attrs = [];
      lbl = Asttypes.Nolabel;
      defaultExpr = None;
      pat = {Parsetree.ppat_desc = Ppat_var stringLoc}
    }] when not uncurried ->
      let txtDoc =
        let var = printIdentLike stringLoc.txt in
        if hasConstraint then addParens var else var
      in
      printComments txtDoc cmtTbl stringLoc.loc
    (* let f = () => () *)
    | [ParsetreeViewer.Parameter {
        attrs = [];
        lbl = Asttypes.Nolabel;
        defaultExpr = None;
        pat = {ppat_desc = Ppat_construct({txt = Longident.Lident "()"}, None)}
    }] when not uncurried ->
      Doc.text "()"
    (* let f = (~greeting, ~from as hometown, ~x=?) => () *)
    | parameters ->
      let lparen = if uncurried then Doc.text "(. " else Doc.lparen in
      let shouldHug = ParsetreeViewer.parametersShouldHug parameters in
      let printedParamaters = Doc.concat [
        if shouldHug || inCallback then Doc.nil else Doc.softLine;
        Doc.join ~sep:(Doc.concat [Doc.comma; if inCallback then Doc.space else Doc.line])
          (List.map (fun p -> printExpFunParameter p cmtTbl) parameters)
      ] in
      Doc.group (
        Doc.concat [
          lparen;
          if shouldHug || inCallback then
            printedParamaters
          else Doc.indent printedParamaters;
          if shouldHug || inCallback then
            Doc.nil
          else
            Doc.concat [Doc.trailingComma; Doc.softLine];
          Doc.rparen;
        ]
      )

  and printExpFunParameter parameter cmtTbl =
    match parameter with
    | ParsetreeViewer.NewTypes {attrs; locs = lbls} ->
      Doc.group (
        Doc.concat [
          printAttributes attrs;
          Doc.text "type ";
          Doc.join ~sep:Doc.space (List.map (fun lbl ->
            printComments (printIdentLike lbl.Asttypes.txt) cmtTbl lbl.Asttypes.loc
          ) lbls)
        ]
      )
    | Parameter {attrs; lbl; defaultExpr; pat = pattern} ->
      let (isUncurried, attrs) = ParsetreeViewer.processUncurriedAttribute attrs in
      let uncurried = if isUncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil in
      let attrs = printAttributes attrs in
      (* =defaultValue *)
      let defaultExprDoc = match defaultExpr with
      | Some expr -> Doc.concat [
          Doc.text "=";
          printExpressionWithComments expr cmtTbl
        ]
      | None -> Doc.nil
      in
      (* ~from as hometown
       * ~from                   ->  punning *)
      let labelWithPattern = match (lbl, pattern) with
      | (Asttypes.Nolabel, pattern) -> printPattern pattern cmtTbl
      | (
          (Asttypes.Labelled lbl | Optional lbl),
          {ppat_desc = Ppat_var stringLoc;
           ppat_attributes = ([] | [({Location.txt = "ns.namedArgLoc";}, _)])
          }
        ) when lbl = stringLoc.txt ->
          (* ~d *)
          Doc.concat [
            Doc.text "~";
            printIdentLike lbl;
          ]
      | (
          (Asttypes.Labelled lbl | Optional lbl),
           ({ppat_desc = Ppat_constraint ({ ppat_desc = Ppat_var { txt } }, typ);
             ppat_attributes = ([] | [({Location.txt = "ns.namedArgLoc";}, _)])
            })
        ) when lbl = txt ->
          (* ~d: e *)
          Doc.concat [
            Doc.text "~";
            printIdentLike lbl;
            Doc.text ": ";
            printTypExpr typ cmtTbl;
          ]
      | ((Asttypes.Labelled lbl | Optional lbl), pattern) ->
          (* ~b as c *)
        Doc.concat [
          Doc.text "~";
          printIdentLike lbl;
          Doc.text " as ";
          printPattern pattern cmtTbl
        ]
      in
      let optionalLabelSuffix = match (lbl, defaultExpr) with
      | (Asttypes.Optional _, None) -> Doc.text "=?"
      | _ -> Doc.nil
      in
      let doc = Doc.group (
        Doc.concat [
          uncurried;
          attrs;
          labelWithPattern;
          defaultExprDoc;
          optionalLabelSuffix;
        ]
      ) in
      let cmtLoc = match defaultExpr with
      | None ->
        begin match pattern.ppat_attributes with
        | ({Location.txt = "ns.namedArgLoc"; loc}, _)::_ ->
          {loc with loc_end = pattern.ppat_loc.loc_end}
        | _ -> pattern.ppat_loc
        end
      | Some expr ->
        let startPos =  match pattern.ppat_attributes with
        | ({Location.txt = "ns.namedArgLoc"; loc}, _)::_ ->
            loc.loc_start
        | _ -> pattern.ppat_loc.loc_start
        in {
          pattern.ppat_loc with
          loc_start = startPos;
          loc_end = expr.pexp_loc.loc_end
        }
      in
      printComments doc cmtTbl cmtLoc

  and printExpressionBlock ~braces expr cmtTbl =
    let rec collectRows acc expr = match expr.Parsetree.pexp_desc with
    | Parsetree.Pexp_letmodule (modName, modExpr, expr2) ->
      let name =
        let doc = Doc.text modName.txt in
        printComments doc cmtTbl modName.loc
      in
      let letModuleDoc = Doc.concat [
        Doc.text "module ";
        name;
        Doc.text " = ";
        printModExpr modExpr cmtTbl;
      ] in
      let loc = {expr.pexp_loc with loc_end = modExpr.pmod_loc.loc_end} in
      collectRows ((loc, letModuleDoc)::acc) expr2
    | Pexp_letexception (extensionConstructor, expr2) ->
      let loc =
        let loc = {expr.pexp_loc with loc_end = extensionConstructor.pext_loc.loc_end} in
        match getFirstLeadingComment cmtTbl loc with
        | None -> loc
        | Some comment ->
          let cmtLoc = Comment.loc comment in
          {cmtLoc with loc_end = loc.loc_end}
      in
      let letExceptionDoc = printExceptionDef extensionConstructor cmtTbl in
      collectRows ((loc, letExceptionDoc)::acc) expr2
    | Pexp_open (overrideFlag, longidentLoc, expr2) ->
      let openDoc = Doc.concat [
        Doc.text "open";
        printOverrideFlag overrideFlag;
        Doc.space;
        printLongidentLocation longidentLoc cmtTbl;
      ] in
      let loc = {expr.pexp_loc with loc_end = longidentLoc.loc.loc_end} in
      collectRows ((loc, openDoc)::acc) expr2
    | Pexp_sequence (expr1, expr2) ->
      let exprDoc =
        let doc = printExpression expr1 cmtTbl in
        match Parens.expr expr1 with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc expr1 braces
        | Nothing -> doc
      in
      let loc = expr1.pexp_loc in
      collectRows ((loc, exprDoc)::acc) expr2
    | Pexp_let (recFlag, valueBindings, expr2) ->
      let loc =
        let loc = match (valueBindings, List.rev valueBindings) with
        | (vb::_, lastVb::_) -> {vb.pvb_loc with loc_end = lastVb.pvb_loc.loc_end}
        | _ -> Location.none
        in
        match getFirstLeadingComment cmtTbl loc with
        | None -> loc
        | Some comment ->
          let cmtLoc = Comment.loc comment in
          {cmtLoc with loc_end = loc.loc_end}
      in
			let recFlag = match recFlag with
			| Asttypes.Nonrecursive -> Doc.nil
			| Asttypes.Recursive -> Doc.text "rec "
			in
      let letDoc = printValueBindings ~recFlag valueBindings cmtTbl in
      (* let () = {
       *   let () = foo()
       *   ()
       * }
       * We don't need to print the () on the last line of the block
       *)
      begin match expr2.pexp_desc with
      | Pexp_construct ({txt = Longident.Lident "()"}, _) ->
        List.rev ((loc, letDoc)::acc)
      | _ ->
        collectRows ((loc, letDoc)::acc) expr2
      end
    | _ ->
      let exprDoc =
        let doc = printExpression expr cmtTbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces  -> printBraces doc expr braces
        | Nothing -> doc
      in
      List.rev ((expr.pexp_loc, exprDoc)::acc)
    in
    let rows = collectRows [] expr in
    let block =
      printList
        ~getLoc:fst
        ~nodes:rows
        ~print:(fun (_, doc) _ -> doc)
        ~forceBreak:true
        cmtTbl
    in
    Doc.breakableGroup ~forceBreak:true (
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
  and printBraces doc expr bracesLoc =
    let overMultipleLines =
      let open Location in
      bracesLoc.loc_end.pos_lnum > bracesLoc.loc_start.pos_lnum
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
      Doc.breakableGroup ~forceBreak:overMultipleLines (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              if Parens.bracedExpr expr then addParens doc else doc;
            ]
          );
          Doc.softLine;
          Doc.rbrace;
        ]
      )

  and printOverrideFlag overrideFlag = match overrideFlag with
    | Asttypes.Override -> Doc.text "!"
    | Fresh -> Doc.nil

  and printDirectionFlag flag = match flag with
    | Asttypes.Downto -> Doc.text " downto "
    | Asttypes.Upto -> Doc.text " to "

  and printRecordRow (lbl, expr) cmtTbl =
    let cmtLoc = {lbl.loc with loc_end = expr.pexp_loc.loc_end} in
    let doc = Doc.group (Doc.concat [
      printLidentPath lbl cmtTbl;
      Doc.text ": ";
      (let doc = printExpressionWithComments expr cmtTbl in
      match Parens.expr expr with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc expr braces
      | Nothing -> doc);
    ]) in
    printComments doc cmtTbl cmtLoc

  and printBsObjectRow (lbl, expr) cmtTbl =
    let cmtLoc = {lbl.loc with loc_end = expr.pexp_loc.loc_end} in
    let lblDoc =
      let doc = Doc.concat [
        Doc.text "\"";
        printLongident lbl.txt;
        Doc.text "\"";
      ] in
      printComments doc cmtTbl lbl.loc
    in
    let doc = Doc.concat [
      lblDoc;
      Doc.text ": ";
      printExpressionWithComments expr cmtTbl
    ] in
    printComments doc cmtTbl cmtLoc

  (* The optional loc indicates whether we need to print the attributes in
   * relation to some location. In practise this means the following:
   *  `@attr type t = string` -> on the same line, print on the same line
   *  `@attr
   *   type t = string` -> attr is on prev line, print the attributes
   *   with a line break between, we respect the users' original layout *)
  and printAttributes ?loc (attrs: Parsetree.attributes) =
    match ParsetreeViewer.filterParsingAttrs attrs with
    | [] -> Doc.nil
    | attrs ->
      let lineBreak = match loc with
      | None -> Doc.line
      | Some loc -> begin match List.rev attrs with
        | ({loc = firstLoc}, _)::_ when loc.loc_start.pos_lnum > firstLoc.loc_end.pos_lnum ->
          Doc.hardLine;
        | _ -> Doc.line
        end
      in
      Doc.concat [
        Doc.group (Doc.join ~sep:Doc.line (List.map printAttribute attrs));
        lineBreak;
      ]

  and printAttribute ((id, payload) : Parsetree.attribute) =
      let attrName = Doc.concat [
        Doc.text "@";
        Doc.text id.txt
      ] in
        match payload with
      | PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
        let exprDoc = printExpression expr CommentTable.empty in
        let needsParens = match attrs with | [] -> false | _ -> true in
        Doc.group (
          Doc.concat [
            attrName;
            addParens (
              Doc.concat [
                printAttributes attrs;
                if needsParens then addParens exprDoc else exprDoc;
              ]
            )
          ]
        )
      | PTyp typ ->
        Doc.group (
          Doc.concat [
            attrName;
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.text ": ";
                printTypExpr typ CommentTable.empty;
              ]
            );
            Doc.softLine;
            Doc.rparen;
          ]
        )
      | _ -> attrName

  and printAttributeWithComments ((id, payload) : Parsetree.attribute) cmtTbl =
      let attrName = Doc.text ("@" ^ id.txt) in
      match payload with
      | PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
        let exprDoc = printExpressionWithComments expr cmtTbl in
        let needsParens = match attrs with | [] -> false | _ -> true in
        Doc.group (
          Doc.concat [
            attrName;
            addParens (
              Doc.concat [
                printAttributes attrs;
                if needsParens then addParens exprDoc else exprDoc;
              ]
            )
          ]
        )
      | _ -> attrName

  and printModExpr modExpr cmtTbl =
    let doc = match modExpr.pmod_desc with
    | Pmod_ident longidentLoc ->
      printLongidentLocation longidentLoc cmtTbl
    | Pmod_structure structure ->
      Doc.breakableGroup ~forceBreak:true (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              printStructure structure cmtTbl;
            ];
          );
          Doc.softLine;
          Doc.rbrace;
        ]
      )
    | Pmod_unpack expr ->
      let shouldHug = match expr.pexp_desc with
      | Pexp_let _ -> true
      | Pexp_constraint (
          {pexp_desc = Pexp_let _ },
          {ptyp_desc = Ptyp_package _packageType}
        ) -> true
      | _ -> false
      in
      let (expr, moduleConstraint) = match expr.pexp_desc with
      | Pexp_constraint (
          expr,
          {ptyp_desc = Ptyp_package packageType; ptyp_loc}
      ) ->
        let packageDoc =
          let doc = printPackageType ~printModuleKeywordAndParens:false packageType cmtTbl in
          printComments doc cmtTbl ptyp_loc
        in
        let typeDoc = Doc.group (Doc.concat [
          Doc.text ":";
          Doc.indent (
            Doc.concat [
              Doc.line;
              packageDoc
            ]
          )
        ]) in
        (expr, typeDoc)
      | _ -> (expr, Doc.nil)
      in
      let unpackDoc = Doc.group(Doc.concat [
        printExpressionWithComments expr cmtTbl;
        moduleConstraint;
      ]) in
      Doc.group (
        Doc.concat [
          Doc.text "unpack(";
          if shouldHug then unpackDoc
          else
            Doc.concat [
              Doc.indent (
                Doc.concat [
                  Doc.softLine;
                  unpackDoc;
                ]
              );
             Doc.softLine;
            ];
          Doc.rparen;
        ]
      )
    | Pmod_extension extension ->
      printExtensionWithComments ~atModuleLvl:false extension cmtTbl
    | Pmod_apply _ ->
      let (args, callExpr) = ParsetreeViewer.modExprApply modExpr in
      let isUnitSugar = match args with
      | [{pmod_desc = Pmod_structure []}] -> true
      | _ -> false
      in
      let shouldHug = match args with
      | [{pmod_desc = Pmod_structure _}] -> true
      | _ -> false
      in
      Doc.group (
        Doc.concat [
          printModExpr callExpr cmtTbl;
          if isUnitSugar then
            printModApplyArg (List.hd args [@doesNotRaise]) cmtTbl
          else
            Doc.concat [
              Doc.lparen;
              if shouldHug then
                printModApplyArg (List.hd args [@doesNotRaise]) cmtTbl
              else
                Doc.indent (
                  Doc.concat [
                    Doc.softLine;
                    Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                      List.map (fun modArg -> printModApplyArg modArg cmtTbl) args
                    )
                  ]
                );
              if not shouldHug then
                Doc.concat [
                  Doc.trailingComma;
                  Doc.softLine;
                ]
              else Doc.nil;
              Doc.rparen;
            ]
        ]
      )
    | Pmod_constraint (modExpr, modType) ->
      Doc.concat [
        printModExpr modExpr cmtTbl;
        Doc.text ": ";
        printModType modType cmtTbl;
      ]
    | Pmod_functor _ ->
      printModFunctor modExpr cmtTbl
    in
    printComments doc cmtTbl modExpr.pmod_loc

  and printModFunctor modExpr cmtTbl =
    let (parameters, returnModExpr) = ParsetreeViewer.modExprFunctor modExpr in
    (* let shouldInline = match returnModExpr.pmod_desc with *)
    (* | Pmod_structure _ | Pmod_ident _ -> true *)
    (* | Pmod_constraint ({pmod_desc = Pmod_structure _}, _) -> true *)
    (* | _ -> false *)
    (* in *)
    let (returnConstraint, returnModExpr) = match returnModExpr.pmod_desc with
    | Pmod_constraint (modExpr, modType) ->
      let constraintDoc =
        let doc = printModType modType cmtTbl in
        if Parens.modExprFunctorConstraint modType then addParens doc else doc
      in
      let modConstraint = Doc.concat [
        Doc.text ": ";
        constraintDoc;
      ] in
      (modConstraint, printModExpr modExpr cmtTbl)
    | _ -> (Doc.nil, printModExpr returnModExpr cmtTbl)
    in
    let parametersDoc = match parameters with
    | [(attrs, {txt = "*"}, None)] ->
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map printAttribute attrs);
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
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun param -> printModFunctorParam param cmtTbl) parameters
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ]
      )
    in
    Doc.group (
      Doc.concat [
        parametersDoc;
        returnConstraint;
        Doc.text " => ";
        returnModExpr
      ]
    )

  and printModFunctorParam (attrs, lbl, optModType) cmtTbl =
    let cmtLoc = match optModType with
    | None -> lbl.Asttypes.loc
    | Some modType -> {lbl.loc with loc_end =
        modType.Parsetree.pmty_loc.loc_end
      }
    in
    let attrs = match attrs with
    | [] -> Doc.nil
    | attrs -> Doc.concat [
      Doc.join ~sep:Doc.line (List.map printAttribute attrs);
      Doc.line;
    ] in
    let lblDoc =
      let doc = Doc.text lbl.txt in
      printComments doc cmtTbl lbl.loc
    in
    let doc = Doc.group (
      Doc.concat [
        attrs;
        lblDoc;
        (match optModType with
        | None -> Doc.nil
        | Some modType ->
          Doc.concat [
            Doc.text ": ";
            printModType modType cmtTbl
          ]);
      ]
    ) in
    printComments doc cmtTbl cmtLoc

  and printModApplyArg modExpr cmtTbl =
    match modExpr.pmod_desc with
    | Pmod_structure [] -> Doc.text "()"
    | _ -> printModExpr modExpr cmtTbl


  and printExceptionDef (constr : Parsetree.extension_constructor) cmtTbl =
    let kind = match constr.pext_kind with
    | Pext_rebind longident -> Doc.indent (
        Doc.concat [
          Doc.text " =";
          Doc.line;
          printLongidentLocation longident cmtTbl;
        ]
     )
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadtDoc = match gadt with
      | Some typ -> Doc.concat [
          Doc.text ": ";
          printTypExpr typ cmtTbl
        ]
      | None -> Doc.nil
      in
      Doc.concat [
        printConstructorArguments ~indent:false args cmtTbl;
        gadtDoc
      ]
    in
    let name =
      printComments
        (Doc.text constr.pext_name.txt)
        cmtTbl
        constr.pext_name.loc
    in
    let doc = Doc.group (
      Doc.concat [
        printAttributes constr.pext_attributes;
        Doc.text "exception ";
        name;
        kind
      ]
    ) in
    printComments doc cmtTbl constr.pext_loc

  and printExtensionConstructor (constr : Parsetree.extension_constructor) cmtTbl i =
    let attrs = printAttributes constr.pext_attributes in
    let bar = if i > 0 then Doc.text "| "
      else Doc.ifBreaks (Doc.text "| ") Doc.nil
    in
    let kind = match constr.pext_kind with
    | Pext_rebind longident -> Doc.indent (
        Doc.concat [
          Doc.text " =";
          Doc.line;
          printLongidentLocation longident cmtTbl;
        ]
     )
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadtDoc = match gadt with
      | Some typ -> Doc.concat [
          Doc.text ": ";
          printTypExpr typ cmtTbl;
        ]
      | None -> Doc.nil
      in
      Doc.concat [
        printConstructorArguments ~indent:false args cmtTbl;
        gadtDoc
      ]
    in
    let name =
      printComments (Doc.text constr.pext_name.txt) cmtTbl constr.pext_name.loc
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

  let printImplementation ~width (s: Parsetree.structure) comments =
    let cmtTbl = CommentTable.make () in
    CommentTable.walkStructure s cmtTbl comments;
    (* CommentTable.log cmtTbl; *)
    let doc = printStructure s cmtTbl in
    (* Doc.debug doc; *)
    let stringDoc = Doc.toString ~width doc in
    print_string stringDoc

  let printInterface ~width (s: Parsetree.signature) comments =
    let cmtTbl = CommentTable.make () in
    CommentTable.walkSignature s cmtTbl comments;
    let stringDoc = Doc.toString ~width (printSignature s cmtTbl) in
    print_string stringDoc

end

module Scanner = struct
  type mode = Template | Jsx | Diamond

  type t = {
    filename: string;
    src: bytes;
    mutable err:
      startPos: Lexing.position
      -> endPos: Lexing.position
      -> Diagnostics.category
      -> unit;
    mutable ch: int; (* current character *)
    mutable offset: int; (* character offset *)
    mutable rdOffset: int; (* reading offset (position after current character) *)
    mutable lineOffset: int; (* current line offset *)
    mutable lnum: int; (* current line number *)
    mutable mode: mode list;
  }

  let setDiamondMode scanner =
    scanner.mode <- Diamond::scanner.mode

  let setTemplateMode scanner =
    scanner.mode <- Template::scanner.mode

  let setJsxMode scanner =
    scanner.mode <- Jsx::scanner.mode

  let popMode scanner mode =
    match scanner.mode with
    | m::ms when m = mode ->
      scanner.mode <- ms
    | _ -> ()

  let inDiamondMode scanner = match scanner.mode with
    | Diamond::_ -> true
    | _ -> false

  let inJsxMode scanner = match scanner.mode with
    | Jsx::_ -> true
    | _ -> false

  let inTemplateMode scanner = match scanner.mode with
    | Template::_ -> true
    | _ -> false

  let position scanner = Lexing.{
    pos_fname = scanner.filename;
    (* line number *)
    pos_lnum = scanner.lnum;
    (* offset of the beginning of the line (number
       of characters between the beginning of the scanner and the beginning
       of the line) *)
    pos_bol = scanner.lineOffset;
    (* [pos_cnum] is the offset of the position (number of
       characters between the beginning of the scanner and the position). *)
    pos_cnum = scanner.offset;
  }

  let next scanner =
    if scanner.rdOffset < (Bytes.length scanner.src) then (
      scanner.offset <- scanner.rdOffset;
      let ch = (Bytes.get [@doesNotRaise]) scanner.src scanner.rdOffset in
      scanner.rdOffset <- scanner.rdOffset + 1;
      scanner.ch <- int_of_char ch
    ) else (
      scanner.offset <- Bytes.length scanner.src;
      scanner.ch <- -1
    )

  let peek scanner =
    if scanner.rdOffset < (Bytes.length scanner.src) then
      int_of_char (Bytes.unsafe_get scanner.src scanner.rdOffset)
    else
      -1

  let make b filename =
    let scanner = {
      filename;
      src = b;
      err = (fun ~startPos:_ ~endPos:_ _ -> ());
      ch = CharacterCodes.space;
      offset = 0;
      rdOffset = 0;
      lineOffset = 0;
      lnum = 1;
      mode = [];
    } in
    next scanner;
    scanner

  let skipWhitespace scanner =
    let rec scan () =
      if scanner.ch == CharacterCodes.space || scanner.ch == CharacterCodes.tab then (
        next scanner;
        scan()
      ) else if CharacterCodes.isLineBreak scanner.ch then (
        scanner.lineOffset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
        next scanner;
        scan()
      ) else (
        ()
      )
    in
    scan()

  let scanIdentifier scanner =
    let startOff = scanner.offset in
    while (
      CharacterCodes.isLetter scanner.ch ||
      CharacterCodes.isDigit scanner.ch ||
      CharacterCodes.underscore == scanner.ch ||
      CharacterCodes.singleQuote == scanner.ch
    ) do
      next scanner
    done;
    let str = Bytes.sub_string scanner.src startOff (scanner.offset - startOff) in
    Token.lookupKeyword str

  let scanDigits scanner ~base =
    if base <= 10 then (
      while CharacterCodes.isDigit scanner.ch || scanner.ch == CharacterCodes.underscore do
        next scanner
      done;
    ) else (
      while CharacterCodes.isHex scanner.ch || scanner.ch == CharacterCodes.underscore do
        next scanner
      done;
    )

  (* float: (0…9) { 0…9∣ _ } [. { 0…9∣ _ }] [(e∣ E) [+∣ -] (0…9) { 0…9∣ _ }]   *)
  let scanNumber scanner =
    let startOff = scanner.offset in

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
    scanDigits scanner ~base;

    (*  *)
    let isFloat = if CharacterCodes.dot == scanner.ch then (
      next scanner;
      scanDigits scanner ~base;
      true
    ) else (
      false
    ) in

    (* exponent part *)
    let isFloat =
      if let exp = CharacterCodes.lower scanner.ch in
        exp == CharacterCodes.Lower.e || exp == CharacterCodes.Lower.p
      then (
        next scanner;
        if scanner.ch == CharacterCodes.plus || scanner.ch == CharacterCodes.minus then
          next scanner;
        scanDigits scanner ~base;
        true
      ) else
        isFloat
    in
    let literal =
      Bytes.sub_string scanner.src startOff (scanner.offset - startOff)
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
    if isFloat then
      Token.Float {f = literal; suffix}
    else
      Token.Int {i = literal; suffix}

  let scanExoticIdentifier scanner =
    next scanner;
    let buffer = Buffer.create 20 in
    let startPos = position scanner in

    let rec scan () =
      if scanner.ch == CharacterCodes.eof then
        let endPos = position scanner in
        scanner.err ~startPos ~endPos (Diagnostics.message "Did you forget a \" here?")
      else if scanner.ch == CharacterCodes.doubleQuote then (
        next scanner
      ) else if CharacterCodes.isLineBreak scanner.ch then (
        scanner.lineOffset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
        let endPos = position scanner in
        scanner.err ~startPos ~endPos (Diagnostics.message "Did you forget a \" here?");
        next scanner
      ) else (
        Buffer.add_char buffer ((Char.chr [@doesNotRaise]) scanner.ch);
        next scanner;
        scan()
      )
    in
    scan();
    Token.Lident (Buffer.contents buffer)

  let scanStringEscapeSequence ~startPos scanner =
    (* \ already consumed *)
    if CharacterCodes.Lower.n == scanner.ch
      || CharacterCodes.Lower.t == scanner.ch
      || CharacterCodes.Lower.b == scanner.ch
      || CharacterCodes.Lower.r == scanner.ch
      || CharacterCodes.backslash == scanner.ch
      || CharacterCodes.space == scanner.ch
      || CharacterCodes.singleQuote == scanner.ch
      || CharacterCodes.doubleQuote == scanner.ch
    then
      next scanner
    else
      let (n, base, max) =
        if CharacterCodes.isDigit scanner.ch then
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
              let d = CharacterCodes.digitValue scanner.ch in
              if d >= base then
                let pos = position scanner in
                let msg = if scanner.ch == -1 then
                  "unclosed escape sequence"
                else "unknown escape sequence"
                in
                scanner.err ~startPos ~endPos:pos (Diagnostics.message msg);
                -1
              else
                let () = next scanner in
                while_ (n - 1) (x * base + d)
          in
          let x = while_ n 0 in
          if x > max then
            let pos = position scanner in
            let msg = "invalid escape sequence (value too high)" in
            scanner.err ~startPos ~endPos:pos (Diagnostics.message msg);
          ()

  let scanString scanner =
    let offs = scanner.offset in

    let startPos = position scanner in
    let rec scan () =
      if scanner.ch == CharacterCodes.eof then
        let endPos = position scanner in
        scanner.err ~startPos ~endPos Diagnostics.unclosedString
      else if scanner.ch == CharacterCodes.doubleQuote then (
        next scanner;
      ) else if scanner.ch == CharacterCodes.backslash then (
        let startPos = position scanner in
        next scanner;
        scanStringEscapeSequence ~startPos scanner;
        scan ()
      ) else if CharacterCodes.isLineBreak scanner.ch then (
        scanner.lineOffset <- scanner.offset + 1;
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
  let convertNumber scanner ~n ~base =
    let x = ref 0 in
    for _ = n downto 1 do
      let d = CharacterCodes.digitValue scanner.ch in
      x := (!x * base) + d;
      next scanner
    done;
    !x

  let scanEscape scanner =
    (* let offset = scanner.offset in *)
    let c = match scanner.ch with
    | 98 (* b *)  -> next scanner; '\008'
    | 110 (* n *) -> next scanner; '\010'
    | 114 (* r *) -> next scanner; '\013'
    | 116 (* t *) -> next scanner; '\009'
    | ch when CharacterCodes.isDigit ch ->
      let x = convertNumber scanner ~n:3 ~base:10 in
      (Char.chr [@doesNotRaise]) x
    | ch when ch == CharacterCodes.Lower.x ->
      next scanner;
      let x = convertNumber scanner ~n:2 ~base:16 in
      (Char.chr [@doesNotRaise]) x
    | ch when ch == CharacterCodes.Lower.o ->
      next scanner;
      let x = convertNumber scanner ~n:3 ~base:8 in
      (Char.chr [@doesNotRaise]) x
    | ch ->
      next scanner;
      (Char.chr [@doesNotRaise]) ch
    in
    next scanner; (* Consume \' *)
    Token.Character c

  let scanSingleLineComment scanner =
    let startOff = scanner.offset in
    let startPos = position scanner in
    while not (CharacterCodes.isLineBreak scanner.ch) && scanner.ch >= 0 do
      next scanner
    done;
    let endPos = position scanner in
    Token.Comment (
      Comment.makeSingleLineComment
        ~loc:(Location.{loc_start = startPos; loc_end = endPos; loc_ghost = false})
        (Bytes.sub_string scanner.src startOff (scanner.offset - startOff))
    )

  let scanMultiLineComment scanner =
    let startOff = scanner.offset in
    let startPos = position scanner in
    let rec scan ~depth () =
      if scanner.ch == CharacterCodes.asterisk &&
         peek scanner == CharacterCodes.forwardslash then (
        next scanner;
        next scanner;
        if depth > 0 then scan ~depth:(depth - 1) () else ()
      ) else if scanner.ch == CharacterCodes.eof then (
        let endPos = position scanner in
        scanner.err ~startPos ~endPos Diagnostics.unclosedComment
      ) else if scanner.ch == CharacterCodes.forwardslash
        && peek scanner == CharacterCodes. asterisk then (
        next scanner;
        next scanner;
        scan ~depth:(depth + 1) ()
      ) else (
        if CharacterCodes.isLineBreak scanner.ch then (
          scanner.lineOffset <- scanner.offset + 1;
          scanner.lnum <- scanner.lnum + 1;
        );
        next scanner;
        scan ~depth ()
      )
    in
    scan ~depth:0 ();
    Token.Comment (
      Comment.makeMultiLineComment
        ~loc:(Location.{loc_start = startPos; loc_end = (position scanner); loc_ghost = false})
        (Bytes.sub_string scanner.src startOff (scanner.offset - 2 - startOff))
    )

  let scanTemplate scanner =
    let startOff = scanner.offset in
    let startPos = position scanner in

    let rec scan () =
      if scanner.ch == CharacterCodes.eof then (
        let endPos = position scanner in
        scanner.err ~startPos ~endPos Diagnostics.unclosedTemplate;
        popMode scanner Template;
        Token.TemplateTail(
          Bytes.sub_string scanner.src startOff (scanner.offset - 2 - startOff)
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
          Bytes.sub_string scanner.src startOff (scanner.offset - 1 - startOff)
        in
        popMode scanner Template;
        Token.TemplateTail contents
      ) else if scanner.ch == CharacterCodes.dollar &&
                peek scanner == CharacterCodes.lbrace
        then (
          next scanner; (* consume $ *)
          next scanner; (* consume { *)
          let contents =
            Bytes.sub_string scanner.src startOff (scanner.offset - 2 - startOff)
          in
          popMode scanner Template;
          Token.TemplatePart contents
      ) else (
        if CharacterCodes.isLineBreak scanner.ch then (
          scanner.lineOffset <- scanner.offset + 1;
          scanner.lnum <- scanner.lnum + 1;
        );
        next scanner;
        scan()
      )
    in
    scan()

  let rec scan scanner =
    if not (inTemplateMode scanner) then skipWhitespace scanner;
    let startPos = position scanner in
    let ch = scanner.ch in
    let token = if inTemplateMode scanner then
      scanTemplate scanner
    else if ch == CharacterCodes.underscore then (
      let nextCh = peek scanner in
      if nextCh == CharacterCodes.underscore || CharacterCodes.isDigit nextCh || CharacterCodes.isLetter nextCh then
        scanIdentifier scanner
      else (
        next scanner;
        Token.Underscore
      )
    ) else if CharacterCodes.isLetter ch then
      scanIdentifier scanner
    else if CharacterCodes.isDigit ch then
      scanNumber scanner
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
      else if ch == CharacterCodes.doubleQuote then
        scanString scanner
      else if ch == CharacterCodes.singleQuote then (
        if scanner.ch == CharacterCodes.backslash
          && not ((peek scanner) == CharacterCodes.doubleQuote) (* start of exotic ident *)
        then (
          next scanner;
          scanEscape scanner
        ) else if (peek scanner) == CharacterCodes.singleQuote then (
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
        if scanner.ch == CharacterCodes.greaterThan then (
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
        ) else if scanner.ch == CharacterCodes.greaterThan then (
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
        ) else if (scanner.ch == CharacterCodes.greaterThan) then (
          next scanner;
          Token.ColonGreaterThan
        ) else (
          Token.Colon
        )
      else if ch == CharacterCodes.backslash then
        scanExoticIdentifier scanner
      else if ch == CharacterCodes.forwardslash then
        if scanner.ch == CharacterCodes.forwardslash then (
          next scanner;
          scanSingleLineComment scanner
        ) else if (scanner.ch == CharacterCodes.asterisk) then (
          next scanner;
          scanMultiLineComment scanner
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
        ) else if scanner.ch == CharacterCodes.greaterThan then (
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
      else if ch == CharacterCodes.greaterThan then
        if scanner.ch == CharacterCodes.equal && not (inDiamondMode scanner) then (
          next scanner;
          Token.GreaterEqual
        ) else (
          Token.GreaterThan
        )
      else if ch == CharacterCodes.lessThan then
        (* Imagine the following: <div><
         * < indicates the start of a new jsx-element, the parser expects
         * the name of a new element after the <
         * Example: <div> <div
         * But what if we have a / here: example </ in  <div></div>
         * This signals a closing element. To simulate the two-token lookahead,
         * the </ is emitted as a single new token LessThanSlash *)
        if inJsxMode scanner then (
          skipWhitespace scanner;
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
        let endPos = position scanner in
        scanner.err ~startPos ~endPos (Diagnostics.unknownUchar ch);
        let (_, _, token) = scan scanner in
        token
      )
    end in
    let endPos = position scanner in
    (startPos, endPos, token)

  (* Imagine: <div> <Navbar /> <
   * is `<` the start of a jsx-child? <div …
   * or is it the start of a closing tag?  </div>
   * reconsiderLessThan peeks at the next token and
   * determines the correct token to disambiguate *)
  let reconsiderLessThan scanner =
    (* < consumed *)
    skipWhitespace scanner;
    if scanner.ch == CharacterCodes.forwardslash then
      let () = next scanner in
      Token.LessThanSlash
    else
      Token.LessThan

  (* If an operator has whitespace around both sides, it's a binary operator *)
  let isBinaryOp src startCnum endCnum =
    if startCnum == 0 then false
    else
      let leftOk =
        let c =
          (startCnum - 1)
          |> (Bytes.get [@doesNotRaise]) src
          |> Char.code
        in
        c == CharacterCodes.space ||
        c == CharacterCodes.tab ||
        CharacterCodes.isLineBreak c
      in
      let rightOk =
        let c =
          if endCnum == Bytes.length src then -1
          else endCnum |> (Bytes.get [@doesNotRaise]) src |> Char.code
        in
        c == CharacterCodes.space ||
        c == CharacterCodes.tab ||
        CharacterCodes.isLineBreak c ||
        c == CharacterCodes.eof
      in
      leftOk && rightOk
end

(* AST for js externals *)
module JsFfi = struct
  type scope =
    | Global
    | Module of string (* bs.module("path") *)
    | Scope of Longident.t (* bs.scope(/"window", "location"/) *)

  type label_declaration = {
    jld_attributes: Parsetree.attributes; [@live]
    jld_name: string;
    jld_alias: string;
    jld_type: Parsetree.core_type;
    jld_loc:  Location.t
  }

  type importSpec =
    | Default of label_declaration
    | Spec of label_declaration list

  type import_description = {
    jid_loc: Location.t;
    jid_spec: importSpec;
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

  let importDescr ~attrs ~scope ~importSpec ~loc = {
    jid_loc = loc;
    jid_spec = importSpec;
    jid_scope = scope;
    jid_attributes = attrs;
  }

  let toParsetree importDescr =
    let bsVal = (Location.mknoloc "bs.val", Parsetree.PStr []) in
    let attrs = match importDescr.jid_scope with
    | Global -> [bsVal]
    (* @genType.import("./MyMath"),
     * @genType.import(/"./MyMath", "default"/) *)
    | Module s ->
      let structure = [
        Parsetree.Pconst_string (s, None)
        |> Ast_helper.Exp.constant
        |> Ast_helper.Str.eval
      ] in
      let genType = (Location.mknoloc "genType.import", Parsetree.PStr structure) in
      [genType]
    | Scope longident ->
      let structureItem =
        let expr = match Longident.flatten longident |> List.map (fun s ->
          Ast_helper.Exp.constant (Parsetree.Pconst_string (s, None))
        ) with
        | [expr] -> expr
        | [] as exprs | (_ as exprs) -> exprs |> Ast_helper.Exp.tuple
        in
        Ast_helper.Str.eval expr
      in
      let bsScope = (
        Location.mknoloc "bs.scope",
        Parsetree. PStr [structureItem]
      ) in
      [bsVal; bsScope]
    in
    let valueDescrs = match importDescr.jid_spec with
    | Default decl ->
      let prim = [decl.jld_name] in
      let allAttrs =
        List.concat [attrs; importDescr.jid_attributes]
        |> List.map (fun attr -> match attr with
          | (
              {Location.txt = "genType.import"} as id,
              Parsetree.PStr [{pstr_desc = Parsetree.Pstr_eval (moduleName, _) }]
            ) ->
            let default =
              Parsetree.Pconst_string ("default", None) |> Ast_helper.Exp.constant
            in
            let structureItem =
              [moduleName; default]
              |> Ast_helper.Exp.tuple
              |> Ast_helper.Str.eval
            in
            (id, Parsetree.PStr [structureItem])
          | attr -> attr
        )
      in
      [Ast_helper.Val.mk
        ~loc:importDescr.jid_loc
        ~prim
        ~attrs:allAttrs
        (Location.mknoloc decl.jld_alias)
        decl.jld_type
      |> Ast_helper.Str.primitive]
    | Spec decls ->
      List.map (fun decl ->
        let prim = [decl.jld_name] in
        let allAttrs = List.concat [attrs; decl.jld_attributes] in
        Ast_helper.Val.mk
          ~loc:importDescr.jid_loc
          ~prim
          ~attrs:allAttrs
          (Location.mknoloc decl.jld_alias)
          decl.jld_type
        |> Ast_helper.Str.primitive ~loc:decl.jld_loc
      ) decls
    in
    let jsFfiAttr = (Location.mknoloc "ns.jsFfi", Parsetree.PStr []) in
    Ast_helper.Mod.structure ~loc:importDescr.jid_loc valueDescrs
    |> Ast_helper.Incl.mk ~attrs:[jsFfiAttr] ~loc:importDescr.jid_loc
    |> Ast_helper.Str.include_ ~loc:importDescr.jid_loc
end

module ParsetreeCompatibility = struct
  let concatLongidents l1 l2 =
    let parts1 = Longident.flatten l1 in
    let parts2 = Longident.flatten l2 in
    match List.concat [parts1; parts2] |> Longident.unflatten with
    | Some longident -> longident
    | None -> l2

  (* TODO: support nested open's ? *)
  let rec rewritePpatOpen longidentOpen pat =
    let open Parsetree in
    match pat.ppat_desc with
    | Ppat_array (first::rest) ->
      (* Color.[Red, Blue, Green] -> [Color.Red, Blue, Green] *)
      {pat with ppat_desc = Ppat_array ((rewritePpatOpen longidentOpen first)::rest)}
    | Ppat_tuple (first::rest) ->
      (* Color.(Red, Blue, Green) -> (Color.Red, Blue, Green) *)
      {pat with ppat_desc = Ppat_tuple ((rewritePpatOpen longidentOpen first)::rest)}
    | Ppat_construct(
        {txt = Longident.Lident "::"} as listConstructor,
        Some ({ppat_desc=Ppat_tuple (pat::rest)} as element)
      ) ->
      (* Color.(list[Red, Blue, Green]) -> list[Color.Red, Blue, Green] *)
      {pat with ppat_desc =
        Ppat_construct (
          listConstructor,
          Some {element with ppat_desc = Ppat_tuple ((rewritePpatOpen longidentOpen pat)::rest)}
        )
      }
    | Ppat_construct ({txt = constructor} as longidentLoc, optPattern) ->
      (* Foo.(Bar(a)) -> Foo.Bar(a) *)
      {pat with ppat_desc =
        Ppat_construct (
          {longidentLoc with txt = concatLongidents longidentOpen constructor},
          optPattern
        )
      }
    | Ppat_record (({txt = lbl} as longidentLoc, firstPat)::rest, flag) ->
      (* Foo.{x} -> {Foo.x: x} *)
      let firstRow = (
        {longidentLoc with txt = concatLongidents longidentOpen lbl},
        firstPat
      ) in
      {pat with ppat_desc = Ppat_record (firstRow::rest, flag)}
    | Ppat_or (pat1, pat2) ->
      {pat with ppat_desc = Ppat_or (
        rewritePpatOpen longidentOpen pat1,
        rewritePpatOpen longidentOpen pat2
      )}
    | Ppat_constraint (pattern, typ) ->
      {pat with ppat_desc = Ppat_constraint (
        rewritePpatOpen longidentOpen pattern,
        typ
      )}
    | Ppat_type ({txt = constructor} as longidentLoc) ->
      {pat with ppat_desc = Ppat_type (
        {longidentLoc with txt = concatLongidents longidentOpen constructor}
      )}
    | Ppat_lazy p ->
      {pat with ppat_desc = Ppat_lazy (rewritePpatOpen longidentOpen p)}
    | Ppat_exception p ->
      {pat with ppat_desc = Ppat_exception (rewritePpatOpen longidentOpen p)}
    | _ -> pat

  let rec rewriteReasonFastPipe expr =
    let open Parsetree in
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_apply (
          {pexp_desc = Pexp_ident {txt = Longident.Lident "|."}} as op,
          [Asttypes.Nolabel, lhs; Nolabel, rhs]
        ); pexp_attributes = subAttrs},
        args
      ) ->
      let rhsLoc = {rhs.pexp_loc with loc_end = expr.pexp_loc.loc_end} in
      let newLhs =
        let expr = rewriteReasonFastPipe lhs in
        {expr with pexp_attributes = subAttrs}
      in
      let allArgs =
        (Asttypes.Nolabel, newLhs)::[
          Asttypes.Nolabel, Ast_helper.Exp.apply ~loc:rhsLoc rhs args
        ]
      in
      Ast_helper.Exp.apply ~attrs:expr.pexp_attributes ~loc:expr.pexp_loc op allArgs
    | _ -> expr

  let makeReasonArityMapper ~forPrinter =
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
          let newArgs = match args with
          | (Some {pexp_desc = Pexp_tuple [{pexp_desc = Pexp_tuple _ } as sp]}) as args ->
            if forPrinter then args else Some sp
          | Some {pexp_desc = Pexp_tuple [sp]} -> Some sp
          | _ -> args
          in
          default_mapper.expr mapper { pexp_desc=Pexp_construct(lid, newArgs); pexp_loc; pexp_attributes}
        | expr ->
          default_mapper.expr mapper (rewriteReasonFastPipe expr)
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
            if forPrinter then args else Some sp
           | Some {ppat_desc = Ppat_tuple [sp]} -> Some sp
           | _ -> args in
           default_mapper.pat mapper { ppat_desc=Ppat_construct(lid, new_args); ppat_loc; ppat_attributes;}
          | x -> default_mapper.pat mapper x
      end;
    }

  let escapeTemplateLiteral s =
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

  let escapeStringContents s =
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

  let looksLikeRecursiveTypeDeclaration typeDeclaration =
    let open Parsetree in
    let name = typeDeclaration.ptype_name.txt in
    let rec checkKind kind =
      match kind with
      | Ptype_abstract | Ptype_open -> false
      | Ptype_variant constructorDeclarations ->
        List.exists checkConstructorDeclaration constructorDeclarations
      | Ptype_record labelDeclarations ->
        List.exists checkLabelDeclaration labelDeclarations

    and checkConstructorDeclaration constrDecl =
      checkConstructorArguments constrDecl.pcd_args
      || (match constrDecl.pcd_res with
      | Some typexpr ->
        checkTypExpr typexpr
      | None -> false
      )

    and checkLabelDeclaration labelDeclaration =
      checkTypExpr labelDeclaration.pld_type

    and checkConstructorArguments constrArg =
      match constrArg with
      | Pcstr_tuple types ->
        List.exists checkTypExpr types
      | Pcstr_record labelDeclarations ->
        List.exists checkLabelDeclaration labelDeclarations

    and checkTypExpr typ =
      match typ.ptyp_desc with
      | Ptyp_any -> false
      | Ptyp_var _ -> false
      | Ptyp_object _ -> false
      | Ptyp_class _ -> false
      | Ptyp_package _ -> false
      | Ptyp_extension _ -> false
      | Ptyp_arrow (_lbl, typ1, typ2) ->
        checkTypExpr typ1 || checkTypExpr typ2
      | Ptyp_tuple types ->
        List.exists checkTypExpr types
      | Ptyp_constr ({txt = longident}, types) ->
        (match longident with
        | Lident ident -> ident = name
        | _ -> false
        ) ||
        List.exists checkTypExpr types
      | Ptyp_alias (typ, _) -> checkTypExpr typ
      | Ptyp_variant (rowFields, _, _) ->
        List.exists checkRowFields rowFields
      | Ptyp_poly (_, typ) ->
        checkTypExpr typ

    and checkRowFields rowField =
      match rowField with
      | Rtag (_, _, _, types) ->
        List.exists checkTypExpr types
      | Rinherit typexpr ->
        checkTypExpr typexpr
    in
    checkKind typeDeclaration.ptype_kind


  let filterReasonRawLiteral attrs =
    List.filter (fun attr ->
      match attr with
      | ({Location.txt = ("reason.raw_literal")}, _) -> false
      | _ -> true
    ) attrs

  let stringLiteralMapper stringData =
    let isSameLocation l1 l2 =
      let open Location in
      l1.loc_start.pos_cnum == l2.loc_start.pos_cnum
    in
    let remainingStringData = stringData in
    let open Ast_mapper in
    { default_mapper with
      expr = (fun mapper expr ->
        match expr.pexp_desc with
        | Pexp_constant (Pconst_string (_txt, None)) ->
          begin match
            List.find_opt (fun (_stringData, stringLoc) ->
              isSameLocation stringLoc expr.pexp_loc
            ) remainingStringData
          with
          | Some(stringData, _) ->
            let stringData =
              let attr = List.find_opt (fun attr -> match attr with
              | ({Location.txt = ("reason.raw_literal")}, _) -> true
              | _ -> false
              ) expr.pexp_attributes in
              match attr with
              | Some (_, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (raw, _))}, _)}]) ->
                raw
              | _ -> (String.sub [@doesNotRaise]) stringData 1 (String.length stringData - 2)
              in
            {expr with
              pexp_attributes = filterReasonRawLiteral expr.pexp_attributes;
              pexp_desc = Pexp_constant (Pconst_string (stringData, None))
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
        | Ppat_open ({txt = longidentOpen}, pattern) ->
          let p = rewritePpatOpen longidentOpen pattern in
          default_mapper.pat mapper p
        | _ ->
          default_mapper.pat mapper p
      end;
      expr = (fun mapper expr ->
        match expr.pexp_desc with
        | Pexp_constant (Pconst_string (txt, None)) ->
          let raw = escapeStringContents txt in
          let s = Parsetree.Pconst_string (raw, None) in
          let expr = Ast_helper.Exp.constant
            ~attrs:expr.pexp_attributes
            ~loc:expr.pexp_loc s
          in
          expr
        | Pexp_constant (Pconst_string (txt, tag)) ->
          let s = Parsetree.Pconst_string ((escapeTemplateLiteral txt), tag) in
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
            [Asttypes.Nolabel, lhs; Nolabel, ({pexp_desc = Pexp_constant (Pconst_string (txt, None))} as stringExpr)]
          ) ->
          let ident = Ast_helper.Exp.ident ~loc:stringExpr.pexp_loc
            (Location.mkloc (Longident.Lident txt) stringExpr.pexp_loc)
          in
          Ast_helper.Exp.apply ~loc:expr.pexp_loc ~attrs:expr.pexp_attributes
            op [Asttypes.Nolabel, lhs; Nolabel, ident]
        | Pexp_apply (
            {pexp_desc = Pexp_ident {txt = Longident.Lident "@@"}},
            [Asttypes.Nolabel, callExpr; Nolabel, argExpr]
          ) ->
          Ast_helper.Exp.apply (mapper.expr mapper callExpr) [
            Asttypes.Nolabel, mapper.expr mapper argExpr
          ]
        | Pexp_apply (
            {pexp_desc = Pexp_ident {txt = Longident.Lident "@"}},
            [Nolabel, arg1; Nolabel, arg2]
          ) ->
          let listConcat = Longident.Ldot (Longident.Lident "List", "append") in
          Ast_helper.Exp.apply
            (Ast_helper.Exp.ident (Location.mknoloc listConcat))
            [Nolabel, mapper.expr mapper arg1; Nolabel, mapper.expr mapper arg2]
        | Pexp_match (
            condition,
            [
              {pc_lhs = {ppat_desc = Ppat_construct ({txt = Longident.Lident "true"}, None)}; pc_rhs = thenExpr };
              {pc_lhs = {ppat_desc = Ppat_construct ({txt = Longident.Lident "false"}, None)}; pc_rhs = elseExpr };
            ]
          ) ->
          let ternaryMarker = (Location.mknoloc "ns.ternary", Parsetree.PStr []) in
          Ast_helper.Exp.ifthenelse
            ~loc:expr.pexp_loc
            ~attrs:(ternaryMarker::expr.pexp_attributes)
            (default_mapper.expr mapper condition)
            (default_mapper.expr mapper thenExpr)
            (Some (default_mapper.expr mapper elseExpr))
        | _ -> default_mapper.expr mapper expr
      );
      structure_item = begin fun mapper structureItem ->
        match structureItem.pstr_desc with
        (* heuristic: if we have multiple type declarations, mark them recursive *)
        | Pstr_type (recFlag, typeDeclarations) ->
          let flag = match typeDeclarations with
          | [td] ->
            if looksLikeRecursiveTypeDeclaration td then Asttypes.Recursive
            else Asttypes.Nonrecursive
          | _ -> recFlag
          in
          {structureItem with pstr_desc = Pstr_type (
            flag,
            List.map (fun typeDeclaration ->
              default_mapper.type_declaration mapper typeDeclaration
            ) typeDeclarations
          )}
        | _ -> default_mapper.structure_item mapper structureItem
      end;
      signature_item = begin fun mapper signatureItem ->
        match signatureItem.psig_desc with
        (* heuristic: if we have multiple type declarations, mark them recursive *)
        | Psig_type (recFlag, typeDeclarations) ->
          let flag = match typeDeclarations with
          | [td] ->
            if looksLikeRecursiveTypeDeclaration td then Asttypes.Recursive
            else Asttypes.Nonrecursive
          | _ -> recFlag
          in
          {signatureItem with psig_desc = Psig_type (
            flag,
            List.map (fun typeDeclaration ->
              default_mapper.type_declaration mapper typeDeclaration
            ) typeDeclarations
          )}
        | _ -> default_mapper.signature_item mapper signatureItem
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
        let newPattern = Ast_helper.Pat.constraint_
          ~loc:{pat.ppat_loc with loc_end = typ.ptyp_loc.loc_end}
          pat typ in
        {vb with
          pvb_pat = newPattern;
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
        let newPattern = Ast_helper.Pat.constraint_
          ~loc:{pat.ppat_loc with loc_end = typ.ptyp_loc.loc_end}
          pat typ in
        {vb with
          pvb_pat = newPattern;
          pvb_expr = expr;
          pvb_attributes = default_mapper.attributes mapper vb.pvb_attributes}
      | _ -> default_mapper.value_binding mapper vb
      end;
    }

  let normalizeReasonArityStructure ~forPrinter s =
    let mapper = makeReasonArityMapper ~forPrinter in
    mapper.Ast_mapper.structure mapper s

  let normalizeReasonAritySignature ~forPrinter s =
    let mapper = makeReasonArityMapper ~forPrinter in
    mapper.Ast_mapper.signature mapper s

  let structure s = normalize.Ast_mapper.structure normalize s
  let signature s = normalize.Ast_mapper.signature normalize s

  let replaceStringLiteralStructure stringData structure =
    let mapper = stringLiteralMapper stringData in
    mapper.Ast_mapper.structure mapper structure

  let replaceStringLiteralSignature stringData signature =
    let mapper = stringLiteralMapper stringData in
    mapper.Ast_mapper.signature mapper signature
end

module OcamlParser = Parser

module Parser = struct
  type mode = ParseForTypeChecker | Default

  type regionStatus = Report | Silent

  type t = {
    mode: mode;
    mutable scanner: Scanner.t;
    mutable token: Token.t;
    mutable startPos: Lexing.position;
    mutable endPos: Lexing.position;
    mutable prevEndPos: Lexing.position;
    mutable breadcrumbs: (Grammar.t * Lexing.position) list;
    mutable errors: Reporting.parseError list;
    mutable diagnostics: Diagnostics.t list;
    mutable comments: Comment.t list;
    mutable regions: regionStatus ref list;
  }

  let err ?startPos ?endPos p error =
    let d = Diagnostics.make
      ~filename:p.scanner.filename
      ~startPos:(match startPos with | Some pos -> pos | None -> p.startPos)
      ~endPos:(match endPos with | Some pos -> pos | None -> p.endPos)
      error
    in
    try
      if (!(List.hd p.regions) = Report) then (
        p.diagnostics <- d::p.diagnostics;
        List.hd p.regions := Silent
      )
    with Failure _ -> ()

  let beginRegion p =
    p.regions <- ref Report :: p.regions
  let endRegion p =
    try p.regions <- List.tl p.regions with Failure _ -> ()

   (* Advance to the next non-comment token and store any encountered comment
    * in the parser's state. Every comment contains the end position of it's
    * previous token to facilite comment interleaving *)
   let rec next ?prevEndPos p =
     let prevEndPos = match prevEndPos with Some pos -> pos | None -> p.endPos in
     let (startPos, endPos, token) = Scanner.scan p.scanner in
     match token with
     | Comment c ->
       Comment.setPrevTokEndPos c p.endPos;
       p.comments <- c::p.comments;
       p.prevEndPos <- p.endPos;
       p.endPos <- endPos;
       next ~prevEndPos p
     | _ ->
       p.token <- token;
       (* p.prevEndPos <- prevEndPos; *)
       p.prevEndPos <- prevEndPos;
       p.startPos <- startPos;
       p.endPos <- endPos

  let checkProgress ~prevEndPos ~result p =
    if p.endPos == prevEndPos
    then None
    else Some result

  let make ?(mode=ParseForTypeChecker) src filename =
    let scanner = Scanner.make (Bytes.of_string src) filename in
    let parserState = {
      mode;
      scanner;
      token = Token.Eof;
      startPos = Lexing.dummy_pos;
      prevEndPos = Lexing.dummy_pos;
      endPos = Lexing.dummy_pos;
      breadcrumbs = [];
      errors = [];
      diagnostics = [];
      comments = [];
      regions = [ref Report];
    } in
    parserState.scanner.err <- (fun ~startPos ~endPos error ->
      let diagnostic = Diagnostics.make
        ~filename
        ~startPos
        ~endPos
        error
      in
      parserState.diagnostics <- diagnostic::parserState.diagnostics
    );
    next parserState;
    parserState

  let leaveBreadcrumb p circumstance =
    let crumb = (circumstance, p.startPos) in
    p.breadcrumbs <- crumb::p.breadcrumbs

  let eatBreadcrumb p =
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
      let error = Diagnostics.expected ?grammar p.prevEndPos token in
      err ~startPos:p.prevEndPos p error

  (* Don't use immutable copies here, it trashes certain heuristics
   * in the ocaml compiler, resulting in massive slowdowns of the parser *)
  let lookahead p callback =
    let err = p.scanner.err in
    let ch = p.scanner.ch in
    let offset = p.scanner.offset in
    let rdOffset = p.scanner.rdOffset in
    let lineOffset = p.scanner.lineOffset in
    let lnum = p.scanner.lnum in
    let mode = p.scanner.mode in
    let token = p.token in
    let startPos = p.startPos in
    let endPos = p.endPos in
    let prevEndPos = p.prevEndPos in
    let breadcrumbs = p.breadcrumbs in
    let errors = p.errors in
    let diagnostics = p.diagnostics in
    let comments = p.comments in

    let res = callback p in

    p.scanner.err <- err;
    p.scanner.ch <- ch;
    p.scanner.offset <- offset;
    p.scanner.rdOffset <- rdOffset;
    p.scanner.lineOffset <- lineOffset;
    p.scanner.lnum <- lnum;
    p.scanner.mode <- mode;
    p.token <- token;
    p.startPos <- startPos;
    p.endPos <- endPos;
    p.prevEndPos <- prevEndPos;
    p.breadcrumbs <- breadcrumbs;
    p.errors <- errors;
    p.diagnostics <- diagnostics;
    p.comments <- comments;

    res
end

module NapkinScript = struct
  let mkLoc startLoc endLoc = Location.{
    loc_start = startLoc;
    loc_end = endLoc;
    loc_ghost = false;
  }


  module Recover = struct
    type action = unit option (* None is abort, Some () is retry *)

    let defaultExpr () =
      let id = Location.mknoloc "napkinscript.exprhole" in
      Ast_helper.Exp.mk (Pexp_extension (id, PStr []))

    let defaultType () =
      let id = Location.mknoloc "napkinscript.typehole" in
      Ast_helper.Typ.extension (id, PStr [])

    let defaultPattern () =
      let id = Location.mknoloc "napkinscript.patternhole" in
      Ast_helper.Pat.extension (id, PStr [])
      (* Ast_helper.Pat.any  () *)

    let defaultModuleExpr () = Ast_helper.Mod.structure []
    let defaultModuleType () = Ast_helper.Mty.signature []

    let recoverEqualGreater p =
      Parser.expect EqualGreater p;
      match p.Parser.token with
      | MinusGreater -> Parser.next p
      | _ -> ()

    let shouldAbortListParse p =
      let rec check breadcrumbs =
        match breadcrumbs with
        | [] -> false
        | (grammar, _)::rest ->
          if Grammar.isPartOfList grammar p.Parser.token then
            true
          else
            check rest
      in
      check p.breadcrumbs
  end

  module ErrorMessages = struct
    let listPatternSpread = "List pattern matches only supports one `...` spread, at the end.
Explanation: a list spread at the tail is efficient, but a spread in the middle would create new list[s]; out of performance concern, our pattern matching currently guarantees to never create new intermediate data."

    let recordPatternSpread = "Record's `...` spread is not supported in pattern matches.
Explanation: you can't collect a subset of a record's field into its own record, since a record needs an explicit declaration and that subset wouldn't have one.
Solution: you need to pull out each field you want explicitly."

    let recordPatternUnderscore = "Record patterns only support one `_`, at the end."
    [@@live]

    let arrayPatternSpread = "Array's `...` spread is not supported in pattern matches.
Explanation: such spread would create a subarray; out of performance concern, our pattern matching currently guarantees to never create new intermediate data.
Solution: if it's to validate the first few elements, use a `when` clause + Array size check + `get` checks on the current pattern. If it's to obtain a subarray, use `Array.sub` or `Belt.Array.slice`."

    let arrayExprSpread = "Arrays can't use the `...` spread currently. Please use `concat` or other Array helpers."

    let recordExprSpread = "Records can only have one `...` spread, at the beginning.
Explanation: since records have a known, fixed shape, a spread like `{a, ...b}` wouldn't make sense, as `b` would override every field of `a` anyway."

    let listExprSpread =  "Lists can only have one `...` spread, and at the end.
Explanation: lists are singly-linked list, where a node contains a value and points to the next node. `list[a, ...bc]` efficiently creates a new item and links `bc` as its next nodes. `[...bc, a]` would be expensive, as it'd need to traverse `bc` and prepend each item to `a` one by one. We therefore disallow such syntax sugar.
Solution: directly use `concat`."

    let variantIdent = "A polymorphic variant (e.g. #id) must start with an alphabetical letter."
end


  let jsxAttr = (Location.mknoloc "JSX", Parsetree.PStr [])
  let uncurryAttr = (Location.mknoloc "bs", Parsetree.PStr [])
  let ternaryAttr = (Location.mknoloc "ns.ternary", Parsetree.PStr [])
  let makeBracesAttr loc = (Location.mkloc "ns.braces" loc, Parsetree.PStr [])

  type typDefOrExt =
    | TypeDef of {recFlag: Asttypes.rec_flag; types: Parsetree.type_declaration list}
    | TypeExt of Parsetree.type_extension

  type labelledParameter =
    | TermParameter of
        {uncurried: bool; attrs: Parsetree.attributes; label: Asttypes.arg_label; expr: Parsetree.expression option;
        pat: Parsetree.pattern; pos: Lexing.position}
    | TypeParameter of {uncurried: bool; attrs: Parsetree.attributes; locs: string Location.loc list; pos: Lexing.position}

  type recordPatternItem =
    | PatUnderscore
    | PatField of (Ast_helper.lid * Parsetree.pattern)

  type context =
    | OrdinaryExpr
    | TernaryTrueBranchExpr
    | WhenExpr

  let getClosingToken = function
    | Token.Lparen -> Token.Rparen
    | Lbrace -> Rbrace
    | Lbracket -> Rbracket
    | _ -> assert false

  let rec goToClosing closingToken state =
    match (state.Parser.token, closingToken) with
    | (Rparen, Token.Rparen) | (Rbrace, Rbrace) | (Rbracket, Rbracket) ->
      Parser.next state;
      ()
    | (Token.Lbracket | Lparen | Lbrace) as t, _ ->
      Parser.next state;
      goToClosing (getClosingToken t) state;
      goToClosing closingToken state
    | ((Rparen | Token.Rbrace | Rbracket | Eof), _)  ->
      () (* TODO: how do report errors here? *)
    | _ ->
      Parser.next state;
      goToClosing closingToken state

  (* Madness *)
  let isEs6ArrowExpression ~inTernary p =
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
        let prevEndPos = state.prevEndPos in
        Parser.next state;
        begin match state.token with
        | Rparen ->
          Parser.next state;
          begin match state.Parser.token with
          | Colon when not inTernary -> true
          | EqualGreater -> true
          | _ -> false
          end
        | Dot (* uncurried *) -> true
        | Tilde -> true
        | Backtick -> false (* (` always indicates the start of an expr, can't be es6 parameter *)
        | _ ->
          goToClosing Rparen state;
          begin match state.Parser.token with
          | EqualGreater -> true
          (* | Lbrace TODO: detect missing =>, is this possible? *)
          | Colon when not inTernary -> true
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
            | EqualGreater when state.startPos.pos_lnum == prevEndPos.pos_lnum -> true
            | _ -> false
            end
          end
        end
      | _ -> false)


  let isEs6ArrowFunctor p =
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
          goToClosing Rparen state;
          begin match state.Parser.token with
          | EqualGreater | Lbrace -> true
          | Colon -> true
          | _ -> false
          end
        end
      | _ -> false
    )

  let isEs6ArrowType p =
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
          goToClosing Rparen state;
          begin match state.Parser.token with
          | EqualGreater  -> true
          | _ -> false
          end
        end
      | Tilde -> true
      | _ -> false
    )

  let buildLongident words = match List.rev words with
    | [] -> assert false
    | hd::tl -> List.fold_left (fun p s -> Longident.Ldot (p, s)) (Lident hd) tl

  let makeInfixOperator p token startPos endPos =
    let stringifiedToken =
      if token = Token.MinusGreater then "|."
      else if token = Token.PlusPlus then "^"
      else if token = Token.BangEqual then "<>"
      else if token = Token.BangEqualEqual then "!="
      else if token = Token.Equal then (
        (* TODO: could have a totally different meaning like x->fooSet(y)*)
        Parser.err ~startPos ~endPos p (
          Diagnostics.message "Did you mean `==` here?"
        );
        "="
      ) else if token = Token.EqualEqual then "="
      else if token = Token.EqualEqualEqual then "=="
      else Token.toString token
    in
    let loc = mkLoc startPos endPos in
    let operator = Location.mkloc
      (Longident.Lident stringifiedToken) loc
    in
    Ast_helper.Exp.ident ~loc operator

  let negateString s =
    if String.length s > 0 && (s.[0] [@doesNotRaise]) = '-'
    then (String.sub [@doesNotRaise]) s 1 (String.length s - 1)
    else "-" ^ s

  let makeUnaryExpr startPos tokenEnd token operand =
    match token, operand.Parsetree.pexp_desc with
    | (Token.Plus | PlusDot), Pexp_constant((Pconst_integer _ | Pconst_float _)) ->
      operand
    | Minus, Pexp_constant(Pconst_integer (n,m)) ->
      {operand with pexp_desc = Pexp_constant(Pconst_integer (negateString n,m))}
    | (Minus | MinusDot), Pexp_constant(Pconst_float (n,m)) ->
      {operand with pexp_desc = Pexp_constant(Pconst_float (negateString n,m))}
    | (Token.Plus | PlusDot | Minus | MinusDot ), _ ->
      let tokenLoc = mkLoc startPos tokenEnd in
      let operator = "~" ^ Token.toString token in
      Ast_helper.Exp.apply
        ~loc:(mkLoc startPos operand.Parsetree.pexp_loc.loc_end)
        (Ast_helper.Exp.ident ~loc:tokenLoc
          (Location.mkloc (Longident.Lident operator) tokenLoc))
        [Nolabel, operand]
    | Token.Bang, _ ->
      let tokenLoc = mkLoc startPos tokenEnd in
      Ast_helper.Exp.apply
        ~loc:(mkLoc startPos operand.Parsetree.pexp_loc.loc_end)
        (Ast_helper.Exp.ident ~loc:tokenLoc
          (Location.mkloc (Longident.Lident "not") tokenLoc))
        [Nolabel, operand]
    | _ ->
      operand

  let makeListExpression loc seq extOpt =
    let rec handleSeq = function
      | [] ->
        begin match extOpt with
        | Some ext -> ext
        | None ->
          let loc = {loc with Location.loc_ghost = true} in
          let nil = Location.mkloc (Longident.Lident "[]") loc in
          Ast_helper.Exp.construct ~loc nil None
        end
      | e1 :: el ->
        let exp_el = handleSeq el in
        let loc = mkLoc
          e1.Parsetree.pexp_loc.Location.loc_start
          exp_el.pexp_loc.loc_end
        in
        let arg = Ast_helper.Exp.tuple ~loc [e1; exp_el] in
        Ast_helper.Exp.construct ~loc
          (Location.mkloc (Longident.Lident "::") loc)
          (Some arg)
    in
    let expr = handleSeq seq in
    {expr with pexp_loc = loc}

  let makeListPattern loc seq ext_opt =
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
          mkLoc p1.Parsetree.ppat_loc.loc_start pat_pl.ppat_loc.loc_end in
        let arg = Ast_helper.Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
        Ast_helper.Pat.mk ~loc (Ppat_construct(Location.mkloc (Longident.Lident "::") loc, Some arg))
    in
    handle_seq seq


  (* {"foo": bar} -> Js.t({. foo: bar})
   * {.. "foo": bar} -> Js.t({.. foo: bar})
   * {..} -> Js.t({..}) *)
  let makeBsObjType ~attrs ~loc ~closed rows =
    let obj = Ast_helper.Typ.object_ ~loc rows closed in
    let jsDotTCtor =
      Location.mkloc (Longident.Ldot (Longident.Lident "Js", "t")) loc
    in
    Ast_helper.Typ.constr ~loc ~attrs jsDotTCtor [obj]

  (* TODO: diagnostic reporting *)
  let lidentOfPath longident =
    match Longident.flatten longident |> List.rev with
    | [] -> ""
    | ident::_ -> ident

  let makeNewtypes ~attrs ~loc newtypes exp =
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
  let wrapTypeAnnotation ~loc newtypes core_type body =
    let exp = makeNewtypes ~attrs:[] ~loc newtypes
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
  let processUnderscoreApplication args =
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

  let rec parseLident p =
    let recoverLident p =
      if (
        Token.isKeyword p.Parser.token &&
        p.Parser.prevEndPos.pos_lnum == p.startPos.pos_lnum
      )
      then (
        Parser.err p (Diagnostics.lident p.Parser.token);
        Parser.next p;
        None
      ) else (
        let rec loop p =
          if not (Recover.shouldAbortListParse p)
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
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      (ident, loc)
    | List ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      ("list", loc)
    | _ ->
      begin match recoverLident p with
      | Some () ->
        parseLident p
      | None ->
        ("_", mkLoc startPos p.prevEndPos)
      end

  let parseIdent ~msg ~startPos p =
    match p.Parser.token with
    | Lident ident
    | Uident ident ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      (ident, loc)
    | List ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      ("list", loc)
    | _token ->
      Parser.err p (Diagnostics.message msg);
      Parser.next p;
      ("_", mkLoc startPos p.prevEndPos)

  let parseHashIdent ~startPos p =
    Parser.expect Hash p;
    parseIdent ~startPos ~msg:ErrorMessages.variantIdent p

  (* Ldot (Ldot (Lident "Foo", "Bar"), "baz") *)
  let parseValuePath p =
    let startPos = p.Parser.startPos in
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
    Location.mkloc ident (mkLoc startPos p.prevEndPos)

 let parseValuePathTail p startPos ident =
    let rec loop p path =
      match p.Parser.token with
      | Lident ident ->
        Parser.next p;
        Location.mkloc (Longident.Ldot(path, ident)) (mkLoc startPos p.prevEndPos)
      | List ->
        Parser.next p;
        Location.mkloc (Longident.Ldot(path, "list")) (mkLoc startPos p.prevEndPos)
      | Uident ident ->
        Parser.next p;
        Parser.expect Dot p;
        loop p (Longident.Ldot (path, ident))
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        Location.mknoloc path
    in
    loop p ident

  let parseModuleLongIdentTail ~lowercase p startPos ident =
    let rec loop p acc =
      match p.Parser.token with
      | List when lowercase ->
        Parser.next p;
        let lident = (Longident.Ldot (acc, "list")) in
        Location.mkloc lident (mkLoc startPos p.prevEndPos)
      | Lident ident when lowercase ->
        Parser.next p;
        let lident = (Longident.Ldot (acc, ident)) in
        Location.mkloc lident (mkLoc startPos p.prevEndPos)
      | Uident ident ->
        Parser.next p;
        let endPos = p.prevEndPos in
        let lident = (Longident.Ldot (acc, ident)) in
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          loop p lident
        | _ -> Location.mkloc lident (mkLoc startPos endPos)
        end
      | t ->
        Parser.err p (Diagnostics.uident t);
        Location.mkloc acc (mkLoc startPos p.prevEndPos)
    in
    loop p ident

  (* Parses module identifiers:
       Foo
       Foo.Bar *)
  let parseModuleLongIdent ~lowercase p =
    (* Parser.leaveBreadcrumb p Reporting.ModuleLongIdent; *)
    let startPos = p.Parser.startPos in
    let moduleIdent = match p.Parser.token with
    | List when lowercase ->
      let loc = mkLoc startPos p.endPos in
      Parser.next p;
      Location.mkloc (Longident.Lident "list") loc
    | Lident ident when lowercase ->
      let loc = mkLoc startPos p.endPos in
      let lident = Longident.Lident ident in
      Parser.next p;
      Location.mkloc lident loc
    | Uident ident ->
      let lident = Longident.Lident ident in
      let endPos = p.endPos in
      Parser.next p;
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        parseModuleLongIdentTail ~lowercase p startPos lident
      | _ -> Location.mkloc lident (mkLoc startPos endPos)
      end
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mkloc (Longident.Lident "_") (mkLoc startPos p.prevEndPos)
    in
    (* Parser.eatBreadcrumb p; *)
    moduleIdent

  (* `window.location` or `Math` or `Foo.Bar` *)
  let parseIdentPath p =
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

  let verifyJsxOpeningClosingName p nameExpr =
    let closing = match p.Parser.token with
    | Lident lident -> Parser.next p; Longident.Lident lident
    | Uident _ ->
      (parseModuleLongIdent ~lowercase:false p).txt
    | _ -> Longident.Lident ""
    in
    match nameExpr.Parsetree.pexp_desc with
    | Pexp_ident openingIdent ->
      let opening =
        let withoutCreateElement =
          Longident.flatten openingIdent.txt
          |> List.filter (fun s -> s <> "createElement")
        in
        match (Longident.unflatten withoutCreateElement) with
        | Some li -> li
        | None -> Longident.Lident ""
      in
      opening = closing
    | _ -> assert false

  let string_of_pexp_ident nameExpr =
    match nameExpr.Parsetree.pexp_desc with
    | Pexp_ident openingIdent ->
      Longident.flatten openingIdent.txt
      |> List.filter (fun s -> s <> "createElement")
      |> String.concat "."
    | _ -> ""

  (* open-def ::=
   *   | open module-path
   *   | open! module-path *)
  let parseOpenDescription ~attrs p =
    Parser.leaveBreadcrumb p Grammar.OpenDescription;
    let startPos = p.Parser.startPos in
    Parser.expect Open p;
    let override = if Parser.optional p Token.Bang then
      Asttypes.Override
    else
      Asttypes.Fresh
    in
    let modident = parseModuleLongIdent ~lowercase:false p in
    let loc = mkLoc startPos p.prevEndPos in
    Parser.eatBreadcrumb p;
    Ast_helper.Opn.mk ~loc ~attrs ~override modident

  let hexValue x =
    match x with
    | '0' .. '9' ->
      (Char.code x) - 48
    | 'A' .. 'Z' ->
      (Char.code x) - 55
    | 'a' .. 'z' ->
      (Char.code x) - 97
    | _ -> 16

  let parseStringLiteral s =
    let len = String.length s in
    let b = Buffer.create (String.length s) in

    let rec loop i =
      if i = len then
        ()
      else
        let c = String.unsafe_get s i in
        match c with
        | '\\' as c ->
          let nextIx = i + 1 in
          if nextIx < len then
            let nextChar = String.unsafe_get s nextIx in
            begin match nextChar with
            | 'n' ->
              Buffer.add_char b '\010';
              loop (nextIx + 1)
            | 'r' ->
              Buffer.add_char b '\013';
              loop (nextIx + 1)
            | 'b' ->
              Buffer.add_char b '\008';
              loop (nextIx + 1)
            | 't' ->
              Buffer.add_char b '\009';
              loop (nextIx + 1)
            | '\\' as c ->
              Buffer.add_char b c;
              loop (nextIx + 1)
            | ' ' as c ->
                Buffer.add_char b c;
              loop (nextIx + 1)
            | '\'' as c ->
                Buffer.add_char b c;
              loop (nextIx + 1)
            | '\"' as c ->
              Buffer.add_char b c;
              loop (nextIx + 1)
            | '0' .. '9' ->
              if nextIx + 2 < len then
                let c0 = nextChar in
                let c1 = (String.unsafe_get s (nextIx + 1)) in
                let c2 = (String.unsafe_get s (nextIx + 2)) in
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
                  loop (nextIx + 3)
                ) else (
                  Buffer.add_char b (Char.unsafe_chr c);
                  loop (nextIx + 3)
                )
              else (
                Buffer.add_char b '\\';
                Buffer.add_char b nextChar;
                loop (nextIx + 1)
              )
            | 'o' ->
              if nextIx + 3 < len then
                let c0 = (String.unsafe_get s (nextIx + 1)) in
                let c1 = (String.unsafe_get s (nextIx + 2)) in
                let c2 = (String.unsafe_get s (nextIx + 3)) in
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
                  loop (nextIx + 4)
                ) else (
                  Buffer.add_char b (Char.unsafe_chr c);
                  loop (nextIx + 4)
                )
              else (
                Buffer.add_char b '\\';
                Buffer.add_char b nextChar;
                loop (nextIx + 1)
              )
            | 'x' as c ->
              if nextIx + 2 < len then
                let c0 = (String.unsafe_get s (nextIx + 1)) in
                let c1 = (String.unsafe_get s (nextIx + 2)) in
                let c = (16 * (hexValue c0)) + (hexValue c1) in
                if (c < 0 || c > 255) then (
                  Buffer.add_char b '\\';
                  Buffer.add_char b 'x';
                  Buffer.add_char b c0;
                  Buffer.add_char b c1;
                  loop (nextIx + 3)
                ) else (
                  Buffer.add_char b (Char.unsafe_chr c);
                  loop (nextIx + 3)
                )
              else (
                Buffer.add_char b '\\';
                Buffer.add_char b c;
                loop (nextIx + 2)
              )
            | _ ->
              Buffer.add_char b c;
              Buffer.add_char b nextChar;
              loop (nextIx + 1)
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

  let parseTemplateStringLiteral s =
    let len = String.length s in
    let b = Buffer.create len in

    let rec loop i =
      if i < len then
        let c = String.unsafe_get s i in
        match c with
        | '\\' as c ->
          if i + 1 < len then
            let nextChar = String.unsafe_get s (i + 1) in
            begin match nextChar with
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
  let parseConstant p =
    let isNegative = match p.Parser.token with
    | Token.Minus -> Parser.next p; true
    | Plus -> Parser.next p; false
    | _ -> false
    in
    let constant = match p.Parser.token with
    | Int {i; suffix} ->
      let intTxt = if isNegative then "-" ^ i else i in
      Parsetree.Pconst_integer (intTxt, suffix)
    | Float {f; suffix} ->
      let floatTxt = if isNegative then "-" ^ f else f in
      Parsetree.Pconst_float (floatTxt, suffix)
    | String s ->
      let txt = if p.mode = ParseForTypeChecker then
        parseStringLiteral s
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

  let parseCommaDelimitedRegion p ~grammar ~closing ~f =
    Parser.leaveBreadcrumb p grammar;
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
          if not (p.token = Eof || p.token = closing || Recover.shouldAbortListParse p) then
            Parser.expect Comma p;
          if p.token = Semicolon then Parser.next p;
          loop (node::nodes)
        end
      | None ->
        if p.token = Eof || p.token = closing || Recover.shouldAbortListParse p then
          List.rev nodes
        else (
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        );
    in
    let nodes = loop [] in
    Parser.eatBreadcrumb p;
    nodes

  let parseCommaDelimitedReversedList p ~grammar ~closing ~f =
    Parser.leaveBreadcrumb p grammar;
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
          if not (p.token = Eof || p.token = closing || Recover.shouldAbortListParse p) then
            Parser.expect Comma p;
          if p.token = Semicolon then Parser.next p;
          loop (node::nodes)
        end
      | None ->
        if p.token = Eof || p.token = closing || Recover.shouldAbortListParse p then
          nodes
        else (
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        );
    in
    let nodes = loop [] in
    Parser.eatBreadcrumb p;
    nodes

  let parseDelimitedRegion p ~grammar ~closing ~f =
    Parser.leaveBreadcrumb p grammar;
    let rec loop nodes =
      match f p with
      | Some node ->
        loop (node::nodes)
      | None ->
        if (
          p.Parser.token = Token.Eof ||
          p.token = closing ||
          Recover.shouldAbortListParse p
        ) then
          List.rev nodes
        else (
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        )
      in
      let nodes = loop [] in
      Parser.eatBreadcrumb p;
      nodes

  let parseRegion p ~grammar ~f =
    Parser.leaveBreadcrumb p grammar;
    let rec loop nodes =
      match f p with
      | Some node ->
        loop (node::nodes)
      | None ->
        if p.Parser.token = Token.Eof || Recover.shouldAbortListParse p then
          List.rev nodes
        else (
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        )
    in
    let nodes = loop [] in
    Parser.eatBreadcrumb p;
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
  let rec parsePattern ?(alias=true) ?(or_=true) p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let pat = match p.Parser.token with
    | (True | False) as token ->
      let endPos = p.endPos in
      Parser.next p;
      let loc = mkLoc startPos endPos in
      Ast_helper.Pat.construct ~loc
        (Location.mkloc (Longident.Lident (Token.toString token)) loc) None
    | Int _ | String _ | Float _ | Character _ | Minus | Plus ->
      let c = parseConstant p in
       begin match p.token with
        | DotDot ->
          Parser.next p;
          let c2 = parseConstant p in
          Ast_helper.Pat.interval ~loc:(mkLoc startPos p.prevEndPos) c c2
        | _ ->
          Ast_helper.Pat.constant ~loc:(mkLoc startPos p.prevEndPos) c
      end
    | Lparen ->
      Parser.next p;
      begin match p.token with
      | Rparen ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        let lid = Location.mkloc (Longident.Lident "()") loc in
        Ast_helper.Pat.construct ~loc lid None
      | _ ->
        let pat = parseConstrainedPattern p in
        begin match p.token with
        | Comma ->
          Parser.next p;
          parseTuplePattern ~attrs ~first:pat ~startPos p
        | _ ->
          Parser.expect Rparen p;
          let loc = mkLoc startPos p.prevEndPos in
          {pat with ppat_loc = loc}
        end
      end
    | Lbracket ->
      parseArrayPattern ~attrs p
    | Lbrace ->
      parseRecordPattern ~attrs p
    | Underscore ->
      let endPos = p.endPos in
      let loc = mkLoc startPos endPos in
      Parser.next p;
      Ast_helper.Pat.any ~loc ~attrs ()
    | Lident ident ->
      let endPos = p.endPos in
      let loc = mkLoc startPos endPos in
      Parser.next p;
      Ast_helper.Pat.var ~loc ~attrs (Location.mkloc ident loc)
    | Uident _ ->
      let constr = parseModuleLongIdent ~lowercase:false p in
      begin match p.Parser.token with
      | Lparen ->
        parseConstructorPatternArgs p constr startPos attrs
      | _ ->
        Ast_helper.Pat.construct ~loc:constr.loc ~attrs constr None
      end
    | Hash ->
      let (ident, loc) = parseHashIdent ~startPos p in
      begin match p.Parser.token with
      | Lparen ->
        parseVariantPatternArgs p ident startPos attrs
      | _ ->
        Ast_helper.Pat.variant ~loc ~attrs ident None
      end
    | HashHash ->
      Parser.next p;
      let ident = parseValuePath p in
      let loc = mkLoc startPos ident.loc.loc_end in
      Ast_helper.Pat.type_ ~loc ~attrs ident
    | Exception ->
      Parser.next p;
      let pat = parsePattern ~alias:false ~or_:false p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Pat.exception_ ~loc ~attrs pat
    | Lazy ->
      Parser.next p;
      let pat = parsePattern ~alias:false ~or_:false p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Pat.lazy_ ~loc ~attrs pat
    | List ->
      Parser.next p;
      begin match p.token with
      | Lbracket ->
        parseListPattern ~startPos ~attrs p
      | _ ->
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Pat.var ~loc ~attrs (Location.mkloc "list" loc)
      end
    | Module ->
      parseModulePattern ~attrs p
    | Percent ->
      let extension = parseExtension p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Pat.extension ~loc ~attrs extension
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      begin match skipTokensAndMaybeRetry p ~isStartOfGrammar:Grammar.isAtomicPatternStart with
      | None ->
        Recover.defaultPattern()
      | Some () ->
        parsePattern p
      end
    in
    let pat = if alias then parseAliasPattern ~attrs pat p else pat in
    if or_ then parseOrPattern pat p else pat

  and skipTokensAndMaybeRetry p ~isStartOfGrammar =
    if Token.isKeyword p.Parser.token
        && p.Parser.prevEndPos.pos_lnum == p.startPos.pos_lnum
    then (
      Parser.next p;
      None
    ) else (
      if Recover.shouldAbortListParse p then
        begin
          if isStartOfGrammar p.Parser.token then
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
            if not (Recover.shouldAbortListParse p)
            then begin
              Parser.next p;
              loop p
            end in
          loop p;
          if isStartOfGrammar p.Parser.token then
            Some ()
          else
            None
        end
    )

  (* alias ::= pattern as lident *)
  and parseAliasPattern ~attrs pattern p =
    match p.Parser.token with
    | As ->
      Parser.next p;
      let (name, loc) = parseLident p in
      let name = Location.mkloc name loc in
      Ast_helper.Pat.alias
        ~loc:({pattern.ppat_loc with loc_end = p.prevEndPos})
        ~attrs
         pattern
         name
    | _ -> pattern

  (* or ::= pattern | pattern
   * precedence: Red | Blue | Green is interpreted as (Red | Blue) | Green *)
  and parseOrPattern pattern1 p =
    let rec loop pattern1 =
      match p.Parser.token with
      | Bar ->
        Parser.next p;
        let pattern2 = parsePattern ~or_:false p in
        let loc = { pattern1.Parsetree.ppat_loc with
          loc_end = pattern2.ppat_loc.loc_end
        } in
        loop (Ast_helper.Pat.or_ ~loc pattern1 pattern2)
      | _ -> pattern1
    in
    loop pattern1

  and parseNonSpreadPattern ~msg p =
    let () = match p.Parser.token with
    | DotDotDot ->
      Parser.err p (Diagnostics.message msg);
      Parser.next p;
    | _ -> ()
    in
    match p.Parser.token with
    | token when Grammar.isPatternStart token ->
      let pat = parsePattern p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let typ = parseTypExpr p in
        let loc = mkLoc pat.ppat_loc.loc_start typ.Parsetree.ptyp_loc.loc_end in
        Some (Ast_helper.Pat.constraint_ ~loc pat typ)
      | _ -> Some pat
      end
    | _ -> None

  and parseConstrainedPattern p =
    let pat = parsePattern p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let typ = parseTypExpr p in
      let loc = mkLoc pat.ppat_loc.loc_start typ.Parsetree.ptyp_loc.loc_end in
      Ast_helper.Pat.constraint_ ~loc pat typ
    | _ -> pat

  and parseConstrainedPatternRegion p =
    match p.Parser.token with
    | token when Grammar.isPatternStart token ->
      Some (parseConstrainedPattern p)
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
  and parseRecordPatternField p =
    let startPos = p.Parser.startPos in
    let label = parseValuePath p in
    let pattern = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parsePattern p
    | _ ->
      Ast_helper.Pat.var
        ~loc:label.loc
        (Location.mkloc (Longident.last label.txt) label.loc)
    in
		match p.token with
		| As ->
			Parser.next p;
      let (name, loc) = parseLident p in
      let name = Location.mkloc name loc in
      let aliasPattern = Ast_helper.Pat.alias
        ~loc:(mkLoc startPos p.prevEndPos)
        pattern
        name
      in
      (Location.mkloc label.txt (mkLoc startPos aliasPattern.ppat_loc.loc_end), aliasPattern)
		| _ ->
      (label, pattern)

   (* TODO: there are better representations than PatField|Underscore ? *)
  and parseRecordPatternItem p =
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      Some (true, PatField (parseRecordPatternField p))
    | Uident _ | Lident _ ->
      Some (false, PatField (parseRecordPatternField p))
    | Underscore ->
      Parser.next p;
      Some (false, PatUnderscore)
    | _ ->
      None

  and parseRecordPattern ~attrs p =
    let startPos = p.startPos in
    Parser.expect Lbrace p;
    let rawFields =
      parseCommaDelimitedReversedList p
       ~grammar:PatternRecord
       ~closing:Rbrace
       ~f:parseRecordPatternItem
    in
    Parser.expect Rbrace p;
    let (fields, closedFlag) =
      let (rawFields, flag) = match rawFields with
      | (_hasSpread, PatUnderscore)::rest ->
        (rest, Asttypes.Open)
      | rawFields ->
        (rawFields, Asttypes.Closed)
      in
      List.fold_left (fun (fields, flag) curr ->
        let (hasSpread, field) = curr in
        match field with
        | PatField field ->
          if hasSpread then (
            let (_, pattern) = field in
            Parser.err ~startPos:pattern.Parsetree.ppat_loc.loc_start p (Diagnostics.message ErrorMessages.recordPatternSpread)
          );
          (field::fields, flag)
        | PatUnderscore ->
          (fields, flag)
      ) ([], flag) rawFields
    in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.record ~loc ~attrs fields closedFlag

  and parseTuplePattern ~attrs ~first ~startPos p =
    let patterns =
      parseCommaDelimitedRegion p
        ~grammar:Grammar.PatternList
        ~closing:Rparen
        ~f:parseConstrainedPatternRegion
    in
    Parser.expect Rparen p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.tuple ~loc ~attrs (first::patterns)

  and parsePatternRegion p =
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      Some (true, parseConstrainedPattern p)
    | token when Grammar.isPatternStart token ->
      Some (false, parseConstrainedPattern p)
    | _ -> None

  and parseModulePattern ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect Module p;
    Parser.expect Lparen p;
    let uident = match p.token with
    | Uident uident ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc uident loc
    | _ -> (* TODO: error recovery *)
      Location.mknoloc "_"
    in
    begin match p.token with
    | Colon ->
      let colonStart = p.Parser.startPos in
      Parser.next p;
      let packageTypAttrs = parseAttributes p in
      let packageType = parsePackageType ~startPos:colonStart ~attrs:packageTypAttrs p in
      Parser.expect Rparen p;
      let loc = mkLoc startPos p.prevEndPos in
      let unpack = Ast_helper.Pat.unpack ~loc:uident.loc uident in
      Ast_helper.Pat.constraint_
        ~loc
        ~attrs
        unpack
        packageType
    | _ ->
      Parser.expect Rparen p;
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Pat.unpack ~loc ~attrs uident
    end

  and parseListPattern ~startPos ~attrs p =
    Parser.expect Lbracket p;
    let listPatterns =
      parseCommaDelimitedReversedList p
        ~grammar:Grammar.PatternOcamlList
        ~closing:Rbracket
        ~f:parsePatternRegion
    in
    Parser.expect Rbracket p;
    let loc = mkLoc startPos p.prevEndPos in
    let filterSpread (hasSpread, pattern) =
      if hasSpread then (
        Parser.err
          ~startPos:pattern.Parsetree.ppat_loc.loc_start
          p
          (Diagnostics.message ErrorMessages.listPatternSpread);
        pattern
      ) else
        pattern
    in
    match listPatterns with
    | (true, pattern)::patterns ->
      let patterns = patterns |> List.map filterSpread |> List.rev in
      let pat = makeListPattern loc patterns (Some pattern) in
      {pat with ppat_loc = loc; ppat_attributes = attrs;}
    | patterns ->
      let patterns = patterns |> List.map filterSpread |> List.rev in
      let pat = makeListPattern loc patterns None in
      {pat with ppat_loc = loc; ppat_attributes = attrs;}

  and parseArrayPattern ~attrs p =
    let startPos = p.startPos in
    Parser.expect Lbracket p;
    let patterns =
      parseCommaDelimitedRegion
        p
        ~grammar:Grammar.PatternList
        ~closing:Rbracket
        ~f:(parseNonSpreadPattern ~msg:ErrorMessages.arrayPatternSpread)
    in
    Parser.expect Rbracket p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.array ~loc ~attrs patterns

  and parseConstructorPatternArgs p constr startPos attrs =
    let lparen = p.startPos in
    Parser.expect Lparen p;
    let args = parseCommaDelimitedRegion
      p ~grammar:Grammar.PatternList ~closing:Rparen ~f:parseConstrainedPatternRegion
    in
    Parser.expect Rparen p;
    let args = match args with
    | [] ->
      let loc = mkLoc lparen p.prevEndPos in
      Some (
        Ast_helper.Pat.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
      )
    | [{ppat_desc = Ppat_tuple _} as pat] as patterns ->
      if p.mode = ParseForTypeChecker then
        (* Some(1, 2) for type-checker *)
        Some pat
      else
      (* Some((1, 2)) for printer *)
        Some (Ast_helper.Pat.tuple ~loc:(mkLoc lparen p.endPos) patterns)
    | [pattern] -> Some pattern
    | patterns ->
      Some (Ast_helper.Pat.tuple ~loc:(mkLoc lparen p.endPos) patterns)
    in
    Ast_helper.Pat.construct ~loc:(mkLoc startPos p.prevEndPos) ~attrs constr args

  and parseVariantPatternArgs p ident startPos attrs =
    let lparen = p.startPos in
    Parser.expect Lparen p;
    let patterns =
      parseCommaDelimitedRegion
        p ~grammar:Grammar.PatternList ~closing:Rparen ~f:parseConstrainedPatternRegion in
    let args =
      match patterns with
      | [{ppat_desc = Ppat_tuple _} as pat] as patterns ->
        if p.mode = ParseForTypeChecker then
          (* #ident(1, 2) for type-checker *)
          Some pat
        else
          (* #ident((1, 2)) for printer *)
          Some (Ast_helper.Pat.tuple ~loc:(mkLoc lparen p.endPos) patterns)
      | [pattern] -> Some pattern
      | patterns ->
        Some (Ast_helper.Pat.tuple ~loc:(mkLoc lparen p.endPos) patterns)
    in
    Parser.expect Rparen p;
    Ast_helper.Pat.variant ~loc:(mkLoc startPos p.prevEndPos) ~attrs ident args

  and parseExpr ?(context=OrdinaryExpr) p =
    let expr = parseOperandExpr ~context p in
    let expr = parseBinaryExpr ~context ~a:expr p 1 in
    parseTernaryExpr expr p

  (* expr ? expr : expr *)
  and parseTernaryExpr leftOperand p =
    match p.Parser.token with
    | Question ->
      Parser.leaveBreadcrumb p Grammar.Ternary;
      Parser.next p;
      let trueBranch = parseExpr ~context:TernaryTrueBranchExpr p in
      Parser.expect Colon p;
      let falseBranch = parseExpr p in
      Parser.eatBreadcrumb p;
      let loc = {leftOperand.Parsetree.pexp_loc with
        loc_start = leftOperand.pexp_loc.loc_start;
        loc_end = falseBranch.Parsetree.pexp_loc.loc_end;
      } in
      Ast_helper.Exp.ifthenelse
        ~attrs:[ternaryAttr] ~loc
        leftOperand trueBranch (Some falseBranch)
    | _ ->
      leftOperand

  and parseEs6ArrowExpression ?parameters p =
    let startPos = p.Parser.startPos in
    Parser.leaveBreadcrumb p Grammar.Es6ArrowExpr;
    let parameters = match parameters with
    | Some params -> params
    | None -> parseParameters p
    in
    let returnType = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parseTypExpr ~es6Arrow:false p)
    | _ ->
      None
    in
    Parser.expect EqualGreater p;
    let body =
      let expr = parseExpr p in
      match returnType with
      | Some typ ->
        Ast_helper.Exp.constraint_
          ~loc:(mkLoc expr.pexp_loc.loc_start typ.Parsetree.ptyp_loc.loc_end) expr typ
      | None -> expr
    in
    Parser.eatBreadcrumb p;
    let endPos = p.prevEndPos in
    let arrowExpr =
      List.fold_right (fun parameter expr ->
        match parameter with
        | TermParameter {uncurried; attrs; label = lbl; expr = defaultExpr; pat; pos = startPos} ->
          let attrs = if uncurried then uncurryAttr::attrs else attrs in
          Ast_helper.Exp.fun_ ~loc:(mkLoc startPos endPos) ~attrs lbl defaultExpr pat expr
        | TypeParameter {uncurried; attrs; locs = newtypes; pos = startPos} ->
          let attrs = if uncurried then uncurryAttr::attrs else attrs in
          makeNewtypes ~attrs ~loc:(mkLoc startPos endPos) newtypes expr
      ) parameters body
    in
    {arrowExpr with pexp_loc = {arrowExpr.pexp_loc with loc_start = startPos}}

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
  and parseParameter p =
    if (
      p.Parser.token = Token.Typ ||
      p.token = Tilde ||
      p.token = Dot ||
      Grammar.isPatternStart p.token
    ) then (
      let startPos = p.Parser.startPos in
      let uncurried = Parser.optional p Token.Dot in
      (* two scenarios:
       *   attrs ~lbl ...
       *   attrs pattern
       * Attributes before a labelled arg, indicate that it's on the whole arrow expr
       * Otherwise it's part of the pattern
       *  *)
      let attrs = parseAttributes p in
      if p.Parser.token = Typ then (
        Parser.next p;
        let lidents = parseLidentList p in
        Some (TypeParameter {uncurried; attrs; locs = lidents; pos = startPos})
      ) else (
      let (attrs, lbl, pat) = match p.Parser.token with
      | Tilde ->
        Parser.next p;
        let (lblName, loc) = parseLident p in
        let propLocAttr = (Location.mkloc "ns.namedArgLoc" loc, Parsetree.PStr []) in
        begin match p.Parser.token with
        | Comma | Equal | Rparen ->
          let loc = mkLoc startPos p.prevEndPos in
          (
            attrs,
            Asttypes.Labelled lblName,
            Ast_helper.Pat.var ~attrs:[propLocAttr] ~loc (Location.mkloc lblName loc)
          )
        | Colon ->
          let lblEnd = p.prevEndPos in
          Parser.next p;
          let typ = parseTypExpr p in
          let loc = mkLoc startPos lblEnd in
          let pat =
            let pat = Ast_helper.Pat.var ~loc (Location.mkloc lblName loc) in
            let loc = mkLoc startPos p.prevEndPos in
            Ast_helper.Pat.constraint_ ~attrs:[propLocAttr] ~loc pat typ in
          (attrs, Asttypes.Labelled lblName, pat)
        | As ->
          Parser.next p;
          let pat =
            let pat = parseConstrainedPattern p in
            {pat with ppat_attributes = propLocAttr::pat.ppat_attributes}
          in
          (attrs, Asttypes.Labelled lblName, pat)
        | t ->
          Parser.err p (Diagnostics.unexpected t p.breadcrumbs);
          let loc = mkLoc startPos p.prevEndPos in
          (
            attrs,
            Asttypes.Labelled lblName,
            Ast_helper.Pat.var ~loc (Location.mkloc lblName loc)
          )
        end
      | _ ->
        let pattern = parseConstrainedPattern p in
        let attrs = List.concat [attrs; pattern.ppat_attributes] in
        ([], Asttypes.Nolabel, {pattern with ppat_attributes = attrs})
      in
      match p.Parser.token with
      | Equal ->
        Parser.next p;
        let lbl = match lbl with
        | Asttypes.Labelled lblName -> Asttypes.Optional lblName
        | Asttypes.Optional _ as lbl -> lbl
        | Asttypes.Nolabel -> Asttypes.Nolabel
        in
        begin match p.Parser.token with
        | Question ->
          Parser.next p;
          Some (TermParameter {uncurried; attrs; label = lbl; expr = None; pat; pos = startPos})
        | _ ->
          let expr = parseConstrainedOrCoercedExpr p in
          Some (TermParameter {uncurried; attrs; label = lbl; expr = Some expr; pat; pos = startPos})
        end
      | _ ->
        Some (TermParameter {uncurried; attrs; label = lbl; expr = None; pat; pos = startPos})
    )
    ) else None

  and parseParameterList p =
    let parameters =
      parseCommaDelimitedRegion
        ~grammar:Grammar.ParameterList
        ~f:parseParameter
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
  and parseParameters p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      let loc = mkLoc startPos p.Parser.prevEndPos in
      [TermParameter {
        uncurried = false;
        attrs = [];
        label = Asttypes.Nolabel;
        expr = None;
        pat = Ast_helper.Pat.var ~loc (Location.mkloc ident loc);
        pos = startPos;
      }]
    | List ->
      Parser.next p;
      let loc = mkLoc startPos p.Parser.prevEndPos in
      [TermParameter {
        uncurried = false;
        attrs = [];
        label = Asttypes.Nolabel;
        expr = None;
        pat = Ast_helper.Pat.var ~loc (Location.mkloc "list" loc);
        pos = startPos;
      }]
    | Underscore ->
      Parser.next p;
      let loc = mkLoc startPos p.Parser.prevEndPos in
      [TermParameter {uncurried = false; attrs = []; label = Asttypes.Nolabel; expr = None; pat = Ast_helper.Pat.any ~loc (); pos = startPos}]
    | Lparen ->
      Parser.next p;
      begin match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let loc = mkLoc startPos p.Parser.prevEndPos in
        let unitPattern = Ast_helper.Pat.construct
          ~loc (Location.mkloc (Longident.Lident "()") loc) None
        in
        [TermParameter {uncurried = false; attrs = []; label = Asttypes.Nolabel; expr = None; pat = unitPattern; pos = startPos}]
      | Dot ->
        Parser.next p;
        begin match p.token with
        | Rparen ->
          Parser.next p;
          let loc = mkLoc startPos p.Parser.prevEndPos in
          let unitPattern = Ast_helper.Pat.construct
            ~loc (Location.mkloc (Longident.Lident "()") loc) None
          in
          [TermParameter {uncurried = true; attrs = []; label = Asttypes.Nolabel; expr = None; pat = unitPattern; pos = startPos}]
        | _ ->
          begin match parseParameterList p with
          | (TermParameter {attrs; label = lbl; expr = defaultExpr; pat = pattern; pos = startPos})::rest ->
            (TermParameter {uncurried = true; attrs; label = lbl; expr = defaultExpr; pat = pattern; pos = startPos})::rest
          | parameters -> parameters
          end
        end
      | _ -> parseParameterList p
      end
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      []

  and parseCoercedExpr ~(expr: Parsetree.expression) p =
    Parser.expect ColonGreaterThan p;
    let typ = parseTypExpr p in
    let loc = mkLoc expr.pexp_loc.loc_start p.prevEndPos in
    Ast_helper.Exp.coerce ~loc expr None typ

  and parseConstrainedOrCoercedExpr p =
    let expr = parseExpr p in
    match p.Parser.token with
    | ColonGreaterThan ->
      parseCoercedExpr ~expr p
    | Colon ->
      Parser.next p;
      begin match p.token with
      | _ ->
        let typ = parseTypExpr p in
        let loc = mkLoc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
        let expr = Ast_helper.Exp.constraint_ ~loc expr typ in
        begin match p.token with
          | ColonGreaterThan ->
            parseCoercedExpr ~expr p
          | _ ->
            expr
        end
      end
    | _ -> expr


  and parseConstrainedExprRegion p =
    match p.Parser.token with
    | token when Grammar.isExprStart token ->
      let expr = parseExpr p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let typ = parseTypExpr p in
        let loc = mkLoc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
        Some (Ast_helper.Exp.constraint_ ~loc expr typ)
      | _ -> Some expr
      end
    | _ -> None

  (* Atomic expressions represent unambiguous expressions.
   * This means that regardless of the context, these expressions
   * are always interpreted correctly. *)
  and parseAtomicExpr p =
    Parser.leaveBreadcrumb p Grammar.ExprOperand;
    let startPos = p.Parser.startPos in
    let expr = match p.Parser.token with
      | (True | False) as token ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Exp.construct ~loc
          (Location.mkloc (Longident.Lident (Token.toString token)) loc) None
      | Int _ | String _ | Float _ | Character _ ->
        let c = parseConstant p in
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Exp.constant ~loc c
      | Backtick ->
        let expr = parseTemplateExpr p in
        {expr with pexp_loc = mkLoc startPos p.prevEndPos}
      | Uident _ | Lident _ ->
        parseValueOrConstructor p
      | Hash ->
        parsePolyVariantExpr p
      | Lparen ->
        Parser.next p;
        begin match p.Parser.token with
        | Rparen ->
          Parser.next p;
          let loc = mkLoc startPos p.prevEndPos in
          Ast_helper.Exp.construct
            ~loc (Location.mkloc (Longident.Lident "()") loc) None
        | _t ->
          let expr = parseConstrainedOrCoercedExpr p in
          begin match p.token with
          | Comma ->
            Parser.next p;
            parseTupleExpr ~startPos ~first:expr p
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
          parseListExpr ~startPos  p
        | _ ->
          let loc = mkLoc startPos p.prevEndPos in
          Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident "list") loc)
        end
      | Module ->
        Parser.next p;
        parseFirstClassModuleExpr ~startPos p
      | Lbracket ->
        parseArrayExp p
      | Lbrace ->
        parseBracedOrRecordExpr  p
      | LessThan ->
        parseJsx p
      | Percent ->
        let extension = parseExtension p in
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Exp.extension ~loc extension
      | Underscore as token ->
        (* This case is for error recovery. Not sure if it's the correct place *)
        Parser.err p (Diagnostics.lident token);
        Parser.next p;
        Recover.defaultExpr ()
      | token ->
        let errPos = p.prevEndPos in
        begin match skipTokensAndMaybeRetry p ~isStartOfGrammar:Grammar.isAtomicExprStart with
        | None ->
          Parser.err ~startPos:errPos p (Diagnostics.unexpected token p.breadcrumbs);
          Recover.defaultExpr ()
        | Some () -> parseAtomicExpr p
        end
    in
    Parser.eatBreadcrumb p;
    expr

  (* module(module-expr)
   * module(module-expr : package-type) *)
  and parseFirstClassModuleExpr ~startPos p =
    Parser.expect Lparen p;

    let modExpr = parseModuleExpr p in
    let modEndLoc = p.prevEndPos in
    begin match p.Parser.token with
    | Colon ->
      let colonStart = p.Parser.startPos in
      Parser.next p;
      let attrs = parseAttributes p in
      let packageType = parsePackageType ~startPos:colonStart ~attrs p in
      Parser.expect Rparen p;
      let loc = mkLoc startPos modEndLoc in
      let firstClassModule = Ast_helper.Exp.pack ~loc modExpr in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.constraint_ ~loc firstClassModule packageType
    | _ ->
      Parser.expect Rparen p;
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.pack ~loc modExpr
    end

  and parseBracketAccess p expr startPos =
    Parser.leaveBreadcrumb p Grammar.ExprArrayAccess;
    let lbracket = p.startPos in
    Parser.next p;
    let stringStart = p.startPos in
    match p.Parser.token with
    | String s ->
      Parser.next p;
      let stringEnd = p.prevEndPos in
      Parser.expect Rbracket p;
      let rbracket = p.prevEndPos in
      let e =
        let identLoc = mkLoc stringStart stringEnd in
        let loc = mkLoc lbracket rbracket in
        Ast_helper.Exp.apply ~loc
        (Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident "##") loc))
        [Nolabel, expr; Nolabel, (Ast_helper.Exp.ident ~loc:identLoc (Location.mkloc (Longident.Lident s) identLoc))]
      in
      let e = parsePrimaryExpr ~operand:e p in
      let equalStart = p.startPos in
      begin match p.token with
      | Equal ->
        Parser.next p;
        let equalEnd = p.prevEndPos in
        let rhsExpr = parseExpr p in
        let loc = mkLoc startPos rhsExpr.pexp_loc.loc_end in
        let operatorLoc = mkLoc equalStart equalEnd in
        Ast_helper.Exp.apply ~loc
          (Ast_helper.Exp.ident ~loc:operatorLoc (Location.mkloc (Longident.Lident "#=") operatorLoc))
          [Nolabel, e; Nolabel, rhsExpr]
      | _ -> e
      end
    | _ ->
      let accessExpr = parseConstrainedOrCoercedExpr p in
      Parser.expect Rbracket p;
      let rbracket = p.prevEndPos in
      let arrayLoc = mkLoc lbracket rbracket in
      begin match p.token with
      | Equal ->
        Parser.leaveBreadcrumb p ExprArrayMutation;
        Parser.next p;
        let rhsExpr = parseExpr p in
        let arraySet = Location.mkloc
          (Longident.Ldot(Lident "Array", "set"))
          arrayLoc
        in
        let endPos = p.prevEndPos in
        let arraySet = Ast_helper.Exp.apply
          ~loc:(mkLoc startPos endPos)
          (Ast_helper.Exp.ident ~loc:arrayLoc arraySet)
          [Nolabel, expr; Nolabel, accessExpr; Nolabel, rhsExpr]
        in
        Parser.eatBreadcrumb p;
        arraySet
      | _ ->
        let endPos = p.prevEndPos in
        let e =
          Ast_helper.Exp.apply
            ~loc:(mkLoc startPos endPos)
            (Ast_helper.Exp.ident
              ~loc:arrayLoc
              (Location.mkloc (Longident.Ldot(Lident "Array", "get")) arrayLoc)
              )
            [Nolabel, expr; Nolabel, accessExpr]
        in
        Parser.eatBreadcrumb p;
        parsePrimaryExpr ~operand:e p
      end

  (* * A primary expression represents
   *  - atomic-expr
   *  - john.age
   *  - array[0]
   *  - applyFunctionTo(arg1, arg2)
   *
   *  The "operand" represents the expression that is operated on
   *)
  and parsePrimaryExpr ~operand ?(noCall=false) p =
    let startPos = operand.pexp_loc.loc_start in
    let rec loop p expr =
      match p.Parser.token with
      | Dot ->
        Parser.next p;
        let lident = parseValuePath p in
        begin match p.Parser.token with
        | Equal when noCall = false ->
          Parser.leaveBreadcrumb p Grammar.ExprSetField;
          Parser.next p;
          let targetExpr = parseExpr p in
          let loc = mkLoc startPos p.prevEndPos in
          let setfield = Ast_helper.Exp.setfield ~loc expr lident targetExpr  in
          Parser.eatBreadcrumb p;
          setfield
        | _ ->
          let endPos = p.prevEndPos in
          let loc = mkLoc startPos endPos in
          loop p (Ast_helper.Exp.field ~loc expr lident)
        end
      | Lbracket when noCall = false && p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
        parseBracketAccess p expr startPos
      | Lparen when noCall = false && p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
        loop p (parseCallExpr p expr)
      | Backtick when noCall = false && p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
        begin match expr.pexp_desc with
        | Pexp_ident {txt = Longident.Lident ident} ->
          parseTemplateExpr ~prefix:ident p
        | _ ->
          Parser.err
            ~startPos:expr.pexp_loc.loc_start
            ~endPos:expr.pexp_loc.loc_end
            p
            (Diagnostics.message "Tagged template literals are currently restricted to identifiers like: json`null`.");
          parseTemplateExpr p
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
  and parseUnaryExpr p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | (Minus | MinusDot | Plus | PlusDot | Bang) as token ->
      Parser.leaveBreadcrumb p Grammar.ExprUnary;
      let tokenEnd = p.endPos in
      Parser.next p;
      let operand = parseUnaryExpr p in
      let unaryExpr = makeUnaryExpr startPos tokenEnd token operand  in
      Parser.eatBreadcrumb p;
      unaryExpr
    | _ ->
      parsePrimaryExpr ~operand:(parseAtomicExpr p) p

  (* Represents an "operand" in a binary expression.
   * If you have `a + b`, `a` and `b` both represent
   * the operands of the binary expression with opeartor `+` *)
  and parseOperandExpr ~context p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let expr = match p.Parser.token with
    | Assert ->
      Parser.next p;
      let expr = parseUnaryExpr p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.assert_ ~loc expr
    | Lazy ->
      Parser.next p;
      let expr = parseUnaryExpr p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.lazy_ ~loc expr
    | Try ->
      parseTryExpression p
    | If ->
      parseIfExpression p
    | For ->
      parseForExpression p
    | While ->
      parseWhileExpression p
    | Switch ->
      parseSwitchExpression p
    | _ ->
      if (context != WhenExpr) &&
         isEs6ArrowExpression ~inTernary:(context=TernaryTrueBranchExpr) p
      then
        parseEs6ArrowExpression p
      else
        parseUnaryExpr p
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
  and parseBinaryExpr ?(context=OrdinaryExpr) ?a p prec =
    let a = match a with
    | Some e -> e
    | None -> parseOperandExpr ~context p
    in
    let rec loop a =
      let token = p.Parser.token in
      let tokenPrec =
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
            Scanner.isBinaryOp p.scanner.src p.startPos.pos_cnum p.endPos.pos_cnum
          ) && p.startPos.pos_lnum > p.prevEndPos.pos_lnum -> -1
        | token -> Token.precedence token
      in
      if tokenPrec < prec then a
      else begin
        Parser.leaveBreadcrumb p (Grammar.ExprBinaryAfterOp token);
        let startPos = p.startPos in
        Parser.next p;
        let endPos = p.prevEndPos in
        let b = parseBinaryExpr ~context p (tokenPrec + 1) in
        let loc = mkLoc a.Parsetree.pexp_loc.loc_start b.pexp_loc.loc_end in
        let expr = Ast_helper.Exp.apply
          ~loc
          (makeInfixOperator p token startPos endPos)
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

  and parseTemplateExpr ?(prefix="") p =
    let hiddenOperator =
      let op = Location.mknoloc (Longident.Lident "^") in
      Ast_helper.Exp.ident op
    in
    let rec loop acc p =
      let startPos = p.Parser.startPos in
      match p.Parser.token with
      | TemplateTail txt ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        if String.length txt > 0 then
          let txt = if p.mode = ParseForTypeChecker then parseTemplateStringLiteral txt else txt in
          let str = Ast_helper.Exp.constant ~loc (Pconst_string(txt, Some prefix)) in
          Ast_helper.Exp.apply ~loc hiddenOperator
            [Nolabel, acc; Nolabel, str]
        else
          acc
      | TemplatePart txt ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        let expr = parseExprBlock p in
        let fullLoc = mkLoc startPos p.prevEndPos in
        Scanner.setTemplateMode p.scanner;
        Parser.expect Rbrace p;
        let txt = if p.mode = ParseForTypeChecker then parseTemplateStringLiteral txt else txt in
        let str = Ast_helper.Exp.constant ~loc (Pconst_string(txt, Some prefix)) in
        let next =
          let a = if String.length txt > 0 then
              Ast_helper.Exp.apply ~loc:fullLoc hiddenOperator [Nolabel, acc; Nolabel, str]
            else acc
          in
          Ast_helper.Exp.apply ~loc:fullLoc hiddenOperator
            [Nolabel, a; Nolabel, expr]
        in
        loop next p
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        acc
    in
    Scanner.setTemplateMode p.scanner;
    Parser.expect Backtick p;
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | TemplateTail txt ->
      let loc = mkLoc startPos p.endPos in
      Parser.next p;
      let txt = if p.mode = ParseForTypeChecker then parseTemplateStringLiteral txt else txt in
      Ast_helper.Exp.constant ~loc (Pconst_string(txt, Some prefix))
    | TemplatePart txt ->
      let constantLoc = mkLoc startPos p.endPos in
      Parser.next p;
      let expr = parseExprBlock p in
      let fullLoc = mkLoc startPos p.prevEndPos in
      Scanner.setTemplateMode p.scanner;
      Parser.expect Rbrace p;
      let txt = if p.mode = ParseForTypeChecker then parseTemplateStringLiteral txt else txt in
      let str = Ast_helper.Exp.constant ~loc:constantLoc (Pconst_string(txt, Some prefix)) in
      let next =
        if String.length txt > 0 then
          Ast_helper.Exp.apply ~loc:fullLoc hiddenOperator [Nolabel, str; Nolabel, expr]
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
  and overParseConstrainedOrCoercedOrArrowExpression p expr =
    match p.Parser.token with
    | ColonGreaterThan ->
      parseCoercedExpr ~expr p
    | Colon ->
      Parser.next p;
      let typ = parseTypExpr ~es6Arrow:false p in
      begin match p.Parser.token with
      | EqualGreater ->
        Parser.next p;
        let body = parseExpr p in
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
          ~loc:(mkLoc expr.pexp_loc.loc_start body.pexp_loc.loc_end)
          Asttypes.Nolabel
          None
          pat
          (Ast_helper.Exp.constraint_ body typ)
        in
        let arrow2 = Ast_helper.Exp.fun_
          ~loc:(mkLoc expr.pexp_loc.loc_start body.pexp_loc.loc_end)
          Asttypes.Nolabel
          None
          (Ast_helper.Pat.constraint_ pat typ)
          body
        in
        let msg =
          Doc.breakableGroup ~forceBreak:true (
            Doc.concat [
              Doc.text "Did you mean to annotate the parameter type or the return type?";
              Doc.indent (
                Doc.concat [
                  Doc.line;
                  Doc.text "1) ";
                  Printer.printExpression arrow1 CommentTable.empty;
                  Doc.line;
                  Doc.text "2) ";
                  Printer.printExpression arrow2 CommentTable.empty;
                ]
              )
            ]
          ) |> Doc.toString ~width:80
        in
        Parser.err
          ~startPos:expr.pexp_loc.loc_start
          ~endPos:body.pexp_loc.loc_end
          p
          (Diagnostics.message msg);
        arrow1
      | _ ->
        let open Parsetree in
        let loc = mkLoc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
        let expr = Ast_helper.Exp.constraint_ ~loc expr typ in
        let () = Parser.err
          ~startPos:expr.pexp_loc.loc_start
          ~endPos:typ.ptyp_loc.loc_end
          p
          (Diagnostics.message
            (Doc.breakableGroup ~forceBreak:true (Doc.concat [
              Doc.text "Expressions with type constraints need to be wrapped in parens:";
              Doc.indent (
                Doc.concat [
                Doc.line;
                Printer.addParens (Printer.printExpression expr CommentTable.empty);
                ]
              )
            ]) |> Doc.toString ~width:80
          ))
        in
        expr
      end
    | _ -> expr

  and parseLetBindingBody ~startPos ~attrs p =
    Parser.beginRegion p;
    Parser.leaveBreadcrumb p Grammar.LetBinding;
    let pat, exp =
      let pat = parsePattern p in
      match p.Parser.token with
      | Colon ->
        Parser.next p;
        begin match p.token with
        | Typ -> (* locally abstract types *)
          Parser.next p;
          let newtypes = parseLidentList p in
          Parser.expect Dot p;
          let typ = parseTypExpr p in
          Parser.expect Equal p;
          let expr = parseExpr p in
          let loc = mkLoc startPos p.prevEndPos in
          let exp, poly = wrapTypeAnnotation ~loc newtypes typ expr in
          let pat = Ast_helper.Pat.constraint_ ~loc pat poly in
          (pat, exp)
        | _ ->
          let polyType = parsePolyTypeExpr p in
          let loc = {pat.ppat_loc with loc_end = polyType.Parsetree.ptyp_loc.loc_end} in
          let pat = Ast_helper.Pat.constraint_ ~loc pat polyType in
          Parser.expect Token.Equal p;
          let exp = parseExpr p in
          let exp = overParseConstrainedOrCoercedOrArrowExpression p exp in
          (pat, exp)
        end
      | _ ->
        Parser.expect Token.Equal p;
        let exp = overParseConstrainedOrCoercedOrArrowExpression p (parseExpr p) in
        (pat, exp)
    in
    let loc = mkLoc startPos p.prevEndPos in
    let vb = Ast_helper.Vb.mk ~loc ~attrs pat exp in
    Parser.eatBreadcrumb p;
    Parser.endRegion p;
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
  and parseAttributesAndBinding (p : Parser.t) =
    let err = p.scanner.err in
    let ch = p.scanner.ch in
    let offset = p.scanner.offset in
    let rdOffset = p.scanner.rdOffset in
    let lineOffset = p.scanner.lineOffset in
    let lnum = p.scanner.lnum in
    let mode = p.scanner.mode in
    let token = p.token in
    let startPos = p.startPos in
    let endPos = p.endPos in
    let prevEndPos = p.prevEndPos in
    let breadcrumbs = p.breadcrumbs in
    let errors = p.errors in
    let diagnostics = p.diagnostics in
    let comments = p.comments in

    match p.Parser.token with
    | At ->
      let attrs = parseAttributes p in
      begin match p.Parser.token with
      | And ->
        attrs
      | _ ->
        p.scanner.err <- err;
        p.scanner.ch <- ch;
        p.scanner.offset <- offset;
        p.scanner.rdOffset <- rdOffset;
        p.scanner.lineOffset <- lineOffset;
        p.scanner.lnum <- lnum;
        p.scanner.mode <- mode;
        p.token <- token;
        p.startPos <- startPos;
        p.endPos <- endPos;
        p.prevEndPos <- prevEndPos;
        p.breadcrumbs <- breadcrumbs;
        p.errors <- errors;
        p.diagnostics <- diagnostics;
        p.comments <- comments;
        []
      end
    | _ -> []

  (* definition	::=	let [rec] let-binding  { and let-binding }   *)
  and parseLetBindings ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.optional p Let |> ignore;
    let recFlag = if Parser.optional p Token.Rec then
      Asttypes.Recursive
    else
      Asttypes.Nonrecursive
    in
    let first = parseLetBindingBody ~startPos ~attrs p in

    let rec loop p bindings =
      let startPos = p.Parser.startPos in
      let attrs = parseAttributesAndBinding p in
      match p.Parser.token with
      | And ->
        Parser.next p;
        let attrs = match p.token with
        | Export ->
          let exportLoc = mkLoc p.startPos p.endPos in
          Parser.next p;
          let genTypeAttr = (Location.mkloc "genType" exportLoc, Parsetree.PStr []) in
          genTypeAttr::attrs
        | _ -> attrs
        in
        ignore(Parser.optional p Let); (* overparse for fault tolerance *)
        let letBinding = parseLetBindingBody ~startPos ~attrs p in
        loop p (letBinding::bindings)
      | _ ->
        List.rev bindings
    in
    (recFlag, loop p [first])

  (*
   * div -> div
   * Foo -> Foo.createElement
   * Foo.Bar -> Foo.Bar.createElement
   *)
  and parseJsxName p =
    let longident = match p.Parser.token with
    | Lident ident ->
      let identStart = p.startPos in
      let identEnd = p.endPos in
      Parser.next p;
      let loc = mkLoc identStart identEnd in
      Location.mkloc (Longident.Lident ident) loc
    | Uident _ ->
      let longident = parseModuleLongIdent ~lowercase:false p in
      Location.mkloc (Longident.Ldot (longident.txt, "createElement")) longident.loc
    | _ ->
      let msg = "A jsx name should start with a lowercase or uppercase identifier, like: div in <div /> or Navbar in <Navbar />"
      in
      Parser.err p (Diagnostics.message msg);
      Location.mknoloc (Longident.Lident "_")
    in
    Ast_helper.Exp.ident ~loc:longident.loc longident

  and parseJsxOpeningOrSelfClosingElement ~startPos p =
    let jsxStartPos = p.Parser.startPos in
    let name = parseJsxName p in
    let jsxProps = parseJsxProps p in
    let children = match p.Parser.token with
    | Forwardslash -> (* <foo a=b /> *)
      let childrenStartPos = p.Parser.startPos in
      Parser.next p;
      let childrenEndPos = p.Parser.startPos in
      Parser.expect GreaterThan p;
      let loc = mkLoc childrenStartPos childrenEndPos in
      makeListExpression loc [] None (* no children *)
    | GreaterThan -> (* <foo a=b> bar </foo> *)
      let childrenStartPos = p.Parser.startPos in
      Scanner.setJsxMode p.scanner;
      Parser.next p;
      let (spread, children) = parseJsxChildren p in
      let childrenEndPos = p.Parser.startPos in
      let () = match p.token with
      | LessThanSlash -> Parser.next p
      | LessThan -> Parser.next p; Parser.expect Forwardslash p
      | token when Grammar.isStructureItemStart token -> ()
      | _ -> Parser.expect LessThanSlash p
      in
      begin match p.Parser.token with
      | Lident _ | Uident _ when verifyJsxOpeningClosingName p name ->
        Parser.expect GreaterThan p;
        let loc = mkLoc childrenStartPos childrenEndPos in
        ( match spread, children with
          | true, child :: _ ->
            child
          | _ ->
            makeListExpression loc children None
        )
      | token ->
        let () = if Grammar.isStructureItemStart token then (
          let closing = "</" ^ (string_of_pexp_ident name) ^ ">" in
          let msg = Diagnostics.message ("Missing " ^ closing) in
          Parser.err ~startPos ~endPos:p.prevEndPos p msg;
        ) else (
          let opening = "</" ^ (string_of_pexp_ident name) ^ ">" in
          let msg = "Closing jsx name should be the same as the opening name. Did you mean " ^ opening ^ " ?" in
          Parser.err ~startPos ~endPos:p.prevEndPos p (Diagnostics.message msg);
          Parser.expect GreaterThan p
        )
        in
        let loc = mkLoc childrenStartPos childrenEndPos in
        ( match spread, children with
          | true, child :: _ ->
            child
          | _ ->
            makeListExpression loc children None
        )
      end
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      makeListExpression Location.none [] None
    in
    let jsxEndPos = p.prevEndPos in
    let loc = mkLoc jsxStartPos jsxEndPos in
    Ast_helper.Exp.apply
      ~loc
      name
      (List.concat [jsxProps; [
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
  and parseJsx p =
    Parser.leaveBreadcrumb p Grammar.Jsx;
    let startPos = p.Parser.startPos in
    Parser.expect LessThan p;
    let jsxExpr = match p.Parser.token with
    | Lident _ | Uident _ ->
      parseJsxOpeningOrSelfClosingElement ~startPos p
    | GreaterThan -> (* fragment: <> foo </> *)
      parseJsxFragment p
    | _ ->
      parseJsxName p
    in
    {jsxExpr with pexp_attributes = [jsxAttr]}

  (*
   * jsx-fragment ::=
   *  | <> </>
   *  | <> jsx-children </>
   *)
  and parseJsxFragment p =
    let childrenStartPos = p.Parser.startPos in
    Scanner.setJsxMode p.scanner;
    Parser.expect GreaterThan p;
    let (_spread, children) = parseJsxChildren p in
    let childrenEndPos = p.Parser.startPos in
    Parser.expect LessThanSlash p;
    Parser.expect GreaterThan p;
    let loc = mkLoc childrenStartPos childrenEndPos in
    makeListExpression loc children None


  (*
   * jsx-prop ::=
   *   |  lident
   *   | ?lident
   *   |  lident =  jsx_expr
   *   |  lident = ?jsx_expr
   *)
  and parseJsxProp p =
    Parser.leaveBreadcrumb p Grammar.JsxAttribute;
    match p.Parser.token with
    | Question | Lident _ ->
      let optional = Parser.optional p Question in
      let (name, loc) = parseLident p in
      let propLocAttr = (Location.mkloc "ns.namedArgLoc" loc, Parsetree.PStr []) in
      (* optional punning: <foo ?a /> *)
      if optional then
        Some (
          Asttypes.Optional name,
          Ast_helper.Exp.ident ~attrs:[propLocAttr]
            ~loc (Location.mkloc (Longident.Lident name) loc)
        )
      else begin
        match p.Parser.token with
        | Equal ->
          Parser.next p;
          (* no punning *)
          let optional = Parser.optional p Question in
          let attrExpr =
            let e = parsePrimaryExpr ~operand:(parseAtomicExpr p) p in
            {e with pexp_attributes = propLocAttr::e.pexp_attributes}
          in
          let label =
            if optional then Asttypes.Optional name else Asttypes.Labelled name
          in
          Some (label, attrExpr)
        | _ ->
          let attrExpr =
            Ast_helper.Exp.ident ~loc ~attrs:[propLocAttr]
              (Location.mknoloc (Longident.Lident name)) in
          let label =
            if optional then Asttypes.Optional name else Asttypes.Labelled name
          in
          Some (label, attrExpr)
      end
    | _ ->
      None

  and parseJsxProps p =
    parseRegion
      ~grammar:Grammar.JsxAttribute
      ~f:parseJsxProp
      p

  and parseJsxChildren p =
    let rec loop p children =
      match p.Parser.token  with
      | Token.Eof | LessThanSlash ->
        Scanner.popMode p.scanner Jsx;
        List.rev children
      | LessThan ->
        (* Imagine: <div> <Navbar /> <
         * is `<` the start of a jsx-child? <div …
         * or is it the start of a closing tag?  </div>
         * reconsiderLessThan peeks at the next token and
         * determines the correct token to disambiguate *)
        let token = Scanner.reconsiderLessThan p.scanner in
        if token = LessThan then
          let child = parsePrimaryExpr ~operand:(parseAtomicExpr p) ~noCall:true p in
          loop p (child::children)
        else (* LessThanSlash *)
          let () = p.token <- token in
          let () = Scanner.popMode p.scanner Jsx in
          List.rev children
      | token when Grammar.isJsxChildStart token ->
        let () = Scanner.popMode p.scanner Jsx in
        let child = parsePrimaryExpr ~operand:(parseAtomicExpr p) ~noCall:true p in
        loop p (child::children)
      | _ ->
        Scanner.popMode p.scanner Jsx;
        List.rev children
    in
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      (true, [parsePrimaryExpr ~operand:(parseAtomicExpr p) ~noCall:true p])
    | _ -> (false, loop p [])

  and parseBracedOrRecordExpr  p =
    let startPos = p.Parser.startPos in
    Parser.expect Lbrace p;
    match p.Parser.token with
    | Rbrace ->
      Parser.err p (Diagnostics.unexpected Rbrace p.breadcrumbs);
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      let braces = makeBracesAttr loc in
      Ast_helper.Exp.construct ~attrs:[braces] ~loc
        (Location.mkloc (Longident.Lident "()") loc) None
    | DotDotDot ->
      (* beginning of record spread, parse record *)
      Parser.next p;
      let spreadExpr = parseConstrainedOrCoercedExpr p in
      Parser.expect Comma p;
      let expr = parseRecordExpr ~startPos ~spread:(Some spreadExpr) [] p in
      Parser.expect Rbrace p;
      expr
    | String s ->
      let field =
        let loc = mkLoc p.startPos p.endPos in
        Parser.next p;
        Location.mkloc (Longident.Lident s) loc
      in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let fieldExpr = parseExpr p in
        Parser.optional p Comma |> ignore;
        let expr = parseRecordExprWithStringKeys ~startPos (field, fieldExpr) p in
        Parser.expect Rbrace p;
        expr
      | _ ->
        let constant = Ast_helper.Exp.constant ~loc:field.loc (Parsetree.Pconst_string(s, None)) in
        let a = parsePrimaryExpr ~operand:constant p in
        let e = parseBinaryExpr ~a p 1 in
        let e = parseTernaryExpr e p in
        begin match p.Parser.token with
        | Semicolon ->
          Parser.next p;
          let expr = parseExprBlock ~first:e p in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        | Rbrace ->
          Parser.next p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {e with pexp_attributes = braces::e.pexp_attributes}
        | _ ->
          let expr = parseExprBlock ~first:e p in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        end
      end
    | Uident _ | Lident _ ->
      let valueOrConstructor = parseValueOrConstructor p in
      begin match valueOrConstructor.pexp_desc with
      | Pexp_ident pathIdent ->
        let identEndPos = p.prevEndPos in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          let expr = parseRecordExpr ~startPos [(pathIdent, valueOrConstructor)] p in
          Parser.expect Rbrace p;
          expr
        | Colon ->
          Parser.next p;
          let fieldExpr = parseExpr p in
          begin match p.token with
          | Rbrace ->
            Parser.next p;
            let loc = mkLoc startPos p.prevEndPos in
            Ast_helper.Exp.record ~loc [(pathIdent, fieldExpr)] None
          | _ ->
            Parser.expect Comma p;
            let expr = parseRecordExpr ~startPos [(pathIdent, fieldExpr)] p in
            Parser.expect Rbrace p;
            expr
          end
        (* error case *)
        | Lident _ ->
          if p.prevEndPos.pos_lnum < p.startPos.pos_lnum then (
            Parser.expect Comma p;
            let expr = parseRecordExpr ~startPos [(pathIdent, valueOrConstructor)] p in
            Parser.expect Rbrace p;
            expr
          ) else (
            Parser.expect Colon p;
            let expr = parseRecordExpr ~startPos [(pathIdent, valueOrConstructor)] p in
            Parser.expect Rbrace p;
            expr
          )
        | Semicolon ->
          Parser.next p;
          let expr = parseExprBlock ~first:(Ast_helper.Exp.ident pathIdent) p in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        | Rbrace ->
          Parser.next p;
          let expr = Ast_helper.Exp.ident ~loc:pathIdent.loc pathIdent in
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        | EqualGreater ->
          let loc = mkLoc startPos identEndPos in
          let ident = Location.mkloc (Longident.last pathIdent.txt) loc in
          let a = parseEs6ArrowExpression
            ~parameters:[TermParameter {
              uncurried = false;
              attrs = [];
              label = Asttypes.Nolabel;
              expr = None;
              pat = Ast_helper.Pat.var ident;
              pos = startPos;
            }]
            p
          in
          let e = parseBinaryExpr ~a p 1 in
          let e = parseTernaryExpr e p in
          begin match p.Parser.token with
          | Semicolon ->
            Parser.next p;
            let expr = parseExprBlock ~first:e p in
            Parser.expect Rbrace p;
            let loc = mkLoc startPos p.prevEndPos in
            let braces = makeBracesAttr loc in
            {expr with pexp_attributes = braces::expr.pexp_attributes}
          | Rbrace ->
            Parser.next p;
            let loc = mkLoc startPos p.prevEndPos in
            let braces = makeBracesAttr loc in
            {e with pexp_attributes = braces::e.pexp_attributes}
          | _ ->
            let expr = parseExprBlock ~first:e p in
            Parser.expect Rbrace p;
            let loc = mkLoc startPos p.prevEndPos in
            let braces = makeBracesAttr loc in
            {expr with pexp_attributes = braces::expr.pexp_attributes}
          end
        | _ ->
          Parser.leaveBreadcrumb p Grammar.ExprBlock;
          let a = parsePrimaryExpr ~operand:(Ast_helper.Exp.ident ~loc:pathIdent.loc pathIdent) p in
          let e = parseBinaryExpr ~a p 1 in
          let e = parseTernaryExpr e p in
          Parser.eatBreadcrumb p;
          begin match p.Parser.token with
          | Semicolon ->
            Parser.next p;
            let expr = parseExprBlock ~first:e p in
            Parser.expect Rbrace p;
            let loc = mkLoc startPos p.prevEndPos in
            let braces = makeBracesAttr loc in
            {expr with pexp_attributes = braces::expr.pexp_attributes}
          | Rbrace ->
            Parser.next p;
            let loc = mkLoc startPos p.prevEndPos in
            let braces = makeBracesAttr loc in
            {e with pexp_attributes = braces::e.pexp_attributes}
          | _ ->
            let expr = parseExprBlock ~first:e p in
            Parser.expect Rbrace p;
            let loc = mkLoc startPos p.prevEndPos in
            let braces = makeBracesAttr loc in
            {expr with pexp_attributes = braces::expr.pexp_attributes}
          end
         end
      | _ ->
        Parser.leaveBreadcrumb p Grammar.ExprBlock;
        let a = parsePrimaryExpr ~operand:valueOrConstructor p in
        let e = parseBinaryExpr ~a p 1 in
        let e = parseTernaryExpr e p in
        Parser.eatBreadcrumb p;
        begin match p.Parser.token with
        | Semicolon ->
          Parser.next p;
          let expr = parseExprBlock ~first:e p in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        | Rbrace ->
          Parser.next p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {e with pexp_attributes = braces::e.pexp_attributes}
        | _ ->
          let expr = parseExprBlock ~first:e p in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        end
         end
    | _ ->
      let expr = parseExprBlock p in
      Parser.expect Rbrace p;
      let loc = mkLoc startPos p.prevEndPos in
      let braces = makeBracesAttr loc in
      {expr with pexp_attributes = braces::expr.pexp_attributes}

  and parseRecordRowWithStringKey p =
    match p.Parser.token with
    | String s ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      let field = Location.mkloc (Longident.Lident s) loc in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let fieldExpr = parseExpr p in
        Some (field, fieldExpr)
      | _ ->
        Some (field, Ast_helper.Exp.ident ~loc:field.loc field)
      end
    | _ -> None

  and parseRecordRow p =
    let () = match p.Parser.token with
    | Token.DotDotDot ->
      Parser.err p (Diagnostics.message ErrorMessages.recordExprSpread);
      Parser.next p;
    | _ -> ()
    in
    match p.Parser.token with
    | Lident _ | Uident _ | List ->
      let field = parseValuePath p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let fieldExpr = parseExpr p in
        Some (field, fieldExpr)
      | _ ->
        Some (field, Ast_helper.Exp.ident ~loc:field.loc  field)
      end
    | _ -> None

  and parseRecordExprWithStringKeys ~startPos firstRow p =
    let rows = firstRow::(
      parseCommaDelimitedRegion ~grammar:Grammar.RecordRowsStringKey ~closing:Rbrace ~f:parseRecordRowWithStringKey p
    ) in
    let loc = mkLoc startPos p.endPos in
    let recordStrExpr = Ast_helper.Str.eval ~loc (
      Ast_helper.Exp.record ~loc rows None
    ) in
    Ast_helper.Exp.extension ~loc
      (Location.mkloc "bs.obj" loc, Parsetree.PStr [recordStrExpr])

  and parseRecordExpr ~startPos ?(spread=None) rows p =
    let exprs =
      parseCommaDelimitedRegion
        ~grammar:Grammar.RecordRows
        ~closing:Rbrace
        ~f:parseRecordRow p
    in
    let rows = List.concat [rows; exprs] in
    let () = match rows with
    | [] ->
      let msg = "Record spread needs at least one field that's updated" in
      Parser.err p (Diagnostics.message msg);
    | _rows -> ()
    in
    let loc = mkLoc startPos p.endPos in
    Ast_helper.Exp.record ~loc rows spread

  and parseExprBlockItem p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Module ->
      Parser.next p;
      begin match p.token with
      | Lparen ->
        parseFirstClassModuleExpr ~startPos p
      | _ ->
        let name = match p.Parser.token with
        | Uident ident ->
          let loc = mkLoc p.startPos p.endPos in
          Parser.next p;
          Location.mkloc ident loc
        | t ->
          Parser.err p (Diagnostics.uident t);
          Location.mknoloc "_"
        in
        let body = parseModuleBindingBody p in
        Parser.optional p Semicolon |> ignore;
        let expr = parseExprBlock p in
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Exp.letmodule ~loc name body expr
      end
    | Exception ->
      let extensionConstructor = parseExceptionDef ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let blockExpr = parseExprBlock  p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.letexception ~loc extensionConstructor blockExpr
    | Open ->
      let od = parseOpenDescription ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let blockExpr = parseExprBlock p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.open_ ~loc od.popen_override od.popen_lid blockExpr
    | Let ->
      let (recFlag, letBindings) = parseLetBindings ~attrs p in
      let next = match p.Parser.token with
      | Semicolon ->
        Parser.next p;
        if Grammar.isBlockExprStart p.Parser.token then
          parseExprBlock p
        else
          let loc = mkLoc p.startPos p.endPos in
          Ast_helper.Exp.construct ~loc
            (Location.mkloc (Longident.Lident "()") loc) None
      | token when Grammar.isBlockExprStart token ->
        parseExprBlock p
      | _ ->
        let loc = mkLoc p.startPos p.endPos in
        Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
      in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.let_ ~loc recFlag letBindings next
    | _ ->
      let e1 =
        let expr = parseExpr p in
        {expr with pexp_attributes = List.concat [attrs; expr.pexp_attributes]}
      in
      ignore (Parser.optional p Semicolon);
      if Grammar.isBlockExprStart p.Parser.token then
        let e2 = parseExprBlock p in
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
  and parseExprBlock ?first p =
    Parser.leaveBreadcrumb p Grammar.ExprBlock;
    let item = match first with
    | Some e -> e
    | None -> parseExprBlockItem p
    in
    let blockExpr = match p.Parser.token with
    | Semicolon ->
      Parser.next p;
      if Grammar.isBlockExprStart p.Parser.token then
        let next = parseExprBlockItem p in
        ignore(Parser.optional p Semicolon);
        let loc = {item.pexp_loc with loc_end = next.pexp_loc.loc_end} in
        Ast_helper.Exp.sequence ~loc item next
      else
        item
    | token when Grammar.isBlockExprStart token ->
      let next = parseExprBlockItem p in
      ignore(Parser.optional p Semicolon);
      let loc = {item.pexp_loc with loc_end = next.pexp_loc.loc_end} in
      Ast_helper.Exp.sequence ~loc item next
    | _ ->
      item
    in
    Parser.eatBreadcrumb p;
    overParseConstrainedOrCoercedOrArrowExpression p blockExpr

  and parseTryExpression p =
    let startPos = p.Parser.startPos in
    Parser.expect Try p;
    let expr = parseExpr ~context:WhenExpr p in
    Parser.expect Catch p;
    Parser.expect Lbrace p;
    let cases = parsePatternMatching p in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.try_ ~loc expr cases

  and parseIfExpression p =
    Parser.beginRegion p;
    Parser.leaveBreadcrumb p Grammar.ExprIf;
    let startPos = p.Parser.startPos in
    Parser.expect If p;
    Parser.leaveBreadcrumb p Grammar.IfCondition;
    (* doesn't make sense to try es6 arrow here? *)
    let conditionExpr = parseExpr ~context:WhenExpr p in
    Parser.eatBreadcrumb p;
    Parser.leaveBreadcrumb p IfBranch;
    Parser.expect Lbrace p;
    let thenExpr = parseExprBlock p in
    Parser.expect Rbrace p;
    Parser.eatBreadcrumb p;
    let elseExpr = match p.Parser.token with
    | Else ->
      Parser.endRegion p;
      Parser.leaveBreadcrumb p Grammar.ElseBranch;
      Parser.next p;
      Parser.beginRegion p;
      let elseExpr = match p.token with
      | If ->
        parseIfExpression p
      | _ ->
        Parser.expect  Lbrace p;
        let blockExpr = parseExprBlock p in
        Parser.expect Rbrace p;
        blockExpr
      in
      Parser.eatBreadcrumb p;
      Parser.endRegion p;
      Some elseExpr
    | _ ->
      Parser.endRegion p;
      None
    in
    let loc = mkLoc startPos p.prevEndPos in
    Parser.eatBreadcrumb p;
    Ast_helper.Exp.ifthenelse ~loc conditionExpr thenExpr elseExpr

  and parseForRest hasOpeningParen pattern startPos p =
    Parser.expect In p;
    let e1 = parseExpr p in
    let direction = match p.Parser.token with
    | To -> Asttypes.Upto
    | Downto -> Asttypes.Downto
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Asttypes.Upto
    in
    Parser.next p;
    let e2 = parseExpr ~context:WhenExpr p in
    if hasOpeningParen then Parser.expect Rparen p;
    Parser.expect Lbrace p;
    let bodyExpr = parseExprBlock p in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.for_ ~loc pattern e1 e2 direction bodyExpr

  and parseForExpression p =
    let startPos = p.Parser.startPos in
    Parser.expect For p;
		match p.token with
		| Lparen ->
			let lparen = p.startPos in
			Parser.next p;
			begin match p.token with
			| Rparen ->
				Parser.next p;
				let unitPattern =
					let loc = mkLoc lparen p.prevEndPos in
					let lid = Location.mkloc (Longident.Lident "()") loc in
					Ast_helper.Pat.construct lid None
				in
        parseForRest false (parseAliasPattern ~attrs:[] unitPattern p) startPos p
			| _ ->
        let pat = parsePattern p in
        begin match p.token with
        | Comma ->
          Parser.next p;
          let tuplePattern =
            parseTuplePattern ~attrs:[] ~startPos:lparen ~first:pat p
          in
          let pattern = parseAliasPattern ~attrs:[] tuplePattern p in
          parseForRest false pattern startPos p
        | _ ->
          parseForRest true pat startPos p
        end
			end
		| _ ->
      parseForRest false (parsePattern p) startPos p

  and parseWhileExpression p =
    let startPos = p.Parser.startPos in
    Parser.expect While p;
    let expr1 = parseExpr ~context:WhenExpr p in
    Parser.expect Lbrace p;
    let expr2 = parseExprBlock p in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.while_ ~loc expr1 expr2

  and parsePatternMatchCase p =
    Parser.beginRegion p;
    Parser.leaveBreadcrumb p Grammar.PatternMatchCase;
    match p.Parser.token with
    | Token.Bar ->
      Parser.next p;
      let lhs = parsePattern p in
      let guard = match p.Parser.token with
      | When ->
        Parser.next p;
        Some (parseExpr ~context:WhenExpr p)
      | _ ->
        None
      in
      let () = match p.token with
      | EqualGreater -> Parser.next p
      | _ -> Recover.recoverEqualGreater p
      in
      let rhs = parseExprBlock p in
      Parser.endRegion p;
      Parser.eatBreadcrumb p;
      Some (Ast_helper.Exp.case lhs ?guard rhs)
    | _ ->
      Parser.endRegion p;
      None

  and parsePatternMatching p =
    Parser.leaveBreadcrumb p Grammar.PatternMatching;
    let cases =
      parseDelimitedRegion
        ~grammar:Grammar.PatternMatching
        ~closing:Rbrace
        ~f:parsePatternMatchCase
        p
    in
    let () = match cases with
    | [] -> Parser.err ~startPos:p.prevEndPos p (
        Diagnostics.message "Pattern matching needs at least one case"
      )
    | _ -> ()
    in
    cases

  and parseSwitchExpression p =
    let startPos = p.Parser.startPos in
    Parser.expect Switch p;
    let switchExpr = parseExpr ~context:WhenExpr p in
    Parser.expect Lbrace p;
    let cases = parsePatternMatching p in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.match_ ~loc switchExpr cases

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
  and parseArgument p =
    if (
      p.Parser.token = Token.Tilde ||
      p.token = Dot ||
      p.token = Underscore ||
      Grammar.isExprStart p.token
    ) then (
      match p.Parser.token with
      | Dot ->
        let uncurried = true in
        let startPos = p.Parser.startPos in
        Parser.next(p);
        begin match p.token with
          (* apply(.) *)
          | Rparen ->
            let loc = mkLoc startPos p.prevEndPos in
            let unitExpr = Ast_helper.Exp.construct ~loc
              (Location.mkloc (Longident.Lident "()") loc) None
            in
            Some (uncurried, Asttypes.Nolabel, unitExpr)
          | _ ->
            parseArgument2 p ~uncurried
        end
      | _ ->
        parseArgument2 p ~uncurried:false
    ) else
      None

  and parseArgument2 p ~uncurried =
    match p.Parser.token with
    (* foo(_), do not confuse with foo(_ => x), TODO: performance *)
    | Underscore when not (isEs6ArrowExpression ~inTernary:false p) ->
      let loc = mkLoc p.startPos p.endPos in
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
        let startPos = p.startPos in
        Parser.next p;
        let endPos = p.prevEndPos in
        let loc = mkLoc startPos endPos in
        let propLocAttr = (Location.mkloc "ns.namedArgLoc" loc, Parsetree.PStr []) in
        let identExpr = Ast_helper.Exp.ident ~attrs:[propLocAttr] ~loc (
          Location.mkloc (Longident.Lident ident) loc
        ) in
        begin match p.Parser.token with
        | Question ->
          Parser.next p;
          Some (uncurried, Asttypes.Optional ident, identExpr)
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
          | Underscore when not (isEs6ArrowExpression ~inTernary:false p) ->
            let loc = mkLoc p.startPos p.endPos in
            Parser.next p;
            Ast_helper.Exp.ident ~loc (
              Location.mkloc (Longident.Lident "_") loc
            )
          | _ ->
           let expr = parseConstrainedOrCoercedExpr p in
           {expr with pexp_attributes = propLocAttr::expr.pexp_attributes}
          in
          Some (uncurried, label, expr)
        | Colon ->
          Parser.next p;
          let typ = parseTypExpr p in
          let loc = mkLoc startPos p.prevEndPos in
          let expr = Ast_helper.Exp.constraint_ ~attrs:[propLocAttr] ~loc identExpr typ in
          Some (uncurried, Labelled ident, expr)
        | _ ->
          Some (uncurried, Labelled ident, identExpr)
        end
      | t ->
        Parser.err p (Diagnostics.lident t);
        Some (uncurried, Nolabel, Recover.defaultExpr ())
      end
    | _ -> Some (uncurried, Nolabel, parseConstrainedOrCoercedExpr p)

  and parseCallExpr p funExpr =
    Parser.expect Lparen p;
    let startPos = p.Parser.startPos in
    Parser.leaveBreadcrumb p Grammar.ExprCall;
    let args =
      parseCommaDelimitedRegion
        ~grammar:Grammar.ArgumentList
        ~closing:Rparen
        ~f:parseArgument p
    in
    Parser.expect Rparen p;
    let args = match args with
    | [] ->
      let loc = mkLoc startPos p.prevEndPos in
     (* No args -> unit sugar: `foo()` *)
      [ false,
        Asttypes.Nolabel,
        Ast_helper.Exp.construct
          ~loc (Location.mkloc (Longident.Lident "()") loc) None
      ]
    | args -> args
    in
    let loc = {funExpr.pexp_loc with loc_end = p.prevEndPos} in
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
    let apply = List.fold_left (fun callBody group ->
      let (uncurried, args) = group in
      let (args, wrap) = processUnderscoreApplication args in
      let exp = if uncurried then
        let attrs = [uncurryAttr] in
        Ast_helper.Exp.apply ~loc ~attrs callBody args
      else
        Ast_helper.Exp.apply ~loc callBody args
      in
      wrap exp
    ) funExpr args
    in
    Parser.eatBreadcrumb p;
    apply

  and parseValueOrConstructor p =
    let startPos = p.Parser.startPos in
    let rec aux p acc =
      match p.Parser.token with
      | Uident ident ->
        let endPosLident = p.endPos in
        Parser.next p;
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          aux p (ident::acc)
        | Lparen when p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
          let lparen = p.startPos in
          let args = parseConstructorArgs p in
          let rparen = p.prevEndPos in
          let lident = buildLongident (ident::acc) in
          let tail = match args with
          | [] -> None
          | [{Parsetree.pexp_desc = Pexp_tuple _} as arg] as args ->
            let loc = mkLoc lparen rparen in
            if p.mode = ParseForTypeChecker then
              (* Some(1, 2) for type-checker *)
              Some arg
            else
              (* Some((1, 2)) for printer *)
              Some (Ast_helper.Exp.tuple ~loc args)
          | [arg] ->
            Some arg
          | args ->
            let loc = mkLoc lparen rparen in
            Some (Ast_helper.Exp.tuple ~loc args)
          in
          let loc = mkLoc startPos p.prevEndPos in
          let identLoc = mkLoc startPos endPosLident in
          Ast_helper.Exp.construct ~loc (Location.mkloc lident identLoc) tail
        | _ ->
          let loc = mkLoc startPos p.prevEndPos in
          let lident = buildLongident (ident::acc) in
          Ast_helper.Exp.construct ~loc (Location.mkloc lident loc) None
        end
      | Lident ident ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        let lident = buildLongident (ident::acc) in
        Ast_helper.Exp.ident ~loc (Location.mkloc lident loc)
      | List ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        let lident = buildLongident ("list"::acc) in
        Ast_helper.Exp.ident ~loc (Location.mkloc lident loc)
      | token ->
        Parser.next p;
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        Recover.defaultExpr()
    in
    aux p []

  and parsePolyVariantExpr p =
    let startPos = p.startPos in
    let (ident, _loc) = parseHashIdent ~startPos p in
    begin match p.Parser.token with
    | Lparen when p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
      let lparen = p.startPos in
      let args = parseConstructorArgs p in
      let rparen = p.prevEndPos in
      let loc_paren = mkLoc lparen rparen in
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
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.variant ~loc ident tail
    | _ ->
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.variant ~loc ident None
    end

  and parseConstructorArgs p =
    let lparen = p.Parser.startPos in
    Parser.expect Lparen p;
    let args =
      parseCommaDelimitedRegion
        ~grammar:Grammar.ExprList ~f:parseConstrainedExprRegion ~closing:Rparen p
    in
    Parser.expect Rparen p;
    match args with
    | [] ->
      let loc = mkLoc lparen p.prevEndPos in
      [Ast_helper.Exp.construct
        ~loc (Location.mkloc (Longident.Lident "()") loc) None]
    | args -> args

  and parseTupleExpr ~first ~startPos p =
    let exprs =
      parseCommaDelimitedRegion
        p ~grammar:Grammar.ExprList ~closing:Rparen ~f:parseConstrainedExprRegion
    in
    Parser.expect Rparen p;
    Ast_helper.Exp.tuple ~loc:(mkLoc startPos p.prevEndPos) (first::exprs)

  and parseSpreadExprRegion p =
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      let expr = parseConstrainedOrCoercedExpr p in
      Some (true, expr)
    | token when Grammar.isExprStart token ->
      Some (false, parseConstrainedOrCoercedExpr p)
    | _ -> None

  and parseListExpr ~startPos p =
    Parser.expect Lbracket p;
    let listExprs =
      parseCommaDelimitedReversedList
      p ~grammar:Grammar.ListExpr ~closing:Rbracket ~f:parseSpreadExprRegion
    in
    Parser.expect Rbracket p;
    let loc = mkLoc startPos p.prevEndPos in
    match listExprs with
    | (true, expr)::exprs ->
      let exprs = exprs |> List.map snd |> List.rev in
      makeListExpression loc exprs (Some expr)
    | exprs ->
     let exprs =
        exprs
        |> List.map (fun (spread, expr) ->
            if spread then
              Parser.err p (Diagnostics.message ErrorMessages.listExprSpread);
            expr)
        |> List.rev
      in
      makeListExpression loc exprs None

  (* Overparse ... and give a nice error message *)
  and parseNonSpreadExp ~msg p =
    let () = match p.Parser.token with
    | DotDotDot ->
      Parser.err p (Diagnostics.message msg);
      Parser.next p;
    | _ -> ()
    in
    match p.Parser.token with
    | token when Grammar.isExprStart token ->
      let expr = parseExpr p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let typ = parseTypExpr p in
        let loc = mkLoc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
        Some (Ast_helper.Exp.constraint_ ~loc expr typ)
      | _ -> Some expr
      end
    | _ -> None

  and parseArrayExp p =
    let startPos = p.Parser.startPos in
    Parser.expect Lbracket p;
    let exprs =
      parseCommaDelimitedRegion
        p
        ~grammar:Grammar.ExprList
        ~closing:Rbracket
        ~f:(parseNonSpreadExp ~msg:ErrorMessages.arrayExprSpread)
    in
    Parser.expect Rbracket p;
    Ast_helper.Exp.array ~loc:(mkLoc startPos p.prevEndPos) exprs

  (* TODO: check attributes in the case of poly type vars,
   * might be context dependend: parseFieldDeclaration (see ocaml) *)
  and parsePolyTypeExpr p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | SingleQuote ->
      let vars = parseTypeVarList p in
      begin match vars with
      | _v1::_v2::_ ->
        Parser.expect Dot p;
        let typ = parseTypExpr p in
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Typ.poly ~loc vars typ
      | [var] ->
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          let typ = parseTypExpr p in
          let loc = mkLoc startPos p.prevEndPos in
          Ast_helper.Typ.poly ~loc vars typ
        | EqualGreater ->
          Parser.next p;
          let typ = Ast_helper.Typ.var ~loc:var.loc var.txt in
          let returnType = parseTypExpr ~alias:false p in
          let loc = mkLoc typ.Parsetree.ptyp_loc.loc_start p.prevEndPos in
          Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ returnType
        | _ ->
          Ast_helper.Typ.var ~loc:var.loc var.txt
        end
      | _ -> assert false
      end
    | _ ->
      parseTypExpr p

  (* 'a 'b 'c *)
  and parseTypeVarList p =
    let rec loop p vars =
      match p.Parser.token with
      | SingleQuote ->
        Parser.next p;
        let (lident, loc) = parseLident p in
        let var = Location.mkloc lident loc in
        loop p (var::vars)
      | _ ->
        List.rev vars
    in
    loop p []

  and parseLidentList p =
    let rec loop p ls =
      match p.Parser.token with
      | Lident lident ->
        let loc = mkLoc p.startPos p.endPos in
        Parser.next p;
        loop p ((Location.mkloc lident loc)::ls)
      | _ ->
        List.rev ls
    in
    loop p []

  and parseAtomicTypExpr ~attrs p =
    Parser.leaveBreadcrumb p Grammar.AtomicTypExpr;
    let startPos = p.Parser.startPos in
    let typ = match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      let (ident, loc) = parseLident p in
      Ast_helper.Typ.var ~loc ~attrs ident
    | Underscore ->
      let endPos = p.endPos in
      Parser.next p;
      Ast_helper.Typ.any ~loc:(mkLoc startPos endPos) ~attrs ()
    | Lparen ->
      Parser.next p;
      begin match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        let unitConstr = Location.mkloc (Longident.Lident "unit") loc in
        Ast_helper.Typ.constr ~attrs unitConstr []
      | _ ->
        let t = parseTypExpr p in
        begin match p.token with
        | Comma ->
          Parser.next p;
          parseTupleType ~attrs ~first:t ~startPos p
        | _ ->
          Parser.expect Rparen p;
          {t with
            ptyp_loc = mkLoc startPos p.prevEndPos;
            ptyp_attributes = List.concat [attrs; t.ptyp_attributes]}
        end
      end
    | Lbracket ->
      parsePolymorphicVariantType ~attrs p
    | Uident _ | Lident _ | List ->
      let constr = parseValuePath p in
      let args =  parseTypeConstructorArgs ~constrName:constr p in
      Ast_helper.Typ.constr ~loc:(mkLoc startPos p.prevEndPos) ~attrs constr args
    | Module ->
      Parser.next p;
      Parser.expect Lparen p;
      let packageType = parsePackageType ~startPos ~attrs p in
      Parser.expect Rparen p;
      {packageType with ptyp_loc = mkLoc startPos p.prevEndPos}
    | Percent ->
      let extension = parseExtension p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Typ.extension ~attrs ~loc extension
    | Lbrace ->
      parseBsObjectType ~attrs p
    | token ->
      begin match skipTokensAndMaybeRetry p ~isStartOfGrammar:Grammar.isAtomicTypExprStart with
      | Some () ->
        parseAtomicTypExpr ~attrs p
      | None ->
        Parser.err ~startPos:p.prevEndPos p (Diagnostics.unexpected token p.breadcrumbs);
        Recover.defaultType()
      end
    in
    Parser.eatBreadcrumb p;
    typ

  (* package-type	::=
      | modtype-path
      ∣ modtype-path with package-constraint  { and package-constraint }
   *)
  and parsePackageType ~startPos ~attrs p =
    let modTypePath = parseModuleLongIdent ~lowercase:true p in
    begin match p.Parser.token with
    | With ->
      Parser.next p;
      let constraints = parsePackageConstraints p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Typ.package ~loc ~attrs modTypePath constraints
    | _ ->
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Typ.package ~loc ~attrs modTypePath []
    end

  (* package-constraint  { and package-constraint } *)
  and parsePackageConstraints p =
    let first =
      Parser.expect Typ p;
      let typeConstr = parseValuePath p in
      Parser.expect Equal p;
      let typ = parseTypExpr p in
      (typeConstr, typ)
    in
    let rest = parseRegion
      ~grammar:Grammar.PackageConstraint
      ~f:parsePackageConstraint
      p
    in
    first::rest

  (* and type typeconstr = typexpr *)
  and parsePackageConstraint p =
    match p.Parser.token with
    | And ->
      Parser.next p;
      Parser.expect Typ p;
      let typeConstr = parseValuePath p in
      Parser.expect Equal p;
      let typ = parseTypExpr p in
      Some (typeConstr, typ)
    | _ -> None

  and parseBsObjectType ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect Lbrace p;
    let closedFlag = match p.token with
    | DotDot -> Parser.next p; Asttypes.Open
    | Dot -> Parser.next p; Asttypes.Closed
    | _ -> Asttypes.Closed
    in
    let fields =
      parseCommaDelimitedRegion
        ~grammar:Grammar.StringFieldDeclarations
        ~closing:Rbrace
        ~f:parseStringFieldDeclaration
        p
    in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    makeBsObjType ~attrs ~loc ~closed:closedFlag fields

  (* TODO: check associativity in combination with attributes *)
  and parseTypeAlias p typ =
    match p.Parser.token with
    | As ->
      Parser.next p;
      Parser.expect SingleQuote p;
      let (ident, _loc) = parseLident p in
      (* TODO: how do we parse attributes here? *)
      Ast_helper.Typ.alias ~loc:(mkLoc typ.Parsetree.ptyp_loc.loc_start p.prevEndPos) typ ident
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
  and parseTypeParameter p =
    if (
      p.Parser.token = Token.Tilde ||
      p.token = Dot ||
      Grammar.isTypExprStart p.token
    ) then (
      let startPos = p.Parser.startPos in
      let uncurried = Parser.optional p Dot in
      let attrs = parseAttributes p in
      match p.Parser.token with
      | Tilde ->
        Parser.next p;
        let (name, _loc) = parseLident p in
        Parser.expect ~grammar:Grammar.TypeExpression Colon p;
        let typ = parseTypExpr p in
        begin match p.Parser.token with
        | Equal ->
          Parser.next p;
          Parser.expect Question p;
          Some (uncurried, attrs, Asttypes.Optional name, typ, startPos)
        | _ ->
          Some (uncurried, attrs, Asttypes.Labelled name, typ, startPos)
        end
      | Lident _ | List ->
        let (name, loc) = parseLident p in
        begin match p.token with
        | Colon ->
          let () =
            let error = Diagnostics.message
              ("Parameter names start with a `~`, like: ~" ^ name)
            in
            Parser.err ~startPos:loc.loc_start ~endPos:loc.loc_end p error
          in
          Parser.next p;
          let typ = parseTypExpr p in
          begin match p.Parser.token with
          | Equal ->
            Parser.next p;
            Parser.expect Question p;
            Some (uncurried, attrs, Asttypes.Optional name, typ, startPos)
          | _ ->
            Some (uncurried, attrs, Asttypes.Labelled name, typ, startPos)
          end
        | _ ->
          let constr = Location.mkloc (Longident.Lident name) loc in
          let args =  parseTypeConstructorArgs ~constrName:constr p in
          let typ = Ast_helper.Typ.constr ~loc:(mkLoc startPos p.prevEndPos) ~attrs constr args
          in

          let typ = parseArrowTypeRest ~es6Arrow:true ~startPos typ p in
          let typ = parseTypeAlias p typ in
          Some (uncurried, [], Asttypes.Nolabel, typ, startPos)
        end
      | _ ->
        let typ = parseTypExpr p in
        let typWithAttributes = {typ with ptyp_attributes = List.concat[attrs; typ.ptyp_attributes]} in
        Some (uncurried, [], Asttypes.Nolabel, typWithAttributes, startPos)
    ) else
      None

  (* (int, ~x:string, float) *)
  and parseTypeParameters p =
    let startPos = p.Parser.startPos in
    Parser.expect Lparen p;
    match p.Parser.token with
    | Rparen ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      let unitConstr = Location.mkloc (Longident.Lident "unit") loc in
      let typ = Ast_helper.Typ.constr unitConstr [] in
      [(false, [], Asttypes.Nolabel, typ, startPos)]
    | _ ->
      let params =
        parseCommaDelimitedRegion ~grammar:Grammar.TypeParameters ~closing:Rparen ~f:parseTypeParameter p
      in
      Parser.expect Rparen p;
      params

  and parseEs6ArrowType ~attrs p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let (name, _loc) = parseLident p in
      Parser.expect ~grammar:Grammar.TypeExpression Colon p;
      let typ = parseTypExpr ~alias:false ~es6Arrow:false p in
      let arg = match p.Parser.token with
      | Equal ->
        Parser.next p;
        Parser.expect Question p;
        Asttypes.Optional name
      | _ ->
        Asttypes.Labelled name
      in
      Parser.expect EqualGreater p;
      let returnType = parseTypExpr ~alias:false p in
      Ast_helper.Typ.arrow ~attrs arg typ returnType
    | _ ->
      let parameters = parseTypeParameters p in
      Parser.expect EqualGreater p;
      let returnType = parseTypExpr ~alias:false p in
      let endPos = p.prevEndPos in
      let typ = List.fold_right (fun (uncurried, attrs, argLbl, typ, startPos) t ->
        let attrs = if uncurried then uncurryAttr::attrs else attrs in
        Ast_helper.Typ.arrow ~loc:(mkLoc startPos endPos) ~attrs argLbl typ t
      ) parameters returnType
      in
      {typ with
        ptyp_attributes = List.concat [typ.ptyp_attributes; attrs];
        ptyp_loc = mkLoc startPos p.prevEndPos}

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
  and parseTypExpr ?attrs ?(es6Arrow=true) ?(alias=true) p =
    (* Parser.leaveBreadcrumb p Grammar.TypeExpression; *)
    let startPos = p.Parser.startPos in
    let attrs = match attrs with
      | Some attrs ->
        attrs
      | None ->
        parseAttributes p in
    let typ = if es6Arrow && isEs6ArrowType p then
      parseEs6ArrowType ~attrs p
    else
      let typ = parseAtomicTypExpr ~attrs p in
      parseArrowTypeRest ~es6Arrow ~startPos typ p
    in
    let typ = if alias then parseTypeAlias p typ else typ in
    (* Parser.eatBreadcrumb p; *)
    typ

  and parseArrowTypeRest ~es6Arrow ~startPos typ p =
    match p.Parser.token with
    | (EqualGreater | MinusGreater) as token when es6Arrow == true ->
      (* error recovery *)
      if token = MinusGreater then (
        Parser.expect EqualGreater p;
      );
      Parser.next p;
      let returnType = parseTypExpr ~alias:false p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ returnType
    | _ -> typ

  and parseTypExprRegion p =
    if Grammar.isTypExprStart p.Parser.token then
      Some (parseTypExpr p)
    else
      None

  and parseTupleType ~attrs ~first ~startPos p =
    let typexprs =
      parseCommaDelimitedRegion
        ~grammar:Grammar.TypExprList
        ~closing:Rparen
        ~f:parseTypExprRegion
        p
    in
    Parser.expect Rparen p;
    let tupleLoc = mkLoc startPos p.prevEndPos in
    Ast_helper.Typ.tuple ~attrs ~loc:tupleLoc (first::typexprs)

  and parseTypeConstructorArgRegion p =
    if Grammar.isTypExprStart p.Parser.token then
      Some (parseTypExpr p)
    else if p.token = LessThan then (
      Parser.next p;
      parseTypeConstructorArgRegion p
    ) else
      None

  (* Js.Nullable.value<'a> *)
  and parseTypeConstructorArgs ~constrName p =
    let opening = p.Parser.token in
    let openingStartPos = p.startPos in
    match opening with
    | LessThan | Lparen ->
      Scanner.setDiamondMode p.scanner;
      Parser.next p;
      let typeArgs =
        (* TODO: change Grammar.TypExprList to TypArgList!!! Why did I wrote this? *)
        parseCommaDelimitedRegion
          ~grammar:Grammar.TypExprList
          ~closing:GreaterThan
          ~f:parseTypeConstructorArgRegion
          p
      in
      let () = match p.token with
      | Rparen when opening = Token.Lparen ->
        let typ = Ast_helper.Typ.constr constrName typeArgs in
        let msg =
          Doc.breakableGroup ~forceBreak:true (
            Doc.concat [
              Doc.text "Type parameters require angle brackets:";
              Doc.indent (
                Doc.concat [
                  Doc.line;
                  Printer.printTypExpr typ CommentTable.empty;
                ]
              )
            ]
          ) |> Doc.toString ~width:80
        in
        Parser.err ~startPos:openingStartPos p (Diagnostics.message msg);
        Parser.next p
      | _ ->
        Parser.expect GreaterThan p
      in
      Scanner.popMode p.scanner Diamond;
      typeArgs
    | _ -> []

  (* string-field-decl ::=
   *  | string: poly-typexpr
   *  | attributes string-field-decl *)
  and parseStringFieldDeclaration p =
    let attrs = parseAttributes p in
    match p.Parser.token with
    | String name ->
      let nameStartPos = p.startPos in
      let nameEndPos = p.endPos in
      Parser.next p;
      let fieldName = Location.mkloc name (mkLoc nameStartPos nameEndPos) in
      Parser.expect ~grammar:Grammar.TypeExpression Colon p;
      let typ = parsePolyTypeExpr p in
      Some(Parsetree.Otag (fieldName, attrs, typ))
    | _token ->
      None

  (* field-decl	::=
   *  | [mutable] field-name : poly-typexpr
   *  | attributes field-decl *)
  and parseFieldDeclaration p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let mut = if Parser.optional p Token.Mutable then
      Asttypes.Mutable
    else
      Asttypes.Immutable
    in
    let (lident, loc) = match p.token with
    | List ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      ("list", loc)
    | _ -> parseLident p
    in
    let name = Location.mkloc lident loc in
    let typ = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parsePolyTypeExpr p
    | _ ->
      Ast_helper.Typ.constr ~loc:name.loc {name with txt = Lident name.txt} []
    in
    let loc = mkLoc startPos typ.ptyp_loc.loc_end in
    Ast_helper.Type.field ~attrs ~loc ~mut name typ


  and parseFieldDeclarationRegion p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let mut = if Parser.optional p Token.Mutable then
      Asttypes.Mutable
    else
      Asttypes.Immutable
    in
    match p.token with
    | Lident _ | List ->
      let (lident, loc) =  match p.token with
      | List ->
        let loc = mkLoc p.startPos p.endPos in
        Parser.next p;
        ("list", loc)
      | _ -> parseLident p
      in
      let name = Location.mkloc lident loc in
      let typ = match p.Parser.token with
      | Colon ->
        Parser.next p;
        parsePolyTypeExpr p
      | _ ->
        Ast_helper.Typ.constr ~loc:name.loc {name with txt = Lident name.txt} []
      in
      let loc = mkLoc startPos typ.ptyp_loc.loc_end in
      Some(Ast_helper.Type.field ~attrs ~loc ~mut name typ)
    | _ ->
      None

  (* record-decl ::=
   *  | { field-decl }
   *  | { field-decl, field-decl }
   *  | { field-decl, field-decl, field-decl, }
   *)
  and parseRecordDeclaration p =
    Parser.leaveBreadcrumb p Grammar.RecordDecl;
    Parser.expect Lbrace p;
    let rows =
      parseCommaDelimitedRegion
        ~grammar:Grammar.RecordDecl
        ~closing:Rbrace
        ~f:parseFieldDeclarationRegion
        p
    in
    Parser.expect Rbrace p;
    Parser.eatBreadcrumb p;
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
  and parseConstrDeclArgs p =
    let constrArgs = match p.Parser.token with
    | Lparen ->
      Parser.next p;
      (* TODO: this could use some cleanup/stratification *)
      begin match p.Parser.token with
      | Lbrace ->
        let lbrace = p.startPos in
        Parser.next p;
        let startPos = p.Parser.startPos in
        begin match p.Parser.token with
        | DotDot | Dot ->
          let closedFlag = match p.token with
          | DotDot -> Parser.next p; Asttypes.Open
          | Dot -> Parser.next p; Asttypes.Closed
          | _ -> Asttypes.Closed
          in
          let fields =
            parseCommaDelimitedRegion
              ~grammar:Grammar.StringFieldDeclarations
              ~closing:Rbrace
              ~f:parseStringFieldDeclaration
              p
          in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let typ = makeBsObjType ~attrs:[] ~loc ~closed:closedFlag fields in
          Parser.optional p Comma |> ignore;
          let moreArgs =
            parseCommaDelimitedRegion
            ~grammar:Grammar.TypExprList
            ~closing:Rparen
            ~f:parseTypExprRegion
            p
          in
          Parser.expect Rparen p;
          Parsetree.Pcstr_tuple (typ::moreArgs)
        | _ ->
          let attrs = parseAttributes p in
          begin match p.Parser.token with
          | String _  ->
            let closedFlag = Asttypes.Closed in
            let fields = match attrs with
            | [] ->
              parseCommaDelimitedRegion
                ~grammar:Grammar.StringFieldDeclarations
                ~closing:Rbrace
                ~f:parseStringFieldDeclaration
                p
            | attrs ->
              let first =
                Parser.leaveBreadcrumb p Grammar.StringFieldDeclarations;
                let field = match parseStringFieldDeclaration p with
                | Some field -> field
                | None -> assert false
                in
                (* parse comma after first *)
                let () = match p.Parser.token with
                | Rbrace | Eof -> ()
                | Comma -> Parser.next p
                | _ -> Parser.expect Comma p
                in
                Parser.eatBreadcrumb p;
                begin match field with
                | Parsetree.Otag (label, _, ct) -> Parsetree.Otag (label, attrs, ct)
                | Oinherit ct -> Oinherit ct
                end
              in
              first::(
                parseCommaDelimitedRegion
                  ~grammar:Grammar.StringFieldDeclarations
                  ~closing:Rbrace
                  ~f:parseStringFieldDeclaration
                  p
              ) in
              Parser.expect Rbrace p;
              let loc = mkLoc startPos p.prevEndPos in
              let typ = makeBsObjType ~attrs:[]  ~loc ~closed:closedFlag fields in
              Parser.optional p Comma |> ignore;
              let moreArgs =
                parseCommaDelimitedRegion
                  ~grammar:Grammar.TypExprList
                  ~closing:Rparen
                  ~f:parseTypExprRegion p
              in
              Parser.expect Rparen p;
              Parsetree.Pcstr_tuple (typ::moreArgs)
            | _ ->
              let fields = match attrs with
              | [] ->
                parseCommaDelimitedRegion
                  ~grammar:Grammar.FieldDeclarations
                  ~closing:Rbrace
                  ~f:parseFieldDeclarationRegion
                  p
              | attrs ->
                let first =
                  let field = parseFieldDeclaration p in
                  Parser.expect Comma p;
                  {field with Parsetree.pld_attributes = attrs}
                in
                first::(
                  parseCommaDelimitedRegion
                    ~grammar:Grammar.FieldDeclarations
                    ~closing:Rbrace
                    ~f:parseFieldDeclarationRegion
                    p
                )
              in
              let () = match fields with
              | [] -> Parser.err ~startPos:lbrace p (
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
            parseCommaDelimitedRegion
              ~grammar:Grammar.TypExprList
              ~closing:Rparen
              ~f:parseTypExprRegion
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
      Some (parseTypExpr p)
    | _ -> None
    in
    (constrArgs, res)

  (* constr-decl ::=
   *  | constr-name
   *  | attrs constr-name
   *  | constr-name const-args
   *  | attrs constr-name const-args *)
   and parseTypeConstructorDeclarationWithBar p =
    match p.Parser.token with
    | Bar ->
      let startPos = p.Parser.startPos in
      Parser.next p;
      Some (parseTypeConstructorDeclaration ~startPos p)
    | _ -> None

   and parseTypeConstructorDeclaration ~startPos p =
     Parser.leaveBreadcrumb p Grammar.ConstructorDeclaration;
     let attrs = parseAttributes p in
     match p.Parser.token with
     | Uident uident ->
       let uidentLoc = mkLoc p.startPos p.endPos in
       Parser.next p;
       let (args, res) = parseConstrDeclArgs p in
       Parser.eatBreadcrumb p;
       let loc = mkLoc startPos p.prevEndPos in
       Ast_helper.Type.constructor ~loc ~attrs ?res ~args (Location.mkloc uident uidentLoc)
     | t ->
      Parser.err p (Diagnostics.uident t);
      Ast_helper.Type.constructor (Location.mknoloc "_")

   (* [|] constr-decl  { | constr-decl }   *)
   and parseTypeConstructorDeclarations ?first p =
    let firstConstrDecl = match first with
    | None ->
      let startPos = p.Parser.startPos in
      ignore (Parser.optional p Token.Bar);
      parseTypeConstructorDeclaration ~startPos p
    | Some firstConstrDecl ->
      firstConstrDecl
    in
    firstConstrDecl::(
      parseRegion
        ~grammar:Grammar.ConstructorDeclaration
        ~f:parseTypeConstructorDeclarationWithBar
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
  and parseTypeRepresentation p =
    Parser.leaveBreadcrumb p Grammar.TypeRepresentation;
    (* = consumed *)
    let privateFlag =
      if Parser.optional p Token.Private
      then Asttypes.Private
      else Asttypes.Public
    in
    let kind = match p.Parser.token with
    | Bar | Uident _ ->
      Parsetree.Ptype_variant (parseTypeConstructorDeclarations p)
    | Lbrace ->
      Parsetree.Ptype_record (parseRecordDeclaration p)
    | DotDot ->
      Parser.next p;
      Ptype_open
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      (* TODO: I have no idea if this is even remotely a good idea *)
      Parsetree.Ptype_variant []
    in
    Parser.eatBreadcrumb p;
    (privateFlag, kind)

  (* type-param	::=
   *  | variance 'lident
   *  | variance _
   *
   * variance ::=
   *   | +
   *   | -
   *   | (* empty *)
   *)
  and parseTypeParam p =
    let variance = match p.Parser.token with
    | Plus -> Parser.next p; Asttypes.Covariant
    | Minus -> Parser.next p; Contravariant
    | _ -> Invariant
    in
    match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      let (ident, loc) = parseLident p in
      Some (Ast_helper.Typ.var ~loc ident, variance)
    | Underscore ->
      let loc = mkLoc p.startPos p.endPos in
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
  and parseTypeParams ~parent p =
    let opening = p.Parser.token in
    match opening with
    | LessThan | Lparen when p.startPos.pos_lnum == p.prevEndPos.pos_lnum ->
      Scanner.setDiamondMode p.scanner;
      let openingStartPos = p.startPos in
      Parser.leaveBreadcrumb p Grammar.TypeParams;
      Parser.next p;
      let params =
        parseCommaDelimitedRegion
          ~grammar:Grammar.TypeParams
          ~closing:GreaterThan
          ~f:parseTypeParam
          p
      in
      let () = match p.token with
      | Rparen when opening = Token.Lparen ->
        let msg =
          Doc.breakableGroup ~forceBreak:true (
            Doc.concat [
              Doc.text "Type parameters require angle brackets:";
              Doc.indent (
                Doc.concat [
                  Doc.line;
                  Doc.concat [
                    Printer.printLongident parent.Location.txt;
                    Printer.printTypeParams params CommentTable.empty;
                  ]
                ]
              )
            ]
          ) |> Doc.toString ~width:80
        in
        Parser.err ~startPos:openingStartPos p (Diagnostics.message msg);
        Parser.next p
      | _ ->
        Parser.expect GreaterThan p
      in
      Scanner.popMode p.scanner Diamond;
      Parser.eatBreadcrumb p;
      params
    | _ -> []

  (* type-constraint	::=	constraint ' ident =  typexpr *)
  and parseTypeConstraint p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | Token.Constraint ->
      Parser.next p;
      Parser.expect SingleQuote p;
      begin match p.Parser.token with
      | Lident ident ->
        let identLoc = mkLoc startPos p.endPos in
        Parser.next p;
        Parser.expect Equal p;
        let typ = parseTypExpr p in
        let loc = mkLoc startPos p.prevEndPos in
        Some (Ast_helper.Typ.var ~loc:identLoc ident, typ, loc)
      | t ->
        Parser.err p (Diagnostics.lident t);
        let loc = mkLoc startPos p.prevEndPos in
        Some (Ast_helper.Typ.any (), parseTypExpr p, loc)
      end
    | _ -> None

  (* type-constraints ::=
   *  | (* empty *)
   *  | type-constraint
   *  | type-constraint type-constraint
   *  | type-constraint type-constraint type-constraint (* 0 or more *)
   *)
  and parseTypeConstraints p =
    parseRegion
      ~grammar:Grammar.TypeConstraint
      ~f:parseTypeConstraint
      p

  and parseTypeEquationOrConstrDecl p =
    let uidentStartPos = p.Parser.startPos in
    match p.Parser.token with
    | Uident uident ->
      Parser.next p;
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        let typeConstr =
          parseValuePathTail p uidentStartPos (Longident.Lident uident)
        in
        let loc = mkLoc uidentStartPos p.prevEndPos in
        let typ = parseTypeAlias p (
          Ast_helper.Typ.constr ~loc typeConstr (parseTypeConstructorArgs ~constrName:typeConstr p)
        ) in
        begin match p.token with
        | Equal ->
          Parser.next p;
          let (priv, kind) = parseTypeRepresentation p in
          (Some typ, priv, kind)
        | EqualGreater ->
          Parser.next p;
          let returnType = parseTypExpr ~alias:false p in
          let loc = mkLoc uidentStartPos p.prevEndPos in
          let arrowType = Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ returnType in
          let typ = parseTypeAlias p arrowType in
          (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
        | _ -> (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
        end
      | _ ->
        let uidentEndPos = p.endPos in
        let (args, res) = parseConstrDeclArgs p in
        let first = Some (
          let uidentLoc = mkLoc uidentStartPos uidentEndPos in
          Ast_helper.Type.constructor
            ~loc:(mkLoc uidentStartPos p.prevEndPos)
            ?res
            ~args
            (Location.mkloc uident uidentLoc)
        ) in
        (None, Asttypes.Public, Parsetree.Ptype_variant (parseTypeConstructorDeclarations p ?first))
      end
    | t ->
      Parser.err p (Diagnostics.uident t);
      (* TODO: is this a good idea? *)
      (None, Asttypes.Public, Parsetree.Ptype_abstract)

  and parseRecordOrBsObjectDecl p =
    let startPos = p.Parser.startPos in
    Parser.expect Lbrace p;
    match p.Parser.token with
    | DotDot | Dot ->
      let closedFlag = match p.token with
      | DotDot -> Parser.next p; Asttypes.Open
      | Dot -> Parser.next p; Asttypes.Closed
      | _ -> Asttypes.Closed
      in
      let fields =
        parseCommaDelimitedRegion
          ~grammar:Grammar.StringFieldDeclarations
          ~closing:Rbrace
          ~f:parseStringFieldDeclaration
          p
      in
      Parser.expect Rbrace p;
      let loc = mkLoc startPos p.prevEndPos in
      let typ =
        makeBsObjType ~attrs:[] ~loc ~closed:closedFlag fields
        |> parseTypeAlias p
      in
      let typ = parseArrowTypeRest ~es6Arrow:true ~startPos typ p in
      (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
    | _ ->
      let attrs = parseAttributes p in
      begin match p.Parser.token with
      | String _  ->
        let closedFlag = Asttypes.Closed in
        let fields = match attrs with
        | [] ->
          parseCommaDelimitedRegion
            ~grammar:Grammar.StringFieldDeclarations
            ~closing:Rbrace
            ~f:parseStringFieldDeclaration
            p
        | attrs ->
          let first =
            Parser.leaveBreadcrumb p Grammar.StringFieldDeclarations;
            let field = match parseStringFieldDeclaration p with
            | Some field -> field
            | None -> assert false
            in
            (* parse comma after first *)
            let () = match p.Parser.token with
            | Rbrace | Eof -> ()
            | Comma -> Parser.next p
            | _ -> Parser.expect Comma p
            in
            Parser.eatBreadcrumb p;
            begin match field with
            | Parsetree.Otag (label, _, ct) -> Parsetree.Otag (label, attrs, ct)
            | Oinherit ct -> Oinherit ct
            end
          in
          first::(
            parseCommaDelimitedRegion
              ~grammar:Grammar.StringFieldDeclarations
              ~closing:Rbrace
              ~f:parseStringFieldDeclaration
              p
          )
          in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let typ =
            makeBsObjType ~attrs:[] ~loc ~closed:closedFlag fields |> parseTypeAlias p
          in
          let typ = parseArrowTypeRest ~es6Arrow:true ~startPos typ p in
          (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
      | _ ->
        Parser.leaveBreadcrumb p Grammar.RecordDecl;
        let fields = match attrs with
        | [] ->
          parseCommaDelimitedRegion
            ~grammar:Grammar.FieldDeclarations
            ~closing:Rbrace
            ~f:parseFieldDeclarationRegion
            p
        | attr::_ as attrs ->
          let first =
            let field = parseFieldDeclaration p in
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
            parseCommaDelimitedRegion
              ~grammar:Grammar.FieldDeclarations
              ~closing:Rbrace
              ~f:parseFieldDeclarationRegion
              p
          )
        in
        let () = match fields with
        | [] -> Parser.err ~startPos p (
            Diagnostics.message "A record needs at least one field"
          )
        | _ -> ()
        in
        Parser.expect Rbrace p;
        Parser.eatBreadcrumb p;
        (None, Asttypes.Public, Parsetree.Ptype_record fields)
      end

  and parsePrivateEqOrRepr p =
    Parser.expect Private p;
    match p.Parser.token with
    | Lbrace ->
      let (manifest, _ ,kind) = parseRecordOrBsObjectDecl p in
      (manifest, Asttypes.Private, kind)
    | Uident _ ->
      let (manifest, _, kind) = parseTypeEquationOrConstrDecl p in
      (manifest, Asttypes.Private, kind)
    | Bar | DotDot ->
      let (_, kind) = parseTypeRepresentation p in
      (None, Asttypes.Private, kind)
    | t when Grammar.isTypExprStart t ->
      (Some (parseTypExpr p), Asttypes.Private, Parsetree.Ptype_abstract)
    | _ ->
      let (_, kind) = parseTypeRepresentation p in
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
  and parsePolymorphicVariantType ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect Lbracket p;
    match p.token with
    | GreaterThan ->
      Parser.next p;
      let rowFields =
        begin match p.token with
        | Rbracket ->
          []
        | Bar ->
          parseTagSpecs p
        | _ ->
          let rowField = parseTagSpec p in
          rowField :: parseTagSpecs p
        end
      in
      let variant =
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Typ.variant ~attrs ~loc rowFields Open None in
      Parser.expect Rbracket p;
      variant
    | LessThan ->
      Parser.next p;
      Parser.optional p Bar |> ignore;
      let rowField = parseTagSpecFull p in
      let rowFields = parseTagSpecFulls p in
      let tagNames =
        if p.token == GreaterThan
        then begin
          Parser.next p;
          let rec loop p = match p.Parser.token with
            | Rbracket -> []
            | _ ->
              let (ident, _loc) = parseHashIdent ~startPos:p.startPos p in
              ident :: loop p
          in
          loop p
        end
        else [] in
      let variant =
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Typ.variant ~attrs ~loc (rowField :: rowFields) Closed (Some tagNames) in
      Parser.expect Rbracket p;
      variant
    | _ ->
      let rowFields1 = parseTagSpecFirst p in
      let rowFields2 = parseTagSpecs p in
      let variant =
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Typ.variant ~attrs ~loc (rowFields1 @ rowFields2) Closed None in
      Parser.expect Rbracket p;
      variant

  and parseTagSpecFulls p =
    match p.Parser.token with
    | Rbracket ->
      []
    | GreaterThan ->
      []
    | Bar ->
      Parser.next p;
      let rowField = parseTagSpecFull p in
      rowField ::parseTagSpecFulls p
    | _ ->
      []

  and parseTagSpecFull p =
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Hash ->
      parsePolymorphicVariantTypeSpecHash ~attrs ~full:true p
    | _ ->
      let typ = parseTypExpr ~attrs p in
      Parsetree.Rinherit typ

  and parseTagSpecs p =
    match p.Parser.token with
    | Bar ->
      Parser.next p;
      let rowField = parseTagSpec p in
      rowField :: parseTagSpecs p
    | _ ->
      []

  and parseTagSpec p =
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Hash ->
      parsePolymorphicVariantTypeSpecHash ~attrs ~full:false p
    | _ ->
      let typ = parseTypExpr ~attrs p in
      Parsetree.Rinherit typ

  and parseTagSpecFirst p =
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Bar ->
      Parser.next p;
      [parseTagSpec p]
    | Hash ->
      [parsePolymorphicVariantTypeSpecHash ~attrs ~full:false p]
    | _ ->
      let typ = parseTypExpr ~attrs p in
      Parser.expect Bar p;
      [Parsetree.Rinherit typ; parseTagSpec p]

  and parsePolymorphicVariantTypeSpecHash ~attrs ~full p : Parsetree.row_field =
    let startPos = p.Parser.startPos in
    let (ident, loc) = parseHashIdent ~startPos p in
    let rec loop p =
      match p.Parser.token with
      | Band when full ->
        Parser.next p;
        let rowField = parsePolymorphicVariantTypeArgs p in
        rowField :: loop p
      | _ ->
        []
    in
    let firstTuple, tagContainsAConstantEmptyConstructor =
      match p.Parser.token with
      | Band when full ->
        Parser.next p;
        [parsePolymorphicVariantTypeArgs p], true
      | Lparen ->
        [parsePolymorphicVariantTypeArgs p], false
      | _ ->
        [], true
    in
    let tuples = firstTuple @ loop p in
    Parsetree.Rtag (
      Location.mkloc ident loc,
      attrs,
      tagContainsAConstantEmptyConstructor,
      tuples
    )

  and parsePolymorphicVariantTypeArgs p =
    let startPos = p.Parser.startPos in
    Parser.expect Lparen p;
    let args = parseCommaDelimitedRegion
      ~grammar:Grammar.TypExprList
      ~closing:Rparen
      ~f:parseTypExprRegion
      p
    in
    Parser.expect Rparen p;
    let attrs = [] in
    let loc = mkLoc startPos p.prevEndPos in
    match args with
    | [{ptyp_desc = Ptyp_tuple _} as typ] as types ->
      if p.mode = ParseForTypeChecker then
        typ
      else
        Ast_helper.Typ.tuple ~loc ~attrs types
    | [typ] -> typ
    | types -> Ast_helper.Typ.tuple ~loc ~attrs types

  and parseTypeEquationAndRepresentation p =
    match p.Parser.token with
    | Equal | Bar as token ->
      if token = Bar then Parser.expect Equal p;
      Parser.next p;
      begin match p.Parser.token with
      | Uident _ ->
        parseTypeEquationOrConstrDecl p
      | Lbrace ->
        parseRecordOrBsObjectDecl p
      | Private ->
        parsePrivateEqOrRepr p
      | Bar | DotDot ->
        let (priv, kind) = parseTypeRepresentation p in
        (None, priv, kind)
      | _ ->
        let manifest = Some (parseTypExpr p) in
        begin match p.Parser.token with
        | Equal ->
          Parser.next p;
          let (priv, kind) = parseTypeRepresentation p in
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
  and parseTypeDef ~attrs ~startPos p =
    Parser.leaveBreadcrumb p Grammar.TypeDef;
    (* let attrs = match attrs with | Some attrs -> attrs | None -> parseAttributes p in *)
    Parser.leaveBreadcrumb p Grammar.TypeConstrName;
    let (name, loc) = parseLident p in
    let typeConstrName = Location.mkloc name loc in
    Parser.eatBreadcrumb p;
    let params =
      let constrName = Location.mkloc (Longident.Lident name) loc in
      parseTypeParams ~parent:constrName p in
    let typeDef =
      let (manifest, priv, kind) = parseTypeEquationAndRepresentation p in
      let cstrs = parseTypeConstraints p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Type.mk
        ~loc ~attrs ~priv ~kind ~params ~cstrs ?manifest typeConstrName
    in
    Parser.eatBreadcrumb p;
    typeDef

  and parseTypeExtension ~params ~attrs ~name p =
    Parser.expect PlusEqual p;
    let priv =
      if Parser.optional p Token.Private
      then Asttypes.Private
      else Asttypes.Public
    in
    let constrStart = p.Parser.startPos in
    Parser.optional p Bar |> ignore;
    let first =
      let (attrs, name, kind) = match p.Parser.token with
      | Bar ->
        Parser.next p;
        parseConstrDef ~parseAttrs:true p
      | _ ->
        parseConstrDef ~parseAttrs:true p
      in
      let loc = mkLoc constrStart p.prevEndPos in
      Ast_helper.Te.constructor ~loc ~attrs name kind
    in
    let rec loop p cs =
      match p.Parser.token with
      | Bar ->
        let startPos = p.Parser.startPos in
        Parser.next p;
        let (attrs, name, kind) = parseConstrDef ~parseAttrs:true p in
        let extConstr =
          Ast_helper.Te.constructor ~attrs ~loc:(mkLoc startPos p.prevEndPos) name kind
        in
        loop p (extConstr::cs)
      | _ ->
        List.rev cs
    in
    let constructors = loop p [first] in
    Ast_helper.Te.mk ~attrs ~params ~priv name constructors

  and parseTypeDefinitions ~attrs ~name ~params ~startPos p =
      let typeDef =
        let (manifest, priv, kind) = parseTypeEquationAndRepresentation p in
        let cstrs = parseTypeConstraints p in
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Type.mk
          ~loc ~attrs ~priv ~kind ~params ~cstrs ?manifest
          {name with txt = lidentOfPath name.Location.txt}
      in
      let rec loop p defs =
        let startPos = p.Parser.startPos in
        let attrs = parseAttributesAndBinding p in
        match p.Parser.token with
        | And ->
          Parser.next p;
          let attrs = match p.token with
          | Export ->
            let exportLoc = mkLoc p.startPos p.endPos in
            Parser.next p;
            let genTypeAttr = (Location.mkloc "genType" exportLoc, Parsetree.PStr []) in
            genTypeAttr::attrs
          | _ -> attrs
          in
          let typeDef = parseTypeDef ~attrs ~startPos p in
          loop p (typeDef::defs)
        | _ ->
          List.rev defs
      in
      loop p [typeDef]

  (* TODO: decide if we really want type extensions (eg. type x += Blue)
   * It adds quite a bit of complexity that can be avoided,
   * implemented for now. Needed to get a feel for the complexities of
   * this territory of the grammar *)
  and parseTypeDefinitionOrExtension ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect Token.Typ p;
    let recFlag = match p.token with
      | Rec -> Parser.next p; Asttypes.Recursive
      | Lident "nonrec" ->
        Parser.next p;
        Asttypes.Nonrecursive
      | _ -> Asttypes.Nonrecursive
    in
    let name = parseValuePath p in
    let params = parseTypeParams ~parent:name p in
    match p.Parser.token with
    | PlusEqual ->
      TypeExt(parseTypeExtension ~params ~attrs ~name p)
    | _ ->
      let typeDefs = parseTypeDefinitions ~attrs ~name ~params ~startPos p in
      TypeDef {recFlag; types = typeDefs}

  and parsePrimitive p =
    match p.Parser.token with
    | String s -> Parser.next p; Some s
    | _ -> None

  and parsePrimitives p =
    match (parseRegion ~grammar:Grammar.Primitive ~f:parsePrimitive p) with
    | [] ->
      let msg = "An external definition should have at least one primitive. Example: \"setTimeout\"" in
      Parser.err p (Diagnostics.message msg);
      []
    | primitives -> primitives

  (* external value-name : typexp = external-declaration *)
  and parseExternalDef ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.leaveBreadcrumb p Grammar.External;
    Parser.expect Token.External p;
    let (name, loc) = parseLident p in
    let name = Location.mkloc name loc in
    Parser.expect ~grammar:(Grammar.TypeExpression) Colon p;
    let typExpr = parseTypExpr p in
    Parser.expect Equal p;
    let prim = parsePrimitives p in
    let loc = mkLoc startPos p.prevEndPos in
    let vb = Ast_helper.Val.mk ~loc ~attrs ~prim name typExpr in
    Parser.eatBreadcrumb p;
    vb

  (* constr-def ::=
   *  | constr-decl
   *  | constr-name = constr
   *
   *  constr-decl ::= constr-name constr-args
   *  constr-name ::= uident
   *  constr      ::= path-uident *)
  and parseConstrDef ~parseAttrs p =
    let attrs = if parseAttrs then parseAttributes p else [] in
    let name = match p.Parser.token with
    | Uident name ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc name loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    let kind = match p.Parser.token with
    | Lparen ->
      let (args, res) = parseConstrDeclArgs p in
      Parsetree.Pext_decl (args, res)
    | Equal ->
      Parser.next p;
      let longident = parseModuleLongIdent ~lowercase:false p in
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
  and parseExceptionDef ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect Token.Exception p;
    let (_, name, kind) = parseConstrDef ~parseAttrs:false p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Te.constructor ~loc ~attrs name kind

  (* module structure on the file level *)
  and parseImplementation p : Parsetree.structure =
    parseRegion p ~grammar:Grammar.Implementation ~f:parseStructureItemRegion
    [@@progress (Parser.next, Parser.expect, Parser.checkProgress)]

  and parseStructureItemRegion p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Open ->
      let openDescription = parseOpenDescription ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Str.open_ ~loc openDescription)
    | Let ->
      let (recFlag, letBindings) = parseLetBindings ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Str.value ~loc recFlag letBindings)
    | Typ ->
      Parser.beginRegion p;
      begin match parseTypeDefinitionOrExtension ~attrs p with
      | TypeDef {recFlag; types} ->
        Parser.optional p Semicolon |> ignore;
        let loc = mkLoc startPos p.prevEndPos in
        Parser.endRegion p;
        Some (Ast_helper.Str.type_ ~loc recFlag types)
      | TypeExt(ext) ->
        Parser.optional p Semicolon |> ignore;
        let loc = mkLoc startPos p.prevEndPos in
        Parser.endRegion p;
        Some (Ast_helper.Str.type_extension ~loc ext)
      end
    | External ->
      let externalDef = parseExternalDef ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Str.primitive ~loc externalDef)
    | Import ->
      let importDescr = parseJsImport ~startPos ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      let structureItem = JsFfi.toParsetree importDescr in
      Some {structureItem with pstr_loc = loc}
    | Exception ->
      let exceptionDef = parseExceptionDef ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Str.exception_ ~loc exceptionDef)
    | Include ->
      let includeStatement = parseIncludeStatement ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Str.include_ ~loc includeStatement)
    | Export ->
      let structureItem = parseJsExport ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some {structureItem with pstr_loc = loc}
    | Module ->
      let structureItem = parseModuleOrModuleTypeImplOrPackExpr ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some {structureItem with pstr_loc = loc}
    | AtAt ->
      let attr = parseStandaloneAttribute p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Str.attribute ~loc attr)
    | PercentPercent ->
      let extension = parseExtension ~moduleLanguage:true p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Str.extension ~attrs ~loc extension)
    | token when Grammar.isExprStart token ->
      let prevEndPos = p.Parser.endPos in
      let exp = parseExpr p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Parser.checkProgress ~prevEndPos ~result:(Ast_helper.Str.eval ~loc ~attrs exp) p
    | _ -> None

  and parseJsImport ~startPos ~attrs p =
    Parser.expect Token.Import p;
    let importSpec = match p.Parser.token with
    | Token.Lident _ | Token.At ->
      let decl = match parseJsFfiDeclaration p with
      | Some decl -> decl
      | None -> assert false
      in
      JsFfi.Default decl
    | _ -> JsFfi.Spec(parseJsFfiDeclarations p)
    in
    let scope = parseJsFfiScope p in
    let loc = mkLoc startPos p.prevEndPos in
    JsFfi.importDescr ~attrs ~importSpec ~scope ~loc

  and parseJsExport ~attrs p =
    let exportStart = p.Parser.startPos in
    Parser.expect Token.Export p;
    let exportLoc = mkLoc exportStart p.prevEndPos in
    let genTypeAttr = (Location.mkloc "genType" exportLoc, Parsetree.PStr []) in
    let attrs = genTypeAttr::attrs in
    match p.Parser.token with
    | Typ ->
      begin match parseTypeDefinitionOrExtension ~attrs p with
      | TypeDef {recFlag; types} ->
        Ast_helper.Str.type_ recFlag types
      | TypeExt(ext) ->
        Ast_helper.Str.type_extension ext
      end
    | (* Let *) _ ->
      let (recFlag, letBindings) = parseLetBindings ~attrs p in
      Ast_helper.Str.value recFlag letBindings

  and parseJsFfiScope p =
    match p.Parser.token with
    | Token.Lident "from" ->
      Parser.next p;
      begin match p.token with
      | String s -> Parser.next p; JsFfi.Module s
      | Uident _ | Lident _ ->
        let value = parseIdentPath p in
        JsFfi.Scope value
      | _ -> JsFfi.Global
      end
    | _ -> JsFfi.Global

  and parseJsFfiDeclarations p =
    Parser.expect Token.Lbrace p;
    let decls = parseCommaDelimitedRegion
      ~grammar:Grammar.JsFfiImport
      ~closing:Rbrace
      ~f:parseJsFfiDeclaration
      p
    in
    Parser.expect Rbrace p;
    decls

  and parseJsFfiDeclaration p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Lident _ ->
      let (ident, _) = parseLident p in
      let alias = match p.token with
      | As ->
        Parser.next p;
        let (ident, _) = parseLident p in
        ident
      | _ ->
        ident
      in
      Parser.expect Token.Colon p;
      let typ = parseTypExpr p in
      let loc = mkLoc startPos p.prevEndPos in
      Some (JsFfi.decl ~loc ~alias ~attrs ~name:ident ~typ)
    | _ -> None

  (* include-statement ::= include module-expr *)
  and parseIncludeStatement ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect Token.Include p;
    let modExpr = parseModuleExpr p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Incl.mk ~loc ~attrs modExpr

  and parseAtomicModuleExpr p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | Uident _ident ->
      let longident = parseModuleLongIdent ~lowercase:false p in
      Ast_helper.Mod.ident ~loc:longident.loc longident
    | Lbrace ->
      Parser.next p;
      let structure = Ast_helper.Mod.structure (
        parseDelimitedRegion
          ~grammar:Grammar.Structure
          ~closing:Rbrace
          ~f:parseStructureItemRegion
          p
      ) in
      Parser.expect Rbrace p;
      let endPos = p.prevEndPos in
      {structure with pmod_loc = mkLoc startPos endPos}
    | Lparen ->
      Parser.next p;
      let modExpr = match p.token with
      | Rparen ->
        Ast_helper.Mod.structure ~loc:(mkLoc startPos p.prevEndPos) []
      | _ ->
        parseConstrainedModExpr p
      in
      Parser.expect Rparen p;
      modExpr
    | Lident "unpack" -> (* TODO: should this be made a keyword?? *)
      Parser.next p;
      Parser.expect Lparen p;
      let expr = parseExpr p in
      begin match p.Parser.token with
      | Colon ->
        let colonStart = p.Parser.startPos in
        Parser.next p;
        let attrs = parseAttributes p in
        let packageType = parsePackageType ~startPos:colonStart ~attrs p in
        Parser.expect Rparen p;
        let loc = mkLoc startPos p.prevEndPos in
        let constraintExpr = Ast_helper.Exp.constraint_
          ~loc
          expr packageType
        in
        Ast_helper.Mod.unpack ~loc constraintExpr
      | _ ->
        Parser.expect Rparen p;
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Mod.unpack ~loc expr
      end
    | Percent ->
      let extension = parseExtension p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Mod.extension ~loc extension
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.defaultModuleExpr()

  and parsePrimaryModExpr p =
    let startPos = p.Parser.startPos in
    let modExpr = parseAtomicModuleExpr p in
    let rec loop p modExpr =
      match p.Parser.token with
      | Lparen when p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
        loop p (parseModuleApplication p modExpr)
      | _ -> modExpr
    in
    let modExpr = loop p modExpr in
    {modExpr with pmod_loc = mkLoc startPos p.prevEndPos}

  (*
   * functor-arg ::=
   *  | uident : modtype
   *  | _ : modtype
   *  | modtype           --> "punning" for _ : modtype
   *  | attributes functor-arg
   *)
  and parseFunctorArg p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      let uidentEndPos = p.prevEndPos in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let moduleType = parseModuleType p in
        let loc = mkLoc startPos uidentEndPos in
        let argName = Location.mkloc ident loc in
        Some (attrs, argName, Some moduleType, startPos)
      | Dot ->
        Parser.next p;
        let moduleType =
          let moduleLongIdent =
            parseModuleLongIdentTail ~lowercase:false p startPos (Longident.Lident ident) in
          Ast_helper.Mty.ident ~loc:moduleLongIdent.loc moduleLongIdent
        in
        let argName = Location.mknoloc "_" in
        Some (attrs, argName, Some moduleType, startPos)
      | _ ->
        let loc = mkLoc startPos uidentEndPos in
        let modIdent = Location.mkloc (Longident.Lident ident) loc in
        let moduleType = Ast_helper.Mty.ident ~loc modIdent in
        let argName = Location.mknoloc "_" in
        Some (attrs, argName, Some moduleType, startPos)
      end
    | Underscore ->
      Parser.next p;
      let argName = Location.mkloc "_" (mkLoc startPos p.prevEndPos) in
      Parser.expect Colon p;
      let moduleType = parseModuleType p in
      Some (attrs, argName, Some moduleType, startPos)
    | _ ->
      None

  and parseFunctorArgs p =
    let startPos = p.Parser.startPos in
    Parser.expect Lparen p;
    let args =
      parseCommaDelimitedRegion
        ~grammar:Grammar.FunctorArgs
        ~closing:Rparen
        ~f:parseFunctorArg
        p
    in
    Parser.expect Rparen p;
    match args with
    | [] ->
      [[], Location.mkloc "*" (mkLoc startPos p.prevEndPos), None, startPos]
    | args -> args

  and parseFunctorModuleExpr p =
    let startPos = p.Parser.startPos in
    let args = parseFunctorArgs p in
    let returnType = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parseModuleType ~es6Arrow:false p)
    | _ -> None
    in
    Parser.expect EqualGreater p;
    let rhsModuleExpr =
      let modExpr = parseModuleExpr p in
      match returnType with
      | Some modType ->
        Ast_helper.Mod.constraint_
          ~loc:(mkLoc modExpr.pmod_loc.loc_start modType.Parsetree.pmty_loc.loc_end)
          modExpr modType
      | None -> modExpr
    in
    let endPos = p.prevEndPos in
    let modExpr = List.fold_right (fun (attrs, name, moduleType, startPos) acc ->
      Ast_helper.Mod.functor_
        ~loc:(mkLoc startPos endPos)
        ~attrs
        name moduleType acc
    ) args rhsModuleExpr
    in
    {modExpr with pmod_loc = mkLoc startPos endPos}

  (* module-expr	::=
   *  | module-path
   *  ∣	{ structure-items }
   *  ∣	functorArgs =>  module-expr
   *  ∣	module-expr(module-expr)
   *  ∣	( module-expr )
   *  ∣	( module-expr : module-type )
   *  | extension
   *  | attributes module-expr *)
  and parseModuleExpr p =
    let attrs = parseAttributes p in
    let modExpr = if isEs6ArrowFunctor p then
        parseFunctorModuleExpr p
      else
        parsePrimaryModExpr p
    in
    {modExpr with pmod_attributes = List.concat [modExpr.pmod_attributes; attrs]}

  and parseConstrainedModExpr p =
    let modExpr = parseModuleExpr p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let modType = parseModuleType p in
      let loc = mkLoc modExpr.pmod_loc.loc_start modType.pmty_loc.loc_end in
      Ast_helper.Mod.constraint_ ~loc modExpr modType
    | _ -> modExpr

  and parseConstrainedModExprRegion p =
    if Grammar.isModExprStart p.Parser.token then
      Some (parseConstrainedModExpr p)
    else
      None

  and parseModuleApplication p modExpr =
    let startPos = p.Parser.startPos in
    Parser.expect Lparen p;
    let args =
      parseCommaDelimitedRegion
        ~grammar:Grammar.ModExprList
        ~closing:Rparen
        ~f:parseConstrainedModExprRegion
        p
    in
    Parser.expect Rparen p;
    let args = match args with
    | [] ->
      let loc = mkLoc startPos p.prevEndPos in
      [Ast_helper.Mod.structure ~loc []]
    | args -> args
    in
    List.fold_left (fun modExpr arg ->
      Ast_helper.Mod.apply
        ~loc:(mkLoc modExpr.Parsetree.pmod_loc.loc_start arg.Parsetree.pmod_loc.loc_end)
        modExpr arg
    ) modExpr args

  and parseModuleOrModuleTypeImplOrPackExpr ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect Module p;
    match p.Parser.token with
    | Typ -> parseModuleTypeImpl ~attrs startPos p
    | Lparen ->
      let expr = parseFirstClassModuleExpr ~startPos p in
      Ast_helper.Str.eval ~attrs expr
    | _ -> parseMaybeRecModuleBinding ~attrs ~startPos p

  and parseModuleTypeImpl ~attrs startPos p =
    Parser.expect Typ p;
    let nameStart = p.Parser.startPos in
    let name = match p.Parser.token with
    | List ->
      Parser.next p;
      let loc = mkLoc nameStart p.prevEndPos in
      Location.mkloc "list" loc
    | Lident ident ->
      Parser.next p;
      let loc = mkLoc nameStart p.prevEndPos in
      Location.mkloc ident loc
    | Uident ident ->
      Parser.next p;
      let loc = mkLoc nameStart p.prevEndPos in
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    Parser.expect Equal p;
    let moduleType = parseModuleType p in
    let moduleTypeDeclaration =
      Ast_helper.Mtd.mk
        ~attrs
        ~loc:(mkLoc nameStart p.prevEndPos)
        ~typ:moduleType
        name
    in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Str.modtype ~loc moduleTypeDeclaration

  (* definition	::=
    ∣	 module rec module-name :  module-type =  module-expr   { and module-name
    :  module-type =  module-expr } *)
  and parseMaybeRecModuleBinding ~attrs ~startPos p =
    match p.Parser.token with
    | Token.Rec ->
      Parser.next p;
      Ast_helper.Str.rec_module (parseModuleBindings ~startPos ~attrs p)
    | _ ->
      Ast_helper.Str.module_ (parseModuleBinding ~attrs ~startPos:p.Parser.startPos p)

  and parseModuleBinding ~attrs ~startPos p =
    let name = match p.Parser.token with
    | Uident ident ->
      let startPos = p.Parser.startPos in
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    let body = parseModuleBindingBody p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Mb.mk ~attrs ~loc name body

  and parseModuleBindingBody p =
    (* TODO: make required with good error message when rec module binding *)
    let returnModType = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parseModuleType p)
    | _ -> None
    in
    Parser.expect Equal p;
    let modExpr = parseModuleExpr p in
    match returnModType with
    | Some modType ->
      Ast_helper.Mod.constraint_
        ~loc:(mkLoc modType.pmty_loc.loc_start modExpr.pmod_loc.loc_end)
        modExpr modType
    | None -> modExpr


  (* module-name :  module-type =  module-expr
   * { and module-name :  module-type =  module-expr } *)
  and parseModuleBindings ~attrs ~startPos p =
    let rec loop p acc =
      let startPos = p.Parser.startPos in
      let attrs = parseAttributesAndBinding p in
      match p.Parser.token with
      | And ->
        Parser.next p;
        ignore(Parser.optional p Module); (* over-parse for fault-tolerance *)
        let modBinding = parseModuleBinding ~attrs ~startPos p in
        loop p (modBinding::acc)
      | _ -> List.rev acc
    in
    let first = parseModuleBinding ~attrs ~startPos p in
    loop p [first]

  and parseAtomicModuleType p =
    let startPos = p.Parser.startPos in
    let moduleType = match p.Parser.token with
    | Uident _ | Lident _ | List ->
      (* Ocaml allows module types to end with lowercase: module Foo : bar = { ... }
       * lets go with uppercase terminal for now *)
      let moduleLongIdent = parseModuleLongIdent ~lowercase:true p in
      Ast_helper.Mty.ident ~loc:moduleLongIdent.loc moduleLongIdent
    | Lparen ->
      Parser.next p;
      let mty = parseModuleType p in
      Parser.expect Rparen p;
      {mty with pmty_loc = mkLoc startPos p.prevEndPos}
    | Lbrace ->
      Parser.next p;
      let spec =
        parseDelimitedRegion
          ~grammar:Grammar.Signature
          ~closing:Rbrace
          ~f:parseSignatureItemRegion
          p
      in
      Parser.expect Rbrace p;
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Mty.signature ~loc spec
    | Module -> (* TODO: check if this is still atomic when implementing first class modules*)
      parseModuleTypeOf p
    | Percent ->
      let extension = parseExtension p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Mty.extension ~loc extension
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.defaultModuleType()
    in
    let moduleTypeLoc = mkLoc startPos p.prevEndPos in
    {moduleType with pmty_loc = moduleTypeLoc}

  and parseFunctorModuleType p =
    let startPos = p.Parser.startPos in
    let args = parseFunctorArgs p in
    Parser.expect EqualGreater p;
    let rhs = parseModuleType p in
    let endPos = p.prevEndPos in
    let modType = List.fold_right (fun (attrs, name, moduleType, startPos) acc ->
      Ast_helper.Mty.functor_
        ~loc:(mkLoc startPos endPos)
        ~attrs
        name moduleType acc
    ) args rhs
    in
    {modType with pmty_loc = mkLoc startPos endPos}

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
   and parseModuleType ?(es6Arrow=true) ?(with_=true) p =
    let attrs = parseAttributes p in
    let modty = if es6Arrow && isEs6ArrowFunctor p then
      parseFunctorModuleType p
    else
      let modty = parseAtomicModuleType p in
      match p.Parser.token with
      | EqualGreater when es6Arrow == true ->
        Parser.next p;
        let rhs = parseModuleType ~with_:false p in
        let str = Location.mknoloc "_" in
        let loc = mkLoc modty.pmty_loc.loc_start p.prevEndPos in
        Ast_helper.Mty.functor_ ~loc str (Some modty) rhs
      | _ -> modty
    in
    let moduleType = { modty with
      pmty_attributes = List.concat [modty.pmty_attributes; attrs]
    } in
    if with_ then
      parseWithConstraints moduleType p
    else moduleType


  and parseWithConstraints moduleType p =
    match p.Parser.token with
    | With ->
      Parser.next p;
      let first = parseWithConstraint p in
      let rec loop p acc =
        match p.Parser.token with
        | And ->
          Parser.next p;
          loop p ((parseWithConstraint p)::acc)
        | _ ->
          List.rev acc
      in
      let constraints = loop p [first] in
      let loc = mkLoc moduleType.pmty_loc.loc_start p.prevEndPos in
      Ast_helper.Mty.with_ ~loc moduleType constraints
    | _ ->
      moduleType

  (* mod-constraint	::=
   *  |  type typeconstr<type-params> type-equation type-constraints?
   *  ∣	 type typeconstr-name<type-params> := typexpr
   *  ∣	 module module-path = extended-module-path
   *  ∣	 module module-path :=  extended-module-path
   *
   *  TODO: split this up into multiple functions, better errors *)
  and parseWithConstraint p =
    match p.Parser.token with
    | Module ->
      Parser.next p;
      let modulePath = parseModuleLongIdent ~lowercase:false p in
      begin match p.Parser.token with
      | ColonEqual ->
        Parser.next p;
        let lident = parseModuleLongIdent ~lowercase:false p in
        Parsetree.Pwith_modsubst (modulePath, lident)
      | Equal ->
        Parser.next p;
        let lident = parseModuleLongIdent ~lowercase:false p in
        Parsetree.Pwith_module (modulePath, lident)
      | token ->
        (* TODO: revisit *)
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        let lident = parseModuleLongIdent ~lowercase:false p in
        Parsetree.Pwith_modsubst (modulePath, lident)
      end
    | Typ ->
      Parser.next p;
      let typeConstr = parseValuePath p in
      let params = parseTypeParams ~parent:typeConstr p in
      begin match p.Parser.token with
      | ColonEqual ->
        Parser.next p;
        let typExpr = parseTypExpr p in
        Parsetree.Pwith_typesubst (
          typeConstr,
          Ast_helper.Type.mk
            ~loc:typeConstr.loc
            ~params
            ~manifest:typExpr
            (Location.mkloc (Longident.last typeConstr.txt) typeConstr.loc)
        )
      | Equal ->
        Parser.next p;
        let typExpr = parseTypExpr p in
        let typeConstraints = parseTypeConstraints p in
        Parsetree.Pwith_type (
          typeConstr,
          Ast_helper.Type.mk
            ~loc:typeConstr.loc
            ~params
            ~manifest:typExpr
            ~cstrs:typeConstraints
            (Location.mkloc (Longident.last typeConstr.txt) typeConstr.loc)
        )
      | token ->
        (* TODO: revisit *)
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        let typExpr = parseTypExpr p in
        let typeConstraints = parseTypeConstraints p in
        Parsetree.Pwith_type (
          typeConstr,
          Ast_helper.Type.mk
            ~loc:typeConstr.loc
            ~params
            ~manifest:typExpr
            ~cstrs:typeConstraints
            (Location.mkloc (Longident.last typeConstr.txt) typeConstr.loc)
        )
      end
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      exit (-1) (* TODO: handle this case *)

  and parseModuleTypeOf p =
    let startPos = p.Parser.startPos in
    Parser.expect Module p;
    Parser.expect Typ p;
    Parser.expect Of p;
    let moduleExpr = parseModuleExpr p in
    Ast_helper.Mty.typeof_ ~loc:(mkLoc startPos p.prevEndPos) moduleExpr

  (* module signature on the file level *)
  and parseSpecification p =
    parseRegion ~grammar:Grammar.Specification ~f:parseSignatureItemRegion p
    [@@progress (Parser.next, Parser.expect, Parser.checkProgress)]

  and parseSignatureItemRegion p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Let ->
      Parser.beginRegion p;
      let valueDesc = parseSignLetDesc ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Parser.endRegion p;
      Some (Ast_helper.Sig.value ~loc valueDesc)
    | Typ ->
      Parser.beginRegion p;
      begin match parseTypeDefinitionOrExtension ~attrs p with
      | TypeDef {recFlag; types} ->
        Parser.optional p Semicolon |> ignore;
        let loc = mkLoc startPos p.prevEndPos in
        Parser.endRegion p;
        Some (Ast_helper.Sig.type_ ~loc recFlag types)
      | TypeExt(ext) ->
        Parser.optional p Semicolon |> ignore;
        let loc = mkLoc startPos p.prevEndPos in
        Parser.endRegion p;
        Some (Ast_helper.Sig.type_extension ~loc ext)
      end
    | External ->
      let externalDef = parseExternalDef ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Sig.value ~loc externalDef)
    | Exception ->
      let exceptionDef = parseExceptionDef ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Sig.exception_ ~loc exceptionDef)
    | Open ->
      let openDescription = parseOpenDescription ~attrs p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Sig.open_ ~loc openDescription)
    | Include ->
      Parser.next p;
      let moduleType = parseModuleType p in
      let includeDescription = Ast_helper.Incl.mk
        ~loc:(mkLoc startPos p.prevEndPos)
        ~attrs
        moduleType
      in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Sig.include_ ~loc includeDescription)
    | Module ->
      Parser.next p;
      begin match p.Parser.token with
      | Uident _ ->
        let modDecl = parseModuleDeclarationOrAlias ~attrs p in
        Parser.optional p Semicolon |> ignore;
        let loc = mkLoc startPos p.prevEndPos in
        Some (Ast_helper.Sig.module_ ~loc modDecl)
      | Rec ->
        let recModule = parseRecModuleSpec ~attrs ~startPos p in
        Parser.optional p Semicolon |> ignore;
        let loc = mkLoc startPos p.prevEndPos in
        Some (Ast_helper.Sig.rec_module ~loc recModule)
      | Typ ->
        Some (parseModuleTypeDeclaration ~attrs ~startPos p)
      | _t ->
        let modDecl = parseModuleDeclarationOrAlias ~attrs p in
        Parser.optional p Semicolon |> ignore;
        let loc = mkLoc startPos p.prevEndPos in
        Some (Ast_helper.Sig.module_ ~loc modDecl)
      end
    | AtAt ->
      let attr = parseStandaloneAttribute p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Sig.attribute ~loc attr)
    | PercentPercent ->
      let extension = parseExtension ~moduleLanguage:true p in
      Parser.optional p Semicolon |> ignore;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Sig.extension ~attrs ~loc extension)
    | Import ->
      Parser.next p;
      parseSignatureItemRegion p
    | _ ->
      None

  (* module rec module-name :  module-type  { and module-name:  module-type } *)
  and parseRecModuleSpec ~attrs ~startPos p =
    Parser.expect Rec p;
    let rec loop p spec =
      let startPos = p.Parser.startPos in
      let attrs = parseAttributesAndBinding p in
      match p.Parser.token with
      | And ->
        (* TODO: give a good error message when with constraint, no parens
         * and ASet: (Set.S with type elt = A.t)
         * and BTree: (Btree.S with type elt = A.t)
         * Without parens, the `and` signals the start of another
         * `with-constraint`
         *)
        Parser.expect And p;
        let decl = parseRecModuleDeclaration ~attrs ~startPos p in
        loop p (decl::spec)
      | _ ->
        List.rev spec
    in
    let first = parseRecModuleDeclaration ~attrs ~startPos p in
    loop p [first]

  (* module-name : module-type *)
  and parseRecModuleDeclaration ~attrs ~startPos p =
    let name = match p.Parser.token with
    | Uident modName ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc modName loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    Parser.expect Colon p;
    let modType = parseModuleType p in
    Ast_helper.Md.mk ~loc:(mkLoc startPos p.prevEndPos) ~attrs name modType

  and parseModuleDeclarationOrAlias ~attrs p =
    let startPos = p.Parser.startPos in
    let moduleName = match p.Parser.token with
    | Uident ident ->
      let loc = mkLoc p.Parser.startPos p.endPos in
      Parser.next p;
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    let body = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parseModuleType p
    | Equal ->
      Parser.next p;
      let lident = parseModuleLongIdent ~lowercase:false p in
      Ast_helper.Mty.alias lident
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.defaultModuleType()
    in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Md.mk ~loc ~attrs moduleName body

  and parseModuleTypeDeclaration ~attrs ~startPos p =
    Parser.expect Typ p;
    let moduleName = match p.Parser.token with
    | Uident ident ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc ident loc
    | Lident ident ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    let typ = match p.Parser.token with
    | Equal ->
      Parser.next p;
      Some (parseModuleType p)
    | _ -> None
    in
    let moduleDecl = Ast_helper.Mtd.mk ~attrs ?typ moduleName in
    Ast_helper.Sig.modtype ~loc:(mkLoc startPos p.prevEndPos) moduleDecl

  and parseSignLetDesc ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect Let p;
    let (name, loc) = parseLident p in
    let name = Location.mkloc name loc in
    Parser.expect Colon p;
    let typExpr = parsePolyTypeExpr p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Val.mk ~loc ~attrs name typExpr

(*    attr-id	::=	lowercase-ident
 	∣	  capitalized-ident
 	∣	  attr-id .  attr-id   *)
  and parseAttributeId p =
    let startPos = p.Parser.startPos in
    let rec loop p acc =
      match p.Parser.token with
      | Lident ident | Uident ident ->
        Parser.next p;
        let id = acc ^ ident in
        begin match p.Parser.token with
        | Dot -> Parser.next p; loop p (id ^ ".")
        | _ -> id
        end
      | token when Token.isKeyword token ->
        Parser.next p;
        let id = acc ^ (Token.toString token) in
        begin match p.Parser.token with
        | Dot -> Parser.next p; loop p (id ^ ".")
        | _ -> id
        end
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        acc
    in
    let id = loop p "" in
    let endPos = p.prevEndPos in
    Location.mkloc id (mkLoc startPos endPos)

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
  and parsePayload p =
    match p.Parser.token with
    | Lparen when p.startPos.pos_cnum = p.prevEndPos.pos_cnum  ->
      Parser.next p;
      begin match p.token with
      | Colon ->
        Parser.next p;
        let typ = parseTypExpr p in
        Parser.expect Rparen p;
        Parsetree.PTyp typ
      | _ ->
        let items = parseDelimitedRegion
          ~grammar:Grammar.Structure
          ~closing:Rparen
          ~f:parseStructureItemRegion
          p
        in
        Parser.expect Rparen p;
        Parsetree.PStr items
      end
    | _ -> Parsetree.PStr []

  (* type attribute = string loc * payload *)
  and parseAttribute p =
    match p.Parser.token with
    | At ->
      Parser.next p;
      let attrId = parseAttributeId p in
      let payload = parsePayload p in
      Some(attrId, payload)
    | _ -> None

  and parseAttributes p =
    parseRegion p
      ~grammar:Grammar.Attribute
      ~f:parseAttribute

  (*
   * standalone-attribute ::=
   *  | @@ atribute-id
   *  | @@ attribute-id ( structure-item )
   *)
  and parseStandaloneAttribute p =
    Parser.expect AtAt p;
    let attrId = parseAttributeId p in
    let payload = parsePayload p in
    (attrId, payload)

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
  and parseExtension ?(moduleLanguage=false) p =
    if moduleLanguage then
      Parser.expect PercentPercent p
    else
      Parser.expect Percent p;
    let attrId = parseAttributeId p in
    let payload = parsePayload p in
    (attrId, payload)
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
  let escapeStringContents s =
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

    let rec printOutIdentDoc (ident : Outcometree.out_ident) =
      match ident with
      | Oide_ident s -> Doc.text s
      | Oide_dot (ident, s) -> Doc.concat [
          printOutIdentDoc ident;
          Doc.dot;
          Doc.text s;
        ]
      | Oide_apply (call, arg) ->Doc.concat [
          printOutIdentDoc call;
          Doc.lparen;
          printOutIdentDoc arg;
          Doc.rparen;
        ]

  let printOutAttributeDoc (outAttribute: Outcometree.out_attribute) =
    Doc.concat [
      Doc.text "@";
      Doc.text outAttribute.oattr_name;
    ]

  let printOutAttributesDoc (attrs: Outcometree.out_attribute list) =
    match attrs with
    | [] -> Doc.nil
    | attrs ->
      Doc.concat [
        Doc.group (
          Doc.join ~sep:Doc.line (List.map printOutAttributeDoc attrs)
        );
        Doc.line;
      ]

  let rec collectArrowArgs (outType: Outcometree.out_type) args =
    match outType with
    | Otyp_arrow (label, argType, returnType) ->
      let arg = (label, argType) in
      collectArrowArgs returnType (arg::args)
    | _ as returnType ->
      (List.rev args, returnType)

  let rec collectFunctorArgs (outModuleType: Outcometree.out_module_type) args =
    match outModuleType with
    | Omty_functor (lbl, optModType, returnModType) ->
      let arg = (lbl, optModType) in
      collectFunctorArgs returnModType (arg::args)
    | _ ->
      (List.rev args, outModuleType)

  let rec printOutTypeDoc (outType: Outcometree.out_type) =
    match outType with
    | Otyp_abstract | Otyp_variant _ (* don't support poly-variants atm *) | Otyp_open -> Doc.nil
    | Otyp_alias (typ, aliasTxt) ->
      Doc.concat [
        printOutTypeDoc typ;
        Doc.text " as '";
        Doc.text aliasTxt
      ]
    | Otyp_constr (outIdent, []) ->
      printOutIdentDoc outIdent
    | Otyp_manifest (typ1, typ2) ->
        Doc.concat [
          printOutTypeDoc typ1;
          Doc.text " = ";
          printOutTypeDoc typ2;
        ]
    | Otyp_record record ->
      printRecordDeclarationDoc ~inline:true record
    | Otyp_stuff txt -> Doc.text txt
    | Otyp_var (ng, s) -> Doc.concat [
        Doc.text ("'" ^ (if ng then "_" else ""));
        Doc.text s
      ]
    | Otyp_object (fields, rest) -> printObjectFields fields rest
    | Otyp_class _ -> Doc.nil
    | Otyp_attribute (typ, attribute) ->
      Doc.group (
        Doc.concat [
          printOutAttributeDoc attribute;
          Doc.line;
          printOutTypeDoc typ;
        ]
      )
    (* example: Red | Blue | Green | CustomColour(float, float, float) *)
    | Otyp_sum constructors ->
      printOutConstructorsDoc constructors

    (* example: {"name": string, "age": int} *)
    | Otyp_constr (
        (Oide_dot ((Oide_ident "Js"), "t")),
        [Otyp_object (fields, rest)]
      ) -> printObjectFields fields rest

    (* example: node<root, 'value> *)
    | Otyp_constr (outIdent, args) ->
      let argsDoc = match args with
      | [] -> Doc.nil
      | args ->
        Doc.concat [
          Doc.lessThan;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printOutTypeDoc args
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.greaterThan;
        ]
      in
      Doc.group (
        Doc.concat [
          printOutIdentDoc outIdent;
          argsDoc;
        ]
      )
    | Otyp_tuple tupleArgs ->
      Doc.group (
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printOutTypeDoc tupleArgs
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ]
      )
    | Otyp_poly (vars, outType) ->
      Doc.group (
        Doc.concat [
          Doc.join ~sep:Doc.space (
            List.map (fun var -> Doc.text ("'" ^ var)) vars
          );
          printOutTypeDoc outType;
        ]
      )
    | Otyp_arrow _ as typ ->
      let (typArgs, typ) = collectArrowArgs typ [] in
      let args = Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
        List.map (fun (lbl, typ) ->
          if lbl = "" then
            printOutTypeDoc typ
          else
            Doc.group (
              Doc.concat [
                Doc.text ("~" ^ lbl ^ ": ");
                printOutTypeDoc typ
              ]
            )
        ) typArgs
      ) in
      let argsDoc =
        let needsParens = match typArgs with
        | [_, (Otyp_tuple _ | Otyp_arrow _)] -> true
        (* single argument should not be wrapped *)
        | ["", _] -> false
        | _ -> true
        in
        if needsParens then
          Doc.group (
            Doc.concat [
              Doc.lparen;
              Doc.indent (
                Doc.concat [
                  Doc.softLine;
                  args;
                ]
              );
              Doc.trailingComma;
              Doc.softLine;
              Doc.rparen;
            ]
          )
        else args
      in
      Doc.concat [
        argsDoc;
        Doc.text " => ";
        printOutTypeDoc typ;
      ]
    | Otyp_module (_modName, _stringList, _outTypes) ->
        Doc.nil

  and printObjectFields fields rest =
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
            Doc.softLine;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
              List.map (fun (lbl, outType) -> Doc.group (
                Doc.concat [
                  Doc.text ("\"" ^ lbl ^ "\": ");
                  printOutTypeDoc outType;
                ]
              )) fields
            )
          ]
        );
        Doc.softLine;
        Doc.trailingComma;
        Doc.rbrace;
      ]
    )


  and printOutConstructorsDoc constructors =
    Doc.group (
      Doc.indent (
        Doc.concat [
          Doc.line;
          Doc.join ~sep:Doc.line (
            List.mapi (fun i constructor ->
              Doc.concat [
                if i > 0 then Doc.text "| " else Doc.ifBreaks (Doc.text "| ") Doc.nil;
                printOutConstructorDoc constructor;
              ]
            ) constructors
          )
        ]
      )
    )

  and printOutConstructorDoc (name, args, gadt) =
      let gadtDoc = match gadt with
      | Some outType ->
        Doc.concat [
          Doc.text ": ";
          printOutTypeDoc outType
        ]
      | None -> Doc.nil
      in
      let argsDoc = match args with
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
            printRecordDeclarationDoc ~inline:true record;
          );
          Doc.rparen;
        ]
      | _types ->
        Doc.indent (
          Doc.concat [
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map printOutTypeDoc args
                )
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
            Doc.rparen;
          ]
        )
      in
      Doc.group (
        Doc.concat [
          Doc.text name;
          argsDoc;
          gadtDoc
        ]
      )

  and printRecordDeclRowDoc (name, mut, arg) =
    Doc.group (
      Doc.concat [
        if mut then Doc.text "mutable " else Doc.nil;
        Doc.text name;
        Doc.text ": ";
        printOutTypeDoc arg;
      ]
    )

  and printRecordDeclarationDoc ~inline rows =
    let content = Doc.concat [
      Doc.lbrace;
      Doc.indent (
        Doc.concat [
          Doc.softLine;
          Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
            List.map printRecordDeclRowDoc rows
          )
        ]
      );
      Doc.trailingComma;
      Doc.softLine;
      Doc.rbrace;
    ] in
    if not inline then
      Doc.group content
    else content

  let printOutType fmt outType =
    Format.pp_print_string fmt
      (Doc.toString ~width:80 (printOutTypeDoc outType))

  let printTypeParameterDoc (typ, (co, cn)) =
    Doc.concat [
      if not cn then Doc.text "+" else if not co then Doc.text "-" else Doc.nil;
      if typ = "_" then Doc.text "_" else Doc.text ("'" ^ typ)
    ]


  let rec printOutSigItemDoc (outSigItem : Outcometree.out_sig_item) =
    match outSigItem with
    | Osig_class _ | Osig_class_type _ -> Doc.nil
    | Osig_ellipsis -> Doc.dotdotdot
    | Osig_value valueDecl ->
      Doc.group (
        Doc.concat [
          printOutAttributesDoc valueDecl.oval_attributes;
          Doc.text (
            match valueDecl.oval_prims with | [] -> "let " | _ -> "external "
          );
          Doc.text valueDecl.oval_name;
          Doc.text ":";
          Doc.space;
          printOutTypeDoc valueDecl.oval_type;
          match valueDecl.oval_prims with
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
  | Osig_typext (outExtensionConstructor, _outExtStatus) ->
    printOutExtensionConstructorDoc outExtensionConstructor
  | Osig_modtype (modName, Omty_signature []) ->
    Doc.concat [
      Doc.text "module type ";
      Doc.text modName;
    ]
  | Osig_modtype (modName, outModuleType) ->
    Doc.group (
      Doc.concat [
        Doc.text "module type ";
        Doc.text modName;
        Doc.text " = ";
        printOutModuleTypeDoc outModuleType;
      ]
    )
  | Osig_module (modName, Omty_alias ident, _) ->
    Doc.group (
      Doc.concat [
        Doc.text "module ";
        Doc.text modName;
        Doc.text " =";
        Doc.line;
        printOutIdentDoc ident;
      ]
    )
  | Osig_module (modName, outModType, outRecStatus) ->
     Doc.group (
      Doc.concat [
        Doc.text (
          match outRecStatus with
          | Orec_not -> "module "
          | Orec_first -> "module rec "
          | Orec_next -> "and"
        );
        Doc.text modName;
        Doc.text " = ";
        printOutModuleTypeDoc outModType;
      ]
    )
  | Osig_type (outTypeDecl, outRecStatus) ->
    (* TODO: manifest ? *)
    let attrs = match outTypeDecl.otype_immediate, outTypeDecl.otype_unboxed with
    | false, false -> Doc.nil
    | true, false ->
      Doc.concat [Doc.text "@immediate"; Doc.line]
    | false, true ->
      Doc.concat [Doc.text "@unboxed"; Doc.line]
    | true, true ->
      Doc.concat [Doc.text "@immediate @unboxed"; Doc.line]
    in
    let kw = Doc.text (
      match outRecStatus with
      | Orec_not -> "type "
      | Orec_first -> "type rec "
      | Orec_next -> "and "
    ) in
    let typeParams = match outTypeDecl.otype_params with
    | [] -> Doc.nil
    | _params -> Doc.group (
        Doc.concat [
          Doc.lessThan;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printTypeParameterDoc outTypeDecl.otype_params
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.greaterThan;
        ]
      )
    in
    let privateDoc = match outTypeDecl.otype_private with
    | Asttypes.Private -> Doc.text "private "
    | Public -> Doc.nil
    in
    let kind = match outTypeDecl.otype_type with
    | Otyp_open -> Doc.concat [
        Doc.text " = ";
        privateDoc;
        Doc.text "..";
      ]
    | Otyp_abstract -> Doc.nil
    | Otyp_record record -> Doc.concat [
        Doc.text " = ";
        privateDoc;
        printRecordDeclarationDoc ~inline:false record;
      ]
    | typ -> Doc.concat [
        Doc.text " = ";
        printOutTypeDoc typ
      ]
    in
    let constraints =  match outTypeDecl.otype_cstrs with
    | [] -> Doc.nil
    | _ -> Doc.group (
      Doc.concat [
        Doc.line;
        Doc.indent (
          Doc.concat [
            Doc.hardLine;
            Doc.join ~sep:Doc.line (List.map (fun (typ1, typ2) ->
              Doc.group (
                Doc.concat [
                  Doc.text "constraint ";
                  printOutTypeDoc typ1;
                  Doc.text " =";
                  Doc.indent (
                    Doc.concat [
                      Doc.line;
                      printOutTypeDoc typ2;
                    ]
                  )
                ]
              )
            ) outTypeDecl.otype_cstrs)
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
            Doc.text outTypeDecl.otype_name;
            typeParams;
            kind
          ]
        );
        constraints
      ]
    )

  and printOutModuleTypeDoc (outModType : Outcometree.out_module_type) =
    match outModType with
    | Omty_abstract -> Doc.nil
    | Omty_ident ident -> printOutIdentDoc ident
    (* example: module Increment = (M: X_int) => X_int *)
    | Omty_functor _ ->
      let (args, returnModType) = collectFunctorArgs outModType [] in
      let argsDoc = match args with
      | [_, None] -> Doc.text "()"
      | args ->
        Doc.group (
          Doc.concat [
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map (fun (lbl, optModType) -> Doc.group (
                    Doc.concat [
                      Doc.text lbl;
                      match optModType with
                      | None -> Doc.nil
                      | Some modType -> Doc.concat [
                          Doc.text ": ";
                          printOutModuleTypeDoc modType;
                        ]
                    ]
                  )) args
                )
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
            Doc.rparen;
          ]
        )
      in
      Doc.group (
        Doc.concat [
          argsDoc;
          Doc.text " => ";
          printOutModuleTypeDoc returnModType
        ]
      )
    | Omty_signature [] -> Doc.nil
    | Omty_signature signature ->
      Doc.breakableGroup ~forceBreak:true (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.line;
              printOutSignatureDoc signature;
            ]
          );
          Doc.softLine;
          Doc.rbrace;
        ]
      )
    | Omty_alias _ident -> Doc.nil

  and printOutSignatureDoc (signature : Outcometree.out_sig_item list) =
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
        let doc = printOutTypeExtensionDoc te in
        loop items (doc::acc)
      | item::items ->
        let doc = printOutSigItemDoc item in
        loop items (doc::acc)
    in
    match loop signature [] with
    | [doc] -> doc
    | docs ->
      Doc.breakableGroup ~forceBreak:true (
        Doc.join ~sep:Doc.line docs
      )

  and printOutExtensionConstructorDoc (outExt : Outcometree.out_extension_constructor) =
    let typeParams = match outExt.oext_type_params with
    | [] -> Doc.nil
    | params ->
      Doc.group(
        Doc.concat [
          Doc.lessThan;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (List.map
                (fun ty -> Doc.text (if ty = "_" then ty else "'" ^ ty))
                params

              )
            ]
          );
          Doc.softLine;
          Doc.greaterThan;
        ]
      )

    in
    Doc.group (
      Doc.concat [
        Doc.text "type ";
        Doc.text outExt.oext_type_name;
        typeParams;
        Doc.text " +=";
        Doc.line;
        if outExt.oext_private = Asttypes.Private then
          Doc.text "private "
        else
          Doc.nil;
        printOutConstructorDoc
          (outExt.oext_name, outExt.oext_args, outExt.oext_ret_type)
      ]
    )

  and printOutTypeExtensionDoc (typeExtension : Outcometree.out_type_extension) =
    let typeParams = match typeExtension.otyext_params with
    | [] -> Doc.nil
    | params ->
      Doc.group(
        Doc.concat [
          Doc.lessThan;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (List.map
                (fun ty -> Doc.text (if ty = "_" then ty else "'" ^ ty))
                params

              )
            ]
          );
          Doc.softLine;
          Doc.greaterThan;
        ]
      )

    in
    Doc.group (
      Doc.concat [
        Doc.text "type ";
        Doc.text typeExtension.otyext_name;
        typeParams;
        Doc.text " +=";
        if typeExtension.otyext_private = Asttypes.Private then
          Doc.text "private "
        else
          Doc.nil;
        printOutConstructorsDoc typeExtension.otyext_constructors;
      ]
    )

  let printOutSigItem fmt outSigItem =
    Format.pp_print_string fmt
      (Doc.toString ~width:80 (printOutSigItemDoc outSigItem))

  let printOutSignature fmt signature =
    Format.pp_print_string fmt
      (Doc.toString ~width:80 (printOutSignatureDoc signature))

  let validFloatLexeme s =
    let l = String.length s in
    let rec loop i =
      if i >= l then s ^ "." else
      match (s.[i] [@doesNotRaise]) with
      | '0' .. '9' | '-' -> loop (i+1)
      | _ -> s
    in loop 0

  let floatRepres f =
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
      in validFloatLexeme float_val

  let rec printOutValueDoc (outValue : Outcometree.out_value) =
    match outValue with
    | Oval_array outValues ->
      Doc.group (
        Doc.concat [
          Doc.lbracket;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printOutValueDoc outValues
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rbracket;
        ]
      )
    | Oval_char c -> Doc.text ("'" ^ (Char.escaped c) ^ "'")
    | Oval_constr (outIdent, outValues) ->
      Doc.group (
        Doc.concat [
          printOutIdentDoc outIdent;
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printOutValueDoc outValues
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ]
      )
    | Oval_ellipsis -> Doc.text "..."
    | Oval_int i -> Doc.text (Format.sprintf "%i" i)
    | Oval_int32 i -> Doc.text (Format.sprintf "%lil" i)
    | Oval_int64 i -> Doc.text (Format.sprintf "%LiL" i)
    | Oval_nativeint i -> Doc.text (Format.sprintf "%nin" i)
    | Oval_float f -> Doc.text (floatRepres f)
    | Oval_list outValues ->
      Doc.group (
        Doc.concat [
          Doc.text "list[";
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printOutValueDoc outValues
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
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
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map (fun (outIdent, outValue) -> Doc.group (
                    Doc.concat [
                      printOutIdentDoc outIdent;
                      Doc.text ": ";
                      printOutValueDoc outValue;
                    ]
                  )
                ) rows
              );
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ]
      )
    | Oval_string (txt, _sizeToPrint, _kind) ->
      Doc.text (escapeStringContents txt)
    | Oval_stuff txt -> Doc.text txt
    | Oval_tuple outValues ->
      Doc.group (
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printOutValueDoc outValues
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ]
      )
    (* Not supported by NapkinScript *)
    | Oval_variant _ -> Doc.nil

  let printOutExceptionDoc exc outValue =
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
            printOutValueDoc outValue;
          ]
        )
      )

  let printOutPhraseSignature signature =
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
        let doc = printOutTypeExtensionDoc te in
        loop signature (doc::acc)
     | (sigItem, optOutValue)::signature ->
       let doc = match optOutValue with
        | None ->
          printOutSigItemDoc sigItem
        | Some outValue ->
          Doc.group (
            Doc.concat [
              printOutSigItemDoc sigItem;
              Doc.text " = ";
              printOutValueDoc outValue;
            ]
          )
       in
       loop signature (doc::acc)
     in
     Doc.breakableGroup ~forceBreak:true (
       Doc.join ~sep:Doc.line (loop signature [])
     )

  let printOutPhraseDoc (outPhrase : Outcometree.out_phrase) =
    match outPhrase with
    | Ophr_eval (outValue, outType) ->
      Doc.group (
        Doc.concat [
          Doc.text "- : ";
          printOutTypeDoc outType;
          Doc.text " =";
          Doc.indent (
            Doc.concat [
              Doc.line;
              printOutValueDoc outValue;
            ]
          )
        ]
      )
    | Ophr_signature [] -> Doc.nil
    | Ophr_signature signature -> printOutPhraseSignature signature
    | Ophr_exception (exc, outValue) ->
      printOutExceptionDoc exc outValue

  let printOutPhase fmt outPhrase =
    Format.pp_print_string fmt
      (Doc.toString ~width:80 (printOutPhraseDoc outPhrase))

  let printOutModuleType fmt outModuleType =
    Format.pp_print_string fmt
      (Doc.toString ~width:80 (printOutModuleTypeDoc outModuleType))

  let printOutTypeExtension fmt typeExtension =
    Format.pp_print_string fmt
      (Doc.toString ~width:80 (printOutTypeExtensionDoc typeExtension))

  let printOutValue fmt outValue =
    Format.pp_print_string fmt
      (Doc.toString ~width:80 (printOutValueDoc outValue))

  (* Not supported in Napkin *)
  let printOutClassType _fmt _ = ()

  let out_value = ref printOutValue
  let out_type = ref printOutType
  let out_module_type = ref printOutModuleType
  let out_sig_item = ref printOutSigItem
  let out_signature = ref printOutSignature
  let out_type_extension = ref printOutTypeExtension
  let out_phrase = ref printOutPhase [@live]
  let out_class_type =  ref printOutClassType
end

module Repl = struct
  let parseToplevelPhrase filename =
    let src = IO.readFile filename in
    let p = Parser.make src filename in
    Parsetree.Ptop_def (NapkinScript.parseImplementation p)

  let typeAndPrintOutcome filename =
    Compmisc.init_path false;
    let env = Compmisc.initial_env () in
    try
      let sstr = match parseToplevelPhrase filename with
      | Parsetree.Ptop_def sstr -> sstr
      | _ -> assert false
      in
      let (_str, signature, _newenv) = Typemod.type_toplevel_phrase env sstr in
      let outSigItems = Printtyp.tree_of_signature signature in
      let fmt = Format.str_formatter in
      !OutcomePrinter.out_signature fmt outSigItems;
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
  let addFilename filename = files := filename::(!files)

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
    ("-outcome", Arg.Bool (fun printOutcomeTree -> outcome := printOutcomeTree), "print outcometree");
    ("-width", Arg.Int (fun w -> width := w), "Specify the line length that the printer will wrap on" );
    ("-interface", Arg.Unit (fun () -> interface := true), "Parse as interface");
    ("-report", Arg.String (fun txt -> report := txt), "Stylize errors and messages using color and context. Accepts `Pretty` and `Plain`. Default `Plain`")
  ]

  let parse () = Arg.parse spec addFilename usage
end

module Driver: sig
  val processFile:
       isInterface: bool
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

  let parseNapkin (type a) (kind : a file_kind) p : a =
    match kind with
    | Structure -> NapkinScript.parseImplementation p
    | Signature -> NapkinScript.parseSpecification p

  let extractOcamlStringData filename =
    let lexbuf = if String.length filename > 0 then
      IO.readFile filename |> Lexing.from_string
    else
      Lexing.from_channel stdin
    in
    let stringLocs = ref [] in
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
        stringLocs := (txt, loc)::(!stringLocs);
        next();
      | OcamlParser.EOF -> ()
      | _ -> next()
    in
    next();
    List.rev !stringLocs

  let parseOcaml (type a) (kind : a file_kind) filename : a =
    let lexbuf = if String.length filename > 0 then
      IO.readFile filename |> Lexing.from_string
    else
      Lexing.from_channel stdin
    in
    let stringData = extractOcamlStringData filename in
    match kind with
    | Structure ->
      Parse.implementation lexbuf
      |> ParsetreeCompatibility.replaceStringLiteralStructure stringData
      |> ParsetreeCompatibility.structure
    | Signature ->
      Parse.interface lexbuf
      |> ParsetreeCompatibility.replaceStringLiteralSignature stringData
      |> ParsetreeCompatibility.signature

  let parseNapkinFile ~destination kind filename =
    let src = if String.length filename > 0 then
      IO.readFile filename
    else
      IO.readStdin ()
    in
    let p =
      let mode = match destination with
      | "napkinscript" | "ns" | "sexp" -> Parser.Default
      | _ -> Parser.ParseForTypeChecker
      in
      Parser.make ~mode src filename in
    let ast = parseNapkin kind p in
    let report = match p.diagnostics with
    | [] -> None
    | diagnostics -> Some(diagnostics)
    in
    (ast, report, p)

  let parseOcamlFile kind filename =
    let ast = parseOcaml kind filename in
    let lexbuf2 = if String.length filename > 0 then
      IO.readFile filename |> Lexing.from_string
    else
      Lexing.from_channel stdin
    in
    let comments =
      let rec next (prevTokEndPos : Lexing.position) comments lb =
        let token = Lexer.token_with_comments lb in
        match token with
        | OcamlParser.EOF -> comments
        | OcamlParser.COMMENT (txt, loc) ->
          let comment = Comment.fromOcamlComment
            ~loc
            ~prevTokEndPos
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

  let reasonFilename = ref ""
  let commentData = ref []
  let stringData = ref []

  let parseReasonBinaryFromStdin (type a) (kind : a file_kind) filename  :a  =
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
    reasonFilename := filename;
    let ast = input_value ic in
    close chan;

    let src =
      if String.length filename > 0 then IO.readFile filename
      else IO.readStdin ()
    in

    let scanner = Scanner.make (Bytes.of_string src) filename in

    let rec next prevEndPos scanner =
      let (startPos, endPos, token) = Scanner.scan scanner in
      match token with
      | Eof -> ()
      | Comment c ->
        Comment.setPrevTokEndPos c prevEndPos;
        commentData := c::(!commentData);
        next endPos scanner
      | String _ ->
        let loc = {Location.loc_start = startPos; loc_end = endPos; loc_ghost = false} in
        let len = endPos.pos_cnum - startPos.pos_cnum in
        let txt = (String.sub [@doesNotRaise]) src startPos.pos_cnum len in
        stringData := (txt, loc)::(!stringData);
        next endPos scanner
      | _ ->
        next endPos scanner
    in

    next Lexing.dummy_pos scanner;

    match kind with
    | Structure ->
      ast
      |> ParsetreeCompatibility.replaceStringLiteralStructure !stringData
      |> ParsetreeCompatibility.normalizeReasonArityStructure ~forPrinter:true
      |> ParsetreeCompatibility.structure
    | Signature ->
      ast
      |> ParsetreeCompatibility.replaceStringLiteralSignature !stringData
      |> ParsetreeCompatibility.normalizeReasonAritySignature ~forPrinter:true
      |> ParsetreeCompatibility.signature

  let isReasonDocComment (comment: Comment.t) =
    let content = Comment.txt comment in
    let len = String.length content in
    if len = 0 then true
    else if len >= 2 && (String.unsafe_get content 0 = '*' && String.unsafe_get content 1 = '*') then false
    else if len >= 1 && (String.unsafe_get content 0 = '*') then true
    else false


  let parseReasonBinary kind filename =
    let ast = parseReasonBinaryFromStdin kind filename in
    let p = Parser.make "" !reasonFilename in
    p.comments <- List.filter (fun c -> not (isReasonDocComment c)) !commentData;
    (ast, None, p)

  let parseImplementation ~origin ~destination filename =
    match origin with
    | "ml" | "ocaml" ->
      parseOcamlFile Structure filename
    | "reasonBinary" ->
      parseReasonBinary Structure filename
    | _ ->
      parseNapkinFile ~destination Structure filename

  let parseInterface ~destination ~origin filename =
    match origin with
    | "ml" | "ocaml" ->
      parseOcamlFile Signature filename
    | "reasonBinary" ->
      parseReasonBinary Signature filename
    | _ ->
      parseNapkinFile ~destination Signature filename

  let process ~reportStyle parseFn printFn recover filename =
    let (ast, report, parserState) = parseFn filename in
    match report with
    | Some report when recover = true ->
      printFn ast parserState;
      prerr_string (
        Diagnostics.stringOfReport
          ~style:(Diagnostics.parseReportStyle reportStyle)
          report (Bytes.to_string parserState.Parser.scanner.src)
      );
    | Some report ->
      prerr_string (
        Diagnostics.stringOfReport
          ~style:(Diagnostics.parseReportStyle reportStyle)
          report (Bytes.to_string parserState.Parser.scanner.src)
      );
      exit 1
    | None ->
      printFn ast parserState

  type action =
    | ProcessImplementation
    | ProcessInterface

  let printImplementation ~target ~width filename ast _parserState =
    match target with
    | "ml" | "ocaml" ->
      Pprintast.structure Format.std_formatter ast
    | "ns" | "napkinscript" ->
      Printer.printImplementation ~width ast (List.rev _parserState.Parser.comments)
    | "ast" ->
      Printast.implementation Format.std_formatter ast
    | "sexp" ->
      ast |> SexpAst.implementation |> Sexp.toString |> print_string
    | _ -> (* default binary *)
      output_string stdout Config.ast_impl_magic_number;
      output_value stdout filename;
      output_value stdout ast

  let printInterface ~target ~width filename ast _parserState =
    match target with
    | "ml" | "ocaml" -> Pprintast.signature Format.std_formatter ast
    | "ns" | "napkinscript" ->
      Printer.printInterface ~width ast (List.rev _parserState.Parser.comments)
    | "ast" -> Printast.interface Format.std_formatter ast
    | "sexp" ->
      ast |> SexpAst.interface |> Sexp.toString |> print_string
    | _ -> (* default binary *)
      output_string stdout Config.ast_intf_magic_number;
      output_value stdout filename;
      output_value stdout ast

  let processFile ~isInterface ~width ~recover ~origin ~target ~report filename =
    try
      let len = String.length filename in
      let action =
        if isInterface || len > 0 && (String.get [@doesNotRaise]) filename (len - 1) = 'i' then
          ProcessInterface
        else ProcessImplementation
      in
      match action with
      | ProcessImplementation ->
        process
          ~reportStyle:report
          (parseImplementation ~origin ~destination:target)
          (printImplementation ~target ~width filename) recover filename
      | ProcessInterface ->
        process
          ~reportStyle:report
          (parseInterface ~origin ~destination:target)
          (printInterface ~target ~width filename) recover filename
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
    Repl.typeAndPrintOutcome (List.hd !Clflags.files)
  ) else (
    let () = match !Clflags.files with
    | (_file::_) as files ->
      List.iter (fun filename ->
        Driver.processFile
          ~isInterface:!Clflags.interface
          ~width:!Clflags.width
          ~recover:!Clflags.recover
          ~target:!Clflags.print
          ~origin:!Clflags.origin
          ~report:!Clflags.report
          filename
      ) files;
    | [] ->
      Driver.processFile
        ~isInterface:!Clflags.interface
        ~width:!Clflags.width
        ~recover:!Clflags.recover
        ~target:!Clflags.print
        ~origin:!Clflags.origin
        ~report:!Clflags.report
        ""
    in
    exit 0
  )