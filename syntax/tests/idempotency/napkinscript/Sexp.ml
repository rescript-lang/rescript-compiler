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

