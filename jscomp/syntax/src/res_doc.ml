module MiniBuffer = Res_minibuffer

type mode = Break | Flat

type lineStyle =
  | Classic (* fits? -> replace with space *)
  | Soft (* fits? -> replaced with nothing *)
  | Hard
  (* always included, forces breaks in parents *)
  (* always included, forces breaks in parents, but doesn't increase indentation
     use case: template literals, multiline string content *)
  | Literal

type t =
  | Nil
  | Text of string
  | Concat of t list
  | Indent of t
  | IfBreaks of {yes: t; no: t; mutable broken: bool}
    (* when broken is true, treat as the yes branch *)
  | LineSuffix of t
  | LineBreak of lineStyle
  | Group of {mutable shouldBreak: bool; doc: t}
  | CustomLayout of t list
  | BreakParent

let nil = Nil
let line = LineBreak Classic
let hardLine = LineBreak Hard
let softLine = LineBreak Soft
let literalLine = LineBreak Literal
let text s = Text s

(* Optimization. We eagerly collapse and reduce whatever allocation we can *)
let rec _concat acc l =
  match l with
  | Text s1 :: Text s2 :: rest -> Text (s1 ^ s2) :: _concat acc rest
  | Nil :: rest -> _concat acc rest
  | Concat l2 :: rest ->
    _concat (_concat acc rest) l2 (* notice the order here *)
  | x :: rest ->
    let rest1 = _concat acc rest in
    if rest1 == rest then l else x :: rest1
  | [] -> acc

let concat l = Concat (_concat [] l)

let indent d = Indent d
let ifBreaks t f = IfBreaks {yes = t; no = f; broken = false}
let lineSuffix d = LineSuffix d
let group d = Group {shouldBreak = false; doc = d}
let breakableGroup ~forceBreak d = Group {shouldBreak = forceBreak; doc = d}
let customLayout gs = CustomLayout gs
let breakParent = BreakParent

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
let trailingComma = ifBreaks comma nil
let doubleQuote = Text "\""

let propagateForcedBreaks doc =
  let rec walk doc =
    match doc with
    | Text _ | Nil | LineSuffix _ -> false
    | BreakParent -> true
    | LineBreak (Hard | Literal) -> true
    | LineBreak (Classic | Soft) -> false
    | Indent children ->
      let childForcesBreak = walk children in
      childForcesBreak
    | IfBreaks ({yes = trueDoc; no = falseDoc} as ib) ->
      let falseForceBreak = walk falseDoc in
      if falseForceBreak then (
        let _ = walk trueDoc in
        ib.broken <- true;
        true)
      else
        let forceBreak = walk trueDoc in
        forceBreak
    | Group ({shouldBreak = forceBreak; doc = children} as gr) ->
      let childForcesBreak = walk children in
      let shouldBreak = forceBreak || childForcesBreak in
      gr.shouldBreak <- shouldBreak;
      shouldBreak
    | Concat children ->
      List.fold_left
        (fun forceBreak child ->
          let childForcesBreak = walk child in
          forceBreak || childForcesBreak)
        false children
    | CustomLayout children ->
      (* When using CustomLayout, we don't want to propagate forced breaks
       * from the children up. By definition it picks the first layout that fits
       * otherwise it takes the last of the list.
       * However we do want to propagate forced breaks in the sublayouts. They
       * might need to be broken. We just don't propagate them any higher here *)
      let _ = walk (Concat children) in
      false
  in
  let _ = walk doc in
  ()

(* See documentation in interface file *)
let rec willBreak doc =
  match doc with
  | LineBreak (Hard | Literal) | BreakParent | Group {shouldBreak = true} ->
    true
  | Group {doc} | Indent doc | CustomLayout (doc :: _) -> willBreak doc
  | Concat docs -> List.exists willBreak docs
  | IfBreaks {yes; no} -> willBreak yes || willBreak no
  | _ -> false

let join ~sep docs =
  let rec loop acc sep docs =
    match docs with
    | [] -> List.rev acc
    | [x] -> List.rev (x :: acc)
    | x :: xs -> loop (sep :: x :: acc) sep xs
  in
  concat (loop [] sep docs)

let joinWithSep docsWithSep =
  let rec loop acc docs =
    match docs with
    | [] -> List.rev acc
    | [(x, _sep)] -> List.rev (x :: acc)
    | (x, sep) :: xs -> loop (sep :: x :: acc) xs
  in
  concat (loop [] docsWithSep)

let fits w stack =
  let width = ref w in
  let result = ref None in

  let rec calculate indent mode doc =
    match (mode, doc) with
    | _ when result.contents != None -> ()
    | _ when width.contents < 0 -> result := Some false
    | _, Nil | _, LineSuffix _ | _, BreakParent -> ()
    | _, Text txt -> width := width.contents - String.length txt
    | _, Indent doc -> calculate (indent + 2) mode doc
    | Flat, LineBreak Hard | Flat, LineBreak Literal -> result := Some true
    | Flat, LineBreak Classic -> width := width.contents - 1
    | Flat, LineBreak Soft -> ()
    | Break, LineBreak _ -> result := Some true
    | _, Group {shouldBreak = true; doc} -> calculate indent Break doc
    | _, Group {doc} -> calculate indent mode doc
    | _, IfBreaks {yes = breakDoc; broken = true} ->
      calculate indent mode breakDoc
    | Break, IfBreaks {yes = breakDoc} -> calculate indent mode breakDoc
    | Flat, IfBreaks {no = flatDoc} -> calculate indent mode flatDoc
    | _, Concat docs -> calculateConcat indent mode docs
    | _, CustomLayout (hd :: _) ->
      (* TODO: if we have nested custom layouts, what we should do here? *)
      calculate indent mode hd
    | _, CustomLayout [] -> ()
  and calculateConcat indent mode docs =
    if result.contents == None then
      match docs with
      | [] -> ()
      | doc :: rest ->
        calculate indent mode doc;
        calculateConcat indent mode rest
  in
  let rec calculateAll stack =
    match (result.contents, stack) with
    | Some r, _ -> r
    | None, [] -> !width >= 0
    | None, (indent, mode, doc) :: rest ->
      calculate indent mode doc;
      calculateAll rest
  in
  calculateAll stack

let toString ~width doc =
  propagateForcedBreaks doc;
  let buffer = MiniBuffer.create 1000 in

  let rec process ~pos lineSuffices stack =
    match stack with
    | ((ind, mode, doc) as cmd) :: rest -> (
      match doc with
      | Nil | BreakParent -> process ~pos lineSuffices rest
      | Text txt ->
        MiniBuffer.add_string buffer txt;
        process ~pos:(String.length txt + pos) lineSuffices rest
      | LineSuffix doc -> process ~pos ((ind, mode, doc) :: lineSuffices) rest
      | Concat docs ->
        let ops = List.map (fun doc -> (ind, mode, doc)) docs in
        process ~pos lineSuffices (List.append ops rest)
      | Indent doc -> process ~pos lineSuffices ((ind + 2, mode, doc) :: rest)
      | IfBreaks {yes = breakDoc; broken = true} ->
        process ~pos lineSuffices ((ind, mode, breakDoc) :: rest)
      | IfBreaks {yes = breakDoc; no = flatDoc} ->
        if mode = Break then
          process ~pos lineSuffices ((ind, mode, breakDoc) :: rest)
        else process ~pos lineSuffices ((ind, mode, flatDoc) :: rest)
      | LineBreak lineStyle ->
        if mode = Break then
          match lineSuffices with
          | [] ->
            if lineStyle = Literal then (
              MiniBuffer.add_char buffer '\n';
              process ~pos:0 [] rest)
            else (
              MiniBuffer.flush_newline buffer;
              MiniBuffer.add_string buffer (String.make ind ' ' [@doesNotRaise]);
              process ~pos:ind [] rest)
          | _docs ->
            process ~pos:ind []
              (List.concat [List.rev lineSuffices; cmd :: rest])
        else
          (* mode = Flat *)
          let pos =
            match lineStyle with
            | Classic ->
              MiniBuffer.add_string buffer " ";
              pos + 1
            | Hard ->
              MiniBuffer.flush_newline buffer;
              0
            | Literal ->
              MiniBuffer.add_char buffer '\n';
              0
            | Soft -> pos
          in
          process ~pos lineSuffices rest
      | Group {shouldBreak; doc} ->
        if shouldBreak || not (fits (width - pos) ((ind, Flat, doc) :: rest))
        then process ~pos lineSuffices ((ind, Break, doc) :: rest)
        else process ~pos lineSuffices ((ind, Flat, doc) :: rest)
      | CustomLayout docs ->
        let rec findGroupThatFits groups =
          match groups with
          | [] -> Nil
          | [lastGroup] -> lastGroup
          | doc :: docs ->
            if fits (width - pos) ((ind, Flat, doc) :: rest) then doc
            else findGroupThatFits docs
        in
        let doc = findGroupThatFits docs in
        process ~pos lineSuffices ((ind, Flat, doc) :: rest))
    | [] -> (
      match lineSuffices with
      | [] -> ()
      | suffices -> process ~pos:0 [] (List.rev suffices))
  in
  process ~pos:0 [] [(0, Flat, doc)];
  MiniBuffer.contents buffer

let debug t =
  let rec toDoc = function
    | Nil -> text "nil"
    | BreakParent -> text "breakparent"
    | Text txt -> text ("text(\"" ^ txt ^ "\")")
    | LineSuffix doc ->
      group
        (concat
           [
             text "linesuffix(";
             indent (concat [line; toDoc doc]);
             line;
             text ")";
           ])
    | Concat [] -> text "concat()"
    | Concat docs ->
      group
        (concat
           [
             text "concat(";
             indent
               (concat
                  [
                    line;
                    join ~sep:(concat [text ","; line]) (List.map toDoc docs);
                  ]);
             line;
             text ")";
           ])
    | CustomLayout docs ->
      group
        (concat
           [
             text "customLayout(";
             indent
               (concat
                  [
                    line;
                    join ~sep:(concat [text ","; line]) (List.map toDoc docs);
                  ]);
             line;
             text ")";
           ])
    | Indent doc ->
      concat [text "indent("; softLine; toDoc doc; softLine; text ")"]
    | IfBreaks {yes = trueDoc; broken = true} -> toDoc trueDoc
    | IfBreaks {yes = trueDoc; no = falseDoc} ->
      group
        (concat
           [
             text "ifBreaks(";
             indent
               (concat
                  [line; toDoc trueDoc; concat [text ","; line]; toDoc falseDoc]);
             line;
             text ")";
           ])
    | LineBreak break ->
      let breakTxt =
        match break with
        | Classic -> "Classic"
        | Soft -> "Soft"
        | Hard -> "Hard"
        | Literal -> "Liteal"
      in
      text ("LineBreak(" ^ breakTxt ^ ")")
    | Group {shouldBreak; doc} ->
      group
        (concat
           [
             text "Group(";
             indent
               (concat
                  [
                    line;
                    text ("{shouldBreak: " ^ string_of_bool shouldBreak ^ "}");
                    concat [text ","; line];
                    toDoc doc;
                  ]);
             line;
             text ")";
           ])
  in
  let doc = toDoc t in
  toString ~width:10 doc |> print_endline
  [@@live]
