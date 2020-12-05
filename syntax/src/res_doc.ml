module MiniBuffer = Res_minibuffer

type mode = Break | Flat

type lineStyle =
  | Classic (* fits? -> replace with space *)
  | Soft (* fits? -> replaced with nothing *)
  | Hard (* always included, forces breaks in parents *)
   (* always included, forces breaks in parents, but doesn't increase indentation
    use case: template literals, multiline string content *)
  | Literal

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

let nil = Nil
let line = LineBreak Classic
let hardLine = LineBreak Hard
let softLine = LineBreak Soft
let literalLine = LineBreak Literal
let text s = Text s
let concat l = Concat l
let indent d = Indent d
let ifBreaks t f = IfBreaks {yes = t; no = f}
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
let trailingComma = IfBreaks {yes = comma; no = nil}
let doubleQuote = Text "\""

let propagateForcedBreaks doc =
  let rec walk doc = match doc with
  | Text _ | Nil | LineSuffix _ ->
    (false, doc)
  | BreakParent ->
    (true, Nil)
  | LineBreak (Hard | Literal) ->
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

(* See documentation in interface file *)
let rec willBreak doc = match doc with
  | LineBreak (Hard | Literal) | BreakParent | Group {shouldBreak = true} -> true
  | Group {doc} | Indent doc | CustomLayout (doc::_) -> willBreak doc
  | Concat docs -> List.exists willBreak docs
  | IfBreaks {yes; no} -> willBreak yes || willBreak no
  | _ -> false

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
      if break = Hard || break = Literal then true
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
            if lineStyle = Literal then (
              MiniBuffer.add_char buffer '\n';
              process ~pos:0 [] rest
            ) else (
              MiniBuffer.flush_newline buffer;
              MiniBuffer.add_string buffer (String.make ind ' ' [@doesNotRaise]);
              process ~pos:ind [] rest
            )
          | _docs ->
            process ~pos:ind [] (List.concat [List.rev lineSuffices; cmd::rest])
          end
        ) else (* mode = Flat *) (
          let pos = match lineStyle with
          | Classic -> MiniBuffer.add_string buffer " "; pos + 1
          | Hard -> MiniBuffer.flush_newline buffer; 0
          | Literal -> MiniBuffer.add_char buffer '\n'; 0
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
  MiniBuffer.contents buffer


let debug t =
  let rec toDoc = function
    | Nil -> text "nil"
    | BreakParent -> text "breakparent"
    | Text txt -> text ("text(\"" ^ txt ^ "\")")
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
    | Concat [] -> text "concat()"
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
        | Literal -> "Liteal"
      in
      text ("LineBreak(" ^ breakTxt ^ ")")
    | Group {shouldBreak; doc} ->
      group(
        concat [
          text "Group(";
          indent (
            concat [
              line;
              text ("{shouldBreak: " ^ (string_of_bool shouldBreak) ^ "}");
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
