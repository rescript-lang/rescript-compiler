module Reporting = struct
  module TerminalDoc = struct
    type break =
      | IfNeed
      | Never
      | Always

    type document =
      | Nil
      | Group of break * document
      | Text of string
      | Indent of int * document
      | Append of document* document

    let group ?(break= IfNeed)  doc = Group (break, doc)
    let text txt = Text (txt)
    let indent i d = Indent (i, d)
    let append d1 d2 = Append (d1, d2)
    let nil = Nil

    type stack =
      | Empty
      | Cons of document* stack

    let push stack doc = Cons (doc, stack)

    type mode =
      | Flat
      | Break


    let rec fits w stack =
      match stack with
      | _ when w < 0 -> false
      | Empty  -> true
      | Cons (doc,stack) ->
        begin match doc with
         | Nil  -> fits w stack
         | Text txt ->
           fits (w - (String.length txt)) stack
         | Append (d1,d2) ->
           let stack =
             let stack = push stack d1 in
             push stack d2
           in
           fits w stack
         | Group (_,d) ->
           fits w (push stack d)
         | Indent (i,d) ->
           fits (w - i) (push stack d)
         end

    let toString ~width (doc : document) =
      let buffer = Buffer.create 100 in
      let rec loop stack mode offset =
        match stack with
        | Empty  -> ()
        | Cons (doc, rest) ->
          begin match doc with
           | Nil -> loop rest mode offset
           | Text txt ->
             Buffer.add_string buffer txt;
             loop rest mode (offset + (String.length txt))
           | Indent (i,doc) ->
             let indentation = String.make i ' ' in
             Buffer.add_string buffer indentation;
             loop (push rest doc) mode (offset + i)
           | Append (doc1,doc2) ->
              let rest = push rest doc2 in
              let rest = push rest
                (match mode = Flat with
                | true  -> Nil
                | false  -> text "\n")
              in
              let rest = push rest doc1 in
              loop rest mode offset
           | Group (break,doc) ->
             let rest = push rest doc in
             begin match break with
             | Always  -> loop rest Break offset
             | Never  -> loop rest Flat offset
             | IfNeed  ->
               if fits (width - offset) rest
               then loop rest Flat offset
               else loop rest Break offset
             end
            end
      in
      loop (push Empty doc) Flat 0;
      Buffer.contents buffer
  end

  type color =
    | NoColor
    | Red

  type style = {
    underline: bool;
    color: color;
  }

  let emptyStyle = {
    underline = false;
    color = NoColor;
  }

  let highlight ~from ~len txt =
    if from < 0 || (String.length txt) == 0 || (from >= String.length txt) then txt else
    let before = String.sub txt 0 from in
    let content =
      "\027[31m" ^ (String.sub txt from len) ^ "\027[0m"
    in
    let after = String.sub txt (from + len) (String.length txt - (from + len)) in
    before ^ content ^ after

  let underline ~from ~len txt =
    let open TerminalDoc in
    let indent = String.make from ' ' in
    let underline = String.make len '^' in
    let line = highlight ~from:0 ~len underline in
    group ~break:Always
      (append (text txt) (text (indent ^ line)))

  let applyStyle ~from ~len style txt =
    let open TerminalDoc in
        let colorizedText =
      if style.color <> NoColor then
        highlight ~from ~len txt
      else
        txt
    in
    underline ~from ~len colorizedText

  let parseContext stack =
    match stack with
    | ((Grammar.ExprOperand, _)::cs) ->
        begin match cs with
        | (ExprBinaryAfterOp _ as c, _)::cs ->
          Grammar.toString c
        | _ -> "a basic expression"
        end
    | ((c, _)::cs) ->
        Grammar.toString c
    | [] -> "your code"

  let rec drop n l =
    if n == 1 then l
    else drop (n - 1) (match l with | x::xs -> xs | _ -> l)

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
          | Some len -> x ^ (String.make 10 ' ')
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
          (append (text rowNr) (text " ‚îÇ"))
          (indent 2 line))
    in

    let reportDoc = ref TerminalDoc.nil in

    let linesLen = Array.length lines in
    for i = 0 to (linesLen - 1) do
      let line = Array.get lines i in
      reportDoc :=
        let open TerminalDoc in
        let ix = startLine + i in
        group ~break:Always (append !reportDoc (renderLine line ix))
    done;

    TerminalDoc.toString ~width:80 !reportDoc

  type problem =
    | Unexpected of Token.t
    | Expected of (Token.t * Lexing.position * Grammar.t option)
    | Message of string
    | Uident
    | Lident
    | Unbalanced of Token.t

  type parseError = Lexing.position * problem
end
