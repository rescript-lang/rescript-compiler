module Token = Napkin_token
module Grammar = Napkin_grammar
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