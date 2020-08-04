module Scanner = Napkin_scanner
module Diagnostics = Napkin_diagnostics
module Token = Napkin_token
module Grammar = Napkin_grammar
module Reporting = Napkin_reporting

module Comment = Napkin_comment

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
* in the parser's state. Every comment contains the end position of its
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

let nextTemplateLiteralToken p =
  let (startPos, endPos, token) = Scanner.scanTemplateLiteralToken p.scanner in
  p.token <- token;
  p.prevEndPos <- p.endPos;
  p.startPos <- startPos;
  p.endPos <- endPos

let checkProgress ~prevEndPos ~result p =
  if p.endPos == prevEndPos
  then None
  else Some result

let make ?(mode=ParseForTypeChecker) ?line src filename =
  let scanner = Scanner.make ~filename ?line (Bytes.of_string src) in
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
