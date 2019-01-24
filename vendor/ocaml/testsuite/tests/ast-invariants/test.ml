(* This test checks all ml files in the ocaml repository that are accepted
   by the parser satisfy [Ast_invariants].

   We don't check the invariants on the output of the parser, so this test
   is to ensure that the parser doesn't accept more than [Ast_invariants].
*)

let root = "../../.."
let () = assert (Sys.file_exists (Filename.concat root "VERSION"))

type _ kind =
  | Implem : Parsetree.structure kind
  | Interf : Parsetree.signature kind

let parse : type a. a kind -> Lexing.lexbuf -> a = function
  | Implem -> Parse.implementation
  | Interf -> Parse.interface

let invariants : type a. a kind -> a -> unit = function
  | Implem -> Ast_invariants.structure
  | Interf -> Ast_invariants.signature

let check_file kind fn =
  Warnings.parse_options false "-a";
  let ic = open_in fn in
  Location.input_name := fn;
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf fn;
  match parse kind lexbuf with
  | exception _ ->
    (* A few files don't parse as they are meant for the toplevel;
       ignore them *)
    close_in ic
  | ast ->
    close_in ic;
    try
      invariants kind ast
    with exn ->
      Location.report_exception Format.std_formatter exn

type file_kind =
  | Regular_file
  | Directory
  | Other

let kind fn =
  match Unix.lstat fn with
  | exception _ -> Other
  | { Unix.st_kind = Unix.S_DIR } -> Directory
  | { Unix.st_kind = Unix.S_REG } -> Regular_file
  | { Unix.st_kind = _          } -> Other

let rec walk dir =
  Array.iter
    (fun fn ->
       if fn = "" || fn.[0] = '.' then
         ()
       else begin
         let fn = Filename.concat dir fn in
         match kind fn with
         | Other -> ()
         | Directory -> walk fn
         | Regular_file ->
           if Filename.check_suffix fn ".mli" then
             check_file Interf fn
           else if Filename.check_suffix fn ".ml" then
             check_file Implem fn
       end)
    (Sys.readdir dir)

let () = walk root
