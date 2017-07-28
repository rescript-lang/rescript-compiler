(* it's not worth adding a dependency on parsing/location.ml(i) or
   compilerlibs just to support location printing, so we re-implement
   that here *)

open Lexing

(* We use a loosely structural type so that this bit of code can be
   easily reused by project that would wish it, without introducing
   any type-compatibility burden. *)
type source = string (* "file", "environment variable", "command-line option" ... *)
type location = source * position * position

let file loc = loc.pos_fname
let line loc = loc.pos_lnum
let char loc = loc.pos_cnum - loc.pos_bol

let print_loc ppf (source, start, end_) =
  let open Format in
  let print one_or_two ppf (start_num, end_num) =
    if one_or_two then fprintf ppf " %d" start_num
    else fprintf ppf "s %d-%d" start_num end_num in
  fprintf ppf "%s %S, line%a, character%a:@."
    (String.capitalize source)
    (file start)
    (print (line start = line end_))
      (line start, line end_)
    (print (line start = line end_ && char start = char end_))
      (char start, char end_)

let of_lexbuf source lexbuf =
  (source, lexbuf.lex_start_p, lexbuf.lex_curr_p)

let print_loc_option ppf = function
  | None -> ()
  | Some loc -> print_loc ppf loc
