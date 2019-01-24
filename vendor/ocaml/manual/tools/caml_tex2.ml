(* $Id$ *)

open StdLabels
open Printf
open Str

let camlbegin = "\\caml"
let camlend = "\\endcaml"
let camlin = {|\\?\1|}
let camlout = {|\\:\1|}
let camlbunderline = "\\<"
let camleunderline = "\\>"

let start newline out s =
  Printf.fprintf out "%s%s" camlbegin s;
  if newline then Printf.fprintf out "\n"

let stop newline out s =
  Printf.fprintf out "%s%s" camlend s;
  if newline then Printf.fprintf out "\n"

let code_env ?(newline=true) env out s =
  Printf.fprintf out "%a%s\n%a"
    (start false) env s (stop newline) env

let main = "example"
let input_env = "input"
let ok_output ="output"
let error ="error"
let warning ="warn"
let phrase_env = ""


let camllight = ref "TERM=norepeat ocaml"
let verbose = ref true
let linelen = ref 72
let outfile = ref ""
let cut_at_blanks = ref false
let files = ref []

let _ =
  Arg.parse ["-n", Arg.Int (fun n -> linelen := n), "line length";
             "-o", Arg.String (fun s -> outfile := s), "output";
             "-caml", Arg.String (fun s -> camllight := s), "toplevel";
             "-w", Arg.Set cut_at_blanks, "cut at blanks";
             "-v", Arg.Bool (fun b -> verbose := b ), "output result on stderr"
            ]
    (fun s -> files := s :: !files)
    "caml-tex2: "

let (~!) =
  let memo = ref [] in
  fun key ->
    try List.assq key !memo
    with Not_found ->
      let data = Str.regexp key in
      memo := (key, data) :: !memo;
      data

(** The Output module deals with the analysis and classification
    of the interpreter output and the parsing of status-related options
    or annotations for the caml_example environment *)
module Output = struct

  (** Interpreter output status *)
  type status =
    | Ok
    | Warning of int
    | Error

  type kind =
    | Annotation (** Local annotation: [ [@@expect (*annotation*) ] ]*)
    | Option (** Global environment option:
                 [\begin{caml_example}[option[=value]]
                 ...
                 \end{caml_example}] *)

  (** Pretty printer for status *)
  let pp_status ppf = function
    | Error -> Printf.fprintf ppf "error"
    | Ok -> Printf.fprintf ppf "ok"
    | Warning n -> Printf.fprintf ppf "warning %d" n

  (** Pretty printer for status preceded with an undefined determinant *)
  let pp_a_status ppf = function
    | Error -> Printf.fprintf ppf "an error"
    | Ok -> Printf.fprintf ppf "an ok"
    | Warning n -> Printf.fprintf ppf "a warning %d" n

  (** {1 Related latex environment } *)
  let env = function
    | Error -> error
    | Warning _ -> warning
    | Ok -> ok_output

  (** {1 Exceptions } *)
  exception Parsing_error of kind * string

  type source = { file:string; lines:int * int; phrase:string; output:string }
  type unexpected_report = {source:source; expected:status; got:status}
  exception Unexpected_status of unexpected_report

  let print_source ppf {file; lines = (start, stop); phrase; output} =
    Printf.fprintf ppf "%s, lines %d to %d:\n\"\n%s\n%s\n\"."
      file start stop phrase output

  let print_unexpected {source; expected; got} =
    if expected = Ok then
      Printf.eprintf
        "Error when evaluating a caml_example environment in %a\n\
         Unexpected %a status.\n\
         If %a status was expected, add an [@@expect %a] annotation.\n"
        print_source source
        pp_status got
        pp_a_status got
        pp_status got
    else
      Printf.eprintf
        "Error when evaluating a guarded caml_example environment in %a\n\
         Unexpected %a status, %a status was expected.\n\
         If %a status was in fact expected, change the status annotation to \
         [@@expect %a].\n"
        print_source source
        pp_status got
        pp_a_status expected
        pp_a_status got
        pp_status got;
    flush stderr

  let print_parsing_error k s =
    match k with
    | Option ->
        Printf.eprintf
          "Unknown caml_example option: [%s].\n\
           Supported options are \"ok\",\"error\", or \"warning=n\" (with n \
           a warning number).\n" s
    | Annotation ->
        Printf.eprintf
          "Unknown caml_example phrase annotation: [@@expect %s].\n\
           Supported annotations are [@@expect ok], [@@expect error],\n\
           and [@@expect warning n] (with n a warning number).\n" s


  (** {1 Output analysis} *)
  let catch_error s =
    if string_match ~!{|Error:|} s 0 then Some Error else None

  let catch_warning s =
    if string_match ~!{|Warning \([0-9]+\):|} s 0 then
      Some (Warning (int_of_string @@ matched_group 1 s))
    else
      None

  let status s = match catch_warning s, catch_error s with
    | Some w, _ -> w
    | None, Some e -> e
    | None, None -> Ok

  (** {1 Parsing caml_example options } *)

  (** Parse [warning=n] options for caml_example options *)
  let parse_warning s =
    if string_match ~!{|warning=\([0-9]+\)|} s 0 then
      Some (Warning (int_of_string @@ matched_group 1 s))
    else
      None

  (** Parse [warning n] annotations *)
  let parse_local_warning s =
    if string_match ~!{|warning \([0-9]+\)|} s 0 then
      Some (Warning (int_of_string @@ matched_group 1 s))
    else
      None

  let parse_error s =
    if s="error" then Some Error else None

  let parse_ok s =
    if s = "ok" then Some Ok else None

  (** Parse the environment-wide expected status output *)
  let expected s =
    match parse_warning s, parse_error s with
    | Some w, _ -> w
    | None, Some e -> e
    | None, None -> raise (Parsing_error (Option,s))

  (** Parse the local (i.e. phrase-wide) expected status output *)
  let local_expected s =
    match parse_local_warning s, parse_error s, parse_ok s with
    | Some w, _, _ -> w
    | None, Some e, _ -> e
    | None, None, Some ok -> ok
    | None, None, None -> raise (Parsing_error (Annotation,s))

end

let caml_input, caml_output =
  let cmd = !camllight ^ " 2>&1" in
  try Unix.open_process cmd with _ -> failwith "Cannot start toplevel"
let () =
  at_exit (fun () -> ignore (Unix.close_process (caml_input, caml_output)));
  ignore (input_line caml_input);
  ignore (input_line caml_input)

let read_output () =
  let input = ref (input_line caml_input) in
  input := replace_first ~!"^# *" "" !input;
  let underline =
    if string_match ~!"Characters *\\([0-9]+\\)-\\([0-9]+\\):$" !input 0
    then
      let b = int_of_string (matched_group 1 !input)
      and e = int_of_string (matched_group 2 !input) in
      input := input_line caml_input;
      b, e
    else 0, 0
  in
  let output = Buffer.create 256 in
  let first_line = ref true in
  while not (string_match ~!".*\"end_of_input\"$" !input 0) do
    if !verbose then prerr_endline !input;
    if not !first_line then Buffer.add_char output '\n' else first_line:=false;
    Buffer.add_string output !input;
    input := input_line caml_input;
  done;
  Buffer.contents output, underline

let escape_specials s =
  let s1 = global_replace ~!"\\\\" "\\\\\\\\" s in
  let s2 = global_replace ~!"'" "\\\\textquotesingle\\\\-" s1 in
  let s3 = global_replace ~!"`" "\\\\textasciigrave\\\\-" s2 in
  s3

exception Missing_double_semicolon of string * int

exception Missing_mode of string * int

let process_file file =
  prerr_endline ("Processing " ^ file);
  let ic = try open_in file with _ -> failwith "Cannot read input file" in
  let phrase_start = ref 1 and phrase_stop = ref 1 in
  let incr_phrase_start () =
    incr phrase_start;
    phrase_stop := !phrase_start in
  let oc =
    try if !outfile = "-" then
      stdout
    else if !outfile = "" then
      open_out (replace_first ~!"\\.tex$" "" file ^ ".ml.tex")
    else
      open_out_gen [Open_wronly; Open_creat; Open_append; Open_text]
        0x666 !outfile
    with _ -> failwith "Cannot open output file" in
  let re_spaces = "[ \t]*" in
  let re_start = ~!(
      {|\\begin{caml_example\(\*?\)}|} ^ re_spaces
      ^ {|\({toplevel}\|{verbatim}\)?|} ^ re_spaces
      ^ {|\(\[\(.*\)\]\)?|} ^ re_spaces
      ^ "$"
    ) in
  try while true do
    let input = ref (input_line ic) in
    incr_phrase_start();
    if string_match re_start !input 0
    then begin
      let omit_answer = matched_group 1 !input = "*" in
      let explicit_stop =
        match matched_group 2 !input with
        | exception Not_found -> raise (Missing_mode(file, !phrase_stop))
        | "{toplevel}" -> true
        | "{verbatim}" -> false
        | _ -> assert false in
      let global_expected = try Output.expected @@ matched_group 4 !input
        with Not_found -> Output.Ok in
      start true oc main;
      let first = ref true in
      let read_phrase () =
        let phrase = Buffer.create 256 in
        let rec read () =
          let input = incr phrase_stop; input_line ic in
          let implicit_stop =
            if string_match ~!"\\\\end{caml_example\\*?}[ \t]*$"
                input 0
            then
              begin
                if !phrase_stop = 1 + !phrase_start then
                  raise End_of_file
                else if explicit_stop then
                  raise @@ Missing_double_semicolon (file,!phrase_stop)
                else
                  true
              end
            else false in
          if Buffer.length phrase > 0 then Buffer.add_char phrase '\n';
          let stop = implicit_stop
                     || string_match ~!"\\(.*\\)[ \t]*;;[ \t]*$" input 0 in
          if not stop then (
            Buffer.add_string phrase input; read ()
          )
          else begin
            decr phrase_stop;
            let last_input = if implicit_stop then "" else matched_group 1 input in
            let expected =
              if string_match ~!{|\(.*\)\[@@expect \(.*\)\]|} last_input 0 then
                ( Buffer.add_string phrase (matched_group 1 last_input);
                  Output.local_expected @@ matched_group 2 last_input )
              else
                (Buffer.add_string phrase last_input; global_expected)
            in
            if not implicit_stop then Buffer.add_string phrase ";;";
            implicit_stop, Buffer.contents phrase, expected
          end in
        read ()
      in
      try while true do
        let implicit_stop, phrase, expected = read_phrase () in
        fprintf caml_output "%s%s" phrase
        (if implicit_stop then ";;\n" else "\n");
        flush caml_output;
        output_string caml_output "\"end_of_input\";;\n";
        flush caml_output;
        let output, (b, e) = read_output () in
        let status = Output.status output in
        if status <> expected then (
          let source = Output.{
              file;
              lines = (!phrase_start, !phrase_stop);
              phrase;
              output
            } in
          raise (Output.Unexpected_status
                   {Output.got=status; expected; source} ) )
        else ( incr phrase_stop; phrase_start := !phrase_stop );
        let phrase =
          if b < e then begin
            let start = String.sub phrase ~pos:0 ~len:b
            and underlined = String.sub phrase ~pos:b ~len:(e-b)
            and rest =
              String.sub phrase ~pos:e ~len:(String.length phrase - e)
            in
            String.concat ""
              [escape_specials start; "\\<";
               escape_specials underlined; "\\>";
               escape_specials rest]
          end else
            escape_specials phrase in
        (* Special characters may also appear in output strings -Didier *)
        let output = escape_specials output in
        let phrase = global_replace ~!{|^\(.\)|} camlin phrase
        and output = global_replace ~!{|^\(.\)|} camlout output in
        start false oc phrase_env;
        code_env ~newline:omit_answer input_env oc phrase;
        if not omit_answer then
          code_env ~newline:false (Output.env status) oc output;
        stop true oc phrase_env;
        flush oc;
        first := false;
        if implicit_stop then raise End_of_file
      done
      with End_of_file -> phrase_start:= !phrase_stop; stop true oc main
    end
    else if string_match ~!"\\\\begin{caml_eval}[ \t]*$" !input 0
    then begin
      while input := input_line ic;
        not (string_match ~!"\\\\end{caml_eval}[ \t]*$" !input 0)
      do
        fprintf caml_output "%s\n" !input;
        if string_match ~!".*;;[ \t]*$" !input 0 then begin
          flush caml_output;
          output_string caml_output "\"end_of_input\";;\n";
          flush caml_output;
          ignore (read_output ())
        end
      done
    end else begin
      fprintf oc "%s\n" !input;
      flush oc
    end
  done with
  | End_of_file -> close_in ic; close_out oc
  | Output.Unexpected_status r ->
          ( Output.print_unexpected r; close_in ic; close_out oc; exit 1 )
  | Output.Parsing_error (k,s) ->
      ( Output.print_parsing_error k s;
        close_in ic; close_out oc; exit 1 )
  | Missing_double_semicolon (file, line_number) ->
      ( Format.eprintf "@[<hov 2> Error \
                        when evaluating a caml_example environment in %s:@;\
                        missing \";;\" at line %d@]@." file (line_number-2);
        close_in ic; close_out oc;
        exit 1
      )
  | Missing_mode (file, line_number) ->
      ( Format.eprintf "@[<hov 2>Error \
                        when parsing a caml_example environment in %s:@;\
                        missing mode argument at line %d,@ \
                        available modes {toplevel,verbatim}@]@."
          file (line_number-2);
        close_in ic; close_out oc;
        exit 1
      )


let _ =
  if !outfile <> "-" && !outfile <> "" then begin
    try close_out (open_out !outfile)
    with _ -> failwith "Cannot open output file"
  end;
  List.iter process_file (List.rev !files)
