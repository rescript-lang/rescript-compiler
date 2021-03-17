(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type key = string
type doc = string
type usage_msg = string
type anon_fun = (string -> unit)

type spec =
  | Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Bool of (bool -> unit)     (* Call the function with a bool argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Set_string of string ref   (* Set the reference to the string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Set_int of int ref         (* Set the reference to the int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | Set_float of float ref     (* Set the reference to the float argument *)
  | Tuple of spec list         (* Take several arguments according to the
                                  spec list *)
  | Symbol of string list * (string -> unit)
                               (* Take one of the symbols as argument and
                                  call the function with the symbol. *)
  | Rest of (string -> unit)   (* Stop interpreting keywords and call the
                                  function with each remaining argument *)
  | Expand of (string -> string array) (* If the remaining arguments to process
                                          are of the form
                                          [["-foo"; "arg"] @ rest] where "foo" is
                                          registered as [Expand f], then the
                                          arguments [f "arg" @ rest] are
                                          processed. Only allowed in
                                          [parse_and_expand_argv_dynamic]. *)

exception Bad of string
exception Help of string

type error =
  | Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

exception Stop of error (* used internally *)



let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, _) :: _ when y1 = x -> y2
  | _ :: t -> assoc3 x t


let split s =
  let i = String.index s '=' in
  let len = String.length s in
  String.sub s 0 i, String.sub s (i+1) (len-(i+1))


let make_symlist prefix sep suffix l =
  match l with
  | [] -> "<none>"
  | h::t -> (List.fold_left (fun x y -> x ^ sep ^ y) (prefix ^ h) t) ^ suffix


let print_spec buf (key, spec, doc) =
  if String.length doc > 0 then
    match spec with
    | Symbol (l, _) ->
#if 0    
        bprintf buf "  %s %s%s\n" key (make_symlist "{" "|" "}" l) doc
#else
        let sym = make_symlist "{" "|" "}" l in 
        Buffer.add_string buf {j|  $(key) $(sym)$(doc)\n|j}
#end
    | _ ->
#if 0    
        bprintf buf "  %s %s\n" key doc
#else
        Buffer.add_string buf {j|  $(key) $(doc)\n|j}
#end


let help_action () = raise (Stop (Unknown "-help"))

let add_help speclist =
  let add1 =
    try ignore (assoc3 "-help" speclist); []
    with Not_found ->
            ["-help", Unit help_action, " Display this list of options"]
  and add2 =
    try ignore (assoc3 "--help" speclist); []
    with Not_found ->
            ["--help", Unit help_action, " Display this list of options"]
  in
  speclist @ (add1 @ add2)


let usage_b buf speclist errmsg =
#if 0  
  bprintf buf "%s\n" errmsg;
#else
  Buffer.add_string buf {j|$(errmsg)\n|j};
#end
  List.iter (print_spec buf) (add_help speclist)


let usage_string speclist errmsg =
  let b = Buffer.create 200 in
  usage_b b speclist errmsg;
  Buffer.contents b


let usage speclist errmsg =
  Js.log (usage_string speclist errmsg)


let current = ref 0

let bool_of_string_opt x =
  try Some (bool_of_string x)
  with Invalid_argument _ -> None

let int_of_string_opt x =
  try Some (int_of_string x)
  with Failure _ -> None

let float_of_string_opt x =
  try Some (float_of_string x)
  with Failure _ -> None

let parse_and_expand_argv_dynamic_aux allow_expand current argv speclist anonfun errmsg =
  let initpos = !current in
  let convert_error error =
    (* convert an internal error to a Bad/Help exception
       *or* add the program name as a prefix and the usage message as a suffix
       to an user-raised Bad exception.
    *)
    let b = Buffer.create 200 in
    let progname = if initpos < (Array.length !argv) then !argv.(initpos) else "(?)" in
    begin match error with
      | Unknown "-help" -> ()
      | Unknown "--help" -> ()
      | Unknown s ->
#if 0      
          bprintf b "%s: unknown option '%s'.\n" progname s
#else
          Buffer.add_string b {j|$(progname): unknown option '$(s)'.\n|j} 
#end          
      | Missing s ->
#if 0      
          bprintf b "%s: option '%s' needs an argument.\n" progname s
#else
          Buffer.add_string b {j|$(progname): option '$(s)' needs an argument.\n|j}
#end          
      | Wrong (opt, arg, expected) ->
#if 0      
          bprintf b "%s: wrong argument '%s'; option '%s' expects %s.\n"
                  progname arg opt expected
#else
          Buffer.add_string b {j|$(progname): wrong argument '$(arg)'; option '$(opt)' expects $(expected).\n|j}
#end                  
      | Message s -> (* user error message *)
#if 0      
          bprintf b "%s: %s.\n" progname s
#else
          Buffer.add_string b {j|$(progname): $(s).\n|j}
#end          
    end;
    usage_b b !speclist errmsg;
    if error = Unknown "-help" || error = Unknown "--help"
    then Help (Buffer.contents b)
    else Bad (Buffer.contents b)
  in
  incr current;
  while !current < (Array.length !argv) do
    begin try
      let s = !argv.(!current) in
      if String.length s >= 1 && s.[0] = '-' then begin
        let action, follow =
          try assoc3 s !speclist, None
          with Not_found ->
          try
            let keyword, arg = split s in
            assoc3 keyword !speclist, Some arg
          with Not_found -> raise (Stop (Unknown s))
        in
        let no_arg () =
          match follow with
          | None -> ()
          | Some arg -> raise (Stop (Wrong (s, arg, "no argument"))) in
        let get_arg () =
          match follow with
          | None ->
              if !current + 1 < (Array.length !argv) then !argv.(!current + 1)
              else raise (Stop (Missing s))
          | Some arg -> arg
        in
        let consume_arg () =
          match follow with
          | None -> incr current
          | Some _ -> ()
        in
        let rec treat_action = function
        | Unit f -> f ();
        | Bool f ->
            let arg = get_arg () in
            begin match bool_of_string_opt arg with
            | None -> raise (Stop (Wrong (s, arg, "a boolean")))
            | Some s -> f s
            end;
            consume_arg ();
        | Set r -> no_arg (); r := true;
        | Clear r -> no_arg (); r := false;
        | String f ->
            let arg = get_arg () in
            f arg;
            consume_arg ();
        | Symbol (symb, f) ->
            let arg = get_arg () in
            if List.mem arg symb then begin
              f arg;
              consume_arg ();
            end else begin
              raise (Stop (Wrong (s, arg, "one of: "
                                          ^ (make_symlist "" " " "" symb))))
            end
        | Set_string r ->
            r := get_arg ();
            consume_arg ();
        | Int f ->
            let arg = get_arg () in
            begin match int_of_string_opt arg with
            | None -> raise (Stop (Wrong (s, arg, "an integer")))
            | Some x -> f x
            end;
            consume_arg ();
        | Set_int r ->
            let arg = get_arg () in
            begin match int_of_string_opt arg with
            | None -> raise (Stop (Wrong (s, arg, "an integer")))
            | Some x -> r := x
            end;
            consume_arg ();
        | Float f ->
            let arg = get_arg () in
            begin match float_of_string_opt arg with
            | None -> raise (Stop (Wrong (s, arg, "a float")))
            | Some x -> f x
            end;
            consume_arg ();
        | Set_float r ->
            let arg = get_arg () in
            begin match float_of_string_opt arg with
            | None -> raise (Stop (Wrong (s, arg, "a float")))
            | Some x -> r := x
            end;
            consume_arg ();
        | Tuple specs ->
            List.iter treat_action specs;
        | Rest f ->
            while !current < (Array.length !argv) - 1 do
              f !argv.(!current + 1);
              consume_arg ();
            done;
        | Expand f ->
            if not allow_expand then
              raise (Invalid_argument "Arg.Expand is is only allowed with Arg.parse_and_expand_argv_dynamic");
            let arg = get_arg () in
            let newarg = f arg in
            consume_arg ();
            let before = Array.sub !argv 0 (!current + 1)
            and after = Array.sub !argv (!current + 1) ((Array.length !argv) - !current - 1) in
            argv:= Array.concat [before;newarg;after];
        in
        treat_action action end
      else anonfun s
    with | Bad m -> raise (convert_error (Message m));
         | Stop e -> raise (convert_error e);
    end;
    incr current
  done

let parse_and_expand_argv_dynamic current argv speclist anonfun errmsg =
  parse_and_expand_argv_dynamic_aux true current argv speclist anonfun errmsg

let parse_argv_dynamic ?(current=current) argv speclist anonfun errmsg =
  parse_and_expand_argv_dynamic_aux false current (ref argv) speclist anonfun errmsg


let parse_argv ?(current=current) argv speclist anonfun errmsg =
  parse_argv_dynamic ~current:current argv (ref speclist) anonfun errmsg


let parse l f msg =
  try
    parse_argv Sys.argv l f msg
  with
  | Bad msg -> Js.log msg; exit 2
  | Help msg -> Js.log msg; exit 0


let parse_dynamic l f msg =
  try
    parse_argv_dynamic Sys.argv l f msg
  with
  | Bad msg -> Js.log msg; exit 2
  | Help msg -> Js.log msg; exit 0

let parse_expand l f msg =
  try
    let argv = ref Sys.argv in
    let spec = ref l in
    let current = ref (!current) in
    parse_and_expand_argv_dynamic current argv spec f msg
  with
  | Bad msg -> Js.log msg; exit 2
  | Help msg -> Js.log msg; exit 0


let second_word s =
  let len = String.length s in
  let rec loop n =
    if n >= len then len
    else if s.[n] = ' ' then loop (n+1)
    else n
  in
  match String.index s '\t' with
  | n -> loop (n+1)
  | exception Not_found ->
      begin match String.index s ' ' with
      | n -> loop (n+1)
      | exception Not_found -> len
      end


let max_arg_len cur (kwd, spec, doc) =
  match spec with
  | Symbol _ -> max cur (String.length kwd)
  | _ -> max cur (String.length kwd + second_word doc)


let replace_leading_tab s =
  let seen = ref false in
  String.map (function '\t' when not !seen -> seen := true; ' ' | c -> c) s

let add_padding len ksd =
  match ksd with
  | (_, _, "") ->
      (* Do not pad undocumented options, so that they still don't show up when
       * run through [usage] or [parse]. *)
      ksd
  | (kwd, (Symbol _ as spec), msg) ->
      let cutcol = second_word msg in
      let spaces = String.make ((max 0 (len - cutcol)) + 3) ' ' in
      (kwd, spec, "\n" ^ spaces ^ replace_leading_tab msg)
  | (kwd, spec, msg) ->
      let cutcol = second_word msg in
      let kwd_len = String.length kwd in
      let diff = len - kwd_len - cutcol in
      if diff <= 0 then
        (kwd, spec, replace_leading_tab msg)
      else
        let spaces = String.make diff ' ' in
        let prefix = String.sub (replace_leading_tab msg) 0 cutcol in
        let suffix = String.sub msg cutcol (String.length msg - cutcol) in
        (kwd, spec, prefix ^ spaces ^ suffix)


let align ?(limit=max_int) speclist =
  let completed = add_help speclist in
  let len = List.fold_left max_arg_len 0 completed in
  let len = min len limit in
  List.map (add_padding len) completed
#if 0
let trim_cr s =
  let len = String.length s in
  if len > 0 && String.get s (len - 1) = '\r' then
    String.sub s 0 (len - 1)
  else
    s

let read_aux trim sep file =
  let ic = open_in_bin file in
  let buf = Buffer.create 200 in
  let words = ref [] in
  let stash () =
    let word =  (Buffer.contents buf) in
    let word = if trim then trim_cr word else word in
    words := word :: !words;
    Buffer.clear buf
  in
  let rec read () =
    try
      let c = input_char ic in
      if c = sep then begin
        stash (); read ()
      end else begin
        Buffer.add_char buf c; read ()
      end
    with End_of_file ->
      if Buffer.length buf > 0 then
        stash () in
  read ();
  close_in ic;
  Array.of_list (List.rev !words)

let read_arg = read_aux true '\n'

let read_arg0 = read_aux false '\x00'

let write_aux sep file args =
  let oc = open_out_bin file in
  Array.iter (fun s -> fprintf oc "%s%c" s sep) args;
  close_out oc

let write_arg = write_aux '\n'

let write_arg0 = write_aux '\x00'
#end