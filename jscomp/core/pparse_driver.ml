


(* Optionally preprocess a source file *)

let call_external_preprocessor sourcefile pp =
      let tmpfile = Filename.temp_file "ocamlpp" "" in
      let comm = Printf.sprintf "%s %s > %s"
                                pp (Filename.quote sourcefile) tmpfile
      in
      if Ccomp.command comm <> 0 then begin
        Misc.remove_file tmpfile;
        Cmd_ast_exception.cannot_run comm
      end;
      tmpfile

let preprocess sourcefile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
    call_external_preprocessor sourcefile pp


let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> Misc.remove_file inputfile






(* Parse a file or get a dumped syntax tree from it *)

let parse (type a) (kind : a Ml_binary.kind) lexbuf : a =
  match kind with
  | Ml_binary.Ml -> Parse.implementation lexbuf
  | Ml_binary.Mli -> Parse.interface lexbuf

let file_aux ppf  inputfile (type a) (parse_fun  : _ -> a)
             (kind : a Ml_binary.kind) : a  =
  let ast_magic = Ml_binary.magic_of_kind kind in
  let ic = open_in_bin inputfile in
  let is_ast_file =
    match really_input_string ic (String.length ast_magic) with 
    | exception _ -> false 
    |  buffer ->
      if buffer = ast_magic then true
      else if Ext_string.starts_with buffer "Caml1999" then
        Cmd_ast_exception.wrong_magic buffer
      else false in 
  let ast =
    try
      if is_ast_file then begin
        if !Clflags.fast then
          (* FIXME make this a proper warning *)
          Format.fprintf ppf "@[Warning: %s@]@."
            "option -unsafe used with a preprocessor returning a syntax tree";
        Location.set_input_name (input_value ic : string);
        (input_value ic : a)
      end else begin
        seek_in ic 0;
        let lexbuf = Lexing.from_channel ic in
        Location.init lexbuf inputfile;
        parse_fun lexbuf
      end
    with x -> close_in ic; raise x
  in
  close_in ic; ast   
  
  



let parse_file (type a) (kind  : a Ml_binary.kind) (ppf : Format.formatter) (sourcefile : string) : a =
  Location.set_input_name  sourcefile;
  let inputfile = preprocess sourcefile in
  let ast =
    try 
      (file_aux ppf  inputfile (parse kind)  kind)
    with exn ->
      remove_preprocessed inputfile;
      raise exn
  in
  remove_preprocessed inputfile;
  ast



let parse_implementation ppf  sourcefile =  
  (parse_file 
       Ml ppf sourcefile)

let parse_interface ppf  sourcefile =
  parse_file Mli ppf sourcefile
