
let make_comment _loc str =           
      {
        Parsetree.pstr_loc = _loc;
        pstr_desc =
          (Pstr_attribute
             (({ loc = _loc; txt =  "ocaml.doc"},
               (PStr
                  [{
                    Parsetree.pstr_loc = _loc;
                    pstr_desc =
                      (Pstr_eval
                         ({
                           pexp_loc = _loc;
                           pexp_desc =
                             (Pexp_constant (** Copy right header *)
                                (Const_string (str, None)));
                           pexp_attributes = []
                         }, []))
                  }])) : Parsetree.attribute))
      }


let _ = 
  let _loc = Location.none in
  let argv = Sys.argv in
  let files = 
    if Array.length argv = 2 && Filename.check_suffix  argv.(1) "mllib" then 
      Line_process.read_lines (Sys.getcwd ())argv.(1)
    else 
      Array.to_list
        (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) 
  in 
  let tasks = Ocaml_extract.process_as_string files in 
  let emit name = 
    output_string stdout "#1 \"";
    (*Note here we do this is mostly to avoid leaking user's 
      information, like private path, in the future, we can have 
      a flag
    *)
    output_string stdout (Filename.basename name) ;
    output_string stdout "\"\n" 
  in
  begin 
    let local_time = Unix.(localtime (gettimeofday ())) in 
    output_string stdout 
      (Printf.sprintf {|(** Bundled by ocaml_pack %02d/%02d-%02d:%02d *)|}
         (local_time.tm_mon + 1) local_time.tm_mday 
         local_time.tm_hour local_time.tm_min 
                         );
    output_string stdout "\n";
    tasks |> List.iter (fun t ->
        match t with
        | `All (base, ml_content,ml_name, mli_content, mli_name) -> 
          let base = String.capitalize base in 
          output_string stdout "module ";
          output_string stdout base ; 
          output_string stdout " : sig \n";

          emit mli_name ;
          output_string stdout mli_content;

          output_string stdout "\nend = struct\n";
          emit ml_name ;
          output_string stdout ml_content;
          output_string stdout "\nend\n"

        | `Ml (base, ml_content, ml_name) -> 
          let base = String.capitalize base in 
          output_string stdout "module \n";
          output_string stdout base ; 
          output_string stdout "\n= struct\n";

          emit ml_name;
          output_string stdout ml_content;

          output_string stdout "\nend\n"

      )
  end

(* local variables: *)
(* compile-command: "ocamlbuild -no-hygiene -cflags -annot -use-ocamlfind -pkg compiler-libs.common ocaml_pack_main.byte " *)
(* end: *)
