
let lexer = Genlex.make_lexer [] (* poor man *)

let rec to_list acc stream = 
  match Stream.next stream with 
  | exception _ -> List.rev acc 
  | v -> to_list (v::acc) stream 
 
let process_line line = 
  match to_list [] (lexer (Stream.of_string line)) with
  | Ident "#" :: _ -> None
  | (Ident v|Kwd v) :: _ -> Some v 
  | (Int _ | Float _ | Char _ | String _ )  :: _ -> 
      assert false 
  | [] -> None 
let read_lines file = 
  let chan = open_in file in
  let rec loop acc = 
    match input_line chan with
    | line -> 
        begin match process_line line with 
        | None -> 
            loop acc 
        | Some f -> 
            loop ((if Sys.file_exists (f ^ ".mli") then
              [f ^ ".mli"]
            else []) @ 
            (if Sys.file_exists (f ^ ".ml")  then
              [f ^ ".ml"]
            else []
            ) @ acc)
        end

    | exception End_of_file -> close_in chan ; acc in
  loop []
          
let _ = 
  let _loc = Location.none in
  let argv = Sys.argv in
  let files = 
    if Array.length argv = 2 && Filename.check_suffix  argv.(1) "mllib" then 
      read_lines argv.(1)
    else 
      Array.to_list
        (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) 
  in 

  let str_item  = Ocaml_extract.process files  in
  Pprintast.structure Format.std_formatter 
    [{
     Parsetree.pstr_loc = _loc;
     pstr_desc =
     (Pstr_attribute
        (({ loc = _loc; txt = "warning" },
          (PStr
             [{
              Parsetree.pstr_loc = _loc;
              pstr_desc =
              (Pstr_eval
                 ({
                  pexp_loc = _loc;
                  pexp_desc =
                  (Pexp_constant (** my personal preferrence *)
                     (Const_string ("-a", None)));
                  pexp_attributes = []
                }, []))
            }])) : Parsetree.attribute))
   };
     str_item
   ] 

(* local variables: *)
(* compile-command: "ocamlbuild -no-hygiene -cflags -annot -use-ocamlfind -pkg compiler-libs.common ocaml_pack_main.byte " *)
(* end: *)
