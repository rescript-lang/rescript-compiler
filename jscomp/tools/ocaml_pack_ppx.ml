open Ast_mapper 
(* 
[%bundle ("x","y", "z") ] 
[%bundle "x" ] 
*) 
let read_file name parse_fun = 
  let ch = open_in name in 
  let lexbuf = Lexing.from_channel ch in 
  let () = Location.init lexbuf  name in 
  let structure = parse_fun lexbuf  in 
  let () = close_in ch in structure 

let getenv_mapper argv = 
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *) 
  { default_mapper with 
    structure_item = fun mapper str -> 
      match str with 
      | { pstr_desc = 
          Pstr_extension (
          ({txt = "bundle"; _} as  t, 
           ( PStr([{pstr_desc = 
                    Pstr_eval ({ pexp_desc;_},_) 
                      ; }]) : Parsetree.payload)),_attrs)} -> 
                        (
                         let names =  
                           match pexp_desc with 
                           | Pexp_tuple ls ->
                               List.map (function 
                                 | ({pexp_desc = 
                                     Pexp_constant (Const_string (y,_) ); _} : Parsetree.expression) -> y
                                 | _ -> assert false ) ls 
                           | Pexp_constant (Const_string (y,_)) -> [y]
                           | _ -> assert false 
                         in 
                         let module_bindings : Parsetree.module_binding list = 
                           names |> 
                           List.map (fun (x  : string)  -> 
                             let structure = read_file (x ^ ".ml") Parse.implementation in 
                             let signature = read_file (x ^ ".mli") Parse.interface in 
                             ({pmb_name =  {txt = String.capitalize x; loc = t.loc} ; 
                               pmb_expr = { pmod_desc = 
                                            Pmod_constraint 
                                              ( {pmod_desc = Pmod_structure structure; 
                                                 pmod_loc = t.loc; 
                                                 pmod_attributes = [] 
                                               }, 
                                                ({
                                                 pmty_desc = 
                                                 Pmty_signature signature; 
                                                 pmty_loc = t.loc; 
                                                 pmty_attributes = [] 
                                               } : Parsetree.module_type) 
                                               ) 
                                 ; pmod_loc = t.loc; pmod_attributes = [] }; 
                               pmb_attributes = []; 
                               pmb_loc = t.loc 
                             } : Parsetree.module_binding)) in 
                         begin match module_bindings with 
                         |[ x] -> 
                             {pstr_loc = t.loc; 
                              pstr_desc = Pstr_module  x}
                         | _ ->
                             {pstr_loc = t.loc; 
                              pstr_desc = Pstr_recmodule  module_bindings}
                         end)
      |{ pstr_desc = 
         Pstr_extension (
         ({txt = "list_of_modules"; _} , 
           ( PStr([{
                   pstr_desc = 
                   Pstr_eval (
                   { pexp_desc = 
                     Pexp_constant (Const_string (y,_));_},_) 
                     ; 
                 }]) : Parsetree.payload)),_attrs)} 
        -> 
          let module_or_interfaces = 
            List.filter (fun x -> String.length x <> 0)
              @@ A_string.split_on_chars y ~on:[' '] in
          Ocaml_extract.process module_or_interfaces
      

      
      | _ -> default_mapper.structure_item mapper str ; 
  } 

(** known issues: address files internally contains list_of_modules *)

let () = register "bundle" getenv_mapper 

(* local variables: *)
(* compile-command: "ocamlbuild  -no-hygiene -cflags -annot -use-ocamlfind -pkg compiler-libs.common ocaml_pack_ppx.native" *)
(* end: *)

