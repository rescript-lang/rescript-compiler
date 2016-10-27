
#directory "+compiler-libs";;
#load "ocamlcommon.cma";;

open Ast_helper

let gen loc top files = 
  files 
  |> List.map begin fun f -> 
    Str.module_   
      (Mb.mk {loc  ; 
              txt = String.capitalize f}
         (Mod.ident {loc; txt = Lident ( top ^ "-" ^ String.capitalize f) }))
  end

type _ kind = 
  | Ml : Parsetree.structure kind 
  | Mli : Parsetree.signature kind 
let write_ast (type t) ~(fname : string) ~output (kind : t kind) ( pt : t) : unit =
  let magic = 
    match kind with 
    | Ml -> Config.ast_impl_magic_number
    | Mli -> Config.ast_intf_magic_number in
  let oc = open_out output in 
  output_string oc magic ;
  output_value oc fname;
  output_value oc pt;
  close_out oc 

let strs = gen Location.none "Tmp" ["Hey"]

let () = write_ast ~fname:"Tmp.mlmap" ~output:"Tmp.mlast" Ml strs

