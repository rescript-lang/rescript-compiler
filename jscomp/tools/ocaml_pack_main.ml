
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
      Line_process.read_lines argv.(1)
    else 
      Array.to_list
        (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) 
  in 

  let str_item  = Ocaml_extract.process files  in
  Pprintast.structure Format.std_formatter 
    [
      {
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
      } ;
      make_comment _loc {|
 BuckleScript compiler
 Copyright (C) 2015-2016 Bloomberg Finance L.P.

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation, with linking exception;
 either version 2.1 of the License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


 Author: Hongbo Zhang  

|};
      make_comment _loc
        (let v = Unix.(localtime (gettimeofday ())) in
         Printf.sprintf "%02d/%02d-%02d:%02d" 
           (v.tm_mon + 1) v.tm_mday v.tm_hour v.tm_min)
      ;
      str_item
    ] 

(* local variables: *)
(* compile-command: "ocamlbuild -no-hygiene -cflags -annot -use-ocamlfind -pkg compiler-libs.common ocaml_pack_main.byte " *)
(* end: *)
