(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

class reload_generic : object
  method reload_operation :
    Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array * Reg.t array
  method reload_test : Mach.test -> Reg.t array -> Reg.t array
    (* Can be overridden to reflect instructions that can operate
       directly on stack locations *)
  method makereg : Reg.t -> Reg.t
    (* Can be overridden to avoid creating new registers of some class
       (i.e. if all "registers" of that class are actually on stack) *)
  method fundecl : Mach.fundecl -> Mach.fundecl * bool
    (* The entry point *)
end
