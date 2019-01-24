(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type expression =
    E_ident of Longident.t              (* x or Mod.x *)
  | E_name of int                       (* $xxx *)
  | E_item of expression * int          (* x.1 x.[2] x.(3) *)
  | E_field of expression * string      (* x.lbl !x *)
  | E_result

type break_arg =
    BA_none                             (* break *)
  | BA_pc of int                        (* break PC *)
  | BA_function of expression           (* break FUNCTION *)
  | BA_pos1 of Longident.t option * int * int option
                                        (* break @ [MODULE] LINE [POS] *)
  | BA_pos2 of Longident.t option * int (* break @ [MODULE] # OFFSET *)
