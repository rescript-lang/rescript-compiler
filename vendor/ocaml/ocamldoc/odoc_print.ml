(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format

let new_fmt () =
  let buf = Buffer.create 512 in
  let fmt = formatter_of_buffer buf in
  let flush () =
    pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.reset buf ;
    s
  in
  (fmt, flush)

let (type_fmt, flush_type_fmt) = new_fmt ()
let _ =
  let outfuns = pp_get_formatter_out_functions type_fmt () in
  pp_set_formatter_out_functions type_fmt
    {outfuns with out_newline = fun () -> outfuns.out_string "\n  " 0 3}

let (modtype_fmt, flush_modtype_fmt) = new_fmt ()




let string_of_type_expr t =
  Printtyp.mark_loops t;
  Printtyp.type_scheme_max ~b_reset_names: false type_fmt t;
  flush_type_fmt ()

exception Use_code of string

(** Return the given module type where methods and vals have been removed
   from the signatures. Used when we don't want to print a too long module type.
   @param code when the code is given, we raise the [Use_code] exception is we
   encouter a signature, to that the calling function can use the code rather
   than the "emptied" type.
*)
let simpl_module_type ?code t =
  let rec iter t =
    match t with
      Types.Mty_ident p -> t
    | Types.Mty_alias p -> t
    | Types.Mty_signature _ ->
        (
         match code with
           None -> Types.Mty_signature []
         | Some s -> raise (Use_code s)
        )
    | Types.Mty_functor (id, mt1, mt2) ->
        Types.Mty_functor (id, Misc.may_map iter mt1, iter mt2)
  in
  iter t

let string_of_module_type ?code ?(complete=false) t =
  try
    let t2 = if complete then t else simpl_module_type ?code t in
    Printtyp.modtype modtype_fmt t2;
    flush_modtype_fmt ()
  with
    Use_code s -> s

(** Return the given class type where methods and vals have been removed
   from the signatures. Used when we don't want to print a too long class type.*)
let simpl_class_type t =
  let rec iter t =
    match t with
      Types.Cty_constr (p,texp_list,ct) -> t
    | Types.Cty_signature cs ->
        (* on vire les vals et methods pour ne pas qu'elles soient imprimees
           quand on affichera le type *)
        let tnil = { Types.desc = Types.Tnil ; Types.level = 0; Types.id = 0 } in
        Types.Cty_signature { Types.csig_self = { cs.Types.csig_self with
                                                  Types.desc = Types.Tobject (tnil, ref None) };
                              csig_vars = Types.Vars.empty ;
                              csig_concr = Types.Concr.empty ;
                              csig_inher = []
                             }
    | Types.Cty_arrow (l, texp, ct) ->
        let new_ct = iter ct in
        Types.Cty_arrow (l, texp, new_ct)
  in
  iter t

let string_of_class_type ?(complete=false) t =
  let t2 = if complete then t else simpl_class_type t in
  (* A VOIR : ma propre version de Printtyp.class_type pour ne pas faire reset_names *)
  Printtyp.class_type modtype_fmt t2;
  flush_modtype_fmt ()
