(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

type shape =
  | Function
  | Lazy
  | Class
  | Module of shape array
  | Value of Obj.t

let rec init_mod loc shape =
  match shape with
  | Function ->
      let pad1 = 1 and pad2 = 2 and pad3 = 3 and pad4 = 4
      and pad5 = 5 and pad6 = 6 and pad7 = 7 and pad8 = 8 in
      Obj.repr(fun _ ->
        ignore pad1; ignore pad2; ignore pad3; ignore pad4;
        ignore pad5; ignore pad6; ignore pad7; ignore pad8;
        raise (Undefined_recursive_module loc))
  | Lazy ->
      Obj.repr (lazy (raise (Undefined_recursive_module loc)))
  | Class ->
      Obj.repr (CamlinternalOO.dummy_class loc)
  | Module comps ->
      Obj.repr (Array.map (init_mod loc) comps)
  | Value v ->
      v

let overwrite o n =
  assert (Obj.size o >= Obj.size n);
  for i = 0 to Obj.size n - 1 do
    Obj.set_field o i (Obj.field n i)
  done

let rec update_mod shape o n =
  match shape with
  | Function ->
      if Obj.tag n = Obj.closure_tag && Obj.size n <= Obj.size o
      then begin overwrite o n; Obj.truncate o (Obj.size n) (* PR #4008 *) end
      else overwrite o (Obj.repr (fun x -> (Obj.obj n : _ -> _) x))
  | Lazy ->
      if Obj.tag n = Obj.lazy_tag then
        Obj.set_field o 0 (Obj.field n 0)
      else if Obj.tag n = Obj.forward_tag then begin (* PR#4316 *)
        Obj.set_tag o Obj.forward_tag;
        Obj.set_field o 0 (Obj.field n 0)
      end else begin
        (* forwarding pointer was shortcut by GC *)
        Obj.set_tag o Obj.forward_tag;
        Obj.set_field o 0 n
      end
  | Class ->
      assert (Obj.tag n = 0 && Obj.size n = 4);
      overwrite o n
  | Module comps ->
      assert (Obj.tag n = 0 && Obj.size n >= Array.length comps);
      for i = 0 to Array.length comps - 1 do
        update_mod comps.(i) (Obj.field o i) (Obj.field n i)
      done
  | Value v -> () (* the value is already there *)
