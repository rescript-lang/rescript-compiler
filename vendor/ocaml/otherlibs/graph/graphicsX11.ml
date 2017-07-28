(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*    Pierre Weis and Jun Furuse, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* Module [GraphicsX11]: additional graphics primitives for
   the X Windows system *)

type window_id = string

external window_id : unit -> window_id = "caml_gr_window_id"

let subwindows = Hashtbl.create 13

external open_subwindow : int -> int -> int -> int -> window_id
    = "caml_gr_open_subwindow"
external close_subwindow : window_id -> unit
    = "caml_gr_close_subwindow"

let open_subwindow ~x ~y ~width ~height =
  let wid = open_subwindow x y width height in
  Hashtbl.add subwindows wid ();
  wid
;;

let close_subwindow wid =
  if Hashtbl.mem subwindows wid then begin
    close_subwindow wid;
    Hashtbl.remove subwindows wid
  end else
    raise (Graphics.Graphic_failure("close_subwindow: no such subwindow: "^wid))
;;
