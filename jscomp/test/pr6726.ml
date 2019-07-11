(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*              Jacques Garrigue, Nagoya University                    *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

module ExtUnixAll = struct
  external unused : unit -> unit = "caml_blit_string"

  module BigEndian = struct let get_uint8 str off = 33 end
end

module ExtUnix = struct module All = ExtUnixAll end

module Test = struct
  open ExtUnix.All

  let test_endian_string x =
    let module B = BigEndian in
    B.get_uint8 x 0

  let v = test_endian_string 1
end
