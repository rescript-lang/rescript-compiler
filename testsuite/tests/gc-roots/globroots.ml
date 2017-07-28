(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

module type GLOBREF = sig
  type t
  val register: string -> t
  val get: t -> string
  val set: t -> string -> unit
  val remove: t -> unit
end

module Classic : GLOBREF = struct
  type t
  external register: string -> t = "gb_classic_register"
  external get: t -> string = "gb_get"
  external set: t -> string -> unit = "gb_classic_set"
  external remove: t -> unit = "gb_classic_remove"
end

module Generational : GLOBREF = struct
  type t
  external register: string -> t = "gb_generational_register"
  external get: t -> string = "gb_get"
  external set: t -> string -> unit = "gb_generational_set"
  external remove: t -> unit = "gb_generational_remove"
end

module Test(G: GLOBREF) = struct

  let size = 1024

  let vals = Array.init size string_of_int

  let a = Array.init size (fun i -> G.register (string_of_int i))

  let check () =
    for i = 0 to size - 1 do
      if G.get a.(i) <> vals.(i) then begin
        print_string "Error on "; print_int i; print_string ": ";
        print_string (String.escaped (G.get a.(i))); print_newline()
      end
    done

  let change () =
    match Random.int 37 with
    | 0 ->
        Gc.full_major()
    | 1|2|3|4 ->
        Gc.minor()
    | 5|6|7|8|9|10|11|12 ->             (* update with young value *)
        let i = Random.int size in
        G.set a.(i) (string_of_int i)
    | 13|14|15|16|17|18|19|20 ->        (* update with old value *)
        let i = Random.int size in
        G.set a.(i) vals.(i)
    | 21|22|23|24|25|26|27|28 ->        (* re-register young value *)
        let i = Random.int size in
        G.remove a.(i);
        a.(i) <- G.register (string_of_int i)
    | (*29|30|31|32|33|34|35|36*) _ ->  (* re-register old value *)
        let i = Random.int size in
        G.remove a.(i);
        a.(i) <- G.register vals.(i)

  let test n =
    for i = 1 to n do
      change();
      print_string "."; flush stdout
    done
end

module TestClassic = Test(Classic)
module TestGenerational = Test(Generational)

let _ =
  let n =
    if Array.length Sys.argv < 2 then 10000 else int_of_string Sys.argv.(1) in
  print_string "Non-generational API\n";
  TestClassic.test n;
  print_newline();
  print_string "Generational API\n";
  TestGenerational.test n;
  print_newline()
