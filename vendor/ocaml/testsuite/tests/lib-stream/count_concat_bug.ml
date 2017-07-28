(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Gabriel Scherer, projet Gallium, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let is_empty s =
  try Stream.empty s; true with Stream.Failure -> false

let test_icons =
  let s = Stream.of_string "ab" in
  let s = Stream.icons 'c' s in
  Testing.test (Stream.next s = 'c');
  Testing.test (Stream.next s = 'a');
  Testing.test (Stream.next s = 'b');
  Testing.test (is_empty s);
  ()

let test_lcons =
  let s = Stream.of_string "ab" in
  let s = Stream.lcons (fun () -> 'c') s in
  Testing.test (Stream.next s = 'c');
  Testing.test (Stream.next s = 'a');
  Testing.test (Stream.next s = 'b');
  Testing.test (is_empty s);
  ()

let test_iapp =
  let s = Stream.of_string "ab" in
  let s = Stream.iapp (Stream.of_list ['c']) s in
  Testing.test (Stream.next s = 'c');
  Testing.test (Stream.next s = 'a');
  Testing.test (Stream.next s = 'b');
  Testing.test (is_empty s);
  ()

let test_lapp_right =
  let s1 = Stream.of_list ['c'] in
  let s2 = Stream.of_string "ab" in
  let s = Stream.lapp (fun () -> s1) s2 in
  Testing.test (Stream.next s = 'c');
  Testing.test (Stream.next s = 'a');
  Testing.test (Stream.next s = 'b');
  Testing.test (is_empty s);
  ()

let test_lapp_left =
  let s1 = Stream.of_string "bc" in
  let s2 = Stream.of_list ['a'] in
  Testing.test (Stream.next s1 = 'b');
  let s = Stream.lapp (fun () -> s1) s2 in
  Testing.test (Stream.next s = 'c');
  Testing.test (Stream.next s = 'a');
  Testing.test (is_empty s);
  ()

let test_slazy =
  let s = Stream.of_string "ab" in
  Testing.test (Stream.next s = 'a');
  let s = Stream.slazy (fun () -> s) in
  Testing.test (Stream.next s = 'b');
  Testing.test (is_empty s);
  ()
