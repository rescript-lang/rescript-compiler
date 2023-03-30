let foo v =
  let s = Marshal.to_string v [Compat_32] in
  let ch = open_out_bin "aaa.marshal" in
  let () = output_string ch s in
  close_out ch

type r = {x: int; s: string}
type v = A | B | C of (int * int) | D of int * int;;

(* foo "abc";; *)
(* foo {x = 3; s = "abc"};; *)
(* foo A;; *)
(* foo B;; *)
(* foo (C (1, 2));; *)
(* foo (D (1, 2));; *)
foo [1;2;3;4;5]
