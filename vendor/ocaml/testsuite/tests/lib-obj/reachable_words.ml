let native =
  match Filename.basename Sys.argv.(0) with
  | "program.byte" | "program.byte.exe" -> false
  | "program.native" | "program.native.exe" -> true
  | s -> print_endline s; assert false


let size x = Obj.reachable_words (Obj.repr x)

let expect_size s x =
  let i = size x in
  if i <> s then
    Printf.printf "size = %i; expected = %i\n%!" i s

type t =
  | A of int
  | B of t * t

let f () =
  let x = Random.int 10 in
  expect_size 0 42;
  expect_size (if native then 0 else 3) (1, 2);
  expect_size 2 [| x |];
  expect_size 3 [| x; 0 |];

  let a = A x in
  expect_size 2 a;
  expect_size 5 (B (a, a)); (* sharing *)
  expect_size 7 (B (a, A (x + 1)));

  let rec b = B (a, b) in (* cycle *)
  expect_size 5 b;

  print_endline "OK"

let () =
  f ()
