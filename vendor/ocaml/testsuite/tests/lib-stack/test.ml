module S = struct
  include Stack

  let to_list s =    (* from bottom to top *)
    let l = ref [] in
    iter (fun x -> l := x :: !l) s;
    !l
end

let does_raise f s =
  try
    ignore (f s : int);
    false
  with S.Empty ->
    true

let () =
  let s = S.create () in
  ();                   assert (S.to_list s = [          ] && S.length s = 0);
  S.push 1 s;           assert (S.to_list s = [1         ] && S.length s = 1);
  S.push 2 s;           assert (S.to_list s = [1; 2      ] && S.length s = 2);
  S.push 3 s;           assert (S.to_list s = [1; 2; 3   ] && S.length s = 3);
  S.push 4 s;           assert (S.to_list s = [1; 2; 3; 4] && S.length s = 4);
  assert (S.pop s = 4); assert (S.to_list s = [1; 2; 3;  ] && S.length s = 3);
  assert (S.pop s = 3); assert (S.to_list s = [1; 2;     ] && S.length s = 2);
  assert (S.pop s = 2); assert (S.to_list s = [1;        ] && S.length s = 1);
  assert (S.pop s = 1); assert (S.to_list s = [          ] && S.length s = 0);
  assert (does_raise S.pop s);
;;

let () =
  let s = S.create () in
  S.push 1 s; assert (S.pop s = 1); assert (does_raise S.pop s);
  S.push 2 s; assert (S.pop s = 2); assert (does_raise S.pop s);
  assert (S.length s = 0);
;;

let () =
  let s = S.create () in
  S.push 1 s; assert (S.top s = 1);
  S.push 2 s; assert (S.top s = 2);
  S.push 3 s; assert (S.top s = 3);
  assert (S.top s = 3); assert (S.pop s = 3);
  assert (S.top s = 2); assert (S.pop s = 2);
  assert (S.top s = 1); assert (S.pop s = 1);
  assert (does_raise S.top s);
  assert (does_raise S.top s);
;;

let () =
  let s = S.create () in
  for i = 1 to 10 do S.push i s done;
  S.clear s;
  assert (S.length s = 0);
  assert (does_raise S.pop s);
  assert (s = S.create ());
  S.push 42 s;
  assert (S.pop s = 42);
;;

let () =
  let s1 = S.create () in
  for i = 1 to 10 do S.push i s1 done;
  let s2 = S.copy s1 in
  assert (S.to_list s1 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);
  assert (S.to_list s2 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);
  assert (S.length s1 = 10);
  assert (S.length s2 = 10);
  for i = 10 downto 1 do
    assert (S.pop s1 = i);
  done;
  for i = 10 downto 1 do
    assert (S.pop s2 = i);
  done;
;;

let () =
  let s = S.create () in
  assert (S.is_empty s);
  for i = 1 to 10 do
    S.push i s;
    assert (S.length s = i);
    assert (not (S.is_empty s));
  done;
  for i = 10 downto 1 do
    assert (S.length s = i);
    assert (not (S.is_empty s));
    ignore (S.pop s : int);
  done;
  assert (S.length s = 0);
  assert (S.is_empty s);
;;

let () =
  let s = S.create () in
  for i = 10 downto 1 do S.push i s done;
  let i = ref 1 in
  S.iter (fun j -> assert (!i = j); incr i) s;
;;

let () =
  let s1 = S.create () in
  assert (S.length s1 = 0); assert (S.to_list s1 = []);
  let s2 = S.copy s1 in
  assert (S.length s1 = 0); assert (S.to_list s1 = []);
  assert (S.length s2 = 0); assert (S.to_list s2 = []);
;;

let () =
  let s1 = S.create () in
  for i = 1 to 4 do S.push i s1 done;
  assert (S.length s1 = 4); assert (S.to_list s1 = [1; 2; 3; 4]);
  let s2 = S.copy s1 in
  assert (S.length s1 = 4); assert (S.to_list s1 = [1; 2; 3; 4]);
  assert (S.length s2 = 4); assert (S.to_list s2 = [1; 2; 3; 4]);
;;

let () = print_endline "OK"
