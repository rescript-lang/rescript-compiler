let opaque = Sys.opaque_identity

let lists n =
  let l = opaque [n; n; n] in
  match List.rev l with
  | [a; b; c] when a = n && b = n && c = n -> ()
  | _ -> assert false

let fresh_exception x =
  opaque @@
    let module M = struct
        exception E of int
        let throw () = raise (E x)
      end in
    try
      M.throw ()
    with
      M.E n -> assert (n = x)

let obj_with_closure x =
  opaque (object method foo = x end)

let r = ref 42
let state () =
  incr r;
  if !r > 43 then print_string "woo" else ()

let classes (x : int) =
  opaque @@
    let module M = struct
        class a = object
          method foo = x
        end
        class c = object
          inherit a
        end
      end in
    let o = new M.c in
    assert (o#foo = x)


class c_global = object
  method foo = 42
end
let obj_ordering () = opaque @@
  (* Object IDs change, but should be in the same relative order *)
  let a = new c_global in
  let b = new c_global in
  if a < b then print_string "a" else print_string "b"

let random () = opaque @@
  (* as long as there's no self_init, this should be deterministic *)
  if Random.int 100 < 50 then print_string "a" else print_string "b";
  if Random.int 100 < 50 then print_string "a" else print_string "b";
  if Random.int 100 < 50 then print_string "a" else print_string "b";
  if Random.int 100 < 50 then print_string "a" else print_string "b";
  if Random.int 100 < 50 then print_string "a" else print_string "b";
  if Random.int 100 < 50 then print_string "a" else print_string "b";
  if Random.int 100 < 50 then print_string "a" else print_string "b";
  if Random.int 100 < 50 then print_string "a" else print_string "b";
  if Random.int 100 < 50 then print_string "a" else print_string "b"

let tests =
  [| ("lists", fun () -> lists 42);
     ("manylists", fun () -> for i = 1 to 10 do lists 42 done);
     ("exceptions", fun () -> fresh_exception 100);
     ("objects", fun () -> ignore (obj_with_closure 42));
     (* ("state", state); *) (* this one should fail *)
     ("classes", fun () -> classes 42);
     ("obj_ordering", obj_ordering);
     (* ("random", random); *)
  |]
  
