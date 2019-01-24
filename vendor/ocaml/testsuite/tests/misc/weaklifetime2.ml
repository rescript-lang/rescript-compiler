let n = 500
let loop = 2

let alive = ref (Array.init n (fun _ -> Array.make 10 0))

let create_weaks () =
  Array.init n (fun i ->
      let w = Weak.create 1 in
      Weak.set w 0 (Some (!alive.(i)));
      w
    )

(** We are trying to keep the weak pointer of weak2 set when the
    weak pointer of weak1 and weak3 are wrongly unset.
    [weak1], [weak2] and [weak3] are identical.
 *)

let weak1 = create_weaks ()
let weak2 = create_weaks ()
let weak3 = create_weaks ()

(** put the weak pointers in the major heap *)
let () =
  let dummy = ref [||] in
  for l=0 to 10 do
    dummy := Array.make 300 0
  done

let gccount () = (Gc.quick_stat ()).Gc.major_collections;;

let () =
  for _l=1 to loop do
    let bad = ref 0 in
    for i=0 to n-1 do
      (** make *this* weak key alive *)
      for _j=0 to n*10 do
        ignore (Weak.get weak2.(i) 0);
      done;
      (** Check that if it is alive in weak2 it is alive in weak1 *)
      if Weak.check weak2.(i) 0 &&
         not (Weak.check weak1.(i) 0) &&
         Weak.check weak2.(i) 0
      then incr bad;
      (** Check that if it is alive in weak2 it is alive in weak3
          This case was failing before the addition of the clean phase in the gc
      *)
      if Weak.check weak2.(i) 0 &&
         not (Weak.check weak3.(i) 0) &&
         Weak.check weak2.(i) 0
      then incr bad;
      !alive.(i) <- Array.make 10 0;
    done;
    (* Printf.printf "bad: %i\  gccount:%i\n%!" !bad (gccount ()); *)
    if !bad > 0
    then Printf.printf "failing\n%!"
    else Printf.printf "success\n%!"
  done
