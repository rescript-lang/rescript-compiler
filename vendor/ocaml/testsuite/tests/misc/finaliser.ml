

let m = 1000
let m' = 100
let k = m*10

(** the printing are not stable between ocamlc and ocamlopt *)
let debug = false

let gc_print where _ =
  if debug then
    let stat = Gc.quick_stat () in
    Printf.printf "minor: %i major: %i %s\n%!"
      stat.Gc.minor_collections
      stat.Gc.major_collections
      where

let r = Array.init m (fun _ -> Array.make m 1)


let () =
  gc_print "[Before]" ();
  let rec aux n =
    if n < k then begin
      r.(n mod m) <- (Array.make m' n);
      begin match n mod m with
      | 0 ->
          (** finalise first major *)
          gc_print (Printf.sprintf "[Create %i first]" n) ();
          Gc.finalise (gc_print (Printf.sprintf "[Finalise %i first]" n)) r.(0)
      | 1 ->
          (** finalise last major *)
          gc_print (Printf.sprintf "[Create %i last]" n) ();
          Gc.finalise_last
            (gc_print (Printf.sprintf "[Finalise %i last]" n)) r.(1)
      | 2 ->
          (** finalise first minor *)
          let m = ref 1 in
          gc_print (Printf.sprintf "[Create %i first minor]" n) ();
          Gc.finalise
            (gc_print (Printf.sprintf "[Finalise %i first minor]" n)) m
      | 3 ->
          (** finalise last minor *)
          let m = ref 1 in
          gc_print (Printf.sprintf "[Create %i last minor]" n) ();
          Gc.finalise_last
            (gc_print (Printf.sprintf "[Finalise %i last minor]" n)) m
      | 4 ->
          (** finalise first-last major *)
          gc_print (Printf.sprintf "[Create %i first]" n) ();
          Gc.finalise (gc_print (Printf.sprintf "[Finalise %i first]" n)) r.(4);
          Gc.finalise_last
            (gc_print (Printf.sprintf "[Finalise %i first]" n)) r.(4)
      | _ -> ()
      end;
      aux (n + 1)
    end
  in
  aux 0;
  gc_print "[Full major]" ();
  Gc.full_major ();
  gc_print "[Second full major]" ();
  Gc.full_major ();
  gc_print "[Third full major]" ();
  Gc.full_major ();
  ()

let () = flush stdout
