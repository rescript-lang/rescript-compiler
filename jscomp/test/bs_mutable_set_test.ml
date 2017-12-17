

module N  = Bs_internalMutableAVLSet





let () = 
  let v = ref N.empty in 
  [%time for i = 0 to 1_000_000 do 
    (* [%assert (N.checkInvariant !v)]; *)
    v := N.add i !v
  done] ;
  Js.log (N.checkInvariant !v);
  Js.log (N.cardinal !v)
