module rec PA : sig
  val print: int array -> unit
end = struct
  (* let () = P.print 3  *)
  let print =
    let iter = Array.iter P.print in
    fun a -> iter a
end
and P : sig
  val print: int -> unit
end = struct
  let print i =
    print_endline (string_of_int i)
end
    
let () =
  PA.print [|1; 2|]