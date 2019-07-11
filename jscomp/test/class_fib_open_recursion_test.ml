let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

class fib =
  object (self)
    method calc x =
      if x = 0 || x = 1 then 1 else self#calc (x - 1) + self#calc (x - 2)
  end

class memo_fib =
  object (self)
    val cache = Hashtbl.create 31

    inherit fib as super

    method calc x =
      match Hashtbl.find cache x with
      | exception Not_found ->
          let v = super#calc x in
          Hashtbl.add cache x v ; v
      | v -> v
  end

let () =
  (* print_endline (string_of_int ((new fib)#calc 50)) *)
  eq __LOC__ ((new memo_fib)#calc 40) 165580141

let () = Mt.from_pair_suites __MODULE__ !suites
