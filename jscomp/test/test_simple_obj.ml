external log : 'a -> unit = "" [@@bs.val "console.log"]

let u =
  object
    method hi v z = v + z

    method id1 = 3

    method id2 = 4

    method hello v = v
  end

let uu =
  object
    method id = "uu"
  end

let uuu =
  object
    method add x y = x + y
  end

let v =
  object (self)
    method add x y = x + y (* method hi x = self#add x 32 *)
  end

let test () =
  assert (uu#id = "uu") ;
  assert (uuu#add 1 20 = 21) ;
  assert (v#add 3 7 = 10) ;
  (* log @@ v#hi 31 *)
  assert (u#id1 = 3) ;
  assert (u#id2 = 4) ;
  assert (u#hi 1 2 = 3) ;
  assert (u#hello 32 = 32)

(* assert (1 <> 1); *)
(* log @@ uuu# add 3 4 ; *)
(* log (v#x); *)
(* log (u#hello 32); *)
(* log (uu#id); *)
(* log (u#id); *)
