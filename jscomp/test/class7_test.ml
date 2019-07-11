let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

class point x_init =
  object
    val mutable x = x_init

    method get_x = x

    method move d = x <- x + d
  end

let () =
  let p = new point 55 in
  (* TODO: see why [field 1] is missing *)
  (* let () = [%bs.debugger] in *)
  let q = Oo.copy p in
  q#move 7 ;
  eq __LOC__ (55, 62) (p#get_x, q#get_x)

class ['a] ref x_init =
  object
    val mutable x : 'a = x_init

    method get = x

    method set y = x <- y
  end

class backup =
  object (self : 'mytype)
    val mutable copy = None

    method save = copy <- Some {<copy = None>}

    method restore = match copy with Some x -> x | None -> self
  end

class ['a] backup_ref x =
  object
    inherit ['a] ref x

    inherit backup
  end

let rec get p n = if n = 0 then p#get else get p#restore (n - 1)

let () =
  let p = new backup_ref 0 in
  p#save ;
  p#set 1 ;
  p#save ;
  p#set 2 ;
  eq __LOC__ [|2; 1; 1; 1; 1|] [|get p 0; get p 1; get p 2; get p 3; get p 4|]

class backup2 =
  object (self : 'mytype)
    val mutable copy = None

    method save = copy <- Some {<>}

    method restore = match copy with Some x -> x | None -> self

    method clear = copy <- None
  end

class ['a] backup_ref2 x =
  object
    inherit ['a] ref x

    inherit backup2
  end

let () =
  let p = new backup_ref2 0 in
  p#save ;
  p#set 1 ;
  p#save ;
  p#set 2 ;
  eq __LOC__ [|2; 1; 0; 0; 0|] [|get p 0; get p 1; get p 2; get p 3; get p 4|]

class window =
  object
    val mutable top_widget : widget option = None

    method top_widget = top_widget
  end

and widget (w : window) =
  object
    val window = w

    method window = window
  end

let () = Mt.from_pair_suites __MODULE__ !suites
