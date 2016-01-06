include (struct

module IntMap = Map.Make(struct type t  = int let compare (x : t) y = compare x y end)

let empty = IntMap.empty 

let m = List.fold_left (fun acc (k,v) -> IntMap.add k v  acc ) empty [(10,'a'); (3,'b'); (7,'c'); (20,'d') ]

module SMap = Map.Make(struct type t = string let compare (x : t ) y = compare x y end)

let s = 
  List.fold_left (fun acc (k,v) -> SMap.add k v  acc ) SMap.empty
    [("10",'a');
     ("3",'b');
     ("7",'c');
     ("20",'d') ]
external log : 'a -> unit = "" [@@js.call "console.log"]

let assert_test () = 
  begin
    Mt.assert_equal ( IntMap.find 10 m)  'a';
    Mt.assert_equal (SMap.find "10"  s)  'a'
  end
end : sig val assert_test : unit -> unit end)
