include (
  struct
    module IntMap = Map.Make (struct
      type t = int

      let compare (x : t) y = compare x y
    end)

    let empty = IntMap.empty

    let m =
      List.fold_left
        (fun acc (k, v) -> IntMap.add k v acc)
        empty
        [(10, 'a'); (3, 'b'); (7, 'c'); (20, 'd')]

    (* external log : 'a -> unit = "" [@@bs.val "console.log"] *)

    let assert_test () =
      if IntMap.find 10 m = 'a' then prerr_endline "hi"
        (* log ('a', "succeed") *)
      else prerr_endline "hi"
  end :
    sig end )
