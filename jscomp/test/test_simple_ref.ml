include (
  struct
    let v = ref 0
    let gen () = incr v ; !v
    let h = ref 0
    let a = ref 0
    let c = ref 0
    let escape v = v

    let not_real_escape =
      let b = a in
      !b

    let real_escape f v =
      let b = c in
      f b

    let u = escape h
  end :
    sig
      val u : int ref
      val gen : unit -> int
      val not_real_escape : int
      val real_escape : (int ref -> unit) -> int -> unit
    end )
