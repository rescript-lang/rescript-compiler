include (
  struct
    let u = ref 0 let v = incr u
  end :
    sig
      val v : unit
    end )
