include (
  struct
    module M = Map.Make (String)

    let v = M.is_empty M.empty
  end :
    sig
      val v : bool
    end )
