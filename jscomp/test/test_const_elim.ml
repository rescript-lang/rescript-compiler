include (
  struct
    let f x =
      let u = (1, 2) in
      let v = (x, x) in
      (fst u, snd v)
  end :
    sig end )
