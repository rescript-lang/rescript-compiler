
let v = ref 0

let reset, incr =
  (fun _ ->  v:=0), (fun _ -> incr v)


let reset2, incr2 =
  let vv   = ref 0 in
  (fun () -> vv := 0),
  (fun () -> incr vv)


let f a b  d e =
  let u,v =
    let h = a b in
    d h, e h in
  u + v


let kf cb v =
  cb v ;
  v + v

let ikf v = kf ignore v
