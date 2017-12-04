


module I = Bs.Cmp.Make(
  struct 
    type t = int   
    let cmp = fun [@bs] (x : int) y ->
      compare x y 
  end
  )
  
module I2 = Bs.Cmp.Make(
  struct 
    type t = int   
    let cmp = fun [@bs] (x : int) y ->
      compare y x 
  end
  )
  
let m = Bs.Map.empty (module I)

let m2 = Bs.Map.empty (module I2)


(* let () = 
  Js.log (m = m2) *)
let () = 
  let count = 1_000_00 in 
  
  (* let {cmp; data} = m in  *)
  let data = ref m.data in 
  let module N = (val m2.cmp) in 
  let module M = ( val m.cmp) in
  (* let vcmp = Bs.Cmp.getCmp  M.cmp in  *)
  for i = 0 to count do 
    data := 
      Bs.Map.add0 
      ~cmp:  M.cmp
      
      (* M.cmp *)
      (* (fun[@bs] x y -> compare x y) *)
     

      i i !data 
  done ;
  let newm = { m with data = !data} in 
  Js.log newm

let () =     
  let  m = Bs.Map.empty0 in 
  let m1 = 
    Bs.Map.add0 ~cmp:I.cmp
    1 1 m 
  in  
  (* let m2 = 
    Bs.Map.add0 ~cmp:I2.cmp 1 3 m1 in *)
  let m20 = Bs.Map.empty (module I) in 
  Js.log {m20 with data = m1}

let () =   
  let count = 100_000 in 
  let v = Bs.Set.empty (module I) in 
  let module M = (val m.cmp) in 
  let cmp = M.cmp in 
  let data = ref v.data in 
  for i = 0 to count do 
    data := Bs.Set.add0 ~cmp i !data
  done ;
  Js.log { v with data = !data}