
module N = 
  (val Bs.Cmp.make 
    (fun[@bs] (x : int) y -> 
      compare x y
     )
  )
module M = Bs.Map
module MI = Bs.MapInt
let m0 : (_,string,_) M.t = M.empty (module N)   

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

let m2 : (int, string, _) M.t = M.empty (module I2)

let vv = MI.empty 
let vv2 = MI.empty
module B = Bs.Bag
(* let () = 
  Js.log (m = m2) *)
let () = 
  let count = 1_000_00 in 
  
  (* let {cmp; data} = m in  *)
  let data = ref (B.data m) in 
  let m2_dict, m_dict = B.(dict m2, dict m) in 
  let module N = (val m2_dict) in 
  let module M = ( val m_dict) in
  (* let vcmp = Bs.Cmp.getCmp  M.cmp in  *)
  for i = 0 to count do 
    data := 
      Bs.Map.add0 
      ~cmp:  M.cmp
      
      (* M.cmp *)
      (* (fun[@bs] x y -> compare x y) *)
     

      i i !data 
  done ;
  let newm = B.bag ~data:!data ~dict:m_dict in 
  Js.log newm

let () =     
  let  m = Bs.Map.empty0 in 
  let m11 = 
    Bs.Map.add0 ~cmp:I.cmp
    1 1 m 
  in  
  (* let m2 = 
    Bs.Map.add0 ~cmp:I2.cmp 1 3 m1 in *)
  let _m20 = Bs.Map.empty (module I) in 
  Js.log m11
module ISet = Bs.Set 
let () =   
  let count = 100_000 in 
  let v = ISet.empty (module I) in 
  let m_dict = B.dict m in 
  let module M = (val m_dict) in 
  let cmp = M.cmp in 
  let data = ref (B.data v) in 
  for i = 0 to count do 
    data := Bs.Set.add0 ~cmp !data i 
  done ;
  Js.log !data
  (* { v with data = !data} *)