

module N = Bs_internalAVLset

type 'elt node = 'elt N.node

type ('elt,'id) t0 = 'elt node Js.null

external unsafeCoerce : 'a Js.null -> 'a = "%identity"



let empty = N.empty0      
let isEmpty = N.isEmpty0
let singleton = N.singleton0
let min = N.min0
let max = N.max0
let iter = N.iter0      
let fold = N.fold0
let forAll = N.forAll0
let exists = N.exists0    
let filter = N.filter0
let partition = N.partition0
let cardinal = N.cardinal0
let elements = N.elements0 
let checkInvariant = N.checkInvariant
(* 
  L rotation, return root node
*)
let rotateWithLeftChild k2 = 
  let k1 = unsafeCoerce (N.left k2) in 
  N.(leftSet k2 (right k1)); 
  N.(rightSet k1 (return k2 ));
  let hlk2, hrk2 = N.(height (left k2), (height (right k2))) in  
  N.(hSet k2 
    (Pervasives.max hlk2 hrk2 + 1));
  let hlk1, hk2 = N.(height (left k1), (h k2)) in 
  N.(hSet k1 (Pervasives.max hlk1 hk2 + 1));
  k1  
(* right rotation *)
let rotateWithRightChild k1 =   
  let k2 = unsafeCoerce (N.right k1) in 
  N.(rightSet k1 (left k2));
  N.(leftSet k2 (return k1));
  let hlk1, hrk1 = N.((height (left k1)), (height (right k1))) in 
  N.(hSet k1 (Pervasives.max  hlk1 hrk1 + 1));
  let hrk2, hk1 = N.(height (right k2), (h k1)) in 
  N.(hSet k2 (Pervasives.max  hrk2 hk1 + 1));
  k2 

(*
  double l rotation
*)  
let doubleWithLeftChild k3 =   
  let v = rotateWithLeftChild (unsafeCoerce N.(left k3)) in 
  N.(leftSet k3 (return v ));
  rotateWithLeftChild k3 

let doubleWithRightChild k2 = 
  let v = rotateWithRightChild (unsafeCoerce N.(right k2)) in   
  N.(rightSet k2 (return v));
  rotateWithRightChild k2
type key = int 

let rec add (x : key) (t : _ t0) =   
  match N.toOpt t with 
  | None -> N.(return @@ node ~left:empty ~right:empty ~key:x ~h:1)
  | Some nt -> 
    let k = N.key nt in 
    if x = k then t 
    else
      begin 
        let l, r = N.(left nt, right nt) in 
        let t =
        (if x < k then             
           begin 
             N.leftSet nt (add x l);
             (if N.height l > 2 +  N.height r then 
                (if x < N.key (unsafeCoerce l) then 
                    rotateWithLeftChild nt 
                 else 
                    doubleWithLeftChild nt )
              else  nt )
           end
         else   
           begin 
             N.rightSet nt (add x r);
             (if N.height r > 2 + N.height l  then 
                (if N.key (unsafeCoerce r) < x then 
                   rotateWithRightChild nt 
                 else 
                   doubleWithRightChild nt
                ) else 
                nt
             )
           end
        ) in 
        let hlt, hrt = N.(height (left t),(height (right t))) in 
        N.hSet t 
          N.(Pervasives.max hlt hrt  + 1);
        N.return t
      end