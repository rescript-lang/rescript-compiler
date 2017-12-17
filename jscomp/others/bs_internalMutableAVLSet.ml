

module N = Bs_internalAVLset

type 'elt node = 'elt N.node

type ('elt,'id) t0 = 'elt node Js.null

external leftSet :
  'elt node ->'elt node Js.null -> unit = "left" [@@bs.set]
external keySet : 
  'elt node -> 'elt -> unit = "key" [@@bs.set]
external rightSet :
  'elt node -> 'elt node Js.null -> unit = "right" 
[@@bs.set]
external hSet : 'elt node -> int -> unit = "h" 
[@@bs.set]  
external unsafeCoerce : 'a Js.null -> 'a = "%identity"

let maxInt (x : int) y = if x > y then x else y  

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
  leftSet k2 (N.right k1); 
  rightSet k1 (N.return k2 );
  hSet k2 
    N.(maxInt (height (left k2)) (height (right k2)) + 1);
  hSet k1 N.(maxInt (height (left k1)) (h k2) + 1);
  k1  
(* right rotation *)
let rotateWithRightChild k1 =   
  let k2 = unsafeCoerce (N.right k1) in 
  rightSet k1 (N.left k2);
  leftSet k2 (N.return k1);
  hSet k1 N.(maxInt (height (left k1)) (height (right k1)) + 1);
  hSet k2 N.(maxInt (height (right k2)) (h k1) + 1);
  k2 

(*
  double l rotation
*)  
let doubleWithLeftChild k3 =   
  let v = rotateWithLeftChild (unsafeCoerce N.(left k3)) in 
  leftSet k3 (N.return v );
  rotateWithLeftChild k3 

let doubleWithRightChild k2 = 
  let v = rotateWithRightChild (unsafeCoerce N.(right k2)) in   
  rightSet k2 (N.return v);
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
             leftSet nt (add x l);
             (if N.height l > 2 +  N.height r then 
                (if x < N.key (unsafeCoerce l) then 
                    rotateWithLeftChild nt 
                 else 
                    doubleWithLeftChild nt )
              else  nt )
           end
         else   
           begin 
             rightSet nt (add x r);
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
        hSet t 
          N.(maxInt (height (left t)) (height (right t))  + 1);
        N.return t
      end