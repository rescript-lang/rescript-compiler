
type map
external empty : unit -> map = "OrderedMap" [@@bs.new] [@@bs.module "immutable"]
external set : map -> int -> int -> map = "set" [@@bs.send]
external get : map -> int -> int option = "get" [@@bs.send] [@@bs.return undefined_to_opt] 
external mem : map -> int -> bool = "has" [@@bs.send] 


module A = Bs_Array
let empty = empty ()
let ofArray kvs = 
  let v = ref empty in 
  for i = 0 to A.length kvs - 1 do
    let key, value = (A.getUnsafe kvs i)  in 
    v := set !v key value 
  done;
  !v 

let should b = 
  if not b  then Js.Exn.raiseError "impossible"
  
let count = 1_000_000 

let shuffledDataAdd = (A.makeByAndShuffle (count +  1) (fun i -> (i,i)))



let test () = 
  let v = ofArray shuffledDataAdd  in 
  for j = 0 to count do 
    should (mem v j)
  done

module M =  Bs.Map.Int


let test2 () = 
  let v = M.ofArray shuffledDataAdd in 
  for j = 0 to count do 
    should (M.has v j)
  done ;


;; [%time test ()]  
;; [%time test2 ()]
