
type seed = int
external caml_hash_mix_string : seed -> string -> seed  = "caml_hash_mix_string"
external final_mix : seed -> seed = "caml_hash_final_mix"

let hash_string  seed s = 
  let hash = ref seed in 
  hash := caml_hash_mix_string !hash s ;
  final_mix !hash

let hashString : string -> int [@bs] = [%raw{|function (str) {
  var hash = 5381,
      i    = str.length | 0;

  while(i !== 0) {
    hash = (hash * 33) ^ str.charCodeAt(--i);
  }
  return hash  
}
|}]

module String = 
  (val Bs.Hash.make 
    ~eq:(fun[@bs] (x:string) y -> x = y )
    ~hash:(fun [@bs] (x : string) -> Hashtbl.hash x ))
    
module String1 = 
  (val Bs.Hash.make 
    ~eq:(fun[@bs] (x:string) y -> x = y )
    ~hash:hashString)
module String2 = 
  (val Bs.Hash.make 
    ~eq:(fun[@bs] (x:string) y -> x = y )
    ~hash:(fun [@bs] (x:string) -> hash_string 0 x))

module Int = 
  (val Bs.Hash.make 
    ~eq:(fun[@bs] (x:int) y -> x = y )
    ~hash:(fun [@bs] x -> Hashtbl.hash x))

let empty = 
  Bs.HashMap.create (module Int) 500_000
    
let bench() = 
  let count  = 1_000_000 in   
  let add = Bs.HashMap.add in 
  let mem = Bs.HashMap.mem in
  for i  = 0 to  count do      
     add empty i i
  done ;
  for i = 0 to count do 
    assert (mem empty i)
  done ;
  Bs.HashMap.logStats empty


let count  = 1_000_000 

(*
    (empty : _ Bs.HashMap.t)
    #.add (string_of_int i) i 
    #.add (string_of_int i) i
*)    
let bench2 (type t) (m : (string,t) Bs.Hash.t) = 
  let empty = 
    Bs.HashMap.create m 500_000 in
  let module String = (val m) in     
  let hash = String.hash in 
  let eq = String.eq in 
  let table = empty.data in 
  for i  = 0 to  count do  
    Bs.HashMap.add0 ~hash
       table (string_of_int i) i 
  done ;
  for i = 0 to count do 
    assert (Bs.HashMap.mem0 
      ~hash ~eq
      table (string_of_int i))
  done; 
  ()
  (* Bs.HashMap.logStats empty *)
  
let bench3 (type t) (m : (string,t) Bs.Cmp.t) = 
  let empty = Bs.Map.empty m in
  let module String = (val m) in 
  let cmp = String.cmp in 
  let table = ref empty.data in 
  for i  = 0 to  count do  
    table := Bs.Map.add0 ~cmp
        (string_of_int i) i !table
  done ;
  for i = 0 to count do 
    assert (Bs.Map.mem0 ~cmp
    
       (string_of_int i) !table)
  done; 
  
module S = (val Bs.Cmp.make (fun [@bs] (x : string) y -> compare x y )) 

;; [%time bench2 (module String1)]
;; [%time bench2 (module String)]
;; [%time bench2 (module String2)]
;; [%time bench3 (module S)]
