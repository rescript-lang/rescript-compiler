
type seed = int
external caml_hash_mix_string : seed -> string -> seed  = "caml_hash_mix_string"
external final_mix : seed -> seed = "caml_hash_final_mix"

let hash_string  s = 
  final_mix (caml_hash_mix_string 0 s) 

let hashString : string -> int  = [%raw{|function (str) {
                                              var hash = 5381,
                                              i    = str.length | 0;

                                              while(i !== 0) {
                                              hash = (hash * 33) ^ str.charCodeAt(--i);
                                              }
                                              return hash  
                                              }
                                            |}]

module String = 
  (val Bs.Id.hashable 
      ~eq:(fun (x:string) y -> x = y )
      ~hash:Hashtbl.hash)

module String1 = 
  (val Bs.Id.hashable
      ~eq:(fun (x:string) y -> x = y )
      ~hash:hashString)
module String2 = 
  (val Bs.Id.hashable
      ~eq:(fun (x:string) y -> x = y )
      ~hash:(fun  (x:string) -> hash_string x))

module Int = 
  (val Bs.Id.hashable
      ~eq:(fun (x:int) y -> x = y )
      ~hash:Hashtbl.hash)
module N = Bs.HashMap
let empty = 
  N.make ~dict:(module Int) 500_000

let bench() = 
  let count  = 1_000_000 in   
  (* let add = N.setDone in  *)
  let mem = N.has in
  for i  = 0 to  count do      
    N.set empty i i
  done ;
  for i = 0 to count do 
    assert (mem empty i)
  done ;
  N.logStats empty


let count  = 1_000_000 
let initial_size = 1_000_000
(* module B = Bs.Bag  *)
(*
    (empty : _ Bs.HashMap.t)
    #.add (string_of_int i) i 
    #.add (string_of_int i) i
*)    
module M = Bs.HashMap
let bench2 (type t) (m : (string,t) Bs.Id.hashable) = 
  let empty = 
    M.make ~dict:m initial_size in
  let module String = (val m) in     
  (* let hash = String.hash in 
  let eq = String.eq in  *)
  (* let table = M.getData empty in  *)
  for i  = 0 to  count do  
     M.set
      empty (string_of_int i) i 
  done ;
  for i = 0 to count do 
    assert (M.has
              empty (string_of_int i))
  done; 
  for i = 0 to count do 
    M.remove empty (string_of_int i)
  done ;
  assert (M.size empty = 0)  

(* Bs.HashMap.logStats empty *)
module Md = Bs.Map 
module Md0 = Bs.Map.Dict
let bench3 (type t) (m : (string,t) Bs.Id.comparable) = 
  
  let empty = Md.make m in
  let module String = (val m) in 
  let cmp = String.cmp in 
  let table = ref (Md.getData empty) in 
  for i  = 0 to  count do  
    table := Md0.set ~cmp !table
        (string_of_int i) i 
  done ;
  for i = 0 to count do 
    assert (Md0.has ~cmp
              !table
              (string_of_int i) )
  done; 
  for i = 0 to count do  
    table := Md0.remove ~cmp !table (string_of_int i) 
  done ;
  assert (Md0.size !table = 0)

module Sx = (val Bs.Id.comparable ~cmp:(fun  (x : string) y -> compare x y )) 
module H = Bs.HashMap.String
let bench4 () = 
  let table = 
    H.make initial_size in

  for i  = 0 to  count do  
    H.set
      table (string_of_int i) i 
  done ;
  for i = 0 to count do 
    assert (H.has
              table (string_of_int i))
  done; 
  for i = 0 to count do 
    H.remove table (string_of_int i)
  done ;
  assert (H.isEmpty table)  

module H0 = Bs.HashMap
let bench5 () =   
  let table = 
    H0.make ~dict:(module Int) initial_size in 
  (* let table_data = M.getData table in    *)
  (* let hash = Int.hash in 
  let eq = Int.eq in  *)
  [%time for i  = 0 to  count do  
      H0.set 
        table i i 
    done] ;
  [%time for i = 0 to count do 
      assert (H0.has
                table i)
    done]; 
  [%time for i = 0 to count do 
      H0.remove  table i
    done ];
  assert (H0.isEmpty table)   

module HI = Bs.HashMap.Int
let bench6 () = 
  let table = 
    HI.make initial_size in

  for i  = 0 to  count do  
    HI.set
      table i i 
  done ;
  for i = 0 to count do 
    assert (HI.has
              table i)
  done; 
  for i = 0 to count do 
    HI.remove table i
  done ;
  assert (HI.size table = 0)  

module S = Bs.HashSet.Int
let bench7 () = 
  let table = 
    (* [%time  *)
    S.make (initial_size* 2)
    (* ]  *)
    in

  (* [%time  *)
  for i  = 0 to  count do  
    S.add
      table i 
  done 
  (* ] *)
  ;
  (* [%time  *)
  for i = 0 to count do 
    assert (S.has
              table i)
  done
  (* ] *)
  ; 
  (* [%time *)
   for i = 0 to count do 
    S.remove table i
  done 
  (* ] *)
  ;
  assert (S.size table = 0)  


(* ;; [%time bench4 ()]
   ;; [%time bench4 ()]
   ;; [%time bench2 (module String1)]
   ;; [%time bench2 (module String2)]

   ;; [%time bench3 (module S)] 
   ;; [%time bench5()] *)
(* ;; [%time bench6 ()] *)
;; [%time bench7 ()]
(* ;; [%time bench7 ()]
;; [%time bench7 ()]
;; [%time bench7 ()]
;; [%time bench7 ()]
;; [%time bench7 ()] *)