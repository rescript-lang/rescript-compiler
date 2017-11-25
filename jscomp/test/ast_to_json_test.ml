
let (+>) ( x : 'a  ) (i : int ) =  
  Obj.magic (Obj.field (Obj.repr x) i )

type t = {
  x : int ; 
  y : int ;
}

let tIsImmediateJson = true   
let tToJson (x : t)  : Js.Json.t = 
  Obj.magic x 

let tJsonValidate (x : Js.Json.t) = 
  Js.Json.test x Js.Json.Array &&
  Array.length (Obj.magic x) = 2 &&
  Js.Json.test (x +> 0 ) Js.Json.Number &&
  Js.Json.test (x +> 1 ) Js.Json.Number 
(* check Number.isInteger or (x | 0 == x)  *)

(* (Js.typeof result.x = "number" && Js.typeof result.y = "number") *)
let tFromJson (x : Js.Json.t)  : t =  
  assert (tJsonValidate x);
  Obj.magic x 





type u = {
  uy : string ; 
  ut : t ;
}  
let uIsImmediateJson = tIsImmediateJson

let uToJson (x : u) : Js.Json.t = 
  if uIsImmediateJson then 
    Obj.magic x
  else  
    Obj.magic (x.uy, tToJson x.ut)

let uJsonValidate ( x : Js.Json.t) = 
  Js.Json.test x Js.Json.Array &&
  Array.length (Obj.magic x) = 2 &&
  Js.Json.test (x +> 0) Js.Json.Number &&
  tJsonValidate (x +> 1)

(** Q: shall we validate json and do the decode at the same time or separately
    1. the former is more efficient but not easy to generate (code bloat)
    2. there is a trade off for json validation, do you really need validate every 
      item of array  is of type int, seems quite expensive
    3. Check JSON schema validation (https://davidwalsh.name/json-validation)  
    4. Maybe we should generate a json schema per type
    5. Validaation should be made optional (only needed for user input)
*)  
let uFromJson ( x : Js.Json.t) : u =     
  assert (uJsonValidate x);
  if uIsImmediateJson then 
    Obj.magic x 
  else 
    Obj.magic (x +> 0 , tFromJson (x +> 1))

type v =     
  | V0 
  | V1 of int * u  * t 
  | V2 of  t



let vIsImmediateJson = false   
let vJsonValidate (x : Js.Json.t)  =    
  if Js.Json.test x Js.Json.Number then 
    true (* refine *)
  else   
    begin 
      Js.Json.test x Js.Json.Array && 
      match x +> 0 with 
      | 0  -> 
        Js.Json.test (x +> 1) Js.Json.Number &&
        uJsonValidate (x +> 2) &&
        tJsonValidate (x +> 3)
      | 1 -> 
        tJsonValidate (x +> 1)
      | _ -> assert false 
    end 
let vToJson ( x : v) : Js.Json.t = 
  match x with 
  | V0 ->  Obj.magic x
  | V1 (i,u,v) ->
    Obj.magic (0, i, uToJson u, tToJson v)
  | V2 v ->
    Obj.magic (1, tToJson v)

let vFromJson ( x : Js.Json.t) : v = 
  if Js.Json.test x Js.Json.Number then 
    Obj.magic x 
  else   
    match x +> 0 with 
    | 0 -> V1 (x +> 1, uFromJson (x +>2), tFromJson (x +>3))
    | 1 -> V2 ( tFromJson (x+>1))
    | _ -> assert false (* [%assert false ]*)

(* recursive data structure 
   it is better to linearize to serialize
*)    
type h =    
  | HNone
  | HSome of v * h 

let hIsImmediateJson = vIsImmediateJson  

let rec hJsonValidate ( x : Js.Json.t) = 
  if Js.Json.test x Js.Json.Number then 
    (Obj.magic x = 0)
  else   
    begin 
      Js.Json.test x Js.Json.Array &&
      x +> 0 = 0 &&
      vJsonValidate (x +> 1) &&
      hJsonValidate (x +> 1)
    end

let rec hToJson ( x : h) : Js.Json.t =     
  if hIsImmediateJson then Obj.magic x 
  else 
    match x with 
    | HNone -> Obj.magic x 
    | HSome (v,h) -> Obj.magic (vToJson v, hToJson h)

let rec hFromJson (x : Js.Json.t) : h =     
  assert (hJsonValidate x) ; 
  if hIsImmediateJson then Obj.magic x 
  else 
  if Js.Json.test x Js.Json.Number then Obj.magic x 
  else
    Obj.magic ( vFromJson ( x +>1 ), hFromJson (x +> 2))


type z =     
  | V0 of int 
  | V1 of int list 
  | V2 of h list 
  | V3 of int array
  | V4 of h array 

let intArrayToJson ( x : int array) : Js.Json.t =   
  Obj.magic x 

let arrayToJson ( p : 'a -> Js.Json.t)  (x : 'a array)  : Js.Json.t =  
  assert false 

let arrayFromJson (p : Js.Json.t -> 'a) ( x : Js.Json.t) : 'a array =   
  assert false 
let intArrayFromJson ( x : Js.Json.t) : int array =   
  assert false 

let arrayJsonValidate (p : Js.Json.t -> bool) (x : Js.Json.t) : bool =   
  assert false 

let intArrayJsonValidate (x : Js.Json.t) : bool =   
  assert false 

(** assume we linearize list *)
let intListJsonValidate (x : Js.Json.t) : bool =   
  intArrayJsonValidate x 
  (* Js.Json.test x Js.Json.Array &&
  for i = 0 to Array.length (Obj.magic x) - 1  do
     ignore (Js.Json.test (Array.unsafe_get (Obj.magic x) i ) Js.Json.Number)
  done *)
  
let listToJson (p : 'a -> Js.Json.t) (x : 'a list) : Js.Json.t =   
  arrayToJson p (Array.of_list x )
let intListToJson ( x : int list) : Js.Json.t =   
  intArrayToJson (Array.of_list x)

let listFromJson (p : Js.Json.t -> 'a) (x : Js.Json.t) : 'a list =   
  Array.to_list (arrayFromJson p  x)

let intListFromJson (x : Js.Json.t) : int list = 
  Array.to_list (intArrayFromJson x)


let listJsonValidate (p : Js.Json.t -> bool)  (x : Js.Json.t) : bool =
  arrayJsonValidate p x

let zIsImmedateJson = false     

let rec zJsonValidate (x : Js.Json.t) = 
  Js.Json.test x Js.Json.Array &&
  match x +> 0 with  
  | 0  -> Js.Json.test (x +> 1) Js.Json.Number
  | 1 ->  intListJsonValidate (x +> 1)
  | 2 -> listJsonValidate hJsonValidate (x+>1)
  | 3 -> intArrayJsonValidate (x +> 1)
  | 4 ->  arrayJsonValidate hJsonValidate (x+>1)
  | _ -> false 

let zToJson (x  : z ) : Js.Json.t = 
  match x with 
  | V0 i  
    -> 
    Obj.magic (0, i)
  | V1 is 
    -> 
    Obj.magic (1, intListToJson is)
  | V2 hs -> 
    Obj.magic (2, listToJson hToJson hs)
  | V3 ias -> 
    Obj.magic (3, intArrayToJson ias)
  | V4 has -> 
    Obj.magic (4, arrayToJson hToJson has)

let zFromJson ( x : Js.Json.t) =     
  assert (zJsonValidate x) ; 
  match x +> 0 with  
  | 0 -> V0 (x+>1)
  | 1  -> V1 ( Array.to_list (x +> 1))
  | 2  -> 
    V2 (if hIsImmediateJson then Array.to_list (x+>1)
        else listFromJson hFromJson (x +> 1))
  | 3  -> 
    V3 (x +> 1)
    (* V3 (intArrayFromJson (x +>1 )) *)
  | 4  -> 
    V4 (if hIsImmediateJson then x+>1 else  arrayFromJson hFromJson ( x+> 1))
  | _ -> assert false