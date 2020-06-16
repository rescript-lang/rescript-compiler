

type value =
  | Link of {
      mutable next : value
    }
  | Root of {id : int }

type t = value Map_int.t


let rec find (v : value) =   
  match v with 
  | Link {next } -> find next 
  | Root {id } -> id   

(* assume k1 != k2 *)
let equal (map : t)  k1 k2 = 
  match Map_int.find_opt map k1, Map_int.find_opt map k2 with 
  | None, None 
  | None, Some _ 
  | Some _, None 
    -> false   
  | Some v1, Some v2 -> 
    find v1 = find v2


(* let union (map : t) k1 k2 = 
  match Map_int.find_opt k1 with 
  | None ->  *)