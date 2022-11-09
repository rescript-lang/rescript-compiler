let equal_int (x : int) y = x = y
let equal_string (x : string) y = x = y
let equal_bool (x : bool) y = x = y
let equal_float (x : float) y = x = y
let equal_int64 (x : int64) y = x = y

let equal_option f x y =
  match x with
  | None -> y = None
  | Some x -> begin
    match y with
    | None -> false
    | Some y -> f x y
  end

let compare_string (x : string) y = compare x y

let compare_option cmp x y =
  match x with
  | None ->
    (match y with
    | None -> 0
    | Some _ -> -1)
  | Some x ->
    (match y with
    | None -> 1
    | Some y -> cmp x y)

let compare_bool (x : bool) (y : bool) = compare x y
(* TODO : turn it into externals *)
module Ppx_compare_lib = struct 
  external polymorphic_compare : 'a -> 'a -> int = "%compare"
  external phys_equal : 'a -> 'a -> bool = "%eq"
  
  external ( && ) : bool -> bool -> bool = "%sequand"

  external   polymorphic_equal : 'a -> 'a -> bool = "%equal"
end  

