
#if 0 then
type t = 
  | Monday
  | Tuesday
  | SpecialDay of int 
  | A of int * int 
  | B of (int * int) 

  | D of t 
        [@@deriving bs.repr]
include (struct 
  let all_branches_of_t : Bs_dyn.variant_shape  = 
    Bs_dyn.shape_of_variant
        [|"Monday"; "Tuesday" ; "SpecialDay" |]
        [| 0; 0; 1; 2 ; 1 |]
  let rec _t_to_value  (value : t) :Bs_dyn.value = 
    match value with 
    | Monday -> Bs_dyn.variant_to_value all_branches_of_t 0 [||]
    | Tuesday -> Bs_dyn.variant_to_value all_branches_of_t 1 [||]
    | SpecialDay v 
      -> 
      Bs_dyn.variant_to_value all_branches_of_t 2  
        [| Bs_dyn.int_to_value v [@bs] |]
    | A (x,y) 
      ->
      Bs_dyn.variant_to_value all_branches_of_t 3 
        [| Bs_dyn.int_to_value x [@bs] ;
           Bs_dyn.int_to_value y [@bs] |]      
    | B z
      -> Bs_dyn.variant_to_value all_branches_of_t 4 
           [| 
             (Bs_dyn.tuple_2_to_value
               Bs_dyn.int_to_value
               Bs_dyn.int_to_value)  z [@bs] 
           |]
    | D x -> 
      Bs_dyn.variant_to_value all_branches_of_t 5
        [| _t_to_value x |]
  let t_to_value : t Bs_dyn.to_value = 
    fun [@bs] v -> _t_to_value v 
end : sig 
  val t_to_value : t Bs_dyn.to_value 
end)

type u = 
  { x : int ; y : t ; z : string ; u : int option}
    [@@deriving bs.repr]
include (struct 
  let shape  = Bs_dyn.shape_of_record
    [| "x"; "y"; "z" |] 
  let u_to_value : u Bs_dyn.to_value  = 
    fun [@bs] (value : u) 
      -> 
        match value with 
        | { x ; y ; z } -> 
          Bs_dyn.record_to_value shape
            [| Bs_dyn.int_to_value  x [@bs] ; 
               t_to_value y [@bs] ;
               Bs_dyn.string_to_value z [@bs] |]
end : sig 
  val u_to_value : u Bs_dyn.to_value 
end)
#end