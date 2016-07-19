

type t = 
  | Monday
  | Tuesday
  | SpecialDay of int 
  | A of int * int 
  | B of (int * int) 

  | D of t 
        [@@deriving bs.repr]
include (struct 
  let all_branches_of_t : Js_dyn.variant_shape  = 
    Js_dyn.shape_of_variant
        [|"Monday"; "Tuesday" ; "SpecialDay" |]
        [| 0; 0; 1; 2 ; 1 |]
  let rec _t_to_value  (value : t) :Js_dyn.value = 
    match value with 
    | Monday -> Js_dyn.variant_to_value all_branches_of_t 0 [||]
    | Tuesday -> Js_dyn.variant_to_value all_branches_of_t 1 [||]
    | SpecialDay v 
      -> 
      Js_dyn.variant_to_value all_branches_of_t 2  
        [| Js_dyn.int_to_value v [@bs] |]
    | A (x,y) 
      ->
      Js_dyn.variant_to_value all_branches_of_t 3 
        [| Js_dyn.int_to_value x [@bs] ;
           Js_dyn.int_to_value y [@bs] |]      
    | B z
      -> Js_dyn.variant_to_value all_branches_of_t 4 
           [| 
             (Js_dyn.tuple_2_to_value
               Js_dyn.int_to_value
               Js_dyn.int_to_value)  z [@bs] 
           |]
    | D x -> 
      Js_dyn.variant_to_value all_branches_of_t 5
        [| _t_to_value x |]
  let t_to_value : t Js_dyn.to_value = 
    fun [@bs] v -> _t_to_value v 
end : sig 
  val t_to_value : t Js_dyn.to_value [@bs]
end)

type u = 
  { x : int ; y : t ; z : string ; u : int option}
    [@@deriving bs.repr]
include (struct 
  let shape  = Js_dyn.shape_of_record
    [| "x"; "y"; "z" |] 
  let u_to_value : u Js_dyn.to_value  = 
    fun [@bs] (value : u) 
      -> 
        match value with 
        | { x ; y ; z } -> 
          Js_dyn.record_to_value shape
            [| Js_dyn.int_to_value  x [@bs] ; 
               t_to_value y [@bs] ;
               Js_dyn.string_to_value z [@bs] |]
end : sig 
  val u_to_value : u Js_dyn.to_value 
end)
