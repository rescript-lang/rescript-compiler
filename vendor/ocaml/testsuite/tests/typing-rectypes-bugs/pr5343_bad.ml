module M : sig
  type 'a t
  type u = u t and v = v t
  val f : int -> u
  val g : v -> bool
end = struct
  type 'a t = 'a
  type u = int and v = bool
  let f x = x
  let g x = x
end;;

let h (x : int) : bool = M.g (M.f x);;
