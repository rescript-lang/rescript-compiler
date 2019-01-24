(* class expressions may also contain local recursive bindings *)
class test =
  let rec f = print_endline "f"; fun x -> g x
      and g = print_endline "g"; fun x -> f x in
object
  method f : 'a 'b. 'a -> 'b = f
  method g : 'a 'b. 'a -> 'b = g
end
