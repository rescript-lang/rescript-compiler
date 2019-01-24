type t = A : t;;

module X1 : sig end = struct
  let _f ~x (* x unused argument *) = function
    | A -> let x = () in x
end;;

module X2 : sig end = struct
  let x = 42 (* unused value *)
  let _f = function
    | A -> let x = () in x
end;;

module X3 : sig end = struct
  module O = struct let x = 42 (* unused *) end
  open O (* unused open *)

  let _f = function
    | A -> let x = () in x
end;;
