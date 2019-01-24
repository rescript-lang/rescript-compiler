module TypEq = struct
 type (_, _) t = Eq : ('a, 'a) t
end

module type T = sig
 type _ is_t = Is : ('a, 'b) TypEq.t -> 'a is_t
 val is_t : unit -> unit is_t option
end

module Make (M : T) =
 struct
   let _ =
     match M.is_t () with
     | None -> 0
     | Some _ -> 0
   let f () =
     match M.is_t () with None -> 0
end;;

module Make2 (M : T) = struct
  type t = T of unit M.is_t
  let g : t -> int = function _ -> .
end;;
