module M = struct
  type t = int
  let x = 42
end
;;
class c =
  let open M in
  object
    method f : t = x
  end
;;
class type ct =
  let open M in
  object
    method f : t
  end
;;
