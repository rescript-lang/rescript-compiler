type t = ..

module M = struct
  type t += A
  type t += B of int
end

type t += C
type t += D of int * string

let () =
  assert (Obj.extension_constructor  M.A
          == [%extension_constructor M.A]);
  assert (Obj.extension_constructor (M.B 42)
          == [%extension_constructor M.B]);
  assert (Obj.extension_constructor  C
          == [%extension_constructor C]);
  assert (Obj.extension_constructor (D (42, ""))
          == [%extension_constructor D])

let () = print_endline "OK"
