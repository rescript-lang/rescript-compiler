type bar = < bar: unit >

type _ ty = Int : int ty

type dyn = Dyn : 'a ty -> dyn;;

class foo =
  object (this)
    method foo (Dyn ty) =
      match ty with
      | Int -> (this :> bar)
  end;;  (* fail, but not for scope *)

[%%expect{|
type bar = < bar : unit >
type _ ty = Int : int ty
type dyn = Dyn : 'a ty -> dyn
Line _, characters 0-108:
Error: This class should be virtual.
       The following methods are undefined : bar
|}];;
