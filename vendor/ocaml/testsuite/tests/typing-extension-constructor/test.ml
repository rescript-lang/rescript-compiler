
type t = ..;;
type t += A;;

[%extension_constructor A];;
([%extension_constructor A] : extension_constructor);;

module M = struct
  type extension_constructor = int
end;;

open M;;

([%extension_constructor A] : extension_constructor);;
