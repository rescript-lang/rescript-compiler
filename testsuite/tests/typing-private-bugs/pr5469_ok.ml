module M (T:sig type t end)
 = struct type t = private { t : T.t } end
module P
 = struct
       module T = struct type t end
       module R = M(T)
 end
