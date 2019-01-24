module T1 = struct
  type t = A
  type s = X
end

module T2 = struct
  type t = T1.t = A
  type s = X
end

module T3 = struct
  open T1 (* unused open *)
  open T2 (* shadow X, which is later used; but not A, see #6762 *)

  let _ = (A, X) (* X belongs to several types *)
end
