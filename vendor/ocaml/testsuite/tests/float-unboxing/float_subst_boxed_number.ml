module PR_6686 = struct
  type t =
   | A of float
   | B of (int * int)

  let rec foo = function
   | A x -> x
   | B (x, y) -> float x +. float y

  let (_ : float) = foo (A 4.)
end

module PR_6770 = struct
  type t =
  | Constant of float
  | Exponent of (float * float)

  let to_string = function
    | Exponent (_b, _e) ->
      ignore _b;
      ignore _e;
      ""
    | Constant _ -> ""

  let _ = to_string (Constant 4.)
end
