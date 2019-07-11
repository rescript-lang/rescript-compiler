let u () = match String.length "123" with n -> 3 / 0 | exception _ -> 42

(* TODO: could be optimized *)

;;
Mt.from_pair_suites __MODULE__
  [(__LOC__, fun _ -> ThrowAny (fun _ -> ignore (u ())))]
