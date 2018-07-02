let fake_c2 a_type b_type  = 
  match a_type, b_type with 
  | "undefined", _ -> - 1
  | _, "undefined" -> 1 
  | "string", _ -> 1 
  | "number", "number" -> 
      33
  | "number", _ -> 3 
  | _ ->        

        0
;; print_endline @@ string_of_int (fake_c2 "number" "xx")