external fact: int -> string = "factorial"

let () =
  Api.reg_mod "plugin_ext";
  Printf.printf "fact 10 = %s\n" (fact 10)
