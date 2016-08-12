let () =
 let hello_ocaml = [|"h";"e";"y";"o";"c";"a";"m";"l"|] in
 hello_ocaml |> Array.to_list |> String.concat "," |> Js.log
        