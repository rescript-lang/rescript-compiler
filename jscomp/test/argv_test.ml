

let anno_fun arg =
  ()
  (* Js.log arg  *)
let usage_msg = "Usage:\n";;
let compile = ref false
let test = ref true
let arg_spec = Arg.[
    "-c" , Set compile ,
    " Compile";
    "-d", Clear test,
    " Test"
  ]

;;
Arg.parse arg_spec anno_fun usage_msg
