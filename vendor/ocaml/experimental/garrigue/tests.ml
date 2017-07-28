(* $Id$ *)

let f1 = function `a x -> x=1 | `b -> true
let f2 = function `a x -> x | `b -> true
let f3 = function `b -> true
let f x = f1 x && f2 x

let sub s ?:pos{=0} ?:len{=String.length s - pos} () =
  String.sub s pos len

let cCAMLtoTKpack_options w = function
        `After v1 -> "-after"
        | `Anchor v1 -> "-anchor"
        | `Before v1 -> "-before"
        | `Expand v1 -> "-expand"
        | `Fill v1 -> "-fill"
        | `In v1 -> "-in"
        | `Ipadx v1 -> "-ipadx"
        | `Ipady v1 -> "-ipady"
        | `Padx v1 -> "-padx"
        | `Pady v1 -> "-pady"
        | `Side v1 -> "-side"
