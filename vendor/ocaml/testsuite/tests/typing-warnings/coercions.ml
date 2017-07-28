(* comment 9644 of PR#6000 *)

fun b -> if b then format_of_string "x" else "y";;
fun b -> if b then "x" else format_of_string "y";;
fun b -> (if b then "x" else "y" : (_,_,_) format);;
