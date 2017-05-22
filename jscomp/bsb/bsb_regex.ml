let string_after s n = String.sub s n (String.length s - n)



(* There seems to be a bug in {!Str.global_substitute} 
{[
Str.global_substitute (Str.regexp "\\${bsb:\\([-a-zA-Z0-9]+\\)}") (fun x -> (x^":found")) {|   ${bsb:hello-world}  ${bsb:x} ${x}|}  ;;
- : bytes =
"      ${bsb:hello-world}  ${bsb:x} ${x}:found     ${bsb:hello-world}  ${bsb:x} ${x}:found ${x}"
]}
*)

let global_substitute expr repl_fun text =
  let text_len = String.length text in 
  let expr = Str.regexp expr in  
  let rec replace accu start last_was_empty =
    let startpos = if last_was_empty then start + 1 else start in
    if startpos > text_len then
      string_after text start :: accu
    else
      match Str.search_forward expr text startpos with
      | exception Not_found -> 
        string_after text start :: accu
      |  pos ->
        let end_pos = Str.match_end() in
        let matched = (Str.matched_string text) in 
        let  groups = 
            let rec aux n  acc = 
                match Str.matched_group n text with 
                | exception (Not_found | Invalid_argument _ ) 
                    -> acc 
                | v -> aux (succ n) (v::acc) in 
             aux 1 []  in 
        let repl_text = repl_fun matched groups  in
        replace (repl_text :: String.sub text start (pos-start) :: accu)
          end_pos (end_pos = pos)
  in
  String.concat "" (List.rev (replace [] 0 false))
