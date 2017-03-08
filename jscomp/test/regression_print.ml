include (struct
external to_str : 'a -> string = "#json_stringify"
let debug x = print_endline (to_str x )

let  () =
  begin
    debug @@ 2  ;
    debug 1
  end

end : sig end)
