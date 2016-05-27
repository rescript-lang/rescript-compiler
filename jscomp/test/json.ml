class type json = object [@uncurry]
  method parse : 'a . string -> 'a
end

type json2 = 
  <
    parse : 'a . string -> 'a
  > [@uncurry] (* can not be inherited *)

class type json3 = object [@uncurry]
  (* inherit json2 *)
  inherit json 
end


external v : json Js.t = "json" [@@bs.val]

let h = v##parse "{ x : 3 , y : 4}"
