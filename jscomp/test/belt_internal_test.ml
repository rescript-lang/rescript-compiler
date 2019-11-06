
type 'value node  = {
  mutable value : 'value;
  mutable height : int;
  mutable left : 'value t;
  mutable right : 'value t;
}
and 'value t =  'value node option


let treeHeight (n : _ t) =
  match n with
  | None -> 0
  | Some n -> n.height

let rec copy n =
  match n with
  | None -> n
  | Some {left = l; right = r; value = v; height = h} ->    
    Some {
      left = copy l; 
      right = copy r;
      value = v ; height = h
    }