type t 
external clearNodeValue :
   t -> (_ [@bs.as {json|null|json}]) -> unit =
   "nodeValue" [@@bs.set]


let test x =
  clearNodeValue x
