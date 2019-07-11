type t

external clearNodeValue : t -> (_[@bs.as {json|null|json}]) -> unit
  = "nodeValue"
  [@@bs.set]

(* TODO: more test cases *)
(* external clearNodeValue2 : *)
(* t -> (_ [@bs.as {json|null|json}]) -> int -> unit = *)
(* "nodeValue" [@@bs.set] *)

let test x = clearNodeValue x
