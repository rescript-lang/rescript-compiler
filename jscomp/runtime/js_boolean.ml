type t
external true_ : t = "true" [@@bs.val]
external false_ : t = "false" [@@bs.val]

let to_js_boolean b = if b then true_ else false_
