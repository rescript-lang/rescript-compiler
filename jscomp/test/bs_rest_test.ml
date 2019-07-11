[%%bs.raw
{|
function x(v){return [v]}
|}]

external f : ('a -> 'a array[@bs]) = "x" [@@bs.val]

let u = (f "3" [@bs])
let v = (f 3 [@bs])

include (
  struct
    external xxx : ('a -> 'a array[@bs]) = "x" [@@bs.val]
  end :
    sig
      val xxx : ('a -> 'a array[@bs])
    end )

let u = (xxx 3 [@bs])

(** Do we need both [bs.val] and [bs.call]* instead of just one [bs.val] *)
let xx = (xxx "3" [@bs])
