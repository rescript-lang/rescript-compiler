type t
@set external clearNodeValue: (t, @as(json`null`) _) => unit = "nodeValue"

/* TODO: more test cases */
/* external clearNodeValue2 : */
/* t -> (_ [@bs.as {json|null|json}]) -> int -> unit = */
/* "nodeValue" [@@bs.set] */

let test = x => clearNodeValue(x)
