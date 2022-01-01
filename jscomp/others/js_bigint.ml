(** JavaScript BigInt API *)

type t

external ofString : string -> t = "BigInt" [@@bs.val]
external ofInt : int -> t = "BigInt" [@@bs.val]
external ofFloat : float -> t = "BigInt" [@@bs.val]

external toString : t -> string = "toString" [@@bs.send]
external toStringWithRadix : t -> radix:int -> string = "toString" [@@bs.send]

external (+) : t -> t -> t = "%addfloat"
external (-) : t -> t -> t = "%subfloat"
external ( * ) : t -> t -> t = "%mulfloat"
external (/) : t -> t -> t = "%divfloat"

external add : t -> t -> t = "%addfloat"
external sub : t -> t -> t = "%subfloat"
external mul : t -> t -> t = "%mulfloat"
external div : t -> t -> t = "%divfloat"

external (mod) : t -> t -> t = "caml_fmod_float" [@@noalloc]

external (land) : t -> t -> t = "%andint"
external (lor) : t -> t -> t = "%orint"
external (lxor) : t -> t -> t = "%xorint"

external (lsl) : t -> t -> t = "%lslint"
external (asr) : t -> t -> t = "%asrint"

let exp x y =
  let _ = x in
  let _ = y in
  [%raw "x ** y"] [@@inline]
