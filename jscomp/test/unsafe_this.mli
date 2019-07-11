val js_obj :
  [%bs.obj:
    < bark: ('a -> int -> int -> int[@bs.this])
    ; length: int
    ; x: int
    ; y: int >
    as
    'a]

val uux_this : (< length: int > Js.t -> int -> int -> int[@bs.this])
