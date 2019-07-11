external test_string_type :
  flag:([`on_closed | `on_open | `in_[@bs.as "in"]][@bs.string]) -> string
  = "hey_string"
  [@@bs.val]

external test_int_type :
     ([`on_closed | `on_open[@bs.as 3] | `in_ | `again[@bs.as 5] | `hey][@bs.int
                                                                      ])
  -> int = "hey_int"
  [@@bs.val]

val uu : string array
val vv : int array

type readline

val register : readline -> unit

val test :
     readline
  -> [`close of (unit -> unit[@bs]) | `line of (string -> unit[@bs])]
  -> unit

val on2 :
     readline
  -> [`line of (string -> unit[@bs]) | `close of (unit -> unit[@bs])]
  -> unit

val read : string -> string
val readN : string -> string
