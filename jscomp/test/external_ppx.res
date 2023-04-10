@obj external make_config: (~length: int, ~width: int) => unit = ""

@obj
@ocaml.doc(" Note that 
    {[ 'a . length: 'a -> width:int -> unit
    ]} is a syntax error -- check where it is allowed
")
external make_config: (~length: 'a, ~width: int) => unit = ""

@obj external opt_make: (~length: int, ~width: int=?) => (_ as 'event) = ""

@obj
external ff: (
  ~hi: int,
  ~lo: @as(3) _,
  ~lo2: @as(json`{hi:-3 }`) _,
  ~lo3: @as(-1) _,
  ~lo4: @as(json`-3`) _,
) => _ = ""

let u = ff(~hi=2)

@genType.import("hh") external f: int => int = "f"
