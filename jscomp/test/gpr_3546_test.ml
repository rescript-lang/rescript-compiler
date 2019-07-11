type 'a terminal = T_error : unit terminal [@@bs.deriving accessors]
type 'a terminal2 = T_error2 : unit terminal2 [@@bs.deriving accessors]
type 'a terminal3 = T_error3 : int -> int terminal3 [@@bs.deriving accessors]
