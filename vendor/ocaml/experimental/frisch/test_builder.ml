type t =
    {
     x: int;
     y [@label foo]: int;
     z [@default 3]: int;
    } [@@builder]

and s =
    {
     a: string;
     b [@opt]: int option;
     c: int [@default 2];
    } [@@builder]

and sum =
  | A of int
  | B of string * (string [@label str])
  | C of (int [@label i] [@default 0]) * (string [@label s] [@default ""])
        [@@builder]
