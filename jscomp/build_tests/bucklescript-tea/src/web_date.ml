

type t = <
> Js.t


type date_obj = <
  now : unit -> float [@bs.meth];
> Js.t


external create_date : unit -> t = "Date" [@@bs.new]

external date_obj : date_obj = "Date" [@@bs.val]


let now () = date_obj##now ()
