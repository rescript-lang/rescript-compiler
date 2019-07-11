(* external obj : < hi : int > Js.t = "{hi:1}" [@@bs.val] *)

external mk : hi:int -> unit -> < hi: int > Js.t = "" [@@bs.obj]

(* external set_name : < > -> string -> unit = "1name" [@@bs.set] *)
