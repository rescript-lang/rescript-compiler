
module Foo :
  sig
    external makeProps : ?bar:string array -> string = ""[@@bs.obj ]
  end =
  struct external makeProps : ?bar:'bar -> string = ""[@@bs.obj ] end 


type 'a arra = 'a array

external
  f0 : 
  int -> int -> int array -> unit
  = ""
  [@@bs.send.pipe:int]
  [@@bs.splice]

external
  f1 : 
  int -> int -> y:int array -> unit
  = ""
  [@@bs.send.pipe:int]
  [@@bs.splice]  


