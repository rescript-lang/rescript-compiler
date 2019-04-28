#if 0 then
module Foo :
  sig
    external makeProps : ?bar:string array -> string = ""[@@bs.obj ]
  end =
  struct external makeProps : ?bar:'bar -> string = ""[@@bs.obj ] end 
#end

type 'a arra = 'a array

external
  f : 
  int -> int -> int array -> unit
  = ""
  [@@bs.send.pipe:int]
  [@@bs.splice]