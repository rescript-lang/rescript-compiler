module Foo :
  sig
    external makeProps : ?bar:string array -> string = ""[@@bs.obj ]
  end =
  struct external makeProps : ?bar:'bar -> string = ""[@@bs.obj ] end 