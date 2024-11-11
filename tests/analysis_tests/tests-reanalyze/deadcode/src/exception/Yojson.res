exception Json_error(string)

module Basic = {
  type t

  @raises(Json_error)
  let from_string: string => t = _ => raise(Json_error("Basic.from_string"))

  module Util = {
    exception Type_error(string, t)

    @raises(Type_error)
    let member: (string, t) => t = (_s, j) => raise(Type_error("Basic.Util.member", j))

    let to_int: t => int = _ => 34

    let to_string: t => string = _ => ""
  }
}
