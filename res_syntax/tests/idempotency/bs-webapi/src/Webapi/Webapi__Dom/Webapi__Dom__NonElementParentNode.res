/* Mixin */
module Impl = (
  T: {
    type t
  },
) => {
  @bs.send.pipe(: T.t) @return(nullable) external getElementById: string => option<Dom.element> = ""
}
