/* Mixin */
module Impl = (
  T: {
    type t
  },
) => {
  @bs.send.pipe(: T.t) external remove: unit = ""
}
