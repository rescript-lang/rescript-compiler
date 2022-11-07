/* Mixin */
module Impl = (
  T: {
    type t
  },
) => {
  @get external children: T.t => Dom.htmlCollection = ""
  @get @return(nullable) external firstElementChild: T.t => option<Dom.element> = ""
  @get @return(nullable) external lastElementChild: T.t => option<Dom.element> = ""
  @get external childElementCount: T.t => int = ""
  @bs.send.pipe(: T.t) @return(nullable) external querySelector: string => option<Dom.element> = ""
  @bs.send.pipe(: T.t) external querySelectorAll: string => Dom.nodeList = ""
}
