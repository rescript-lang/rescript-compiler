type t = Dom.nodeIterator

@get external root: t => Dom.node = ""
@get external referenceNode: t => Dom.node = ""
@get external pointerBeforeReferenceNode: t => bool = ""
@get external whatToShow: t => Webapi__Dom__Types.WhatToShow.t = ""
@get @return(nullable) external filter: t => option<Dom.nodeFilter> = ""

@bs.send.pipe(: t) @return(nullable) external nextNode: option<Dom.node> = ""
@bs.send.pipe(: t) @return(nullable) external previousNode: option<Dom.node> = ""
@bs.send.pipe(: t) external detach: unit = ""
