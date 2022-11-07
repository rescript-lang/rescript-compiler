type t = Dom.treeWalker

@get external root: t => Dom.node = ""
@get external whatToShow: t => Webapi__Dom__Types.WhatToShow.t = ""
@get @return(nullable) external filter: t => option<Dom.nodeFilter> = ""
@get external currentNode: t => Dom.node = ""
@set external setCurrentNode: (t, Dom.node) => unit = ""

@bs.send.pipe(: t) @return(nullable) external parentNode: option<Dom.node> = ""
@bs.send.pipe(: t) @return(nullable) external firstChild: option<Dom.node> = ""
@bs.send.pipe(: t) @return(nullable) external lastChild: option<Dom.node> = ""
@bs.send.pipe(: t) @return(nullable) external previousSibling: option<Dom.node> = ""
@bs.send.pipe(: t) @return(nullable) external nextSibling: option<Dom.node> = ""
@bs.send.pipe(: t) @return(nullable) external previousNode: option<Dom.node> = ""
@bs.send.pipe(: t) @return(nullable) external nextNode: option<Dom.node> = ""
