type t = Dom.selection

@get @return(nullable) external anchorNode: t => option<Dom.node> = ""
@get external anchorOffset: t => int = ""
@get @return(nullable) external focusNode: t => option<Dom.node> = ""
@get external focusOffset: t => int = ""
@get external isCollapsed: t => bool = ""
@get external rangeCount: t => int = ""

@bs.send.pipe(: t) external getRangeAt: int => Dom.range = ""
@bs.send.pipe(: t) external collapse: (Dom.node_like<_>, int) => unit = ""
@bs.send.pipe(: t) external extend: (Dom.node_like<_>, int) => unit = ""
@bs.send.pipe(: t) external collapseToStart: unit = ""
@bs.send.pipe(: t) external collapseToEnd: unit = ""
@bs.send.pipe(: t) external selectAllChildren: Dom.node_like<_> => unit = ""
@bs.send.pipe(: t)
external setBaseAndExtent: (Dom.node_like<_>, int, Dom.node_like<_>, int) => unit = ""
@bs.send.pipe(: t) external addRange: Dom.range => unit = ""
@bs.send.pipe(: t) external removeRange: Dom.range => unit = ""
@bs.send.pipe(: t) external removeAllRanges: unit = ""
@bs.send.pipe(: t) external deleteFromDocument: unit = ""
@bs.send.pipe(: t) external toString: string = ""
@bs.send.pipe(: t) external containsNode: (Dom.node_like<_>, @as(json`false`) _) => bool = ""
@bs.send.pipe(: t)
external containsNodePartly: (Dom.node_like<_>, @as(json`true`) _) => bool = "containsNode"
