type t = Dom.nodeList

@val external toArray: t => array<Dom.node> = "Array.prototype.slice.call"

@bs.send.pipe(: t) external forEach: ((Dom.node, int) => unit) => unit = ""

@get external length: t => int = ""

@bs.send.pipe(: t) @return(nullable) external item: int => option<Dom.node> = ""
