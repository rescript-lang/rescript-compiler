type t = Dom.namedNodeMap

@get external length: t => int = ""

@bs.send.pipe(: t) @return(nullable) external item: int => option<Dom.attr> = ""
@bs.send.pipe(: t) @return(nullable) external getNamedItem: string => option<Dom.attr> = ""
@bs.send.pipe(: t) @return(nullable)
external getNamedItemNS: (string, string) => option<Dom.attr> = ""
@bs.send.pipe(: t) external setNamedItem: Dom.attr => unit = ""
@bs.send.pipe(: t) external setNamedItemNS: Dom.attr => unit = ""
@bs.send.pipe(: t) external removeNamedItem: string => Dom.attr = ""
@bs.send.pipe(: t) external removeNamedItemNS: (string, string) => Dom.attr = ""

@val @scope(("Array", "prototype", "slice")) external toArray: t => array<Dom.attr> = "call"
