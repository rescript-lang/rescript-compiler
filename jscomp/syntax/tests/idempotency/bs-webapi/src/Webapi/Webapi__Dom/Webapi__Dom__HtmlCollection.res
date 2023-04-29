type t = Dom.htmlCollection

@val @scope(("Array", "prototype", "slice")) external toArray: t => array<Dom.element> = "call"

@get external length: t => int = ""
@bs.send.pipe(: t) @return(nullable) external item: int => option<Dom.element> = ""
@bs.send.pipe(: t) @return(nullable) external namedItem: string => option<Dom.element> = ""
