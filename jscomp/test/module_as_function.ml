external nightmare : [%bs.obj: < show : bool > ]  ->  int  = "" [@@bs.module]

let v = nightmare [%bs.obj {show = true}]
