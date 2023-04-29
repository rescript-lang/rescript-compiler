type t = Dom.history
type state /* TODO: should be "anything that can be serializable" apparently */

@get external length: t => int = ""
@get external scrollRestoration: t => bool = "" /* experimental */
@set external setScrollRestoration: (t, bool) => unit = "scrollRestoration" /* experimental */
@get external state: t => state = ""

@bs.send.pipe(: t) external back: unit = ""
@bs.send.pipe(: t) external forward: unit = ""
@bs.send.pipe(: t) external go: int => unit = ""
@bs.send.pipe(: t) external pushState: (state, string, string) => unit = ""
@bs.send.pipe(: t) external replaceState: (state, string, string) => unit = ""
