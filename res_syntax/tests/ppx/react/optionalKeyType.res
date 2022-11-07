let key = None

@@jsxConfig({version:3})

let _ = <C key="k" />
let _ = <C key=?Some("k") />
let _ = <C ?key />
let _ = <div key="k" />
let _ = <div key=?Some("k") />
let _ = <div ?key />
let _ = <div key="k"> <br /><br /> </div>
let _ = <div key=?Some("k")> <br /><br /> </div>
let _ = <div ?key> <br /><br /> </div>

@@jsxConfig({version:4, mode: "classic"})

let _ = <C key="k" />
let _ = <C key=?Some("k") />
let _ = <C ?key />
let _ = <div key="k" />
let _ = <div key=?Some("k") />
let _ = <div ?key />
let _ = <div key="k"> <br /><br /> </div>
let _ = <div key=?Some("k")> <br /><br /> </div>
let _ = <div ?key> <br /><br /> </div>

@@jsxConfig({version:4, mode: "automatic"})

let _ = <C key="k" />
let _ = <C key=?Some("k") />
let _ = <C ?key />
let _ = <div key="k" />
let _ = <div key=?Some("k") />
let _ = <div ?key />
let _ = <div key="k"> <br /><br /> </div>
let _ = <div key=?Some("k")> <br /><br /> </div>
let _ = <div ?key> <br /><br /> </div>