@@jsxConfig({version:4, mode:"classic"})

let _ = <div />
let _ = <div key="k" />
let _ = <div x />
let _ = <div><p>{React.string(x)}</p></div>
let _ = <div {...str} />
let _ = <div {...str} x="x" />

// syntax error
// let _ = <div x="x" {...str} />
// let _ = <div {...str} {...str} />