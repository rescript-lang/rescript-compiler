type x = [#One | #Two]

@tag("kind")
type y = | @as("one") One({hello: [#hello]}) | @as(null) Two

let x: x = #One

let xx = #One({"hello": "hi"})

let y: y = One({hello: #hello})

let z = (x :> y)
