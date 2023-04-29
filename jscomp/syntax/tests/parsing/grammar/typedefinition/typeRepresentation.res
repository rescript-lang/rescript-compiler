// extensible variants
type t = ..
type t = private ..

// constructor declaration
type t = Blue
type t = | Blue
type t = | Blue | Red
type t = Blue | Red
type t = | Blue | Red | Green
type t = Blue | Red | Green

// private constructor declaration
type t = private Blue
type t = private | Blue
type t = private | Blue | Red
type t = private Blue | Red
type t = private | Blue | Red | Green
type t = private Blue | Red | Green

// empty variant, not implemented
// type t = |
// type t = private |

// record declaration
type t = {x: int, y: int}
// private record declaration
type t = private {x: int, y: int}
