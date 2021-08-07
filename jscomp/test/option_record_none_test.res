

// type t0 = {
//   x : int ,
//   y : option<string>
// }

// let h = { x : 3 }
// module N = {
//   type c = option<string>
// }

// module Make = () => {
//   type t<'a> = {
//     x: int,
//     @as("Y") y: option<string>,
//     z: option<'a>,
//     h: N.c,
//   }
// }

// module N0 = Make()
// open N0

// let f = {
//   x: 3,
// }
