// @@config({flags: ["-bs-diagnose"] })

type t0 = {x: int, @optional y: int, @optional yy: option<int>, z: int}

let f1 = {x: 3, z: 2}

let f2 = {x: 3, z: 3, y: 3}

let f3 = {...f1, y: 3}

let f4 = {...f3, yy: None}

let theseTwoShouldBeIdentical = [f4.yy, (Some(None): option<option<int>>)]

type r = {
  x: int,
  y: option<int>,
  z: int,
}

// let v0 = { x :  3 , z : 2 }

// let v2 = { ... v0 , x : 3 }

let v1: t0 = {
  x: 3,
  z: 3,
}

let v2: r = {x: 3, y: None, z: 2}

type config = {
  x: int,
  @optional y0: int,
  @optional y1: int,
  @optional y2: int,
  @optional y3: int,
  @optional y4: int,
  @optional y5: int,
  @optional y6: int,
  @optional y7: int,
  @optional y8: int,
  @optional y9: int,
  @optional y10: int,
  @optional y11: int,
  @optional y12: int,
  @optional y13: int,
  @optional y14: int,
  @optional y15: int,
  @optional y16: int,
  @optional y17: int,
  @optional y18: int,
  @optional y19: int,
  @optional y20: int,
  @optional y21: int,
  @optional y22: int,
  @optional y23: int,
  z: int,
}

let v: config = {x: 2, z: 3}

let h: config = {...v, y1: 22}

type small_config = {
  x: int,
  @optional y0: int,
  @optional y1: int,
  z: int,
}

let v1: small_config = {x: 2, z: 3}

let h10: small_config = {...v1, y1: 22}

let h11 = (v1): small_config => {
  {...v1, y1: 22}
}

type partiallyOptional = {
  @optional aa: int,
  bb: option<int>,
}

let po = {aa: 3, bb: Some(4)}

// Trigger representation mismatch error.
// module M: {
//   type partiallyOptional = {
//     @optional aa: int,
//     bb: option<int>,
//   }
// } = {
//   type partiallyOptional = {
//     @optional aa: int,
//     @optional bb: int,
//   }
// }
