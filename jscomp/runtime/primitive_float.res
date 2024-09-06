let compare = (x: float, y: float): int =>
  if x == y {
    0
  } else if x < y {
    -1
  } else if x > y {
    1
  } else if x == x {
    1
  } else if y == y {
    -1
  } else {
    0
  }

let min = (x: float, y: float): float =>
  if x < y {
    x
  } else {
    y
  }

let max = (x: float, y: float): float =>
  if x > y {
    x
  } else {
    y
  }

// TODO: delete below

/* borrowed from others/js_math.ml */
@val external _LOG2E: float = "Math.LOG2E"
@val external _LOG10E: float = "Math.LOG10E"
@val external abs_float: float => float = "Math.abs"
@val external floor: float => float = "Math.floor"
@val @scope("Math") external exp: float => float = "exp"
@val external log: float => float = "Math.log"
@val @scope("Math") external sqrt: float => float = "sqrt"
@val external pow_float: (~base: float, ~exp: float) => float = "Math.pow"
external int_of_float: float => int = "%intoffloat"
external float_of_int: int => float = "%floatofint"

let int_float_of_bits: int => float = %raw(`function(x){
    return new Float32Array(new Int32Array([x]).buffer)[0] 
    }`)
let int_bits_of_float: float => int = %raw(`function(x){
  return new Int32Array(new Float32Array([x]).buffer)[0] 
}`)

let modf_float = (x: float): (float, float) =>
  if Primitive_float_extern.isFinite(x) {
    let neg = 1. /. x < 0.
    let x = abs_float(x)
    let i = floor(x)
    let f = x -. i
    if neg {
      (-.f, -.i)
    } else {
      (f, i)
    }
  } else if Primitive_float_extern.isNaN(x) {
    (Primitive_float_extern._NaN, Primitive_float_extern._NaN)
  } else {
    (1. /. x, x)
  }

let ldexp_float = (x: float, exp: int): float => {
  let (x', exp') = (ref(x), ref(float_of_int(exp)))
  if exp'.contents > 1023. {
    exp'.contents = exp'.contents -. 1023.
    x'.contents = x'.contents *. pow_float(~base=2., ~exp=1023.)
    if exp'.contents > 1023. {
      /* in case x is subnormal */
      exp'.contents = exp'.contents -. 1023.
      x'.contents = x'.contents *. pow_float(~base=2., ~exp=1023.)
    }
  } else if exp'.contents < -1023. {
    exp'.contents = exp'.contents +. 1023.
    x'.contents = x'.contents *. pow_float(~base=2., ~exp=-1023.)
  }
  x'.contents *. pow_float(~base=2., ~exp=exp'.contents)
}

let frexp_float = (x: float): (float, int) =>
  if x == 0. || !Primitive_float_extern.isFinite(x) {
    (x, 0)
  } else {
    let neg = x < 0.
    let x' = ref(abs_float(x))
    let exp = ref(floor(_LOG2E *. log(x'.contents)) +. 1.)

    x'.contents = x'.contents *. pow_float(~base=2., ~exp=-.exp.contents)
    if x'.contents < 0.5 {
      x'.contents = x'.contents *. 2.
      exp.contents = exp.contents -. 1.
    }
    if neg {
      x'.contents = -.x'.contents
    }
    (x'.contents, int_of_float(exp.contents))
  }

let copysign_float = (x: float, y: float): float => {
  let x = abs_float(x)
  let y = if y == 0. {
    1. /. y
  } else {
    y
  }
  if y < 0. {
    -.x
  } else {
    x
  }
}

/* http://www.johndcook.com/blog/cpp_expm1/ */
let expm1_float: float => float = x =>
  switch x {
  | x =>
    let y = exp(x)
    let z = y -. 1.
    if abs_float(x) > 1. {
      z
    } else if z == 0. {
      x
    } else {
      x *. z /. log(y)
    }
  }

let hypot_float = (x: float, y: float): float => {
  let (x0, y0) = (abs_float(x), abs_float(y))
  let a = max(x0, y0)
  let b =
    min(x0, y0) /. if a != 0. {
      a
    } else {
      1.
    }
  a *. sqrt(1. +. b *. b)
}
