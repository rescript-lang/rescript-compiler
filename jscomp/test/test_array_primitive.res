@new("Array") external new_uninitialized_array: int => array<'a> = ""

let caml_array_sub = (x: array<'a>, offset: int, len: int) => {
  let result = new_uninitialized_array(len)
  for j in 0 to len - 1 {
    result[j] = x[offset + j]
  }
  result
}

/* let rec len l  = */
/* match l with */
/* | [] -> 0 */
/* | x::xs -> Array.length x + len xs */

/* let  caml_array_concat (l : 'a array list) : 'a array = */
/* let v = len l in */
/* let result = new_uninitialized_array v in */

let caml_array_set = (xs, index, newval) =>
  if index < 0 || index >= Array.length(xs) {
    invalid_arg("index out of bounds")
  } else {
    xs[index] = newval
  }

let caml_array_get = (xs, index) =>
  if index < 0 || index >= Array.length(xs) {
    invalid_arg("index out of bounds")
  } else {
    xs[index]
  }

let caml_make_vect = (len, init) => {
  let b = new_uninitialized_array(len)
  for i in 0 to len - 1 {
    b[i] = init
  }
  b
}
