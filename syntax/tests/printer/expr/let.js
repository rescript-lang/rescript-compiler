let z = {
  let a = 1
  a
}

let x = {
  let x = 1 and y = 2 and z = 3

  let x = 1
  and y = 2
  and z = 3


  let x = 1

  and y = 2

  and z = 3

  x + y + z
}

let x = {let a = true; let b = false; a || b}

// don't add whitespace here
let highlight_dumb = (ppf, lb, loc) => {
  let line_start = ref(0)
  and line_end = ref(0)
  foo
}

// should contain a newline before foo
let highlight_dumb = (ppf, lb, loc) => {
  let line_start = ref(0)
  and line_end = ref(0)

  foo
}
