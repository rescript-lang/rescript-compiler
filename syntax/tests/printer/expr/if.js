let name = if true {
  user.name
}

let name = if true {
  user.name
} else {
  "steve"
}

let name = if true {
  user.name
} else if false {
  user.lastName 
} else {
  defaultName
}

let () = if true {
  let a = 1
  let b = 2
  open Belt
  sideEffect()
  ()
} else {
  let a = 5
  let b = 6
  open React
  render()
  ()
}

let x = @attr if truth {
  sideEffect()
}

if inclusions[index] = (uid, url) {
  onChange(inclusions)
}
