let for_ = x =>
  for i in 0 to {
    print_endline("hi")
    ignore(3)
    Array.length(x)
  } {
    print_endline(x[i])
  }

let for_2 = x =>
  for i in 0 to Array.length(x) {
    print_endline(x[i])
  }

let for_3 = x => {
  let v = ref(0)
  let arr = Array.map((_, _) => (), x)
  for i in 0 to Array.length(x) {
    let j = i * 2
    arr[i] = _ => v := v.contents + j
  }
  Array.iter(x => x(), arr)
  v.contents
}

let for_4 = x => {
  let v = ref(0)
  let arr = Array.map((_, _) => (), x)
  for i in 0 to Array.length(x) {
    let j = i * 2
    let k = 2 * j
    arr[i] = _ => v := v.contents + k
  }
  Array.iter(x => x(), arr)
  v.contents
}

let for_5 = (x, u) => {
  let v = ref(0)
  let arr = Array.map((_, _) => (), x)
  for i in 0 to Array.length(x) {
    let _j = i * 2
    let k = 2 * u * u
    arr[i] = _ => v := v.contents + k
  }
  Array.iter(x => x(), arr)
  v.contents
}

let for_6 = (x, u) => {
  let v = ref(0)
  let arr = Array.map((_, _) => (), x)
  let v4 = ref(0)
  let v5 = ref(0)
  incr(v4)
  for j in 0 to 1 {
    incr(v5)
    let v2 = ref(0)
    let v3 = u
    for i in 0 to Array.length(x) {
      let _j = i * 2
      let k = 2 * u * u
      let h = 2 * v5.contents
      incr(v2)
      arr[i] = _ => v := v.contents + k + v2.contents + v3 + v4.contents + v5.contents + h
    }
  }
  Array.iter(x => x(), arr)
  v.contents
}
