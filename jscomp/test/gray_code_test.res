let gray_encode = b => lxor(b, lsr(b, 1))

let gray_decode = n => {
  let rec aux = (p, n) =>
    if n == 0 {
      p
    } else {
      aux(lxor(p, n), lsr(n, 1))
    }

  aux(n, lsr(n, 1))
}

let bool_string = (len, n) => {
  let s = Bytes.make(len, '0')
  let rec aux = (i, n) => {
    if land(n, 1) == 1 {
      Bytes.set(s, i, '1')
    }
    if i <= 0 {
      s
    } else {
      aux(pred(i), lsr(n, 1))
    }
  }

  aux(pred(len), n)
}

let next_power = v => {
  let v = v - 1
  let v = lor(lsr(v, 1), v)
  let v = lor(lsr(v, 2), v)
  let v = lor(lsr(v, 4), v)
  let v = lor(lsr(v, 8), v)
  let v = lor(lsr(v, 16), v)
  v + 1
}
