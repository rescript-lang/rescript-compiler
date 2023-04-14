let v = "ghos"

let u_a = String.get(v, 0)

let u_b = () => String.get(v, -1)
let u_c = String.get("ghos", 0)

let u_d = () => String.get("ghos", -1)

let u_e = Bytes.create(32)

let u_f = Bytes.get(u_e, 0)

let u_g = () => Bytes.get(u_e, -1)
