let v = "ghos"
let u_a = v.[0]
let u_b () = v.[-1]
let u_c = "ghos".[0]
let u_d () = "ghos".[-1]
let u_e = Bytes.create 32
let u_f = Bytes.get u_e 0
let u_g () = Bytes.get u_e (-1)
