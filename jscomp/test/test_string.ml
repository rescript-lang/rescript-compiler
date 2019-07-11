let f = function "aaaabb" -> 0 | "bbbb" -> 1 | _ -> (* 2 *) assert false
let a x = "hello" ^ "world" ^ "hello" ^ x
let b y x = y ^ "hello" ^ "world" ^ "hello" ^ x
let c x y = (x ^ "hello") ^ "hi" ^ "u" ^ "hi" ^ y
let v = String.length "xx"

let h (s : string) (b : bytes) =
  s.[0] = 'a' && Bytes.get b 0 = 'b' && s.[1] = Bytes.get b 2
