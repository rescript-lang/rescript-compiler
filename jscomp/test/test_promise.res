let p1 = Js.Promise2.make((~resolve, ~reject) => resolve(1))
let p2 = Js.Promise2.make((~resolve, ~reject) => resolve("foo"))

switch await Js.Promise2.allSettled2((p1, p2)) {
| (Fulfilled({value: v1}), Fulfilled({value: v2})) => Js.log2(v1, v2)
| (Rejected({reason}), Fulfilled(_)) => Js.log2("first rejected", reason)
| (Fulfilled(_), Rejected({reason})) => Js.log2("second rejected", reason)
| _ => Js.log("both rejected")
}
