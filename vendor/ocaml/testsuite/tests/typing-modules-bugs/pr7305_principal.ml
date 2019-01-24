type c1 = < c1: c1 >
type c2 = < c1: c1; c2: c1; c3: c1; c4: c1; c5: c1; c6: c1 >
type c3 = < c1: c2; c2: c2; c3: c2; c4: c2; c5: c2; c6: c2 >
type c4 = < c1: c3; c2: c3; c3: c3; c4: c3; c5: c3; c6: c3 >
type c5 = < c1: c4; c2: c4; c3: c4; c4: c4; c5: c4; c6: c4 >
type c6 = < c1: c5; c2: c5; c3: c5; c4: c5; c5: c5; c6: c5 >
type c7 = < c1: c6; c2: c6; c3: c6; c4: c6; c5: c6; c6: c6 >

(* If you use this example, then checking the types themselves
   takes a long time.
type c1 = < c1: c1; c2: c2; c3: c3; c4: c4; c5: c5; c6: c6 >
and  c2 = < c1: c1; c2: c2; c3: c3; c4: c4; c5: c5; c6: c6 >
and  c3 = < c1: c1; c2: c2; c3: c3; c4: c4; c5: c5; c6: c6 >
and  c4 = < c1: c1; c2: c2; c3: c3; c4: c4; c5: c5; c6: c6 >
and  c5 = < c1: c1; c2: c2; c3: c3; c4: c4; c5: c5; c6: c6 >
and  c6 = < c1: c1; c2: c2; c3: c3; c4: c4; c5: c5; c6: c6 >
*)

(* Same for this example
type 'a c1 = <c1: 'a c1>
type 'a c2 = <c1: 'a c1; c2: 'a c1; c3: 'a c1; c4: 'a c1; c5: 'a c1; c6: 'a c1>
type 'a c3 = <c1: 'a c2; c2: 'a c2; c3: 'a c2; c4: 'a c2; c5: 'a c2; c6: 'a c2>
type 'a c4 = <c1: 'a c3; c2: 'a c3; c3: 'a c3; c4: 'a c3; c5: 'a c3; c6: 'a c3>
type 'a c5 = <c1: 'a c4; c2: 'a c4; c3: 'a c4; c4: 'a c4; c5: 'a c4; c6: 'a c4>
type 'a c6 = <c1: 'a c5; c2: 'a c5; c3: 'a c5; c4: 'a c5; c5: 'a c5; c6: 'a c5>
type 'a c7 = <c1: 'a c6; c2: 'a c6; c3: 'a c6; c4: 'a c6; c5: 'a c6; c6: 'a c6>
*)

let x = ref ([] : c7 list)
