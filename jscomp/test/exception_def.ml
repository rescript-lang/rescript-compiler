[@@@bs.config{flags = [|"-drawlambda"|]}]


exception A of int 
module U = struct 
  exception A of int 
end

module H = Test_other_exn.Make()
type H.t += Bx


let a = 3

let u : exn = Bx


type Test_other_exn.V.t += Ax
type exn += XXX

exception Aa = Match_failure

let v = Aa ("",0,0)

exception H0 = Not_found 
exception H1
exception H2 
exception H3 = H2 

let h2 = H2 
let h3 = H3 
let h4 = H0 
exception H4 = Invalid_argument 

let h5 = H4 "xx"
;; Printexc.register_printer (function 
    | A s -> Some "A"
    | _ -> None
)
