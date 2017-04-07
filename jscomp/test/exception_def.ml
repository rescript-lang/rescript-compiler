


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

;; Printexc.register_printer (function 
    | A s -> Some "A"
    | _ -> None
)
