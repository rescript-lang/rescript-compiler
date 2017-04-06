


exception A of int 
module U = struct 
  exception A of int 
end
;; Printexc.register_printer (function 
    | A s -> Some "A"
    | _ -> None
)
