[@@@config {
  flags = [|
    "-w";
       "@A";
    "-drawlambda";
    "-dtypedtree";
    "-bs-diagnose";
    "-dparsetree";
    (* "-dsource"; *)
  |]
}]


module N = struct 
  type 'a t = 'a option = 
    | None    
    | Some of 'a
end   


let u = N.(None, Some 3)


let h = N.None