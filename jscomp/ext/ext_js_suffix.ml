type t = 
  | Js 
  | Bs_js   


let to_string (x : t) =   
  match x with 
  | Js -> Literals.suffix_js
  | Bs_js -> Literals.suffix_bs_js  

let to_bsc_flag (x : t) (buf : Ext_buffer.t) = 
    match x with 
    | Js -> ()
    | Bs_js -> Ext_buffer.add_string buf " -bs-suffix"