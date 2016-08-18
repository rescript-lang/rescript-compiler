



type u =
  < height : int [@bs.set] > Js.t


class type v = object
  method height  : int 
end  [@bs]

module type  X = sig
  class xx : int -> object
      method height : int    
    end [@bs] (* should give warning *)
  (* class type xx : int -> object *)
  (*     method height : int     *)
  (*   end [@bs]   syntax error  *)
      
end


(* let u x = *)
(*   [%bs.obj{ *)
(*             x with y = 3              *)
(*           }] *)
