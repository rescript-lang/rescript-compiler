

let uux_this : < length : int >  -> int -> int -> int [@bs.this] 
  =
  fun[@bs.this] o x y -> o##length + x + y

let  even = fun [@bs.this] o x ->  x + o

let bark () = 
  fun [@bs.this] (o : 'self) x y -> 
    begin 
      Js.log (o##length, o##x, o##y,x,y);
      x + y
    end

let js_obj : 'self = 
  [%bs.obj 
      {
        bark = 
          (fun [@bs.this] (o : 'self) x y -> 
            Js.log o;
            x + y
          );

      }
  ]
class type x = object 
  method onload : x  -> unit [@this] [@@bs.set]
  method addEventListener : string -> (x  -> unit [@bs.this]) -> unit 
  method response : string
end


let f (x : x ) = 
  begin 
    x##onload #=  (fun [@bs.this] o -> Js.log o);
    x##addEventListener "onload" begin fun [@bs.this] o -> 
      Js.log o##response
    end
  end

let u = fun [@this] (_ : int) (x : int) -> x 
