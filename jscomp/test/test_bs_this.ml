let uux_this :[%bs.obj: < length : int > ] -> int -> int -> int [@bs.this] 
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
