


class type y = object 
  method height : int [@@bs.set {no_get}]
end 

class type y0 = object 
  method height : int [@@bs.set] [@@bs.get {null}]
end 


class type y1 = object 
  method height : int [@@bs.set] [@@bs.get {undefined}]
end


class type y2 = object 
  method height : int [@@bs.set] [@@bs.get {undefined; null}]
end 


class type y3 = object 
  method height : int  [@@bs.get {undefined ; null}]
end



type yy2 = < height : int [@bs.get{undefined ; null}] [@bs.set] > 


let fff (x : yy2) = 
   x##height #= 2 


let ff (x : y2) (z : yy2) = 
   [ x ##height ; 
    z##height
   ]
