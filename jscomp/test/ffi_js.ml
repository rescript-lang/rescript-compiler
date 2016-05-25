let keys :  Obj.t -> string array [@uncurry] = [%bs.raw " function (x){return Object.keys(x)}" ]
