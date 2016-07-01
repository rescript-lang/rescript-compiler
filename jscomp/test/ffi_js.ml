let keys :  Obj.t -> string array [@fn] = [%bs.raw " function (x){return Object.keys(x)}" ]
