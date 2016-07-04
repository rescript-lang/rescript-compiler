let keys :  Obj.t -> string array [@bs] = [%bs.raw " function (x){return Object.keys(x)}" ]
