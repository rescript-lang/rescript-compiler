
let vv = object (self)
  method hi x y  = x + y
  method hello z = self # hi 10 z
end

let v = object method x = 3 method y = 32 end


let u = object
  method hi v z = v + z
  method id1 = 3
  method id2 = 4
  method hello v = v
end



let uu = object method id = "uu" end
let uuu = object method add x y = x  + y end
let vvvv =
  object(self)
    method add x y = x + y
    method hi x = self#add x 32
  end

let suites = Mt.[
    "single_obj", (fun _ -> Eq([|3;32|],[|v#x; v#y|]) )  ;
    "single_obj_cache", (fun _ -> Eq([|3;32|],[|v#x; v#y|]) )  ;
    "self_obj", (fun _ -> Eq (13, vv # hello 3))    ;
    "uu_id"  , (fun _ -> Eq("uu", uu#id));
    "uu_add", (fun _ -> Eq ( uuu# add 1 20 , 21));
    "v_add"  , (fun _ -> Eq(vvvv#add 3 7, 10));
    "u_id1",  (fun _ -> Eq( u#id1 , 3));
    "u_id2",  (fun _ -> Eq(u#id2 , 4));         
    "u hi", (fun _ -> Eq( u#hi 1 2,  3));
    "u hello",  (fun _ -> Eq(u#hello 32 , 32));
    "v hi"    , (fun _ -> Eq(vvvv#hi 31, 63));
    "uuu add" , (fun _-> Eq(uuu#add 3 4,7)) ;
    "v x"    , (fun _ -> Eq(v#x, 3))
  ]

;; Mt.from_pair_suites __MODULE__ suites
