let t = 
  (function `A | `B -> () : 'a) (`A : [`A]);
  (failwith "dummy" : 'a) (* to know how 'a is unified *)
