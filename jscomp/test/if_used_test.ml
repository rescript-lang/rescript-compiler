class point x_init =
  object
    val mutable x = x_init

    method get_x = x

    method move d = x <- x + d
  end

let p = new point 7

(** test>ocamlc.opt -c -drawlambda if_used_test.ml (setglobal If_used_test!
    (let (shared/1044 =a [0: #"move" #"get_x"] shared/1043 =a [0: #"x"]
    point/1011 =r (let (point_tables/1032 =o (makemutable 0 0a 0a 0a)
    point_init/1046 = (function class/1036 (let (x_init/1013 =o (apply (field 2
    (global CamlinternalOO!)) class/1036 #"") ids/1042 = (apply (field 3
    (global CamlinternalOO!)) class/1036 shared/1044 shared/1043) move/1018 =o
    (field 0 ids/1042) get_x/1017 =o (field 1 ids/1042) x/1016 =o (field 2
    ids/1042)) (seq (apply (field 10 (global CamlinternalOO!)) class/1036
    (makeblock 0 get_x/1017 1a x/1016 move/1018 (function self-1/1022 d/1023
    (let (self-*/1021 =a self-1/1022) (array.unsafe_set self-1/1022 x/1016 (+
    (array.unsafe_get self-1/1022 x/1016) d/1023)))))) (function env/1038
    self/1037 x_init/1012 (let (self/1039 = (apply (field 23 (global
    CamlinternalOO!)) self/1037 class/1036)) (seq (seq (ifused x_init/1013
    (array.unsafe_set self/1039 x_init/1013 x_init/1012)) (array.unsafe_set
    self/1039 x/1016 x_init/1012)) self/1039))))))) (apply (field 18 (global
    CamlinternalOO!)) shared/1044 point_init/1046)) p/1024 = (apply (field 0
    point/1011) 0a 7)) (makeblock 0 point/1011 p/1024))) *)
