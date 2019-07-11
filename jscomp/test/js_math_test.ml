open Js.Math

let suites =
  Mt.
    [ ("_E", fun _ -> ApproxThreshold (0.001, 2.718, _E))
    ; ("_LN2", fun _ -> ApproxThreshold (0.001, 0.693, _LN2))
    ; ("_LN10", fun _ -> ApproxThreshold (0.001, 2.303, _LN10))
    ; ("_LOG2E", fun _ -> ApproxThreshold (0.001, 1.443, _LOG2E))
    ; ("_LOG10E", fun _ -> ApproxThreshold (0.001, 0.434, _LOG10E))
    ; ("_PI", fun _ -> ApproxThreshold (0.00001, 3.14159, _PI))
    ; ("_SQRT1_2", fun _ -> ApproxThreshold (0.001, 0.707, _SQRT1_2))
    ; ("_SQRT2", fun _ -> ApproxThreshold (0.001, 1.414, _SQRT2))
    ; ("abs_int", fun _ -> Eq (4, abs_int (-4)))
    ; ("abs_float", fun _ -> Eq (1.2, abs_float (-1.2)))
    ; ("acos", fun _ -> ApproxThreshold (0.001, 1.159, acos 0.4))
    ; ("acosh", fun _ -> ApproxThreshold (0.001, 0.622, acosh 1.2))
    ; ("asin", fun _ -> ApproxThreshold (0.001, 0.411, asin 0.4))
    ; ("asinh", fun _ -> ApproxThreshold (0.001, 0.390, asinh 0.4))
    ; ("atan", fun _ -> ApproxThreshold (0.001, 0.380, atan 0.4))
    ; ("atanh", fun _ -> ApproxThreshold (0.001, 0.423, atanh 0.4))
    ; ("atan2", fun _ -> ApproxThreshold (0.001, 0.588, atan2 ~x:0.6 ~y:0.4 ()))
    ; ("cbrt", fun _ -> Eq (2., cbrt 8.))
    ; ("unsafe_ceil_int", fun _ -> Eq (4, unsafe_ceil_int 3.2))
    ; ("ceil_int", fun _ -> Eq (4, ceil_int 3.2))
    ; ("ceil_float", fun _ -> Eq (4., ceil_float 3.2))
    ; ("cos", fun _ -> ApproxThreshold (0.001, 0.921, cos 0.4))
    ; ("cosh", fun _ -> ApproxThreshold (0.001, 1.081, cosh 0.4))
    ; ("exp", fun _ -> ApproxThreshold (0.001, 1.491, exp 0.4))
    ; ("expm1", fun _ -> ApproxThreshold (0.001, 0.491, expm1 0.4))
    ; ("unsafe_floor_int", fun _ -> Eq (3, unsafe_floor_int 3.2))
    ; ("floor_int", fun _ -> Eq (3, floor_int 3.2))
    ; ("floor_float", fun _ -> Eq (3., floor_float 3.2))
    ; ("fround", fun _ -> Approx (3.2, fround 3.2))
    ; ("hypot", fun _ -> ApproxThreshold (0.001, 0.721, hypot 0.4 0.6))
    ; ( "hypotMany"
      , fun _ -> ApproxThreshold (0.001, 1.077, hypotMany [|0.4; 0.6; 0.8|]) )
    ; ("imul", fun _ -> Eq (8, imul 4 2))
    ; ("log", fun _ -> ApproxThreshold (0.001, -0.916, log 0.4))
    ; ("log1p", fun _ -> ApproxThreshold (0.001, 0.336, log1p 0.4))
    ; ("log10", fun _ -> ApproxThreshold (0.001, -0.397, log10 0.4))
    ; ("log2", fun _ -> ApproxThreshold (0.001, -1.321, log2 0.4))
    ; ("max_int", fun _ -> Eq (4, max_int 2 4))
    ; ("maxMany_int", fun _ -> Eq (4, maxMany_int [|2; 4; 3|]))
    ; ("max_float", fun _ -> Eq (4.2, max_float 2.7 4.2))
    ; ("maxMany_float", fun _ -> Eq (4.2, maxMany_float [|2.7; 4.2; 3.9|]))
    ; ("min_int", fun _ -> Eq (2, min_int 2 4))
    ; ("minMany_int", fun _ -> Eq (2, minMany_int [|2; 4; 3|]))
    ; ("min_float", fun _ -> Eq (2.7, min_float 2.7 4.2))
    ; ("minMany_float", fun _ -> Eq (2.7, minMany_float [|2.7; 4.2; 3.9|]))
    ; ( "random"
      , fun _ ->
          Ok
            (let a = random () in
             a >= 0. && a < 1.) )
    ; ( "random_int"
      , fun _ ->
          Ok
            (let a = random_int 1 3 in
             a >= 1 && a < 3) )
    ; ("unsafe_round", fun _ -> Eq (3, unsafe_round 3.2))
    ; ("round", fun _ -> Eq (3., round 3.2))
    ; ("sign_int", fun _ -> Eq (-1, sign_int (-4)))
    ; ("sign_float", fun _ -> Eq (-1., sign_float (-4.2)))
    ; ("sign_float -0", fun _ -> Eq (-0., sign_float (-0.)))
    ; ("sin", fun _ -> ApproxThreshold (0.001, 0.389, sin 0.4))
    ; ("sinh", fun _ -> ApproxThreshold (0.001, 0.410, sinh 0.4))
    ; ("sqrt", fun _ -> ApproxThreshold (0.001, 0.632, sqrt 0.4))
    ; ("tan", fun _ -> ApproxThreshold (0.001, 0.422, tan 0.4))
    ; ("tanh", fun _ -> ApproxThreshold (0.001, 0.379, tanh 0.4))
    ; ("unsafe_trunc", fun _ -> Eq (4, unsafe_trunc 4.2156))
    ; ("trunc", fun _ -> Eq (4., trunc 4.2156)) ]

;;
Mt.from_pair_suites __MODULE__ suites
