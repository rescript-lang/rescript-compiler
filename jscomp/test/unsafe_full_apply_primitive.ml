

external unsafeInvariantApply : 'a -> 'a = "#full_apply"


let f1 x = unsafeInvariantApply (x ())


let f2 x y = unsafeInvariantApply (x y ())