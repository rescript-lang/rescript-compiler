[@@@warning "-a"]

let f x = match x with 1 | 2 | 3 -> 'a'

type t = A of int | B of int | C of int | D of int

let f2 x = match x with Some _ -> 0 | None -> 1

(** if there is [default] in [Lswitch] it means its patten match are complete,
    in that case we can transform
    {[
        switch(x[0]){
                  case 0:
                  case 2:return x[1]+1;
                  case 1:
                  case 3:return x[1]+2;
                  
                }

    ]}

    into

    {[
    var v = x[0] 
    if (v ===0 || v ===2)
    { return v[1] + 1 }
    else {return v[1] + 2 }
    ]} *)
let f3 x =
  match x with A v -> v + 1 | B v -> v + 2 | C v -> v + 1 | D v -> v + 2
