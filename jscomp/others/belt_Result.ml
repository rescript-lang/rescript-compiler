(* https://gist.github.com/NicolasT/65dad40b203da7c65b4c *)

type ('a, 'b) t =
  | Ok of 'a
  | Error of 'b
                                      
(** Constructor functions *)
let ok a = Ok a
let error b = error b

let result a b = function
  | Ok v -> a v
  | Error v -> b v

(** Bifunctor interface *)
let bimap a b = result (fun v -> ok (a v)) (fun v -> error (b v))

external id : 'a -> 'a = "%identity"
let const v = fun _ -> v

(** Functor interface *)
let map f = bimap f id
let (<$>) = map
let mapError f = bimap id f

(** Predicates *)
let isOk v = result (const true) (const false) v
let isError v = result (const false) (const true) v

let toString a b = result
                     (fun v -> "Ok (" ^ (a v) ^ ")")
                     (fun v -> "Error (" ^ (b v) ^ ")")

let fold f z = result (const z) (fun v -> f v z)

let oks xs =
  List.fold_left(fun acc x ->
      match x with
      | Ok a -> List.append acc [a]
      | Error _ -> acc
    ) [] xs

let errors xs =
  List.fold_left(fun acc x ->
      match x with
      | Ok _ -> acc
      | Error b -> List.append acc [b]
    ) [] xs

let arrayLefts xs =
  Array.fold_left(fun acc x ->
      match x with
      | Ok a -> Array.append acc [|a|]
      | Error _ -> acc
    ) [||] xs

let arrayRights xs =
  Array.fold_left(fun acc x ->
      match x with
      | Ok _ -> acc
      | Error b -> Array.append acc [|b|]
    ) [||] xs

