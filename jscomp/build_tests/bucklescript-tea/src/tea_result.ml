

(* TODO:  Remove this when Bucklescript is updated to OCaml 4.03 as it includes result *)
type ('a, 'b) t (* result *) =
  | Ok of 'a
  | Error of 'b

let result_to_option = function
  | Ok a -> Some a
  | Error _ -> None

let option_of_result = function
  | Ok a -> Some a
  | Error _ -> None

let ok = function
  | Ok a -> Some a
  | Error _ -> None

let error = function
  | Ok _ -> None
  | Error e -> Some e

let rec last_of = function
  | [] -> failwith "`Tea.Result.do` must never be passed the empty list"
  | [last] -> last
  | next :: tl ->
    match next with
    | Error _ as e -> e
    | Ok _ -> last_of tl

let rec accumulate = function
  | [] -> Ok []
  | [last] ->
    begin match last with
    | Error _ as e -> e
    | Ok o -> Ok [o]
    end
  | next :: tl ->
    match next with
    | Error _ as e -> e
    | Ok o ->
      match accumulate tl with
      | Error _ as e -> e
      | Ok os -> Ok (o :: os)

let first fst = function
  | Error _ as e -> e
  | Ok _ -> fst

let rec error_of_any = function
  | [] -> None
  | hd :: tl ->
    match hd with
    | Error e -> Some e
    | Ok _ -> error_of_any tl

let rec error_of_first fst = function
  | Error e -> Some e
  | Ok _ -> error fst
