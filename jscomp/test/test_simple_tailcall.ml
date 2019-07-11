let rec length acc x =
  match x with [] -> acc | _ :: tl -> length (acc + 1) tl

let rec tailcall x = tailcall x
let rec non_length x = match x with [] -> 0 | _ :: tl -> 1 + non_length tl

let rec length acc x =
  match x with
  | [] -> acc
  | x :: y :: tl -> 1 + length (acc + 1) tl
  | _ :: tl -> length (acc + 1) tl
