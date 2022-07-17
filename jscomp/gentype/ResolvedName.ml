type t = string list

let dot s x = x @ [ s ]
let fromString x = [ x ]
let toList x = x
let toString x = x |> String.concat "_"

type eq = t * t

module NameSet = Set.Make (struct
  type nonrec t = t

  let rec compare (x : t) (y : t) =
    match (x, y) with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> -1
    | s1 :: rest1, s2 :: rest2 -> (
        let n = String.compare s1 s2 in
        match n <> 0 with true -> n | false -> compare rest1 rest2)
end)

let rec applyEquation ~(el : t) (eq : eq) : t list =
  match (eq, el) with
  | ([], rhs), _ -> [ rhs @ el ]
  | (s1 :: rest1, rhs), s2 :: rest2 -> (
      match s1 = s2 with
      | true -> (rest1, rhs) |> applyEquation ~el:rest2
      | false -> [])
  | (_ :: _, _), [] -> []

let rec applyEquationsToElements ~(eqs : eq list) ~seen (elements : t list) :
    eq list =
  let applyEqs el =
    let freshElements =
      eqs
      |> List.map (applyEquation ~el)
      |> List.concat
      |> List.filter (fun y -> not (NameSet.mem y seen))
    in
    freshElements |> List.map (fun elFresh -> (elFresh, el))
  in
  let newEquations = elements |> List.map applyEqs |> List.concat in
  let newElements = newEquations |> List.map fst in
  let newSeen = NameSet.union seen (newElements |> NameSet.of_list) in
  match newEquations = [] with
  | true -> newEquations
  | false ->
      newEquations @ (newElements |> applyEquationsToElements ~eqs ~seen:newSeen)

(* Apply equations of the form e.g. X.Y = A from the alias: module A = X.Y.
   Return a list of equations on types.
   E.g. if the element is X.Y.t, return equation A.t = X.Y.t *)

let applyEquations ~(eqs : eq list) (el : t) : eq list =
  [ el ] |> applyEquationsToElements ~eqs ~seen:NameSet.empty
