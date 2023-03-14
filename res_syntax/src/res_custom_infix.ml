type t = (string * string) list

let addSymbol ~name ~alias x =
  (* Put "++" before "+" *)
  let cmp (n1, _) (n2, _) = String.length n2 - String.length n1 in
  List.stable_sort cmp ((name, alias) :: x)

let findAlias ~alias x = x |> List.find_opt (fun (_, a) -> alias = a)

let lookupName ~name ~src ~srcLen ~offset =
  let nameLen = String.length name in
  let restLen = srcLen - offset in
  if nameLen <= restLen then (
    let found = ref true in
    for i = 0 to nameLen - 1 do
      if (name.[i] [@doesNotRaise]) <> (src.[offset + i] [@doesNotRaise]) then
        found := false
    done;
    !found)
  else false
