let generic_basename is_dir_sep current_dir_name name =
  let rec find_end n =
    if n < 0 then String.sub name 0 1
    else if is_dir_sep name n then find_end (n - 1)
    else find_beg n (n + 1)
  and find_beg n p =
    if n < 0 then String.sub name 0 p
    else if is_dir_sep name n then String.sub name (n + 1) (p - n - 1)
    else find_beg (n - 1) p in
  if name = "" then current_dir_name else find_end (String.length name - 1)

let basename =
  generic_basename (fun s i -> s.[i] = '/') Filename.current_dir_name

let suites = Mt.[("basename", fun _ -> Eq (basename "b/c/a.b", "a.b"))]

;;
Mt.from_pair_suites __MODULE__ suites
