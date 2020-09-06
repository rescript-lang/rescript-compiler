let (//) = Filename.concat



let normalize_exn (s : string) : string =
  let old_cwd = Sys.getcwd () in
  Unix.chdir s ;
  let normalized = Sys.getcwd () in
  Unix.chdir old_cwd;
  normalized

let real_path p =
  match (try Some (Sys.is_directory p) with  _ -> None) with
  | None ->
    let rec resolve dir =
      if Sys.file_exists dir then normalize_exn dir else
      let parent = Filename.dirname dir in
      if dir = parent then dir
      else  (resolve parent) // (Filename.basename dir)
    in
    let p =
      if Filename.is_relative p then (Sys.getcwd ()) // p
      else p
    in
    resolve p
  | Some true -> normalize_exn p
  | Some false ->
    let dir = normalize_exn (Filename.dirname p) in
    match Filename.basename p with
    | "." -> dir
    | base -> dir // base


let is_same_paths_via_io a b =
  if a = b
  then true
  else (real_path a) = (real_path b)
