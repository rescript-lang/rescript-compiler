
let package_json = "package.json"

let rec find_package_json dir =
  if Node.Fs2.existsSync
      (Node.Path.join [|dir; package_json|])   then
    dir
  else
    (* symlink error ? *)
    let new_dir = (Node.Path.dirname dir) in
    if new_dir = dir then raise Not_found
    else find_package_json new_dir

let () =
  match  [%node __dirname] with
  | Some x ->
    Js.log (find_package_json x)
  | None -> ()
