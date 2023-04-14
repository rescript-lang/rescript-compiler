let package_json = "package.json"

let rec find_package_json = dir =>
  if Node.Fs.existsSync(Node.Path.join([dir, package_json])) {
    dir
  } else {
    /* symlink error ? */
    let new_dir = Node.Path.dirname(dir)
    if new_dir == dir {
      raise(Not_found)
    } else {
      find_package_json(new_dir)
    }
  }

let () = switch %node(__dirname) {
| Some(x) => Js.log(find_package_json(x))
| None => ()
}
