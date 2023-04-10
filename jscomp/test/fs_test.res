let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, (x, y)) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

@module("fs") external readFileSync: (string, [#utf8 | #ascii]) => string = "readFileSync"

type watcher

@module("fs") external watch: unit => watcher = "watch"

type error

@send
external on: (
  watcher,
  @string
  [
    | #change((string, string) => unit)
    | #error((. error) => unit)
  ],
) => unit = "on"

open Node
let () = {
  let current_file: string = switch %node(__filename) {
  | Some(x) => x
  | None => "<Not Node JS>"
  }
  let current_dir_name: string = switch %node(__dirname) {
  | Some(x) => x
  | None => "<Not Node Js>"
  }
  let _content = readFileSync(current_file, #utf8)
  let _file_list = Fs.readdirSync(current_dir_name)
  let pathobj = Path.parse(current_dir_name)
  switch %node(_module) {
  | Some(module_) =>
    Js.log((module_["id"], module_["paths"]))
    eq(__LOC__, (pathobj["name"], "test"))
  | None => ()
  }
}

let () = /* Js.log ("ARGV", Node.Process.process##argv); */
Mt.from_pair_suites(__MODULE__, suites.contents)
