let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc (x, y) =
  incr test_id ;
  suites :=
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


external readFileSync : string -> ([`utf8 | `ascii] [@bs.string]) -> string
  = "" [@@bs.module "fs"]


type watcher

external watch : unit -> watcher = "" [@@bs.module "fs"]

type error

external on :
  watcher ->

  (
    [
      | `change of (string (* event*) -> string (*filename*) -> unit )
      | `error of (error -> unit [@bs]) ]
      [@bs.string]
  ) -> unit = "on" [@@bs.send]

open Node
let () =
  let current_file : string =
    match [%node __filename] with
    | Some x -> x
    | None -> "<Not Node JS>"  in
  let current_dir_name : string =
    match  [%node __dirname] with
    | Some x -> x
    | None -> "<Not Node Js>"  in
  let _content = readFileSync current_file `utf8 in
  let _file_list = Fs.readdirSync current_dir_name in
  let pathobj =   Path.parse current_dir_name in
  match  [%node _module] with
  | Some module_ ->
    Js.log (module_##id, module_##paths) ;
    eq __LOC__ (pathobj##name, "test" )
  | None -> ()



let () =
  (* Js.log ("ARGV", Node.Process.process##argv);   *)
  Mt.from_pair_suites __MODULE__ !suites

