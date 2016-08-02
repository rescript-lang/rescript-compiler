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
  ) -> unit = "" [@@bs.send]

open Bs_node 
let () =
  let current_file : string = [%bs.node __filename] in
  let current_dir_name : string = [%bs.node __dirname] in
  let _content = readFileSync current_file `utf8 in
  let _file_list = Fs.readdirSync current_dir_name in
  let pathobj =   Path.parse current_dir_name in
  let module_  = [%bs.node __module] in
  Js.log (module_##id, module_##paths) ;   
  eq __LOC__ (pathobj##name, "test" )


  
let () = Mt.from_pair_suites __FILE__ !suites
