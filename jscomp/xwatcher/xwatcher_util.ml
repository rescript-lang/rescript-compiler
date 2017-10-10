open Node

let getWatchFiles file = 
  if Fs.existsSync file then 
    Js.String.split  "\n" (Fs.readFileAsUtf8Sync file )
    |> Js.Array.filter (fun x -> String.length (Js.String.trim x) <> 0)
  else [||]



class type _eventObj = object 
  method empty : unit -> unit 
  method needRebuild : unit -> bool
  method push : string * string -> unit
  method currentEvents : unit -> (string * string) array
end [@bs]

type eventObj = _eventObj  Js.t

let rec findFile ~prev ~cwd f = 
  if String.length prev = String.length cwd then 
    Js.Exn.raiseError {j| $f not found |j}
  else if Fs.existsSync (Node.Path.join [|cwd; f|]) then cwd 
  else 
    findFile cwd (Node.Path.dirname cwd ) f 

let makeEventObj () : eventObj = 
  object (self)
    val events : (string * string) array = [||]
    method empty () = Js.Vector.empty self##events
    method push a = Js.Vector.pushBack a self##events
    method needRebuild () = Array.length self##events <> 0
    method currentEvents () = self##events
  end [@bs]

(* let basic = makeEventObj () *)

type t 

external spawnInheritNoShell : string -> 
  (_ [@bs.as {json| [ ]|json}]) -> 
  (_ [@bs.as {json| { "stdio" : "inherit" }|json}]) -> t = "spawn" [@@bs.module "child_process"]

external spawnInherit : string -> 
  (_ [@bs.as {json| [ ]|json}]) -> 
  (_ [@bs.as {json| { "stdio" : "inherit", "shell" : true }|json}]) -> t = "spawn" [@@bs.module "child_process"]

external onExit : (_ [@bs.as "exit"]) -> (unit -> unit [@bs.uncurry]) -> unit = "on" [@@bs.send.pipe: t]


external spawnInheritIgnore : string -> 
  (_ [@bs.as {json| [ ]|json}]) -> 
  (_ [@bs.as {json| { "stdio" : "inherit", "shell" : true }|json}]) -> unit = "spawn" [@@bs.module "child_process"]

(* let (acquireBuild, releaseBuild) =  *)
(*   let isBuilding = ref false in  *)
(*   (fun [@bs]() ->  if !isBuilding then false else begin isBuilding := true ; true end), *)
(*   (fun [@bs]() ->   isBuilding := false ) *)

class type _lock  = object
  method acquire : unit -> bool
  method release : unit -> unit 
end [@bs]
type lock =  _lock Js.t 

let makeLock () : lock = 
  object(self)
    val mutable isBuilding = false 
    method acquire () = if self##isBuilding then false else begin self##isBuilding #= true; true end
    method release () =  self##isBuilding #= false 
  end [@bs]

(* let needRebuild events = Array.length events <> 0 *)

let rec build cmd (event : eventObj) (lock : lock ) idle = 
  if lock##acquire ()  then 
    begin 
      Js.log ">>>> Start compiling";
      let events = event##currentEvents () in 
      Js.log {j|Rebuilding since $events |j};
      event##empty ();
      spawnInheritNoShell cmd
      |> onExit (fun () -> 
          Js.log ">>>> Finish compiling";
          lock##release () ;
          if event##needRebuild () then 
            build cmd event lock idle
          else idle () [@bs]
        )
    end

let rec buildWithShell cmd (event : eventObj) (lock : lock ) idle = 
  if lock##acquire ()  then 
    begin 
      Js.log ">>>> Start compiling";
      let events = event##currentEvents () in 
      Js.log {j|Rebuilding since $events |j};
      event##empty ();
      spawnInherit cmd
      |> onExit (fun () -> 
          Js.log ">>>> Finish compiling";
          lock##release () ;
          if event##needRebuild () then 
            build cmd event lock idle
          else idle () [@bs]
        )
    end

type watcher = { dir : string ; watcher : Fs.Watch.t }

external watchOnChange : 
  string -> 
  (string (*eventType*) -> string (* filename *) -> unit  [@bs])
  -> Node.Fs.Watch.t = "watch" [@@bs.module "fs"]
(**default is string Js.undefined , with buffer encoding it would be buffer  *)
let makeWatcher file onChange = 
  { watcher = watchOnChange file onChange;
    dir = file 
  }

(* local variables: *)
(* compile-command: "bscc -bs-package-output es6:jscomp/xwatcher -c xwatcher_util.mli xwatcher_util.ml" *)
(* end: *)
