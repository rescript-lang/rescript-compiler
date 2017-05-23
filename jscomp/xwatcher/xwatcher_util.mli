
type lock 

val makeLock : unit -> lock

class type _eventObj = object 
  method empty : unit -> unit 
  method needRebuild : unit -> bool
  method push : string * string -> unit
  method currentEvents : unit -> (string * string) array
end [@bs]

type eventObj = _eventObj  Js.t

val makeEventObj : unit -> eventObj

val build : string -> eventObj -> lock -> (unit -> unit [@bs]) -> unit

val buildWithShell : string -> eventObj -> lock -> (unit -> unit [@bs]) -> unit

val findFile : prev:string -> cwd:string -> string -> string

val getWatchFiles : string -> Js.String.t Js.Array.t
type watcher = { dir : string ; watcher : Node.Fs.Watch.t }
val makeWatcher : string -> (string -> string -> unit [@bs]) -> watcher

external spawnInheritIgnore : string -> 
  (_ [@bs.as {json| [ ]|json}]) -> 
  (_ [@bs.as {json| { "stdio" : "inherit", "shell" : true }|json}]) -> unit = "spawn" [@@bs.module "child_process"]
