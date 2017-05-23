[@@@bs.config {no_export}]

open Xwatcher_util

(** start customized for bsb-watcher *)

let sourceDirs = Node.Path.join [|"lib"; "bs"; ".sourcedirs"|]

let lock = Xwatcher_util.makeLock ()
let events = Xwatcher_util.makeEventObj ()
let watchers : watcher array  = [||]
let source_dirname  = 
  Js.Option.getExn [%external __dirname]

let bsb_exe = "bsb.exe"

let bsb = Node.Path.join [|source_dirname; bsb_exe|]
let bsconfig = "bsconfig.json"

let rec onChange  = fun  eventType fileName -> 
  Js.log {j|Event $eventType $fileName|j} ; 
  events##push  (eventType, fileName);
  build bsb events lock (fun [@bs] () -> idle () )

and idle () = 
  let watchFiles = getWatchFiles sourceDirs in 
  watchers 
   |> Js.Vector.filterInPlace (fun [@bs] {dir ; watcher} -> 
      if dir = bsconfig || Js.Vector.memByRef dir watchFiles then true 
      else 
        begin 
          Js.log {j| $dir is no longer watched|j} ;
          Node.Fs.Watch.close  watcher   ;
          false 
        end
    ) ;
   watchFiles |> Js.Array.forEach (fun dir -> 
    if not (Js.Array.some (fun {dir = watcher_dir} -> watcher_dir = dir) watchers) then 
      begin 
        Js.log {j|watching dir $dir now |j};
        Js.Vector.pushBack (makeWatcher dir (fun [@bs] x y  -> onChange x y ))  watchers
      end
     )

let () = 
  Node.Process.putEnvVar "BS_VSCODE" "1";
  Js.Vector.pushBack (makeWatcher bsconfig (fun [@bs] x y -> onChange x y)) watchers; 
  build bsb events lock  (fun [@bs] () -> idle () )
(* local variables: *)
(* compile-command: "bscc -bs-package-output es6:jscomp/xwatcher -c xwatcher_util.mli xwatcher_util.ml xwatcher.ml && rollup --silent -f cjs xwatcher.js -o ../bin/bsb_watcher.future.js " *)
(* end: *)
