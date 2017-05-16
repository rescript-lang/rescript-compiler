[@@@bs.config {no_export}]

open Xwatcher_util


let source_dirname  = 
  Bs.Option.getExn [%external __dirname]


let cwd = Node.Process.cwd ()
let bsconfig = "bsconfig.json"

let root = 
  if Node.Fs.existsSync (Node.Path.join [|cwd; bsconfig |]) then  
    cwd 
  else 
    Xwatcher_util.findFile ~prev:cwd  ~cwd:(Node.Path.dirname cwd) bsconfig 

let jscomp = 
  Js.log {j| Root: $root |j}; 
  Node.Path.join [|root ; "jscomp" |]

let lock = Xwatcher_util.makeLock ()
let events = Xwatcher_util.makeEventObj ()
let command = "./watch-build.sh"
let exec () = 
  buildWithShell command events lock (fun [@bs] () -> ())
let watch dir   = 
  makeWatcher dir 
    (fun [@bs] event fileName -> 
       (* Js.log {j| $event :  $fileName |j}; *)
       if Js.String.endsWith ".ml" fileName 
       || Js.String.endsWith ".mli" fileName
       || Js.String.endsWith ".cppo" fileName then 
         exec ()
    )
let () = 

  Node.Process.putEnvVar "BS_VSCODE" "1";
  Bs.Vector.iter (fun [@bs] x -> 
      ignore @@ watch (Node.Path.join [|jscomp; x|])
    ) 
    [| "core"; "syntax"; "ext"; "depends"; 
       "others";
       "ounit"; "ounit_tests"; "test"; 
       "runtime";
       "xwatcher";
       "bsb";
       "common"
       |];

  exec ()


