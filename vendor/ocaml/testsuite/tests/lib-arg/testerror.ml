(** Test that the right message errors are emitted by Arg *)


let usage= "Arg module testing"

let test total i (spec,anon,argv) =
  let argv = Array.of_list ("testerror" :: argv) in
  try Arg.parse_argv ~current:(ref 0) argv spec anon usage with
  | Arg.Bad s-> Printf.printf "(%d/%d) Bad:\n%s\n" (i+1) total s
  | Arg.Help s -> Printf.printf "(%d/%d) Help:\n%s\n" (i+1) total s


let tests = [
(** missing argument error *)
  ["-s",  Arg.String ignore, "missing arg"], ignore, ["-s"]

(** No argument expected *)
; ["-set",  Arg.Set (ref false), "no argument expected"], ignore, ["-set=true"]

(** help message *)
; [], ignore, ["-help" ]

(** wrong argument type *)
; ["-int", Arg.Int ignore, "wrong argument type" ], ignore, ["-int"; "not_an_int" ]

(** unknown option *)
; [], ignore, [ "-an-unknown-option" ]

(** user-error in anon fun *)
; [], (fun _ -> raise @@ Arg.Bad("User-raised error")), [ "argument" ]

(** user-error in anon fun *)
; ["-error",
   Arg.Unit (fun () -> raise @@ Arg.Bad("User-raised error bis")),
   "user raised error"]
, ignore, [ "-error" ]
]

let () =
  let n = List.length tests in
  List.iteri (test n) tests
