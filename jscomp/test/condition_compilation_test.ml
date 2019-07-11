let b = "u"
let buffer_size = 1

type open_flag = Unix.open_flag =
  | O_RDONLY
  | O_WRONLY
  | O_RDWR
  | O_NONBLOCK
  | O_APPEND
  | O_CREAT
  | O_TRUNC
  | O_EXCL
  | O_NOCTTY
  | O_DSYNC
  | O_SYNC
  | O_RSYNC
  | O_SHARE_DELETE
  | O_CLOEXEC
  | O_KEEPEXEC

let vv = 3
let v = ref 1

let a =
  let () = incr v in
  !v

let version_gt_3 = true
let version = -1
let ocaml_veriosn = "unknown"

(* #if OCAML_VERSION =~ ">4.02" #then *)
(* #else *)
(* #end *)

(** #if OCAML_VERSION =~ "4.02.3" #then

    #elif OCAML_VERSION =~ "4.03" #then gsho #end *)
let suites : Mt.pair_suites ref = ref []

let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let () = eq __LOC__ vv 3 ; eq __LOC__ !v 2

;;
Mt.from_pair_suites __MODULE__ !suites
