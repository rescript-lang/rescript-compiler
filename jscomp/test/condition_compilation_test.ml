let b = 
#if BUFFER_SIZE 
    && 
    BUFFER_SIZE > 20 
      "cool"
#else
      "u"
#end

let buffer_size =
#if true || BUFFER_SIZE 
      1
#else
      32
#end



type open_flag = 
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
#if OCAML_VERSION =~ ">=3.13"
  | O_SHARE_DELETE
#end
#if OCAML_VERSION =~ ">=4.01"
  | O_CLOEXEC
#end
#if OCAML_VERSION =~ ">=4.03"
  | O_KEEPEXEC
#end

let vv =
#if OCAML_PATCH = "BS"
      3
#else
      1
#end


let v = ref 1

let a =
#if OCAML_PATCH = "BS"
let () = incr v  in
#end !v

let version_gt_3 =
#if OCAML_VERSION  (* comment *) =~ ">1" 
      true
#else
      false
#end

let version =
#if OCAML_VERSION =~ "~2" 
      2
#elif OCAML_VERSION =~ "~1"
      1
#elif OCAML_VERSION =~ "~0" 
      0
#else
                       -1
#end

let ocaml_veriosn =
#if OCAML_VERSION =~ "~4.02.0" 
      "4.02.3"
#else "unknown"
#end

(**
   #if OCAML_VERSION =~ "4.02.3" #then

   #elif OCAML_VERSION =~ "4.03" #then
   gsho
   #end
*)
(* #if OCAML_VERSION =~ ">4.02" #then *)
(* #else *)
(* #end *)
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let () =
  eq __LOC__ vv 3  ;
  eq __LOC__ !v 2


;; Mt.from_pair_suites __MODULE__ !suites
