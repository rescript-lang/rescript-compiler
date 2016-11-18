module OUnitTypes
= struct
#1 "oUnitTypes.ml"

(**
  * Commont types for OUnit
  *
  * @author Sylvain Le Gall
  *
  *)

(** See OUnit.mli. *) 
type node = ListItem of int | Label of string

(** See OUnit.mli. *) 
type path = node list 

(** See OUnit.mli. *) 
type log_severity = 
  | LError
  | LWarning
  | LInfo

(** See OUnit.mli. *) 
type test_result =
  | RSuccess of path
  | RFailure of path * string
  | RError of path * string
  | RSkip of path * string
  | RTodo of path * string

(** See OUnit.mli. *) 
type test_event =
  | EStart of path
  | EEnd of path
  | EResult of test_result
  | ELog of log_severity * string
  | ELogRaw of string

(** Events which occur at the global level. *)
type global_event =
  | GStart  (** Start running the tests. *)
  | GEnd    (** Finish running the tests. *)
  | GResults of (float * test_result list * int)

(* The type of test function *)
type test_fun = unit -> unit 

(* The type of tests *)
type test = 
  | TestCase of test_fun
  | TestList of test list
  | TestLabel of string * test

type state = 
    {
      tests_planned : (path * (unit -> unit)) list;
      results : test_result list;
    }


end
module OUnitChooser
= struct
#1 "oUnitChooser.ml"


(**
    Heuristic to pick a test to run.
   
    @author Sylvain Le Gall
  *)

open OUnitTypes

(** Most simple heuristic, just pick the first test. *)
let simple state =
  List.hd state.tests_planned

end
module OUnitUtils
= struct
#1 "oUnitUtils.ml"

(**
  * Utilities for OUnit
  *
  * @author Sylvain Le Gall
  *)

open OUnitTypes

let is_success = 
  function
    | RSuccess _  -> true 
    | RFailure _ | RError _  | RSkip _ | RTodo _ -> false 

let is_failure = 
  function
    | RFailure _ -> true
    | RSuccess _ | RError _  | RSkip _ | RTodo _ -> false

let is_error = 
  function 
    | RError _ -> true
    | RSuccess _ | RFailure _ | RSkip _ | RTodo _ -> false

let is_skip = 
  function
    | RSkip _ -> true
    | RSuccess _ | RFailure _ | RError _  | RTodo _ -> false

let is_todo = 
  function
    | RTodo _ -> true
    | RSuccess _ | RFailure _ | RError _  | RSkip _ -> false

let result_flavour = 
  function
    | RError _ -> "Error"
    | RFailure _ -> "Failure"
    | RSuccess _ -> "Success"
    | RSkip _ -> "Skip"
    | RTodo _ -> "Todo"

let result_path = 
  function
    | RSuccess path 
    | RError (path, _)
    | RFailure (path, _)
    | RSkip (path, _)
    | RTodo (path, _) -> path

let result_msg = 
  function
    | RSuccess _ -> "Success"
    | RError (_, msg)
    | RFailure (_, msg)
    | RSkip (_, msg)
    | RTodo (_, msg) -> msg

(* Returns true if the result list contains successes only. *)
let rec was_successful = 
  function
    | [] -> true
    | RSuccess _::t 
    | RSkip _::t -> 
        was_successful t

    | RFailure _::_
    | RError _::_ 
    | RTodo _::_ -> 
        false

let string_of_node = 
  function
    | ListItem n -> 
        string_of_int n
    | Label s -> 
        s

(* Return the number of available tests *)
let rec test_case_count = 
  function
    | TestCase _ -> 1 
    | TestLabel (_, t) -> test_case_count t
    | TestList l -> 
        List.fold_left 
          (fun c t -> c + test_case_count t) 
          0 l

let string_of_path path =
  String.concat ":" (List.rev_map string_of_node path)

let buff_format_printf f = 
  let buff = Buffer.create 13 in
  let fmt = Format.formatter_of_buffer buff in
    f fmt;
    Format.pp_print_flush fmt ();
    Buffer.contents buff

(* Applies function f in turn to each element in list. Function f takes
   one element, and integer indicating its location in the list *)
let mapi f l = 
  let rec rmapi cnt l = 
    match l with 
      | [] -> 
          [] 

      | h :: t -> 
          (f h cnt) :: (rmapi (cnt + 1) t) 
  in
    rmapi 0 l

let fold_lefti f accu l =
  let rec rfold_lefti cnt accup l = 
    match l with
      | [] -> 
          accup

      | h::t -> 
          rfold_lefti (cnt + 1) (f accup h cnt) t
  in
    rfold_lefti 0 accu l

end
module OUnitLogger
= struct
#1 "oUnitLogger.ml"
(*
 * Logger for information and various OUnit events.
 *)

open OUnitTypes
open OUnitUtils

type event_type = GlobalEvent of global_event | TestEvent of test_event

let format_event verbose event_type =
  match event_type with
    | GlobalEvent e ->
        begin
          match e with 
            | GStart ->
                ""
            | GEnd ->
                ""
            | GResults (running_time, results, test_case_count) -> 
                let separator1 = String.make (Format.get_margin ()) '=' in
                let separator2 = String.make (Format.get_margin ()) '-' in
                let buf = Buffer.create 1024 in
                let bprintf fmt = Printf.bprintf buf fmt in
                let print_results = 
                  List.iter 
                    (fun result -> 
                       bprintf "%s\n%s: %s\n\n%s\n%s\n" 
                         separator1 
                         (result_flavour result) 
                         (string_of_path (result_path result)) 
                         (result_msg result) 
                         separator2)
                in
                let errors   = List.filter is_error results in
                let failures = List.filter is_failure results in
                let skips    = List.filter is_skip results in
                let todos    = List.filter is_todo results in

                  if not verbose then
                    bprintf "\n";

                  print_results errors;
                  print_results failures;
                  bprintf "Ran: %d tests in: %.2f seconds.\n" 
                    (List.length results) running_time;

                  (* Print final verdict *)
                  if was_successful results then 
                    begin
                      if skips = [] then
                        bprintf "OK"
                      else 
                        bprintf "OK: Cases: %d Skip: %d"
                          test_case_count (List.length skips)
                    end
                  else
                    begin
                      bprintf
                        "FAILED: Cases: %d Tried: %d Errors: %d \
                              Failures: %d Skip:%d Todo:%d" 
                        test_case_count (List.length results) 
                        (List.length errors) (List.length failures)
                        (List.length skips) (List.length todos);
                    end;
                  bprintf "\n";
                  Buffer.contents buf
        end

    | TestEvent e ->
        begin
          let string_of_result = 
            if verbose then
              function
                | RSuccess _      -> "ok\n"
                | RFailure (_, _) -> "FAIL\n"
                | RError (_, _)   -> "ERROR\n"
                | RSkip (_, _)    -> "SKIP\n"
                | RTodo (_, _)    -> "TODO\n"
            else
              function
                | RSuccess _      -> "."
                | RFailure (_, _) -> "F"
                | RError (_, _)   -> "E"
                | RSkip (_, _)    -> "S"
                | RTodo (_, _)    -> "T"
          in
            if verbose then
              match e with 
                | EStart p -> 
                    Printf.sprintf "%s start\n" (string_of_path p)
                | EEnd p -> 
                    Printf.sprintf "%s end\n" (string_of_path p)
                | EResult result -> 
                    string_of_result result
                | ELog (lvl, str) ->
                    let prefix = 
                      match lvl with 
                        | LError -> "E"
                        | LWarning -> "W"
                        | LInfo -> "I"
                    in
                      prefix^": "^str
                | ELogRaw str ->
                    str
            else 
              match e with 
                | EStart _ | EEnd _ | ELog _ | ELogRaw _ -> ""
                | EResult result -> string_of_result result
        end

let file_logger fn =
  let chn = open_out fn in
    (fun ev ->
       output_string chn (format_event true ev);
       flush chn),
    (fun () -> close_out chn)

let std_logger verbose =
  (fun ev -> 
     print_string (format_event verbose ev);
     flush stdout),
  (fun () -> ())

let null_logger =
  ignore, ignore

let create output_file_opt verbose (log,close) =
  let std_log, std_close = std_logger verbose in
  let file_log, file_close = 
    match output_file_opt with 
      | Some fn ->
          file_logger fn
      | None ->
          null_logger
  in
    (fun ev ->
       std_log ev; file_log ev; log ev),
    (fun () ->
       std_close (); file_close (); close ())

let printf log fmt =
  Printf.ksprintf
    (fun s ->
       log (TestEvent (ELogRaw s)))
    fmt

end
module OUnit : sig 
#1 "oUnit.mli"
(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright (C) 2002-2008 Maas-Maarten Zeeman.                        *)
(* Copyright (C) 2010 OCamlCore SARL                                   *)
(*                                                                     *)
(* See LICENSE for details.                                            *)
(***********************************************************************)

(** Unit test building blocks
 
    @author Maas-Maarten Zeeman
    @author Sylvain Le Gall
  *)

(** {2 Assertions} 

    Assertions are the basic building blocks of unittests. *)

(** Signals a failure. This will raise an exception with the specified
    string. 

    @raise Failure signal a failure *)
val assert_failure : string -> 'a

(** Signals a failure when bool is false. The string identifies the 
    failure.
    
    @raise Failure signal a failure *)
val assert_bool : string -> bool -> unit

(** Shorthand for assert_bool 

    @raise Failure to signal a failure *)
val ( @? ) : string -> bool -> unit

(** Signals a failure when the string is non-empty. The string identifies the
    failure. 
    
    @raise Failure signal a failure *) 
val assert_string : string -> unit

(** [assert_command prg args] Run the command provided.

    @param exit_code expected exit code
    @param sinput provide this [char Stream.t] as input of the process
    @param foutput run this function on output, it can contains an
                   [assert_equal] to check it
    @param use_stderr redirect [stderr] to [stdout]
    @param env Unix environment
    @param verbose if a failure arise, dump stdout/stderr of the process to stderr

    @since 1.1.0
  *)
val assert_command : 
    ?exit_code:Unix.process_status ->
    ?sinput:char Stream.t ->
    ?foutput:(char Stream.t -> unit) ->
    ?use_stderr:bool ->
    ?env:string array ->
    ?verbose:bool ->
    string -> string list -> unit

(** [assert_equal expected real] Compares two values, when they are not equal a
    failure is signaled.

    @param cmp customize function to compare, default is [=]
    @param printer value printer, don't print value otherwise
    @param pp_diff if not equal, ask a custom display of the difference
                using [diff fmt exp real] where [fmt] is the formatter to use
    @param msg custom message to identify the failure

    @raise Failure signal a failure 
    
    @version 1.1.0
  *)
val assert_equal : 
  ?cmp:('a -> 'a -> bool) ->
  ?printer:('a -> string) -> 
  ?pp_diff:(Format.formatter -> ('a * 'a) -> unit) ->
  ?msg:string -> 'a -> 'a -> unit

(** Asserts if the expected exception was raised. 
   
    @param msg identify the failure

    @raise Failure description *)
val assert_raises : ?msg:string -> exn -> (unit -> 'a) -> unit

(** {2 Skipping tests } 
  
   In certain condition test can be written but there is no point running it, because they
   are not significant (missing OS features for example). In this case this is not a failure
   nor a success. Following functions allow you to escape test, just as assertion but without
   the same error status.
  
   A test skipped is counted as success. A test todo is counted as failure.
  *)

(** [skip cond msg] If [cond] is true, skip the test for the reason explain in [msg].
    For example [skip_if (Sys.os_type = "Win32") "Test a doesn't run on windows"].
    
    @since 1.0.3
  *)
val skip_if : bool -> string -> unit

(** The associated test is still to be done, for the reason given.
    
    @since 1.0.3
  *)
val todo : string -> unit

(** {2 Compare Functions} *)

(** Compare floats up to a given relative error. 
    
    @param epsilon if the difference is smaller [epsilon] values are equal
  *)
val cmp_float : ?epsilon:float -> float -> float -> bool

(** {2 Bracket}

    A bracket is a functional implementation of the commonly used
    setUp and tearDown feature in unittests. It can be used like this:

    ["MyTestCase" >:: (bracket test_set_up test_fun test_tear_down)] 
    
  *)

(** [bracket set_up test tear_down] The [set_up] function runs first, then
    the [test] function runs and at the end [tear_down] runs. The 
    [tear_down] function runs even if the [test] failed and help to clean
    the environment.
  *)
val bracket: (unit -> 'a) -> ('a -> unit) -> ('a -> unit) -> unit -> unit

(** [bracket_tmpfile test] The [test] function takes a temporary filename
    and matching output channel as arguments. The temporary file is created
    before the test and removed after the test.

    @param prefix see [Filename.open_temp_file]
    @param suffix see [Filename.open_temp_file]
    @param mode see [Filename.open_temp_file]
    
    @since 1.1.0
  *)
val bracket_tmpfile: 
  ?prefix:string -> 
  ?suffix:string -> 
  ?mode:open_flag list ->
  ((string * out_channel) -> unit) -> unit -> unit 

(** {2 Constructing Tests} *)

(** The type of test function *)
type test_fun = unit -> unit

(** The type of tests *)
type test =
    TestCase of test_fun
  | TestList of test list
  | TestLabel of string * test

(** Create a TestLabel for a test *)
val (>:) : string -> test -> test

(** Create a TestLabel for a TestCase *)
val (>::) : string -> test_fun -> test

(** Create a TestLabel for a TestList *)
val (>:::) : string -> test list -> test

(** Some shorthands which allows easy test construction.

   Examples:

   - ["test1" >: TestCase((fun _ -> ()))] =>  
   [TestLabel("test2", TestCase((fun _ -> ())))]
   - ["test2" >:: (fun _ -> ())] => 
   [TestLabel("test2", TestCase((fun _ -> ())))]
   - ["test-suite" >::: ["test2" >:: (fun _ -> ());]] =>
   [TestLabel("test-suite", TestSuite([TestLabel("test2", TestCase((fun _ -> ())))]))]
*)

(** [test_decorate g tst] Apply [g] to test function contains in [tst] tree.
    
    @since 1.0.3
  *)
val test_decorate : (test_fun -> test_fun) -> test -> test

(** [test_filter paths tst] Filter test based on their path string representation. 
    
    @param skip] if set, just use [skip_if] for the matching tests.
    @since 1.0.3
  *)
val test_filter : ?skip:bool -> string list -> test -> test option

(** {2 Retrieve Information from Tests} *)

(** Returns the number of available test cases *)
val test_case_count : test -> int

(** Types which represent the path of a test *)
type node = ListItem of int | Label of string
type path = node list (** The path to the test (in reverse order). *)

(** Make a string from a node *)
val string_of_node : node -> string

(** Make a string from a path. The path will be reversed before it is 
    tranlated into a string *)
val string_of_path : path -> string

(** Returns a list with paths of the test *)
val test_case_paths : test -> path list

(** {2 Performing Tests} *)

(** Severity level for log. *) 
type log_severity = 
  | LError
  | LWarning
  | LInfo

(** The possible results of a test *)
type test_result =
    RSuccess of path
  | RFailure of path * string
  | RError of path * string
  | RSkip of path * string
  | RTodo of path * string

(** Events which occur during a test run. *)
type test_event =
    EStart of path                (** A test start. *)
  | EEnd of path                  (** A test end. *)
  | EResult of test_result        (** Result of a test. *)
  | ELog of log_severity * string (** An event is logged in a test. *)
  | ELogRaw of string             (** Print raw data in the log. *)

(** Perform the test, allows you to build your own test runner *)
val perform_test : (test_event -> 'a) -> test -> test_result list

(** A simple text based test runner. It prints out information
    during the test. 

    @param verbose print verbose message
  *)
val run_test_tt : ?verbose:bool -> test -> test_result list

(** Main version of the text based test runner. It reads the supplied command 
    line arguments to set the verbose level and limit the number of test to 
    run.
    
    @param arg_specs add extra command line arguments
    @param set_verbose call a function to set verbosity

    @version 1.1.0
  *)
val run_test_tt_main : 
    ?arg_specs:(Arg.key * Arg.spec * Arg.doc) list -> 
    ?set_verbose:(bool -> unit) -> 
    test -> test_result list

end = struct
#1 "oUnit.ml"
(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright (C) 2002-2008 Maas-Maarten Zeeman.                        *)
(* Copyright (C) 2010 OCamlCore SARL                                   *)
(*                                                                     *)
(* See LICENSE for details.                                            *)
(***********************************************************************)

open OUnitUtils
include OUnitTypes

(*
 * Types and global states.
 *)

let global_verbose = ref false

let global_output_file = 
  let pwd = Sys.getcwd () in
  let ocamlbuild_dir = Filename.concat pwd "_build" in
  let dir = 
    if Sys.file_exists ocamlbuild_dir && Sys.is_directory ocamlbuild_dir then
      ocamlbuild_dir
    else 
      pwd
  in
    ref (Some (Filename.concat dir "oUnit.log"))

let global_logger = ref (fst OUnitLogger.null_logger)

let global_chooser = ref OUnitChooser.simple

let bracket set_up f tear_down () =
  let fixture = 
    set_up () 
  in
  let () = 
    try
      let () = f fixture in
        tear_down fixture
    with e -> 
      let () = 
        tear_down fixture
      in
        raise e
  in
    ()

let bracket_tmpfile ?(prefix="ounit-") ?(suffix=".txt") ?mode f =
  bracket
    (fun () ->
       Filename.open_temp_file ?mode prefix suffix)
    f 
    (fun (fn, chn) ->
       begin
         try 
           close_out chn
         with _ ->
           ()
       end;
       begin
         try
           Sys.remove fn
         with _ ->
           ()
       end)

exception Skip of string
let skip_if b msg =
  if b then
    raise (Skip msg)

exception Todo of string
let todo msg =
  raise (Todo msg)

let assert_failure msg = 
  failwith ("OUnit: " ^ msg)

let assert_bool msg b =
  if not b then assert_failure msg

let assert_string str =
  if not (str = "") then assert_failure str

let assert_equal ?(cmp = ( = )) ?printer ?pp_diff ?msg expected actual =
  let get_error_string () =
    let res =
      buff_format_printf
        (fun fmt ->
           Format.pp_open_vbox fmt 0;
           begin
             match msg with 
               | Some s ->
                   Format.pp_open_box fmt 0;
                   Format.pp_print_string fmt s;
                   Format.pp_close_box fmt ();
                   Format.pp_print_cut fmt ()
               | None -> 
                   ()
           end;

           begin
             match printer with
               | Some p ->
                   Format.fprintf fmt
                     "@[expected: @[%s@]@ but got: @[%s@]@]@,"
                     (p expected)
                     (p actual)

               | None ->
                   Format.fprintf fmt "@[not equal@]@,"
           end;

           begin
             match pp_diff with 
               | Some d ->
                   Format.fprintf fmt 
                     "@[differences: %a@]@,"
                      d (expected, actual)

               | None ->
                   ()
           end;
           Format.pp_close_box fmt ())
    in
    let len = 
      String.length res
    in
      if len > 0 && res.[len - 1] = '\n' then
        String.sub res 0 (len - 1)
      else
        res
  in
    if not (cmp expected actual) then 
      assert_failure (get_error_string ())

let assert_command 
    ?(exit_code=Unix.WEXITED 0)
    ?(sinput=Stream.of_list [])
    ?(foutput=ignore)
    ?(use_stderr=true)
    ?env
    ?verbose
    prg args =

    bracket_tmpfile 
      (fun (fn_out, chn_out) ->
         let cmd_print fmt =
           let () = 
             match env with
               | Some e ->
                   begin
                     Format.pp_print_string fmt "env";
                     Array.iter (Format.fprintf fmt "@ %s") e;
                     Format.pp_print_space fmt ()
                   end
               
               | None ->
                   ()
           in
             Format.pp_print_string fmt prg;
             List.iter (Format.fprintf fmt "@ %s") args
         in

         (* Start the process *)
         let in_write = 
           Unix.dup (Unix.descr_of_out_channel chn_out)
         in
         let (out_read, out_write) = 
           Unix.pipe () 
         in
         let err = 
           if use_stderr then
             in_write
           else
             Unix.stderr
         in
         let args = 
           Array.of_list (prg :: args)
         in
         let pid =
           OUnitLogger.printf !global_logger "%s"
             (buff_format_printf
                (fun fmt ->
                   Format.fprintf fmt "@[Starting command '%t'@]\n" cmd_print));
           Unix.set_close_on_exec out_write;
           match env with 
             | Some e -> 
                 Unix.create_process_env prg args e out_read in_write err
             | None -> 
                 Unix.create_process prg args out_read in_write err
         in
         let () =
           Unix.close out_read; 
           Unix.close in_write
         in
         let () =
           (* Dump sinput into the process stdin *)
           let buff = Bytes.of_string " " in
             Stream.iter 
               (fun c ->
                  let _i : int =
                    Bytes.set buff 0  c;
                    Unix.write out_write buff 0 1
                  in
                    ())
               sinput;
             Unix.close out_write
         in
         let _, real_exit_code =
           let rec wait_intr () = 
             try 
               Unix.waitpid [] pid
             with Unix.Unix_error (Unix.EINTR, _, _) ->
               wait_intr ()
           in
             wait_intr ()
         in
         let exit_code_printer =
           function
             | Unix.WEXITED n ->
                 Printf.sprintf "exit code %d" n
             | Unix.WSTOPPED n ->
                 Printf.sprintf "stopped by signal %d" n
             | Unix.WSIGNALED n ->
                 Printf.sprintf "killed by signal %d" n
         in

           (* Dump process output to stderr *)
           begin
             let chn = open_in fn_out in
             let buff = String.make 4096 'X' in
             let len = ref (-1) in
               while !len <> 0 do 
                 len := input chn buff 0 (String.length buff);
                 OUnitLogger.printf !global_logger "%s" (String.sub buff 0 !len);
               done;
               close_in chn
           end;

           (* Check process status *)
           assert_equal 
             ~msg:(buff_format_printf 
                     (fun fmt ->
                        Format.fprintf fmt 
                          "@[Exit status of command '%t'@]" cmd_print))
             ~printer:exit_code_printer
             exit_code
             real_exit_code;

           begin
             let chn = open_in fn_out in
               try 
                 foutput (Stream.of_channel chn)
               with e ->
                 close_in chn;
                 raise e
           end)
      ()

let raises f =
  try
    f ();
    None
  with e -> 
    Some e

let assert_raises ?msg exn (f: unit -> 'a) = 
  let pexn = 
    Printexc.to_string 
  in
  let get_error_string () =
    let str = 
      Format.sprintf 
        "expected exception %s, but no exception was raised." 
        (pexn exn)
    in
      match msg with
        | None -> 
            assert_failure str
              
        | Some s -> 
            assert_failure (s^"\n"^str)
  in    
    match raises f with
      | None -> 
          assert_failure (get_error_string ())

      | Some e -> 
          assert_equal ?msg ~printer:pexn exn e

(* Compare floats up to a given relative error *)
let cmp_float ?(epsilon = 0.00001) a b =
  abs_float (a -. b) <= epsilon *. (abs_float a) ||
    abs_float (a -. b) <= epsilon *. (abs_float b) 
      
(* Now some handy shorthands *)
let (@?) = assert_bool

(* Some shorthands which allows easy test construction *)
let (>:) s t = TestLabel(s, t)             (* infix *)
let (>::) s f = TestLabel(s, TestCase(f))  (* infix *)
let (>:::) s l = TestLabel(s, TestList(l)) (* infix *)

(* Utility function to manipulate test *)
let rec test_decorate g =
  function
    | TestCase f -> 
        TestCase (g f)
    | TestList tst_lst ->
        TestList (List.map (test_decorate g) tst_lst)
    | TestLabel (str, tst) ->
        TestLabel (str, test_decorate g tst)

let test_case_count = OUnitUtils.test_case_count 
let string_of_node = OUnitUtils.string_of_node
let string_of_path = OUnitUtils.string_of_path
    
(* Returns all possible paths in the test. The order is from test case
   to root 
 *)
let test_case_paths test = 
  let rec tcps path test = 
    match test with 
      | TestCase _ -> 
          [path] 

      | TestList tests -> 
          List.concat 
            (mapi (fun t i -> tcps ((ListItem i)::path) t) tests)

      | TestLabel (l, t) -> 
          tcps ((Label l)::path) t
  in
    tcps [] test

(* Test filtering with their path *)
module SetTestPath = Set.Make(String)

let test_filter ?(skip=false) only test =
  let set_test =
    List.fold_left 
      (fun st str -> SetTestPath.add str st)
      SetTestPath.empty
      only
  in
  let rec filter_test path tst =
    if SetTestPath.mem (string_of_path path) set_test then
      begin
        Some tst
      end

    else
      begin
        match tst with
          | TestCase f ->
              begin
                if skip then
                  Some 
                    (TestCase 
                       (fun () ->
                          skip_if true "Test disabled";
                          f ()))
                else
                  None
              end

          | TestList tst_lst ->
              begin
                let ntst_lst =
                  fold_lefti 
                    (fun ntst_lst tst i ->
                       let nntst_lst =
                         match filter_test ((ListItem i) :: path) tst with
                           | Some tst ->
                               tst :: ntst_lst
                           | None ->
                               ntst_lst
                       in
                         nntst_lst)
                    []
                    tst_lst
                in
                  if not skip && ntst_lst = [] then
                    None
                  else
                    Some (TestList (List.rev ntst_lst))
              end

          | TestLabel (lbl, tst) ->
              begin
                let ntst_opt =
                  filter_test 
                    ((Label lbl) :: path)
                    tst
                in
                  match ntst_opt with 
                    | Some ntst ->
                        Some (TestLabel (lbl, ntst))
                    | None ->
                        if skip then
                          Some (TestLabel (lbl, tst))
                        else
                          None
              end
      end
  in
    filter_test [] test


(* The possible test results *)
let is_success = OUnitUtils.is_success
let is_failure = OUnitUtils.is_failure
let is_error   = OUnitUtils.is_error  
let is_skip    = OUnitUtils.is_skip   
let is_todo    = OUnitUtils.is_todo   

(* TODO: backtrace is not correct *)
let maybe_backtrace = ""
  (* Printexc.get_backtrace () *)
    (* (if Printexc.backtrace_status () then *)
    (*    "\n" ^ Printexc.get_backtrace () *)
    (*  else "") *)
(* Events which can happen during testing *)

(* DEFINE MAYBE_BACKTRACE = *)
(* IFDEF BACKTRACE THEN *)
(*     (if Printexc.backtrace_status () then *)
(*        "\n" ^ Printexc.get_backtrace () *)
(*      else "") *)
(* ELSE *)
(*     "" *)
(* ENDIF *)

(* Run all tests, report starts, errors, failures, and return the results *)
let perform_test report test =
  let run_test_case f path =
    try 
      f ();
      RSuccess path
    with
      | Failure s -> 
          RFailure (path, s ^ maybe_backtrace)

      | Skip s -> 
          RSkip (path, s)

      | Todo s -> 
          RTodo (path, s)

      | s -> 
          RError (path, (Printexc.to_string s) ^ maybe_backtrace)
  in
  let rec flatten_test path acc = 
    function
      | TestCase(f) -> 
          (path, f) :: acc

      | TestList (tests) ->
          fold_lefti 
            (fun acc t cnt -> 
               flatten_test 
                 ((ListItem cnt)::path) 
                 acc t)
            acc tests
      
      | TestLabel (label, t) -> 
          flatten_test ((Label label)::path) acc t
  in
  let test_cases = List.rev (flatten_test [] [] test) in
  let runner (path, f) = 
    let result = 
      report (EStart path);
      run_test_case f path 
    in
      report (EResult result);
      report (EEnd path);
      result
  in
  let rec iter state = 
    match state.tests_planned with 
      | [] ->
          state.results
      | _ ->
          let (path, f) = !global_chooser state in            
          let result = runner (path, f) in
            iter 
              {
                results = result :: state.results;
                tests_planned = 
                  List.filter 
                    (fun (path', _) -> path <> path') state.tests_planned
              }
  in
    iter {results = []; tests_planned = test_cases}

(* Function which runs the given function and returns the running time
   of the function, and the original result in a tuple *)
let time_fun f x y =
  let begin_time = Unix.gettimeofday () in
  let result = f x y in
  let end_time = Unix.gettimeofday () in
    (end_time -. begin_time, result)

(* A simple (currently too simple) text based test runner *)
let run_test_tt ?verbose test =
  let log, log_close = 
    OUnitLogger.create 
      !global_output_file 
      !global_verbose 
      OUnitLogger.null_logger
  in
  let () = 
    global_logger := log
  in

  (* Now start the test *)
  let running_time, results = 
    time_fun 
      perform_test 
      (fun ev ->
         log (OUnitLogger.TestEvent ev))
      test 
  in
    
    (* Print test report *)
    log (OUnitLogger.GlobalEvent (GResults (running_time, results, test_case_count test)));

    (* Reset logger. *)
    log_close ();
    global_logger := fst OUnitLogger.null_logger;

    (* Return the results possibly for further processing *)
    results
      
(* Call this one from you test suites *)
let run_test_tt_main ?(arg_specs=[]) ?(set_verbose=ignore) suite = 
  let only_test = ref [] in
  let () = 
    Arg.parse
      (Arg.align
         [
           "-verbose", 
           Arg.Set global_verbose, 
           " Run the test in verbose mode.";

           "-only-test", 
           Arg.String (fun str -> only_test := str :: !only_test),
           "path Run only the selected test";

           "-output-file",
           Arg.String (fun s -> global_output_file := Some s),
           "fn Output verbose log in this file.";

           "-no-output-file",
           Arg.Unit (fun () -> global_output_file := None),
           " Prevent to write log in a file.";

           "-list-test",
           Arg.Unit
             (fun () -> 
                List.iter
                  (fun pth ->
                     print_endline (string_of_path pth))
                  (test_case_paths suite);
                exit 0),
           " List tests";
         ] @ arg_specs
      )
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("usage: " ^ Sys.argv.(0) ^ " [-verbose] [-only-test path]*")
  in
  let nsuite = 
    if !only_test = [] then
      suite
    else
      begin
        match test_filter ~skip:true !only_test suite with 
          | Some test ->
              test
          | None ->
              failwith ("Filtering test "^
                        (String.concat ", " !only_test)^
                        " lead to no test")
      end
  in

  let result = 
    set_verbose !global_verbose;
    run_test_tt ~verbose:!global_verbose nsuite 
  in
    if not (was_successful result) then
      exit 1
    else
      result

end
module Ext_array : sig 
#1 "ext_array.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)






(** Some utilities for {!Array} operations *)

val reverse_in_place : 'a array -> unit

val reverse_of_list : 'a list -> 'a array

val filter : ('a -> bool) -> 'a array -> 'a array

val filter_map : ('a -> 'b option) -> 'a array -> 'b array

val range : int -> int -> int array

val map2i : (int -> 'a -> 'b -> 'c ) -> 'a array -> 'b array -> 'c array

val to_list_map : ('a -> 'b option) -> 'a array -> 'b list 

val rfind_with_index : 'a array -> ('a -> 'b -> bool) -> 'b -> int


type 'a split = [ `No_split | `Split of 'a array * 'a array ]

val rfind_and_split : 
  'a array ->
  ('a -> 'b -> bool) ->
  'b -> 'a split

val find_and_split : 
  'a array ->
  ('a -> 'b -> bool) ->
  'b -> 'a split

end = struct
#1 "ext_array.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)







let reverse_in_place a =
  let aux a i len =
    if len=0 then ()
    else
      for k = 0 to (len-1)/2 do
        let t = Array.unsafe_get a (i+k) in
        Array.unsafe_set a (i+k) ( Array.unsafe_get a (i+len-1-k));
        Array.unsafe_set a (i+len-1-k) t;
      done
  in
  aux a 0 (Array.length a)


let reverse_of_list =  function
  | [] -> [||]
  | hd::tl as l ->
    let len = List.length l in
    let a = Array.make len hd in
    let rec fill i = function
      | [] -> a
      | hd::tl -> Array.unsafe_set a (len - i - 2) hd; fill (i+1) tl in
    fill 0 tl

let filter f a =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      if f  v then 
        aux (v::acc) (i+1)
      else aux acc (i + 1) 
  in aux [] 0


let filter_map (f : _ -> _ option) a =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      match f  v with 
      | Some v -> 
        aux (v::acc) (i+1)
      | None -> 
        aux acc (i + 1) 
  in aux [] 0

let range from to_ =
  if from > to_ then invalid_arg "Ext_array.range"  
  else Array.init (to_ - from + 1) (fun i -> i + from)

let map2i f a b = 
  let len = Array.length a in 
  if len <> Array.length b then 
    invalid_arg "Ext_array.map2i"  
  else
    Array.mapi (fun i a -> f i  a ( Array.unsafe_get b i )) a 

let to_list_map f a =
  let rec tolist i res =
    if i < 0 then res else
      let v = Array.unsafe_get a i in
      tolist (i - 1)
        (match f v with
         | Some v -> v :: res
         | None -> res) in
  tolist (Array.length a - 1) []

(**
{[
# rfind_with_index [|1;2;3|] (=) 2;;
- : int = 1
# rfind_with_index [|1;2;3|] (=) 1;;
- : int = 0
# rfind_with_index [|1;2;3|] (=) 3;;
- : int = 2
# rfind_with_index [|1;2;3|] (=) 4;;
- : int = -1
]}
*)
let rfind_with_index arr cmp v = 
  let len = Array.length arr in 
  let rec aux i = 
    if i < 0 then i
    else if  cmp (Array.unsafe_get arr i) v then i
    else aux (i - 1) in 
  aux (len - 1)

type 'a split = [ `No_split | `Split of 'a array * 'a array ]
let rfind_and_split arr cmp v : _ split = 
  let i = rfind_with_index arr cmp v in 
  if  i < 0 then 
    `No_split 
  else 
    `Split (Array.sub arr 0 i , Array.sub arr  (i + 1 ) (Array.length arr - i - 1 ))


let find_with_index arr cmp v = 
  let len  = Array.length arr in 
  let rec aux i len = 
    if i >= len then -1 
    else if cmp (Array.unsafe_get arr i ) v then i 
    else aux (i + 1) len in 
  aux 0 len

let find_and_split arr cmp v : _ split = 
  let i = find_with_index arr cmp v in 
  if i < 0 then 
    `No_split
  else
    `Split (Array.sub arr 0 i, Array.sub arr (i + 1 ) (Array.length arr - i - 1))        

end
module Ext_bytes : sig 
#1 "ext_bytes.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)







(** Port the {!Bytes.escaped} from trunk to make it not locale sensitive *)

val escaped : bytes -> bytes

end = struct
#1 "ext_bytes.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let escaped s =
  let n = ref 0 in
  for i = 0 to Bytes.length s - 1 do
    n := !n +
      (match Bytes.unsafe_get s i with
       | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | ' ' .. '~' -> 1
       | _ -> 4)
  done;
  if !n = Bytes.length s then Bytes.copy s else begin
    let s' = Bytes.create !n in
    n := 0;
    for i = 0 to Bytes.length s - 1 do
      begin match Bytes.unsafe_get s i with
      | ('"' | '\\') as c ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n c
      | '\n' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'n'
      | '\t' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 't'
      | '\r' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'r'
      | '\b' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'b'
      | (' ' .. '~') as c -> Bytes.unsafe_set s' !n c
      | c ->
          let a = char_code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a / 100));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a mod 10));
      end;
      incr n
    done;
    s'
  end

end
module Ext_string : sig 
#1 "ext_string.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








(** Extension to the standard library [String] module, avoid locale sensitivity *) 


val trim : string -> string 

val split_by : ?keep_empty:bool -> (char -> bool) -> string -> string list
(** default is false *)

val split : ?keep_empty:bool -> string -> char -> string list
(** default is false *)

val starts_with : string -> string -> bool

(**
   return [-1] when not found, the returned index is useful 
   see [ends_with_then_chop]
*)
val ends_with_index : string -> string -> int

val ends_with : string -> string -> bool

(**
   {[
     ends_with_then_chop "a.cmj" ".cmj"
     "a"
   ]}
   This is useful in controlled or file case sensitve system
*)
val ends_with_then_chop : string -> string -> string option


val escaped : string -> string

val for_all : (char -> bool) -> string -> bool

val is_empty : string -> bool

val repeat : int -> string -> string 

val equal : string -> string -> bool

val find : ?start:int -> sub:string -> string -> int

val rfind : sub:string -> string -> int

val tail_from : string -> int -> string

val digits_of_str : string -> offset:int -> int -> int

val starts_with_and_number : string -> offset:int -> string -> int

val unsafe_concat_with_length : int -> string -> string list -> string

end = struct
#1 "ext_string.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








let split_by ?(keep_empty=false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      if last_pos = 0 && not keep_empty then
        (*
           {[ split " test_unsafe_obj_ffi_ppx.cmi" ~keep_empty:false ' ']}
        *)
        acc
      else 
        String.sub str 0 last_pos :: acc
    else
      if is_delim str.[pos] then
        let new_len = (last_pos - pos - 1) in
        if new_len <> 0 || keep_empty then 
          let v = String.sub str (pos + 1) new_len in
          loop ( v :: acc)
            pos (pos - 1)
        else loop acc pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let trim s = 
  let i = ref 0  in
  let j = String.length s in 
  while !i < j &&  let u = s.[!i] in u = '\t' || u = '\n' || u = ' ' do 
    incr i;
  done;
  let k = ref (j - 1)  in 
  while !k >= !i && let u = s.[!k] in u = '\t' || u = '\n' || u = ' ' do 
    decr k ;
  done;
  String.sub s !i (!k - !i + 1)

let split ?keep_empty  str on = 
  if str = "" then [] else 
  split_by ?keep_empty (fun x -> (x : char) = on) str  ;;

let starts_with s beg = 
  let beg_len = String.length beg in
  let s_len = String.length s in
   beg_len <=  s_len &&
  (let i = ref 0 in
    while !i <  beg_len 
          && String.unsafe_get s !i =
             String.unsafe_get beg !i do 
      incr i 
    done;
    !i = beg_len
  )



let ends_with_index s beg = 
  let s_finish = String.length s - 1 in
  let s_beg = String.length beg - 1 in
  if s_beg > s_finish then -1
  else
    let rec aux j k = 
      if k < 0 then (j + 1)
      else if String.unsafe_get s j = String.unsafe_get beg k then 
        aux (j - 1) (k - 1)
      else  -1 in 
    aux s_finish s_beg

let ends_with s beg = ends_with_index s beg >= 0 


let ends_with_then_chop s beg = 
  let i =  ends_with_index s beg in 
  if i >= 0 then Some (String.sub s 0 i) 
  else None

(**  In OCaml 4.02.3, {!String.escaped} is locale senstive, 
     this version try to make it not locale senstive, this bug is fixed
     in the compiler trunk     
*)
let escaped s =
  let rec needs_escape i =
    if i >= String.length s then false else
      match String.unsafe_get s i with
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> true
      | ' ' .. '~' -> needs_escape (i+1)
      | _ -> true
  in
  if needs_escape 0 then
    Bytes.unsafe_to_string (Ext_bytes.escaped (Bytes.unsafe_of_string s))
  else
    s


let for_all (p : char -> bool) s = 
  let len = String.length s in
  let rec aux i = 
    if i >= len then true 
    else  p (String.unsafe_get s i) && aux (i + 1)
  in aux 0 

let is_empty s = String.length s = 0


let repeat n s  =
  let len = String.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len
  done;
  Bytes.to_string res

let equal (x : string) y  = x = y



let _is_sub ~sub i s j ~len =
  let rec check k =
    if k = len
    then true
    else 
      String.unsafe_get sub (i+k) = 
      String.unsafe_get s (j+k) && check (k+1)
  in
  j+len <= String.length s && check 0



let find ?(start=0) ~sub s =
  let n = String.length sub in
  let i = ref start in
  let module M = struct exception Exit end  in
  try
    while !i + n <= String.length s do
      if _is_sub ~sub 0 s !i ~len:n then raise M.Exit;
      incr i
    done;
    -1
  with M.Exit ->
    !i


let rfind ~sub s =
  let n = String.length sub in
  let i = ref (String.length s - n) in
  let module M = struct exception Exit end in 
  try
    while !i >= 0 do
      if _is_sub ~sub 0 s !i ~len:n then raise M.Exit;
      decr i
    done;
    -1
  with M.Exit ->
    !i

let tail_from s x = 
  let len = String.length s  in 
  if  x > len then invalid_arg ("Ext_string.tail_from " ^s ^ " : "^ string_of_int x )
  else String.sub s x (len - x)


(**
   {[ 
     digits_of_str "11_js" 2 == 11     
   ]}
*)
let digits_of_str s ~offset x = 
  let rec aux i acc s x  = 
    if i >= x then acc 
    else aux (i + 1) (10 * acc + Char.code s.[offset + i] - 48 (* Char.code '0' *)) s x in 
  aux 0 0 s x 



(*
   {[
     starts_with_and_number "js_fn_mk_01" 0 "js_fn_mk_" = 1 ;;
     starts_with_and_number "js_fn_run_02" 0 "js_fn_mk_" = -1 ;;
     starts_with_and_number "js_fn_mk_03" 6 "mk_" = 3 ;;
     starts_with_and_number "js_fn_mk_04" 6 "run_" = -1;;
     starts_with_and_number "js_fn_run_04" 6 "run_" = 4;;
     (starts_with_and_number "js_fn_run_04" 6 "run_" = 3) = false ;;
   ]}
*)
let starts_with_and_number s ~offset beg =
  let beg_len = String.length beg in
  let s_len = String.length s in
  let finish_delim = offset + beg_len in 

   if finish_delim >  s_len  then -1 
   else 
     let i = ref offset  in
      while !i <  finish_delim
            && String.unsafe_get s !i =
               String.unsafe_get beg (!i - offset) do 
        incr i 
      done;
      if !i = finish_delim then 
        digits_of_str ~offset:finish_delim s 2 
      else 
        -1 

let equal (x : string) y  = x = y

let unsafe_concat_with_length len sep l =
  match l with 
  | [] -> ""
  | hd :: tl -> (* num is positive *)
  let r = Bytes.create len in
  let hd_len = String.length hd in 
  let sep_len = String.length sep in 
  String.unsafe_blit hd 0 r 0 hd_len;
  let pos = ref hd_len in
  List.iter
    (fun s ->
       let s_len = String.length s in
       String.unsafe_blit sep 0 r !pos sep_len;
       pos := !pos +  sep_len;
       String.unsafe_blit s 0 r !pos s_len;
       pos := !pos + s_len)
    tl;
  Bytes.unsafe_to_string r

end
module Ounit_array_tests
= struct
#1 "ounit_array_tests.ml"
let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal
let suites = 
    __FILE__
    >:::
    [
     __LOC__ >:: begin fun _ ->
        Ext_array.find_and_split 
            [|"a"; "b";"c"|]
            Ext_string.equal "--" =~ `No_split
     end;
    __LOC__ >:: begin fun _ ->
        Ext_array.find_and_split 
            [|"a"; "b";"c";"--"|]
            Ext_string.equal "--" =~ `Split ([|"a";"b";"c"|],[||])
     end;
     __LOC__ >:: begin fun _ ->
        Ext_array.find_and_split 
            [|"--"; "a"; "b";"c";"--"|]
            Ext_string.equal "--" =~ `Split ([||], [|"a";"b";"c";"--"|])
     end;
    __LOC__ >:: begin fun _ ->
        Ext_array.find_and_split 
            [| "u"; "g"; "--"; "a"; "b";"c";"--"|]
            Ext_string.equal "--" =~ `Split ([|"u";"g"|], [|"a";"b";"c";"--"|])
     end;
    ]
end
module String_map : sig 
#1 "string_map.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








include Map.S with type key = string 

val of_list : (string * 'a) list -> 'a t

val add_list : (string * 'b) list -> 'b t -> 'b t

val find_opt : string -> 'a t -> 'a option

val find_default : string -> 'a -> 'a t -> 'a

val print :  (Format.formatter -> 'a -> unit) -> Format.formatter ->  'a t -> unit

end = struct
#1 "string_map.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








include Map.Make(String)

let of_list (xs : ('a * 'b) list ) = 
  List.fold_left (fun acc (k,v) -> add k v acc) empty xs 

let add_list (xs : ('a * 'b) list ) init = 
  List.fold_left (fun acc (k,v) -> add k v acc) init xs 


let find_opt k m =
  match find k m with 
  | exception v -> None
  | u -> Some u

let find_default k default m =
  match find k m with 
  | exception v -> default 
  | u -> u

let print p_v fmt  m =
  iter (fun k v -> 
      Format.fprintf fmt "@[%s@ ->@ %a@]@." k p_v v 
    ) m



end
module Bsb_json : sig 
#1 "bsb_json.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type js_array =  
  { content : t array ; 
    loc_start : Lexing.position ; 
    loc_end : Lexing.position ; 
  }
and js_str = 
  { str : string ; loc : Lexing.position}
and t = 
  [
    `True
  | `False
  | `Null
  | `Flo of string 
  | `Str of js_str
  | `Arr of js_array
  | `Obj of t String_map.t 
  ]

val parse_json : Lexing.lexbuf -> t 
val parse_json_from_string : string -> t 
val parse_json_from_chan : in_channel -> t 
val parse_json_from_file  : string -> t

type path = string list 
type status = 
  | No_path
  | Found of t 
  | Wrong_type of path 


type callback = 
  [
    `Str of (string -> unit) 
  | `Str_loc of (string -> Lexing.position -> unit)
  | `Flo of (string -> unit )
  | `Bool of (bool -> unit )
  | `Obj of (t String_map.t -> unit)
  | `Arr of (t array -> unit )
  | `Arr_loc of (t array -> Lexing.position -> Lexing.position -> unit)
  | `Null of (unit -> unit)
  ]

val test:
  ?fail:(unit -> unit) ->
  string -> callback -> t String_map.t -> t String_map.t

val query : path -> t ->  status

end = struct
#1 "bsb_json.ml"
# 1 "bsb/bsb_json.mll"
 
type error =
  | Illegal_character of char
  | Unterminated_string
  | Unterminated_comment
  | Illegal_escape of string
  | Unexpected_token 
  | Expect_comma_or_rbracket
  | Expect_comma_or_rbrace
  | Expect_colon
  | Expect_string_or_rbrace 
  | Expect_eof 
  (* | Trailing_comma_in_obj *)
  (* | Trailing_comma_in_array *)
exception Error of error * Lexing.position * Lexing.position;;

let fprintf  = Format.fprintf
let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_string -> 
      fprintf ppf "Unterminated_string"
  | Expect_comma_or_rbracket ->
    fprintf ppf "Expect_comma_or_rbracket"
  | Expect_comma_or_rbrace -> 
    fprintf ppf "Expect_comma_or_rbrace"
  | Expect_colon -> 
    fprintf ppf "Expect_colon"
  | Expect_string_or_rbrace  -> 
    fprintf ppf "Expect_string_or_rbrace"
  | Expect_eof  -> 
    fprintf ppf "Expect_eof"
  | Unexpected_token 
    ->
    fprintf ppf "Unexpected_token"
  (* | Trailing_comma_in_obj  *)
  (*   -> fprintf ppf "Trailing_comma_in_obj" *)
  (* | Trailing_comma_in_array  *)
  (*   -> fprintf ppf "Trailing_comma_in_array" *)
  | Unterminated_comment 
    -> fprintf ppf "Unterminated_comment"
         
let print_position fmt (pos : Lexing.position) = 
  Format.fprintf fmt "(%d,%d)" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)


let () = 
  Printexc.register_printer
    (function x -> 
     match x with 
     | Error (e , a, b) -> 
       Some (Format.asprintf "@[%a:@ %a@ -@ %a)@]" report_error e 
               print_position a print_position b)
     | _ -> None
    )
  
type path = string list 



type token = 
  | Comma
  | Eof
  | False
  | Lbrace
  | Lbracket
  | Null
  | Colon
  | Number of string
  | Rbrace
  | Rbracket
  | String of string
  | True   
  

let error  (lexbuf : Lexing.lexbuf) e = 
  raise (Error (e, lexbuf.lex_start_p, lexbuf.lex_curr_p))

let lexeme_len (x : Lexing.lexbuf) =
  x.lex_curr_pos - x.lex_start_pos

let update_loc ({ lex_curr_p; _ } as lexbuf : Lexing.lexbuf) diff =
  lexbuf.lex_curr_p <-
    {
      lex_curr_p with
      pos_lnum = lex_curr_p.pos_lnum + 1;
      pos_bol = lex_curr_p.pos_cnum - diff;
    }

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let dec_code c1 c2 c3 =
  100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

let hex_code c1 c2 =
  let d1 = Char.code c1 in
  let val1 =
    if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48 in
  let d2 = Char.code c2 in
  let val2 =
    if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48 in
  val1 * 16 + val2

let lf = '\010'

# 119 "bsb/bsb_json.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\239\255\240\255\241\255\000\000\025\000\011\000\244\255\
    \245\255\246\255\247\255\248\255\249\255\000\000\000\000\000\000\
    \041\000\001\000\254\255\005\000\005\000\253\255\001\000\002\000\
    \252\255\000\000\000\000\003\000\251\255\001\000\003\000\250\255\
    \079\000\089\000\099\000\121\000\131\000\141\000\153\000\163\000\
    \001\000\253\255\254\255\023\000\255\255\006\000\246\255\189\000\
    \248\255\215\000\255\255\249\255\249\000\181\000\252\255\009\000\
    \063\000\075\000\234\000\251\255\032\001\250\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\013\000\013\000\016\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\016\000\016\000\016\000\
    \016\000\016\000\255\255\000\000\012\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\013\000\255\255\013\000\255\255\013\000\255\255\
    \255\255\255\255\255\255\001\000\255\255\255\255\255\255\008\000\
    \255\255\255\255\255\255\255\255\006\000\006\000\255\255\006\000\
    \001\000\002\000\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\020\000\000\000\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \042\000\000\000\000\000\255\255\000\000\047\000\000\000\047\000\
    \000\000\051\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\018\000\018\000\019\000\017\000\019\000\255\255\
    \048\000\019\000\255\255\057\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\000\000\003\000\000\000\000\000\019\000\000\000\000\000\
    \050\000\000\000\000\000\043\000\008\000\006\000\033\000\016\000\
    \004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\007\000\004\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\032\000\044\000\033\000\
    \056\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\021\000\057\000\000\000\000\000\000\000\
    \020\000\000\000\000\000\012\000\000\000\011\000\032\000\056\000\
    \000\000\025\000\049\000\000\000\000\000\032\000\014\000\024\000\
    \028\000\000\000\000\000\057\000\026\000\030\000\013\000\031\000\
    \000\000\000\000\022\000\027\000\015\000\029\000\023\000\000\000\
    \000\000\000\000\039\000\010\000\039\000\009\000\032\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\037\000\000\000\037\000\000\000\
    \035\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\255\255\
    \035\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\000\000\000\000\255\255\
    \000\000\056\000\000\000\000\000\055\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\054\000\
    \000\000\054\000\000\000\000\000\000\000\000\000\054\000\000\000\
    \002\000\041\000\000\000\000\000\000\000\255\255\046\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\000\000\000\000\000\000\000\000\
    \000\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\054\000\000\000\000\000\000\000\000\000\
    \000\000\054\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \000\000\000\000\000\000\000\000\000\000\054\000\000\000\000\000\
    \000\000\054\000\000\000\054\000\000\000\000\000\000\000\052\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \000\000\061\000\061\000\061\000\061\000\061\000\061\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\061\000\061\000\061\000\061\000\061\000\061\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\017\000\000\000\000\000\019\000\020\000\
    \045\000\019\000\020\000\055\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\019\000\255\255\255\255\
    \045\000\255\255\255\255\040\000\000\000\000\000\004\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\004\000\043\000\005\000\
    \056\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\016\000\057\000\255\255\255\255\255\255\
    \016\000\255\255\255\255\000\000\255\255\000\000\005\000\056\000\
    \255\255\014\000\045\000\255\255\255\255\004\000\000\000\023\000\
    \027\000\255\255\255\255\057\000\025\000\029\000\000\000\030\000\
    \255\255\255\255\015\000\026\000\000\000\013\000\022\000\255\255\
    \255\255\255\255\032\000\000\000\032\000\000\000\005\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\035\000\255\255\035\000\255\255\
    \034\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\047\000\
    \034\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\255\255\255\255\047\000\
    \255\255\049\000\255\255\255\255\049\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\049\000\
    \255\255\049\000\255\255\255\255\255\255\255\255\049\000\255\255\
    \000\000\040\000\255\255\255\255\255\255\020\000\045\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\047\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\255\255\255\255\255\255\255\255\
    \255\255\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\049\000\255\255\255\255\255\255\255\255\
    \255\255\049\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \255\255\255\255\255\255\255\255\255\255\049\000\255\255\255\255\
    \255\255\049\000\255\255\049\000\255\255\255\255\255\255\049\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \255\255\060\000\060\000\060\000\060\000\060\000\060\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\060\000\060\000\060\000\060\000\060\000\060\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\047\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\049\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec lex_json buf lexbuf =
    __ocaml_lex_lex_json_rec buf lexbuf 0
and __ocaml_lex_lex_json_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 137 "bsb/bsb_json.mll"
          ( lex_json buf lexbuf)
# 309 "bsb/bsb_json.ml"

  | 1 ->
# 138 "bsb/bsb_json.mll"
                   ( 
    update_loc lexbuf 0;
    lex_json buf  lexbuf
  )
# 317 "bsb/bsb_json.ml"

  | 2 ->
# 142 "bsb/bsb_json.mll"
                ( comment buf lexbuf)
# 322 "bsb/bsb_json.ml"

  | 3 ->
# 143 "bsb/bsb_json.mll"
         ( True)
# 327 "bsb/bsb_json.ml"

  | 4 ->
# 144 "bsb/bsb_json.mll"
          (False)
# 332 "bsb/bsb_json.ml"

  | 5 ->
# 145 "bsb/bsb_json.mll"
         (Null)
# 337 "bsb/bsb_json.ml"

  | 6 ->
# 146 "bsb/bsb_json.mll"
       (Lbracket)
# 342 "bsb/bsb_json.ml"

  | 7 ->
# 147 "bsb/bsb_json.mll"
       (Rbracket)
# 347 "bsb/bsb_json.ml"

  | 8 ->
# 148 "bsb/bsb_json.mll"
       (Lbrace)
# 352 "bsb/bsb_json.ml"

  | 9 ->
# 149 "bsb/bsb_json.mll"
       (Rbrace)
# 357 "bsb/bsb_json.ml"

  | 10 ->
# 150 "bsb/bsb_json.mll"
       (Comma)
# 362 "bsb/bsb_json.ml"

  | 11 ->
# 151 "bsb/bsb_json.mll"
        (Colon)
# 367 "bsb/bsb_json.ml"

  | 12 ->
# 152 "bsb/bsb_json.mll"
                      (lex_json buf lexbuf)
# 372 "bsb/bsb_json.ml"

  | 13 ->
# 154 "bsb/bsb_json.mll"
         ( Number (Lexing.lexeme lexbuf))
# 377 "bsb/bsb_json.ml"

  | 14 ->
# 156 "bsb/bsb_json.mll"
      (
  let pos = Lexing.lexeme_start_p lexbuf in
  scan_string buf pos lexbuf;
  let content = (Buffer.contents  buf) in 
  Buffer.clear buf ;
  String content 
)
# 388 "bsb/bsb_json.ml"

  | 15 ->
# 163 "bsb/bsb_json.mll"
       (Eof )
# 393 "bsb/bsb_json.ml"

  | 16 ->
let
# 164 "bsb/bsb_json.mll"
       c
# 399 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 164 "bsb/bsb_json.mll"
          ( error lexbuf (Illegal_character c ))
# 403 "bsb/bsb_json.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_lex_json_rec buf lexbuf __ocaml_lex_state

and comment buf lexbuf =
    __ocaml_lex_comment_rec buf lexbuf 40
and __ocaml_lex_comment_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 166 "bsb/bsb_json.mll"
              (lex_json buf lexbuf)
# 415 "bsb/bsb_json.ml"

  | 1 ->
# 167 "bsb/bsb_json.mll"
     (comment buf lexbuf)
# 420 "bsb/bsb_json.ml"

  | 2 ->
# 168 "bsb/bsb_json.mll"
       (error lexbuf Unterminated_comment)
# 425 "bsb/bsb_json.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_comment_rec buf lexbuf __ocaml_lex_state

and scan_string buf start lexbuf =
    __ocaml_lex_scan_string_rec buf start lexbuf 45
and __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 172 "bsb/bsb_json.mll"
      ( () )
# 437 "bsb/bsb_json.ml"

  | 1 ->
# 174 "bsb/bsb_json.mll"
  (
        let len = lexeme_len lexbuf - 2 in
        update_loc lexbuf len;

        scan_string buf start lexbuf
      )
# 447 "bsb/bsb_json.ml"

  | 2 ->
# 181 "bsb/bsb_json.mll"
      (
        let len = lexeme_len lexbuf - 3 in
        update_loc lexbuf len;
        scan_string buf start lexbuf
      )
# 456 "bsb/bsb_json.ml"

  | 3 ->
let
# 186 "bsb/bsb_json.mll"
                                               c
# 462 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 187 "bsb/bsb_json.mll"
      (
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf start lexbuf
      )
# 469 "bsb/bsb_json.ml"

  | 4 ->
let
# 191 "bsb/bsb_json.mll"
                 c1
# 475 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 191 "bsb/bsb_json.mll"
                               c2
# 480 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 191 "bsb/bsb_json.mll"
                                             c3
# 485 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and
# 191 "bsb/bsb_json.mll"
                                                    s
# 490 "bsb/bsb_json.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 4) in
# 192 "bsb/bsb_json.mll"
      (
        let v = dec_code c1 c2 c3 in
        if v > 255 then
          error lexbuf (Illegal_escape s) ;
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 501 "bsb/bsb_json.ml"

  | 5 ->
let
# 200 "bsb/bsb_json.mll"
                        c1
# 507 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 200 "bsb/bsb_json.mll"
                                         c2
# 512 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3) in
# 201 "bsb/bsb_json.mll"
      (
        let v = hex_code c1 c2 in
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 521 "bsb/bsb_json.ml"

  | 6 ->
let
# 207 "bsb/bsb_json.mll"
             c
# 527 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 208 "bsb/bsb_json.mll"
      (
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;

        scan_string buf start lexbuf
      )
# 536 "bsb/bsb_json.ml"

  | 7 ->
# 215 "bsb/bsb_json.mll"
      (
        update_loc lexbuf 0;
        Buffer.add_char buf lf;

        scan_string buf start lexbuf
      )
# 546 "bsb/bsb_json.ml"

  | 8 ->
# 222 "bsb/bsb_json.mll"
      (
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Buffer.add_substring buf lexbuf.lex_buffer ofs len;

        scan_string buf start lexbuf
      )
# 557 "bsb/bsb_json.ml"

  | 9 ->
# 230 "bsb/bsb_json.mll"
      (
        error lexbuf Unterminated_string
      )
# 564 "bsb/bsb_json.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state

;;

# 234 "bsb/bsb_json.mll"
 

type js_array =
  { content : t array ; 
    loc_start : Lexing.position ; 
    loc_end : Lexing.position ; 
  }
and js_str = 
  { str : string ; loc : Lexing.position}
and t = 
  [  
    `True
  | `False
  | `Null
  | `Flo of string 
  | `Str of js_str
  | `Arr  of js_array
  | `Obj of t String_map.t 
   ]

type status = 
  | No_path
  | Found  of t 
  | Wrong_type of path 



let rec parse_json lexbuf =
  let buf = Buffer.create 64 in 
  let look_ahead = ref None in
  let token () : token = 
    match !look_ahead with 
    | None ->  
      lex_json buf lexbuf 
    | Some x -> 
      look_ahead := None ;
      x 
  in
  let push e = look_ahead := Some e in 
  let rec json (lexbuf : Lexing.lexbuf) : t = 
    match token () with 
    | True -> `True
    | False -> `False
    | Null -> `Null
    | Number s ->  `Flo s 
    | String s -> `Str { str = s; loc =    lexbuf.lex_start_p}
    | Lbracket -> parse_array false lexbuf.lex_start_p lexbuf.lex_curr_p [] lexbuf
    | Lbrace -> parse_map false String_map.empty lexbuf
    |  _ -> error lexbuf Unexpected_token
  and parse_array  trailing_comma loc_start loc_finish acc lexbuf : t =
    match token () with 
    | Rbracket ->
      (* if trailing_comma then  *)
      (*   error lexbuf Trailing_comma_in_array *)
      (* else  *)
        `Arr {loc_start ; content = Ext_array.reverse_of_list acc ; 
              loc_end = lexbuf.lex_curr_p }
    | x -> 
      push x ;
      let new_one = json lexbuf in 
      begin match token ()  with 
      | Comma -> 
          parse_array true loc_start loc_finish (new_one :: acc) lexbuf 
      | Rbracket 
        -> `Arr {content = (Ext_array.reverse_of_list (new_one::acc));
                     loc_start ; 
                     loc_end = lexbuf.lex_curr_p }
      | _ -> 
        error lexbuf Expect_comma_or_rbracket
      end
  and parse_map trailing_comma acc lexbuf : t = 
    match token () with 
    | Rbrace -> 
      (* if trailing_comma then  *)
      (*   error lexbuf Trailing_comma_in_obj *)
      (* else  *)
        `Obj acc 
    | String key -> 
      begin match token () with 
      | Colon ->
        let value = json lexbuf in
        begin match token () with 
        | Rbrace -> `Obj (String_map.add key value acc )
        | Comma -> 
          parse_map true  (String_map.add key value acc) lexbuf 
        | _ -> error lexbuf Expect_comma_or_rbrace
        end
      | _ -> error lexbuf Expect_colon
      end
    | _ -> error lexbuf Expect_string_or_rbrace
  in 
  let v = json lexbuf in 
  match token () with 
  | Eof -> v 
  | _ -> error lexbuf Expect_eof

let parse_json_from_string s = 
  parse_json (Lexing.from_string s )

let parse_json_from_chan in_chan = 
  let lexbuf = Lexing.from_channel in_chan in 
  parse_json lexbuf 

let parse_json_from_file s = 
  let in_chan = open_in s in 
  let lexbuf = Lexing.from_channel in_chan in 
  match parse_json lexbuf with 
  | exception e -> close_in in_chan ; raise e
  | v  -> close_in in_chan;  v



type callback = 
  [
    `Str of (string -> unit) 
  | `Str_loc of (string -> Lexing.position -> unit)
  | `Flo of (string -> unit )
  | `Bool of (bool -> unit )
  | `Obj of (t String_map.t -> unit)
  | `Arr of (t array -> unit )
  | `Arr_loc of (t array -> Lexing.position -> Lexing.position -> unit)
  | `Null of (unit -> unit)
  ]

let test   ?(fail=(fun () -> ())) key 
    (cb : callback) m 
     =
     begin match String_map.find key m, cb with 
       | exception Not_found -> fail ()
       | `True, `Bool cb -> cb true
       | `False, `Bool cb  -> cb false 
       | `Flo s , `Flo cb  -> cb s 
       | `Obj b , `Obj cb -> cb b 
       | `Arr {content}, `Arr cb -> cb content 
       | `Arr {content; loc_start ; loc_end}, `Arr_loc cb -> 
         cb content  loc_start loc_end 
       | `Null, `Null cb  -> cb ()
       | `Str {str = s }, `Str cb  -> cb s 
       | `Str {str = s ; loc }, `Str_loc cb -> cb s loc 
       | _, _ -> fail () 
     end;
     m
let query path (json : t ) =
  let rec aux acc paths json =
    match path with 
    | [] ->  Found json
    | p :: rest -> 
      begin match json with 
        | `Obj m -> 
          begin match String_map.find p m with 
            | m' -> aux (p::acc) rest m'
            | exception Not_found ->  No_path
          end
        | _ -> Wrong_type acc 
      end
  in aux [] path json

# 729 "bsb/bsb_json.ml"

end
module Ounit_json_tests
= struct
#1 "ounit_json_tests.ml"

let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

open Bsb_json
let (|?)  m (key, cb) =
    m  |> Bsb_json.test key cb 

exception Parse_error 
let suites = 
  __FILE__ 
  >:::
  [
    "empty_json" >:: begin fun _ -> 
      let v =parse_json_from_string "{}" in
      match v with 
      | `Obj v -> OUnit.assert_equal (String_map.is_empty v ) true
      | _ -> OUnit.assert_failure "should be empty"
    end
    ;
    "empty_arr" >:: begin fun _ -> 
      let v =parse_json_from_string "[]" in
      match v with 
      | `Arr {content = [||]} -> ()
      | _ -> OUnit.assert_failure "should be empty"
    end
    ;
    "empty trails" >:: begin fun _ -> 
      (OUnit.assert_raises Parse_error @@ fun _ -> 
       try parse_json_from_string {| [,]|} with _ -> raise Parse_error);
      OUnit.assert_raises Parse_error @@ fun _ -> 
        try parse_json_from_string {| {,}|} with _ -> raise Parse_error
    end;
    "two trails" >:: begin fun _ -> 
      (OUnit.assert_raises Parse_error @@ fun _ -> 
       try parse_json_from_string {| [1,2,,]|} with _ -> raise Parse_error);
      (OUnit.assert_raises Parse_error @@ fun _ -> 
       try parse_json_from_string {| { "x": 3, ,}|} with _ -> raise Parse_error)
    end;

    "two trails fail" >:: begin fun _ -> 
      (OUnit.assert_raises Parse_error @@ fun _ -> 
       try parse_json_from_string {| { "x": 3, 2 ,}|} with _ -> raise Parse_error)
    end;

    "trail comma obj" >:: begin fun _ -> 
      let v =  parse_json_from_string {| { "x" : 3 , }|} in 
      let v1 =  parse_json_from_string {| { "x" : 3 , }|} in 
      let test v = 
        match v with 
        |`Obj v -> 
          v
          |? ("x" , `Flo (fun x -> OUnit.assert_equal x "3"))
          |> ignore 
        | _ -> OUnit.assert_failure "trail comma" in 
      test v ;
      test v1 
    end
    ;
    "trail comma arr" >:: begin fun _ -> 
      let v = parse_json_from_string {| [ 1, 3, ]|} in
      let v1 = parse_json_from_string {| [ 1, 3 ]|} in
      let test v = 
        match v with 
        | `Arr { content = [|`Flo "1" ; `Flo "3" |] } -> ()
        | _ -> OUnit.assert_failure "trailing comma array" in 
      test v ;
      test v1
    end
  ]

end
module Ext_pervasives : sig 
#1 "ext_pervasives.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








(** Extension to standard library [Pervavives] module, safe to open 
  *)

external reraise: exn -> 'a = "%reraise"

val finally : 'a -> ('a -> 'c) -> ('a -> 'b) -> 'b

val with_file_as_chan : string -> (out_channel -> 'a) -> 'a

val with_file_as_pp : string -> (Format.formatter -> 'a) -> 'a

val is_pos_pow : Int32.t -> int

val failwithf : loc:string -> ('a, unit, string, 'b) format4 -> 'a

val invalid_argf : ('a, unit, string, 'b) format4 -> 'a

val bad_argf : ('a, unit, string, 'b) format4 -> 'a



val dump : 'a -> string 

external id : 'a -> 'a = "%identity"

(** Copied from {!Btype.hash_variant}:
    need sync up and add test case
 *)
val hash_variant : string -> int

end = struct
#1 "ext_pervasives.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)






external reraise: exn -> 'a = "%reraise"

let finally v action f   = 
  match f v with
  | exception e -> 
      action v ;
      reraise e 
  | e ->  action v ; e 

let with_file_as_chan filename f = 
  finally (open_out_bin filename) close_out f 

let with_file_as_pp filename f = 
  finally (open_out_bin filename) close_out
    (fun chan -> 
      let fmt = Format.formatter_of_out_channel chan in
      let v = f  fmt in
      Format.pp_print_flush fmt ();
      v
    ) 


let  is_pos_pow n = 
  let module M = struct exception E end in 
  let rec aux c (n : Int32.t) = 
    if n <= 0l then -2 
    else if n = 1l then c 
    else if Int32.logand n 1l =  0l then   
      aux (c + 1) (Int32.shift_right n 1 )
    else raise M.E in 
  try aux 0 n  with M.E -> -1

let failwithf ~loc fmt = Format.ksprintf (fun s -> failwith (loc ^ s))
    fmt
    
let invalid_argf fmt = Format.ksprintf invalid_arg fmt

let bad_argf fmt = Format.ksprintf (fun x -> raise (Arg.Bad x ) ) fmt


let rec dump r =
  if Obj.is_int r then
    string_of_int (Obj.magic r : int)
  else (* Block. *)
    let rec get_fields acc = function
      | 0 -> acc
      | n -> let n = n-1 in get_fields (Obj.field r n :: acc) n
    in
    let rec is_list r =
      if Obj.is_int r then
        r = Obj.repr 0 (* [] *)
      else
        let s = Obj.size r and t = Obj.tag r in
        t = 0 && s = 2 && is_list (Obj.field r 1) (* h :: t *)
    in
    let rec get_list r =
      if Obj.is_int r then
        []
      else
        let h = Obj.field r 0 and t = get_list (Obj.field r 1) in
        h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible
       * in pure OCaml at the moment.  *)
      "<" ^ name ^ ">"
    in
    let s = Obj.size r and t = Obj.tag r in
    (* From the tag, determine the type of block. *)
    match t with
    | _ when is_list r ->
      let fields = get_list r in
      "[" ^ String.concat "; " (List.map dump fields) ^ "]"
    | 0 ->
      let fields = get_fields [] s in
      "(" ^ String.concat ", " (List.map dump fields) ^ ")"
    | x when x = Obj.lazy_tag ->
      (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
         * clear if very large constructed values could have the same
         * tag. XXX *)
      opaque "lazy"
    | x when x = Obj.closure_tag ->
      opaque "closure"
    | x when x = Obj.object_tag ->
      let fields = get_fields [] s in
      let _clasz, id, slots =
        match fields with
        | h::h'::t -> h, h', t
        | _ -> assert false
      in
      (* No information on decoding the class (first field).  So just print
         * out the ID and the slots. *)
      "Object #" ^ dump id ^ " (" ^ String.concat ", " (List.map dump slots) ^ ")"
    | x when x = Obj.infix_tag ->
      opaque "infix"
    | x when x = Obj.forward_tag ->
      opaque "forward"
    | x when x < Obj.no_scan_tag ->
      let fields = get_fields [] s in
      "Tag" ^ string_of_int t ^
      " (" ^ String.concat ", " (List.map dump fields) ^ ")"
    | x when x = Obj.string_tag ->
      "\"" ^ String.escaped (Obj.magic r : string) ^ "\""
    | x when x = Obj.double_tag ->
      string_of_float (Obj.magic r : float)
    | x when x = Obj.abstract_tag ->
      opaque "abstract"
    | x when x = Obj.custom_tag ->
      opaque "custom"
    | x when x = Obj.custom_tag ->
      opaque "final"
    | x when x = Obj.double_array_tag ->
      "[|"^
      String.concat ";"
        (Array.to_list (Array.map string_of_float (Obj.magic r : float array))) ^
      "|]"
    | _ ->
      opaque (Printf.sprintf "unknown: tag %d size %d" t s)

let dump v = dump (Obj.repr v)

external id : 'a -> 'a = "%identity"


let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu


end
module Literals : sig 
#1 "literals.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)






val js_array_ctor : string 
val js_type_number : string
val js_type_string : string
val js_type_object : string
val js_undefined : string
val js_prop_length : string

val param : string
val partial_arg : string
val prim : string

(**temporary varaible used in {!Js_ast_util} *)
val tmp : string 

val create : string 

val app : string
val app_array : string

val runtime : string
val stdlib : string
val imul : string

val setter_suffix : string
val setter_suffix_len : int


val js_debugger : string
val js_pure_expr : string
val js_pure_stmt : string
val js_unsafe_downgrade : string
val js_fn_run : string
val js_method_run : string
val js_fn_method : string
val js_fn_mk : string

(** callback actually, not exposed to user yet *)
val js_fn_runmethod : string 

val bs_deriving : string
val bs_deriving_dot : string
val bs_type : string

(** nodejs *)

val node_modules : string
val node_modules_length : int
val package_json : string
val bsconfig_json : string
val build_ninja : string
val suffix_cmj : string
val suffix_cmi : string
val suffix_ml : string
val suffix_mlast : string 
val suffix_mliast : string
val suffix_mll : string
val suffix_d : string
val suffix_mlastd : string
val suffix_mliastd : string
val suffix_js : string

end = struct
#1 "literals.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)







let js_array_ctor = "Array"
let js_type_number = "number"
let js_type_string = "string"
let js_type_object = "object" 
let js_undefined = "undefined"
let js_prop_length = "length"

let prim = "prim"
let param = "param"
let partial_arg = "partial_arg"
let tmp = "tmp"

let create = "create" (* {!Caml_exceptions.create}*)

let app = "_"
let app_array = "app" (* arguments are an array*)

let runtime = "runtime" (* runtime directory *)

let stdlib = "stdlib"

let imul = "imul" (* signed int32 mul *)

let setter_suffix = "#="
let setter_suffix_len = String.length setter_suffix

let js_debugger = "js_debugger"
let js_pure_expr = "js_pure_expr"
let js_pure_stmt = "js_pure_stmt"
let js_unsafe_downgrade = "js_unsafe_downgrade"
let js_fn_run = "js_fn_run"
let js_method_run = "js_method_run"

let js_fn_method = "js_fn_method"
let js_fn_mk = "js_fn_mk"
let js_fn_runmethod = "js_fn_runmethod"

let bs_deriving = "bs.deriving"
let bs_deriving_dot = "bs.deriving."
let bs_type = "bs.type"


(** nodejs *)
let node_modules = "node_modules"
let node_modules_length = String.length "node_modules"
let package_json = "package.json"
let bsconfig_json = "bsconfig.json"
let build_ninja = "build.ninja"

let suffix_cmj = ".cmj"
let suffix_cmi = ".cmi"
let suffix_mll = ".mll"
let suffix_ml = ".ml"
let suffix_mlast = ".mlast"
let suffix_mliast = ".mliast"
let suffix_d = ".d"
let suffix_mlastd = ".mlast.d"
let suffix_mliastd = ".mliast.d"
let suffix_js = ".js"


end
module Ext_filename : sig 
#1 "ext_filename.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)





(* TODO:
   Change the module name, this code is not really an extension of the standard 
    library but rather specific to JS Module name convention. 
*)

type t = 
  [ `File of string 
  | `Dir of string ]

val combine : string -> string -> string 
val path_as_directory : string -> string

(** An extension module to calculate relative path follow node/npm style. 
    TODO : this short name will have to change upon renaming the file.
 *)

(** Js_output is node style, which means 
    separator is only '/'

    if the path contains 'node_modules', 
    [node_relative_path] will discard its prefix and 
    just treat it as a library instead
 *)

val node_relative_path : t -> [`File of string] -> string

val chop_extension : ?loc:string -> string -> string






val cwd : string Lazy.t

(* It is lazy so that it will not hit errors when in script mode *)
val package_dir : string Lazy.t

val replace_backward_slash : string -> string

val module_name_of_file : string -> string

val chop_extension_if_any : string -> string

val absolute_path : string -> string

val module_name_of_file_if_any : string -> string

(**
   1. add some simplifications when concatenating
   2. when the second one is absolute, drop the first one
*)
val combine : string -> string -> string

val normalize_absolute_path : string -> string

(** 
TODO: could be highly optimized
if [from] and [to] resolve to the same path, a zero-length string is returned 
Given that two paths are directory

A typical use case is 
{[
Filename.concat 
  (rel_normalized_absolute_path cwd (Filename.dirname a))
  (Filename.basename a)
]}
*)
val rel_normalized_absolute_path : string -> string -> string 



(**
{[
get_extension "a.txt" = ".txt"
get_extension "a" = ""
]}
*)
val get_extension : string -> string

val replace_backward_slash : string -> string


val no_slash : string -> int -> int -> bool
(** if no conversion happens, reference equality holds *)
val replace_slash_backward : string -> string 

end = struct
#1 "ext_filename.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








(** Used when produce node compatible paths *)
let node_sep = "/"
let node_parent = ".."
let node_current = "."

type t = 
  [ `File of string 
  | `Dir of string ]

let cwd = lazy (Sys.getcwd ())

let (//) = Filename.concat 

let combine path1 path2 =
  if path1 = "" then
    path2
  else if path2 = "" then path1
  else 
  if Filename.is_relative path2 then
     path1// path2 
  else
    path2

(* Note that [.//] is the same as [./] *)
let path_as_directory x =
  if x = "" then x
  else
  if Ext_string.ends_with x  Filename.dir_sep then
    x 
  else 
    x ^ Filename.dir_sep

let absolute_path s = 
  let process s = 
    let s = 
      if Filename.is_relative s then
        Lazy.force cwd // s 
      else s in
    (* Now simplify . and .. components *)
    let rec aux s =
      let base,dir  = Filename.basename s, Filename.dirname s  in
      if dir = s then dir
      else if base = Filename.current_dir_name then aux dir
      else if base = Filename.parent_dir_name then Filename.dirname (aux dir)
      else aux dir // base
    in aux s  in 
  process s 


let chop_extension ?(loc="") name =
  try Filename.chop_extension name 
  with Invalid_argument _ -> 
    Ext_pervasives.invalid_argf 
      "Filename.chop_extension ( %s : %s )"  loc name

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname





let os_path_separator_char = String.unsafe_get Filename.dir_sep 0 


(** example
    {[
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
    ]}

    The other way
    {[
    
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
    ]}
    {[
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib//ocaml_array.ml"
    ]}
    {[
    /a/b
    /c/d
    ]}
 *)
let relative_path file_or_dir_1 file_or_dir_2 = 
  let sep_char = os_path_separator_char in
  let relevant_dir1 = 
    (match file_or_dir_1 with 
    | `Dir x -> x 
    | `File file1 ->  Filename.dirname file1) in
  let relevant_dir2 = 
    (match file_or_dir_2 with 
    |`Dir x -> x 
    |`File file2 -> Filename.dirname file2 ) in
  let dir1 = Ext_string.split relevant_dir1 sep_char   in
  let dir2 = Ext_string.split relevant_dir2 sep_char  in
  let rec go (dir1 : string list) (dir2 : string list) = 
    match dir1, dir2 with 
    | x::xs , y :: ys when x = y
      -> go xs ys 
    | _, _
      -> 
        List.map (fun _ -> node_parent) dir2 @ dir1 
  in
  match go dir1 dir2 with
  | (x :: _ ) as ys when x = node_parent -> 
      String.concat node_sep ys
  | ys -> 
      String.concat node_sep  @@ node_current :: ys


(** path2: a/b 
    path1: a 
    result:  ./b 
    TODO: [Filename.concat] with care

    [file1] is currently compilation file 
    [file2] is the dependency
 *)
let node_relative_path (file1 : t) 
    (`File file2 as dep_file : [`File of string]) = 
  let v = Ext_string.find  file2 ~sub:Literals.node_modules in 
  let len = String.length file2 in 
  if v >= 0 then
    let rec skip  i =       
      if i >= len then
        Ext_pervasives.failwithf ~loc:__LOC__ "invalid path: %s"  file2
      else 
        (* https://en.wikipedia.org/wiki/Path_(computing))
           most path separator are a single char 
        *)
        let curr_char = String.unsafe_get file2 i  in 
        if curr_char = os_path_separator_char || curr_char = '.' then 
          skip (i + 1) 
        else i
        (*
          TODO: we need do more than this suppose user 
          input can be
           {[
           "xxxghsoghos/ghsoghso/node_modules/../buckle-stdlib/list.js"
           ]}
           This seems weird though
        *)
    in 
    Ext_string.tail_from file2
      (skip (v + Literals.node_modules_length)) 
  else 
    relative_path 
      (  match dep_file with 
         | `File x -> `File (absolute_path x)
         | `Dir x -> `Dir (absolute_path x))

       (match file1 with 
         | `File x -> `File (absolute_path x)
         | `Dir x -> `Dir(absolute_path x))
     ^ node_sep ^
    chop_extension_if_any (Filename.basename file2)





let find_package_json_dir cwd  = 
  let rec aux cwd  = 
    if Sys.file_exists (cwd // Literals.package_json) then cwd
    else 
      let cwd' = Filename.dirname cwd in 
      if String.length cwd' < String.length cwd then  
        aux cwd'
      else 
        Ext_pervasives.failwithf 
          ~loc:__LOC__
            "package.json not found from %s" cwd
  in
  aux cwd 

let package_dir = lazy (find_package_json_dir (Lazy.force cwd))

let replace_backward_slash (x : string)= 
  String.map (function 
    |'\\'-> '/'
    | x -> x) x


let rec no_slash x i len = 
  i >= len  || 
  (String.unsafe_get x i <> '/' && no_slash x (i + 1)  len)

let replace_slash_backward (x : string ) = 
  let len = String.length x in 
  if no_slash x 0 len then x 
  else 
    String.map (function 
        | '/' -> '\\'
        | x -> x ) x 

let module_name_of_file file =
    String.capitalize 
      (Filename.chop_extension @@ Filename.basename file)  

let module_name_of_file_if_any file = 
    String.capitalize 
      (chop_extension_if_any @@ Filename.basename file)  


(** For win32 or case insensitve OS 
    [".cmj"] is the same as [".CMJ"]
  *)
(* let has_exact_suffix_then_chop fname suf =  *)
  
let combine p1 p2 = 
  if p1 = "" || p1 = Filename.current_dir_name then p2 else 
  if p2 = "" || p2 = Filename.current_dir_name then p1 
  else 
  if Filename.is_relative p2 then 
    Filename.concat p1 p2 
  else p2 



(**
{[
split_aux "//ghosg//ghsogh/";;
- : string * string list = ("/", ["ghosg"; "ghsogh"])
]}
*)
let split_aux p =
  let rec go p acc =
    let dir = Filename.dirname p in
    if dir = p then dir, acc
    else go dir (Filename.basename p :: acc)
  in go p []

(** 
TODO: optimization
if [from] and [to] resolve to the same path, a zero-length string is returned 
*)
let rel_normalized_absolute_path from to_ =
  let root1, paths1 = split_aux from in 
  let root2, paths2 = split_aux to_ in 
  if root1 <> root2 then root2 else
    let rec go xss yss =
      match xss, yss with 
      | x::xs, y::ys -> 
        if x = y then go xs ys 
        else 
          let start = 
            List.fold_left (fun acc _ -> acc // ".." ) ".." xs in 
          List.fold_left (fun acc v -> acc // v) start yss
      | [], [] -> ""
      | [], y::ys -> List.fold_left (fun acc x -> acc // x) y ys
      | x::xs, [] ->
        List.fold_left (fun acc _ -> acc // ".." ) ".." xs in
    go paths1 paths2

(*TODO: could be hgighly optimized later 
{[
  normalize_absolute_path "/gsho/./..";;

  normalize_absolute_path "/a/b/../c../d/e/f";;

  normalize_absolute_path "/gsho/./..";;

  normalize_absolute_path "/gsho/./../..";;

  normalize_absolute_path "/a/b/c/d";;

  normalize_absolute_path "/a/b/c/d/";;

  normalize_absolute_path "/a/";;

  normalize_absolute_path "/a";;
]}
*)
let normalize_absolute_path x =
  let drop_if_exist xs =
    match xs with 
    | [] -> []
    | _ :: xs -> xs in 
  let rec normalize_list acc paths =
    match paths with 
    | [] -> acc 
    | "." :: xs -> normalize_list acc xs
    | ".." :: xs -> 
      normalize_list (drop_if_exist acc ) xs 
    | x :: xs -> 
      normalize_list (x::acc) xs 
  in
  let root, paths = split_aux x in
  let rev_paths =  normalize_list [] paths in 
  let rec go acc rev_paths =
    match rev_paths with 
    | [] -> Filename.concat root acc 
    | last::rest ->  go (Filename.concat last acc ) rest  in 
  match rev_paths with 
  | [] -> root 
  | last :: rest -> go last rest 


let get_extension x =
  try
    let pos = String.rindex x '.' in
    Ext_string.tail_from x pos
  with Not_found -> ""



end
module Ounit_path_tests
= struct
#1 "ounit_path_tests.ml"
let ((>::),
    (>:::)) = OUnit.((>::),(>:::))


let normalize = Ext_filename.normalize_absolute_path
let (=~) x y = 
  OUnit.assert_equal ~cmp:(fun x y ->   String.compare x y = 0) x y
    
let suites = 
  __FILE__ 
  >:::
  [
    "linux path tests" >:: begin fun _ -> 
      let norm = 
        Array.map normalize
          [|
            "/gsho/./..";
            "/a/b/../c../d/e/f";
            "/a/b/../c/../d/e/f";
            "/gsho/./../..";
            "/a/b/c/d";
            "/a/b/c/d/";
            "/a/";
            "/a";
            "/a.txt/";
            "/a.txt"
          |] in 
      OUnit.assert_equal norm 
        [|
          "/";
          "/a/c../d/e/f";
          "/a/d/e/f";
          "/";
          "/a/b/c/d" ;
          "/a/b/c/d";
          "/a";
          "/a";
          "/a.txt";
          "/a.txt"
        |]
    end;
    __LOC__ >:: begin fun _ ->
      normalize "/./a/.////////j/k//../////..///././b/./c/d/./." =~ "/a/b/c/d"
    end;
    __LOC__ >:: begin fun _ -> 
      normalize "/./a/.////////j/k//../////..///././b/./c/d/././../" =~ "/a/b/c"
    end
  ]

end
module Resize_array : sig 
#1 "resize_array.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

module type ResizeType = 
sig 
  type t 
  val null : t (* used to populate new allocated array checkout {!Obj.new_block} for more performance *)
end


module type S = 
sig 
  type elt 
  type t
  val length : t -> int 
  val compact : t -> unit
  val empty : unit -> t 
  val make : int -> t 
  val init : int -> (int -> elt) -> t
  val is_empty : t -> bool
  val of_array : elt array -> t
  val of_sub_array : elt array -> int -> int -> t
  
  (** Exposed for some APIs which only take array as input, 
      when exposed   
  *)
  val unsafe_internal_array : t -> elt array
  val reserve : t -> int -> unit
  val push : t -> elt -> unit
  val delete : t -> int -> unit 
  val pop : t -> unit
  val delete_range : t -> int -> int -> unit 
  val clear : t -> unit 
  val reset : t -> unit 
  val to_list : t -> elt list 
  val of_list : elt list -> t
  val to_array : t -> elt array 
  val of_array : elt array -> t
  val copy : t -> t 
  val iter : (elt -> unit) -> t -> unit 
  val iteri : (int -> elt -> unit ) -> t -> unit 
  val iter_range : int -> int -> (elt -> unit) -> t -> unit 
  val iteri_range : int -> int -> (int -> elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t ->  t
  val mapi : (int -> elt -> elt) -> t -> t
  val fold_left : ('f -> elt -> 'f) -> 'f -> t -> 'f
  val fold_right : (elt -> 'g -> 'g) -> t -> 'g -> 'g
  val filter : (elt -> bool) -> t -> t
  val inplace_filter : (elt -> bool) -> t -> unit
  val equal : (elt -> elt -> bool) -> t -> t -> bool 
  val get : t -> int -> elt
  val last : t -> elt
  val capacity : t -> int
end
module Make ( Resize : ResizeType) : S with type elt = Resize.t 



end = struct
#1 "resize_array.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


external unsafe_blit :
  'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"

module type ResizeType = 
sig 
  type t 
  val null : t (* used to populate new allocated array checkout {!Obj.new_block} for more performance *)
end

module type S = 
sig 
  type elt 
  type t
  val length : t -> int 
  val compact : t -> unit
  val empty : unit -> t 
  val make : int -> t 
  val init : int -> (int -> elt) -> t
  val is_empty : t -> bool
  val of_array : elt array -> t
  val of_sub_array : elt array -> int -> int -> t
  
  (** Exposed for some APIs which only take array as input, 
      when exposed   
  *)
  val unsafe_internal_array : t -> elt array
  val reserve : t -> int -> unit
  val push : t -> elt -> unit
  val delete : t -> int -> unit 
  val pop : t -> unit
  val delete_range : t -> int -> int -> unit 
  val clear : t -> unit 
  val reset : t -> unit 
  val to_list : t -> elt list 
  val of_list : elt list -> t
  val to_array : t -> elt array 
  val of_array : elt array -> t
  val copy : t -> t 
  val iter : (elt -> unit) -> t -> unit 
  val iteri : (int -> elt -> unit ) -> t -> unit 
  val iter_range : int -> int -> (elt -> unit) -> t -> unit 
  val iteri_range : int -> int -> (int -> elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t ->  t
  val mapi : (int -> elt -> elt) -> t -> t
  val fold_left : ('f -> elt -> 'f) -> 'f -> t -> 'f
  val fold_right : (elt -> 'g -> 'g) -> t -> 'g -> 'g
  val filter : (elt -> bool) -> t -> t
  val inplace_filter : (elt -> bool) -> t -> unit
  val equal : (elt -> elt -> bool) -> t -> t -> bool 
  val get : t -> int -> elt
  val last : t -> elt
  val capacity : t -> int
end

module Make ( Resize : ResizeType) = struct
  type elt = Resize.t 

  type t = {
    mutable arr : elt array; (* changed when resizing*)
    mutable len : int;
  }

  let length d = d.len

  let compact d =
    let d_arr = d.arr in 
    if d.len <> Array.length d_arr then 
      begin
        let newarr = Array.sub d_arr 0 d.len in 
        d.arr <- newarr
      end

  let empty () =
    {
      len = 0;
      arr = [||];
    }

  let make initsize =
    if initsize < 0 then invalid_arg  "Resize_array.make" ;
    {

      len = 0;
      arr = Array.make  initsize Resize.null ;
    }

  let init len f =
    if len < 0 then invalid_arg  "Resize_array.init";
    let arr = Array.make  len Resize.null in
    for i = 0 to len - 1 do
      Array.unsafe_set arr i (f i)
    done;
    {

      len ;
      arr 
    }

  let is_empty d =
    d.len = 0

  let reserve d s = 
    let d_len = d.len in 
    let d_arr = d.arr in 
    if s < d_len || s < Array.length d_arr then ()
    else 
      let new_capacity = min Sys.max_array_length s in 
      let new_d_arr = Array.make new_capacity Resize.null in 
      unsafe_blit d_arr 0 new_d_arr 0 d_len;
      d.arr <- new_d_arr 

  let push d v =
    let d_len = d.len in
    let d_arr = d.arr in 
    let d_arr_len = Array.length d_arr in
    if d_arr_len = 0 then
      begin 
        d.len <- 1 ;
        d.arr <- [| v |]
      end
    else  
      begin 
        if d_len = d_arr_len then 
          begin
            if d_len >= Sys.max_array_length then 
              failwith "exceeds max_array_length";
            let new_capacity = min Sys.max_array_length d_len * 2 
            (* [d_len] can not be zero, so [*2] will enlarge   *)
            in
            let new_d_arr = Array.make new_capacity Resize.null in 
            d.arr <- new_d_arr;
            unsafe_blit d_arr 0 new_d_arr 0 d_len ;
          end;
        d.len <- d_len + 1;
        Array.unsafe_set d.arr d_len v
      end

  let delete d idx =
    if idx < 0 || idx >= d.len then invalid_arg "Resize_array.delete" ;
    let arr = d.arr in 
    unsafe_blit arr (idx + 1) arr idx  (d.len - idx - 1);
    Array.unsafe_set arr (d.len - 1) Resize.null;
    d.len <- d.len - 1

  let pop d = 
    let idx  = d.len - 1  in
    if idx < 0 then invalid_arg "Resize_array.pop";
    Array.unsafe_set d.arr idx Resize.null;
    d.len <- idx
             
  let delete_range d idx len =
    if len < 0 || idx < 0 || idx + len > d.len then invalid_arg  "Resize_array.delete_range"  ;
    let arr = d.arr in 
    unsafe_blit arr (idx + len) arr idx (d.len  - idx - len);
    for i = d.len - len to d.len - 1 do
      Array.unsafe_set d.arr i Resize.null
    done;
    d.len <- d.len - len



(** Below are simple wrapper around normal Array operations *)  

  let clear d =
    for i = 0 to d.len - 1 do 
      Array.unsafe_set d.arr i Resize.null
    done;
    d.len <- 0

  let reset d = 
    d.len <- 0; 
    d.arr <- [||]

  
  (* For [to_*] operations, we should be careful to call {!Array.*} function 
     in case we operate on the whole array
  *)
  let to_list d =
    let rec loop d_arr idx accum =
      if idx < 0 then accum else loop d_arr (idx - 1) (Array.unsafe_get d_arr idx :: accum)
    in
    loop d.arr (d.len - 1) []


  let of_list lst =
    let arr = Array.of_list lst in 
    { arr ; len = Array.length arr}

  (* TODO *)
  (* let append_array arr =  *)
    
  let to_array d = 
    Array.sub d.arr 0 d.len

  let of_array src =
    {
      len = Array.length src;
      arr = Array.copy src;
      (* okay to call {!Array.copy}*)
    }
  let of_sub_array arr off len = 
    { 
      len = len ; 
      arr = Array.sub arr off len  
    }  
  let unsafe_internal_array v = v.arr  
  (* we can not call {!Array.copy} *)
  let copy src =
    let len = src.len in
    {
      len ;
      arr = Array.sub src.arr 0 len ;
    }

  let sub src start len =
    { len ; 
      arr = Array.sub src.arr start len }

  let iter f d = 
    let arr = d.arr in 
    for i = 0 to d.len - 1 do
      f (Array.unsafe_get arr i)
    done

  let iteri f d =
    let arr = d.arr in
    for i = 0 to d.len - 1 do
      f i (Array.unsafe_get arr i)
    done

  let iter_range from to_ f d =
    if from < 0 || to_ >= d.len then invalid_arg "Resize_array.iter_range"
    else 
      let d_arr = d.arr in 
      for i = from to to_ do 
        f  (Array.unsafe_get d_arr i)
      done

  let iteri_range from to_ f d =
    if from < 0 || to_ >= d.len then invalid_arg "Resize_array.iteri_range"
    else 
      let d_arr = d.arr in 
      for i = from to to_ do 
        f i (Array.unsafe_get d_arr i)
      done
    
  let map f src =
    let src_len = src.len in 
    let arr = Array.make  src_len Resize.null in
    let src_arr = src.arr in 
    for i = 0 to src_len - 1 do
      Array.unsafe_set arr i (f (Array.unsafe_get src_arr i))
    done;
    {
      len = src_len;
      arr = arr;
    }

  let mapi f src =
    let len = src.len in 
    if len = 0 then { len ; arr = [| |] }
    else 
      let src_arr = src.arr in 
      let arr = Array.make len (Array.unsafe_get src_arr 0) in
      for i = 1 to len - 1 do
        Array.unsafe_set arr i (f i (Array.unsafe_get src_arr i))
      done;
      {
        len ;
        arr ;
      }

  let fold_left f x a =
    let rec loop a_len a_arr idx x =
      if idx >= a_len then x else 
        loop a_len a_arr (idx + 1) (f x (Array.unsafe_get a_arr idx))
    in
    loop a.len a.arr 0 x

  let fold_right f a x =
    let rec loop a_arr idx x =
      if idx < 0 then x
      else loop a_arr (idx - 1) (f (Array.unsafe_get a_arr idx) x)
    in
    loop a.arr (a.len - 1) x

(**  
   [filter] and [inplace_filter]
*)
  let filter f d =
    let new_d = copy d in 
    let new_d_arr = new_d.arr in 
    let d_arr = d.arr in
    let p = ref 0 in
    for i = 0 to d.len  - 1 do
      let x = Array.unsafe_get d_arr i in
      (* TODO: can be optimized for segments blit *)
      if f x  then
        begin
          Array.unsafe_set new_d_arr !p x;
          incr p;
        end;
    done;
    new_d.len <- !p;
    new_d 

  let inplace_filter f d = 
    let d_arr = d.arr in 
    let p = ref 0 in
    for i = 0 to d.len - 1 do 
      let x = Array.unsafe_get d_arr i in 
      if f x then 
        begin 
          let curr_p = !p in 
          (if curr_p <> i then 
            Array.unsafe_set d_arr curr_p x) ;
          incr p
        end
    done ;
    let last = !p  in 
    delete_range d last  (d.len - last)


  let equal eq x y : bool = 
    if x.len <> y.len then false 
    else 
      let rec aux x_arr y_arr i =
        if i < 0 then true else  
        if eq (Array.unsafe_get x_arr i) (Array.unsafe_get y_arr i) then 
          aux x_arr y_arr (i - 1)
        else false in 
      aux x.arr y.arr (x.len - 1)

  let get d i = 
    if i < 0 || i >= d.len then invalid_arg "Resize_array.get"
    else Array.unsafe_get d.arr i

  let last d = 
    if d.len <= 0 then invalid_arg   "Resize_array.last"
    else Array.unsafe_get d.arr (d.len - 1)

  let capacity d = Array.length d.arr
end

end
module Int_vec : sig 
#1 "int_vec.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

include Resize_array.S with type elt = int
end = struct
#1 "int_vec.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


include Resize_array.Make(struct type t = int let null = 0 end)
end
module Ext_scc : sig 
#1 "ext_scc.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
 



type  node = {
  mutable index : int;
  mutable lowlink : int ;
  (* mutable onstack : bool; *)
  data : int ;
  next : Int_vec.t ;    

}

(** Assume input is int array with offset from 0 
    Typical input 
    {[
      [|
        [|0; 1 ; 2 |]; 
        [|1 ; |]; 
        [|2|]
      |]
    ]}
    Note that we can tell how many nodes by calculating 
    [Array.length] of the input 
*)
val graph : node array -> Int_vec.t Queue.t

val graph_check : node array -> int * int list 

end = struct
#1 "ext_scc.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
 
type elt = int


type  node = {
  mutable index : int;
  mutable lowlink : int ;
  (* mutable onstack : bool; *)
  data : elt ;
  next : Int_vec.t ;    

}

(** 
   [int] as data for this algorithm
   Pros:
   1. Easy to eoncode algorithm (especially given that the capacity of node is known)
   2. Algorithms itself are much more efficient
   3. Node comparison semantics is clear
   4. Easy to print output
   Cons:
   1. post processing input data  
 *)
let min_int (x : int) y = if x < y then x else y  

let graph  e =
  let index = ref 0 in 
  let s = Stack.create () in

  (* collect output *)
  let output = Queue.create () in 
  let node_numes = Array.length e in 
  let on_stack_array = Array.make node_numes false in
  let rec scc v  =
    let new_index = !index + 1 in 
    index := new_index ;
    Stack.push v.data s; 
    v.index <- new_index ;
    v.lowlink <- new_index ;
    on_stack_array. (v.data) <- true ; 
    (* v.onstack <- true; *)

    v.next 
    |> Int_vec.iter (fun w  ->
        let w = e.(w) in 
        if w.index < 0 then
          begin  
            scc w;
            v.lowlink <- min_int v.lowlink w.lowlink
          end  
        else if (* w.onstack *) on_stack_array.(w.data) then 
          v.lowlink <- min_int v.lowlink w.lowlink 
      ) ; 
    if v.lowlink = v.index then
      begin
        (* TODO: if we use stack as vector we can do batch update here *)
        let curr_vec = Int_vec.make 4 in 

        let curr_ele = Stack.pop s in
        let curr = ref curr_ele in

        on_stack_array.(curr_ele) <- false;
        Int_vec.push curr_vec  curr_ele; 

        while !curr != v.data do
          let curr_ele = Stack.pop s in
          curr :=  curr_ele ;

          on_stack_array.(curr_ele) <- false ; 
          Int_vec.push curr_vec curr_ele
        done;
        Queue.push curr_vec  output
      end   
  in

  Array.iter (fun v -> if v.index < 0 then scc v) e;
  output 

let graph_check v = 
  let v = graph v in 
  Queue.length v, 
  Queue.fold (fun acc x -> Int_vec.length x :: acc ) [] v  

end
module Ounit_scc_tests
= struct
#1 "ounit_scc_tests.ml"
let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal

let tiny_test_cases = {|
13
22
 4  2
 2  3
 3  2
 6  0
 0  1
 2  0
11 12
12  9
 9 10
 9 11
 7  9
10 12
11  4
 4  3
 3  5
 6  8
 8  6
 5  4
 0  5
 6  4
 6  9
 7  6
|}     

let medium_test_cases = {|
50
147
 0  7
 0 34
 1 14
 1 45
 1 21
 1 22
 1 22
 1 49
 2 19
 2 25
 2 33
 3  4
 3 17
 3 27
 3 36
 3 42
 4 17
 4 17
 4 27
 5 43
 6 13
 6 13
 6 28
 6 28
 7 41
 7 44
 8 19
 8 48
 9  9
 9 11
 9 30
 9 46
10  0
10  7
10 28
10 28
10 28
10 29
10 29
10 34
10 41
11 21
11 30
12  9
12 11
12 21
12 21
12 26
13 22
13 23
13 47
14  8
14 21
14 48
15  8
15 34
15 49
16  9
17 20
17 24
17 38
18  6
18 28
18 32
18 42
19 15
19 40
20  3
20 35
20 38
20 46
22  6
23 11
23 21
23 22
24  4
24  5
24 38
24 43
25  2
25 34
26  9
26 12
26 16
27  5
27 24
27 32
27 31
27 42
28 22
28 29
28 39
28 44
29 22
29 49
30 23
30 37
31 18
31 32
32  5
32  6
32 13
32 37
32 47
33  2
33  8
33 19
34  2 
34 19
34 40
35  9
35 37
35 46
36 20
36 42
37  5
37  9
37 35
37 47
37 47
38 35
38 37
38 38
39 18
39 42
40 15
41 28
41 44
42 31
43 37
43 38
44 39
45  8
45 14
45 14
45 15
45 49
46 16
47 23
47 30
48 12
48 21
48 33
48 33
49 34
49 22
49 49
|}
(* 
reference output: 
http://algs4.cs.princeton.edu/42digraph/KosarajuSharirSCC.java.html 
*)

let handle_lines tiny_test_cases = 
  match Ext_string.split  tiny_test_cases '\n' with 
  | nodes :: edges :: rest -> 
    let nodes_num = int_of_string nodes in 
    let node_array = 
      Array.init nodes_num
        (fun i -> {Ext_scc.data = i ; index = -1; lowlink = -1; next = Int_vec.empty ()})
    in 
    begin 
      rest |> List.iter (fun x ->
          match Ext_string.split x ' ' with 
          | [ a ; b] -> 
            let a , b = int_of_string a , int_of_string b in 
            Int_vec.push node_array.(a).next b 
          | _ -> assert false 
        );
      node_array 
    end
  | _ -> assert false



let test  (input : (string * string list) list) = 
  (* string -> int mapping 
  *)
  let tbl = Hashtbl.create 32 in
  let idx = ref 0 in 
  let add x =
    if not (Hashtbl.mem tbl x ) then 
      begin 
        Hashtbl.add  tbl x !idx ;
        incr idx 
      end in
  input |> List.iter 
    (fun (x,others) -> List.iter add (x::others));
  let nodes_num = Hashtbl.length tbl in
  let node_array = 
      Array.init nodes_num
        (fun i -> {Ext_scc.data = i ; index = -1; lowlink = -1;  next = Int_vec.empty ()}) in 
  input |> 
  List.iter (fun (x,others) -> 
      let idx = Hashtbl.find tbl  x  in 
      others |> 
      List.iter (fun y -> Int_vec.push node_array.(idx).next (Hashtbl.find tbl y ))
    ) ; 
  Ext_scc.graph_check node_array 


let suites = 
    __FILE__
    >::: [
      __LOC__ >:: begin fun _ -> 
        OUnit.assert_equal (fst @@ Ext_scc.graph_check (handle_lines tiny_test_cases))  5
      end       ;
      __LOC__ >:: begin fun _ -> 
        OUnit.assert_equal (fst @@ Ext_scc.graph_check (handle_lines medium_test_cases))  10
      end       ;
      __LOC__ >:: begin fun _ ->
        OUnit.assert_equal (test [
            "a", ["b" ; "c"];
            "b" , ["c" ; "d"];
            "c", [ "b"];
            "d", [];
          ]) (3 , [1;2;1])
      end ; 
      __LOC__ >:: begin fun _ ->
        OUnit.assert_equal (test [
            "a", ["b" ; "c"];
            "b" , ["c" ; "d"];
            "c", [ "b"];
            "d", [];
            "e", []
          ])  (4, [1;1;2;1])
      end ;
      __LOC__ >:: begin fun _ ->
        OUnit.assert_equal (test [
            "a", ["b" ; "c"];
            "b" , ["c" ; "d"];
            "c", [ "b"];
            "d", ["e"];
            "e", []
          ]) (4 , [1;2;1;1])
      end ; 
      __LOC__ >:: begin fun _ ->
        OUnit.assert_equal (test [
            "a", ["b" ; "c"];
            "b" , ["c" ; "d"];
            "c", [ "b"];
            "d", ["e"];
            "e", ["c"]
          ]) (2, [1;4])
      end ;
      __LOC__ >:: begin fun _ ->
        OUnit.assert_equal (test [
            "a", ["b" ; "c"];
            "b" , ["c" ; "d"];
            "c", [ "b"];
            "d", ["e"];
            "e", ["a"]
          ]) (1, [5])
      end ; 
      __LOC__ >:: begin fun _ ->
        OUnit.assert_equal (test [
            "a", ["b"];
            "b" , ["c" ];
            "c", [ ];
            "d", [];
            "e", []
          ]) (5, [1;1;1;1;1])
      end ; 
      __LOC__ >:: begin fun _ ->
        OUnit.assert_equal (test [
            "1", ["0"];
            "0" , ["2" ];
            "2", ["1" ];
            "0", ["3"];
            "3", [ "4"]
          ]) (3, [3;1;1])
      end ; 

    ]

end
module Ounit_vec_test
= struct
#1 "ounit_vec_test.ml"
let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

open Bsb_json

let v = Int_vec.init 10 (fun i -> i);;
let (=~) x y = OUnit.assert_equal ~cmp:(Int_vec.equal  (fun (x: int) y -> x=y)) x y
let (=~~) x y 
  = 
  OUnit.assert_equal ~cmp:(Int_vec.equal  (fun (x: int) y -> x=y)) x (Int_vec.of_array y) 

let suites = 
  __FILE__ 
  >:::
  [
    "inplace_filter" >:: begin fun _ -> 
      v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|];
      ignore @@ Int_vec.push v 32;
      v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 32|];
      Int_vec.inplace_filter (fun x -> x mod 2 = 0) v ;
      v =~~ [|0; 2; 4; 6; 8; 32|];
      Int_vec.inplace_filter (fun x -> x mod 3 = 0) v ;
      v =~~ [|0;6|];
      Int_vec.inplace_filter (fun x -> x mod 3 <> 0) v ;
      v =~~ [||]
    end
    ;
    "filter" >:: begin fun _ -> 
      let v = Int_vec.of_array [|1;2;3;4;5;6|] in 
      v |> Int_vec.filter (fun x -> x mod 3 = 0) |> (fun x -> x =~~ [|3;6|]);
      v =~~ [|1;2;3;4;5;6|];
      Int_vec.pop v ; 
      v =~~ [|1;2;3;4;5|]
    end
    ;

    "capacity" >:: begin fun _ -> 
      let v = Int_vec.of_array [|3|] in 
      Int_vec.reserve v 10 ;
      v =~~ [|3 |];
      Int_vec.push v 1;
      Int_vec.push v 2;
      Int_vec.push v 5;
      v=~~ [|3;1;2;5|];
      OUnit.assert_equal (Int_vec.capacity v  ) 10 ;
      for i = 0 to 5 do
        Int_vec.push v i
      done;
      v=~~ [|3;1;2;5;0;1;2;3;4;5|];
      Int_vec.push v 100;
      v=~~[|3;1;2;5;0;1;2;3;4;5;100|];
      OUnit.assert_equal (Int_vec.capacity v ) 20
    end
    ;
    __LOC__  >:: begin fun _ -> 
      let empty = Int_vec.empty () in 
      Int_vec.push  empty 3;
      empty =~~ [|3|];

    end
  ]

end
module Ounit_tests_main : sig 
#1 "ounit_tests_main.mli"

end = struct
#1 "ounit_tests_main.ml"




module Int_array = Resize_array.Make(struct type t = int let null = 0 end);;
let v = Int_array.init 10 (fun i -> i);;

let ((>::),
    (>:::)) = OUnit.((>::),(>:::))


let (=~) x y = OUnit.assert_equal ~cmp:(Int_array.equal  (fun (x: int) y -> x=y)) x y
let (=~~) x y 
  = 
  OUnit.assert_equal ~cmp:(Int_array.equal  (fun (x: int) y -> x=y)) x (Int_array.of_array y) 

let suites = 
  __FILE__ >:::
  [
    Ounit_vec_test.suites;
    Ounit_json_tests.suites;
    Ounit_path_tests.suites;
    Ounit_array_tests.suites;    
    Ounit_scc_tests.suites;
  ]
let _ = 
  OUnit.run_test_tt_main suites

end
