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

val assert_raise_any : ?msg:string ->  (unit -> 'a) -> unit

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
