let (//) = Filename.concat




let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal





(* let output_of_exec_command command args =
    let readme, writeme = Unix.pipe () in 
    let pid = Unix.create_process command args Unix.stdin writeme Unix.stderr in 
    let in_chan = Unix.in_channel_of_descr readme *)


let react = {|
type u 

external a : u = "react" [@@bs.module]

external b : unit -> int = "bool" [@@bs.module "react"]

let v = a
let h = b ()

|}        
let foo_react = {|
type bla


external foo : bla = "foo.react" [@@bs.module]

external bar : unit -> bla  = "bar" [@@bs.val] [@@bs.module "foo.react"]

let c = foo 

let d = bar ()

|}

let perform_bsc = Ounit_cmd_util.perform_bsc
let bsc_eval = Ounit_cmd_util.bsc_eval


let suites = 
  __FILE__
  >::: [
    __LOC__ >:: begin fun _ -> 
      let v_output = perform_bsc  [| "-v" |] in 
      OUnit.assert_bool __LOC__ ((perform_bsc [| "-h" |]).exit_code  <> 0  );
      OUnit.assert_bool __LOC__ (v_output.exit_code = 0);
      (* Printf.printf "\n*>%s" v_output.stdout; *)
      (* Printf.printf "\n*>%s" v_output.stderr ; *)
    end; 
    __LOC__ >:: begin fun _ -> 
      let simple_quote = 
        perform_bsc  [| "-bs-eval"; {|let str = "'a'" |}|] in 
      OUnit.assert_bool __LOC__ (simple_quote.exit_code = 0)
    end;
    __LOC__ >:: begin fun _ -> 
      let should_be_warning = 
        bsc_eval  {|let bla4 foo x y= foo##(method1 x y [@bs]) |} in 
      (* debug_output should_be_warning; *)
      OUnit.assert_bool __LOC__ (Ext_string.contain_substring
                                   should_be_warning.stderr Literals.unused_attribute)
    end;
    __LOC__ >:: begin fun _ -> 
      let dedupe_require = 
        bsc_eval (react ^ foo_react) in 
      OUnit.assert_bool __LOC__ (Ext_string.non_overlap_count
                                   dedupe_require.stdout ~sub:"require" = 2
                                )     
    end;
    __LOC__ >:: begin fun _ -> 
      let dedupe_require = 
        bsc_eval react in 
      OUnit.assert_bool __LOC__ (Ext_string.non_overlap_count
                                   dedupe_require.stdout ~sub:"require" = 1
                                )     
    end;
    __LOC__ >:: begin fun _ -> 
      let dedupe_require = 
        bsc_eval foo_react in 
      OUnit.assert_bool __LOC__ (Ext_string.non_overlap_count
                                   dedupe_require.stdout ~sub:"require" = 1
                                )     
    end;
    __LOC__ >:: begin fun _ -> 
      let should_err = bsc_eval {|
external ff : 
    resp -> (_ [@bs.as "x"]) -> int -> unit = 
    "x" [@@bs.set]      
      |} in 
      OUnit.assert_bool __LOC__ 
      (Ext_string.contain_substring should_err.stderr
      "Ill defined"
      )
    end;

    __LOC__ >:: begin fun _ -> 
(** used in return value 
    This should fail, we did not 
    support uncurry return value yet
*)
    let should_err = bsc_eval {|
    external v3 :
    int -> int -> (int -> int -> int [@bs.uncurry])
    = ""[@@bs.val]

    |} in 
    (* Ounit_cmd_util.debug_output should_err;*)
    OUnit.assert_bool __LOC__
    (Ext_string.contain_substring 
    should_err.stderr "bs.uncurry")
    end ;

    __LOC__ >:: begin fun _ -> 
    let should_err = bsc_eval {|
    external v4 :  
    (int -> int -> int [@bs.uncurry]) = ""
    [@@bs.val]

    |} in 
    (* Ounit_cmd_util.debug_output should_err ; *)
    OUnit.assert_bool __LOC__
    (Ext_string.contain_substring 
    should_err.stderr "bs.uncurry")
  end ;

    __LOC__ >:: begin fun _ -> 
      let should_err = bsc_eval {|
      {js| \uFFF|js}
      |} in 
      OUnit.assert_bool __LOC__ (not @@ Ext_string.is_empty should_err.stderr)
    end;

    __LOC__ >:: begin fun _ -> 
      let should_err = bsc_eval {|
      external mk : int -> ([`a|`b] [@bs.string]) = "" [@@bs.val]
      |} in 
      OUnit.assert_bool __LOC__ (not @@ Ext_string.is_empty should_err.stderr)
    end;
    
    __LOC__ >:: begin fun _ -> 
      let should_err = bsc_eval {|
      external mk : int -> ([`a|`b] ) = "" [@@bs.val]
      |} in 
      OUnit.assert_bool __LOC__ ( Ext_string.is_empty should_err.stderr)
      (* give a warning or ? 
         ( [`a | `b ] [@bs.string] ) 
         (* auto-convert to ocaml poly-variant *)
      *)
    end;

    __LOC__ >:: begin fun _ -> 
      let should_err = bsc_eval {|
      type t 
      external mk : int -> (_ [@bs.as {json| { x : 3 } |json}]) ->  t = "" [@@bs.val]
      |} in 
      OUnit.assert_bool __LOC__ (Ext_string.contain_substring should_err.stderr "Invalid json literal")
    end
    ;
    __LOC__ >:: begin fun _ -> 
      let should_err = bsc_eval {|
      type t 
      external mk : int -> (_ [@bs.as {json| { "x" : 3 } |json}]) ->  t = "" [@@bs.val]
      |} in 
      OUnit.assert_bool __LOC__ (Ext_string.is_empty should_err.stderr)
    end
    ;
    (* #1510 *)
    __LOC__ >:: begin fun _ -> 
      let should_err = bsc_eval {|
       let should_fail = fun [@bs.this] (Some x) y u -> y + u 
      |} in 
      OUnit.assert_bool __LOC__ 
        (Ext_string.contain_substring  should_err.stderr "simple")
    end;

    __LOC__ >:: begin fun _ -> 
      let should_err = bsc_eval {|
       let should_fail = fun [@bs.this] (Some x as v) y u -> y + u 
      |} in 
      (* Ounit_cmd_util.debug_output should_err; *)
      OUnit.assert_bool __LOC__ 
        (Ext_string.contain_substring  should_err.stderr "simple")
    end;
    
    __LOC__ >:: begin fun _ -> 
      let should_err = bsc_eval {|
let handler2 = fun [@bs.exn] (a,b) x -> 
  match a with 
  | _ -> x + 1

|} in 
      OUnit.assert_bool __LOC__ 
        (Ext_string.contain_substring should_err.stderr "identifier")
    end
  ]

