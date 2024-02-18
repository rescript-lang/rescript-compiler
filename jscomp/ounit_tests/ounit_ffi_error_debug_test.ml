let (//) = Filename.concat




let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal




let bsc_eval = Ounit_cmd_util.bsc_check_eval

let debug_output = Ounit_cmd_util.debug_output


let suites = 
    __FILE__ 
    >::: [
        __LOC__ >:: begin fun _ -> 
        let output = bsc_eval {|
external err : 
   hi_should_error:([`a of int | `b of string ] [@string]) ->         
   unit -> _ = "" [@@obj]
        |} in
        OUnit.assert_bool __LOC__
            (Ext_string.contain_substring output.stderr "hi_should_error")
        end;
        __LOC__ >:: begin fun _ -> 
let output = bsc_eval {|
    external err : 
   ?hi_should_error:([`a of int | `b of string ] [@string]) ->         
   unit -> _ = "" [@@obj]
        |} in
        OUnit.assert_bool __LOC__
            (Ext_string.contain_substring output.stderr "hi_should_error")        
        end;
        __LOC__ >:: begin fun _ -> 
        let output = bsc_eval {|
    external err : 
   ?hi_should_error:([`a of int | `b of string ] [@string]) ->         
   unit -> unit = "err" [@@val]
        |} in
        OUnit.assert_bool __LOC__
            (Ext_string.contain_substring output.stderr "hi_should_error")        
        end;

        __LOC__ >:: begin fun _ ->
          (*
             Each [@unwrap] variant constructor requires an argument
          *)
          let output =
            bsc_eval {|
              external err :
              ?hi_should_error:([`a of int | `b] [@unwrap]) -> unit -> unit = "err" [@@val]
            |}
          in
          OUnit.assert_bool __LOC__
            (Ext_string.contain_substring output.stderr "unwrap")
        end;

        __LOC__ >:: begin fun _ ->
          (*
             [@unwrap] args are not supported in [@@obj] functions
          *)
          let output =
            bsc_eval {|
              external err :
              ?hi_should_error:([`a of int] [@unwrap]) -> unit -> _ = "" [@@obj]
            |}
          in
          OUnit.assert_bool __LOC__
            (Ext_string.contain_substring output.stderr "hi_should_error")
        end

    ]
