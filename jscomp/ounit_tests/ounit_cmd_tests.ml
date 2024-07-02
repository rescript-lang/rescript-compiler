let (//) = Filename.concat




let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal





(* let output_of_exec_command command args =
    let readme, writeme = Unix.pipe () in
    let pid = Unix.create_process command args Unix.stdin writeme Unix.stderr in
    let in_chan = Unix.in_channel_of_descr readme *)



let perform_bsc = Ounit_cmd_util.perform_bsc

let ok b output = 
  if not b then 
    Ounit_cmd_util.debug_output output;
  OUnit.assert_bool __LOC__ b  

let suites =
  __FILE__
  >::: [
    __LOC__ >:: begin fun _ ->
      let v_output = perform_bsc  [| "-v" |] in
      OUnit.assert_bool __LOC__ ((perform_bsc [| "-h" |]).exit_code  = 0  );
      OUnit.assert_bool __LOC__ (v_output.exit_code = 0);
      (* Printf.printf "\n*>%s" v_output.stdout; *)
      (* Printf.printf "\n*>%s" v_output.stderr ; *)
    end;



    (* __LOC__ >:: begin fun _ ->
       let should_err = bsc_check_eval {|
       external f : string -> unit -> unit = "x.y" [@@send]
       |} in
       OUnit.assert_bool __LOC__
        (Ext_string.contain_substring should_err.stderr "Not a valid method name")
       end; *)


  ]

