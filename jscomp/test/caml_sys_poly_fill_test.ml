let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites
 

let () =

  (* test Sys.getenv *)
  eq __LOC__ "X" (
    Node_process.putEnvVar __FILE__ "X";
    let v = Sys.getenv __FILE__ in 
    Node_process.deleteEnvVar __FILE__ ;
    v
  );
  eq __LOC__ "Y" (
    Node_process.putEnvVar __FILE__ "Y";
    let v = Sys.getenv __FILE__ in 
    Node_process.deleteEnvVar __FILE__ ;
    v
  );
  eq __LOC__ "Z" (
    Node_process.deleteEnvVar __FILE__ ;
    let v = try Sys.getenv __FILE__ with Not_found -> "Z" in 
    v
  );

  (* test Sys.is_directory *)
  eq __LOC__ true (
    Sys.is_directory "."
  );
  eq __LOC__ false (
    Sys.is_directory "Makefile"
  );
  eq __LOC__ "sys_error" (
    try
      begin
        ignore @@ Sys.is_directory "path_that_does_not_exist";
        "no_error"
      end
    with
    | Sys_error _e -> "sys_error"
  );

  (* test Sys.file_exists *)
  eq __LOC__ true (
    Sys.file_exists "."
  );
  eq __LOC__ false (
    Sys.file_exists "path_that_does_not_exist"
  );

  (* test Sys.command *)
  eq __LOC__ 0 (
    Sys.command "true"
  );
  eq __LOC__ 0 (
    (* make sure commands are interpreted by a shell *)
    Sys.command "type true"
  );
  eq __LOC__ 1 (
    Sys.command "false"
  );
  eq __LOC__ 127 (
    Sys.command "not_a_real_command"
  );

  (* test we can open an out_channel from a file, close it, and delete it *)
  eq __LOC__ 0 (
    let (file, oc) = Filename.open_temp_file "pre." ".txt" in
    output_string oc "test contents";
    flush oc;
    close_out oc;
    Sys.remove file;
    0
  );
  (* test we can write to a file and read what we wrote *)
  eq __LOC__ "test contents" (
    let (file, oc) = Filename.open_temp_file "pre." ".txt" in
    output_string oc "test contents";
    close_out oc;
    let read_contents = ([%bs.raw {|
      function (file) { return require('fs').readFileSync(file, 'ascii'); }
    |}] : string -> string) file in
    Sys.remove file;
    read_contents
  )
  (* eq __LOC__ true (
   *   (\* this will work once input_channel stuff is implemented *\)
   *   let oc = open_out "tmp_foo.txt" in
   *   output_string oc "foo";
   *   close_out_noerr oc;
   *   let ic = open_in "tmp_foo.txt" in
   *   let line = input_line ic in
   *   close_in_noerr ic;
   *   line = "foo"
   * ) *)



let () = Mt.from_pair_suites __FILE__ !suites
