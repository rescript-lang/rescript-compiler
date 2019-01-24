let values =
  [
    "\xD0\xB2\xD0\xB5\xD1\x80\xD0\xB1\xD0\xBB\xD1\x8E\xD0\xB4\xD1\x8B"; (* "верблюды" *)
    "\xE9\xAA\x86\xE9\xA9\xBC"; (* "骆驼" *)
    "\215\167\215\162\215\158\215\156"; (* "קעמל" *)
    "\216\167\217\136\217\134\217\185"; (* "اونٹ" *)
  ]

let env0 =
  List.sort compare (List.mapi (fun i v -> Printf.sprintf "OCAML_UTF8_VAR%d=%s" i v) values)

let split sep s =
  match String.index s sep with
  | i ->
      String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)
  | exception Not_found ->
      s, ""

let test_environment () =
  print_endline "test_environment";
  let vars = List.map (fun s -> fst (split '=' s)) env0 in
  let f s = List.mem (fst (split '=' s)) vars in
  let env = List.filter f (Array.to_list (Unix.environment ())) in
  assert (List.length env0 = List.length env);
  List.iter2 (fun s1 s2 -> assert (s1 = s2)) env0 env

let test0 () =
  print_endline "test0";
  Unix.execve Sys.executable_name [|Sys.executable_name; "1"|] (Array.of_list env0)

let test_argv () =
  print_endline "test_argv";
  let argv = match Array.to_list Sys.argv with _ :: _ :: argv -> argv | _ -> assert false in
  List.iter2 (fun s1 s2 -> assert (s1 = s2)) argv values

let test1 () =
  print_endline "test1";
  Unix.execv Sys.executable_name (Array.of_list (Sys.executable_name :: "2" :: values))

let restart = function
  | 0 -> test0 ()
  | 1 -> test_environment (); test1 ()
  | 2 -> test_argv ()
  | _ -> assert false

let main () =
  match Array.length Sys.argv with
  | 1 ->
      let pid = Unix.create_process Sys.executable_name [|Sys.executable_name; "0"|] Unix.stdin Unix.stdout Unix.stderr in
      begin match Unix.waitpid [] pid with
      | _, Unix.WEXITED 0 -> ()
      | _, (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _) -> failwith "Child process error"
      end
  | _ ->
      restart (int_of_string Sys.argv.(1))

let () =
  match main () with
  | () ->
      Printf.printf "OK\n%!"
  | exception e ->
      Printf.printf "BAD: %s\n%!" (Printexc.to_string e);
      exit 1
