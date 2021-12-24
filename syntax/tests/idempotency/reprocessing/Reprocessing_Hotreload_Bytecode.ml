(* #if BYTECODE then *)
let load_plug fname =
  let fname = Dynlink.adapt_filename fname in
  if Sys.file_exists fname
  then
    try Dynlink.loadfile fname
    with
    | ((Dynlink.Error (err))[@explicit_arity ]) ->
        print_endline
          ("ERROR loading plugin: " ^ (Dynlink.error_message err))
    | e ->
        failwith
          ("Unknown error while loading plugin: " ^ (Printexc.to_string e))
  else failwith "Plugin file does not exist"
let last_st_mtime = ref 0.
let extension =
  match Sys.win32 || Sys.cygwin with | true -> ".exe" | false -> ""
let ocaml =
  (match Dynlink.is_native with
   | true -> "ocamlopt.opt"
   | false -> "ocamlc.opt") ^ extension
let extension =
  match Dynlink.is_native with | true -> "cmxs" | false -> "cmo"
let shared = match Dynlink.is_native with | true -> "-shared" | false -> "-c"
let folder =
  match Dynlink.is_native with | true -> "native" | false -> "bytecode"
let (+/) = Filename.concat
let bsb_native = ("node_modules" +/ "bs-platform-native") +/ "bsb"
let checkRebuild firstTime filePath =
  if firstTime
  then
    (print_endline
       (bsb_native ^
          (" -run-ninja -backend bytecode -build-library " ^
             (String.capitalize_ascii
                (Filename.chop_extension (Filename.basename filePath)))));
     (match Unix.system
              (bsb_native ^
                 (" -run-ninja -backend bytecode -build-library " ^
                    (String.capitalize_ascii
                       (Filename.chop_extension (Filename.basename filePath)))))
      with
      | ((WEXITED (0))[@explicit_arity ]) -> ()
      | WEXITED _|WSIGNALED _|WSTOPPED _ -> print_endline "Hotreload failed");
     (let pid =
        Unix.create_process bsb_native
          [|bsb_native;"-w";"-run-ninja";"-backend";"bytecode";"-build-library";(
            String.capitalize_ascii
              (Filename.chop_extension (Filename.basename filePath)))|]
          Unix.stdin Unix.stdout Unix.stderr in
      print_endline ("bsb running with pid: " ^ (string_of_int pid));
      at_exit (fun () -> Unix.kill pid Sys.sigkill; Sys.remove ".bsb.lock");
      ()));
  (let filePath = ("lib" +/ "ocaml-bytecode") +/ "lib.cma" in
   if Sys.file_exists filePath
   then
     let { Unix.st_mtime = st_mtime } = Unix.stat filePath in
     (if st_mtime > (!last_st_mtime)
      then
        (print_endline "Reloading hotloaded module";
         load_plug filePath;
         last_st_mtime := st_mtime;
         true)
      else false)
   else false)
(* #end *)
