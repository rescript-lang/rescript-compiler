let total = ref 0
let failed = ref 0
let num = ref 0

external to_utf16 : string -> string = "caml_to_utf16"
external create_file : string -> string -> unit = "caml_create_file"

let foreign_names =
  List.sort compare
    [
      "simple";
      "\xE4\xBD\xA0\xE5\xA5\xBD"; (* "你好" *)
      "\x73\xC5\x93\x75\x72"; (* "sœur" *)
      "e\204\129te\204\129"; (* "été" *)
    ]

let test_files =
  List.map (fun s -> s ^ ".txt") foreign_names

let to_create_and_delete_files =
  [
    "\xD0\xB2\xD0\xB5\xD1\x80\xD0\xB1\xD0\xBB\xD1\x8E\xD0\xB4\xD1\x8B"; (* "верблюды" *)
    "\xE9\xAA\x86\xE9\xA9\xBC"; (* "骆驼" *)
    "\215\167\215\162\215\158\215\156"; (* "קעמל" *)
    "\216\167\217\136\217\134\217\185"; (* "اونٹ" *)
    "L\225\186\161c \196\145\195\160"; (* "Lạc đà" *)
    "\224\176\146\224\176\130\224\176\159\224\177\134"; (* "ఒంటె" *)
    "\224\174\146\224\174\159\224\175\141\224\174\159\224\174\149\224\
     \174\174\224\175\141"; (* "ஒட்டகம்" *)
    "\217\136\216\180\216\170\216\177"; (* "وشتر" *)
    "\224\164\137\224\164\183\224\165\141\224\164\159\224\165\141\224\
     \164\176\224\164\131"; (* "उष्ट्रः" *)
    "\216\167\217\186"; (* "اٺ" *)
  ]

let rec take n l =
  if n = 0 then []
  else List.hd l :: take (n-1) (List.tl l)

let foreign_names2 =
  take (List.length foreign_names) to_create_and_delete_files

let env0 =
  List.sort compare (List.mapi (fun i v -> Printf.sprintf "OCAML_UTF8_VAR%d=%s" i v) foreign_names2)

let read_all ic =
  set_binary_mode_in ic false;
  let rec loop acc =
    match input_line ic with
    | exception End_of_file ->
        List.rev acc
    | s ->
        loop (s :: acc)
  in
  loop []

let split sep s =
  match String.index s sep with
  | i ->
      String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1)
  | exception Not_found ->
      s, ""

(** WRAPPERS *)

let quote s = "\"" ^ s ^ "\""

let ok _ = "OK"

let unit _ = "()"

let list f l = String.concat " " (List.map f l)

let ell _ = "..."

let file_kind = function
  | Unix.S_REG -> "S_REG"
  | Unix.S_DIR -> "S_DIR"
  | Unix.S_CHR -> "S_CHR"
  | Unix.S_BLK -> "S_BLK"
  | Unix.S_LNK -> "S_LNK"
  | Unix.S_FIFO -> "S_FIFO"
  | Unix.S_SOCK -> "S_SOCK"

let wrap s f quote_in x quote_out =
  Printf.printf "%s %s ... " s (quote_in x);
  match f x with
  | x ->
      Printf.printf "%s\n%!" (quote_out x);
      x
  | exception e ->
      Printf.printf "FAILED: %s\n%!" (Printexc.to_string e);
      raise e

let wrap2 s f quote_in1 quote_in2 x y quote_out =
  Printf.printf "%s %s %s ... " s (quote_in1 x) (quote_in2 y);
  match f x y with
  | x ->
      Printf.printf "%s\n%!" (quote_out x);
      x
  | exception e ->
      Printf.printf "FAILED: %s\n%!" (Printexc.to_string e);
      raise e

let getenv s =
  wrap "Sys.getenv" Sys.getenv quote s quote

let getenvironmentenv s =
  let get s =
    let env = Unix.environment () in
    let rec loop i =
      if i >= Array.length env then
        ""
      else begin
        let e = env.(i) in
        let pos = String.index e '=' in
        if String.sub e 0 pos = s then
          String.sub e (pos+1) (String.length e - pos - 1)
        else
          loop (i+1)
      end
    in
    loop 0
  in
  wrap "Unix.environment" get quote s quote

let putenv s x =
  wrap2 "Unix.putenv" Unix.putenv quote quote s x ok

let sys_rename s x =
  wrap2 "Sys.rename" Sys.rename quote quote s x ok

let unix_rename s x =
  wrap2 "Unix.rename" Unix.rename quote quote s x ok

let mkdir s mode =
  wrap2 "Unix.mkdir" Unix.mkdir quote string_of_int s mode ok

let file_exists s =
  wrap "Sys.file_exists" Sys.file_exists quote s string_of_bool

let is_directory s =
  wrap "Sys.is_directory" Sys.is_directory quote s string_of_bool

let unix_chdir s =
  wrap "Unix.chdir" Unix.chdir quote s ok

let sys_chdir s =
  wrap "Sys.chdir" Sys.chdir quote s ok

let unix_getcwd () =
  wrap "Unix.getcwd" (fun s -> Filename.basename (Unix.getcwd s)) unit () quote

let sys_getcwd () =
  wrap "Sys.getcwd" (fun s -> Filename.basename (Sys.getcwd s)) unit () quote

let rmdir s =
  wrap "Unix.rmdir" Unix.rmdir quote s ok

let remove s =
  wrap "Sys.remove" Sys.remove quote s ok

let unlink s =
  wrap "Unix.unlink" Unix.unlink quote s ok

let stat s =
  let f s = (Unix.stat s).Unix.st_kind in
  wrap "Unix.stat" f quote s file_kind

let lstat s =
  let f s = (Unix.lstat s).Unix.st_kind in
  wrap "Unix.lstat" f quote s file_kind

let large_stat s =
  let f s = (Unix.LargeFile.stat s).Unix.LargeFile.st_kind in
  wrap "Unix.LargeFile.stat" f quote s file_kind

let large_lstat s =
  let f s = (Unix.LargeFile.lstat s).Unix.LargeFile.st_kind in
  wrap "Unix.LargeFile.lstat" f quote s file_kind

let access s =
  let f s = Unix.access s [Unix.F_OK] in
  wrap "Unix.access" f quote s ok

let unix_readdir f s =
  let f s =
    let h = Unix.opendir s in
    let rec loop acc =
      match Unix.readdir h with
      | s ->
          if f s then
            loop (s :: acc)
          else
            loop acc
      | exception End_of_file ->
          Unix.closedir h;
          List.sort compare acc
    in
    loop []
  in
  wrap "Unix.{opendir,readdir}" f quote s (list quote)

let sys_readdir f s =
  let f s =
    let entries = Sys.readdir s in
    List.sort compare (List.filter f (Array.to_list entries))
  in
  wrap "Sys.readdir" f quote s (list quote)

let open_in s =
  wrap "open_in" open_in quote s ok

let open_out s =
  wrap "open_out" open_out quote s ok

let open_process_in cmdline =
  let f cmdline =
    let ic as proc = Unix.open_process_in cmdline in
    let l = List.tl (read_all ic) in
    ignore (Unix.close_process_in proc);
    l
  in
  wrap "Unix.open_process_in" f ell cmdline (list quote)

let open_process_full filter cmdline env =
  let f cmdline env =
    let (ic, _, _) as proc = Unix.open_process_full cmdline (Array.of_list env) in
    let l = read_all ic in
    ignore (Unix.close_process_full proc);
    List.sort compare (List.filter filter l)
  in
  wrap2 "Unix.open_process_full" f ell (list quote) cmdline env (list quote)

(** TESTS *)

let title s =
  let s = Printf.sprintf "Testing %s" s in
  let u = String.make (String.length s) '=' in
  Printf.printf "\n#%02d. %s\n%s\n\n%!" !num s u

let expect_gen quote x b =
  total := !total + 1;
  if x <> b then begin
    Printf.printf "** ERROR: EXPECTED RESULT = %s ACTUAL RESULT = %s\n%!" (quote x) (quote b);
    failed := !failed + 1
  end

let expect_file_kind x b =
  expect_gen file_kind x b

let expect_string x s =
  expect_gen quote x s

let expect_bool x b =
  expect_gen string_of_bool x b

let expect_int x b =
  expect_gen string_of_int x b

let test_readdir readdir =
  let filter s = List.mem s test_files in
  let entries = readdir filter Filename.current_dir_name in
  let entries = List.filter (fun s -> Filename.check_suffix s ".txt") entries in
  expect_int (List.length entries) (List.length test_files);
  List.iter2 expect_string entries test_files

let test_open_in () =
  let dump_file s =
    let ic = open_in s in
    let l = input_line ic in
    close_in ic;
    expect_string s l
  in
  let filter s = List.mem s test_files in
  let files = sys_readdir filter Filename.current_dir_name in
  List.iter dump_file files

let test_getenv () =
  let doit key s =
    putenv key s;
    expect_string (getenv key) s;
    expect_string (getenvironmentenv key) s
  in
  List.iter2 doit foreign_names foreign_names2

let test_mkdir () =
  let doit s =
    mkdir s 0o755;
    expect_bool (file_exists s) true;
    expect_bool (is_directory s) true
  in
  List.iter doit foreign_names

let test_chdir chdir getcwd =
  let doit s =
    chdir s;
    expect_string (getcwd ()) s;
    chdir Filename.parent_dir_name
  in
  List.iter doit foreign_names

let test_rmdir () =
  let doit s =
    rmdir s;
    expect_bool (file_exists s) false
  in
  List.iter doit foreign_names

let test_stat () =
  let doit s =
    expect_file_kind (stat s) Unix.S_REG;
    expect_file_kind (lstat s) Unix.S_REG;
    expect_file_kind (large_stat s) Unix.S_REG;
    expect_file_kind (large_lstat s) Unix.S_REG
  in
  List.iter doit to_create_and_delete_files

let test_access () =
  List.iter access to_create_and_delete_files

let test_rename rename =
  let doit s =
    let s' = s ^ "-1" in
    rename s s';
    expect_bool (file_exists s) false;
    expect_bool (file_exists s') true;
    rename s' s;
    expect_bool (file_exists s) true;
    expect_bool (file_exists s') false
  in
  List.iter doit to_create_and_delete_files

let test_open_out () =
  let doit s =
    let oc = open_out s in
    Printf.fprintf oc "Hello, %s\n" s;
    close_out oc
  in
  List.iter doit to_create_and_delete_files

let test_file_exists expected =
  let doit s =
    expect_bool (file_exists s) expected;
  in
  List.iter doit to_create_and_delete_files

let test_remove remove =
  let doit s =
    remove s;
    expect_bool (file_exists s) false
  in
  List.iter doit to_create_and_delete_files

let test_open_process_in () =
  let cmdline =
    String.concat " " (Filename.concat Filename.current_dir_name "printargv.exe" :: List.map Filename.quote to_create_and_delete_files)
  in
  let l = open_process_in cmdline in
  List.iter2 expect_string l to_create_and_delete_files

let test_open_process_full () =
  let vars = List.map (fun s -> fst (split '=' s)) env0 in
  let filter s = List.mem (fst (split '=' s)) vars in
  let l = open_process_full filter (Filename.concat Filename.current_dir_name "printenv.exe") env0 in
  expect_int (List.length env0) (List.length l);
  List.iter2 expect_string env0 l

(* Order matters *)
let tests =
  [|
    "test_readdir unix_readdir", (fun () -> test_readdir unix_readdir);
    "test_readdir sys_readdir", (fun () -> test_readdir sys_readdir);
    "test_open_in", test_open_in;
    "test_open_out", test_open_out;
    "test_file_exists", (fun () -> test_file_exists true);
    "test_stat", test_stat;
    "test_access", test_access;
    "test_rename unix_rename", (fun () -> test_rename unix_rename);
    "test_rename sys_rename", (fun () -> test_rename sys_rename);
    "test_remove remove", (fun () -> test_remove remove);
    "test_file_exists", (fun () -> test_file_exists false);
    "test_mkdir", test_mkdir;
    "test_chdir sys_chdir sys_getcwd", (fun () -> test_chdir sys_chdir sys_getcwd);
    "test_chdir unix_chdir unix_getcwd", (fun () -> test_chdir unix_chdir unix_getcwd);
    "test_rmdir", test_rmdir;
    "test_getenv", test_getenv;
    "test_open_process_in", test_open_process_in;
    "test_open_process_full", test_open_process_full;
  |]

(** MAIN *)

let prepare () =
  List.iter (fun s -> create_file (to_utf16 s) s) test_files

let cleanup () =
  List.iter Sys.remove test_files

let main () =
  for i = 0 to Array.length tests - 1 do
    num := !num + 1;
    let s, f = tests.(i) in
    title s;
    f ()
  done;
  Printf.printf "\n\n*** ALL TESTS DONE (%d/%d OK) ***\n%!" (!total - !failed) !total

let () =
  try
    prepare ();
    main ();
    cleanup ()
  with e ->
    Printf.printf "** ERROR: %s\n%!" (Printexc.to_string e);
    exit 1
