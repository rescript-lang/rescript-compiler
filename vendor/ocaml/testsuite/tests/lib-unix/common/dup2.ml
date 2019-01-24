let cat file =
  let fd = Unix.openfile file [Unix.O_RDONLY] 0 in
  let buf = Bytes.create 1024 in
  let rec cat () =
    let n = Unix.read fd buf 0 (Bytes.length buf) in
    if n > 0 then (ignore(Unix.write Unix.stdout buf 0 n); cat ())
  in cat (); Unix.close fd

let out fd txt =
  ignore (Unix.write_substring fd txt 0 (String.length txt))

let _ =
  let fd =
    Unix.(openfile "./tmp.txt"
                   [O_WRONLY;O_TRUNC;O_CREAT;O_SHARE_DELETE]
		   0o600) in
  out fd "---\n";
  Unix.dup2 ~cloexec:true fd Unix.stderr;
  Unix.close fd;
  out Unix.stderr "Some output\n";
  cat "./tmp.txt";
  Sys.remove "./tmp.txt"

    
