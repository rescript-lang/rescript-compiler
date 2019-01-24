let current = ref 0;;

let accum = ref [];;

let record fmt (* args *) =
  Printf.kprintf (fun s -> accum := s :: !accum) fmt
;;

let f_unit () = record "unit()";;
let f_bool b = record "bool(%B)" b;;
let r_set = ref false;;
let r_clear = ref true;;
let f_string s = record "string(%s)" s;;
let r_string = ref "";;
let f_int i = record "int(%d)" i;;
let r_int = ref 0;;
let f_float f = record "float(%g)" f;;
let r_float = ref 0.0;;
let f_symbol s = record "symbol(%s)" s;;
let f_rest s = record "rest(%s)" s;;
let f_anon s = record "anon(%s)" s;;

let spec = Arg.[
  "-u", Unit f_unit, "Unit (0)";
  "-b", Bool f_bool, "Bool (1)";
  "-s", Set r_set, "Set (0)";
  "-c", Clear r_clear, "Clear (0)";
  "-str", String f_string, "String (1)";
  "-sstr", Set_string r_string, "Set_string (1)";
  "-i", Int f_int, "Int (1)";
  "-si", Set_int r_int, "Set_int (1)";
  "-f", Float f_float, "Float (1)";
  "-sf", Set_float r_float, "Set_float (1)";
  "-t", Tuple [Bool f_bool; String f_string; Int f_int], "Tuple (3)";
  "-sym", Symbol (["a"; "b"; "c"], f_symbol), "Symbol (1)";
  "-rest", Rest f_rest, "Rest (*)";
];;

let args1 = [|
  "prog";
  "anon1";
  "-u";
  "-b"; "true";
  "-s";
  "anon2";
  "-c";
  "-str"; "foo";
  "-sstr"; "bar";
  "-i"; "19";
  "-si"; "42";
  "-f"; "3.14";
  "-sf"; "2.72";
  "anon3";
  "-t"; "false"; "gee"; "1436";
  "-sym"; "c";
  "anon4";
  "-rest"; "r1"; "r2";
|];;

let args2 = [|
  "prog";
  "anon1";
  "-u";
  "-b=true";
  "-s";
  "anon2";
  "-c";
  "-str=foo";
  "-sstr=bar";
  "-i=19";
  "-si=42";
  "-f=3.14";
  "-sf=2.72";
  "anon3";
  "-t"; "false"; "gee"; "1436";
  "-sym=c";
  "anon4";
  "-rest"; "r1"; "r2";
|];;

let error s = Printf.printf "error (%s)\n" s;;
let check r v msg = if !r <> v then error msg;;

let test spec argv =
  current := 0;
  r_set := false;
  r_clear := true;
  r_string := "";
  r_int := 0;
  r_float := 0.0;
  accum := [];
  Arg.parse_and_expand_argv_dynamic current argv (ref spec) f_anon "usage";
  let result = List.rev !accum in
  let reference = [
      "anon(anon1)";
      "unit()";
      "bool(true)";
      "anon(anon2)";
      "string(foo)";
      "int(19)";
      "float(3.14)";
      "anon(anon3)";
      "bool(false)"; "string(gee)"; "int(1436)";
      "symbol(c)";
      "anon(anon4)";
      "rest(r1)"; "rest(r2)";
    ]
  in
  if result <> reference then begin
    let f x y =
      Printf.printf "%20s %c %-20s\n%!" x (if x = y then '=' else '#') y
    in
    List.iter2 f result reference;
  end;
  check r_set true "Set";
  check r_clear false "Clear";
  check r_string "bar" "Set_string";
  check r_int 42 "Set_int";
  check r_float 2.72 "Set_float";
;;

let test_arg args = test spec (ref args);;

test_arg args1;;
test_arg args2;;


let safe_rm file =
  try
    Sys.remove file
  with _ -> ()

let test_rw argv =
  safe_rm "test_rw";
  safe_rm "test_rw0";
  Arg.write_arg "test_rw" argv;
  Arg.write_arg0 "test_rw0" argv;
  let argv' = Arg.read_arg "test_rw" in
  let argv0 = Arg.read_arg0 "test_rw0" in
  let f x y =
    if x <> y then
      Printf.printf "%20s %c %-20s\n%!" x (if x = y then '=' else '#') y
  in
  Array.iter2 f argv argv';
  Array.iter2 f argv argv0;
  safe_rm "test_rw";
  safe_rm "test_rw0";
;;

test_rw args1;;
test_rw args2;;
test_rw (Array.make 0 "");;
test_rw [|"";""|];;

let f_expand r msg arg s =
  if s <> r then error msg;
  arg;
;;

let expand1,args1,expected1 =
  let l = Array.length args1  - 1 in
  let args = Array.sub args1 1 l in
  let args1 =  [|"prog";"-expand";"expand_arg1"|] in
  Arg.["-expand", Expand (f_expand "expand_arg1" "Expand" args), "Expand (1)";],
  args1,
  Array.append  args1 args
;;

let expand2,args2,expected2 =
  let l = Array.length args2  - 1 in
  let args = Array.sub args2 1 l in
  let args2 = [|"prog";"-expand";"expand_arg2"|] in
  Arg.["-expand", Expand (f_expand "expand_arg2" "Expand" args), "Expand (1)";],
  args2,
  Array.append args2 args
;;

let test_expand spec argv reference =
  let result = ref argv in
  test spec result;
  let f x y =
    if x <> y then
      Printf.printf "%20s %c %-20s\n%!" x (if x = y then '=' else '#') y
  in
  Array.iter2 f !result reference;
;;

test_expand (expand1@spec) args1 expected1;;
test_expand (expand2@spec) args2 expected2;;

let test_align () =
  let spec =
    [
      "-foo", Arg.String ignore, "FOO Do foo with FOO";
      "-bar", Arg.Tuple [Arg.String ignore; Arg.String ignore], "FOO BAR\tDo bar with FOO and BAR";
      "-cha", Arg.Unit ignore, " Another option";
      "-sym", Arg.Symbol (["a"; "b"], ignore), "\ty\tfoo";
      "-sym2", Arg.Symbol (["a"; "b"], ignore), "x bar";
    ]
  in
  print_endline (Arg.usage_string (Arg.align spec) "")
;;

test_align ();;
