let current = ref 0;;

let accum = ref [];;

let record fmt (* args *) =
  Printf.kprintf (fun s -> accum := s :: !accum) fmt
;;

let f_unit () = record "unit()";;
let f_bool b = record "bool(%b)" b;;
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

let test argv =
  current := 0;
  r_set := false;
  r_clear := true;
  r_string := "";
  r_int := 0;
  r_float := 0.0;
  accum := [];
  Arg.parse_argv ~current argv spec f_anon "usage";
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

test args1;;
#if OCAML_VERSION =~ ">4.03.0" then
test args2;; 
let suites = [];;
#else
  (* Arg accept -key=value since 4.04 *)
let suites = 
  Mt.[ "should raise", fun _ -> ThrowAny (fun _ -> test args2)]
#end
;; Mt.from_pair_suites __FILE__ suites

