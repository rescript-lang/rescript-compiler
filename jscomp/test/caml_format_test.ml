
let of_string = 
  [| (0, "0"); (3, "03"); (-3, "-03"); (-63, "-0x3f"); (-31, "-0x1f");
     (47, "0X2f"); (11, "0O13"); (8, "0o10"); (3, "0b11"); (1, "0b01");
     (0, "0b00"); (-3, "-0b11"); (-5, "-0B101"); (332, "0332"); (-32, "-32");
     (-4294967295, "-0xffff_ffff");
     (-1, "0xffff_ffff")
  |]

(* let float_of_string =  *)
(*   [| "nan", nan ; *)
(*      "infinity", infinity; *)
(*      "0.", 0. *)
(*   |] *)

let from_float_of_string xs = 
  xs 
  |> Array.mapi (fun i (a,b) -> 
      string_of_float
    )

let from_of_string xs = 
  of_string 
  |>  Array.mapi (fun i (a,b) -> 
      (Printf.sprintf "of_string %L" i), fun _ -> Mt.Eq(int_of_string b,a ))
  |> Array.to_list 

let u v = Printf.sprintf "%33d" v 

let to_str s = int_of_string s

let v = Printf.sprintf "%3d" 3333
external format_int : string -> int -> string = "caml_format_int"

let suites : _ Mt.pair_suites = 
  from_of_string of_string @
  ["isnan_of_string", (fun _ -> 
       Mt.Eq (true,classify_float( float_of_string "nan") = FP_nan))] @
  (let pairs =
    [| FP_infinite, "infinity";
       FP_infinite, "+infinity"; 
       FP_infinite, "-infinity";
       FP_zero, "0";
       FP_zero, "0."
    |] 
   in 
   pairs 
   |> Array.mapi 
     (fun i (a,b) ->
        (Printf.sprintf "infinity_of_string %d" i ),
        (fun _ -> Mt.Eq(a,
                        classify_float @@ float_of_string b))) 
   |> Array.to_list ) @ 
  [ "throw", (fun _ -> Mt.ThrowAny (fun _ -> ignore @@ float_of_string ""));
    "format_int", (fun _ -> 
        Mt.Eq("                              33", format_int "%32d" 33))
  ] @
  (let pairs =
    [| 3232., "32_32.0";
       1.000, "1.000";
       12.000, "12.000"
    |] 
   in 
   pairs 
   |> Array.mapi 
     (fun i (a,b) ->
        (Printf.sprintf "normal_float_of_string %d" i ),
        (fun _ -> Mt.Eq(a,
                        float_of_string b))) 
   |> Array.to_list ) 


    
let ff = format_int "%32d"
let a = Format.asprintf

let formatter_suites = Mt.[
    "fmt_concat", 
    (fun _ -> Eq (Format.asprintf ("%s %03d %L" ^^ "%S %03d %L" ) "32" 33 33 "a" 33 3, "32 033 33\"a\" 033 3"));
    "fmt_gen",
    (fun _ -> Eq(Format.asprintf ("%s %03d %L" ^^ "%S %03d %L %a" ) "32" 33 33 "a" 33 3 (Format.pp_print_list Format.pp_print_int) [1;2;3], "32 033 33\"a\" 033 3 12\n3"));
    "long_fmt", (fun _ -> Eq(Format.asprintf "%d %i %u %n %l %L %N %x %X %o %s %S %c %C %f %F %e %E %g %G %B %b %ld %li %lu %lx %lX %lo %nd %ni %nu %nx %nx %no  " 1 2 3 4 5 6 7 8 9 10 "a" "b" 'c' 'd' 1. 2. 3. 4. 5. 6. true false 0l 1l 2l 3l 4l 5l 6n 7n 8n 9n 10n 11n  , "1 2 3 4 5 6 7 8 9 12 a \"b\" c 'd' 1.000000 2. 3.000000e+00 4.000000E+00 5 6 true false 0 1 2 3 4 5 6 7 8 9 a 13  "));
    "long_fmt_2", (fun _ -> Eq(Format.asprintf "@[%23d %2i %3u %4n %0xl %0xL %N %03x %X %o %s %S %c %C %3f %2F %2e %E %g %G %B %b %ld %li %lu %lx %lX %lo %nd %ni %nu %nx %nx %no  @]" 1 2 3 4 5 6 7 8 9 10 "a" "b" 'c' 'd' 1. 2. 3. 4. 5. 6. true false 0l 1l 2l 3l 4l 5l 6n 7n 8n 9n 10n 11n, "                      1  2   3 4 5l 6L 7 008 9 12 a \"b\" c 'd' 1.000000 2. 3.000000e+00 4.000000E+00 5 6 true false 0 1 2 3 4 5 6 7 8 9 a 13  "));
    "width_1", (fun _ -> Eq(Format.asprintf "%014d" 32, "00000000000032"));
    "width_2", (fun _ -> Eq(Format.asprintf "%10.3f" 32333.02, " 32333.020"));
    "alternate_1", (fun _ -> Eq(Format.asprintf "%0x" 32333, "7e4d"));
    "alternate_2", (fun _ -> Eq(Format.asprintf "%#0x" 32333, "0x7e4d"));
    "alternate_3", (fun _ -> Eq((a "%#o" 32, a "%o" 32), ("040", "40")));
    "justify_0", (fun _ -> Eq(format_int "%-8d" 32, "32      "));
    "sign_p",  (fun _ -> Eq(Format.asprintf "%+4d" 32, " +32"));
    "sign_2p",  (fun _ -> Eq(Format.asprintf "% 4d" 32, "  32"));
    "sign_3p", (fun _ -> Eq(a "%lu" (-1l), "4294967295"));
    "sign_4p", (fun _ -> Eq(a "%ld" (-1l), "-1"));
    "width_3",(fun _ ->     Eq(format_int "%032d" 32, "00000000000000000000000000000032"));
    "prec_1", (fun _ -> Eq(a "%.10d" 32, "0000000032"));
    "prec_2", (fun _ -> Eq(format_int "%.10d" 32, "0000000032"));
    "prec_3", (fun _ -> Eq(format_int "%.d" 32, "32"));
    "prec_4", (fun _ -> Eq(format_int "%.d" 32, "32"));
    (* "long_fmt", (fun _ -> Eq(Format.asprintf "%d %i %u %n %l %L %N %x %X %o %s %S %c %C %f %F %e %E %g %G %B %b %ld %li %lu %lx %lX %lo %nd %ni %nu %nx %nx %no %Ld %Li %Lu %Lx %LX %Lo " 1 2 3 4 5 6 7 8 9 10 "a" "b" 'c' 'd' 1. 2. 3. 4. 5. 6. true false 0l 1l 2l 3l 4l 5l 6n 7n 8n 9n 10n 11n 12L 13L 14L 15L 16L 17L , "1 2 3 4 5 6 7 8 9 12 a \"b\" c 'd' 1.000000 2. 3.000000e+00 4.000000E+00 5 6 true false 0 1 2 3 4 5 6 7 8 9 a 13 12 13 14 f 10 21 ")) *)
  ]

module Lambda_suites = struct 
  open Format;;

  let ident ppf s = fprintf ppf "%s" s;;
  let kwd ppf s = fprintf ppf "%s" s;;
  type lambda =
    | Lambda of string * lambda
    | Var of string
    | Apply of lambda * lambda

  let rec pr_exp0 ppf = function
    | Var s -> fprintf ppf "%a" ident s
    | lam -> fprintf ppf "@[<1>(%a)@]" pr_lambda lam

  and pr_app ppf = function
    | e ->  fprintf ppf "@[<2>%a@]" pr_other_applications e

  and pr_other_applications ppf f =
    match f with
    | Apply (f, arg) -> fprintf ppf "%a@ %a" pr_app f pr_exp0 arg
    | f -> pr_exp0 ppf f

  and pr_lambda ppf = function
    | Lambda (s, lam) ->
      fprintf ppf "@[<1>%a%a%a@ %a@]" kwd "\\" ident s kwd "." pr_lambda lam
    | e -> pr_app ppf e

  let string_of_lambda  = Format.asprintf "%a" pr_lambda
end

let lambda_suites = 
  [|(Lambda_suites.Var "x", "x");
    (Lambda_suites.Apply (Lambda_suites.Var "x", Lambda_suites.Var "y"), "x y");
    (Lambda_suites.Lambda ("z",
                           Lambda_suites.Apply (Lambda_suites.Var "x", Lambda_suites.Var "y")),
     "\\z. x y");
    (Lambda_suites.Lambda ("z",
                           Lambda_suites.Lambda ("z",
                                                 Lambda_suites.Apply (Lambda_suites.Var "x", Lambda_suites.Var "y"))),
     "\\z. \\z. x y")|]

let from_lambda_pairs p = 
  lambda_suites 
  |> Array.mapi (fun i (a,b) -> Printf.sprintf "lambda_print %d" i, (fun _ -> Mt.Eq(Lambda_suites.string_of_lambda a, b)))
  |> Array.to_list


let ksprintf_suites = Mt.[
    "ksprintf", (fun _ -> Eq ((let f fmt = Format.ksprintf (fun x -> x ^ x ) fmt in f "%s %s a " "x" "xx"),"x xx a x xx a "));
    "sprintf", (fun _ -> Eq(Format.sprintf "%s %S" "x" "X", "x \"X\""))
]

(* module Mt = Mock_mt *)

let () = 
  Mt.from_pair_suites __FILE__ @@
  suites @
  formatter_suites @
  from_lambda_pairs lambda_suites @
  ksprintf_suites


    
