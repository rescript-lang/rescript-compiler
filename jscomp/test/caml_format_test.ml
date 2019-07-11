let of_string =
  [| (0, "0"); (3, "03"); (-3, "-03"); (-63, "-0x3f"); (-31, "-0x1f")
   ; (47, "0X2f"); (11, "0O13"); (8, "0o10"); (3, "0b11"); (1, "0b01")
   ; (0, "0b00"); (-3, "-0b11"); (-5, "-0B101"); (332, "0332"); (-32, "-32")
   ; (-4294967295, "-0xffff_ffff")
   ; (-1, "0xffff_ffff") |]

(* let float_of_string = *)
(* [| "nan", nan ; *)
(* "infinity", infinity; *)
(* "0.", 0. *)
(* |] *)

let from_float_of_string xs =
  xs |> Array.mapi (fun i (a, b) -> string_of_float)

let from_of_string xs =
  of_string
  |> Array.mapi (fun i (a, b) ->
         (Printf.sprintf "of_string %L" i, fun _ -> Mt.Eq (int_of_string b, a)))
  |> Array.to_list

let u v = Printf.sprintf "%33d" v
let to_str s = int_of_string s
let v = Printf.sprintf "%3d" 3333

external format_int : string -> int -> string = "caml_format_int"

let suites : Mt.pair_suites =
  from_of_string of_string
  @ [ ( "isnan_of_string"
      , fun _ -> Mt.Eq (true, classify_float (float_of_string "nan") = FP_nan)
      ) ]
  @ (let pairs =
       [| (FP_infinite, "infinity"); (FP_infinite, "+infinity")
        ; (FP_infinite, "-infinity"); (FP_zero, "0"); (FP_zero, "0.") |] in
     pairs
     |> Array.mapi (fun i (a, b) ->
            ( Printf.sprintf "infinity_of_string %d" i
            , fun _ -> Mt.Eq (a, classify_float @@ float_of_string b) ))
     |> Array.to_list)
  @ [ ("throw", fun _ -> Mt.ThrowAny (fun _ -> ignore @@ float_of_string ""))
    ; ( "format_int"
      , fun _ ->
          Mt.Eq ("                              33", format_int "%32d" 33) ) ]
  @
  let pairs = [|(3232., "32_32.0"); (1.000, "1.000"); (12.000, "12.000")|] in
  pairs
  |> Array.mapi (fun i (a, b) ->
         ( Printf.sprintf "normal_float_of_string %d" i
         , fun _ -> Mt.Eq (a, float_of_string b) ))
  |> Array.to_list

let ff = format_int "%32d"
let a = Format.asprintf

let formatter_suites =
  Mt.
    [ ( "fmt_concat"
      , fun _ ->
          Eq
            ( Format.asprintf ("%s %03d %L" ^^ "%S %03d %L") "32" 33 33 "a" 33 3
            , "32 033 33\"a\" 033 3" ) )
    ; ( "fmt_gen"
      , fun _ ->
          Eq
            ( Format.asprintf
                ("%s %03d %L" ^^ "%S %03d %L %a")
                "32" 33 33 "a" 33 3
                (Format.pp_print_list Format.pp_print_int)
                [1; 2; 3]
            , "32 033 33\"a\" 033 3 12\n3" ) )
    ; ( "long_fmt"
      , fun _ ->
          Eq
            ( Format.asprintf
                "%d %i %u %n %l %L %N %x %X %o %s %S %c %C %f %F %e %E %g %G \
                 %B %b %ld %li %lu %lx %lX %lo %nd %ni %nu %nx %nx %no  "
                1 2 3 4 5 6 7 8 9 10 "a" "b" 'c' 'd' 1. 2. 3. 4. 5. 6. true
                false 0l 1l 2l 3l 4l 5l 6n 7n 8n 9n 10n 11n
            , "1 2 3 4 5 6 7 8 9 12 a \"b\" c 'd' 1.000000 2. 3.000000e+00 \
               4.000000E+00 5 6 true false 0 1 2 3 4 5 6 7 8 9 a 13  " ) )
    ; ( "long_fmt_2"
      , fun _ ->
          Eq
            ( Format.asprintf
                "@[%23d %2i %3u %4n %0xl %0xL %N %03x %X %o %s %S %c %C %3f \
                 %2F %2e %E %g %G %B %b %ld %li %lu %lx %lX %lo %nd %ni %nu \
                 %nx %nx %no  @]"
                1 2 3 4 5 6 7 8 9 10 "a" "b" 'c' 'd' 1. 2. 3. 4. 5. 6. true
                false 0l 1l 2l 3l 4l 5l 6n 7n 8n 9n 10n 11n
            , "                      1  2   3 4 5l 6L 7 008 9 12 a \"b\" c \
               'd' 1.000000 2. 3.000000e+00 4.000000E+00 5 6 true false 0 1 2 \
               3 4 5 6 7 8 9 a 13  " ) )
    ; ("width_1", fun _ -> Eq (Format.asprintf "%014d" 32, "00000000000032"))
    ; ("width_2", fun _ -> Eq (Format.asprintf "%10.3f" 32333.02, " 32333.020"))
    ; ("alternate_1", fun _ -> Eq (Format.asprintf "%0x" 32333, "7e4d"))
    ; ("alternate_2", fun _ -> Eq (Format.asprintf "%#0x" 32333, "0x7e4d"))
    ; ("alternate_3", fun _ -> Eq ((a "%#o" 32, a "%o" 32), ("040", "40")))
    ; ("justify_0", fun _ -> Eq (format_int "%-8d" 32, "32      "))
    ; ("sign_p", fun _ -> Eq (Format.asprintf "%+4d" 32, " +32"))
    ; ("sign_2p", fun _ -> Eq (Format.asprintf "% 4d" 32, "  32"))
    ; ("sign_3p", fun _ -> Eq (a "%lu" (-1l), "4294967295"))
    ; ("sign_4p", fun _ -> Eq (a "%ld" (-1l), "-1"))
    ; ( "width_3"
      , fun _ -> Eq (format_int "%032d" 32, "00000000000000000000000000000032")
      )
    ; ("prec_1", fun _ -> Eq (a "%.10d" 32, "0000000032"))
    ; ("prec_2", fun _ -> Eq (format_int "%.10d" 32, "0000000032"))
    ; ("prec_3", fun _ -> Eq (format_int "%.d" 32, "32"))
    ; ("prec_4", fun _ -> Eq (format_int "%.d" 32, "32"))
      (* "long_fmt", (fun _ -> Eq(Format.asprintf "%d %i %u %n %l %L %N %x %X
         %o %s %S %c %C %f %F %e %E %g %G %B %b %ld %li %lu %lx %lX %lo %nd %ni
         %nu %nx %nx %no %Ld %Li %Lu %Lx %LX %Lo " 1 2 3 4 5 6 7 8 9 10 "a" "b"
         'c' 'd' 1. 2. 3. 4. 5. 6. true false 0l 1l 2l 3l 4l 5l 6n 7n 8n 9n 10n
         11n 12L 13L 14L 15L 16L 17L , "1 2 3 4 5 6 7 8 9 12 a \"b\" c 'd'
         1.000000 2. 3.000000e+00 4.000000E+00 5 6 true false 0 1 2 3 4 5 6 7 8
         9 a 13 12 13 14 f 10 21 ")) *)
     ]

external format_float : string -> float -> string = "caml_format_float"

(* ("%3.10f", 3e+56, *)
(* "300000000000000005792779041490073052596128503513888063488.0000000000"); *)

let float_data =
  [| ("%f", 32., "32.000000"); ("%f", nan, "nan"); ("%f", infinity, "inf")
   ; ("%f", neg_infinity, "-inf")
   ; ("%1.e", 13000., "1e+04")
   ; ("%1.3e", 2.3e-05, "2.300e-05")
   ; ("%3.10e", 3e+56, "3.0000000000e+56")
   ; ("%3.10f", 20000000000., "20000000000.0000000000")
   ; ("%3.3f", -3300., "-3300.000")
   ; ("%1.g", 13000., "1e+04")
   ; ("%1.3g", 2.3e-05, "2.3e-05")
   ; ("%3.10g", 3e+56, "3e+56")
   ; ("%3.10g", 20000000000., "2e+10")
   ; ("%3.3g", -3300., "-3.3e+03")
   ; ("%3.3g", -0.0033, "-0.0033")
   ; ("%3.10g", 30000000000., "3e+10")
   ; ("%3.0g", 30000000000., "3e+10")
   ; ("%3.g", 30000000000., "3e+10")
   ; ("%3.g", 3., "  3"); ("%1.1g", 2.1, "2"); ("%1.2g", 2.1, "2.1") |]

let float_suites = Mt.["float_nan"]

module Lambda_suites = struct
  open Format

  let ident ppf s = fprintf ppf "%s" s
  let kwd ppf s = fprintf ppf "%s" s

  type lambda =
    | Lambda of string * lambda
    | Var of string
    | Apply of lambda * lambda

  let rec pr_exp0 ppf = function
    | Var s -> fprintf ppf "%a" ident s
    | lam -> fprintf ppf "@[<1>(%a)@]" pr_lambda lam

  and pr_app ppf = function
    | e -> fprintf ppf "@[<2>%a@]" pr_other_applications e

  and pr_other_applications ppf f =
    match f with
    | Apply (f, arg) -> fprintf ppf "%a@ %a" pr_app f pr_exp0 arg
    | f -> pr_exp0 ppf f

  and pr_lambda ppf = function
    | Lambda (s, lam) ->
        fprintf ppf "@[<1>%a%a%a@ %a@]" kwd "\\" ident s kwd "." pr_lambda lam
    | e -> pr_app ppf e

  let string_of_lambda = Format.asprintf "%a" pr_lambda
end

let lambda_suites =
  [| (Lambda_suites.Var "x", "x")
   ; (Lambda_suites.Apply (Lambda_suites.Var "x", Lambda_suites.Var "y"), "x y")
   ; ( Lambda_suites.Lambda
         ( "z"
         , Lambda_suites.Apply (Lambda_suites.Var "x", Lambda_suites.Var "y")
         )
     , "\\z. x y" )
   ; ( Lambda_suites.Lambda
         ( "z"
         , Lambda_suites.Lambda
             ( "z"
             , Lambda_suites.Apply
                 (Lambda_suites.Var "x", Lambda_suites.Var "y") ) )
     , "\\z. \\z. x y" ) |]

let from_lambda_pairs p =
  lambda_suites
  |> Array.mapi (fun i (a, b) ->
         ( Printf.sprintf "lambda_print %d" i
         , fun _ -> Mt.Eq (Lambda_suites.string_of_lambda a, b) ))
  |> Array.to_list

let ksprintf_suites =
  Mt.
    [ ( "ksprintf"
      , fun _ ->
          Eq
            ( (let f fmt = Format.ksprintf (fun x -> x ^ x) fmt in
               f "%s %s a " "x" "xx")
            , "x xx a x xx a " ) )
    ; ("sprintf", fun _ -> Eq (Format.sprintf "%s %S" "x" "X", "x \"X\"")) ]

(* module Mt = Mock_mt *)

let int64_suites =
  let a = Format.asprintf in
  Mt.
    [ ( "i32_simple"
      , fun _ -> Eq (Format.asprintf "%nx" 0xffff_ffffn, "ffffffff") )
    ; ( "i32_simple1"
      , fun _ -> Eq (Format.asprintf "%no" 0xffff_ffffn, "37777777777") )
    ; ("i64_simple", fun _ -> Eq (Format.asprintf "%Ld" 3L, "3"))
    ; ("i64_simple2", fun _ -> Eq (Format.asprintf "%Lx" 33L, "21"))
    ; ("i64_simple3", fun _ -> Eq (Format.asprintf "%Li" 33L, "33"))
    ; ("i64_simple4", fun _ -> Eq (a "%LX" 44L, "2C"))
    ; ("i64_simple5", fun _ -> Eq (a "%Lx" 44L, "2c"))
    ; ("i64_simple6", fun _ -> Eq (a "%*Lx" 5 44L, "   2c"))
    ; ("i64_simple7", fun _ -> Eq (Int64.to_string 3333L, "3333"))
    ; ("i64_simple8", fun _ -> Eq (a "%Ld%018Ld" 3L 3L, "3000000000000000003"))
    ; ( "i64_simple9"
      , fun _ ->
          Eq
            ( a "%Ld%018Ld" 460800000000000L 0L
            , "460800000000000000000000000000000" ) )
    ; ("i64_simple10", fun _ -> Eq (a "%Lx" Int64.max_int, "7fffffffffffffff"))
    ; ("i64_simple15", fun _ -> Eq (a "%Ld" (-1L), "-1"))
    ; ("i64_simple16", fun _ -> Eq (a "%Ld" (-11111L), "-11111"))
    ; ("i64_simple14", fun _ -> Eq (a "%LX" (-1L), "FFFFFFFFFFFFFFFF"))
    ; ("i64_simple17", fun _ -> Eq (a "%Lx" (-1L), "ffffffffffffffff"))
    ; ("i64_simple11", fun _ -> Eq (a "%LX" Int64.max_int, "7FFFFFFFFFFFFFFF"))
    ; ("i64_simple12", fun _ -> Eq (a "%LX" Int64.min_int, "8000000000000000"))
    ; ("i64_simple17", fun _ -> Eq (a "%Lu" (-1L), "18446744073709551615"))
    ; ("i64_simple21", fun _ -> Eq (a "%Lu" (-10000L), "18446744073709541616"))
    ; ( "i64_simple19"
      , fun _ -> Eq (a "%Lo" Int64.min_int, "1000000000000000000000") )
    ; ( "i64_simple13"
      , fun _ -> Eq (a "%LX" Int64.(add min_int 1L), "8000000000000001") )
    ; ("i64_simple20", fun _ -> Eq (a "%12Lx" 3L, "           3"))
    ; ( "i64_simple21"
      , fun _ -> Eq (a "%LX" 7985179176134664640L, "6ED10E27455A61C0") )
    ; ("missing_neline", fun _ -> Eq (Format.asprintf "%Ld\n" 32L, "32\n"))
    ; ( "missing_newline2"
      , fun _ ->
          Eq
            ( (let buf = Buffer.create 30 in
               Printf.bprintf buf "%Ld\n" 32L ;
               Buffer.contents buf)
            , "32\n" ) ) ]

let hh = 922337203685477580L (* 2 ^ 63 / 10 *)

let hhh = 1152921504606846976L

let of_string_data =
  [| (0L, "0"); (3L, "3"); (33L, "33"); (333L, "33_3"); (33333L, "33_33_3")
   ; (333333333333L, "333333333333")
   ; (-1L, "0xffff_ffff_ffff_ffff")
   ; (113L, "0b01110001")
   ; (1L, "-0xffff_ffff_ffff_ffff") |]

(* module Mt = Mock_mt *)

let () =
  Mt.from_pair_suites __MODULE__
  @@ suites @ formatter_suites
  @ from_lambda_pairs lambda_suites
  @ ksprintf_suites
  @ ( Array.mapi
        (fun i (fmt, f, str_result) ->
          ( Printf.sprintf "float_format %d" i
          , fun _ -> Mt.Eq (format_float fmt f, str_result) ))
        float_data
    |> Array.to_list )
  @ int64_suites
  @ ( of_string_data
    |> Array.mapi (fun i (a, b) ->
           ( Printf.sprintf "int64_of_string %d " i
           , fun _ -> Mt.Eq (Int64.of_string b, a) ))
    |> Array.to_list )
