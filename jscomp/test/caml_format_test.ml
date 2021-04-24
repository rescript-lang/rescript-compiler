[@@@warning "-107"]

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
      ({j|of_string $i|j} ), fun _ -> Mt.Eq(int_of_string b,a ))
  |> Array.to_list 


let to_str s = int_of_string s


external format_int : string -> int -> string = "caml_format_int"

let suites :  Mt.pair_suites = 
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
        ({j|infinity_of_string $i|j}  ),
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
        ({j|normal_float_of_string $i|j}  ),
        (fun _ -> Mt.Eq(a,
                        float_of_string b))) 
   |> Array.to_list ) 


    
let ff = format_int "%32d"

external format_float: string -> float -> string
  = "caml_format_float"

(* ("%3.10f", 3e+56, *)
    (*  "300000000000000005792779041490073052596128503513888063488.0000000000"); *)

let float_data = 
  [|("%f", 32., "32.000000"); ("%f", nan, "nan"); ("%f", infinity, "inf");
  ("%f", neg_infinity, "-inf"); ("%1.e", 13000., "1e+04");
  ("%1.3e", 2.3e-05, "2.300e-05"); ("%3.10e", 3e+56, "3.0000000000e+56");
  ("%3.10f", 20000000000., "20000000000.0000000000");
  ("%3.3f", -3300., "-3300.000"); ("%1.g", 13000., "1e+04");
  ("%1.3g", 2.3e-05, "2.3e-05"); ("%3.10g", 3e+56, "3e+56");
  ("%3.10g", 20000000000., "2e+10"); ("%3.3g", -3300., "-3.3e+03");
  ("%3.3g", -0.0033, "-0.0033"); ("%3.10g", 30000000000., "3e+10");
  ("%3.0g", 30000000000., "3e+10"); ("%3.g", 30000000000., "3e+10");
  ("%3.g", 3., "  3"); ("%1.1g", 2.1, "2"); ("%1.2g", 2.1, "2.1")|]

let float_suites = Mt.[
    "float_nan"
  ]




(* module Mt = Mock_mt *)

let int64_suites = 

  Mt.[


    "i64_simple7", (fun _ -> Eq(Int64.to_string 3333L, "3333"));    
    "i64_simple15", (fun _ ->
        Eq( Int64.to_string  (-1L), "-1"));
    "i64_simple16", (fun _ ->
        Eq( Int64.to_string (-11111L), "-11111"));
 
  ]

let hh = 922337203685477580L (* 2 ^ 63 / 10 *)
let hhh = 1152921504606846976L
let of_string_data =
  [| (0L, "0");
     (3L, "3");
     (33L, "33");
     (333L, "33_3");
     (33333L, "33_33_3");
     (333333333333L, "333333333333");
     (-1L, "0xffff_ffff_ffff_ffff");
     (113L, "0b01110001");
     (1L, "-0xffff_ffff_ffff_ffff")
  |]
  
(* module Mt = Mock_mt *)

let () =
  Mt.from_pair_suites __MODULE__ @@
  suites @


  (Array.mapi (fun i (fmt, f,str_result) -> ({j|loat_format $i|j}  ) , (fun _ -> Mt.Eq(format_float fmt f, str_result))) float_data |> Array.to_list) @
  
  int64_suites @
  (of_string_data
   |>
   Array.mapi (fun i (a,b) ->
       (({j|int64_of_string $i |j}),  fun _ -> Mt.Eq(Int64.of_string b, a) )      
     ) 
   |> Array.to_list  )


    
