(* module Mt = Mock_mt *)
let f (u : nativeint) v = u > v  
let v   = 
  Int64.add (Int64.of_int32 Int32.max_int ) Int64.one
let h = Int64.neg v  
let a  = Int64.of_int32 2147483647l

open Int64

let commutative_add result a b = Mt.Eq((result, result), (add a b, add b a))
let generic_compare = Pervasives.compare

let shift_left_tests =
  (Ext_array_test.range 0 63 |> Array.map (fun i -> Int64.shift_left 1L i) , [|1L; 2L; 4L; 8L; 16L; 32L; 64L; 128L; 256L; 512L; 1024L; 2048L; 4096L;
  8192L; 16384L; 32768L; 65536L; 131072L; 262144L; 524288L; 1048576L;
  2097152L; 4194304L; 8388608L; 16777216L; 33554432L; 67108864L; 134217728L;
  268435456L; 536870912L; 1073741824L; 2147483648L; 4294967296L; 8589934592L;
  17179869184L; 34359738368L; 68719476736L; 137438953472L; 274877906944L;
  549755813888L; 1099511627776L; 2199023255552L; 4398046511104L;
  8796093022208L; 17592186044416L; 35184372088832L; 70368744177664L;
  140737488355328L; 281474976710656L; 562949953421312L; 1125899906842624L;
  2251799813685248L; 4503599627370496L; 9007199254740992L;
  18014398509481984L; 36028797018963968L; 72057594037927936L;
  144115188075855872L; 288230376151711744L; 576460752303423488L;
  1152921504606846976L; 2305843009213693952L; 4611686018427387904L;
  -9223372036854775808L|]) 

let shift_right_tests = 
  (Ext_array_test.range 0 63 |> Array.map (fun i -> Int64.shift_right 0x8000_0000_0000_0000L i),
   [|-9223372036854775808L; -4611686018427387904L; -2305843009213693952L;
  -1152921504606846976L; -576460752303423488L; -288230376151711744L;
  -144115188075855872L; -72057594037927936L; -36028797018963968L;
  -18014398509481984L; -9007199254740992L; -4503599627370496L;
  -2251799813685248L; -1125899906842624L; -562949953421312L;
  -281474976710656L; -140737488355328L; -70368744177664L; -35184372088832L;
  -17592186044416L; -8796093022208L; -4398046511104L; -2199023255552L;
  -1099511627776L; -549755813888L; -274877906944L; -137438953472L;
  -68719476736L; -34359738368L; -17179869184L; -8589934592L; -4294967296L;
  -2147483648L; -1073741824L; -536870912L; -268435456L; -134217728L;
  -67108864L; -33554432L; -16777216L; -8388608L; -4194304L; -2097152L;
  -1048576L; -524288L; -262144L; -131072L; -65536L; -32768L; -16384L; -8192L;
  -4096L; -2048L; -1024L; -512L; -256L; -128L; -64L; -32L; -16L; -8L; -4L;
  -2L; -1L|])

let shift_right_logical_suites = 
  (Ext_array_test.range 0 63 |> Array.map (fun i -> Int64.shift_right_logical 0x8000_0000_0000_0000L i),
   [|-9223372036854775808L; 4611686018427387904L; 2305843009213693952L;
     1152921504606846976L; 576460752303423488L; 288230376151711744L;
     144115188075855872L; 72057594037927936L; 36028797018963968L;
     18014398509481984L; 9007199254740992L; 4503599627370496L;
     2251799813685248L; 1125899906842624L; 562949953421312L; 281474976710656L;
     140737488355328L; 70368744177664L; 35184372088832L; 17592186044416L;
     8796093022208L; 4398046511104L; 2199023255552L; 1099511627776L;
     549755813888L; 274877906944L; 137438953472L; 68719476736L; 34359738368L;
     17179869184L; 8589934592L; 4294967296L; 2147483648L; 1073741824L;
     536870912L; 268435456L; 134217728L; 67108864L; 33554432L; 16777216L;
     8388608L; 4194304L; 2097152L; 1048576L; 524288L; 262144L; 131072L; 65536L;
     32768L; 16384L; 8192L; 4096L; 2048L; 1024L; 512L; 256L; 128L; 64L; 32L;
     16L; 8L; 4L; 2L; 1L|]
  )
let rec fib n a b = if n = 0 then a else  fib (n-1) b (Int64.add a b);;
let rec fac n acc = if n = 0 then acc else fac (n-1) (Int64.mul acc (Int64.of_int n));;
let suites :  Mt.pair_suites = Mt.[
    "add_one", (fun _ -> Eq (v, 2147483648L));
    "add_2", (fun _ -> Eq(4294967294L, Int64.(add a a )));
    "add_3", (fun _ -> Eq(0L, Int64.(add 0L 0L)));
    "add_4", (fun _ -> commutative_add (-2L) (-3L) 1L);
    "add_5", (fun _ -> commutative_add (-3L) (-3L) 0L);
    "add_6", (fun _ -> commutative_add 4L (-3L) 7L);
    "add_7", (fun _ -> commutative_add 0x100000000L 0x80000000L 0x80000000L);
    "add_8", (fun _ -> commutative_add 0x100000000L 0xffffffffL 1L);
    "add_9", (fun _ -> commutative_add 0xffffffffL 0x80000000L 0x7fffffffL);
    "add_10", (fun _ -> commutative_add 0x80000000L 0x80000000L 0L);
    "add_11", (fun _ -> commutative_add 0xffffffffL 0xffffffffL 0L);
    "to_int32", (fun _ -> Eq(3l, Int64.to_int32 3L));
    "to_int", (fun _ -> Eq(3, Int64.to_int 3L));
    "of_int", (fun _ -> Eq(3L, Int64.of_int 3));
    "lognot", (fun _ -> Eq(-3L, Int64.lognot 2L));
    "neg", (fun _ -> Eq(-2L, Int64.neg 2L));
    "sub1", (fun _ -> Eq (2L, Int64.(sub 3L 1L)));
    "xor1", (fun _ -> 
        Eq ((logxor 0xEEFFEEFFL 0xFFEEFFEEL, logxor a 0xEEFFEEFFL), 
            (286331153L, 2432700672L))
      );
    "or", (fun _ -> Eq(logor 0xEEFFEEFFL 0xFFEEFFEEL, 4294967295L)
      );
    "and", (fun _ -> Eq(logand 0xEEFFEEFFL 0xFFEEFFEEL, 4008636142L));
    "lsl", (fun _ -> Eq(
        Array.init 64 (fun i -> i) |> Array.map (fun x -> shift_left 1L x ),
        [|1L; 2L; 4L; 8L; 16L; 32L; 64L; 128L; 256L; 512L; 1024L; 2048L; 4096L;
          8192L; 16384L; 32768L; 65536L; 131072L; 262144L; 524288L; 1048576L;
          2097152L; 4194304L; 8388608L; 16777216L; 33554432L; 67108864L; 134217728L;
          268435456L; 536870912L; 1073741824L; 2147483648L; 4294967296L; 8589934592L;
          17179869184L; 34359738368L; 68719476736L; 137438953472L; 274877906944L;
          549755813888L; 1099511627776L; 2199023255552L; 4398046511104L;
          8796093022208L; 17592186044416L; 35184372088832L; 70368744177664L;
          140737488355328L; 281474976710656L; 562949953421312L; 1125899906842624L;
          2251799813685248L; 4503599627370496L; 9007199254740992L;
          18014398509481984L; 36028797018963968L; 72057594037927936L;
          144115188075855872L; 288230376151711744L; 576460752303423488L;
          1152921504606846976L; 2305843009213693952L; 4611686018427387904L;
          -9223372036854775808L|]        
                       ));
    "lsr", (fun _ -> (Eq (Array.init 64 (fun i -> i) |> Array.map (fun x -> shift_right_logical (-1L) x ),
                          [|-1L; 9223372036854775807L; 4611686018427387903L; 2305843009213693951L;
                            1152921504606846975L; 576460752303423487L; 288230376151711743L;
                            144115188075855871L; 72057594037927935L; 36028797018963967L;
                            18014398509481983L; 9007199254740991L; 4503599627370495L;
                            2251799813685247L; 1125899906842623L; 562949953421311L; 281474976710655L;
                            140737488355327L; 70368744177663L; 35184372088831L; 17592186044415L;
                            8796093022207L; 4398046511103L; 2199023255551L; 1099511627775L;
                            549755813887L; 274877906943L; 137438953471L; 68719476735L; 34359738367L;
                            17179869183L; 8589934591L; 4294967295L; 2147483647L; 1073741823L;
                            536870911L; 268435455L; 134217727L; 67108863L; 33554431L; 16777215L;
                            8388607L; 4194303L; 2097151L; 1048575L; 524287L; 262143L; 131071L; 65535L;
                            32767L; 16383L; 8191L; 4095L; 2047L; 1023L; 511L; 255L; 127L; 63L; 31L;
                            15L; 7L; 3L; 1L|])));
    "asr", (fun _ -> Eq(Array.init 64 (fun i -> i) |> Array.map (fun x -> shift_right (-1L) x ),
                        [|-1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L;
                          -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L;
                          -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L;
                          -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L; -1L;
                          -1L; -1L; -1L; -1L|]
                       ));
    "mul simple", (fun _ -> Eq (6L, mul 3L 2L ));
    "of_int32", (fun _ ->
        Eq(Array.map Int64.of_int32 [|0l;(-2147483648l)|], [|0L; (-2147483648L)|]));
    "to_int32", (fun _ -> 
        Eq(Array.map Int64.to_int32
             [|0L; 0x0000_0000_8000_0000L|], [|0l;-2147483648l|]));
    "discard_sign", (fun _ ->
        Eq(Js_int64.discard_sign (-1L), 0x7fff_ffff_ffff_ffffL));
    "div_mod", (fun _ -> Eq(Js_int64.div_mod 7L 3L , (2L,1L)));
    "to_hex", (fun _ -> Eq(Js_int64.to_hex (-1L), "ffffffffffffffff"));
    "generic_compare", (fun _ ->
        Eq(generic_compare 0x0000_0001_0000_0000L 0x0000_0000_0000_0001L > 0 , true));
    "test_compier_literal", (fun _ ->
        Eq(0x0000_0000_ffff_ffffL, 
           Int64.add (Int64.add 0x0000_0000_7fff_ffffL 0x0000_0000_7fff_ffffL) 1L)        
      )    ;
    "generic_compare2", (fun _ ->
        Eq(generic_compare 0x0000_0000_8000_0000L 0x0000_0000_0000_0001L > 0 , true));
    "shift_left", (fun _ -> 
        Eq(Int64.shift_left 0x00ff_ffffL 8, 0xffff_ff00L));
    "shift_right",(fun _ ->
        Eq(Nativeint.shift_right_logical 0xffff_ffffn 0, 0xffff_ffffn)
      ); 
    "fib_int64", (fun _ -> 
        Eq(fib 1000 1L 2L, -8549408682174725832L)
      );
    "fac_int64", (fun _ -> 
        Eq(fac 30 1L, -8764578968847253504L)
      )
      
]
                                 @ (let (a,b) = shift_left_tests in    
     Ext_array_test.map2i (fun i a b -> Format.asprintf "shift_left_cases %d" i, (fun _ -> Mt.Eq(a,b)) ) a b
     |>  Array.to_list)
                                 @ 
                                 ((let (a,b) = shift_right_tests in    
                                   Ext_array_test.map2i (fun i a b -> Format.asprintf "shift_right_cases %d" i, (fun _ -> Mt.Eq(a,b)) ) a b
                                   |>  Array.to_list))    @
                                 ((let (a,b) = shift_right_logical_suites in    
                                   Ext_array_test.map2i (fun i a b -> Format.asprintf "shift_right_logical_cases %d" i, (fun _ -> Mt.Eq(a,b)) ) a b
                                   |>  Array.to_list)) 





;; Mt.from_pair_suites __FILE__ suites
