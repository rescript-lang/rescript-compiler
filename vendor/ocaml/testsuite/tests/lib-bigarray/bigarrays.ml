open Bigarray
open Printf
open Complex

(* Test harness *)

let error_occurred = ref false

let function_tested = ref ""

let testing_function s =
    function_tested := s;
    print_newline();
    print_string s;
    print_newline()

let test test_number answer correct_answer =
 flush stdout;
 flush stderr;
 if answer <> correct_answer then begin
   eprintf "*** Bad result (%s, test %d)\n" !function_tested test_number;
   flush stderr;
   error_occurred := true
 end else begin
   printf " %d..." test_number
 end

(* One-dimensional arrays *)

(* flambda can cause some of these values not to be reclaimed by the Gc, which
 * can undermine the use of Gc.full_major for the Windows ports. All the tests
 * are wrapped in a non-inlineable function to prevent this behaviour.
 *)
let tests () =
  testing_function "------ Array1 --------";
  testing_function "create/set/get";
  let test_setget kind vals =
    let rec set a i = function
        [] -> ()
      | (v1, v2) :: tl -> a.{i} <- v1; set a (i+1) tl in
    let rec test a i = function
        [] -> true
      | (v1, v2) :: tl -> a.{i} = v2 && test a (i+1) tl in
    let ca = Array1.create kind c_layout (List.length vals) in
    let fa = Array1.create kind fortran_layout (List.length vals) in
    set ca 0 vals;
    set fa 1 vals;
    test ca 0 vals && test fa 1 vals in
  test 1 true
    (test_setget int8_signed
                 [0, 0;
                  123, 123;
                  -123, -123;
                  456, -56;
                  0x101, 1]);
  test 2 true
    (test_setget int8_unsigned
                 [0, 0;
                  123, 123;
                  -123, 133;
                  456, 0xc8;
                  0x101, 1]);
  test 3 true
    (test_setget int16_signed
                 [0, 0;
                  123, 123;
                  -123, -123;
                  31456, 31456;
                  -31456, -31456;
                  65432, -104;
                  0x10001, 1]);
  test 4 true
    (test_setget int16_unsigned
                 [0, 0;
                  123, 123;
                  -123, 65413;
                  31456, 31456;
                  -31456, 34080;
                  65432, 65432;
                  0x10001, 1]);
  test 5 true
    (test_setget int
                 [0, 0;
                  123, 123;
                  -456, -456;
                  max_int, max_int;
                  min_int, min_int;
                  0x12345678, 0x12345678;
                  -0x12345678, -0x12345678]);
  test 6 true
    (test_setget int32
                 [Int32.zero, Int32.zero;
                  Int32.of_int 123, Int32.of_int 123;
                  Int32.of_int (-456), Int32.of_int (-456);
                  Int32.max_int, Int32.max_int;
                  Int32.min_int, Int32.min_int;
                  Int32.of_string "0x12345678", Int32.of_string "0x12345678"]);
  test 7 true
    (test_setget int64
                 [Int64.zero, Int64.zero;
                  Int64.of_int 123, Int64.of_int 123;
                  Int64.of_int (-456), Int64.of_int (-456);
                  Int64.max_int, Int64.max_int;
                  Int64.min_int, Int64.min_int;
                  Int64.of_string "0x123456789ABCDEF0",
                     Int64.of_string "0x123456789ABCDEF0"]);
  test 8 true
    (test_setget nativeint
                 [Nativeint.zero, Nativeint.zero;
                  Nativeint.of_int 123, Nativeint.of_int 123;
                  Nativeint.of_int (-456), Nativeint.of_int (-456);
                  Nativeint.max_int, Nativeint.max_int;
                  Nativeint.min_int, Nativeint.min_int;
                  Nativeint.of_string "0x12345678",
                    Nativeint.of_string "0x12345678"]);
  test 9 true
    (test_setget float32
                 [0.0, 0.0;
                  4.0, 4.0;
                  -0.5, -0.5;
                  655360.0, 655360.0]);
  test 10 true
    (test_setget float64
                 [0.0, 0.0;
                  4.0, 4.0;
                  -0.5, -0.5;
                  1.2345678, 1.2345678;
                  3.1415e10, 3.1415e10]);
  test 11 true
    (test_setget complex32
                 [Complex.zero, Complex.zero;
                  Complex.one, Complex.one;
                  Complex.i, Complex.i;
                  {im = 0.5; re = -2.0}, {im = 0.5; re = -2.0}]);
  test 12 true
    (test_setget complex64
                 [Complex.zero, Complex.zero;
                  Complex.one, Complex.one;
                  Complex.i, Complex.i;
                  {im=0.5;re= -2.0}, {im=0.5;re= -2.0};
                  {im=3.1415;re=1.2345678}, {im=3.1415;re=1.2345678}]);

  let from_list kind vals =
    let a = Array1.create kind c_layout (List.length vals) in
    let rec set i = function
        [] -> ()
      | hd :: tl -> a.{i} <- hd; set (i+1) tl in
    set 0 vals;
    a in
  let from_list_fortran kind vals =
    let a = Array1.create kind fortran_layout (List.length vals) in
    let rec set i = function
        [] -> ()
      | hd :: tl -> a.{i} <- hd; set (i+1) tl in
    set 1 vals;
    a in

  (* Test indexing arrays.  This test has to be copy-pasted, otherwise
     indexing may not use the optimizations in
     Cmmgen.bigarray_indexing. *)
  begin
    let v = 123 in
    let cb = Array1.create int8_signed c_layout 1000 in
    let fb = Array1.create int8_signed fortran_layout 1000 in
    Array1.fill cb v;
    Array1.fill fb v;
    let return = ref true in
    for i = 1 to 99 do
      let i = i * 10 in
      return := !return
        && Array1.unsafe_get cb (i - 10) = v
        && Array1.unsafe_get cb (i     ) = v
        && Array1.unsafe_get cb (i +  9) = v
        && Array1.unsafe_get fb (i -  9) = v
        && Array1.unsafe_get fb (i     ) = v
        && Array1.unsafe_get fb (i + 10) = v
    done;
    test 13 true !return
  end;
  begin
    let v = 123 in
    let cb = Array1.create int16_unsigned c_layout 1000 in
    let fb = Array1.create int16_unsigned fortran_layout 1000 in
    Array1.fill cb v;
    Array1.fill fb v;
    let return = ref true in
    for i = 1 to 99 do
      let i = i * 10 in
      return := !return
        && Array1.unsafe_get cb (i - 10) = v
        && Array1.unsafe_get cb (i     ) = v
        && Array1.unsafe_get cb (i +  9) = v
        && Array1.unsafe_get fb (i -  9) = v
        && Array1.unsafe_get fb (i     ) = v
        && Array1.unsafe_get fb (i + 10) = v
    done;
    test 14 true !return
  end;
  begin
    let v = 123. in
    let cb = Array1.create float32 c_layout 1000 in
    let fb = Array1.create float32 fortran_layout 1000 in
    Array1.fill cb v;
    Array1.fill fb v;
    let return = ref true in
    for i = 1 to 99 do
      let i = i * 10 in
      return := !return
        && Array1.unsafe_get cb (i - 10) = v
        && Array1.unsafe_get cb (i     ) = v
        && Array1.unsafe_get cb (i +  9) = v
        && Array1.unsafe_get fb (i -  9) = v
        && Array1.unsafe_get fb (i     ) = v
        && Array1.unsafe_get fb (i + 10) = v
    done;
    test 15 true !return
  end;

  begin
    let v = 123. in
    let cb = Array1.create float64 c_layout 1000 in
    let fb = Array1.create float64 fortran_layout 1000 in
    Array1.fill cb v;
    Array1.fill fb v;
    let return = ref true in
    for i = 1 to 99 do
      let i = i * 10 in
      return := !return
        && Array1.unsafe_get cb (i - 10) = v
        && Array1.unsafe_get cb (i     ) = v
        && Array1.unsafe_get cb (i +  9) = v
        && Array1.unsafe_get fb (i -  9) = v
        && Array1.unsafe_get fb (i     ) = v
        && Array1.unsafe_get fb (i + 10) = v
    done;
    test 16 true !return
  end;

  testing_function "set/get (specialized)";
  let a = Array1.create int c_layout 3 in
  for i = 0 to 2 do a.{i} <- i done;
  for i = 0 to 2 do test (i+1) a.{i} i done;
  test 4 true (try ignore a.{3}; false with Invalid_argument _ -> true);
  test 5 true (try ignore a.{-1}; false with Invalid_argument _ -> true);

  let b = Array1.create float64 fortran_layout 3 in
  for i = 1 to 3 do b.{i} <- float i done;
  for i = 1 to 3 do test (5 + i) b.{i} (float i) done;
  test 8 true (try ignore b.{4}; false with Invalid_argument _ -> true);
  test 9 true (try ignore b.{0}; false with Invalid_argument _ -> true);

  let c = Array1.create complex64 c_layout 3 in
  for i = 0 to 2 do c.{i} <- {re=float i; im=0.0} done;
  for i = 0 to 2 do test (10 + i) c.{i} {re=float i; im=0.0} done;
  test 13 true (try ignore c.{3}; false with Invalid_argument _ -> true);
  test 14 true (try ignore c.{-1}; false with Invalid_argument _ -> true);

  let d = Array1.create complex32 fortran_layout 3 in
  for i = 1 to 3 do d.{i} <- {re=float i; im=0.0} done;
  for i = 1 to 3 do test (14 + i) d.{i} {re=float i; im=0.0} done;
  test 18 true (try ignore d.{4}; false with Invalid_argument _ -> true);
  test 19 true (try ignore d.{0}; false with Invalid_argument _ -> true);

  testing_function "set/get (unsafe, specialized)";
  let a = Array1.create int c_layout 3 in
  for i = 0 to 2 do Array1.unsafe_set a i i done;
  for i = 0 to 2 do test (i+1) (Array1.unsafe_get a i) i done;

  let b = Array1.create float64 fortran_layout 3 in
  for i = 1 to 3 do Array1.unsafe_set b i (float i) done;
  for i = 1 to 3 do test (5 + i) (Array1.unsafe_get b i) (float i) done;

  testing_function "comparisons";
  let normalize_comparison n =
    if n = 0 then 0 else if n < 0 then -1 else 1 in
  test 1 0 (normalize_comparison (compare
     (from_list int8_signed [1;2;3;-4;127;-128])
     (from_list int8_signed [1;2;3;-4;127;-128])));
  test 2 (-1) (normalize_comparison (compare
     (from_list int8_signed [1;2;3;-4;127;-128])
     (from_list int8_signed [1;2;3;4;127;-128])));
  test 3 1 (normalize_comparison (compare
     (from_list int8_signed [1;2;3;-4;127;-128])
     (from_list int8_signed [1;2;3;-4;42;-128])));
  test 4 (-1) (normalize_comparison (compare
     (from_list int8_signed [1;2;3;-4])
     (from_list int8_signed [1;2;3;4;127;-128])));
  test 5 1 (normalize_comparison (compare
     (from_list int8_signed [1;2;3;4;127;-128])
     (from_list int8_signed [1;2;3;-4])));

  test 6 0 (normalize_comparison (compare
     (from_list int8_unsigned [1;2;3;-4;127;-128])
     (from_list int8_unsigned [1;2;3;-4;127;-128])));
  test 7 1 (normalize_comparison (compare
     (from_list int8_unsigned [1;2;3;-4;127;-128])
     (from_list int8_unsigned [1;2;3;4;127;-128])));
  test 8 1 (normalize_comparison (compare
     (from_list int8_unsigned [1;2;3;-4;127;-128])
     (from_list int8_unsigned [1;2;3;-4;42;-128])));

  test 9 0 (normalize_comparison (compare
     (from_list int16_signed [1;2;3;-4;127;-128])
     (from_list int16_signed [1;2;3;-4;127;-128])));
  test 10 (-1) (normalize_comparison (compare
     (from_list int16_signed [1;2;3;-4;127;-128])
     (from_list int16_signed [1;2;3;4;127;-128])));
  test 11 1 (normalize_comparison (compare
     (from_list int16_signed [1;2;3;-4;127;-128])
     (from_list int16_signed [1;2;3;-4;42;-128])));

  test 12 0 (normalize_comparison (compare
     (from_list int16_unsigned [1;2;3;-4;127;-128])
     (from_list int16_unsigned [1;2;3;-4;127;-128])));
  test 13 (-1) (normalize_comparison (compare
     (from_list int16_unsigned [1;2;3;4;127;-128])
     (from_list int16_unsigned [1;2;3;0xFFFF;127;-128])));
  test 14 1 (normalize_comparison (compare
     (from_list int16_unsigned [1;2;3;-4;127;-128])
     (from_list int16_unsigned [1;2;3;-4;42;-128])));

  test 15 0 (normalize_comparison (compare
     (from_list int [1;2;3;-4;127;-128])
     (from_list int [1;2;3;-4;127;-128])));
  test 16 (-1) (normalize_comparison (compare
     (from_list int [1;2;3;-4;127;-128])
     (from_list int [1;2;3;4;127;-128])));
  test 17 1 (normalize_comparison (compare
     (from_list int [1;2;3;-4;127;-128])
     (from_list int [1;2;3;-4;42;-128])));

  test 18 0 (normalize_comparison (compare
     (from_list int32 (List.map Int32.of_int [1;2;3;-4;127;-128]))
     (from_list int32 (List.map Int32.of_int [1;2;3;-4;127;-128]))));
  test 19 (-1) (normalize_comparison (compare
     (from_list int32 (List.map Int32.of_int [1;2;3;-4;127;-128]))
     (from_list int32 (List.map Int32.of_int [1;2;3;4;127;-128]))));
  test 20 1 (normalize_comparison (compare
     (from_list int32 (List.map Int32.of_int [1;2;3;-4;127;-128]))
     (from_list int32 (List.map Int32.of_int [1;2;3;-4;42;-128]))));

  test 21 0 (normalize_comparison (compare
     (from_list int64 (List.map Int64.of_int [1;2;3;-4;127;-128]))
     (from_list int64 (List.map Int64.of_int [1;2;3;-4;127;-128]))));
  test 22 (-1) (normalize_comparison (compare
     (from_list int64 (List.map Int64.of_int [1;2;3;-4;127;-128]))
     (from_list int64 (List.map Int64.of_int [1;2;3;4;127;-128]))));
  test 23 1 (normalize_comparison (compare
     (from_list int64 (List.map Int64.of_int [1;2;3;-4;127;-128]))
     (from_list int64 (List.map Int64.of_int [1;2;3;-4;42;-128]))));

  test 24 0 (normalize_comparison (compare
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;-4;127;-128]))
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;-4;127;-128]))));
  test 25 (-1) (normalize_comparison (compare
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;-4;127;-128]))
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;4;127;-128]))));
  test 26 1 (normalize_comparison (compare
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;-4;127;-128]))
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;-4;42;-128]))));

  test 27 0 (normalize_comparison (compare
     (from_list float32 [0.0; 0.25; -4.0; 3.141592654])
     (from_list float32 [0.0; 0.25; -4.0; 3.141592654])));
  test 28 (-1) (normalize_comparison (compare
     (from_list float32 [0.0; 0.25; -4.0])
     (from_list float32 [0.0; 0.25; 3.14159])));
  test 29 1 (normalize_comparison (compare
     (from_list float32 [0.0; 2.718; -4.0])
     (from_list float32 [0.0; 0.25; 3.14159])));

  test 30 0 (normalize_comparison (compare
     (from_list float64 [0.0; 0.25; -4.0; 3.141592654])
     (from_list float64 [0.0; 0.25; -4.0; 3.141592654])));
  test 31 (-1) (normalize_comparison (compare
     (from_list float64 [0.0; 0.25; -4.0])
     (from_list float64 [0.0; 0.25; 3.14159])));
  test 32 1 (normalize_comparison (compare
     (from_list float64 [0.0; 2.718; -4.0])
     (from_list float64 [0.0; 0.25; 3.14159])));

  test 44 0 (normalize_comparison (compare
     (from_list complex32 [Complex.zero; Complex.one; Complex.i])
     (from_list complex32 [Complex.zero; Complex.one; Complex.i])));
  test 45 (-1) (normalize_comparison (compare
     (from_list complex32 [Complex.zero; Complex.one; Complex.i])
     (from_list complex32 [Complex.zero; Complex.one; Complex.one])));
  test 46 1 (normalize_comparison (compare
     (from_list complex32 [Complex.zero; Complex.one; Complex.one])
     (from_list complex32 [Complex.zero; Complex.one; Complex.i])));

  test 47 0 (normalize_comparison (compare
     (from_list complex64 [Complex.zero; Complex.one; Complex.i])
     (from_list complex64 [Complex.zero; Complex.one; Complex.i])));
  test 48 (-1) (normalize_comparison (compare
     (from_list complex64 [Complex.zero; Complex.one; Complex.i])
     (from_list complex64 [Complex.zero; Complex.one; Complex.one])));
  test 49 1 (normalize_comparison (compare
     (from_list complex64 [Complex.zero; Complex.one; Complex.one])
     (from_list complex64 [Complex.zero; Complex.one; Complex.i])));

  testing_function "dim";
  test 1 (Array1.dim (from_list int [1;2;3;4;5])) 5;
  test 2 (Array1.dim (from_list_fortran int [1;2;3])) 3;

  testing_function "size_in_bytes_one";
  test 1 (Array1.size_in_bytes (from_list int [1;2;3;4;5]))
    (5 * (kind_size_in_bytes int));
  test 2 (Array1.size_in_bytes (from_list int [])) 0;
  let int64list = (from_list int64 (List.map Int64.of_int [1;2;3;4;5])) in
  test 3 (Array1.size_in_bytes int64list) (5 * (kind_size_in_bytes int64));
  test 4 (Array1.size_in_bytes (from_list int64 (List.map Int64.of_int []))) 0;

  testing_function "kind & layout";
  let a = from_list int [1;2;3] in
  test 1 (Array1.kind a) int;
  test 2 (Array1.layout a) c_layout;
  let a = from_list_fortran float32 [1.0;2.0;3.0] in
  test 1 (Array1.kind a) float32;
  test 2 (Array1.layout a) fortran_layout;

  testing_function "sub";
  let a = from_list int [1;2;3;4;5;6;7;8] in
  test 1 (Array1.sub a 2 5)
         (from_list int [3;4;5;6;7]);
  test 2 (Array1.sub a 0 2)
         (from_list int [1;2]);
  test 3 (Array1.sub a 0 8)
         (from_list int [1;2;3;4;5;6;7;8]);
  let a = from_list float64 [1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0] in
  test 4 (Array1.sub a 2 5)
         (from_list float64 [3.0;4.0;5.0;6.0;7.0]);
  test 5 (Array1.sub a 0 2)
         (from_list float64 [1.0;2.0]);
  test 6 (Array1.sub a 0 8)
         (from_list float64 [1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0]);
  let a = from_list_fortran float64 [1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0] in
  test 7 (Array1.sub a 2 5)
         (from_list_fortran float64 [2.0;3.0;4.0;5.0;6.0]);
  test 8 (Array1.sub a 1 2)
         (from_list_fortran float64 [1.0;2.0]);
  test 9 (Array1.sub a 1 8)
         (from_list_fortran float64 [1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0]);
  Gc.full_major();  (* test GC of proxies *)

  testing_function "blit, fill";
  let test_blit_fill kind data initval ofs len =
    let a = from_list kind data in
    let b = Array1.create kind c_layout (List.length data) in
    Array1.blit a b;
    (a = b) &&
    (Array1.fill (Array1.sub b ofs len) initval;
     let rec check i = function
         [] -> true
       | hd :: tl -> b.{i} = (if i >= ofs && i < ofs + len
                                       then initval else hd)
                     && check (i+1) tl
     in check 0 data) in
  test 1 true (test_blit_fill int8_signed [1;2;5;8;-100;127] 7 3 2);
  test 2 true (test_blit_fill int8_unsigned [1;2;5;8;-100;212] 7 3 2);
  test 3 true (test_blit_fill int16_signed [1;2;5;8;-100;212] 7 3 2);
  test 4 true (test_blit_fill int16_unsigned [1;2;5;8;-100;212] 7 3 2);
  test 5 true (test_blit_fill int [1;2;5;8;-100;212] 7 3 2);
  test 6 true (test_blit_fill int32 (List.map Int32.of_int [1;2;5;8;-100;212])
                                    (Int32.of_int 7) 3 2);
  test 7 true (test_blit_fill int64 (List.map Int64.of_int [1;2;5;8;-100;212])
                                    (Int64.of_int 7) 3 2);
  test 8 true (test_blit_fill nativeint
                             (List.map Nativeint.of_int [1;2;5;8;-100;212])
                             (Nativeint.of_int 7) 3 2);
  test 9 true (test_blit_fill float32 [1.0;2.0;0.5;0.125;256.0;512.0]
                             0.25 3 2);
  test 10 true (test_blit_fill float64 [1.0;2.0;5.0;8.123;-100.456;212e19]
                             3.1415 3 2);
  test 11 true (test_blit_fill complex32 [Complex.zero; Complex.one; Complex.i]
                             Complex.i 1 1);
  test 12 true (test_blit_fill complex64 [Complex.zero; Complex.one; Complex.i]
                             Complex.i 1 1);
  testing_function "slice";
  let a = Array1.of_array int c_layout [| 5; 4; 3 |] in
  test 1 (Array1.slice a 0) (Array0.of_value int c_layout 5);
  test 2 (Array1.slice a 1) (Array0.of_value int c_layout 4);
  test 3 (Array1.slice a 2) (Array0.of_value int c_layout 3);
  let a = Array1.of_array int fortran_layout [| 5; 4; 3 |] in
  test 6 (Array1.slice a 1) (Array0.of_value int fortran_layout 5);
  test 7 (Array1.slice a 2) (Array0.of_value int fortran_layout 4);
  test 8 (Array1.slice a 3) (Array0.of_value int fortran_layout 3);


(* Bi-dimensional arrays *)

  print_newline();
  testing_function "------ Array2 --------";
  testing_function "create/set/get";
  let make_array2 kind layout ind0 dim1 dim2 fromint =
    let a = Array2.create kind layout dim1 dim2 in
    for i = ind0 to dim1 - 1 + ind0 do
      for j = ind0 to dim2 - 1 + ind0 do
        a.{i,j} <- (fromint (i * 1000 + j))
      done
    done;
    a in
  let check_array2 a ind0 dim1 dim2 fromint =
    try
      for i = ind0 to dim1 - 1 + ind0 do
        for j = ind0 to dim2 - 1 + ind0 do
          if a.{i,j} <> (fromint (i * 1000 + j)) then raise Exit
        done
      done;
      true
    with Exit -> false in
  let id x = x in
  test 1 true
    (check_array2 (make_array2 int16_signed c_layout 0 10 20 id) 0 10 20 id);
  test 2 true
    (check_array2 (make_array2 int c_layout 0 10 20 id) 0 10 20 id);
  test 3 true
    (check_array2 (make_array2 int32 c_layout 0 10 20 Int32.of_int)
                  0 10 20 Int32.of_int);
  test 4 true
    (check_array2 (make_array2 float32 c_layout 0 10 20 float)
                  0 10 20 float);
  test 5 true
    (check_array2 (make_array2 float64 c_layout 0 10 20 float)
                  0 10 20 float);
  test 6 true
    (check_array2 (make_array2 int16_signed fortran_layout 1 10 20 id)
                  1 10 20 id);
  test 7 true
    (check_array2 (make_array2 int fortran_layout 1 10 20 id) 1 10 20 id);
  test 8 true
    (check_array2 (make_array2 int32 fortran_layout 1 10 20 Int32.of_int)
                  1 10 20 Int32.of_int);
  test 9 true
    (check_array2 (make_array2 float32 fortran_layout 1 10 20 float)
                  1 10 20 float);
  test 10 true
    (check_array2 (make_array2 float64 fortran_layout 1 10 20 float)
                  1 10 20 float);
  let makecomplex i = {re = float i; im = float (-i)} in
  test 11 true
    (check_array2 (make_array2 complex32 c_layout 0 10 20 makecomplex)
                  0 10 20 makecomplex);
  test 12 true
    (check_array2 (make_array2 complex64 c_layout 0 10 20 makecomplex)
                  0 10 20 makecomplex);
  test 13 true
    (check_array2 (make_array2 complex32 fortran_layout 1 10 20 makecomplex)
                  1 10 20 makecomplex);
  test 14 true
    (check_array2 (make_array2 complex64 fortran_layout 1 10 20 makecomplex)
                  1 10 20 makecomplex);

  testing_function "set/get (specialized)";
  let a = Array2.create int16_signed c_layout 3 3 in
  for i = 0 to 2 do for j = 0 to 2 do a.{i,j} <- i-j done done;
  let ok = ref true in
  for i = 0 to 2 do
    for j = 0 to 2 do if a.{i,j} <> i-j then ok := false done
  done;
  test 1 true !ok;
  test 2 true (try ignore a.{3,0}; false with Invalid_argument _ -> true);
  test 3 true (try ignore a.{-1,0}; false with Invalid_argument _ -> true);
  test 4 true (try ignore a.{0,3}; false with Invalid_argument _ -> true);
  test 5 true (try ignore a.{0,-1}; false with Invalid_argument _ -> true);

  let b = Array2.create float32 fortran_layout 3 3 in
  for i = 1 to 3 do for j = 1 to 3 do b.{i,j} <- float(i-j) done done;
  let ok = ref true in
  for i = 1 to 3 do
    for j = 1 to 3 do if b.{i,j} <> float(i-j) then ok := false done
  done;
  test 6 true !ok;
  test 7 true (try ignore b.{4,1}; false with Invalid_argument _ -> true);
  test 8 true (try ignore b.{0,1}; false with Invalid_argument _ -> true);
  test 9 true (try ignore b.{1,4}; false with Invalid_argument _ -> true);
  test 10 true (try ignore b.{1,0}; false with Invalid_argument _ -> true);

  testing_function "set/get (unsafe, specialized)";
  let a = Array2.create int16_signed c_layout 3 3 in
  for i = 0 to 2 do for j = 0 to 2 do Array2.unsafe_set a i j (i-j) done done;
  let ok = ref true in
  for i = 0 to 2 do
    for j = 0 to 2 do if Array2.unsafe_get a i j <> i-j then ok := false done
  done;
  test 1 true !ok;

  let b = Array2.create float32 fortran_layout 3 3 in
  for i = 1 to 3 do
    for j = 1 to 3 do Array2.unsafe_set b i j (float(i-j)) done
  done;
  let ok = ref true in
  for i = 1 to 3 do
    for j = 1 to 3 do
      if Array2.unsafe_get b i j <> float(i-j) then ok := false
    done
  done;
  test 2 true !ok;

  testing_function "dim";
  let a = (make_array2 int c_layout 0 4 6 id) in
  test 1 (Array2.dim1 a) 4;
  test 2 (Array2.dim2 a) 6;
  let b =  (make_array2 int fortran_layout 1 4 6 id) in
  test 3 (Array2.dim1 b) 4;
  test 4 (Array2.dim2 b) 6;

  testing_function "size_in_bytes_two";
  let a = Array2.create int c_layout 4 6 in
  test 1 (Array2.size_in_bytes a) (24 * (kind_size_in_bytes int));

  testing_function "sub";
  let a = make_array2 int c_layout 0 5 3 id in
  let b = Array2.sub_left a 2 2 in
  test 1 true
         (b.{0,0} = 2000 &&
          b.{0,1} = 2001 &&
          b.{0,2} = 2002 &&
          b.{1,0} = 3000 &&
          b.{1,1} = 3001 &&
          b.{1,2} = 3002);
  let a = make_array2 int fortran_layout 1 5 3 id in
  let b = Array2.sub_right a 2 2 in
  test 2 true
         (b.{1,1} = 1002 &&
          b.{1,2} = 1003 &&
          b.{2,1} = 2002 &&
          b.{2,2} = 2003 &&
          b.{3,1} = 3002 &&
          b.{3,2} = 3003 &&
          b.{4,1} = 4002 &&
          b.{4,2} = 4003 &&
          b.{5,1} = 5002 &&
          b.{5,2} = 5003);

  testing_function "slice";
  let a = make_array2 int c_layout 0 5 3 id in
  test 1 (Array2.slice_left a 0) (from_list int [0;1;2]);
  test 2 (Array2.slice_left a 1) (from_list int [1000;1001;1002]);
  test 3 (Array2.slice_left a 2) (from_list int [2000;2001;2002]);
  test 4 (Array2.slice_left a 3) (from_list int [3000;3001;3002]);
  test 5 (Array2.slice_left a 4) (from_list int [4000;4001;4002]);
  let a = make_array2 int fortran_layout 1 5 3 id in
  test 6 (Array2.slice_right a 1)
       (from_list_fortran int [1001;2001;3001;4001;5001]);
  test 7 (Array2.slice_right a 2)
       (from_list_fortran int [1002;2002;3002;4002;5002]);
  test 8 (Array2.slice_right a 3)
       (from_list_fortran int [1003;2003;3003;4003;5003]);

(* Tri-dimensional arrays *)

  print_newline();
  testing_function "------ Array3 --------";
  testing_function "create/set/get";
  let make_array3 kind layout ind0 dim1 dim2 dim3 fromint =
    let a = Array3.create kind layout dim1 dim2 dim3 in
    for i = ind0 to dim1 - 1 + ind0 do
      for j = ind0 to dim2 - 1 + ind0 do
        for k = ind0 to dim3 - 1 + ind0 do
          a.{i, j, k} <- (fromint (i * 100 + j * 10 + k))
        done
      done
    done;
    a in
  let check_array3 a ind0 dim1 dim2 dim3 fromint =
    try
      for i = ind0 to dim1 - 1 + ind0 do
        for j = ind0 to dim2 - 1 + ind0 do
          for k = ind0 to dim3 - 1 + ind0 do
            if a.{i, j, k} <> (fromint (i * 100 + j * 10 + k))
            then raise Exit
          done
        done
      done;
      true
    with Exit -> false in
  let id x = x in
  test 1 true
    (check_array3 (make_array3 int16_signed c_layout 0 4 5 6 id) 0 4 5 6 id);
  test 2 true
    (check_array3 (make_array3 int c_layout 0 4 5 6 id) 0 4 5 6 id);
  test 3 true
    (check_array3 (make_array3 int32 c_layout 0 4 5 6 Int32.of_int)
                  0 4 5 6 Int32.of_int);
  test 4 true
    (check_array3 (make_array3 float32 c_layout 0 4 5 6 float)
                  0 4 5 6 float);
  test 5 true
    (check_array3 (make_array3 float64 c_layout 0 4 5 6 float)
                  0 4 5 6 float);
  test 6 true
    (check_array3 (make_array3 int16_signed fortran_layout 1 4 5 6 id)
                  1 4 5 6 id);
  test 7 true
    (check_array3 (make_array3 int fortran_layout 1 4 5 6 id) 1 4 5 6 id);
  test 8 true
    (check_array3 (make_array3 int32 fortran_layout 1 4 5 6 Int32.of_int)
                  1 4 5 6 Int32.of_int);
  test 9 true
    (check_array3 (make_array3 float32 fortran_layout 1 4 5 6 float)
                  1 4 5 6 float);
  test 10 true
    (check_array3 (make_array3 float64 fortran_layout 1 4 5 6 float)
                  1 4 5 6 float);
  test 11 true
    (check_array3 (make_array3 complex32 c_layout 0 4 5 6 makecomplex)
                  0 4 5 6 makecomplex);
  test 12 true
    (check_array3 (make_array3 complex64 c_layout 0 4 5 6 makecomplex)
                  0 4 5 6 makecomplex);
  test 13 true
    (check_array3 (make_array3 complex32 fortran_layout 1 4 5 6 makecomplex)
                  1 4 5 6 makecomplex);
  test 14 true
    (check_array3 (make_array3 complex64 fortran_layout 1 4 5 6 makecomplex)
                  1 4 5 6 makecomplex);


  testing_function "set/get (specialized)";
  let a = Array3.create int32 c_layout 2 3 4 in
  for i = 0 to 1 do for j = 0 to 2 do for k = 0 to 3 do
     a.{i,j,k} <- Int32.of_int((i lsl 4) + (j lsl 2) + k)
  done done done;
  let ok = ref true in
  for i = 0 to 1 do for j = 0 to 2 do for k = 0 to 3 do
     if Int32.to_int a.{i,j,k} <> (i lsl 4) + (j lsl 2) + k then ok := false
  done done done;
  test 1 true !ok;

  let b = Array3.create int64 fortran_layout 2 3 4 in
  for i = 1 to 2 do for j = 1 to 3 do for k = 1 to 4 do
     b.{i,j,k} <- Int64.of_int((i lsl 4) + (j lsl 2) + k)
  done done done;
  let ok = ref true in
  for i = 1 to 2 do for j = 1 to 3 do for k = 1 to 4 do
     if Int64.to_int b.{i,j,k} <> (i lsl 4) + (j lsl 2) + k then ok := false
  done done done;
  test 2 true !ok;

  testing_function "set/get (unsafe, specialized)";
  let a = Array3.create int32 c_layout 2 3 4 in
  for i = 0 to 1 do for j = 0 to 2 do for k = 0 to 3 do
     Array3.unsafe_set a i j k (Int32.of_int((i lsl 4) + (j lsl 2) + k))
  done done done;
  let ok = ref true in
  for i = 0 to 1 do for j = 0 to 2 do for k = 0 to 3 do
     if Int32.to_int (Array3.unsafe_get a i j k) <> (i lsl 4) + (j lsl 2) + k
     then ok := false
  done done done;
  test 1 true !ok;

  testing_function "dim";
  let a = (make_array3 int c_layout 0 4 5 6 id) in
  test 1 (Array3.dim1 a) 4;
  test 2 (Array3.dim2 a) 5;
  test 3 (Array3.dim3 a) 6;
  let b =  (make_array3 int fortran_layout 1 4 5 6 id) in
  test 4 (Array3.dim1 b) 4;
  test 5 (Array3.dim2 b) 5;
  test 6 (Array3.dim3 b) 6;

  testing_function "size_in_bytes_three";
  let a = Array3.create int c_layout 4 5 6 in
  test 1 (Array3.size_in_bytes a) (120 * (kind_size_in_bytes int));

  testing_function "slice1";
  let a = make_array3 int c_layout 0 3 3 3 id in
  test 1 (Array3.slice_left_1 a 0 0) (from_list int [0;1;2]);
  test 2 (Array3.slice_left_1 a 0 1) (from_list int [10;11;12]);
  test 3 (Array3.slice_left_1 a 0 2) (from_list int [20;21;22]);
  test 4 (Array3.slice_left_1 a 1 1) (from_list int [110;111;112]);
  test 5 (Array3.slice_left_1 a 2 1) (from_list int [210;211;212]);
  let a = make_array3 int fortran_layout 1 3 3 3 id in
  test 6 (Array3.slice_right_1 a 1 2) (from_list_fortran int [112;212;312]);
  test 7 (Array3.slice_right_1 a 3 1) (from_list_fortran int [131;231;331]);

  testing_function "size_in_bytes_general";
  let a = Genarray.create int c_layout [|2;2;2;2;2|] in
  test 1 (Genarray.size_in_bytes a) (32 * (kind_size_in_bytes int));

(* Zero-dimensional arrays *)
  testing_function "------ Array0 --------";
  testing_function "create/set/get";
  let test_setget kind vals =
    List.for_all (fun (v1, v2) ->
      let ca = Array0.create kind c_layout in
      let fa = Array0.create kind fortran_layout in
      Array0.set ca v1;
      Array0.set fa v1;
      Array0.get ca = v2 && Array0.get fa = v2) vals in
  test 1 true
    (test_setget int8_signed
                 [0, 0;
                  123, 123;
                  -123, -123;
                  456, -56;
                  0x101, 1]);
  test 2 true
    (test_setget int8_unsigned
                 [0, 0;
                  123, 123;
                  -123, 133;
                  456, 0xc8;
                  0x101, 1]);
  test 3 true
    (test_setget int16_signed
                 [0, 0;
                  123, 123;
                  -123, -123;
                  31456, 31456;
                  -31456, -31456;
                  65432, -104;
                  0x10001, 1]);
  test 4 true
    (test_setget int16_unsigned
                 [0, 0;
                  123, 123;
                  -123, 65413;
                  31456, 31456;
                  -31456, 34080;
                  65432, 65432;
                  0x10001, 1]);
  test 5 true
    (test_setget int
                 [0, 0;
                  123, 123;
                  -456, -456;
                  max_int, max_int;
                  min_int, min_int;
                  0x12345678, 0x12345678;
                  -0x12345678, -0x12345678]);
  test 6 true
    (test_setget int32
                 [Int32.zero, Int32.zero;
                  Int32.of_int 123, Int32.of_int 123;
                  Int32.of_int (-456), Int32.of_int (-456);
                  Int32.max_int, Int32.max_int;
                  Int32.min_int, Int32.min_int;
                  Int32.of_string "0x12345678", Int32.of_string "0x12345678"]);
  test 7 true
    (test_setget int64
                 [Int64.zero, Int64.zero;
                  Int64.of_int 123, Int64.of_int 123;
                  Int64.of_int (-456), Int64.of_int (-456);
                  Int64.max_int, Int64.max_int;
                  Int64.min_int, Int64.min_int;
                  Int64.of_string "0x123456789ABCDEF0",
                     Int64.of_string "0x123456789ABCDEF0"]);
  test 8 true
    (test_setget nativeint
                 [Nativeint.zero, Nativeint.zero;
                  Nativeint.of_int 123, Nativeint.of_int 123;
                  Nativeint.of_int (-456), Nativeint.of_int (-456);
                  Nativeint.max_int, Nativeint.max_int;
                  Nativeint.min_int, Nativeint.min_int;
                  Nativeint.of_string "0x12345678",
                    Nativeint.of_string "0x12345678"]);
  test 9 true
    (test_setget float32
                 [0.0, 0.0;
                  4.0, 4.0;
                  -0.5, -0.5;
                  655360.0, 655360.0]);
  test 10 true
    (test_setget float64
                 [0.0, 0.0;
                  4.0, 4.0;
                  -0.5, -0.5;
                  1.2345678, 1.2345678;
                  3.1415e10, 3.1415e10]);
  test 11 true
    (test_setget complex32
                 [Complex.zero, Complex.zero;
                  Complex.one, Complex.one;
                  Complex.i, Complex.i;
                  {im = 0.5; re = -2.0}, {im = 0.5; re = -2.0}]);
  test 12 true
    (test_setget complex64
                 [Complex.zero, Complex.zero;
                  Complex.one, Complex.one;
                  Complex.i, Complex.i;
                  {im=0.5;re= -2.0}, {im=0.5;re= -2.0};
                  {im=3.1415;re=1.2345678}, {im=3.1415;re=1.2345678}]);


(* Kind size *)
  testing_function "kind_size_in_bytes";
  let arr1 = Array1.create Float32 c_layout 1 in
  test 1 (kind_size_in_bytes Float32) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Float64 c_layout 1 in
  test 2 (kind_size_in_bytes Float64) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Int8_signed c_layout 1 in
  test 3 (kind_size_in_bytes Int8_signed) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Int8_unsigned c_layout 1 in
  test 4 (kind_size_in_bytes Int8_unsigned) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Int16_signed c_layout 1 in
  test 5 (kind_size_in_bytes Int16_signed) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Int16_unsigned c_layout 1 in
  test 6 (kind_size_in_bytes Int16_unsigned) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Int32 c_layout 1 in
  test 7 (kind_size_in_bytes Int32) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Int64 c_layout 1 in
  test 8 (kind_size_in_bytes Int64) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Int c_layout 1 in
  test 9 (kind_size_in_bytes Int) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Nativeint c_layout 1 in
  test 10 (kind_size_in_bytes Nativeint) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Complex32 c_layout 1 in
  test 11 (kind_size_in_bytes Complex32) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Complex64 c_layout 1 in
  test 12 (kind_size_in_bytes Complex64) (Array1.size_in_bytes arr1);
  let arr1 = Array1.create Char c_layout 1 in
  test 13 (kind_size_in_bytes Char) (Array1.size_in_bytes arr1);

(* Reshaping *)
  print_newline();
  testing_function "------ Reshaping --------";
  testing_function "reshape_1";
  let a = make_array2 int c_layout 0 3 4 id in
  let b = make_array2 int fortran_layout 1 3 4 id in
  let c = reshape_1 (genarray_of_array2 a) 12 in
  test 1 c (from_list int [0;1;2;3;1000;1001;1002;1003;2000;2001;2002;2003]);
  let d = reshape_1 (genarray_of_array2 b) 12 in
  test 2 d (from_list_fortran int
              [1001;2001;3001;1002;2002;3002;1003;2003;3003;1004;2004;3004]);
  testing_function "reshape_2";
  let c = reshape_2 (genarray_of_array2 a) 4 3 in
  test 1 (Array2.slice_left c 0) (from_list int [0;1;2]);
  test 2 (Array2.slice_left c 1) (from_list int [3;1000;1001]);
  test 3 (Array2.slice_left c 2) (from_list int [1002;1003;2000]);
  test 4 (Array2.slice_left c 3) (from_list int [2001;2002;2003]);
  let d = reshape_2 (genarray_of_array2 b) 4 3 in
  test 5 (Array2.slice_right d 1) (from_list_fortran int [1001;2001;3001;1002]);
  test 6 (Array2.slice_right d 2) (from_list_fortran int [2002;3002;1003;2003]);
  test 7 (Array2.slice_right d 3) (from_list_fortran int [3003;1004;2004;3004]);
  testing_function "reshape";
  let a = make_array2 int c_layout 0 1 1 (fun i -> i + 3) in
  let b = reshape_0 (genarray_of_array2 a) in
  let c = reshape (genarray_of_array0 b) [|1|] in
  test 8 (Array0.get b) 3;
  test 9 (Genarray.get c [|0|]) 3;
  test 10 (Genarray.get (Genarray.slice_left c [|0|]) [||]) 3;

(* I/O *)

  print_newline();
  testing_function "------ I/O --------";
  testing_function "output_value/input_value";
  let test_structured_io testno value =
    let tmp = Filename.temp_file "bigarray" ".data" in
    let oc = open_out_bin tmp in
    output_value oc value;
    close_out oc;
    let ic = open_in_bin tmp in
    let value' = input_value ic in
    close_in ic;
    Sys.remove tmp;
    test testno value value' in
  test_structured_io 1 (from_list int8_signed [1;2;3;-4;127;-128]);
  test_structured_io 2 (from_list int16_signed [1;2;3;-4;127;-128]);
  test_structured_io 3 (from_list int [1;2;3;-4;127;-128]);
  test_structured_io 4
    (from_list int32 (List.map Int32.of_int [1;2;3;-4;127;-128]));
  test_structured_io 5
    (from_list int64 (List.map Int64.of_int [1;2;3;-4;127;-128]));
  test_structured_io 6
    (from_list nativeint (List.map Nativeint.of_int [1;2;3;-4;127;-128]));
  test_structured_io 7 (from_list float32 [0.0; 0.25; -4.0; 3.141592654]);
  test_structured_io 8 (from_list float64 [0.0; 0.25; -4.0; 3.141592654]);
  test_structured_io 9 (make_array2 int c_layout 0 100 100 id);
  test_structured_io 10 (make_array2 float64 fortran_layout 1 200 200 float);
  test_structured_io 11 (make_array3 int32 c_layout 0 20 30 40 Int32.of_int);
  test_structured_io 12 (make_array3 float32 fortran_layout 1 10 50 100 float);
  test_structured_io 13 (make_array2 complex32 c_layout 0 100 100 makecomplex);
  test_structured_io 14 (make_array3 complex64 fortran_layout 1 10 20 30
                                     makecomplex);

  ()
  [@@inline never]

(********* End of test *********)

let _ =
  tests ();
  print_newline();
  if !error_occurred then begin
    prerr_endline "************* TEST FAILED ****************"; exit 2
  end else
    exit 0
