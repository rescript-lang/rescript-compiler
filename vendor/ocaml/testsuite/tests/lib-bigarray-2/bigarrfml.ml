(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Bigarray
open Printf

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

(* External C and Fortran functions *)

external c_filltab :
  unit -> (float, float64_elt, c_layout) Array2.t = "c_filltaab"
external c_printtab :
  (float, float64_elt, c_layout) Array2.t -> unit = "c_printtab"
external fortran_filltab :
  unit -> (float, float32_elt, fortran_layout) Array2.t = "fortran_filltab"
external fortran_printtab :
  (float, float32_elt, fortran_layout) Array2.t -> unit = "fortran_printtab"

let _ =

  let make_array2 kind layout ind0 dim1 dim2 fromint =
    let a = Array2.create kind layout dim1 dim2 in
    for i = ind0 to dim1 - 1 + ind0 do
      for j = ind0 to dim2 - 1 + ind0 do
        a.{i,j} <- (fromint (i * 1000 + j))
      done
    done;
    a in

  print_newline();
  testing_function "------ Foreign function interface --------";
  testing_function "Passing an array to C";
  c_printtab (make_array2 float64 c_layout 0 6 8 float);
  testing_function "Accessing a C array";
  let a = c_filltab () in
  test 1 a.{0,0} 0.0;
  test 2 a.{1,0} 100.0;
  test 3 a.{0,1} 1.0;
  test 4 a.{5,4} 504.0;
  testing_function "Passing an array to Fortran";
  fortran_printtab (make_array2 float32 fortran_layout 1 5 4 float);
  testing_function "Accessing a Fortran array";
  let a = fortran_filltab () in
  test 1 a.{1,1} 101.0;
  test 2 a.{2,1} 201.0;
  test 3 a.{1,2} 102.0;
  test 4 a.{5,4} 504.0;
