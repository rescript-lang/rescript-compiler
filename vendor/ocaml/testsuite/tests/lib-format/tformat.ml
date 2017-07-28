(*************************************************************************)
(*                                                                       *)
(*                                OCaml                                  *)
(*                                                                       *)
(*         Pierre Weis, projet Pomdapi, INRIA Rocquencourt               *)
(*                                                                       *)
(*   Copyright 2011 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

(*

A test file for the Format module.

*)

open Testing;;
open Format;;

let say s = Printf.printf s;;

try

  say "d/i positive\n%!";
  test (sprintf "%d/%i" 42 43 = "42/43");
  test (sprintf "%-4d/%-5i" 42 43 = "42  /43   ");
  test (sprintf "%04d/%05i" 42 43 = "0042/00043");
  test (sprintf "%+d/%+i" 42 43 = "+42/+43");
  test (sprintf "% d/% i" 42 43 = " 42/ 43");
  test (sprintf "%#d/%#i" 42 43 = "42/43");
  test (sprintf "%4d/%5i" 42 43 = "  42/   43");
  test (sprintf "%*d" (-4) 42 = "42  ");
  test (sprintf "%*d/%*i" 4 42 5 43 = "  42/   43");
  test (sprintf "%-0+#4d/%-0 #5i" 42 43 = "+42 / 43  ");

  say "\nd/i negative\n%!";
  test (sprintf "%d/%i" (-42) (-43) = "-42/-43");
  test (sprintf "%-4d/%-5i" (-42) (-43) = "-42 /-43  ");
  test (sprintf "%04d/%05i" (-42) (-43) = "-042/-0043");
  test (sprintf "%+d/%+i" (-42) (-43) = "-42/-43");
  test (sprintf "% d/% i" (-42) (-43) = "-42/-43");
  test (sprintf "%#d/%#i" (-42) (-43) = "-42/-43");
  test (sprintf "%4d/%5i" (-42) (-43) = " -42/  -43");
  test (sprintf "%*d" (-4) (-42) = "-42 ");
  test (sprintf "%*d/%*i" 4 (-42) 5 (-43) = " -42/  -43");
  test (sprintf "%-0+ #4d/%-0+ #5i" (-42) (-43) = "-42 /-43  ");

  say "\nu positive\n%!";
  test (sprintf "%u" 42 = "42");
  test (sprintf "%-4u" 42 = "42  ");
  test (sprintf "%04u" 42 = "0042");
  test (sprintf "%+u" 42 = "42");
  test (sprintf "% u" 42 = "42");
  test (sprintf "%#u" 42 = "42");
  test (sprintf "%4u" 42 = "  42");
  test (sprintf "%*u" 4 42 = "  42");
  test (sprintf "%*u" (-4) 42 = "42  ");

  say "\nu negative\n%!";
  begin match Sys.word_size with
  | 32 ->
     test (sprintf "%u" (-1) = "2147483647");
  | 64 ->
     test (sprintf "%u" (-1) = "9223372036854775807");
  | _ -> test false
  end;

  say "\nx positive\n%!";
  test (sprintf "%x" 42 = "2a");
  test (sprintf "%-4x" 42 = "2a  ");
  test (sprintf "%04x" 42 = "002a");
  test (sprintf "%+x" 42 = "2a");
  test (sprintf "% x" 42 = "2a");
  test (sprintf "%#x" 42 = "0x2a");
  test (sprintf "%4x" 42 = "  2a");
  test (sprintf "%*x" 5 42 = "   2a");
  test (sprintf "%*x" (-5) 42 = "2a   ");
  test (sprintf "%#*x" 5 42 = " 0x2a");
  test (sprintf "%#*x" (-5) 42 = "0x2a ");
  test (sprintf "%#-*x" 5 42 = "0x2a ");
  test (sprintf "%-0+ #*x" 5 42 = "0x2a ");

  say "\nx negative\n%!";
  begin match Sys.word_size with
  | 32 ->
     test (sprintf "%x" (-42) = "7fffffd6");
  | 64 ->
     test (sprintf "%x" (-42) = "7fffffffffffffd6");
  | _ -> test false
  end;

  say "\nX positive\n%!";
  test (sprintf "%X" 42 = "2A");
  test (sprintf "%-4X" 42 = "2A  ");
  test (sprintf "%04X" 42 = "002A");
  test (sprintf "%+X" 42 = "2A");
  test (sprintf "% X" 42 = "2A");
  test (sprintf "%#X" 42 = "0X2A");
  test (sprintf "%4X" 42 = "  2A");
  test (sprintf "%*X" 5 42 = "   2A");
  test (sprintf "%-0+ #*X" 5 42 = "0X2A ");

  say "\nx negative\n%!";
  begin match Sys.word_size with
  | 32 ->
     test (sprintf "%X" (-42) = "7FFFFFD6");
  | 64 ->
     test (sprintf "%X" (-42) = "7FFFFFFFFFFFFFD6");
  | _ -> test false
  end;

  say "\no positive\n%!";
  test (sprintf "%o" 42 = "52");
  test (sprintf "%-4o" 42 = "52  ");
  test (sprintf "%04o" 42 = "0052");
  test (sprintf "%+o" 42 = "52");
  test (sprintf "% o" 42 = "52");
  test (sprintf "%#o" 42 = "052");
  test (sprintf "%4o" 42 = "  52");
  test (sprintf "%*o" 5 42 = "   52");
  test (sprintf "%-0+ #*o" 5 42 = "052  ");

  say "\no negative\n%!";
  begin match Sys.word_size with
  | 32 ->
     test (sprintf "%o" (-42) = "17777777726");
  | 64 ->
     test (sprintf "%o" (-42) = "777777777777777777726");
  | _ -> test false
  end;

  say "\ns\n%!";
  test (sprintf "%s" "foo" = "foo");
  test (sprintf "%-5s" "foo" = "foo  ");
  test (sprintf "%05s" "foo" = "  foo");
  test (sprintf "%+s" "foo" = "foo");
  test (sprintf "% s" "foo" = "foo");
  test (sprintf "%#s" "foo" = "foo");
  test (sprintf "%5s" "foo" = "  foo");
  test (sprintf "%1s" "foo" = "foo");
  test (sprintf "%*s" 6 "foo" = "   foo");
  test (sprintf "%*s" (-6) "foo" = "foo   ");
  test (sprintf "%*s" 2 "foo" = "foo");
  test (sprintf "%-0+ #5s" "foo" = "foo  ");
  test (sprintf "%s@@" "foo" = "foo@");
  test (sprintf "%s@@inria.fr" "foo" = "foo@inria.fr");
  test (sprintf "%s@@%s" "foo" "inria.fr" = "foo@inria.fr");

  say "\nS\n%!";
  test (sprintf "%S" "fo\"o" = "\"fo\\\"o\"");
  test (sprintf "%-7S" "foo" = "\"foo\"  ");
(*  test (sprintf "%07S" "foo" = "  \"foo\""); *)
  (* %S is incompatible with '0' *)
  test (sprintf "%+S" "foo" = "\"foo\"");
  test (sprintf "% S" "foo" = "\"foo\"");
  test (sprintf "%#S" "foo" = "\"foo\"");
  test (sprintf "%7S" "foo" = "  \"foo\"");
  test (sprintf "%1S" "foo" = "\"foo\"");
  test (sprintf "%*S" 8 "foo" = "   \"foo\"");
  test (sprintf "%*S" (-8) "foo" = "\"foo\"   ");
  test (sprintf "%*S" 2 "foo" = "\"foo\"");
(*  test (sprintf "%-0+ #5S" "foo" = "\"foo\"  ");  padding not done *)
  (* %S is incompatible with '0','+' and ' ' *)
  test (sprintf "%S@@" "foo" = "\"foo\"@");
  test (sprintf "%S@@inria.fr" "foo" = "\"foo\"@inria.fr");
  test (sprintf "%S@@%S" "foo" "inria.fr" = "\"foo\"@\"inria.fr\"");

  say "\nc\n%!";
  test (sprintf "%c" 'c' = "c");
(*  test (sprintf "%-4c" 'c' = "c   ");    padding not done *)
(*  test (sprintf "%04c" 'c' = "   c");    padding not done *)
  test (sprintf "%+c" 'c' = "c");
  test (sprintf "% c" 'c' = "c");
  test (sprintf "%#c" 'c' = "c");
(*  test (sprintf "%4c" 'c' = "   c");     padding not done *)
(*  test (sprintf "%*c" 2 'c' = " c");     padding not done *)
(*  test (sprintf "%-0+ #4c" 'c' = "c   ");  padding not done *)

  say "\nC\n%!";
  test (sprintf "%C" 'c' = "'c'");
  test (sprintf "%C" '\'' = "'\\''");
(*  test (sprintf "%-4C" 'c' = "c   ");    padding not done *)
(*  test (sprintf "%04C" 'c' = "   c");    padding not done *)
  test (sprintf "%+C" 'c' = "'c'");
  test (sprintf "% C" 'c' = "'c'");
  test (sprintf "%#C" 'c' = "'c'");
(*  test (sprintf "%4C" 'c' = "   c");     padding not done *)
(*  test (sprintf "%*C" 2 'c' = " c");     padding not done *)
(*  test (sprintf "%-0+ #4C" 'c' = "c   ");  padding not done *)

  say "\nf\n%!";
  test (sprintf "%f" (-42.42) = "-42.420000");
  test (sprintf "%-13f" (-42.42) = "-42.420000   ");
  test (sprintf "%013f" (-42.42) = "-00042.420000");
  test (sprintf "%+f" 42.42 = "+42.420000");
  test (sprintf "% f" 42.42 = " 42.420000");
  test (sprintf "%#f" 42.42 = "42.420000");
  test (sprintf "%13f" 42.42 = "    42.420000");
  test (sprintf "%*f" 12 42.42 = "   42.420000");
  test (sprintf "%-0+ #12f" 42.42 = "+42.420000  ");
  test (sprintf "%.3f" (-42.42) = "-42.420");
  test (sprintf "%-13.3f" (-42.42) = "-42.420      ");
  test (sprintf "%013.3f" (-42.42) = "-00000042.420");
  test (sprintf "%+.3f" 42.42 = "+42.420");
  test (sprintf "% .3f" 42.42 = " 42.420");
  test (sprintf "%#.3f" 42.42 = "42.420");
  test (sprintf "%13.3f" 42.42 = "       42.420");
  test (sprintf "%*.*f" 12 3 42.42 = "      42.420");
  test (sprintf "%-0+ #12.3f" 42.42 = "+42.420     ");

  (* Under Windows (mingw and maybe also MSVC), the stdlib uses three
     digits for the exponent instead of the two used by Linux and BSD.
     Check that the two strings are equal, except that there may be an
     extra zero, and if there is one, there may be a missing space or
     zero. All in the first string relative to the second. *)
  let ( =* ) s1 s2 =
    let ss1 = s1 ^ "$" in
    let ss2 = s2 ^ "$" in
    let rec loop i1 i2 extra missing =
      if i1 = String.length ss1 && i2 = String.length ss2 then begin
        if extra then true else not missing
      end else if i1 = String.length ss1 || i2 = String.length ss2 then
        false
      else begin
        match ss1.[i1], ss2.[i2] with
        | x, y when x = y -> loop (i1+1) (i2+1) extra missing
        | '0', _ when not extra -> loop (i1+1) i2 true missing
        | _, (' '|'0') when not missing -> loop i1 (i2+1) extra true
        | _, _ -> false
      end
    in
    loop 0 0 false false
  in

  say "\nF\n%!";
  test (sprintf "%F" 42.42 = "42.42");
  test (sprintf "%F" 42.42e42 =* "4.242e+43");
  test (sprintf "%F" 42.00 = "42.");
  test (sprintf "%F" 0.042 = "0.042");
  test (sprintf "%4F" 3. = "  3.");
  test (sprintf "%-4F" 3. = "3.  ");
  test (sprintf "%04F" 3. = "003.");
(* plus-padding unsupported
  test (sprintf "%+4F" 3. = " +3.");
*)
(* no precision
  test (sprintf "%.3F" 42.42 = "42.420");
  test (sprintf "%12.3F" 42.42e42 = "   4.242e+43");
  test (sprintf "%.3F" 42.00 = "42.000");
  test (sprintf "%.3F" 0.0042 = "0.004");
*)

  say "\ne\n%!";
  test (sprintf "%e" (-42.42) =* "-4.242000e+01");
  test (sprintf "%-15e" (-42.42) =* "-4.242000e+01  ");
  test (sprintf "%015e" (-42.42) =* "-004.242000e+01");
  test (sprintf "%+e" 42.42 =* "+4.242000e+01");
  test (sprintf "% e" 42.42 =* " 4.242000e+01");
  test (sprintf "%#e" 42.42 =* "4.242000e+01");
  test (sprintf "%15e" 42.42 =* "   4.242000e+01");
  test (sprintf "%*e" 14 42.42 =* "  4.242000e+01");
  test (sprintf "%-0+ #14e" 42.42 =* "+4.242000e+01 ");
  test (sprintf "%.3e" (-42.42) =* "-4.242e+01");
  test (sprintf "%-15.3e" (-42.42) =* "-4.242e+01     ");
  test (sprintf "%015.3e" (-42.42) =* "-000004.242e+01");
  test (sprintf "%+.3e" 42.42 =* "+4.242e+01");
  test (sprintf "% .3e" 42.42 =* " 4.242e+01");
  test (sprintf "%#.3e" 42.42 =* "4.242e+01");
  test (sprintf "%15.3e" 42.42 =* "      4.242e+01");
  test (sprintf "%*.*e" 11 3 42.42 =* "  4.242e+01");
  test (sprintf "%-0+ #14.3e" 42.42 =* "+4.242e+01    ");

  say "\nE\n%!";
  test (sprintf "%E" (-42.42) =* "-4.242000E+01");
  test (sprintf "%-15E" (-42.42) =* "-4.242000E+01  ");
  test (sprintf "%015E" (-42.42) =* "-004.242000E+01");
  test (sprintf "%+E" 42.42 =* "+4.242000E+01");
  test (sprintf "% E" 42.42 =* " 4.242000E+01");
  test (sprintf "%#E" 42.42 =* "4.242000E+01");
  test (sprintf "%15E" 42.42 =* "   4.242000E+01");
  test (sprintf "%*E" 14 42.42 =* "  4.242000E+01");
  test (sprintf "%-0+ #14E" 42.42 =* "+4.242000E+01 ");
  test (sprintf "%.3E" (-42.42) =* "-4.242E+01");
  test (sprintf "%-15.3E" (-42.42) =* "-4.242E+01     ");
  test (sprintf "%015.3E" (-42.42) =* "-000004.242E+01");
  test (sprintf "%+.3E" 42.42 =* "+4.242E+01");
  test (sprintf "% .3E" 42.42 =* " 4.242E+01");
  test (sprintf "%#.3E" 42.42 =* "4.242E+01");
  test (sprintf "%15.3E" 42.42 =* "      4.242E+01");
  test (sprintf "%*.*E" 11 3 42.42 =* "  4.242E+01");
  test (sprintf "%-0+ #14.3E" 42.42 =* "+4.242E+01    ");

(* %g gives strange results that correspond to neither %f nor %e
  say "\ng\n%!";
  test (sprintf "%g" (-42.42) = "-42.42000");
  test (sprintf "%-15g" (-42.42) = "-42.42000      ");
  test (sprintf "%015g" (-42.42) = "-00000042.42000");
  test (sprintf "%+g" 42.42 = "+42.42000");
  test (sprintf "% g" 42.42 = " 42.42000");
  test (sprintf "%#g" 42.42 = "42.42000");
  test (sprintf "%15g" 42.42 = "       42.42000");
  test (sprintf "%*g" 14 42.42 = "      42.42000");
  test (sprintf "%-0+ #14g" 42.42 = "+42.42000     ");
  test (sprintf "%.3g" (-42.42) = "-42.420");
*)

(* Same for %G
  say "\nG\n%!";
*)

  say "\nB\n%!";
  test (sprintf "%B" true = "true");
  test (sprintf "%B" false = "false");
 (* test (sprintf "%8B" false = "   false"); *)
  (* padding not done *)

  say "\nld/li positive\n%!";
  test (sprintf "%ld/%li" 42l 43l = "42/43");
  test (sprintf "%-4ld/%-5li" 42l 43l = "42  /43   ");
  test (sprintf "%04ld/%05li" 42l 43l = "0042/00043");
  test (sprintf "%+ld/%+li" 42l 43l = "+42/+43");
  test (sprintf "% ld/% li" 42l 43l = " 42/ 43");
  test (sprintf "%#ld/%#li" 42l 43l = "42/43");
  test (sprintf "%4ld/%5li" 42l 43l = "  42/   43");
  test (sprintf "%*ld/%*li" 4 42l 5 43l = "  42/   43");
  test (sprintf "%-0+#4ld/%-0 #5li" 42l 43l = "+42 / 43  ");

  say "\nld/li negative\n%!";
  test (sprintf "%ld/%li" (-42l) (-43l) = "-42/-43");
  test (sprintf "%-4ld/%-5li" (-42l) (-43l) = "-42 /-43  ");
  test (sprintf "%04ld/%05li" (-42l) (-43l) = "-042/-0043");
  test (sprintf "%+ld/%+li" (-42l) (-43l) = "-42/-43");
  test (sprintf "% ld/% li" (-42l) (-43l) = "-42/-43");
  test (sprintf "%#ld/%#li" (-42l) (-43l) = "-42/-43");
  test (sprintf "%4ld/%5li" (-42l) (-43l) = " -42/  -43");
  test (sprintf "%*ld/%*li" 4 (-42l) 5 (-43l) = " -42/  -43");
  test (sprintf "%-0+ #4ld/%-0+ #5li" (-42l) (-43l) = "-42 /-43  ");

  say "\nlu positive\n%!";
  test (sprintf "%lu" 42l = "42");
  test (sprintf "%-4lu" 42l = "42  ");
  test (sprintf "%04lu" 42l = "0042");
  test (sprintf "%+lu" 42l = "42");
  test (sprintf "% lu" 42l = "42");
  test (sprintf "%#lu" 42l = "42");
  test (sprintf "%4lu" 42l = "  42");
  test (sprintf "%*lu" 4 42l = "  42");
  test (sprintf "%-0+ #6ld" 42l = "+42   ");

  say "\nlu negative\n%!";
  test (sprintf "%lu" (-1l) = "4294967295");

  say "\nlx positive\n%!";
  test (sprintf "%lx" 42l = "2a");
  test (sprintf "%-4lx" 42l = "2a  ");
  test (sprintf "%04lx" 42l = "002a");
  test (sprintf "%+lx" 42l = "2a");
  test (sprintf "% lx" 42l = "2a");
  test (sprintf "%#lx" 42l = "0x2a");
  test (sprintf "%4lx" 42l = "  2a");
  test (sprintf "%*lx" 5 42l = "   2a");
  test (sprintf "%-0+ #*lx" 5 42l = "0x2a ");

  say "\nlx negative\n%!";
  test (sprintf "%lx" (-42l) = "ffffffd6");

  say "\nlX positive\n%!";
  test (sprintf "%lX" 42l = "2A");
  test (sprintf "%-4lX" 42l = "2A  ");
  test (sprintf "%04lX" 42l = "002A");
  test (sprintf "%+lX" 42l = "2A");
  test (sprintf "% lX" 42l = "2A");
  test (sprintf "%#lX" 42l = "0X2A");
  test (sprintf "%4lX" 42l = "  2A");
  test (sprintf "%*lX" 5 42l = "   2A");
  test (sprintf "%-0+ #*lX" 5 42l = "0X2A ");

  say "\nlx negative\n%!";
  test (sprintf "%lX" (-42l) = "FFFFFFD6");

  say "\nlo positive\n%!";
  test (sprintf "%lo" 42l = "52");
  test (sprintf "%-4lo" 42l = "52  ");
  test (sprintf "%04lo" 42l = "0052");
  test (sprintf "%+lo" 42l = "52");
  test (sprintf "% lo" 42l = "52");
  test (sprintf "%#lo" 42l = "052");
  test (sprintf "%4lo" 42l = "  52");
  test (sprintf "%*lo" 5 42l = "   52");
  test (sprintf "%-0+ #*lo" 5 42l = "052  ");

  say "\nlo negative\n%!";
  test (sprintf "%lo" (-42l) = "37777777726");

  (* Nativeint not tested: looks like too much work, and anyway it should
     work like Int32 or Int64. *)

  say "\nLd/Li positive\n%!";
  test (sprintf "%Ld/%Li" 42L 43L = "42/43");
  test (sprintf "%-4Ld/%-5Li" 42L 43L = "42  /43   ");
  test (sprintf "%04Ld/%05Li" 42L 43L = "0042/00043");
  test (sprintf "%+Ld/%+Li" 42L 43L = "+42/+43");
  test (sprintf "% Ld/% Li" 42L 43L = " 42/ 43");
  test (sprintf "%#Ld/%#Li" 42L 43L = "42/43");
  test (sprintf "%4Ld/%5Li" 42L 43L = "  42/   43");
  test (sprintf "%*Ld/%*Li" 4 42L 5 43L = "  42/   43");
  test (sprintf "%-0+#4Ld/%-0 #5Li" 42L 43L = "+42 / 43  ");

  say "\nLd/Li negative\n%!";
  test (sprintf "%Ld/%Li" (-42L) (-43L) = "-42/-43");
  test (sprintf "%-4Ld/%-5Li" (-42L) (-43L) = "-42 /-43  ");
  test (sprintf "%04Ld/%05Li" (-42L) (-43L) = "-042/-0043");
  test (sprintf "%+Ld/%+Li" (-42L) (-43L) = "-42/-43");
  test (sprintf "% Ld/% Li" (-42L) (-43L) = "-42/-43");
  test (sprintf "%#Ld/%#Li" (-42L) (-43L) = "-42/-43");
  test (sprintf "%4Ld/%5Li" (-42L) (-43L) = " -42/  -43");
  test (sprintf "%*Ld/%*Li" 4 (-42L) 5 (-43L) = " -42/  -43");
  test (sprintf "%-0+ #4Ld/%-0+ #5Li" (-42L) (-43L) = "-42 /-43  ");

  say "\nLu positive\n%!";
  test (sprintf "%Lu" 42L = "42");
  test (sprintf "%-4Lu" 42L = "42  ");
  test (sprintf "%04Lu" 42L = "0042");
  test (sprintf "%+Lu" 42L = "42");
  test (sprintf "% Lu" 42L = "42");
  test (sprintf "%#Lu" 42L = "42");
  test (sprintf "%4Lu" 42L = "  42");
  test (sprintf "%*Lu" 4 42L = "  42");
  test (sprintf "%-0+ #6Ld" 42L = "+42   ");

  say "\nLu negative\n%!";
  test (sprintf "%Lu" (-1L) = "18446744073709551615");

  say "\nLx positive\n%!";
  test (sprintf "%Lx" 42L = "2a");
  test (sprintf "%-4Lx" 42L = "2a  ");
  test (sprintf "%04Lx" 42L = "002a");
  test (sprintf "%+Lx" 42L = "2a");
  test (sprintf "% Lx" 42L = "2a");
  test (sprintf "%#Lx" 42L = "0x2a");
  test (sprintf "%4Lx" 42L = "  2a");
  test (sprintf "%*Lx" 5 42L = "   2a");
  test (sprintf "%-0+ #*Lx" 5 42L = "0x2a ");

  say "\nLx negative\n%!";
  test (sprintf "%Lx" (-42L) = "ffffffffffffffd6");

  say "\nLX positive\n%!";
  test (sprintf "%LX" 42L = "2A");
  test (sprintf "%-4LX" 42L = "2A  ");
  test (sprintf "%04LX" 42L = "002A");
  test (sprintf "%+LX" 42L = "2A");
  test (sprintf "% LX" 42L = "2A");
  test (sprintf "%#LX" 42L = "0X2A");
  test (sprintf "%4LX" 42L = "  2A");
  test (sprintf "%*LX" 5 42L = "   2A");
  test (sprintf "%-0+ #*LX" 5 42L = "0X2A ");

  say "\nLx negative\n%!";
  test (sprintf "%LX" (-42L) = "FFFFFFFFFFFFFFD6");

  say "\nLo positive\n%!";
  test (sprintf "%Lo" 42L = "52");
  test (sprintf "%-4Lo" 42L = "52  ");
  test (sprintf "%04Lo" 42L = "0052");
  test (sprintf "%+Lo" 42L = "52");
  test (sprintf "% Lo" 42L = "52");
  test (sprintf "%#Lo" 42L = "052");
  test (sprintf "%4Lo" 42L = "  52");
  test (sprintf "%*Lo" 5 42L = "   52");
  test (sprintf "%-0+ #*Lo" 5 42L = "052  ");

  say "\nLo negative\n%!";
  test (sprintf "%Lo" (-42L) = "1777777777777777777726");

  say "\na\n%!";
  let x = ref () in
  let f () y = if y == x then "ok" else "wrong" in
  test (sprintf "%a" f x = "ok");

  say "\nt\n%!";
  let f () = "ok" in
  test (sprintf "%t" f = "ok");

(* %{ fmt %} prints the signature of [fmt], i.e. a canonical representation
   of the conversions present in [fmt].
*)
  say "\n{...%%}\n%!";
  let f = format_of_string "%f/%s" in
  test (sprintf "%{%f%s%}" f = "%f%s");

  say "\n(...%%)\n%!";
  let f = format_of_string "%d/foo/%s" in
  test (sprintf "%(%d%s%)" f 42 "bar" = "42/foo/bar");

  say "\n! %% @ , and constants\n%!";
  test (sprintf "%!" = "");
  test (sprintf "%%" = "%");
  test (sprintf "%@" = "@");
  test (sprintf "%," = "");
  test (sprintf "@@" = "@");
  test (sprintf "@@@@" = "@@");
  test (sprintf "@@%%" = "@%");
  say "\nend of tests\n%!";

with e ->
  say "unexpected exception: %s\n%!" (Printexc.to_string e);
  test false;
;;
