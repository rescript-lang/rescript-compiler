(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Printf

let build_result ngroups input =
  let res = Array.make (ngroups + 1) "~" in
  for i = 0 to ngroups do
    try
      res.(i) <- Str.matched_group i input
    with Not_found -> ()
  done;
  res

let search_forward re ng input start =
  try
    ignore(Str.search_forward re input start);
    build_result ng input
  with Not_found ->
    [||]

let search_backward re ng input start =
  try
    ignore(Str.search_backward re input start);
    build_result ng input
  with Not_found ->
    [||]

let partial_match re ng input start =
  if Str.string_partial_match re input start
  then build_result ng input
  else [||]

let start_test msg =
  print_newline(); printf "%s\n  " msg

let num_failures = ref 0

let test res1 res2 =
  if res1 = res2
  then print_char '.'
  else begin print_string " FAIL "; incr num_failures end

let test_search_forward r ng s exp =
  test (search_forward r ng s 0) exp

let test_search_backward r ng s exp =
  test (search_backward r ng s (String.length s)) exp

let test_partial_match r ng s exp =
  test (partial_match r ng s 0) exp

let end_test () =
  print_newline();
  if !num_failures = 0 then
    printf "All tests passed\n"
  else begin
    printf "TEST FAILED: %d failure(s)\n" !num_failures;
    exit 2
  end

let automated_test() =

  (** Forward searches *)
  start_test "Search for /the quick brown fox/";
  let r = Str.regexp "the quick brown fox" in
  let n = 0 in
  test_search_forward r n "the quick brown fox"
    [|"the quick brown fox"|];
  test_search_forward r n "What do you know about the quick brown fox?"
    [|"the quick brown fox"|];
  test_search_forward r n "The quick brown FOX"
    [||];
  test_search_forward r n "What do you know about THE QUICK BROWN FOX?"
    [||];

  start_test "Search for /the quick brown fox/ (case-insensitive)";
  let r = Str.regexp_case_fold "the quick brown fox" in
  let n = 0 in
  test_search_forward r n "the quick brown fox"
    [|"the quick brown fox"|];
  test_search_forward r n "What do you know about the quick brown fox?"
    [|"the quick brown fox"|];
  test_search_forward r n "The quick brown FOX"
    [|"The quick brown FOX"|];
  test_search_forward r n "What do you know about THE QUICK BROWN FOX?"
    [|"THE QUICK BROWN FOX"|];
  test_search_forward r n "The slow white snail"
    [||];

  start_test "Search for /a*abc?xyz+pqrrrabbb*xyyyyy?pq?q?q?q?q?q?AB*zz/";
  let r = Str.regexp "a*abc?xyz+pqrrrabbb*xyyyyy?pq?q?q?q?q?q?AB*zz" in
  let n = 0 in
  test_search_forward r n "abxyzpqrrrabbxyyyypqAzz"
    [|"abxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "abxyzpqrrrabbxyyyypqAzz"
    [|"abxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aabxyzpqrrrabbxyyyypqAzz"
    [|"aabxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaabxyzpqrrrabbxyyyypqAzz"
    [|"aaabxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaaabxyzpqrrrabbxyyyypqAzz"
    [|"aaaabxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "abcxyzpqrrrabbxyyyypqAzz"
    [|"abcxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aabcxyzpqrrrabbxyyyypqAzz"
    [|"aabcxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypAzz"
    [|"aaabcxyzpqrrrabbxyyyypAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqqqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqqqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqqqqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqqqqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqqqqqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqqqqqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqqqqqqAzz"|];
  test_search_forward r n "aaaabcxyzpqrrrabbxyyyypqAzz"
    [|"aaaabcxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "abxyzzpqrrrabbxyyyypqAzz"
    [|"abxyzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aabxyzzzpqrrrabbxyyyypqAzz"
    [|"aabxyzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaabxyzzzzpqrrrabbxyyyypqAzz"
    [|"aaabxyzzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaaabxyzzzzpqrrrabbxyyyypqAzz"
    [|"aaaabxyzzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "abcxyzzpqrrrabbxyyyypqAzz"
    [|"abcxyzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aabcxyzzzpqrrrabbxyyyypqAzz"
    [|"aabcxyzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaabcxyzzzzpqrrrabbxyyyypqAzz"
    [|"aaabcxyzzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaaabcxyzzzzpqrrrabbxyyyypqAzz"
    [|"aaaabcxyzzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaaabcxyzzzzpqrrrabbbxyyyypqAzz"
    [|"aaaabcxyzzzzpqrrrabbbxyyyypqAzz"|];
  test_search_forward r n "aaaabcxyzzzzpqrrrabbbxyyyyypqAzz"
    [|"aaaabcxyzzzzpqrrrabbbxyyyyypqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypABzz"
    [|"aaabcxyzpqrrrabbxyyyypABzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypABBzz"
    [|"aaabcxyzpqrrrabbxyyyypABBzz"|];
  test_search_forward r n ">>>aaabxyzpqrrrabbxyyyypqAzz"
    [|"aaabxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n ">aaaabxyzpqrrrabbxyyyypqAzz"
    [|"aaaabxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n ">>>>abcxyzpqrrrabbxyyyypqAzz"
    [|"abcxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "abxyzpqrrabbxyyyypqAzz"
    [||];
  test_search_forward r n "abxyzpqrrrrabbxyyyypqAzz"
    [||];
  test_search_forward r n "abxyzpqrrrabxyyyypqAzz"
    [||];
  test_search_forward r n "aaaabcxyzzzzpqrrrabbbxyyyyyypqAzz"
    [||];
  test_search_forward r n "aaaabcxyzzzzpqrrrabbbxyyypqAzz"
    [||];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqqqqqqAzz"
    [||];

  start_test "Search for /^abc\\(abc\\)?zz/";
  let r = Str.regexp "^abc\\(abc\\)?zz" in
  let n = 1 in
  test_search_forward r n "abczz"
    [|"abczz"; "~"|];
  test_search_forward r n "abcabczz"
    [|"abcabczz"; "abc"|];
  test_search_forward r n "zz"
    [||];
  test_search_forward r n "abcabcabczz"
    [||];
  test_search_forward r n ">>abczz"
    [||];

  start_test "Search for /^\\(b+\\|a\\)\\(b+\\|a\\)?c/";
  let r = Str.regexp "^\\(b+\\|a\\)\\(b+\\|a\\)?c" in
  let n = 2 in
  test_search_forward r n "bc"
    [|"bc"; "b"; "~"|];
  test_search_forward r n "bbc"
    [|"bbc"; "bb"; "~"|];
  test_search_forward r n "bbbc"
    [|"bbbc"; "bbb"; "~"|];
  test_search_forward r n "bac"
    [|"bac"; "b"; "a"|];
  test_search_forward r n "bbac"
    [|"bbac"; "bb"; "a"|];
  test_search_forward r n "aac"
    [|"aac"; "a"; "a"|];
  test_search_forward r n "abbbbbbbbbbbc"
    [|"abbbbbbbbbbbc"; "a"; "bbbbbbbbbbb"|];
  test_search_forward r n "bbbbbbbbbbbac"
    [|"bbbbbbbbbbbac"; "bbbbbbbbbbb"; "a"|];
  test_search_forward r n "aaac"
    [||];
  test_search_forward r n "abbbbbbbbbbbac"
    [||];

  start_test "Search for /r\\(\\(g*\\|k\\)y?\\)*A/";
  let r = Str.regexp "r\\(\\(g*\\|k\\)y?\\)*A" in
  let n = 2 in
  test_search_forward r n "ArA"
    [|"rA"; "~"; "~"|];
  test_search_forward r n "ArkA"
    [|"rkA"; "k"; "k"|];
  test_search_forward r n "AryA"
    [|"ryA"; "y"; ""|];
  test_search_forward r n "ArgggkyggkA"
    [|"rgggkyggkA"; "k"; "k"|];

  start_test "Search for /A\\(\\(t\\|v\\)\\(q?\\|n\\)\\)*A/";
  let r = Str.regexp "A\\(\\(t\\|v\\)\\(q?\\|n\\)\\)*A" in
  let n = 3 in
  test_search_forward r n "AvA"
    [|"AvA"; "v"; "v"; ""|];

  start_test "Search for /A\\(\\(b\\(\\(d\\|l*\\)?\\|w\\)\\)*a\\)A/";
  let r = Str.regexp "A\\(\\(b\\(\\(d\\|l*\\)?\\|w\\)\\)*a\\)A" in
  let n = 4 in
  test_search_forward r n "AbbaA"
    [|"AbbaA"; "bba"; "b"; ""; ""|];

  start_test "Search for /\\(\\|f\\)*x/";
  let r = Str.regexp "\\(\\|f\\)*x" in
  let n = 1 in
  test_search_forward r n "abcd"
    [||];
  test_search_forward r n "fffff"
    [||];
  test_search_forward r n "fffxab"
    [|"fffx"; "f"|];
  test_search_forward r n "zzzxab"
    [|"x"; "~"|];

  start_test "Search for /\\(\\|f\\)+x/";
  let r = Str.regexp "\\(\\|f\\)+x" in
  let n = 1 in
  test_search_forward r n "abcd"
    [||];
  test_search_forward r n "fffff"
    [||];
  test_search_forward r n "fffxab"
    [|"fffx"; "f"|];
  test_search_forward r n "zzzxab"
    [|"x"; ""|];

  start_test "Search for /A\\(.?\\)*A/";
  let r = Str.regexp "A\\(.?\\)*A" in
  let n = 1 in
  test_search_forward r n "AA"
    [|"AA"; "~"|];
  test_search_forward r n "AAA"
    [|"AAA"; "A"|];
  test_search_forward r n "AbA"
    [|"AbA"; "b"|];
  test_search_forward r n "A"
    [||];

  start_test "Search for /\\([ab]*\\)\\1+c/";
  let r = Str.regexp "\\([ab]*\\)\\1+c" in
  let n = 1 in
  test_search_forward r n "abababc"
    [| "abababc"; "ab" |];
  test_search_forward r n "abbc"
    [| "bbc"; "b" |];
  test_search_forward r n "abc"
    [| "c"; "" |];

  start_test "Search for /^\\(\\(b+\\|a\\)\\(b+\\|a\\)?\\)?bc/";
  let r = Str.regexp "^\\(\\(b+\\|a\\)\\(b+\\|a\\)?\\)?bc" in
  let n = 3 in
  test_search_forward r n "bbc"
    [|"bbc"; "b"; "b"; "~"|];

  start_test "Search for /^\\(\\(b*\\|ba\\)\\(b*\\|ba\\)?\\)?bc/";
  let r = Str.regexp "^\\(\\(b*\\|ba\\)\\(b*\\|ba\\)?\\)?bc" in
  let n = 3 in
  test_search_forward r n "babc"
    [|"babc"; "ba"; ""; "ba"|];
  test_search_forward r n "bbabc"
    [|"bbabc"; "bba"; "b"; "ba"|];
  test_search_forward r n "bababc"
    [|"bababc"; "baba"; "ba"; "ba"|];
  test_search_forward r n "bababbc"
    [||];
  test_search_forward r n "babababc"
    [||];

  start_test "Search for /[^a]/";
  let r = Str.regexp "[^a]" in
  let n = 0 in
  test_search_forward r n "athing" [|"t"|];
  test_search_forward r n "Athing" [|"A"|];

  start_test "Search for /[^a]/ (case-insensitive)";
  let r = Str.regexp_case_fold "[^a]" in
  let n = 0 in
  test_search_forward r n "athing" [|"t"|];
  test_search_forward r n "Athing" [|"t"|];

  start_test "Search for /^[]abcde]/";
  let r = Str.regexp "^[]abcde]" in
  let n = 0 in
  test_search_forward r n "athing"
    [|"a"|];
  test_search_forward r n "bthing"
    [|"b"|];
  test_search_forward r n "]thing"
    [|"]"|];
  test_search_forward r n "cthing"
    [|"c"|];
  test_search_forward r n "dthing"
    [|"d"|];
  test_search_forward r n "ething"
    [|"e"|];
  test_search_forward r n "fthing"
    [||];
  test_search_forward r n "[thing"
    [||];
  test_search_forward r n "\\\\thing"
    [||];

  start_test "Search for /^[]cde]/";
  let r = Str.regexp "^[]cde]" in
  let n = 0 in
  test_search_forward r n "]thing"
    [|"]"|];
  test_search_forward r n "cthing"
    [|"c"|];
  test_search_forward r n "dthing"
    [|"d"|];
  test_search_forward r n "ething"
    [|"e"|];
  test_search_forward r n "athing"
    [||];
  test_search_forward r n "fthing"
    [||];

  start_test "Search for /^[^]abcde]/";
  let r = Str.regexp "^[^]abcde]" in
  let n = 0 in
  test_search_forward r n "fthing"
    [|"f"|];
  test_search_forward r n "[thing"
    [|"["|];
  test_search_forward r n "\\\\thing"
    [|"\\"|];
  test_search_forward r n "athing"
    [||];
  test_search_forward r n "bthing"
    [||];
  test_search_forward r n "]thing"
    [||];
  test_search_forward r n "cthing"
    [||];
  test_search_forward r n "dthing"
    [||];
  test_search_forward r n "ething"
    [||];

  start_test "Search for /^[^]cde]/";
  let r = Str.regexp "^[^]cde]" in
  let n = 0 in
  test_search_forward r n "athing"
    [|"a"|];
  test_search_forward r n "fthing"
    [|"f"|];
  test_search_forward r n "]thing"
    [||];
  test_search_forward r n "cthing"
    [||];
  test_search_forward r n "dthing"
    [||];
  test_search_forward r n "ething"
    [||];

  start_test "Search for /^ÿ/";
  let r = Str.regexp "^ÿ" in
  let n = 0 in
  test_search_forward r n "ÿ"
    [|"ÿ"|];

  start_test "Search for /^[0-9]+$/";
  let r = Str.regexp "^[0-9]+$" in
  let n = 0 in
  test_search_forward r n "0"
    [|"0"|];
  test_search_forward r n "1"
    [|"1"|];
  test_search_forward r n "2"
    [|"2"|];
  test_search_forward r n "3"
    [|"3"|];
  test_search_forward r n "4"
    [|"4"|];
  test_search_forward r n "5"
    [|"5"|];
  test_search_forward r n "6"
    [|"6"|];
  test_search_forward r n "7"
    [|"7"|];
  test_search_forward r n "8"
    [|"8"|];
  test_search_forward r n "9"
    [|"9"|];
  test_search_forward r n "10"
    [|"10"|];
  test_search_forward r n "100"
    [|"100"|];
  test_search_forward r n "abc"
    [||];

  start_test "Search for /^.*nter/";
  let r = Str.regexp "^.*nter" in
  let n = 0 in
  test_search_forward r n "enter"
    [|"enter"|];
  test_search_forward r n "inter"
    [|"inter"|];
  test_search_forward r n "uponter"
    [|"uponter"|];

  start_test "Search for /^xxx[0-9]+$/";
  let r = Str.regexp "^xxx[0-9]+$" in
  let n = 0 in
  test_search_forward r n "xxx0"
    [|"xxx0"|];
  test_search_forward r n "xxx1234"
    [|"xxx1234"|];
  test_search_forward r n "xxx"
    [||];

  start_test "Search for /^.+[0-9][0-9][0-9]$/";
  let r = Str.regexp "^.+[0-9][0-9][0-9]$" in
  let n = 0 in
  test_search_forward r n "x123"
    [|"x123"|];
  test_search_forward r n "xx123"
    [|"xx123"|];
  test_search_forward r n "123456"
    [|"123456"|];
  test_search_forward r n "123"
    [||];
  test_search_forward r n "x123x"
    [||];

  start_test "Search for /^\\([^!]+\\)!\\(.+\\)=apquxz\\.ixr\\.zzz\\.ac\\.uk$/";
  let r = Str.regexp "^\\([^!]+\\)!\\(.+\\)=apquxz\\.ixr\\.zzz\\.ac\\.uk$" in
  let n = 2 in
  test_search_forward r n "abc!pqr=apquxz.ixr.zzz.ac.uk"
    [|"abc!pqr=apquxz.ixr.zzz.ac.uk"; "abc"; "pqr"|];
  test_search_forward r n "!pqr=apquxz.ixr.zzz.ac.uk"
    [||];
  test_search_forward r n "abc!=apquxz.ixr.zzz.ac.uk"
    [||];
  test_search_forward r n "abc!pqr=apquxz:ixr.zzz.ac.uk"
    [||];
  test_search_forward r n "abc!pqr=apquxz.ixr.zzz.ac.ukk"
    [||];

  start_test "Search for /\\([0-9a-f:]+\\)$/";
  let r = Str.regexp_case_fold "\\([0-9a-f:]+\\)$" in
  let n = 1 in
  test_search_forward r n "0abc"
    [|"0abc"; "0abc"|];
  test_search_forward r n "abc"
    [|"abc"; "abc"|];
  test_search_forward r n "fed"
    [|"fed"; "fed"|];
  test_search_forward r n "E"
    [|"E"; "E"|];
  test_search_forward r n "::"
    [|"::"; "::"|];
  test_search_forward r n "5f03:12C0::932e"
    [|"5f03:12C0::932e"; "5f03:12C0::932e"|];
  test_search_forward r n "fed def"
    [|"def"; "def"|];
  test_search_forward r n "Any old stuff"
    [|"ff"; "ff"|];
  test_search_forward r n "0zzz"
    [||];
  test_search_forward r n "gzzz"
    [||];
  test_search_forward r n "fed "
    [||];
  test_search_forward r n "Any old rubbish"
    [||];

  start_test "Search for /^[a-z0-9][a-z0-9-]*\\(\\.[a-z0-9][A-Z0-9-]*\\)*\\.$/";
  let r = Str.regexp_case_fold "^[a-z0-9][a-z0-9-]*\\(\\.[a-z0-9][A-Z0-9-]*\\)*\\.$" in
  let n = 1 in
  test_search_forward r n "a."
    [|"a."; "~"|];
  test_search_forward r n "Z."
    [|"Z."; "~"|];
  test_search_forward r n "2."
    [|"2."; "~"|];
  test_search_forward r n "ab-c."
    [|"ab-c."; "~"|];
  test_search_forward r n "ab-c.pq-r."
    [|"ab-c.pq-r."; ".pq-r"|];
  test_search_forward r n "sxk.zzz.ac.uk."
    [|"sxk.zzz.ac.uk."; ".uk"|];
  test_search_forward r n "sxk.ZZZ.ac.UK."
    [|"sxk.ZZZ.ac.UK."; ".UK"|];
  test_search_forward r n "x-.y-."
    [|"x-.y-."; ".y-"|];
  test_search_forward r n "-abc.peq."
    [||];

  start_test "Search for /^\\*\\.[a-z]\\([a-z0-9-]*[a-z0-9]+\\)?\\(\\.[a-z]\\([a-z0-9-]*[a-z0-9]+\\)?\\)*$/";
  let r = Str.regexp "^\\*\\.[a-z]\\([a-z0-9-]*[a-z0-9]+\\)?\\(\\.[a-z]\\([a-z0-9-]*[a-z0-9]+\\)?\\)*$" in
  let n = 3 in
  test_search_forward r n "*.a"
    [|"*.a"; "~"; "~"; "~"|];
  test_search_forward r n "*.b0-a"
    [|"*.b0-a"; "0-a"; "~"; "~"|];
  test_search_forward r n "*.c3-b.c"
    [|"*.c3-b.c"; "3-b"; ".c"; "~"|];
  test_search_forward r n "*.c-a.b-c"
    [|"*.c-a.b-c"; "-a"; ".b-c"; "-c"|];
  test_search_forward r n "*.0"
    [||];
  test_search_forward r n "*.a-"
    [||];
  test_search_forward r n "*.a-b.c-"
    [||];
  test_search_forward r n "*.c-a.0-c"
    [||];

  start_test "Search for /^[0-9a-fA-F]\\(\\.[0-9a-fA-F]\\)*$/";
  let r = Str.regexp "^[0-9a-fA-F]\\(\\.[0-9a-fA-F]\\)*$" in
  let n = 1 in
  test_search_forward r n "a.b.c.d"
    [|"a.b.c.d"; ".d"|];
  test_search_forward r n "A.B.C.D"
    [|"A.B.C.D"; ".D"|];
  test_search_forward r n "a.b.c.1.2.3.C"
    [|"a.b.c.1.2.3.C"; ".C"|];
  test_search_forward r n "a.b.c.dz"
    [||];
  test_search_forward r n "za"
    [||];

  start_test "Search for /^\\\".*\\\" *\\(;.*\\)?$/";
  let r = Str.regexp "^\\\".*\\\" *\\(;.*\\)?$" in
  let n = 1 in
  test_search_forward r n "\"1234\""
    [|"\"1234\""; "~"|];
  test_search_forward r n "\"abcd\" ;"
    [|"\"abcd\" ;"; ";"|];
  test_search_forward r n "\"\" ; rhubarb"
    [|"\"\" ; rhubarb"; "; rhubarb"|];
  test_search_forward r n "\"1234\" : things"
    [||];

  start_test "Search for /^\\(a\\(b\\(c\\)\\)\\)\\(d\\(e\\(f\\)\\)\\)\\(h\\(i\\(j\\)\\)\\)$/";
  let r = Str.regexp "^\\(a\\(b\\(c\\)\\)\\)\\(d\\(e\\(f\\)\\)\\)\\(h\\(i\\(j\\)\\)\\)$" in
  let n = 9 in
  test_search_forward r n "abcdefhij"
    [|"abcdefhij"; "abc"; "bc"; "c"; "def"; "ef"; "f"; "hij"; "ij"; "j"|];

  start_test "Search for /^[.^$|()*+?{,}]+/";
  let r = Str.regexp "^[.^$|()*+?{,}]+" in
  let n = 0 in
  test_search_forward r n ".^$*(+)|{?,?}"
    [|".^$*(+)|{?,?}"|];

  start_test "Search for /\\(cat\\(a\\(ract\\|tonic\\)\\|erpillar\\)\\) \\1\\(\\)2\\(3\\)/";
  let r = Str.regexp "\\(cat\\(a\\(ract\\|tonic\\)\\|erpillar\\)\\) \\1\\(\\)2\\(3\\)" in
  let n = 5 in
  test_search_forward r n "cataract cataract23"
    [|"cataract cataract23"; "cataract"; "aract"; "ract"; ""; "3"|];
  test_search_forward r n "catatonic catatonic23"
    [|"catatonic catatonic23"; "catatonic"; "atonic"; "tonic"; ""; "3"|];
  test_search_forward r n "caterpillar caterpillar23"
    [|"caterpillar caterpillar23"; "caterpillar"; "erpillar"; "~"; ""; "3"|];

  start_test "Search for /^From +\\([^ ]+\\) +[a-zA-Z][a-zA-Z][a-zA-Z] +[a-zA-Z][a-zA-Z][a-zA-Z] +[0-9]?[0-9] +[0-9][0-9]:[0-9][0-9]/";
  let r = Str.regexp "^From +\\([^ ]+\\) +[a-zA-Z][a-zA-Z][a-zA-Z] +[a-zA-Z][a-zA-Z][a-zA-Z] +[0-9]?[0-9] +[0-9][0-9]:[0-9][0-9]" in
  let n = 1 in
  test_search_forward r n "From abcd  Mon Sep 01 12:33:02 1997"
    [|"From abcd  Mon Sep 01 12:33"; "abcd"|];

  start_test "Search for /\\ba/";
  let r = Str.regexp "\\ba" in
  let n = 0 in
  test_search_forward r n "a2cd"
    [|"a"|];
  test_search_forward r n "the a"
    [|"a"|];
  test_search_forward r n ".ab"
    [|"a"|];
  test_search_forward r n "bad"
    [||];
  test_search_forward r n "the ba"
    [||];
  test_search_forward r n "ba."
    [||];

  start_test "Search for /a\\b/";
  let r = Str.regexp "a\\b" in
  let n = 0 in
  test_search_forward r n "a"
    [|"a"|];
  test_search_forward r n "bc_a"
    [|"a"|];
  test_search_forward r n "a foo"
    [|"a"|];
  test_search_forward r n "a."
    [|"a"|];
  test_search_forward r n "bad"
    [||];
  test_search_forward r n "ab"
    [||];

  start_test "Search for /\\([a-z]*\\)b/";
  let r = Str.regexp "\\([a-z]*\\)b" in
  let n = 1 in
  test_search_forward r n "abbb"
    [|"abbb"; "abb"|];

  start_test "Search for /\\([a-z]+\\)b/";
  let r = Str.regexp "\\([a-z]+\\)b" in
  let n = 1 in
  test_search_forward r n "abbb"
    [|"abbb"; "abb"|];

  start_test "Search for /\\([a-z]?\\)b/";
  let r = Str.regexp "\\([a-z]?\\)b" in
  let n = 1 in
  test_search_forward r n "bbbb"
    [|"bb"; "b"|];

  start_test "Search for /^a/";
  let r = Str.regexp "^a" in
  let n = 0 in
  test_search_forward r n "abcdef"
    [|"a"|];
  test_search_forward r n "zzzz\nabcdef"
    [|"a"|];

  start_test "Search for /a$/";
  let r = Str.regexp "a$" in
  let n = 0 in
  test_search_forward r n "xyza"
    [|"a"|];
  test_search_forward r n "xyza\nbcdef"
    [|"a"|];

  start_test "Null characters in regexps";
  let r = Str.regexp "ab\000cd" in
  let n = 0 in
  test_search_forward r n "qerpoiuab\000cdwerltkh"
    [| "ab\000cd" |];
  let r = Str.regexp "\000cd" in
  let n = 0 in
  test_search_forward r n "qerpoiuab\000cdwerltkh"
    [| "\000cd" |];

  (** Backward searches *)
  start_test "Backward search for /the quick/";
  let r = Str.regexp "the quick" in
  let n = 0 in
  test_search_backward r n "the quick brown fox"
    [|"the quick"|];
  test_search_backward r n "What do you know about the quick brown fox?"
    [|"the quick"|];
  test_search_backward r n "The quick brown FOX"
    [||];
  test_search_backward r n "What do you know about THE QUICK BROWN FOX?"
    [||];

  start_test "Backward search for /a\\([0-9]+\\)/";
  let r = Str.regexp "a\\([0-9]+\\)" in
  let n = 1 in
  test_search_backward r n "a123 a456zzzz"
    [|"a456"; "456"|];
  test_search_backward r n "ab123"
    [||];

  (** Partial match searches *)

  start_test "Partial match for /partial match/";
  let r = Str.regexp "partial match" in
  let n = 0 in
  test_partial_match r n ""
    [|""|];
  test_partial_match r n "partial matching"
    [|"partial match"|];
  test_partial_match r n "partial m"
    [|"partial m"|];

  start_test "Partial match for /\\(partial\\)\\|\\(match\\)/";
  let r = Str.regexp "\\(partial\\)\\|\\(match\\)" in
  let n = 2 in
  test_partial_match r n ""
    [|""; "~"; "~"|];
  test_partial_match r n "part"
    [|"part"; "~"; "~"|];
  test_partial_match r n "partial"
    [|"partial"; "partial"; "~"|];
  test_partial_match r n "matching"
    [|"match"; "~"; "match"|];
  test_partial_match r n "mat"
    [|"mat"; "~"; "~"|];
  test_partial_match r n "zorglub"
    [||];

  (** Replacement *)
  start_test "Global replacement";
  test (Str.global_replace (Str.regexp "[aeiou]") ".."
          "abcdefghijklmnopqrstuvwxyz")
       "..bcd..fgh..jklmn..pqrst..vwxyz";
  test (Str.global_replace (Str.regexp "[0-9]\\([0-9]*\\)") "-\\0-\\1-"
          "abc012def3ghi45")
       "abc-012-12-def-3--ghi-45-5-";
  test (Str.global_replace (Str.regexp "[0-9]?") "."
          "abc012def3ghi45")
       ".a.b.c....d.e.f..g.h.i...";

  start_test "First replacement";
  test (Str.replace_first (Str.regexp "[eiou]") ".."
          "abcdefghijklmnopqrstuvwxyz")
       "abcd..fghijklmnopqrstuvwxyz";
  test (Str.replace_first (Str.regexp "[0-9]\\([0-9]*\\)") "-\\0-\\1-"
          "abc012def3ghi45")
       "abc-012-12-def3ghi45";

  (** Splitting *)
  start_test "Splitting";
  test (Str.split (Str.regexp "[ \t]+") "si non e vero")
       ["si"; "non"; "e"; "vero"];
  test (Str.split (Str.regexp "[ \t]+") " si non\te vero\t")
       ["si"; "non"; "e"; "vero"];
  test (Str.bounded_split (Str.regexp "[ \t]+") " si non e vero " 3)
       ["si"; "non"; "e vero "];
  test (Str.split (Str.regexp "[ \t]*") "si non e vero")
       ["s"; "i"; "n"; "o"; "n"; "e"; "v"; "e"; "r"; "o"];
  test (Str.split_delim (Str.regexp "[ \t]+") " si non e vero\t")
       [""; "si"; "non"; "e"; "vero"; ""];
  test (Str.full_split (Str.regexp "[ \t]+") " si non\te vero\t")
       [Str.Delim " "; Str.Text "si";
        Str.Delim " "; Str.Text "non";
        Str.Delim "\t"; Str.Text "e";
        Str.Delim " "; Str.Text "vero"; Str.Delim "\t"];

  (** XML tokenization *)
  (* See "REX: XML Shallow Parsing with Regular Expressions",
     Robert D. Cameron, Simon Fraser University, CMPT TR 1998-17. *)
  start_test "XML tokenization";
  begin
    let _TextSE = "[^<]+" in
    let _UntilHyphen = "[^-]*-" in
    let _Until2Hyphens = _UntilHyphen ^ "\\([^-]" ^ _UntilHyphen ^ "\\)*-" in
    let _CommentCE = _Until2Hyphens ^ ">?" in
    let _UntilRSBs = "[^]]*]\\([^]]+]\\)*]+" in
    let _CDATA_CE = _UntilRSBs ^ "\\([^]>]" ^ _UntilRSBs ^ "\\)*>" in
    let _S = "[ \n\t\r]+" in
    let _NameStrt = "[A-Za-z_:]\\|[^\x00-\x7F]" in
    let _NameChar = "[A-Za-z0-9_:.-]\\|[^\x00-\x7F]" in
    let _Name = "\\(" ^ _NameStrt ^ "\\)\\(" ^ _NameChar ^ "\\)*" in
    let _QuoteSE = "\"[^\"]*\"\\|'[^']*'" in
    let _DT_IdentSE = _S ^ _Name ^ "\\(" ^ _S ^ "\\(" ^ _Name ^ "\\|" ^ _QuoteSE ^ "\\)\\)*" in
    let _MarkupDeclCE = "\\([^]\"'><]\\|" ^ _QuoteSE ^ "\\)*>" in
    let _S1 = "[\n\r\t ]" in
    let _UntilQMs = "[^?]*\\?+" in
    let _PI_Tail = "\\?>\\|" ^ _S1 ^ _UntilQMs ^ "\\([^>?]" ^ _UntilQMs ^ "\\)*>" in
    let _DT_ItemSE = "<\\(!\\(--" ^ _Until2Hyphens ^ ">\\|[^-]" ^ _MarkupDeclCE ^ "\\)\\|\\?" ^ _Name ^ "\\(" ^ _PI_Tail ^ "\\)\\)\\|%" ^ _Name ^ ";\\|" ^ _S1 in
    let _DocTypeCE = _DT_IdentSE ^ "\\(" ^ _S ^ "\\)?\\(\\[\\(" ^ _DT_ItemSE ^ "\\)*]\\(" ^ _S ^ "\\)?\\)?>?" in
    let _DeclCE = "--\\(" ^ _CommentCE ^ "\\)?\\|\\[_CDATA\\[\\(" ^ _CDATA_CE ^ "\\)?\\|_DOCTYPE\\(" ^ _DocTypeCE ^ "\\)?" in
    let _PI_CE = _Name ^ "\\(" ^ _PI_Tail ^ "\\)?" in
    let _EndTagCE = _Name ^ "\\(" ^ _S ^ "\\)?>?" in
    let _AttValSE = "\"[^<\"]*\"\\|'[^<']*'" in
    let _ElemTagCE = _Name ^ "\\(" ^ _S ^ _Name ^ "\\(" ^ _S ^ "\\)?=\\(" ^ _S ^ "\\)?\\(" ^ _AttValSE ^ "\\)\\)*\\(" ^ _S ^ "\\)?/?>?" in
    let _MarkupSPE = "<\\(!\\(" ^ _DeclCE ^ "\\)?\\|\\?\\(" ^ _PI_CE ^ "\\)?\\|/\\(" ^ _EndTagCE ^ "\\)?\\|\\(" ^ _ElemTagCE ^ "\\)?\\)" in
    let _XML_SPE = _TextSE ^ "\\|" ^ _MarkupSPE in
    let input = "\
<?xml version=\"1.0\"?>
<?xml-stylesheet type=\"text/css\" href=\"nutrition.css\"?>
<!DOCTYPE root [
   <!ELEMENT root (stem)>
   <!ELEMENT stem EMPTY>
]>
<!ELEMENT name (#PCDATA)>
<![CDATA[my
escaped text]]> 
<nutrition>
<daily-values>
	<total-fat units=\"g\">65</total-fat>
	<saturated-fat units=\"g\">20</saturated-fat>
	<cholesterol units=\"mg\">300</cholesterol>
	<sodium units=\"mg\">2400</sodium>
	<carb units=\"g\">300</carb>
	<fiber units=\"g\">25</fiber>
	<protein units=\"g\">50</protein>
</daily-values>
<food>
	<name>Avocado Dip</name>
	<mfr>Sunnydale</mfr>
	<serving units=\"g\">29</serving>
	<calories total=\"110\" fat=\"100\"/>
	<total-fat>11</total-fat>
	<saturated-fat>3</saturated-fat>
	<cholesterol>5</cholesterol>
	<sodium>210</sodium>
	<carb>2</carb>
	<fiber>0</fiber>
	<protein>1</protein>
	<vitamins>
		<a>0</a>
		<c>0</c>
	</vitamins>
	<minerals>
		<ca>0</ca>
		<fe>0</fe>
	</minerals>
</food>
<!--
<food>
	<name></name>
	<mfr></mfr>
	<serving units=\"g\"></serving>
	<calories total=\"\" fat=\"\"/>
	<total-fat></total-fat>
	<saturated-fat></saturated-fat>
	<cholesterol></cholesterol>
	<sodium></sodium>
	<carb></carb>
	<fiber></fiber>
	<protein></protein>
	<vitamins>
		<a></a>
		<c></c>
	</vitamins>
	<minerals>
		<ca></ca>
		<fe></fe>
	</minerals>
</food>
-->
" in
    let result = [
  "<?xml version=\"1.0\"?>";
  "\n";
  "<?xml-stylesheet type=\"text/css\" href=\"nutrition.css\"?>";
  "\n";
  "<!";
  "DOCTYPE root [\n   ";
  "<!";
  "ELEMENT root (stem)>\n   ";
  "<!";
  "ELEMENT stem EMPTY>\n]>\n";
  "<!";
  "ELEMENT name (#PCDATA)>\n";
  "<!";
  "[CDATA[my\nescaped text]]> \n";
  "<nutrition>";
  "\n";
  "<daily-values>";
  "\n\t";
  "<total-fat units=\"g\">";
  "65";
  "</total-fat>";
  "\n\t";
  "<saturated-fat units=\"g\">";
  "20";
  "</saturated-fat>";
  "\n\t";
  "<cholesterol units=\"mg\">";
  "300";
  "</cholesterol>";
  "\n\t";
  "<sodium units=\"mg\">";
  "2400";
  "</sodium>";
  "\n\t";
  "<carb units=\"g\">";
  "300";
  "</carb>";
  "\n\t";
  "<fiber units=\"g\">";
  "25";
  "</fiber>";
  "\n\t";
  "<protein units=\"g\">";
  "50";
  "</protein>";
  "\n";
  "</daily-values>";
  "\n";
  "<food>";
  "\n\t";
  "<name>";
  "Avocado Dip";
  "</name>";
  "\n\t";
  "<mfr>";
  "Sunnydale";
  "</mfr>";
  "\n\t";
  "<serving units=\"g\">";
  "29";
  "</serving>";
  "\n\t";
  "<calories total=\"110\" fat=\"100\"/>";
  "\n\t";
  "<total-fat>";
  "11";
  "</total-fat>";
  "\n\t";
  "<saturated-fat>";
  "3";
  "</saturated-fat>";
  "\n\t";
  "<cholesterol>";
  "5";
  "</cholesterol>";
  "\n\t";
  "<sodium>";
  "210";
  "</sodium>";
  "\n\t";
  "<carb>";
  "2";
  "</carb>";
  "\n\t";
  "<fiber>";
  "0";
  "</fiber>";
  "\n\t";
  "<protein>";
  "1";
  "</protein>";
  "\n\t";
  "<vitamins>";
  "\n\t\t";
  "<a>";
  "0";
  "</a>";
  "\n\t\t";
  "<c>";
  "0";
  "</c>";
  "\n\t";
  "</vitamins>";
  "\n\t";
  "<minerals>";
  "\n\t\t";
  "<ca>";
  "0";
  "</ca>";
  "\n\t\t";
  "<fe>";
  "0";
  "</fe>";
  "\n\t";
  "</minerals>";
  "\n";
  "</food>";
  "\n";
  "<!--\n<food>\n\t<name></name>\n\t<mfr></mfr>\n\t<serving units=\"g\"></serving>\n\t<calories total=\"\" fat=\"\"/>\n\t<total-fat></total-fat>\n\t<saturated-fat></saturated-fat>\n\t<cholesterol></cholesterol>\n\t<sodium></sodium>\n\t<carb></carb>\n\t<fiber></fiber>\n\t<protein></protein>\n\t<vitamins>\n\t\t<a></a>\n\t\t<c></c>\n\t</vitamins>\n\t<minerals>\n\t\t<ca></ca>\n\t\t<fe></fe>\n\t</minerals>\n</food>\n-->";
  "\n"] in
    let re = Str.regexp _XML_SPE in
    let rec process i l =
      let j = try Str.search_forward re input i with Not_found -> (-1) in
      if j < 0 then begin
        test l []
      end else begin
        match l with
          [] -> test 0 1 (* failure *)
        | hd :: tl ->
            test (Str.matched_string input) hd; process (Str.match_end()) tl
      end in
    process 0 result
  end;

  end_test()

let manual_test regexp text =
  try
    ignore (Str.search_forward (Str.regexp regexp) text 0);
    printf "Matched,";
    begin try
      for i = 0 to 31 do
        try
          let s = Str.matched_group i text in
          printf " \\%d=%s" i s
        with Not_found ->
          ()
      done
    with Invalid_argument "Str.matched_group" -> (*yuck*)
      ()
    end;
    print_newline()
  with Not_found ->
    printf "Not matched\n"

let _ =
  if Array.length Sys.argv >= 3
  then manual_test Sys.argv.(1) Sys.argv.(2)
  else automated_test()
