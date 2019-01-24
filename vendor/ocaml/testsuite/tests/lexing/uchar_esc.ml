
(* Correct escapes and their encoding *)

let () =
  assert ("\xF0\x9F\x90\xAB" = "\u{1F42B}");
  assert ("\xF0\x9F\x90\xAB" = "\u{01F42B}");
  assert ("\x00" = "\u{0}");
  assert ("\x00" = "\u{00}");
  assert ("\x00" = "\u{000}");
  assert ("\x00" = "\u{0000}");
  assert ("\x00" = "\u{00000}");
  assert ("\x00" = "\u{000000}");
  assert ("\xC3\xA9" = "\u{E9}");
  assert ("\xC3\xA9" = "\u{0E9}");
  assert ("\xC3\xA9" = "\u{00E9}");
  assert ("\xC3\xA9" = "\u{000E9}");
  assert ("\xC3\xA9" = "\u{0000E9}");
  assert ("\xC3\xA9" = "\u{0000E9}");
  assert ("\xF4\x8F\xBF\xBF" = "\u{10FFFF}");
  ()
;;


(* Errors *)

let invalid_sv = "\u{0D800}" ;;
let invalid_sv = "\u{D800}" ;;
let invalid_sv = "\u{D900}" ;;
let invalid_sv = "\u{DFFF}" ;;
let invalid_sv = "\u{110000} ;;

let too_many_digits = "\u{01234567}" ;;
let no_hex_digits = "\u{}" ;;
let illegal_hex_digit = "\u{u}" ;;
