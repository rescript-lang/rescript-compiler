external string_length : string -> int = "%string_length"
external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
                     = "caml_blit_string" "noalloc"

external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"
open Bytes
let escaped s =
  let n = ref 0 in
  for i = 0 to length s - 1 do
    n := !n +
      (match unsafe_get s i with
       | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | c -> if Test_char.caml_is_printable c then 1 else 4)
  done;
  if !n = length s then copy s else begin
    let s' = create !n in
    n := 0;
    for i = 0 to length s - 1 do
      begin match unsafe_get s i with
      | ('"' | '\\') as c ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
      | '\n' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
      | '\t' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
      | '\r' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'r'
      | '\b' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'b'
      | c ->
          if Test_char.caml_is_printable c then
            unsafe_set s' !n c
          else begin
            let a = char_code c in
            unsafe_set s' !n '\\';
            incr n;
            unsafe_set s' !n (char_chr (48 + a / 100));
            incr n;
            unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
            incr n;
            unsafe_set s' !n (char_chr (48 + a mod 10))
          end
      end;
      incr n
    done;
    s'
  end
