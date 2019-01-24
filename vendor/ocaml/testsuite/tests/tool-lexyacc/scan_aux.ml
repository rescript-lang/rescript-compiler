(* Auxiliaries for the lexical analyzer *)

let brace_depth = ref 0
let comment_depth = ref 0

exception Lexical_error of string

let initial_string_buffer = Bytes.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0


let store_string_char c =
  begin
    if !string_index >= Bytes.length !string_buff then begin
      let new_buff = Bytes.create (Bytes.length !string_buff * 2) in
      Bytes.blit new_buff 0 !string_buff 0 (Bytes.length !string_buff);
      string_buff := new_buff
    end
  end;
  Bytes.unsafe_set !string_buff !string_index c;
  incr string_index

let get_stored_string () =
  let s = Bytes.sub_string !string_buff 0 !string_index in
  string_buff := initial_string_buffer;
  s


let char_for_backslash = function
    'n' -> '\010' (* '\n' when bootstrapped *)
  | 't' -> '\009' (* '\t' *)
  | 'b' -> '\008' (* '\b' *)
  | 'r' -> '\013' (* '\r' *)
  | c   -> c


let char_for_decimal_code lexbuf i =
  Char.chr(100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
            10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                 (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48))
