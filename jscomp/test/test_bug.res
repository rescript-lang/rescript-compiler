external string_length: string => int = "%string_length"
external unsafe_blit_string: (string, int, bytes, int, int) => unit = "?blit_string"

external char_code: char => int = "%identity"
external char_chr: int => char = "%identity"
open Bytes
let escaped = s => {
  let n = ref(0)
  for i in 0 to length(s) - 1 {
    n :=
      n.contents +
      switch unsafe_get(s, i) {
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' => 2
      | c =>
        if Test_char.caml_is_printable(c) {
          1
        } else {
          4
        }
      }
  }
  if n.contents == length(s) {
    copy(s)
  } else {
    let s' = create(n.contents)
    n := 0
    for i in 0 to length(s) - 1 {
      switch unsafe_get(s, i) {
      | ('"' | '\\') as c =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, c)
      | '\n' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 'n')
      | '\t' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 't')
      | '\r' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 'r')
      | '\b' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 'b')
      | c =>
        if Test_char.caml_is_printable(c) {
          unsafe_set(s', n.contents, c)
        } else {
          let a = char_code(c)
          unsafe_set(s', n.contents, '\\')
          incr(n)
          unsafe_set(s', n.contents, char_chr(48 + a / 100))
          incr(n)
          unsafe_set(s', n.contents, char_chr(48 + mod(a / 10, 10)))
          incr(n)
          unsafe_set(s', n.contents, char_chr(48 + mod(a, 10)))
        }
      }
      incr(n)
    }
    s'
  }
}
