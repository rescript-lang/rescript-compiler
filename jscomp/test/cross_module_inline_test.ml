external is_printable : char -> bool = "caml_is_printable"

let v = is_printable 'a'
