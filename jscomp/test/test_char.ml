let caml_is_printable c = 
  let code = Char.code c in
  code > 31 && code < 127