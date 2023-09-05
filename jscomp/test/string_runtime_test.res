module S = Caml_string
/** TODO: performance could be improved, however, 
    this function is not in critical Path
 */
module B = Caml_bytes

/** TODO: performance could be improved, however, 
    this function is not in critical Path
 */
let suites = {
  open Mt
  list{
    /* "string_of_char_array", (fun _ -> 
    Eq(S.caml_string_of_char_array [|'a';'b';'c'|], "abc")
                          ); */
    ("?is_printable", _ => Eq(Test_char.caml_is_printable('a'), true)),
    (
      "?string_of_bytes",
      _ => {
        let f = len => {
          let b = Bytes.create(len)
          Bytes.fill(b, 0, len, 'c')
          (Bytes.to_string(b), String.init(len, _ => 'c'))
        }
        let (a, b) = \"@@"(
          List.split,
          List.map(x => f(x), list{1000, 1024, 1025, 4095, 4096, 5000, 10000}),
        )
        Eq(a, b)
      },
    ),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
