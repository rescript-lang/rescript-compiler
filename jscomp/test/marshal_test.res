@module("./marshal_bin")
external marshal: 'a => string = "marshal"

@module("./marshal_bin")
external unmarshal: string => 'a = "unmarshal"

@module("./marshal_bin")
external caml_read_file_content: string => 'a = "caml_read_file_content"

// let s = marshal((3, 4, 5))
// let s1 = marshal(42)
// Js.log2("s1", s1)

let s = caml_read_file_content("./aaa.marshal")

@unboxed
type rec myList<'a> = | @as(0) Empty | Cons((unknown, int, myList<'a>))

let v: myList<int> = unmarshal(s)

let rec sum = l =>
  switch l {
  | Empty => 0
  | Cons((_, i, l)) => i + sum(l)
  }

Js.log2("v", v)
Js.log2("sum:", sum(v))
