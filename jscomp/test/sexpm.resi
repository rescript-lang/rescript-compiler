/* This file is free software, part of containers. See file "license" for more details. */

/* {1 Simple and efficient S-expression parsing/printing}

    @since 0.7 " */

type or_error<'a> = [#Ok('a) | #Error(string)]
type sequence<'a> = ('a => unit) => unit
type gen<'a> = unit => option<'a>

/* {2 Basics} */

type rec t = [
  | #Atom(string)
  | #List(list<t>)
]
type sexp = t

/* {2 Serialization (encoding)} */

let to_buf: (Buffer.t, t) => unit

let to_string: t => string

/** Parse a string */
let parse_string: string => or_error<t>
