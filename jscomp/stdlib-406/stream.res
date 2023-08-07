/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1997 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

type rec t<'a> = option<cell<'a>>
and cell<'a> = {mutable count: int, mutable data: data<'a>}
and data<'a> =
  | Sempty
  | Scons('a, data<'a>)
  | Sapp(data<'a>, data<'a>)
  | Slazy(Lazy.t<data<'a>>)
  | Sgen(gen<'a>)
and gen<'a> = {mutable curr: option<option<'a>>, func: int => option<'a>}

exception Failure
exception Error(string)

let count = param =>
  switch param {
  | None => 0
  | Some({count}) => count
  }
let data = param =>
  switch param {
  | None => Sempty
  | Some({data}) => data
  }

let rec get_data:
  type v. (int, data<v>) => data<v> =
  (count, d) =>
    switch d {
    /* Returns either Sempty or Scons(a, _) even when d is a generator
    or a buffer. In those cases, the item a is seen as extracted from
 the generator/buffer.
 The count parameter is used for calling `Sgen-functions'. */
    | Sempty | Scons(_, _) => d
    | Sapp(d1, d2) =>
      switch get_data(count, d1) {
      | Scons(a, d11) => Scons(a, Sapp(d11, d2))
      | Sempty => get_data(count, d2)
      | _ => assert(false)
      }
    | Sgen({curr: Some(None)}) => Sempty
    | Sgen({curr: Some(Some(a))} as g) =>
      g.curr = None
      Scons(a, d)
    | Sgen(g) =>
      switch g.func(count) {
      | None =>
        g.curr = Some(None)
        Sempty
      | Some(a) => Scons(a, d)
      /* Warning: anyone using g thinks that an item has been read */
      }
    | Slazy(f) => get_data(count, Lazy.force(f))
    }

let rec peek_data:
  type v. cell<v> => option<v> =
  s =>
    /* consult the first item of s */
    switch s.data {
    | Sempty => None
    | Scons(a, _) => Some(a)
    | Sapp(_, _) =>
      switch get_data(s.count, s.data) {
      | Scons(a, _) as d =>
        s.data = d
        Some(a)
      | Sempty => None
      | _ => assert(false)
      }
    | Slazy(f) =>
      s.data = Lazy.force(f)
      peek_data(s)
    | Sgen({curr: Some(a)}) => a
    | Sgen(g) =>
      let x = g.func(s.count)
      g.curr = Some(x)
      x
    }

let peek = param =>
  switch param {
  | None => None
  | Some(s) => peek_data(s)
  }

let rec junk_data:
  type v. cell<v> => unit =
  s =>
    switch s.data {
    | Scons(_, d) =>
      s.count = succ(s.count)
      s.data = d
    | Sgen({curr: Some(_)} as g) =>
      s.count = succ(s.count)
      g.curr = None
    | _ =>
      switch peek_data(s) {
      | None => ()
      | Some(_) => junk_data(s)
      }
    }

let junk = param =>
  switch param {
  | None => ()
  | Some(data) => junk_data(data)
  }

let rec nget_data = (n, s) =>
  if n <= 0 {
    (list{}, s.data, 0)
  } else {
    switch peek_data(s) {
    | Some(a) =>
      junk_data(s)
      let (al, d, k) = nget_data(pred(n), s)
      (list{a, ...al}, Scons(a, d), succ(k))
    | None => (list{}, s.data, 0)
    }
  }

let npeek_data = (n, s) => {
  let (al, d, len) = nget_data(n, s)
  s.count = s.count - len
  s.data = d
  al
}

let npeek = (n, param) =>
  switch param {
  | None => list{}
  | Some(d) => npeek_data(n, d)
  }

let next = s =>
  switch peek(s) {
  | Some(a) =>
    junk(s)
    a
  | None => raise(Failure)
  }

let empty = s =>
  switch peek(s) {
  | Some(_) => raise(Failure)
  | None => ()
  }

let iter = (f, strm) => {
  let rec do_rec = () =>
    switch peek(strm) {
    | Some(a) =>
      junk(strm)
      ignore(f(a))
      do_rec()
    | None => ()
    }

  do_rec()
}

/* Stream building functions */

let from = f => Some({count: 0, data: Sgen({curr: None, func: f})})

let of_list = l => Some({count: 0, data: List.fold_right((x, l) => Scons(x, l), l, Sempty)})

let of_string = s => {
  let count = ref(0)
  from(_ => {
    /* We cannot use the index passed by the [from] function directly
       because it returns the current stream count, with absolutely no
       guarantee that it will start from 0. For example, in the case
       of [Stream.icons 'c' (Stream.from_string "ab")], the first
       access to the string will be made with count [1] already.
 */
    let c = count.contents
    if c < String.length(s) {
      incr(count)
      Some(String.get(s, c))
    } else {
      None
    }
  })
}

let of_bytes = s => {
  let count = ref(0)
  from(_ => {
    let c = count.contents
    if c < Bytes.length(s) {
      incr(count)
      Some(Bytes.get(s, c))
    } else {
      None
    }
  })
}

/* Stream expressions builders */

let iapp = (i, s) => Some({count: 0, data: Sapp(data(i), data(s))})
let icons = (i, s) => Some({count: 0, data: Scons(i, data(s))})
let ising = i => Some({count: 0, data: Scons(i, Sempty)})

let lapp = (f, s) => Some({count: 0, data: Slazy(lazy Sapp(data(f()), data(s)))})

let lcons = (f, s) => Some({count: 0, data: Slazy(lazy Scons(f(), data(s)))})
let lsing = f => Some({count: 0, data: Slazy(lazy Scons(f(), Sempty))})

let sempty = None
let slazy = f => Some({count: 0, data: Slazy(lazy data(f()))})

/* For debugging use */

let rec dump:
  type v. (v => unit, t<v>) => unit =
  (f, s) => {
    print_string("{count = ")
    print_int(count(s))
    print_string("; data = ")
    dump_data(f, data(s))
    print_string("}")
    print_newline()
  }
and dump_data:
  type v. (v => unit, data<v>) => unit =
  (f, param) =>
    switch param {
    | Sempty => print_string("Sempty")
    | Scons(a, d) =>
      print_string("Scons (")
      f(a)
      print_string(", ")
      dump_data(f, d)
      print_string(")")
    | Sapp(d1, d2) =>
      print_string("Sapp (")
      dump_data(f, d1)
      print_string(", ")
      dump_data(f, d2)
      print_string(")")
    | Slazy(_) => print_string("Slazy")
    | Sgen(_) => print_string("Sgen")
    }
