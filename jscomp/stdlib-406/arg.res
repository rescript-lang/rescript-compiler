/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Damien Doligez, projet Para, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

type key = string
type doc = string
type usage_msg = string
type anon_fun = string => unit

type rec spec =
  | Unit(unit => unit) /* Call the function with unit argument */
  | Bool(bool => unit) /* Call the function with a bool argument */
  | Set(ref<bool>) /* Set the reference to true */
  | Clear(ref<bool>) /* Set the reference to false */
  | String(string => unit) /* Call the function with a string argument */
  | Set_string(ref<string>) /* Set the reference to the string argument */
  | Int(int => unit) /* Call the function with an int argument */
  | Set_int(ref<int>) /* Set the reference to the int argument */
  | Float(float => unit) /* Call the function with a float argument */
  | Set_float(ref<float>) /* Set the reference to the float argument */
  | Tuple(list<spec>) /* Take several arguments according to the
   spec list */

  | Symbol(list<string>, string => unit)
  /* Take one of the symbols as argument and
   call the function with the symbol. */
  | Rest(string => unit) /* Stop interpreting keywords and call the
   function with each remaining argument */

  | Expand(
      string => array<string>,
    ) /* If the remaining arguments to process
        are of the form
        [["-foo"; "arg"] @ rest] where "foo" is
        registered as [Expand f], then the
        arguments [f "arg" @ rest] are
        processed. Only allowed in
        [parse_and_expand_argv_dynamic]. */

exception Bad(string)
exception Help(string)

type error =
  | Unknown(string)
  | Wrong(string, string, string) /* option, actual, expected */
  | Missing(string)
  | Message(string)

exception Stop(error) /* used internally */

let rec assoc3 = (x, l) =>
  switch l {
  | list{} => raise(Not_found)
  | list{(y1, y2, _), ..._} if y1 == x => y2
  | list{_, ...t} => assoc3(x, t)
  }

let split = s => {
  let i = String.index(s, '=')
  let len = String.length(s)
  (String.sub(s, 0, i), String.sub(s, i + 1, len - (i + 1)))
}

let make_symlist = (prefix, sep, suffix, l) =>
  switch l {
  | list{} => "<none>"
  | list{h, ...t} => List.fold_left((x, y) => x ++ (sep ++ y), prefix ++ h, t) ++ suffix
  }

let print_spec = (buf, (key, spec, doc)) =>
  if String.length(doc) > 0 {
    switch spec {
    | Symbol(l, _) =>
      let sym = make_symlist("{", "|", "}", l)
      Buffer.add_string(buf, `  ${key} ${sym}${doc}\n`)
    | _ => Buffer.add_string(buf, `  ${key} ${doc}\n`)
    }
  }

let help_action = () => raise(Stop(Unknown("-help")))

let add_help = speclist => {
  let add1 = try {
    ignore(assoc3("-help", speclist))
    list{}
  } catch {
  | Not_found => list{("-help", Unit(help_action), " Display this list of options")}
  }
  and add2 = try {
    ignore(assoc3("--help", speclist))
    list{}
  } catch {
  | Not_found => list{("--help", Unit(help_action), " Display this list of options")}
  }

  \"@"(speclist, \"@"(add1, add2))
}

let usage_b = (buf, speclist, errmsg) => {
  Buffer.add_string(buf, `${errmsg}\n`)
  List.iter(print_spec(buf), add_help(speclist))
}

let usage_string = (speclist, errmsg) => {
  let b = Buffer.create(200)
  usage_b(b, speclist, errmsg)
  Buffer.contents(b)
}

let usage = (speclist, errmsg) => Js.log(usage_string(speclist, errmsg))

let current = ref(0)

let bool_of_string_opt = x =>
  try Some(bool_of_string(x)) catch {
  | Invalid_argument(_) => None
  }

let int_of_string_opt = x =>
  try Some(int_of_string(x)) catch {
  | Failure(_) => None
  }

let float_of_string_opt = x =>
  try Some(float_of_string(x)) catch {
  | Failure(_) => None
  }

let parse_and_expand_argv_dynamic_aux = (
  allow_expand,
  current,
  argv,
  speclist,
  anonfun,
  errmsg,
) => {
  let initpos = current.contents
  let convert_error = error => {
    /* convert an internal error to a Bad/Help exception
       *or* add the program name as a prefix and the usage message as a suffix
       to an user-raised Bad exception.
 */
    let b = Buffer.create(200)
    let progname = if initpos < Array.length(argv.contents) {
      argv.contents[initpos]
    } else {
      "(?)"
    }
    switch error {
    | Unknown("-help") => ()
    | Unknown("--help") => ()
    | Unknown(s) => Buffer.add_string(b, `${progname}: unknown option '${s}'.\n`)
    | Missing(s) => Buffer.add_string(b, `${progname}: option '${s}' needs an argument.\n`)
    | Wrong(opt, arg, expected) =>
      Buffer.add_string(
        b,
        `${progname}: wrong argument '${arg}'; option '${opt}' expects ${expected}.\n`,
      )
    | Message(s) =>
      /* user error message */
      Buffer.add_string(b, `${progname}: ${s}.\n`)
    }
    usage_b(b, speclist.contents, errmsg)
    if error == Unknown("-help") || error == Unknown("--help") {
      Help(Buffer.contents(b))
    } else {
      Bad(Buffer.contents(b))
    }
  }

  incr(current)
  while current.contents < Array.length(argv.contents) {
    try {
      let s = argv.contents[current.contents]
      if String.length(s) >= 1 && String.get(s, 0) == '-' {
        let (action, follow) = try (assoc3(s, speclist.contents), None) catch {
        | Not_found =>
          try {
            let (keyword, arg) = split(s)
            (assoc3(keyword, speclist.contents), Some(arg))
          } catch {
          | Not_found => raise(Stop(Unknown(s)))
          }
        }

        let no_arg = () =>
          switch follow {
          | None => ()
          | Some(arg) => raise(Stop(Wrong(s, arg, "no argument")))
          }
        let get_arg = () =>
          switch follow {
          | None =>
            if current.contents + 1 < Array.length(argv.contents) {
              argv.contents[current.contents + 1]
            } else {
              raise(Stop(Missing(s)))
            }
          | Some(arg) => arg
          }

        let consume_arg = () =>
          switch follow {
          | None => incr(current)
          | Some(_) => ()
          }

        let rec treat_action = f =>
          switch f {
          | Unit(f) => f()
          | Bool(f) =>
            let arg = get_arg()
            switch bool_of_string_opt(arg) {
            | None => raise(Stop(Wrong(s, arg, "a boolean")))
            | Some(s) => f(s)
            }
            consume_arg()
          | Set(r) =>
            no_arg()
            r := true
          | Clear(r) =>
            no_arg()
            r := false
          | String(f) =>
            let arg = get_arg()
            f(arg)
            consume_arg()
          | Symbol(symb, f) =>
            let arg = get_arg()
            if List.mem(arg, symb) {
              f(arg)
              consume_arg()
            } else {
              raise(Stop(Wrong(s, arg, "one of: " ++ make_symlist("", " ", "", symb))))
            }
          | Set_string(r) =>
            r := get_arg()
            consume_arg()
          | Int(f) =>
            let arg = get_arg()
            switch int_of_string_opt(arg) {
            | None => raise(Stop(Wrong(s, arg, "an integer")))
            | Some(x) => f(x)
            }
            consume_arg()
          | Set_int(r) =>
            let arg = get_arg()
            switch int_of_string_opt(arg) {
            | None => raise(Stop(Wrong(s, arg, "an integer")))
            | Some(x) => r := x
            }
            consume_arg()
          | Float(f) =>
            let arg = get_arg()
            switch float_of_string_opt(arg) {
            | None => raise(Stop(Wrong(s, arg, "a float")))
            | Some(x) => f(x)
            }
            consume_arg()
          | Set_float(r) =>
            let arg = get_arg()
            switch float_of_string_opt(arg) {
            | None => raise(Stop(Wrong(s, arg, "a float")))
            | Some(x) => r := x
            }
            consume_arg()
          | Tuple(specs) => List.iter(treat_action, specs)
          | Rest(f) =>
            while current.contents < Array.length(argv.contents) - 1 {
              f(argv.contents[current.contents + 1])
              consume_arg()
            }
          | Expand(f) =>
            if !allow_expand {
              raise(
                Invalid_argument(
                  "Arg.Expand is is only allowed with Arg.parse_and_expand_argv_dynamic",
                ),
              )
            }
            let arg = get_arg()
            let newarg = f(arg)
            consume_arg()
            let before = Array.sub(argv.contents, 0, current.contents + 1)
            and after = Array.sub(
              argv.contents,
              current.contents + 1,
              Array.length(argv.contents) - current.contents - 1,
            )
            argv := Array.concat(list{before, newarg, after})
          }

        treat_action(action)
      } else {
        anonfun(s)
      }
    } catch {
    | Bad(m) => raise(convert_error(Message(m)))
    | Stop(e) => raise(convert_error(e))
    }
    incr(current)
  }
}

let parse_and_expand_argv_dynamic = (current, argv, speclist, anonfun, errmsg) =>
  parse_and_expand_argv_dynamic_aux(true, current, argv, speclist, anonfun, errmsg)

let parse_argv_dynamic = (~current=current, argv, speclist, anonfun, errmsg) =>
  parse_and_expand_argv_dynamic_aux(false, current, ref(argv), speclist, anonfun, errmsg)

let parse_argv = (~current=current, argv, speclist, anonfun, errmsg) =>
  parse_argv_dynamic(~current, argv, ref(speclist), anonfun, errmsg)

let parse = (l, f, msg) =>
  try parse_argv(Sys.argv, l, f, msg) catch {
  | Bad(msg) =>
    Js.log(msg)
    exit(2)
  | Help(msg) =>
    Js.log(msg)
    exit(0)
  }

let parse_dynamic = (l, f, msg) =>
  try parse_argv_dynamic(Sys.argv, l, f, msg) catch {
  | Bad(msg) =>
    Js.log(msg)
    exit(2)
  | Help(msg) =>
    Js.log(msg)
    exit(0)
  }

let parse_expand = (l, f, msg) =>
  try {
    let argv = ref(Sys.argv)
    let spec = ref(l)
    let current = ref(current.contents)
    parse_and_expand_argv_dynamic(current, argv, spec, f, msg)
  } catch {
  | Bad(msg) =>
    Js.log(msg)
    exit(2)
  | Help(msg) =>
    Js.log(msg)
    exit(0)
  }

let second_word = s => {
  let len = String.length(s)
  let rec loop = n =>
    if n >= len {
      len
    } else if String.get(s, n) == ' ' {
      loop(n + 1)
    } else {
      n
    }

  switch String.index(s, '\t') {
  | n => loop(n + 1)
  | exception Not_found =>
    switch String.index(s, ' ') {
    | n => loop(n + 1)
    | exception Not_found => len
    }
  }
}

let max_arg_len = (cur, (kwd, spec, doc)) =>
  switch spec {
  | Symbol(_) => max(cur, String.length(kwd))
  | _ => max(cur, String.length(kwd) + second_word(doc))
  }

let replace_leading_tab = s => {
  let seen = ref(false)
  String.map(c =>
    switch c {
    | '\t' if !seen.contents =>
      seen := true
      ' '
    | c => c
    }
  , s)
}

let add_padding = (len, ksd) =>
  switch ksd {
  | (_, _, "") => /* Do not pad undocumented options, so that they still don't show up when
     * run through [usage] or [parse]. */
    ksd
  | (kwd, Symbol(_) as spec, msg) =>
    let cutcol = second_word(msg)
    let spaces = String.make(max(0, len - cutcol) + 3, ' ')
    (kwd, spec, "\n" ++ (spaces ++ replace_leading_tab(msg)))
  | (kwd, spec, msg) =>
    let cutcol = second_word(msg)
    let kwd_len = String.length(kwd)
    let diff = len - kwd_len - cutcol
    if diff <= 0 {
      (kwd, spec, replace_leading_tab(msg))
    } else {
      let spaces = String.make(diff, ' ')
      let prefix = String.sub(replace_leading_tab(msg), 0, cutcol)
      let suffix = String.sub(msg, cutcol, String.length(msg) - cutcol)
      (kwd, spec, prefix ++ (spaces ++ suffix))
    }
  }

let align = (~limit=max_int, speclist) => {
  let completed = add_help(speclist)
  let len = List.fold_left(max_arg_len, 0, completed)
  let len = min(len, limit)
  List.map(add_padding(len), completed)
}
