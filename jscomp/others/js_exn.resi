/*** Provide utilities for dealing with JS exceptions. */

/** Represents a JS exception */
type t

type exn += private Error(t)

external asJsExn: exn => option<t> = "?as_js_exn"

@get external stack: t => option<string> = "stack"
@get external message: t => option<string> = "message"
@get external name: t => option<string> = "name"
@get external fileName: t => option<string> = "fileName"

/** internal use only */
external isCamlExceptionOrOpenVariant: 'a => bool = "?is_extension"

/**
  `anyToExnInternal obj` will take any value `obj` and wrap it
  in a Js.Exn.Error if given value is not an exn already. If
  `obj` is an exn, it will return `obj` without any changes.

  This function is mostly useful for cases where you want to unify a type of a value
  that potentially is either exn, a JS error, or any other JS value really (e.g. for
  a value passed to a Promise.catch callback)

  **IMPORTANT**: This is an internal API and may be changed / removed any time in the future.

  ```
  switch (Js.Exn.unsafeAnyToExn(\"test\")) {
  | Js.Exn.Error(v) =>
    switch(Js.Exn.message(v)) {
    | Some(str) => Js.log(\"We won't end up here\")
    | None => Js.log2(\"We will land here: \", v)
    }
  }
  ```
*/
external anyToExnInternal: 'a => exn = "#wrap_exn"

/** Raise Js exception Error object with stacktrace */
let raiseError: string => 'a
let raiseEvalError: string => 'a
let raiseRangeError: string => 'a
let raiseReferenceError: string => 'a
let raiseSyntaxError: string => 'a
let raiseTypeError: string => 'a
let raiseUriError: string => 'a
