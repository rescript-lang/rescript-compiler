(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(**
  Provide bindings to JS regular expressions (RegExp).

  **Note:** This is not an immutable API. A RegExp object with the `global` ("g")
  flag set will modify the [`lastIndex`]() property when the RegExp object is used,
  and subsequent uses will continue the search from the previous [`lastIndex`]().
*)

(** The RegExp object. *)
type t

(** The result of a executing a RegExp on a string. *)
type result

(**
  An `array` of the match and captures, the first is the full match and the
  remaining are the substring captures.
*)
external captures : result -> string Js.nullable array = "%identity"

(**
  Deprecated. Use `captures` instead.
*)
external matches : result -> string array = "%identity"
[@@deprecated "Use Js.Re.captures instead"]

(** 0-based index of the match in the input string. *)
external index : result -> int = "index" [@@bs.get]

(** The original input string. *)
external input : result -> string = "input" [@@bs.get]


(**
  Constructs a RegExp object (Js.Re.t) from a `string`.
  Regex literals `%re("/.../")` should generally be preferred, but `fromString`
  is useful when you need to dynamically construct a regex using strings,
  exactly like when you do so in JavaScript.

  ```res example
  let firstReScriptFileExtension = (filename, content) => {
    let result = Js.Re.fromString(filename ++ "\.(res|resi)")->Js.Re.exec_(content)
    switch result {
    | Some(r) => Js.Nullable.toOption(Js.Re.captures(r)[1])
    | None => None
    }
  }

  // outputs "res"
  firstReScriptFileExtension("School", "School.res School.resi Main.js School.bs.js")
  ```
*)
external fromString : string -> t = "RegExp" [@@bs.new]

(**
  Constructs a RegExp object (`Js.Re.t`) from a string with the given flags.
  See `Js.Re.fromString`.

  Valid flags:

  - **g** global
  - **i** ignore case
  - **m** multiline
  - **u** unicode (es2015)
  - **y** sticky (es2015)
*)
external fromStringWithFlags : string -> flags:string -> t = "RegExp" [@@bs.new]

(** Returns the enabled flags as a string. *)
external flags : t -> string = "flags" [@@bs.get]

(** Returns a `bool` indicating whether the global flag is set. *)
external global : t -> bool = "global" [@@bs.get]

(** Returns a `bool` indicating whether the ignoreCase flag is set. *)
external ignoreCase : t -> bool = "ignoreCase" [@@bs.get]

(**
  Returns the index where the next match will start its search. This property
  will be modified when the RegExp object is used, if the global ("g") flag is
  set.

  ```res example
  let re = %re("/ab*/g")
  let str = "abbcdefabh"

  let break = ref(false)
  while !break.contents {
    switch Js.Re.exec_(re, str) {
    | Some(result) => Js.Nullable.iter(Js.Re.captures(result)[0], (. match_) => {
        let next = Belt.Int.toString(Js.Re.lastIndex(re))
        Js.log("Found " ++ (match_ ++ (". Next match starts at " ++ next)))
      })
    | None => break := true
    }
  }
  ```

  See
  [`RegExp: lastIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/lastIndex)
  on MDN.
*)
external lastIndex : t -> int = "lastIndex" [@@bs.get]

(** Sets the index at which the next match will start its search from. *)
external setLastIndex : t -> int -> unit = "lastIndex" [@@bs.set]

(** Returns a `bool` indicating whether the multiline flag is set. *)
external multiline : t -> bool = "multiline" [@@bs.get]

(** Returns the pattern as a `string`. *)
external source : t -> string = "source" [@@bs.get]

(** Returns a `bool` indicating whether the sticky flag is set. *)
external sticky : t -> bool = "sticky" [@@bs.get]

(** Returns a `bool` indicating whether the unicode flag is set. *)
external unicode : t -> bool = "unicode" [@@bs.get]

(**
  Executes a search on a given string using the given RegExp object.
  Returns `Some(Js.Re.result)` if a match is found, `None` otherwise.

  ```res example
  /* Match "quick brown" followed by "jumps", ignoring characters in between
   * Remember "brown" and "jumps"
   * Ignore case
   */

  let re = %re("/quick\s(brown).+?(jumps)/ig")
  let result = Js.Re.exec_(re, "The Quick Brown Fox Jumps Over The Lazy Dog")
  ```

  See
  [`RegExp.prototype.exec()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/exec)
  on MDN.
*)
external exec_ : t -> string -> result option = "exec" [@@bs.send] [@@bs.return null_to_opt]


(**
  Tests whether the given RegExp object will match a given `string`.
  Returns true if a match is found, false otherwise.

  ```res example
  /* A simple implementation of Js.String.startsWith */

  let str = "hello world!"

  let startsWith = (target, substring) =>
    Js.Re.fromString("^" ++ substring)->Js.Re.test_(target)

  Js.log(str->startsWith("hello")) /* prints "true" */
  ```

  See
  [`RegExp.prototype.test()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/test)
  on MDN.
*)
external test_ : t -> string -> bool = "test" [@@bs.send]
