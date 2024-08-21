/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

type t = unknown

@@warning("-38") /* unused extension constructor */
exception Error = JsError

external asJsExn: exn => option<t> = "?as_js_exn"

@get external stack: t => option<string> = "stack"
@get external message: t => option<string> = "message"
@get external name: t => option<string> = "name"
@get external fileName: t => option<string> = "fileName"

type error
@new external makeError: string => error = "Error"

external isCamlExceptionOrOpenVariant: 'a => bool = "?is_extension"

external anyToExnInternal: 'a => exn = "#wrap_exn"

let throw: exn => 'a = %raw("function (exn) {
  throw exn;
}")

let raiseError = str => throw((Obj.magic((makeError(str): error)): exn))

type eval_error
@new external makeEvalError: string => eval_error = "EvalError"

let raiseEvalError = str => throw((Obj.magic((makeEvalError(str): eval_error)): exn))

type range_error
@new external makeRangeError: string => range_error = "RangeError"

let raiseRangeError = str => throw((Obj.magic((makeRangeError(str): range_error)): exn))

type reference_error

@new external makeReferenceError: string => reference_error = "ReferenceError"

let raiseReferenceError = str => throw(Obj.magic(makeReferenceError(str)))

type syntax_error
@new external makeSyntaxError: string => syntax_error = "SyntaxError"

let raiseSyntaxError = str => throw(Obj.magic(makeSyntaxError(str)))

type type_error
@new external makeTypeError: string => type_error = "TypeError"

let raiseTypeError = str => throw(Obj.magic(makeTypeError(str)))

type uri_error
@new external makeURIError: string => uri_error = "URIError"

let raiseUriError = str => throw(Obj.magic(makeURIError(str)))

/* TODO add predicate to tell which error is which " */

/*
exception EvalError of error
exception RangeError of error
exception ReferenceError of error
exception SyntaxError of error
exception TypeError of error

 The URIError object represents an error when a global URI handling function was used in a wrong way. 
exception URIError of error    
*/
