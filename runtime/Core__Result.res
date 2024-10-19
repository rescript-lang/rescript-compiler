/* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

let getExn = x =>
  switch x {
  | Ok(x) => x
  | Error(_) => raise(Not_found)
  }

let mapOr = (opt, default, f) =>
  switch opt {
  | Ok(x) => f(x)
  | Error(_) => default
  }

let mapWithDefault = mapOr

let map = (opt, f) =>
  switch opt {
  | Ok(x) => Ok(f(x))
  | Error(_) as result => result
  }

let flatMap = (opt, f) =>
  switch opt {
  | Ok(x) => f(x)
  | Error(_) as result => result
  }

let getOr = (opt, default) =>
  switch opt {
  | Ok(x) => x
  | Error(_) => default
  }

let getWithDefault = getOr

let isOk = x =>
  switch x {
  | Ok(_) => true
  | Error(_) => false
  }

let isError = x =>
  switch x {
  | Ok(_) => false
  | Error(_) => true
  }

let equal = (a, b, f) =>
  switch (a, b) {
  | (Ok(a), Ok(b)) => f(a, b)
  | (Error(_), Ok(_))
  | (Ok(_), Error(_)) => false
  | (Error(_), Error(_)) => true
  }

let compare = (a, b, f) =>
  switch (a, b) {
  | (Ok(a), Ok(b)) => f(a, b)
  | (Error(_), Ok(_)) => Core__Ordering.less
  | (Ok(_), Error(_)) => Core__Ordering.greater
  | (Error(_), Error(_)) => Core__Ordering.equal
  }

let forEach = (r, f) =>
  switch r {
  | Ok(ok) => f(ok)
  | Error(_) => ()
  }

let mapError = (r, f) =>
  switch r {
  | Ok(_) as result => result
  | Error(e) => Error(f(e))
  }
