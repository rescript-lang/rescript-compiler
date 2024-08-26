/* Copyright (C) 2015- Hongbo Zhang, Authors of ReScript
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

exception Error = JsError

let internalAnyToExn = (any: 'a): exn =>
  if Obj.magic(any) && Js.typeof(Obj.magic(any)["RE_EXN_ID"]) === "string" {
    any->Obj.magic
  } else {
    {
      "RE_EXN_ID": "JsError",
      "_1": any,
    }->Obj.magic
  }

%%raw(`class RescriptError extends Error {
  constructor(message) {
    super(message);
    this.RE_EXN_ID = message;
  }
}`)

@new
external internalMakeExn: string => exn = "RescriptError"
// Reassign it here from external to let, since RescriptError is not exported
let internalMakeExn = internalMakeExn

let internalFromExtension = (_ext: 'a): exn => {
  %raw(`Object.assign(new RescriptError(_ext.RE_EXN_ID), _ext)`)
}

let as_js_exn = exn =>
  switch exn {
  | Error(t) => Some(t)
  | _ => None
  }
