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

type (+'a, +'e) rejectable
type never

type +'a t = ('a, never) rejectable
type +'a promise = 'a t



let onUnhandledException = ref (fun exn ->
  prerr_endline "Unhandled exception in promise callback:";
  prerr_endline (Printexc.to_string exn);
  exit 2)



[%%bs.raw {|
function WrappedRepromise(p) {
    this.wrapped = p;
};

function unwrap(value) {
    if (value instanceof WrappedRepromise)
        return value.wrapped;
    else
        return value;
}

function wrap(value) {
    if (value != null && typeof value.then === 'function')
        return new WrappedRepromise(value);
    else
        return value;
}

function new_(executor) {
    return new Promise(function (resolve, reject) {
        var wrappingResolve = function(value) {
            resolve(wrap(value));
        };
        executor(wrappingResolve, reject);
    });
};

function resolve(value) {
    return Promise.resolve(wrap(value));
};

function then(callback, promise) {
    var safeCallback = function (value) {
        try {
            return callback(value);
        }
        catch (exception) {
            onUnhandledException[0](exception);
        }
    };

    return promise.then(function (value) {
        return safeCallback(unwrap(value));
    });
};

function catch_(callback, promise) {
    var safeCallback = function (error) {
        try {
            return callback(error);
        }
        catch (exception) {
            onUnhandledException[0](exception);
        }
    };

    return promise.catch(safeCallback);
}
|}]



module Rejectable =
struct
  type (+'a, +'e) t = ('a, 'e) rejectable

  external relax : 'a promise -> ('a, _) rejectable = "%identity"

  external jsNew :
    (('a -> unit) -> ('e -> unit) -> unit) -> ('a, 'e) rejectable = "new_"
    [@@bs.val]

  let new_ () =
    let resolve = ref ignore in
    let reject = ref ignore in
    let p =
      jsNew (fun resolve' reject' ->
        resolve := resolve';
        reject := reject')
    in
    (p, !resolve, !reject)

  external resolve : 'a -> ('a, _) rejectable = ""
    [@@bs.val]

  external then_ :
    ('a -> ('b, 'e) rejectable) -> ('a, 'e) rejectable -> ('b, 'e) rejectable =
      "then"
    [@@bs.val]

  let map callback promise =
    promise |> then_(fun value -> resolve (callback value))

  external reject : 'e -> (_, 'e) rejectable = ""
    [@@bs.val]
    [@@bs.scope "Promise"]

  external catch :
    ('e -> ('a, 'e2) rejectable) -> ('a, 'e) rejectable ->
      ('a, 'e2) rejectable =
      "catch_"
    [@@bs.val]

  external unwrap : 'a -> 'a = ""
    [@@bs.val]

  external jsAll :
    ('a, 'e) rejectable array -> ('a array, 'e) rejectable = "all"
    [@@bs.val]
    [@@bs.scope "Promise"]

  let all promises =
    promises
    |> jsAll
    |> map (Array.map unwrap)

  external jsRace : ('a, 'e) rejectable array -> ('a, 'e) rejectable = "race"
    [@@bs.val]
    [@@bs.scope "Promise"]

  let race promises =
    if promises = [||] then
      raise (Invalid_argument "Js.Promise.race([||]) would be pending forever")
    else
      jsRace promises
end


let make executor =
  let (p, resolve, _reject) = Rejectable.new_ () in
  executor ~resolve;
  p

let resolve = Rejectable.resolve
let then_ = Rejectable.then_
let map = Rejectable.map
let all = Rejectable.all
let race = Rejectable.race
