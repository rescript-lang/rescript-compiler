/*** Placeholder for Node bindings */

@@warning("-49")

module Path = Node_path

module Fs = Node_fs

module Process = Node_process

module Module = Node_module
module Buffer = Node_buffer
module Child_process = Node_child_process

type node_exports
type rec node_module = {
  "id": string,
  "exports": node_exports,
  "parrent": Js.null_undefined<node_module>,
  /* in REPL V4 it is `undefined`
          in CLI it can be `null`
 */

  "filename": string,
  "loaded": bool,
  "children": array<node_module>,
  "paths": array<string>,
}
/* WARN:
  its path name should not be changed
  see [`Ppx_entry`]()
*/

type node_require = {
  "main": Js.undefined<node_module>,
  "resolve": string => string,
  /* **raise** exception */
}
/* WARN:
  its path name should not be changed
  see [`Ppx_entry`]()
*/

type string_buffer /* can be either string or buffer */

type buffer

type rec string_buffer_kind<_> =
  | String: string_buffer_kind<string>
  | Buffer: string_buffer_kind<buffer>

/** We expect a good inliner will eliminate such boxing in the future */
let test = (type t, x: string_buffer): (string_buffer_kind<t>, t) =>
  if Js.typeof(x) == "string" {
    ((Obj.magic(String): string_buffer_kind<t>), (Obj.magic(x): t))
  } else {
    ((Obj.magic(Buffer): string_buffer_kind<t>), (Obj.magic(x): t))
  }
