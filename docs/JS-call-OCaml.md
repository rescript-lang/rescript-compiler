# JS Calling OCaml

Since BuckleScript guarantees that all OCaml functions are exported as is, no extra work is required to expose OCaml function to JavaScript.

Some things need be taken care of:

* `external` exports are not exported as js functions, if you really
   want to export those external functions, please write `val` instead. 

* `operators` are escaped, since Javascript does not support user
   defined operators. For example instead of calling `Pervasives.(^)`,
   you have to call `Pervasives.$caret` from your Javascript functions
   (TODO: document the conversion rules).

