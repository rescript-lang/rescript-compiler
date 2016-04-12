# JS Call OCaml

Since ocamlscript guarantees all ocaml functions are exported as it
is, there is not too much work needed.

Some things need be taken care of:

* `external` exports are not exported as js functions, if you really
   want to export those external functions, please write `val` instead. 

* `operators` are escaped, since Javascript does not support user
   defined operators, for example instead of calling `Pervasives.(^)`,
   you have to call `Pervasives.$caret` from your Javascript functions
   (TODO: document the conversion rules)

