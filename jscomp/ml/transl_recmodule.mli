
val eval_rec_bindings :
  ((Ident.t * (Lambda.lambda * Lambda.lambda) option * Lambda.lambda) list ->
  Lambda.lambda ->
  Lambda.lambda)
  ref

val compile_recmodule :
  (Ident.t -> Typedtree.module_expr -> Location.t -> Lambda.lambda) ->
  Typedtree.module_binding list ->
  Lambda.lambda ->
  Lambda.lambda
