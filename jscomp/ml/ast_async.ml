let is_async : Parsetree.attribute -> bool =
  fun ({txt}, _) -> txt = "async" || txt = "res.async"

let add_promise_type ?(loc = Location.none) ~async
    (result : Parsetree.expression) =
  if async then
    let unsafe_async =
      Ast_helper.Exp.ident ~loc
        {txt = Ldot (Ldot (Lident "Js", "Promise"), "unsafe_async"); loc}
    in
    Ast_helper.Exp.apply ~loc unsafe_async [(Nolabel, result)]
  else result

let add_async_attribute ~async (body : Parsetree.expression) =
  if async then
    {
      body with
      pexp_attributes =
        ({txt = "res.async"; loc = Location.none}, PStr [])
        :: body.pexp_attributes;
    }
  else body

let rec add_promise_to_result ~loc (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_fun (label, eo, pat, body) ->
    let body = add_promise_to_result ~loc body in
    {e with pexp_desc = Pexp_fun (label, eo, pat, body)}
  | _ -> add_promise_type ~loc ~async:true e

let make_function_async ~async (e : Parsetree.expression) =
  if async then
    match e.pexp_desc with
    | Pexp_fun (_, _, {ppat_loc}, _) -> add_promise_to_result ~loc:ppat_loc e
    | _ -> assert false
  else e
