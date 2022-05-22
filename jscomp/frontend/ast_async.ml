let add_promise_type ~async (result : Parsetree.expression) =
  if async then
    let txt =
      Longident.Ldot (Longident.Ldot (Lident "Js", "Promise"), "unsafe_async")
    in
    let pexp_desc = Parsetree.Pexp_ident { txt; loc = result.pexp_loc } in
    {
      result with
      pexp_desc = Pexp_apply ({ result with pexp_desc }, [ (Nolabel, result) ]);
    }
  else result

let add_async_attribute ~async (body : Parsetree.expression) =
  if async then
    {
      body with
      pexp_attributes =
        ({ txt = "async"; loc = Location.none }, PStr [])
        :: body.pexp_attributes;
    }
  else body

let rec add_promise_to_result (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_fun (label, eo, pat, body) ->
      let body = add_promise_to_result body in
      { e with pexp_desc = Pexp_fun (label, eo, pat, body) }
  | _ -> add_promise_type ~async:true e

let make_function_async ~async (e : Parsetree.expression) =
  if async then
    match e.pexp_desc with
    | Pexp_fun _ -> add_async_attribute ~async (add_promise_to_result e)
    | _ -> assert false
  else e
