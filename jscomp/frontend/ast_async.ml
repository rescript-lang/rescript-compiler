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
