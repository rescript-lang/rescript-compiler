let has_dict_pattern_attribute attrs =
  attrs
  |> List.find_opt (fun (({txt}, _) : Parsetree.attribute) ->
         txt = "res.dictPattern")
  |> Option.is_some

let has_dict_attribute attrs =
  attrs
  |> List.find_opt (fun (({txt}, _) : Parsetree.attribute) -> txt = "res.dict")
  |> Option.is_some