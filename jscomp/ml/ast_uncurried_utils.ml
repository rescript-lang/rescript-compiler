let typeIsUncurriedFun (typ : Types.type_expr) =
  match typ.desc with
  | Tconstr (Pident {name = "function$"}, [{desc = Tarrow _}; _], _) ->
    true
  | _ -> false