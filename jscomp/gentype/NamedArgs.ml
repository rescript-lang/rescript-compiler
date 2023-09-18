open GenTypeCommon

let group labeledTypes =
  let types =
    Ext_list.map labeledTypes (fun (lbl, aType) ->
        match lbl with
        | Nolabel -> {aName = ""; aType}
        | Label lbl -> {aName = lbl; aType}
        | OptLabel lbl -> {aName = lbl; aType = Option aType})
  in
  match types with
  | [{aType}] when aType = unitT ->
    [] (* treat a single argument of type unit as no argument *)
  | _ -> types
