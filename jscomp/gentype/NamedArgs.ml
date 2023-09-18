open GenTypeCommon

let group labeledTypes =
  let types =
    Ext_list.map labeledTypes (fun (lbl, type_) ->
        let aName =
          match lbl with
          | Nolabel -> ""
          | Label lbl | OptLabel lbl -> lbl
        in

        {aName; aType = type_})
  in
  match types with
  | [{aType}] when aType = unitT ->
    [] (* treat a single argument of type unit as no argument *)
  | _ -> types
