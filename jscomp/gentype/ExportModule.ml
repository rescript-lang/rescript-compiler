open GenTypeCommon

type exportModuleItem = (string, exportModuleValue) Hashtbl.t

and exportModuleValue =
  | S of {name: string; type_: type_; docString: DocString.t}
  | M of {exportModuleItem: exportModuleItem}

type exportModuleItems = (string, exportModuleItem) Hashtbl.t

type types = {typeForValue: type_; typeForType: type_; docString: DocString.t}

type fieldInfo = {fieldForValue: field; fieldForType: field}

let rec exportModuleValueToType ~config exportModuleValue =
  match exportModuleValue with
  | S {name; type_; docString} ->
    {typeForValue = ident name; typeForType = type_; docString}
  | M {exportModuleItem} ->
    let fieldsInfo = exportModuleItem |> exportModuleItemToFields ~config in
    let fieldsForValue =
      fieldsInfo |> List.map (fun {fieldForValue} -> fieldForValue)
    in
    let fieldsForType =
      fieldsInfo |> List.map (fun {fieldForType} -> fieldForType)
    in
    {
      typeForValue = Object (Open, fieldsForValue);
      typeForType = Object (Open, fieldsForType);
      docString = DocString.empty;
    }

and exportModuleItemToFields =
  (fun ~config exportModuleItem ->
     Hashtbl.fold
       (fun fieldName exportModuleValue fields ->
         let {typeForValue; typeForType; docString} =
           exportModuleValue |> exportModuleValueToType ~config
         in
         let fieldForType =
           {
             mutable_ = Mutable;
             nameJS = fieldName;
             optional = Mandatory;
             type_ = typeForType;
             docString;
           }
         in
         let fieldForValue = {fieldForType with type_ = typeForValue} in
         {fieldForValue; fieldForType} :: fields)
       exportModuleItem []
    : config:Config.t -> exportModuleItem -> fieldInfo list)

let rec extendExportModuleItem ~docString x
    ~(exportModuleItem : exportModuleItem) ~type_ ~valueName =
  match x with
  | [] -> ()
  | [fieldName] ->
    Hashtbl.replace exportModuleItem fieldName
      (S {name = valueName; type_; docString})
  | fieldName :: rest ->
    let innerExportModuleItem =
      match Hashtbl.find exportModuleItem fieldName with
      | M {exportModuleItem = innerExportModuleItem} -> innerExportModuleItem
      | S _ -> assert false
      | exception Not_found ->
        let innerExportModuleItem = Hashtbl.create 1 in
        Hashtbl.replace exportModuleItem fieldName
          (M {exportModuleItem = innerExportModuleItem});
        innerExportModuleItem
    in
    rest
    |> extendExportModuleItem ~docString ~exportModuleItem:innerExportModuleItem
         ~valueName ~type_

let extendExportModuleItems x ~docString
    ~(exportModuleItems : exportModuleItems) ~type_ ~valueName =
  match x with
  | [] -> assert false
  | [_valueName] -> ()
  | moduleName :: rest ->
    let exportModuleItem =
      match Hashtbl.find exportModuleItems moduleName with
      | exportModuleItem -> exportModuleItem
      | exception Not_found ->
        let exportModuleItem = Hashtbl.create 1 in
        Hashtbl.replace exportModuleItems moduleName exportModuleItem;
        exportModuleItem
    in
    rest
    |> extendExportModuleItem ~docString ~exportModuleItem ~type_ ~valueName

let createModuleItemsEmitter =
  (fun () -> Hashtbl.create 1 : unit -> exportModuleItems)

let rev_fold f tbl base =
  let list = Hashtbl.fold (fun k v l -> (k, v) :: l) tbl [] in
  List.fold_left (fun x (k, v) -> f k v x) base list

let emitAllModuleItems ~config ~emitters ~fileName
    (exportModuleItems : exportModuleItems) =
  emitters
  |> rev_fold
       (fun moduleName exportModuleItem emitters ->
         let {typeForType; docString} =
           M {exportModuleItem} |> exportModuleValueToType ~config
         in
         if !Debug.codeItems then Log_.item "EmitModule %s @." moduleName;
         let emittedModuleItem =
           ModuleName.forInnerModule ~fileName ~innerModuleName:moduleName
           |> ModuleName.toString
         in
         emittedModuleItem
         |> EmitType.emitExportConst ~docString ~early:false ~config ~emitters
              ~name:moduleName ~type_:typeForType ~typeNameIsInterface:(fun _ ->
                false))
       exportModuleItems

let extendExportModules ~(moduleItemsEmitter : exportModuleItems) ~docString
    ~type_ resolvedName =
  resolvedName |> ResolvedName.toList
  |> extendExportModuleItems ~exportModuleItems:moduleItemsEmitter ~type_
       ~docString
       ~valueName:(resolvedName |> ResolvedName.toString)
