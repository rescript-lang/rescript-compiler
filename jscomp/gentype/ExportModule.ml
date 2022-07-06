open GenTypeCommon

type exportModuleItem = (string, exportModuleValue) Hashtbl.t

and exportModuleValue =
  | S of string * type_ * Converter.t
  | M of exportModuleItem

type exportModuleItems = (string, exportModuleItem) Hashtbl.t
type types = {typeForValue : type_; typeForType : type_; needsConversion : bool}

type fieldInfo = {
  fieldForValue : field;
  fieldForType : field;
  needsConversion : bool;
}

let rec exportModuleValueToType ~config exportModuleValue =
  match exportModuleValue with
  | S (s, type_, converter) ->
    {
      typeForValue = ident s;
      typeForType = type_;
      needsConversion =
        not (converter |> Converter.converterIsIdentity ~config ~toJS:true);
    }
  | M exportModuleItem ->
    let fieldsInfo = exportModuleItem |> exportModuleItemToFields ~config in
    let fieldsForValue =
      fieldsInfo |> List.map (fun {fieldForValue} -> fieldForValue)
    in
    let fieldsForType =
      fieldsInfo |> List.map (fun {fieldForType} -> fieldForType)
    in
    let needsConversion =
      fieldsInfo
      |> List.fold_left
           (fun acc {needsConversion} -> acc || needsConversion)
           false
    in
    {
      typeForValue = Object (Open, fieldsForValue);
      typeForType = Object (Open, fieldsForType);
      needsConversion;
    }

and exportModuleItemToFields =
  (fun ~config exportModuleItem ->
     Hashtbl.fold
       (fun fieldName exportModuleValue fields ->
         let {typeForValue; typeForType; needsConversion} =
           exportModuleValue |> exportModuleValueToType ~config
         in
         let fieldForType =
           {
             mutable_ = Mutable;
             nameJS = fieldName;
             nameRE = fieldName;
             optional = Mandatory;
             type_ = typeForType;
           }
         in
         let fieldForValue = {fieldForType with type_ = typeForValue} in
         {fieldForValue; fieldForType; needsConversion} :: fields)
       exportModuleItem []
    : config:config -> exportModuleItem -> fieldInfo list)

let rec extendExportModuleItem x ~converter
    ~(exportModuleItem : exportModuleItem) ~type_ ~valueName =
  match x with
  | [] -> ()
  | [fieldName] ->
    Hashtbl.replace exportModuleItem fieldName (S (valueName, type_, converter))
  | fieldName :: rest ->
    let innerExportModuleItem =
      match Hashtbl.find exportModuleItem fieldName with
      | M innerExportModuleItem -> innerExportModuleItem
      | S _ -> assert false
      | exception Not_found ->
        let innerExportModuleItem = Hashtbl.create 1 in
        Hashtbl.replace exportModuleItem fieldName (M innerExportModuleItem);
        innerExportModuleItem
    in
    rest
    |> extendExportModuleItem ~converter ~exportModuleItem:innerExportModuleItem
         ~valueName ~type_

let extendExportModuleItems x ~converter
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
    |> extendExportModuleItem ~converter ~exportModuleItem ~type_ ~valueName

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
         let {typeForType; needsConversion} =
           M exportModuleItem |> exportModuleValueToType ~config
         in
         if !Debug.codeItems then
           Log_.item "EmitModule %s needsConversion:%b@." moduleName
             needsConversion;
         if needsConversion then emitters
         else
           let emittedModuleItem =
             ModuleName.forInnerModule ~fileName ~innerModuleName:moduleName
             |> ModuleName.toString
           in
           emittedModuleItem
           |> EmitType.emitExportConst ~early:false ~config ~emitters
                ~name:moduleName ~type_:typeForType
                ~typeNameIsInterface:(fun _ -> false))
       exportModuleItems

let extendExportModules ~converter ~(moduleItemsEmitter : exportModuleItems)
    ~type_ resolvedName =
  resolvedName |> ResolvedName.toList
  |> extendExportModuleItems ~converter ~exportModuleItems:moduleItemsEmitter
       ~type_
       ~valueName:(resolvedName |> ResolvedName.toString)
