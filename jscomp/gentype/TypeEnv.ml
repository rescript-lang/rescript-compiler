open GenTypeCommon

type moduleEquation = { internal : bool; dep : dep }

type t = {
  mutable map : entry StringMap.t;
  mutable mapModuleTypes : (Typedtree.signature * t) StringMap.t;
  mutable moduleEquation : moduleEquation option;
  mutable moduleItem : Runtime.moduleItem;
  name : string;
  parent : t option;
  typeEquations : type_ StringMap.t;
}

and entry = Module of t | Type of string

let createTypeEnv ~name parent =
  let moduleItem = Runtime.newModuleItem ~name in
  {
    map = StringMap.empty;
    mapModuleTypes = StringMap.empty;
    moduleEquation = None;
    moduleItem;
    name;
    parent;
    typeEquations = StringMap.empty;
  }

let root () = None |> createTypeEnv ~name:"__root__"
let toString typeEnv = typeEnv.name

let newModule ~name typeEnv =
  if !Debug.typeEnv then
    Log_.item "TypeEnv.newModule %s %s\n" (typeEnv |> toString) name;
  let newTypeEnv = Some typeEnv |> createTypeEnv ~name in
  typeEnv.map <- typeEnv.map |> StringMap.add name (Module newTypeEnv);
  newTypeEnv

let newModuleType ~name ~signature typeEnv =
  if !Debug.typeEnv then
    Log_.item "TypeEnv.newModuleType %s %s\n" (typeEnv |> toString) name;
  let newTypeEnv = Some typeEnv |> createTypeEnv ~name in
  typeEnv.mapModuleTypes <-
    typeEnv.mapModuleTypes |> StringMap.add name (signature, newTypeEnv);
  newTypeEnv

let newType ~name typeEnv =
  if !Debug.typeEnv then
    Log_.item "TypeEnv.newType %s %s\n" (typeEnv |> toString) name;
  typeEnv.map <- typeEnv.map |> StringMap.add name (Type name)

let getModule ~name typeEnv =
  match typeEnv.map |> StringMap.find name with
  | Module typeEnv1 -> Some typeEnv1
  | Type _ -> None
  | exception Not_found -> None

let expandAliasToExternalModule ~name typeEnv =
  match typeEnv |> getModule ~name with
  | Some { moduleEquation = Some { internal = false; dep } } ->
      if !Debug.typeEnv then
        Log_.item "TypeEnv.expandAliasToExternalModule %s %s aliased to %s\n"
          (typeEnv |> toString) name (dep |> depToString);
      Some dep
  | _ -> None

let addModuleEquation ~dep ~internal typeEnv =
  if !Debug.typeEnv then
    Log_.item "Typenv.addModuleEquation %s %s dep:%s\n" (typeEnv |> toString)
      (match internal with true -> "Internal" | false -> "External")
      (dep |> depToString);
  typeEnv.moduleEquation <- Some { internal; dep }

let rec addTypeEquation ~flattened ~type_ typeEnv =
  match flattened with
  | [ name ] ->
      {
        typeEnv with
        typeEquations = typeEnv.typeEquations |> StringMap.add name type_;
      }
  | moduleName :: rest -> (
      match typeEnv |> getModule ~name:moduleName with
      | Some typeEnv1 ->
          {
            typeEnv with
            map =
              typeEnv.map
              |> StringMap.add moduleName
                   (Module (typeEnv1 |> addTypeEquation ~flattened:rest ~type_));
          }
      | None -> typeEnv)
  | [] -> typeEnv

let addTypeEquations ~typeEquations typeEnv =
  typeEquations
  |> List.fold_left
       (fun te (longIdent, type_) ->
         te
         |> addTypeEquation ~flattened:(longIdent |> Longident.flatten) ~type_)
       typeEnv

let applyTypeEquations ~config ~path typeEnv =
  match path with
  | Path.Pident id -> (
      match typeEnv.typeEquations |> StringMap.find (id |> Ident.name) with
      | type_ ->
          if !Debug.typeResolution then
            Log_.item "Typenv.applyTypeEquations %s name:%s type_:%s\n"
              (typeEnv |> toString) (id |> Ident.name)
              (type_
              |> EmitType.typeToString ~config ~typeNameIsInterface:(fun _ ->
                     false));
          Some type_
      | exception Not_found -> None)
  | _ -> None

let rec lookup ~name typeEnv =
  match typeEnv.map |> StringMap.find name with
  | _ -> Some typeEnv
  | exception Not_found -> (
      match typeEnv.parent with
      | None -> None
      | Some parent -> parent |> lookup ~name)

let rec lookupModuleType ~path typeEnv =
  match path with
  | [ moduleTypeName ] -> (
      if !Debug.typeEnv then
        Log_.item "Typenv.lookupModuleType %s moduleTypeName:%s\n"
          (typeEnv |> toString) moduleTypeName;
      match typeEnv.mapModuleTypes |> StringMap.find moduleTypeName with
      | x -> Some x
      | exception Not_found -> (
          match typeEnv.parent with
          | None -> None
          | Some parent -> parent |> lookupModuleType ~path))
  | moduleName :: path1 -> (
      if !Debug.typeEnv then
        Log_.item "Typenv.lookupModuleType %s moduleName:%s\n"
          (typeEnv |> toString) moduleName;
      match typeEnv.map |> StringMap.find moduleName with
      | Module typeEnv1 -> typeEnv1 |> lookupModuleType ~path:path1
      | Type _ -> None
      | exception Not_found -> (
          match typeEnv.parent with
          | None -> None
          | Some parent -> parent |> lookupModuleType ~path))
  | [] -> None

let rec pathToList path =
  match path with
  | Path.Pident id -> [ id |> Ident.name ]
  | Path.Pdot (p, s, _) -> s :: (p |> pathToList)
  | Path.Papply _ -> []

let lookupModuleTypeSignature ~path typeEnv =
  if !Debug.typeEnv then
    Log_.item "TypeEnv.lookupModuleTypeSignature %s %s\n" (typeEnv |> toString)
      (path |> Path.name);
  typeEnv |> lookupModuleType ~path:(path |> pathToList |> List.rev)

let updateModuleItem ~moduleItem typeEnv = typeEnv.moduleItem <- moduleItem

let rec addModulePath ~typeEnv name =
  match typeEnv.parent with
  | None -> name |> ResolvedName.fromString
  | Some parent ->
      typeEnv.name |> addModulePath ~typeEnv:parent |> ResolvedName.dot name

let rec getModuleEquations typeEnv : ResolvedName.eq list =
  let subEquations =
    typeEnv.map |> StringMap.bindings
    |> List.map (fun (_, entry) ->
           match entry with
           | Module te -> te |> getModuleEquations
           | Type _ -> [])
    |> List.concat
  in
  match (typeEnv.moduleEquation, typeEnv.parent) with
  | None, _ | _, None -> subEquations
  | Some { dep }, Some parent ->
      [
        (dep |> depToResolvedName, typeEnv.name |> addModulePath ~typeEnv:parent);
      ]

let getModuleAccessPath ~name typeEnv =
  let rec accessPath typeEnv =
    match typeEnv.parent with
    | None -> Runtime.Root name (* not nested *)
    | Some parent ->
        Dot
          ( (match parent.parent = None with
            | true -> Root typeEnv.name
            | false -> parent |> accessPath),
            typeEnv.moduleItem )
  in

  typeEnv |> accessPath
