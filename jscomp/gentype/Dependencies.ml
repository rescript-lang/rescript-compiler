open GenTypeCommon

let rec handleNamespace ~name dep =
  match dep with
  | External _ | Internal _ -> dep
  | Dot (External s, moduleName) when s = name -> External moduleName
  | Dot (dep1, s) -> Dot (dep1 |> handleNamespace ~name, s)

let rec fromPath1 ~config ~typeEnv (path : Path.t) =
  match path with
  | Pident id -> (
      let name = id |> Ident.name in
      match typeEnv |> TypeEnv.lookup ~name with
      | None -> External name
      | Some typeEnv1 -> (
          match typeEnv1 |> TypeEnv.expandAliasToExternalModule ~name with
          | Some dep -> dep
          | None ->
              let resolvedName =
                name |> TypeEnv.addModulePath ~typeEnv:typeEnv1
              in
              Internal resolvedName))
  | Pdot (Pident id, s, _pos) when id |> ScopedPackage.isGeneratedModule ~config
    ->
      External (s |> ScopedPackage.addGeneratedModule ~generatedModule:id)
  | Pdot (p, s, _pos) -> Dot (p |> fromPath1 ~config ~typeEnv, s)
  | Papply _ ->
      Internal ("__Papply_unsupported_genType__" |> ResolvedName.fromString)

let rec isInternal dep =
  match dep with
  | External _ -> false
  | Internal _ -> true
  | Dot (d, _) -> d |> isInternal

let fromPath ~config ~typeEnv path =
  let dep = path |> fromPath1 ~config ~typeEnv in
  if !Debug.typeResolution then
    Log_.item "fromPath path:%s typeEnv:%s %s resolved:%s\n" (path |> Path.name)
      (typeEnv |> TypeEnv.toString)
      (match dep |> isInternal with true -> "Internal" | false -> "External")
      (dep |> depToString);
  match config.namespace with
  | None -> dep
  | Some name -> dep |> handleNamespace ~name

let rec getOuterModuleName dep =
  match dep with
  | External name -> name |> ModuleName.fromStringUnsafe
  | Internal resolvedName ->
      resolvedName |> ResolvedName.toString |> ModuleName.fromStringUnsafe
  | Dot (dep1, _) -> dep1 |> getOuterModuleName

let rec removeExternalOuterModule dep =
  match dep with
  | External _ | Internal _ -> dep
  | Dot (External _, s) -> External s
  | Dot (dep1, s) -> Dot (dep1 |> removeExternalOuterModule, s)
