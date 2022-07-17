open GenTypeCommon

let translateSignatureValue ~config ~outputFileRelative ~resolver ~typeEnv
    (valueDescription : Typedtree.value_description) : Translation.t =
  let { Typedtree.val_attributes; val_desc; val_id; val_loc } =
    valueDescription
  in
  if !Debug.translation then
    Log_.item "Translate Signature Value %s\n" (val_id |> Ident.name);
  let typeExpr = val_desc.ctyp_type in
  let addAnnotationsToFunction type_ = type_ in
  match (val_id, val_attributes |> Annotation.fromAttributes ~loc:val_loc) with
  | id, GenType ->
      id |> Ident.name
      |> Translation.translateValue ~attributes:val_attributes ~config
           ~docString:(Annotation.getDocString val_attributes)
           ~outputFileRelative ~resolver ~typeEnv ~typeExpr
           ~addAnnotationsToFunction
  | _ -> Translation.empty

let rec translateModuleDeclaration ~config ~outputFileRelative ~resolver
    ~typeEnv ({ md_id; md_type } : Typedtree.module_declaration) =
  let name = md_id |> Ident.name in
  if !Debug.translation then Log_.item "Translate Module Declaration %s\n" name;
  let typeEnv = typeEnv |> TypeEnv.newModule ~name in
  match md_type.mty_desc with
  | Tmty_signature signature ->
      signature
      |> translateSignature ~config ~outputFileRelative ~resolver ~typeEnv
      |> Translation.combine
  | Tmty_ident (path, _) -> (
      match typeEnv |> TypeEnv.lookupModuleTypeSignature ~path with
      | None -> Translation.empty
      | Some (signature, _) ->
          signature
          |> translateSignature ~config ~outputFileRelative ~resolver ~typeEnv
          |> Translation.combine)
  | Tmty_functor _ ->
      logNotImplemented ("Tmty_functor " ^ __LOC__);
      Translation.empty
  | Tmty_with _ ->
      logNotImplemented ("Tmty_with " ^ __LOC__);
      Translation.empty
  | Tmty_typeof _ ->
      logNotImplemented ("Tmty_typeof " ^ __LOC__);
      Translation.empty
  | Tmty_alias _ ->
      logNotImplemented ("Tmty_alias " ^ __LOC__);
      Translation.empty

and translateModuleTypeDeclaration ~config ~outputFileRelative ~resolver
    ~typeEnv (moduleTypeDeclaration : Typedtree.module_type_declaration) =
  if !Debug.translation then
    Log_.item "Translate Module Type Declaration %s\n"
      (moduleTypeDeclaration.mtd_id |> Ident.name);
  match moduleTypeDeclaration with
  | { mtd_type = None } -> Translation.empty
  | { mtd_id; mtd_type = Some mtd_type } -> (
      match mtd_type.mty_desc with
      | Tmty_signature signature ->
          let name = mtd_id |> Ident.name in
          let translation =
            signature
            |> translateSignature ~config ~outputFileRelative ~resolver
                 ~typeEnv:(typeEnv |> TypeEnv.newModuleType ~name ~signature)
            |> Translation.combine
          in
          translation
      | Tmty_ident _ ->
          logNotImplemented ("Tmty_ident " ^ __LOC__);
          Translation.empty
      | Tmty_functor _ ->
          logNotImplemented ("Tmty_functor " ^ __LOC__);
          Translation.empty
      | Tmty_with _ ->
          logNotImplemented ("Tmty_with " ^ __LOC__);
          Translation.empty
      | Tmty_typeof _ ->
          logNotImplemented ("Tmty_typeof " ^ __LOC__);
          Translation.empty
      | Tmty_alias _ ->
          logNotImplemented ("Tmty_alias " ^ __LOC__);
          Translation.empty)

and translateSignatureItem ~config ~outputFileRelative ~resolver ~typeEnv
    signatureItem : Translation.t =
  match signatureItem with
  | { Typedtree.sig_desc = Typedtree.Tsig_type (recFlag, typeDeclarations) } ->
      {
        importTypes = [];
        codeItems = [];
        typeDeclarations =
          typeDeclarations
          |> TranslateTypeDeclarations.translateTypeDeclarations ~config
               ~outputFileRelative ~recursive:(recFlag = Recursive) ~resolver
               ~typeEnv;
      }
  | { Typedtree.sig_desc = Tsig_value valueDescription } ->
      let isImport =
        valueDescription.val_attributes
        |> Annotation.hasAttribute Annotation.tagIsGenTypeImport
      in
      if valueDescription.val_prim <> [] || isImport then
        valueDescription
        |> Translation.translatePrimitive ~config ~outputFileRelative ~resolver
             ~typeEnv
      else
        let moduleItem =
          Runtime.newModuleItem ~name:(valueDescription.val_id |> Ident.name)
        in
        typeEnv |> TypeEnv.updateModuleItem ~moduleItem;
        valueDescription
        |> translateSignatureValue ~config ~outputFileRelative ~resolver
             ~typeEnv
  | { Typedtree.sig_desc = Typedtree.Tsig_module moduleDeclaration } ->
      moduleDeclaration
      |> translateModuleDeclaration ~config ~outputFileRelative ~resolver
           ~typeEnv
  | { Typedtree.sig_desc = Typedtree.Tsig_modtype moduleTypeDeclaration } ->
      let moduleItem =
        Runtime.newModuleItem ~name:(moduleTypeDeclaration.mtd_id |> Ident.name)
      in
      typeEnv |> TypeEnv.updateModuleItem ~moduleItem;
      moduleTypeDeclaration
      |> translateModuleTypeDeclaration ~config ~outputFileRelative ~resolver
           ~typeEnv
  | { Typedtree.sig_desc = Typedtree.Tsig_typext _ } ->
      logNotImplemented ("Tsig_typext " ^ __LOC__);
      Translation.empty
  | { Typedtree.sig_desc = Typedtree.Tsig_exception _ } ->
      logNotImplemented ("Tsig_exception " ^ __LOC__);
      Translation.empty
  | { Typedtree.sig_desc = Typedtree.Tsig_recmodule _ } ->
      logNotImplemented ("Tsig_recmodule " ^ __LOC__);
      Translation.empty
  | { Typedtree.sig_desc = Typedtree.Tsig_open _ } ->
      logNotImplemented ("Tsig_open " ^ __LOC__);
      Translation.empty
  | { Typedtree.sig_desc = Typedtree.Tsig_include _ } ->
      logNotImplemented ("Tsig_include " ^ __LOC__);
      Translation.empty
  | { Typedtree.sig_desc = Typedtree.Tsig_class _ } ->
      logNotImplemented ("Tsig_class " ^ __LOC__);
      Translation.empty
  | { Typedtree.sig_desc = Typedtree.Tsig_class_type _ } ->
      logNotImplemented ("Tsig_class_type " ^ __LOC__);
      Translation.empty
  | { Typedtree.sig_desc = Typedtree.Tsig_attribute _ } ->
      logNotImplemented ("Tsig_attribute " ^ __LOC__);
      Translation.empty

and translateSignature ~config ~outputFileRelative ~resolver ~typeEnv signature
    : Translation.t list =
  if !Debug.translation then Log_.item "Translate Signature\n";
  signature.Typedtree.sig_items
  |> List.map
       (translateSignatureItem ~config ~outputFileRelative ~resolver ~typeEnv)
