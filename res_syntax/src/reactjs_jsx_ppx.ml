open Ast_mapper
open Asttypes
open Parsetree
open Longident

let getPayloadFields payload =
  match payload with
  | PStr
      ({
         pstr_desc =
           Pstr_eval ({pexp_desc = Pexp_record (recordFields, None)}, _);
       }
      :: _rest) ->
    recordFields
  | _ -> []

type configKey = Int | String

let getJsxConfigByKey ~key ~type_ recordFields =
  let values =
    List.filter_map
      (fun ((lid, expr) : Longident.t Location.loc * expression) ->
        match (type_, lid, expr) with
        | ( Int,
            {txt = Lident k},
            {pexp_desc = Pexp_constant (Pconst_integer (value, None))} )
          when k = key ->
          Some value
        | ( String,
            {txt = Lident k},
            (* accept both normal strings and "js" strings *)
            {pexp_desc = Pexp_constant (Pconst_string (value, _))} )
          when k = key ->
          Some value
        | _ -> None)
      recordFields
  in
  match values with
  | [] -> None
  | [v] | v :: _ -> Some v

let getInt ~key fields =
  match fields |> getJsxConfigByKey ~key ~type_:Int with
  | None -> None
  | Some s -> int_of_string_opt s

let getString ~key fields = fields |> getJsxConfigByKey ~key ~type_:String

let updateConfig config payload =
  let fields = getPayloadFields payload in
  (match getInt ~key:"version" fields with
  | None -> ()
  | Some i -> config.React_jsx_common.version <- i);
  (match getString ~key:"module" fields with
  | None -> ()
  | Some s -> config.module_ <- s);
  match getString ~key:"mode" fields with
  | None -> ()
  | Some s -> config.mode <- s

let isJsxConfigAttr ((loc, _) : attribute) = loc.txt = "jsxConfig"

let processConfigAttribute attribute config =
  if isJsxConfigAttr attribute then updateConfig config (snd attribute)

let getMapper ~config =
  let expr3, module_binding3, transformSignatureItem3, transformStructureItem3 =
    Reactjs_jsx_v3.jsxMapper ~config
  in
  let expr4, module_binding4, transformSignatureItem4, transformStructureItem4 =
    Reactjs_jsx_v4.jsxMapper ~config
  in

  let expr mapper e =
    match config.version with
    | 3 -> expr3 mapper e
    | 4 -> expr4 mapper e
    | _ -> default_mapper.expr mapper e
  in
  let module_binding mapper mb =
    match config.version with
    | 3 -> module_binding3 mapper mb
    | 4 -> module_binding4 mapper mb
    | _ -> default_mapper.module_binding mapper mb
  in
  let saveConfig () =
    {
      config with
      version = config.version;
      module_ = config.module_;
      mode = config.mode;
      hasReactComponent = config.hasReactComponent;
    }
  in
  let restoreConfig oldConfig =
    config.version <- oldConfig.React_jsx_common.version;
    config.module_ <- oldConfig.module_;
    config.mode <- oldConfig.mode;
    config.hasReactComponent <- oldConfig.hasReactComponent
  in
  let signature mapper items =
    let oldConfig = saveConfig () in
    config.hasReactComponent <- false;
    let result =
      List.map
        (fun item ->
          (match item.psig_desc with
          | Psig_attribute attr -> processConfigAttribute attr config
          | _ -> ());
          let item = default_mapper.signature_item mapper item in
          if config.version = 3 then transformSignatureItem3 mapper item
          else if config.version = 4 then transformSignatureItem4 mapper item
          else [item])
        items
      |> List.flatten
    in
    restoreConfig oldConfig;
    result
  in
  let structure mapper items =
    let oldConfig = saveConfig () in
    config.hasReactComponent <- false;
    let result =
      List.map
        (fun item ->
          (match item.pstr_desc with
          | Pstr_attribute attr -> processConfigAttribute attr config
          | _ -> ());
          let item = default_mapper.structure_item mapper item in
          if config.version = 3 then transformStructureItem3 item
          else if config.version = 4 then transformStructureItem4 item
          else [item])
        items
      |> List.flatten
    in
    restoreConfig oldConfig;
    result
  in

  {default_mapper with expr; module_binding; signature; structure}

let rewrite_implementation ~jsxVersion ~jsxModule ~jsxMode
    (code : Parsetree.structure) : Parsetree.structure =
  let config =
    {
      React_jsx_common.version = jsxVersion;
      module_ = jsxModule;
      mode = jsxMode;
      nestedModules = [];
      hasReactComponent = false;
    }
  in
  let mapper = getMapper ~config in
  mapper.structure mapper code

let rewrite_signature ~jsxVersion ~jsxModule ~jsxMode
    (code : Parsetree.signature) : Parsetree.signature =
  let config =
    {
      React_jsx_common.version = jsxVersion;
      module_ = jsxModule;
      mode = jsxMode;
      nestedModules = [];
      hasReactComponent = false;
    }
  in
  let mapper = getMapper ~config in
  mapper.signature mapper code
