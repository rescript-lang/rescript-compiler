open GenTypeCommon

let fileHeader ~sourceFile =
  let makeHeader ~lines =
    match lines with
    | [line] -> "/* " ^ line ^ " */\n\n"
    | _ ->
      "/** \n"
      ^ (lines |> List.map (fun line -> " * " ^ line) |> String.concat "\n")
      ^ "\n */\n\n"
  in
  makeHeader
    ~lines:["TypeScript file generated from " ^ sourceFile ^ " by genType."]
  ^ "/* eslint-disable */\n" ^ "/* tslint:disable */\n"

let interfaceName ~(config : Config.t) name =
  match config.exportInterfaces with
  | true -> "I" ^ name
  | false -> name

let typeAny = ident ~builtin:true "any"

let typeReactComponent ~propsType =
  "React.ComponentType" |> ident ~builtin:true ~typeArgs:[propsType]

let typeReactContext ~type_ =
  "React.Context" |> ident ~builtin:true ~typeArgs:[type_]

let typeReactElementTypeScript = ident ~builtin:true "JSX.Element"
let typeReactChildTypeScript = ident ~builtin:true "React.ReactNode"
let typeReactElement = typeReactElementTypeScript
let typeReactChild = typeReactChildTypeScript
let isTypeReactElement type_ = type_ == typeReactElement

let typeReactDOMReDomRef =
  "React.Ref" |> ident ~builtin:true ~typeArgs:[unknown]

let typeReactEventMouseT = "MouseEvent" |> ident ~builtin:true
let reactRefCurrent = "current"

let typeReactRef ~type_ =
  Object
    ( Open,
      [
        {
          mutable_ = Mutable;
          nameJS = reactRefCurrent;
          optional = Mandatory;
          type_ = Null type_;
          docString = DocString.empty;
        };
      ] )

let isTypeReactRef ~fields =
  match fields with
  | [{mutable_ = Mutable; nameJS; optional = Mandatory}] ->
    nameJS == reactRefCurrent
  | _ -> false

let isTypeFunctionComponent ~fields type_ =
  type_ |> isTypeReactElement && not (isTypeReactRef ~fields)

let rec renderType ~(config : Config.t) ?(indent = None) ~typeNameIsInterface
    ~inFunType type0 =
  match type0 with
  | Array (t, arrayKind) ->
    let typeIsSimple =
      match t with
      | Ident _ | TypeVar _ -> true
      | _ -> false
    in
    if typeIsSimple && arrayKind = Mutable then
      (t |> renderType ~config ~indent ~typeNameIsInterface ~inFunType) ^ "[]"
    else
      let arrayName =
        match arrayKind = Mutable with
        | true -> "Array"
        | false -> "ReadonlyArray"
      in
      arrayName ^ "<"
      ^ (t |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
      ^ ">"
  | Dict type_ ->
    "{[id: string]: "
    ^ (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
    ^ "}"
  | Function
      {argTypes = [{aType = Object (closedFlag, fields)}]; retType; typeVars}
    when retType |> isTypeFunctionComponent ~fields ->
    let fields =
      fields
      |> List.map (fun field ->
             {
               field with
               type_ =
                 field.type_
                 |> TypeVars.substitute ~f:(fun s ->
                        if typeVars |> List.mem s then Some typeAny else None);
             })
    in
    let componentType =
      typeReactComponent ~propsType:(Object (closedFlag, fields))
    in
    componentType |> renderType ~config ~indent ~typeNameIsInterface ~inFunType
  | Function {argTypes; retType; typeVars} ->
    renderFunType ~config ~indent ~inFunType ~typeNameIsInterface ~typeVars
      argTypes retType
  | Object (_, fields) ->
    let indent1 = fields |> Indent.heuristicFields ~indent in
    fields
    |> renderFields ~config ~indent:indent1 ~inFunType ~typeNameIsInterface
  | Ident {builtin; name; typeArgs} ->
    let name = name |> sanitizeTypeName in
    (match
       (not builtin) && config.exportInterfaces && name |> typeNameIsInterface
     with
    | true -> name |> interfaceName ~config
    | false -> name)
    ^ EmitText.genericsString
        ~typeVars:
          (typeArgs
          |> List.map
               (renderType ~config ~indent ~typeNameIsInterface ~inFunType))
  | Null type_ ->
    "(null | "
    ^ (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
    ^ ")"
  | Nullable type_ ->
    let useParens x =
      match type_ with
      | Function _ | Variant _ -> EmitText.parens [x]
      | _ -> x
    in
    "(null | undefined | "
    ^ useParens
        (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
    ^ ")"
  | Option type_ ->
    let useParens x =
      match type_ with
      | Function _ | Variant _ -> EmitText.parens [x]
      | _ -> x
    in
    "(undefined | "
    ^ useParens
        (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
    ^ ")"
  | Promise type_ ->
    "Promise" ^ "<"
    ^ (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
    ^ ">"
  | Tuple innerTypes ->
    "["
    ^ (innerTypes
      |> List.map (renderType ~config ~indent ~typeNameIsInterface ~inFunType)
      |> String.concat ", ")
    ^ "]"
  | TypeVar s -> s
  | Variant {inherits; noPayloads; payloads; polymorphic; tag; unboxed} ->
    let inheritsRendered =
      inherits
      |> List.map (fun type_ ->
             type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
    in
    let noPayloadsRendered = noPayloads |> List.map labelJSToString in
    let field ~name value =
      {
        mutable_ = Mutable;
        nameJS = name;
        optional = Mandatory;
        type_ = TypeVar value;
        docString = DocString.empty;
      }
    in
    let fields fields =
      fields |> renderFields ~config ~indent ~inFunType ~typeNameIsInterface
    in
    let payloadsRendered =
      payloads
      |> List.map (fun {case; t = type_} ->
             let render t =
               t |> renderType ~config ~indent ~typeNameIsInterface ~inFunType
             in
             let tagField =
               case |> labelJSToString
               |> field ~name:(Runtime.jsVariantTag ~polymorphic:false ~tag)
             in
             match (unboxed, type_) with
             | true, type_ ->
               let needParens =
                 match type_ with
                 | Function _ -> true
                 | _ -> false
               in
               let t = type_ |> render in
               if needParens then EmitText.parens [t] else t
             | false, type_ when polymorphic ->
               (* poly variant *)
               [
                 case |> labelJSToString
                 |> field ~name:(Runtime.jsVariantTag ~polymorphic ~tag);
                 type_ |> render
                 |> field ~name:(Runtime.jsVariantValue ~polymorphic);
               ]
               |> fields
             | false, Object (Inline, flds) ->
               (* inlined record *)
               tagField :: flds |> fields
             | false, type_ ->
               (* ordinary variant *)
               let payloads =
                 match type_ with
                 | Tuple ts -> ts
                 | _ -> [type_]
               in
               let flds =
                 tagField
                 :: Ext_list.mapi payloads (fun n t ->
                        t |> render
                        |> field ~name:(Runtime.jsVariantPayloadTag ~n))
               in
               flds |> fields)
    in
    let rendered = inheritsRendered @ noPayloadsRendered @ payloadsRendered in
    let indent1 = rendered |> Indent.heuristicVariants ~indent in
    (match indent1 = None with
    | true -> ""
    | false -> Indent.break ~indent:indent1 ^ "  ")
    ^ (rendered
      |> String.concat
           ((match indent1 = None with
            | true -> " "
            | false -> Indent.break ~indent:indent1)
           ^ "| "))

and renderField ~config ~indent ~typeNameIsInterface ~inFunType
    {mutable_; nameJS = lbl; optional; type_; docString} =
  let optMarker =
    match optional == Optional with
    | true -> "?"
    | false -> ""
  in
  let mutMarker =
    match mutable_ = Immutable with
    | true -> "readonly "
    | false -> ""
  in
  let lbl =
    match isJSSafePropertyName lbl with
    | true -> lbl
    | false -> EmitText.quotes lbl
  in

  let defStr =
    mutMarker ^ lbl ^ optMarker ^ ": "
    ^ (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
  in
  if DocString.hasContent docString then
    (* Always print comments on newline before definition. *)
    let indentStr = indent |> Option.value ~default:"" in
    "\n" ^ indentStr ^ DocString.render docString ^ indentStr ^ defStr
  else Indent.break ~indent ^ defStr

and renderFields ~config ~indent ~inFunType ~typeNameIsInterface fields =
  let indent1 = indent |> Indent.more in
  let space =
    match indent = None && fields <> [] with
    | true -> " "
    | false -> ""
  in
  let renderedFields =
    fields
    |> List.map
         (renderField ~config ~indent:indent1 ~typeNameIsInterface ~inFunType)
  in
  ("{" ^ space)
  ^ String.concat "; " renderedFields
  ^ Indent.break ~indent ^ space ^ "}"

and renderFunType ~config ~indent ~inFunType ~typeNameIsInterface ~typeVars
    argTypes retType =
  (match inFunType with
  | true -> "("
  | false -> "")
  ^ EmitText.genericsString ~typeVars
  ^ "("
  ^ String.concat ", "
      (List.mapi
         (fun i {aName; aType} ->
           let parameterName =
             (match aName = "" with
             | true -> "_" ^ string_of_int (i + 1)
             | false -> aName)
             ^ ":"
           in
           parameterName
           ^ (aType
             |> renderType ~config ~indent ~typeNameIsInterface ~inFunType:true
             ))
         argTypes)
  ^ ") => "
  ^ (retType |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
  ^
  match inFunType with
  | true -> ")"
  | false -> ""

let typeToString ~config ~typeNameIsInterface type_ =
  type_ |> renderType ~config ~typeNameIsInterface ~inFunType:false

let emitExportConst ~early ?(comment = "") ~config
    ?(docString = DocString.empty) ~emitters ~name ~type_ ~typeNameIsInterface
    line =
  let typeString = type_ |> typeToString ~config ~typeNameIsInterface in
  (match comment = "" with
  | true -> comment
  | false -> "// " ^ comment ^ "\n")
  ^ DocString.render docString ^ "export const " ^ name ^ ": " ^ typeString
  ^ " = " ^ line ^ " as any;"
  |> (match early with
     | true -> Emitters.exportEarly
     | false -> Emitters.export)
       ~emitters

let emitExportDefault ~emitters name =
  "export default " ^ name ^ ";" |> Emitters.export ~emitters

let emitExportType ~(config : Config.t) ~emitters ~nameAs ~opaque ~type_
    ~typeNameIsInterface ~typeVars ~docString resolvedTypeName =
  let docString = DocString.render docString in
  let typeParamsString = EmitText.genericsString ~typeVars in
  let isInterface = resolvedTypeName |> typeNameIsInterface in
  let resolvedTypeName =
    match config.exportInterfaces && isInterface with
    | true -> resolvedTypeName |> interfaceName ~config
    | false -> resolvedTypeName
  in
  let exportNameAs =
    match nameAs with
    | None -> ""
    | Some s ->
      "\nexport type " ^ s ^ typeParamsString ^ " = " ^ resolvedTypeName
      ^ typeParamsString ^ ";"
  in
  if opaque then
    (* Represent an opaque type as an absract class with a field called 'opaque'.
       Any type parameters must occur in the type of opaque, so that different
       instantiations are considered different types. *)
    let typeOfOpaqueField =
      match typeVars = [] with
      | true -> "any"
      | false -> typeVars |> String.concat " | "
    in
    docString ^ "export abstract class " ^ resolvedTypeName ^ typeParamsString
    ^ " { protected opaque!: " ^ typeOfOpaqueField
    ^ " }; /* simulate opaque types */" ^ exportNameAs
    |> Emitters.export ~emitters
  else
    (if isInterface && config.exportInterfaces then
       docString ^ "export interface " ^ resolvedTypeName ^ typeParamsString
       ^ " "
     else
       docString ^ "export type " ^ resolvedTypeName ^ typeParamsString ^ " = ")
    ^ (match type_ with
      | _ -> type_ |> typeToString ~config ~typeNameIsInterface)
    ^ ";" ^ exportNameAs
    |> Emitters.export ~emitters

let emitImportValueAsEarly ~emitters ~name ~nameAs importPath =
  "import "
  ^ (match nameAs with
    | Some nameAs -> "{" ^ name ^ " as " ^ nameAs ^ "}"
    | None -> name)
  ^ " from " ^ "'"
  ^ (importPath |> ImportPath.emit)
  ^ "';"
  |> Emitters.requireEarly ~emitters

let emitRequire ~importedValueOrComponent ~early ~emitters ~(config : Config.t)
    ~moduleName importPath =
  let moduleNameString = ModuleName.toString moduleName in
  let importPathString = ImportPath.emit importPath in
  let output =
    match config.module_ with
    | ESModule when not importedValueOrComponent ->
      "import * as " ^ moduleNameString ^ " from '" ^ importPathString ^ "';"
    | _ ->
      "const " ^ moduleNameString ^ " = require('" ^ importPathString ^ "');"
  in
  output
  |> (match early with
     | true -> Emitters.requireEarly
     | false -> Emitters.require)
       ~emitters

let require ~early =
  match early with
  | true -> Emitters.requireEarly
  | false -> Emitters.require

let emitImportReact ~emitters =
  "import * as React from 'react';" |> require ~early:true ~emitters

let emitImportTypeAs ~emitters ~config ~typeName ~asTypeName
    ~typeNameIsInterface ~importPath =
  let typeName = sanitizeTypeName typeName in
  let asTypeName =
    match asTypeName with
    | None -> asTypeName
    | Some s -> Some (sanitizeTypeName s)
  in
  let typeName, asTypeName =
    match asTypeName with
    | Some asName -> (
      match asName |> typeNameIsInterface with
      | true ->
        ( typeName |> interfaceName ~config,
          Some (asName |> interfaceName ~config) )
      | false -> (typeName, asTypeName))
    | None -> (typeName, asTypeName)
  in
  let importPathString = importPath |> ImportPath.emit in
  let importPrefix = "import type" in
  importPrefix ^ " " ^ "{" ^ typeName
  ^ (match asTypeName with
    | Some asT -> " as " ^ asT
    | None -> "")
  ^ "} from '" ^ importPathString ^ "';"
  |> Emitters.import ~emitters

let emitTypeCast ~config ~type_ ~typeNameIsInterface s =
  s ^ " as " ^ (type_ |> typeToString ~config ~typeNameIsInterface)
