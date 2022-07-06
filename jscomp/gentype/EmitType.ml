open GenTypeCommon

let fileHeader ~sourceFile =
  let makeHeader ~lines =
    match lines with
    | [line] -> "/* " ^ line ^ " */\n"
    | _ ->
      "/** \n"
      ^ (lines |> List.map (fun line -> " * " ^ line) |> String.concat "\n")
      ^ "\n */\n"
  in
  makeHeader
    ~lines:["TypeScript file generated from " ^ sourceFile ^ " by genType."]
  ^ "/* eslint-disable import/first */\n\n"

let generatedFilesExtension ~config =
  match config.generatedFileExtension with
  | Some s ->
    (* from .foo.bar to .foo *)
    Filename.remove_extension s
  | None -> ".gen"

let outputFileSuffix ~config =
  match config.generatedFileExtension with
  | Some s when Filename.extension s <> "" (* double extension  *) -> s
  | _ -> generatedFilesExtension ~config ^ ".tsx"

let generatedModuleExtension ~config = generatedFilesExtension ~config
let shimExtension = ".shim.ts"

let interfaceName ~config name =
  match config.exportInterfaces with true -> "I" ^ name | false -> name

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
          nameRE = reactRefCurrent;
          optional = Mandatory;
          type_ = Null type_;
        };
      ] )

let isTypeReactRef ~fields =
  match fields with
  | [{mutable_ = Mutable; nameJS; nameRE; optional = Mandatory}] ->
    nameJS == reactRefCurrent && nameJS == nameRE
  | _ -> false

let isTypeFunctionComponent ~fields type_ =
  type_ |> isTypeReactElement && not (isTypeReactRef ~fields)

let rec renderType ~config ?(indent = None) ~typeNameIsInterface ~inFunType
    type0 =
  match type0 with
  | Array (t, arrayKind) ->
    let typeIsSimple =
      match t with Ident _ | TypeVar _ -> true | _ -> false
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
  | GroupOfLabeledArgs fields | Object (_, fields) | Record fields ->
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
  | Nullable type_ | Option type_ ->
    let useParens x =
      match type_ with Function _ | Variant _ -> EmitText.parens [x] | _ -> x
    in
    "(null | undefined | "
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
  | Variant {inherits; noPayloads; payloads; polymorphic; unboxed} ->
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
        nameRE = name;
        optional = Mandatory;
        type_ = TypeVar value;
      }
    in
    let fields fields =
      fields |> renderFields ~config ~indent ~inFunType ~typeNameIsInterface
    in
    let payloadsRendered =
      payloads
      |> List.map (fun {case; t = type_} ->
             let typeRendered =
               type_
               |> renderType ~config ~indent ~typeNameIsInterface ~inFunType
             in
             match unboxed with
             | true -> typeRendered
             | false ->
               [
                 case |> labelJSToString
                 |> field ~name:(Runtime.jsVariantTag ~polymorphic);
                 typeRendered
                 |> field ~name:(Runtime.jsVariantValue ~polymorphic);
               ]
               |> fields)
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
    {mutable_; nameJS = lbl; optional; type_} =
  let optMarker = match optional == Optional with true -> "?" | false -> "" in
  let mutMarker =
    match mutable_ = Immutable with true -> "readonly " | false -> ""
  in
  let lbl =
    match isJSSafePropertyName lbl with
    | true -> lbl
    | false -> EmitText.quotes lbl
  in

  Indent.break ~indent ^ mutMarker ^ lbl ^ optMarker ^ ": "
  ^ (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)

and renderFields ~config ~indent ~inFunType ~typeNameIsInterface fields =
  let indent1 = indent |> Indent.more in
  let space =
    match indent = None && fields <> [] with true -> " " | false -> ""
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
  (match inFunType with true -> "(" | false -> "")
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
  ^ match inFunType with true -> ")" | false -> ""

let typeToString ~config ~typeNameIsInterface type_ =
  type_ |> renderType ~config ~typeNameIsInterface ~inFunType:false

let ofType ~config ?(typeNameIsInterface = fun _ -> false) ~type_ s =
  s ^ ": " ^ (type_ |> typeToString ~config ~typeNameIsInterface)

let emitExportConst ~early ?(comment = "") ~config ?(docString = "") ~emitters
    ~name ~type_ ~typeNameIsInterface line =
  (match comment = "" with true -> comment | false -> "// " ^ comment ^ "\n")
  ^ docString ^ "export const "
  ^ (name |> ofType ~config ~typeNameIsInterface ~type_)
  ^ " = " ^ line
  |> (match early with
     | true -> Emitters.exportEarly
     | false -> Emitters.export)
       ~emitters

let emitExportDefault ~emitters name =
  "export default " ^ name ^ ";" |> Emitters.export ~emitters

let emitExportType ~config ~emitters ~nameAs ~opaque ~type_ ~typeNameIsInterface
    ~typeVars resolvedTypeName =
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
    "// tslint:disable-next-line:max-classes-per-file \n"
    ^ (match String.capitalize_ascii resolvedTypeName <> resolvedTypeName with
      | true -> "// tslint:disable-next-line:class-name\n"
      | false -> "")
    ^ "export abstract class " ^ resolvedTypeName ^ typeParamsString
    ^ " { protected opaque!: " ^ typeOfOpaqueField
    ^ " }; /* simulate opaque types */" ^ exportNameAs
    |> Emitters.export ~emitters
  else
    (if isInterface && config.exportInterfaces then
     "export interface " ^ resolvedTypeName ^ typeParamsString ^ " "
    else
      "// tslint:disable-next-line:interface-over-type-literal\n"
      ^ "export type " ^ resolvedTypeName ^ typeParamsString ^ " = ")
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

let emitRequire ~importedValueOrComponent ~early ~emitters ~config ~moduleName
    importPath =
  let commentBeforeRequire =
    match importedValueOrComponent with
    | true -> "// tslint:disable-next-line:no-var-requires\n"
    | false -> "// @ts-ignore: Implicit any on import\n"
  in
  match config.module_ with
  | ES6 when not importedValueOrComponent ->
    let moduleNameString = ModuleName.toString moduleName in
    (let es6ImportModule = moduleNameString ^ "__Es6Import" in
     commentBeforeRequire ^ "import * as " ^ es6ImportModule ^ " from '"
     ^ (importPath |> ImportPath.emit)
     ^ "';\n" ^ "const " ^ moduleNameString ^ ": any = " ^ es6ImportModule ^ ";")
    |> (match early with
       | true -> Emitters.requireEarly
       | false -> Emitters.require)
         ~emitters
  | _ ->
    commentBeforeRequire ^ "const "
    ^ ModuleName.toString moduleName
    ^ " = require('"
    ^ (importPath |> ImportPath.emit)
    ^ "');"
    |> (match early with
       | true -> Emitters.requireEarly
       | false -> Emitters.require)
         ~emitters

let require ~early =
  match early with true -> Emitters.requireEarly | false -> Emitters.require

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
  ^ (match asTypeName with Some asT -> " as " ^ asT | None -> "")
  ^ "} from '" ^ importPathString ^ "';"
  |> Emitters.import ~emitters

let ofTypeAny ~config s = s |> ofType ~config ~type_:typeAny

let emitTypeCast ~config ~type_ ~typeNameIsInterface s =
  s ^ " as " ^ (type_ |> typeToString ~config ~typeNameIsInterface)
