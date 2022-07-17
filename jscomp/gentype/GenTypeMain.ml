module StringSet = Set.Make (String)

let cmtCheckAnnotations ~checkAnnotation inputCMT =
  match inputCMT.Cmt_format.cmt_annots with
  | Implementation structure ->
      structure |> Annotation.structureCheckAnnotation ~checkAnnotation
  | Interface signature ->
      signature |> Annotation.signatureCheckAnnotation ~checkAnnotation
  | _ -> false

let cmtHasTypeErrors inputCMT =
  match inputCMT.Cmt_format.cmt_annots with
  | Partial_implementation _ | Partial_interface _ -> true
  | _ -> false

let structureItemIsDeclaration structItem =
  match structItem.Typedtree.str_desc with
  | Typedtree.Tstr_type _ | Tstr_modtype _ | Tstr_module _ -> true
  | _ -> false

let signatureItemIsDeclaration signatureItem =
  match signatureItem.Typedtree.sig_desc with
  | Typedtree.Tsig_type _ | Tsig_modtype _ -> true
  | _ -> false

let inputCmtTranslateTypeDeclarations ~config ~outputFileRelative ~resolver
    inputCMT : CodeItem.translation =
  let { Cmt_format.cmt_annots } = inputCMT in
  let typeEnv = TypeEnv.root () in
  let translations =
    match cmt_annots with
    | Implementation structure ->
        {
          structure with
          str_items =
            structure.str_items |> List.filter structureItemIsDeclaration;
        }
        |> TranslateStructure.translateStructure ~config ~outputFileRelative
             ~resolver ~typeEnv
    | Interface signature ->
        {
          signature with
          sig_items =
            signature.sig_items |> List.filter signatureItemIsDeclaration;
        }
        |> TranslateSignature.translateSignature ~config ~outputFileRelative
             ~resolver ~typeEnv
    | Packed _ | Partial_implementation _ | Partial_interface _ -> []
  in
  translations |> Translation.combine
  |> Translation.addTypeDeclarationsFromModuleEquations ~typeEnv

let translateCMT ~config ~outputFileRelative ~resolver inputCMT : Translation.t
    =
  let { Cmt_format.cmt_annots } = inputCMT in
  let typeEnv = TypeEnv.root () in
  let translations =
    match cmt_annots with
    | Implementation structure ->
        structure
        |> TranslateStructure.translateStructure ~config ~outputFileRelative
             ~resolver ~typeEnv
    | Interface signature ->
        signature
        |> TranslateSignature.translateSignature ~config ~outputFileRelative
             ~resolver ~typeEnv
    | _ -> []
  in
  translations |> Translation.combine
  |> Translation.addTypeDeclarationsFromModuleEquations ~typeEnv

let emitTranslation ~config ~fileName ~outputFile ~outputFileRelative ~resolver
    ~sourceFile translation =
  let codeText =
    translation
    |> EmitJs.emitTranslationAsString ~config ~fileName ~outputFileRelative
         ~resolver ~inputCmtTranslateTypeDeclarations
  in
  let fileContents =
    EmitType.fileHeader ~sourceFile:(Filename.basename sourceFile)
    ^ "\n" ^ codeText ^ "\n"
  in
  GeneratedFiles.writeFileIfRequired ~outputFile ~fileContents

let readCmt cmtFile =
  try Cmt_format.read_cmt cmtFile
  with Cmi_format.Error _ ->
    Log_.item "Error loading %s\n\n" cmtFile;
    Log_.item
      "It looks like you might be using an old version of Bucklescript, or \
       have stale compilation artifacts.\n";
    Log_.item "Check that bs-platform is version 6.2.x or later.\n";
    Log_.item "And try to clean and rebuild.\n\n";
    assert false

let processCmtFile cmt =
  let config = Paths.readConfig ~namespace:(cmt |> Paths.findNameSpace) in
  if !Debug.basic then Log_.item "Cmt %s\n" cmt;
  let cmtFile = cmt |> Paths.getCmtFile in
  if cmtFile <> "" then
    let outputFile = cmt |> Paths.getOutputFile ~config in
    let outputFileRelative = cmt |> Paths.getOutputFileRelative ~config in
    let fileName = cmt |> Paths.getModuleName in
    let isInterface = Filename.check_suffix cmtFile ".cmti" in
    let resolver =
      ModuleResolver.createLazyResolver ~config
        ~extensions:[ ".re"; ".res"; EmitType.shimExtension ]
        ~excludeFile:(fun fname ->
          fname = "React.re" || fname = "ReasonReact.re")
    in
    let inputCMT, hasGenTypeAnnotations =
      let inputCMT = readCmt cmtFile in
      let ignoreInterface = ref false in
      let checkAnnotation ~loc:_ attributes =
        if
          attributes
          |> Annotation.getAttributePayload
               Annotation.tagIsGenTypeIgnoreInterface
          <> None
        then ignoreInterface := true;
        attributes
        |> Annotation.getAttributePayload
             Annotation.tagIsOneOfTheGenTypeAnnotations
        <> None
      in
      let hasGenTypeAnnotations =
        inputCMT |> cmtCheckAnnotations ~checkAnnotation
      in
      if isInterface then
        let cmtFileImpl = (cmtFile |> Filename.chop_extension) ^ ".cmt" in
        let inputCMTImpl = readCmt cmtFileImpl in
        let hasGenTypeAnnotationsImpl =
          inputCMTImpl
          |> cmtCheckAnnotations ~checkAnnotation:(fun ~loc attributes ->
                 if attributes |> checkAnnotation ~loc then (
                   if not !ignoreInterface then (
                     Log_.Color.setup ();
                     Log_.info ~loc ~name:"Warning genType" (fun ppf () ->
                         Format.fprintf ppf
                           "Annotation is ignored as there's a .rei file"));
                   true)
                 else false)
        in
        ( (match !ignoreInterface with
          | true -> inputCMTImpl
          | false -> inputCMT),
          match !ignoreInterface with
          | true -> hasGenTypeAnnotationsImpl
          | false -> hasGenTypeAnnotations )
      else (inputCMT, hasGenTypeAnnotations)
    in
    if hasGenTypeAnnotations then
      let sourceFile =
        match inputCMT.cmt_annots |> FindSourceFile.cmt with
        | Some sourceFile -> sourceFile
        | None -> (
            (fileName |> ModuleName.toString)
            ^ match isInterface with true -> ".resi" | false -> ".res")
      in
      inputCMT
      |> translateCMT ~config ~outputFileRelative ~resolver
      |> emitTranslation ~config ~fileName ~outputFile ~outputFileRelative
           ~resolver ~sourceFile
    else if inputCMT |> cmtHasTypeErrors then
      outputFile |> GeneratedFiles.logFileAction TypeError
    else (
      outputFile |> GeneratedFiles.logFileAction NoMatch;
      if Sys.file_exists outputFile then Sys.remove outputFile)
