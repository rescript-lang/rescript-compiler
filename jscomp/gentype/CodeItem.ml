open GenTypeCommon

type exportType = {
  loc: Location.t;
  nameAs: string option;
  opaque: bool option;
  type_: type_;
  typeVars: string list;
  resolvedTypeName: ResolvedName.t;
  docString: DocString.t;
}

type importValue = {
  asPath: string;
  importAnnotation: Annotation.import;
  type_: type_;
  valueName: string;
}

type exportValue = {
  docString: DocString.t;
  moduleAccessPath: Runtime.moduleAccessPath;
  originalName: string;
  resolvedName: ResolvedName.t;
  type_: type_;
}

type exportFromTypeDeclaration = {
  exportType: exportType;
  annotation: Annotation.t;
}

type importType = {
  typeName: string;
  asTypeName: string option;
  importPath: ImportPath.t;
}

type exportTypeItem = {
  typeVars: string list;
  type_: type_;
  annotation: Annotation.t;
}

type exportTypeMap = exportTypeItem StringMap.t

type typeDeclaration = {
  exportFromTypeDeclaration: exportFromTypeDeclaration;
  importTypes: importType list;
}

type t = ExportValue of exportValue | ImportValue of importValue

type translation = {
  importTypes: importType list;
  codeItems: t list;
  typeDeclarations: typeDeclaration list;
}
