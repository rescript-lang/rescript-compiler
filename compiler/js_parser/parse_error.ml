(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open Primitive_deriving

type t =
  | EnumBooleanMemberNotInitialized of {
      enum_name: string;
      member_name: string;
    }
  | EnumDuplicateMemberName of {
      enum_name: string;
      member_name: string;
    }
  | EnumInconsistentMemberValues of { enum_name: string }
  | EnumInvalidExplicitType of {
      enum_name: string;
      supplied_type: string option;
    }
  | EnumInvalidExport
  | EnumInvalidInitializerSeparator of { member_name: string }
  | EnumInvalidMemberInitializer of {
      enum_name: string;
      explicit_type: Enum_common.explicit_type option;
      member_name: string;
    }
  | EnumInvalidMemberName of {
      enum_name: string;
      member_name: string;
    }
  | EnumInvalidMemberSeparator
  | EnumInvalidEllipsis of { trailing_comma: bool }
  | EnumNumberMemberNotInitialized of {
      enum_name: string;
      member_name: string;
    }
  | EnumStringMemberInconsistentlyInitailized of { enum_name: string }
  | Unexpected of string
  | UnexpectedWithExpected of string * string
  | UnexpectedTokenWithSuggestion of string * string
  | UnexpectedReserved
  | UnexpectedReservedType
  | UnexpectedSuper
  | UnexpectedSuperCall
  | UnexpectedEOS
  | UnexpectedVariance
  | UnexpectedStatic
  | UnexpectedProto
  | UnexpectedTypeAlias
  | UnexpectedOpaqueTypeAlias
  | UnexpectedTypeAnnotation
  | UnexpectedTypeDeclaration
  | UnexpectedTypeImport
  | UnexpectedTypeExport
  | UnexpectedTypeInterface
  | UnexpectedSpreadType
  | UnexpectedExplicitInexactInObject
  | InexactInsideExact
  | InexactInsideNonObject
  | NewlineAfterThrow
  | InvalidFloatBigInt
  | InvalidSciBigInt
  | InvalidRegExp
  | InvalidRegExpFlags of string
  | UnterminatedRegExp
  | InvalidLHSInAssignment
  | InvalidLHSInExponentiation
  | InvalidLHSInForIn
  | InvalidLHSInForOf
  | InvalidIndexedAccess of { has_bracket: bool }
  | InvalidOptionalIndexedAccess
  | ExpectedPatternFoundExpression
  | MultipleDefaultsInSwitch
  | NoCatchOrFinally
  | UnknownLabel of string
  | Redeclaration of string * string
  | IllegalContinue
  | IllegalBreak
  | IllegalReturn
  | IllegalUnicodeEscape
  | StrictModeWith
  | StrictCatchVariable
  | StrictVarName
  | StrictParamName
  | StrictParamDupe
  | StrictParamNotSimple
  | StrictFunctionName
  | StrictOctalLiteral
  | StrictNonOctalLiteral
  | StrictDelete
  | StrictDuplicateProperty
  | AccessorDataProperty
  | AccessorGetSet
  | InvalidTypeof
  | StrictLHSAssignment
  | StrictLHSPostfix
  | StrictLHSPrefix
  | StrictReservedWord
  | JSXAttributeValueEmptyExpression
  | InvalidJSXAttributeValue
  | ExpectedJSXClosingTag of string
  | NoUninitializedConst
  | NoUninitializedDestructuring
  | NewlineBeforeArrow
  | FunctionAsStatement of { in_strict_mode: bool }
  | AsyncFunctionAsStatement
  | GeneratorFunctionAsStatement
  | AdjacentJSXElements
  | ParameterAfterRestParameter
  | ElementAfterRestElement
  | PropertyAfterRestElement
  | DeclareAsync
  | DeclareClassElement
  | DeclareClassFieldInitializer
  | DeclareOpaqueTypeInitializer
  | DeclareExportLet
  | DeclareExportConst
  | DeclareExportType
  | DeclareExportInterface
  | DuplicateExport of string
  | UnsupportedDecorator
  | MissingTypeParamDefault
  | DuplicateDeclareModuleExports
  | AmbiguousDeclareModuleKind
  | GetterArity
  | SetterArity
  | InvalidNonTypeImportInDeclareModule
  | ImportTypeShorthandOnlyInPureImport
  | ImportSpecifierMissingComma
  | ExportSpecifierMissingComma
  | MalformedUnicode
  | DuplicateConstructor
  | DuplicatePrivateFields of string
  | InvalidClassMemberName of {
      name: string;
      static: bool;
      method_: bool;
      private_: bool;
    }
  | PrivateDelete
  | UnboundPrivate of string
  | PrivateNotInClass
  | SuperPrivate
  | YieldInFormalParameters
  | AwaitAsIdentifierReference
  | YieldAsIdentifierReference
  | AmbiguousLetBracket
  | LiteralShorthandProperty
  | ComputedShorthandProperty
  | MethodInDestructuring
  | TrailingCommaAfterRestElement
  | OptionalChainNew
  | OptionalChainTemplate
  | NullishCoalescingUnexpectedLogical of string
  | WhitespaceInPrivateName
  | ThisParamAnnotationRequired
  | ThisParamMustBeFirst
  | ThisParamMayNotBeOptional
  | GetterMayNotHaveThisParam
  | SetterMayNotHaveThisParam
  | ThisParamBannedInArrowFunctions
  | ThisParamBannedInConstructor
[@@deriving_inline compare]
let _ = fun (_ : t) -> ()
let compare =
  (fun a__001_ ->
     fun b__002_ ->
       if Ppx_compare_lib.phys_equal a__001_ b__002_
       then 0
       else
         (match (a__001_, b__002_) with
          | (EnumBooleanMemberNotInitialized _a__003_,
             EnumBooleanMemberNotInitialized _b__004_) ->
              (match compare_string _a__003_.enum_name _b__004_.enum_name
               with
               | 0 ->
                   compare_string _a__003_.member_name _b__004_.member_name
               | n -> n)
          | (EnumBooleanMemberNotInitialized _, _) -> (-1)
          | (_, EnumBooleanMemberNotInitialized _) -> 1
          | (EnumDuplicateMemberName _a__005_, EnumDuplicateMemberName
             _b__006_) ->
              (match compare_string _a__005_.enum_name _b__006_.enum_name
               with
               | 0 ->
                   compare_string _a__005_.member_name _b__006_.member_name
               | n -> n)
          | (EnumDuplicateMemberName _, _) -> (-1)
          | (_, EnumDuplicateMemberName _) -> 1
          | (EnumInconsistentMemberValues _a__007_,
             EnumInconsistentMemberValues _b__008_) ->
              compare_string _a__007_.enum_name _b__008_.enum_name
          | (EnumInconsistentMemberValues _, _) -> (-1)
          | (_, EnumInconsistentMemberValues _) -> 1
          | (EnumInvalidExplicitType _a__009_, EnumInvalidExplicitType
             _b__010_) ->
              (match compare_string _a__009_.enum_name _b__010_.enum_name
               with
               | 0 ->
                   compare_option compare_string _a__009_.supplied_type
                     _b__010_.supplied_type
               | n -> n)
          | (EnumInvalidExplicitType _, _) -> (-1)
          | (_, EnumInvalidExplicitType _) -> 1
          | (EnumInvalidExport, EnumInvalidExport) -> 0
          | (EnumInvalidExport, _) -> (-1)
          | (_, EnumInvalidExport) -> 1
          | (EnumInvalidInitializerSeparator _a__013_,
             EnumInvalidInitializerSeparator _b__014_) ->
              compare_string _a__013_.member_name _b__014_.member_name
          | (EnumInvalidInitializerSeparator _, _) -> (-1)
          | (_, EnumInvalidInitializerSeparator _) -> 1
          | (EnumInvalidMemberInitializer _a__015_,
             EnumInvalidMemberInitializer _b__016_) ->
              (match compare_string _a__015_.enum_name _b__016_.enum_name
               with
               | 0 ->
                   (match compare_option Enum_common.compare_explicit_type
                            _a__015_.explicit_type _b__016_.explicit_type
                    with
                    | 0 ->
                        compare_string _a__015_.member_name
                          _b__016_.member_name
                    | n -> n)
               | n -> n)
          | (EnumInvalidMemberInitializer _, _) -> (-1)
          | (_, EnumInvalidMemberInitializer _) -> 1
          | (EnumInvalidMemberName _a__019_, EnumInvalidMemberName _b__020_)
              ->
              (match compare_string _a__019_.enum_name _b__020_.enum_name
               with
               | 0 ->
                   compare_string _a__019_.member_name _b__020_.member_name
               | n -> n)
          | (EnumInvalidMemberName _, _) -> (-1)
          | (_, EnumInvalidMemberName _) -> 1
          | (EnumInvalidMemberSeparator, EnumInvalidMemberSeparator) -> 0
          | (EnumInvalidMemberSeparator, _) -> (-1)
          | (_, EnumInvalidMemberSeparator) -> 1
          | (EnumInvalidEllipsis _a__021_, EnumInvalidEllipsis _b__022_) ->
              compare_bool _a__021_.trailing_comma _b__022_.trailing_comma
          | (EnumInvalidEllipsis _, _) -> (-1)
          | (_, EnumInvalidEllipsis _) -> 1
          | (EnumNumberMemberNotInitialized _a__023_,
             EnumNumberMemberNotInitialized _b__024_) ->
              (match compare_string _a__023_.enum_name _b__024_.enum_name
               with
               | 0 ->
                   compare_string _a__023_.member_name _b__024_.member_name
               | n -> n)
          | (EnumNumberMemberNotInitialized _, _) -> (-1)
          | (_, EnumNumberMemberNotInitialized _) -> 1
          | (EnumStringMemberInconsistentlyInitailized _a__025_,
             EnumStringMemberInconsistentlyInitailized _b__026_) ->
              compare_string _a__025_.enum_name _b__026_.enum_name
          | (EnumStringMemberInconsistentlyInitailized _, _) -> (-1)
          | (_, EnumStringMemberInconsistentlyInitailized _) -> 1
          | (Unexpected _a__027_, Unexpected _b__028_) ->
              compare_string _a__027_ _b__028_
          | (Unexpected _, _) -> (-1)
          | (_, Unexpected _) -> 1
          | (UnexpectedWithExpected (_a__029_, _a__031_),
             UnexpectedWithExpected (_b__030_, _b__032_)) ->
              (match compare_string _a__029_ _b__030_ with
               | 0 -> compare_string _a__031_ _b__032_
               | n -> n)
          | (UnexpectedWithExpected _, _) -> (-1)
          | (_, UnexpectedWithExpected _) -> 1
          | (UnexpectedTokenWithSuggestion (_a__033_, _a__035_),
             UnexpectedTokenWithSuggestion (_b__034_, _b__036_)) ->
              (match compare_string _a__033_ _b__034_ with
               | 0 -> compare_string _a__035_ _b__036_
               | n -> n)
          | (UnexpectedTokenWithSuggestion _, _) -> (-1)
          | (_, UnexpectedTokenWithSuggestion _) -> 1
          | (UnexpectedReserved, UnexpectedReserved) -> 0
          | (UnexpectedReserved, _) -> (-1)
          | (_, UnexpectedReserved) -> 1
          | (UnexpectedReservedType, UnexpectedReservedType) -> 0
          | (UnexpectedReservedType, _) -> (-1)
          | (_, UnexpectedReservedType) -> 1
          | (UnexpectedSuper, UnexpectedSuper) -> 0
          | (UnexpectedSuper, _) -> (-1)
          | (_, UnexpectedSuper) -> 1
          | (UnexpectedSuperCall, UnexpectedSuperCall) -> 0
          | (UnexpectedSuperCall, _) -> (-1)
          | (_, UnexpectedSuperCall) -> 1
          | (UnexpectedEOS, UnexpectedEOS) -> 0
          | (UnexpectedEOS, _) -> (-1)
          | (_, UnexpectedEOS) -> 1
          | (UnexpectedVariance, UnexpectedVariance) -> 0
          | (UnexpectedVariance, _) -> (-1)
          | (_, UnexpectedVariance) -> 1
          | (UnexpectedStatic, UnexpectedStatic) -> 0
          | (UnexpectedStatic, _) -> (-1)
          | (_, UnexpectedStatic) -> 1
          | (UnexpectedProto, UnexpectedProto) -> 0
          | (UnexpectedProto, _) -> (-1)
          | (_, UnexpectedProto) -> 1
          | (UnexpectedTypeAlias, UnexpectedTypeAlias) -> 0
          | (UnexpectedTypeAlias, _) -> (-1)
          | (_, UnexpectedTypeAlias) -> 1
          | (UnexpectedOpaqueTypeAlias, UnexpectedOpaqueTypeAlias) -> 0
          | (UnexpectedOpaqueTypeAlias, _) -> (-1)
          | (_, UnexpectedOpaqueTypeAlias) -> 1
          | (UnexpectedTypeAnnotation, UnexpectedTypeAnnotation) -> 0
          | (UnexpectedTypeAnnotation, _) -> (-1)
          | (_, UnexpectedTypeAnnotation) -> 1
          | (UnexpectedTypeDeclaration, UnexpectedTypeDeclaration) -> 0
          | (UnexpectedTypeDeclaration, _) -> (-1)
          | (_, UnexpectedTypeDeclaration) -> 1
          | (UnexpectedTypeImport, UnexpectedTypeImport) -> 0
          | (UnexpectedTypeImport, _) -> (-1)
          | (_, UnexpectedTypeImport) -> 1
          | (UnexpectedTypeExport, UnexpectedTypeExport) -> 0
          | (UnexpectedTypeExport, _) -> (-1)
          | (_, UnexpectedTypeExport) -> 1
          | (UnexpectedTypeInterface, UnexpectedTypeInterface) -> 0
          | (UnexpectedTypeInterface, _) -> (-1)
          | (_, UnexpectedTypeInterface) -> 1
          | (UnexpectedSpreadType, UnexpectedSpreadType) -> 0
          | (UnexpectedSpreadType, _) -> (-1)
          | (_, UnexpectedSpreadType) -> 1
          | (UnexpectedExplicitInexactInObject,
             UnexpectedExplicitInexactInObject) -> 0
          | (UnexpectedExplicitInexactInObject, _) -> (-1)
          | (_, UnexpectedExplicitInexactInObject) -> 1
          | (InexactInsideExact, InexactInsideExact) -> 0
          | (InexactInsideExact, _) -> (-1)
          | (_, InexactInsideExact) -> 1
          | (InexactInsideNonObject, InexactInsideNonObject) -> 0
          | (InexactInsideNonObject, _) -> (-1)
          | (_, InexactInsideNonObject) -> 1
          | (NewlineAfterThrow, NewlineAfterThrow) -> 0
          | (NewlineAfterThrow, _) -> (-1)
          | (_, NewlineAfterThrow) -> 1
          | (InvalidFloatBigInt, InvalidFloatBigInt) -> 0
          | (InvalidFloatBigInt, _) -> (-1)
          | (_, InvalidFloatBigInt) -> 1
          | (InvalidSciBigInt, InvalidSciBigInt) -> 0
          | (InvalidSciBigInt, _) -> (-1)
          | (_, InvalidSciBigInt) -> 1
          | (InvalidRegExp, InvalidRegExp) -> 0
          | (InvalidRegExp, _) -> (-1)
          | (_, InvalidRegExp) -> 1
          | (InvalidRegExpFlags _a__037_, InvalidRegExpFlags _b__038_) ->
              compare_string _a__037_ _b__038_
          | (InvalidRegExpFlags _, _) -> (-1)
          | (_, InvalidRegExpFlags _) -> 1
          | (UnterminatedRegExp, UnterminatedRegExp) -> 0
          | (UnterminatedRegExp, _) -> (-1)
          | (_, UnterminatedRegExp) -> 1
          | (InvalidLHSInAssignment, InvalidLHSInAssignment) -> 0
          | (InvalidLHSInAssignment, _) -> (-1)
          | (_, InvalidLHSInAssignment) -> 1
          | (InvalidLHSInExponentiation, InvalidLHSInExponentiation) -> 0
          | (InvalidLHSInExponentiation, _) -> (-1)
          | (_, InvalidLHSInExponentiation) -> 1
          | (InvalidLHSInForIn, InvalidLHSInForIn) -> 0
          | (InvalidLHSInForIn, _) -> (-1)
          | (_, InvalidLHSInForIn) -> 1
          | (InvalidLHSInForOf, InvalidLHSInForOf) -> 0
          | (InvalidLHSInForOf, _) -> (-1)
          | (_, InvalidLHSInForOf) -> 1
          | (InvalidIndexedAccess _a__039_, InvalidIndexedAccess _b__040_) ->
              compare_bool _a__039_.has_bracket _b__040_.has_bracket
          | (InvalidIndexedAccess _, _) -> (-1)
          | (_, InvalidIndexedAccess _) -> 1
          | (InvalidOptionalIndexedAccess, InvalidOptionalIndexedAccess) -> 0
          | (InvalidOptionalIndexedAccess, _) -> (-1)
          | (_, InvalidOptionalIndexedAccess) -> 1
          | (ExpectedPatternFoundExpression, ExpectedPatternFoundExpression)
              -> 0
          | (ExpectedPatternFoundExpression, _) -> (-1)
          | (_, ExpectedPatternFoundExpression) -> 1
          | (MultipleDefaultsInSwitch, MultipleDefaultsInSwitch) -> 0
          | (MultipleDefaultsInSwitch, _) -> (-1)
          | (_, MultipleDefaultsInSwitch) -> 1
          | (NoCatchOrFinally, NoCatchOrFinally) -> 0
          | (NoCatchOrFinally, _) -> (-1)
          | (_, NoCatchOrFinally) -> 1
          | (UnknownLabel _a__041_, UnknownLabel _b__042_) ->
              compare_string _a__041_ _b__042_
          | (UnknownLabel _, _) -> (-1)
          | (_, UnknownLabel _) -> 1
          | (Redeclaration (_a__043_, _a__045_), Redeclaration
             (_b__044_, _b__046_)) ->
              (match compare_string _a__043_ _b__044_ with
               | 0 -> compare_string _a__045_ _b__046_
               | n -> n)
          | (Redeclaration _, _) -> (-1)
          | (_, Redeclaration _) -> 1
          | (IllegalContinue, IllegalContinue) -> 0
          | (IllegalContinue, _) -> (-1)
          | (_, IllegalContinue) -> 1
          | (IllegalBreak, IllegalBreak) -> 0
          | (IllegalBreak, _) -> (-1)
          | (_, IllegalBreak) -> 1
          | (IllegalReturn, IllegalReturn) -> 0
          | (IllegalReturn, _) -> (-1)
          | (_, IllegalReturn) -> 1
          | (IllegalUnicodeEscape, IllegalUnicodeEscape) -> 0
          | (IllegalUnicodeEscape, _) -> (-1)
          | (_, IllegalUnicodeEscape) -> 1
          | (StrictModeWith, StrictModeWith) -> 0
          | (StrictModeWith, _) -> (-1)
          | (_, StrictModeWith) -> 1
          | (StrictCatchVariable, StrictCatchVariable) -> 0
          | (StrictCatchVariable, _) -> (-1)
          | (_, StrictCatchVariable) -> 1
          | (StrictVarName, StrictVarName) -> 0
          | (StrictVarName, _) -> (-1)
          | (_, StrictVarName) -> 1
          | (StrictParamName, StrictParamName) -> 0
          | (StrictParamName, _) -> (-1)
          | (_, StrictParamName) -> 1
          | (StrictParamDupe, StrictParamDupe) -> 0
          | (StrictParamDupe, _) -> (-1)
          | (_, StrictParamDupe) -> 1
          | (StrictParamNotSimple, StrictParamNotSimple) -> 0
          | (StrictParamNotSimple, _) -> (-1)
          | (_, StrictParamNotSimple) -> 1
          | (StrictFunctionName, StrictFunctionName) -> 0
          | (StrictFunctionName, _) -> (-1)
          | (_, StrictFunctionName) -> 1
          | (StrictOctalLiteral, StrictOctalLiteral) -> 0
          | (StrictOctalLiteral, _) -> (-1)
          | (_, StrictOctalLiteral) -> 1
          | (StrictNonOctalLiteral, StrictNonOctalLiteral) -> 0
          | (StrictNonOctalLiteral, _) -> (-1)
          | (_, StrictNonOctalLiteral) -> 1
          | (StrictDelete, StrictDelete) -> 0
          | (StrictDelete, _) -> (-1)
          | (_, StrictDelete) -> 1
          | (StrictDuplicateProperty, StrictDuplicateProperty) -> 0
          | (StrictDuplicateProperty, _) -> (-1)
          | (_, StrictDuplicateProperty) -> 1
          | (AccessorDataProperty, AccessorDataProperty) -> 0
          | (AccessorDataProperty, _) -> (-1)
          | (_, AccessorDataProperty) -> 1
          | (AccessorGetSet, AccessorGetSet) -> 0
          | (AccessorGetSet, _) -> (-1)
          | (_, AccessorGetSet) -> 1
          | (InvalidTypeof, InvalidTypeof) -> 0
          | (InvalidTypeof, _) -> (-1)
          | (_, InvalidTypeof) -> 1
          | (StrictLHSAssignment, StrictLHSAssignment) -> 0
          | (StrictLHSAssignment, _) -> (-1)
          | (_, StrictLHSAssignment) -> 1
          | (StrictLHSPostfix, StrictLHSPostfix) -> 0
          | (StrictLHSPostfix, _) -> (-1)
          | (_, StrictLHSPostfix) -> 1
          | (StrictLHSPrefix, StrictLHSPrefix) -> 0
          | (StrictLHSPrefix, _) -> (-1)
          | (_, StrictLHSPrefix) -> 1
          | (StrictReservedWord, StrictReservedWord) -> 0
          | (StrictReservedWord, _) -> (-1)
          | (_, StrictReservedWord) -> 1
          | (JSXAttributeValueEmptyExpression,
             JSXAttributeValueEmptyExpression) -> 0
          | (JSXAttributeValueEmptyExpression, _) -> (-1)
          | (_, JSXAttributeValueEmptyExpression) -> 1
          | (InvalidJSXAttributeValue, InvalidJSXAttributeValue) -> 0
          | (InvalidJSXAttributeValue, _) -> (-1)
          | (_, InvalidJSXAttributeValue) -> 1
          | (ExpectedJSXClosingTag _a__047_, ExpectedJSXClosingTag _b__048_)
              -> compare_string _a__047_ _b__048_
          | (ExpectedJSXClosingTag _, _) -> (-1)
          | (_, ExpectedJSXClosingTag _) -> 1
          | (NoUninitializedConst, NoUninitializedConst) -> 0
          | (NoUninitializedConst, _) -> (-1)
          | (_, NoUninitializedConst) -> 1
          | (NoUninitializedDestructuring, NoUninitializedDestructuring) -> 0
          | (NoUninitializedDestructuring, _) -> (-1)
          | (_, NoUninitializedDestructuring) -> 1
          | (NewlineBeforeArrow, NewlineBeforeArrow) -> 0
          | (NewlineBeforeArrow, _) -> (-1)
          | (_, NewlineBeforeArrow) -> 1
          | (FunctionAsStatement _a__049_, FunctionAsStatement _b__050_) ->
              compare_bool _a__049_.in_strict_mode _b__050_.in_strict_mode
          | (FunctionAsStatement _, _) -> (-1)
          | (_, FunctionAsStatement _) -> 1
          | (AsyncFunctionAsStatement, AsyncFunctionAsStatement) -> 0
          | (AsyncFunctionAsStatement, _) -> (-1)
          | (_, AsyncFunctionAsStatement) -> 1
          | (GeneratorFunctionAsStatement, GeneratorFunctionAsStatement) -> 0
          | (GeneratorFunctionAsStatement, _) -> (-1)
          | (_, GeneratorFunctionAsStatement) -> 1
          | (AdjacentJSXElements, AdjacentJSXElements) -> 0
          | (AdjacentJSXElements, _) -> (-1)
          | (_, AdjacentJSXElements) -> 1
          | (ParameterAfterRestParameter, ParameterAfterRestParameter) -> 0
          | (ParameterAfterRestParameter, _) -> (-1)
          | (_, ParameterAfterRestParameter) -> 1
          | (ElementAfterRestElement, ElementAfterRestElement) -> 0
          | (ElementAfterRestElement, _) -> (-1)
          | (_, ElementAfterRestElement) -> 1
          | (PropertyAfterRestElement, PropertyAfterRestElement) -> 0
          | (PropertyAfterRestElement, _) -> (-1)
          | (_, PropertyAfterRestElement) -> 1
          | (DeclareAsync, DeclareAsync) -> 0
          | (DeclareAsync, _) -> (-1)
          | (_, DeclareAsync) -> 1
          | (DeclareClassElement, DeclareClassElement) -> 0
          | (DeclareClassElement, _) -> (-1)
          | (_, DeclareClassElement) -> 1
          | (DeclareClassFieldInitializer, DeclareClassFieldInitializer) -> 0
          | (DeclareClassFieldInitializer, _) -> (-1)
          | (_, DeclareClassFieldInitializer) -> 1
          | (DeclareOpaqueTypeInitializer, DeclareOpaqueTypeInitializer) -> 0
          | (DeclareOpaqueTypeInitializer, _) -> (-1)
          | (_, DeclareOpaqueTypeInitializer) -> 1
          | (DeclareExportLet, DeclareExportLet) -> 0
          | (DeclareExportLet, _) -> (-1)
          | (_, DeclareExportLet) -> 1
          | (DeclareExportConst, DeclareExportConst) -> 0
          | (DeclareExportConst, _) -> (-1)
          | (_, DeclareExportConst) -> 1
          | (DeclareExportType, DeclareExportType) -> 0
          | (DeclareExportType, _) -> (-1)
          | (_, DeclareExportType) -> 1
          | (DeclareExportInterface, DeclareExportInterface) -> 0
          | (DeclareExportInterface, _) -> (-1)
          | (_, DeclareExportInterface) -> 1
          | (DuplicateExport _a__051_, DuplicateExport _b__052_) ->
              compare_string _a__051_ _b__052_
          | (DuplicateExport _, _) -> (-1)
          | (_, DuplicateExport _) -> 1
          | (UnsupportedDecorator, UnsupportedDecorator) -> 0
          | (UnsupportedDecorator, _) -> (-1)
          | (_, UnsupportedDecorator) -> 1
          | (MissingTypeParamDefault, MissingTypeParamDefault) -> 0
          | (MissingTypeParamDefault, _) -> (-1)
          | (_, MissingTypeParamDefault) -> 1
          | (DuplicateDeclareModuleExports, DuplicateDeclareModuleExports) ->
              0
          | (DuplicateDeclareModuleExports, _) -> (-1)
          | (_, DuplicateDeclareModuleExports) -> 1
          | (AmbiguousDeclareModuleKind, AmbiguousDeclareModuleKind) -> 0
          | (AmbiguousDeclareModuleKind, _) -> (-1)
          | (_, AmbiguousDeclareModuleKind) -> 1
          | (GetterArity, GetterArity) -> 0
          | (GetterArity, _) -> (-1)
          | (_, GetterArity) -> 1
          | (SetterArity, SetterArity) -> 0
          | (SetterArity, _) -> (-1)
          | (_, SetterArity) -> 1
          | (InvalidNonTypeImportInDeclareModule,
             InvalidNonTypeImportInDeclareModule) -> 0
          | (InvalidNonTypeImportInDeclareModule, _) -> (-1)
          | (_, InvalidNonTypeImportInDeclareModule) -> 1
          | (ImportTypeShorthandOnlyInPureImport,
             ImportTypeShorthandOnlyInPureImport) -> 0
          | (ImportTypeShorthandOnlyInPureImport, _) -> (-1)
          | (_, ImportTypeShorthandOnlyInPureImport) -> 1
          | (ImportSpecifierMissingComma, ImportSpecifierMissingComma) -> 0
          | (ImportSpecifierMissingComma, _) -> (-1)
          | (_, ImportSpecifierMissingComma) -> 1
          | (ExportSpecifierMissingComma, ExportSpecifierMissingComma) -> 0
          | (ExportSpecifierMissingComma, _) -> (-1)
          | (_, ExportSpecifierMissingComma) -> 1
          | (MalformedUnicode, MalformedUnicode) -> 0
          | (MalformedUnicode, _) -> (-1)
          | (_, MalformedUnicode) -> 1
          | (DuplicateConstructor, DuplicateConstructor) -> 0
          | (DuplicateConstructor, _) -> (-1)
          | (_, DuplicateConstructor) -> 1
          | (DuplicatePrivateFields _a__053_, DuplicatePrivateFields
             _b__054_) -> compare_string _a__053_ _b__054_
          | (DuplicatePrivateFields _, _) -> (-1)
          | (_, DuplicatePrivateFields _) -> 1
          | (InvalidClassMemberName _a__055_, InvalidClassMemberName
             _b__056_) ->
              (match compare_string _a__055_.name _b__056_.name with
               | 0 ->
                   (match compare_bool _a__055_.static _b__056_.static with
                    | 0 ->
                        (match compare_bool _a__055_.method_ _b__056_.method_
                         with
                         | 0 ->
                             compare_bool _a__055_.private_ _b__056_.private_
                         | n -> n)
                    | n -> n)
               | n -> n)
          | (InvalidClassMemberName _, _) -> (-1)
          | (_, InvalidClassMemberName _) -> 1
          | (PrivateDelete, PrivateDelete) -> 0
          | (PrivateDelete, _) -> (-1)
          | (_, PrivateDelete) -> 1
          | (UnboundPrivate _a__057_, UnboundPrivate _b__058_) ->
              compare_string _a__057_ _b__058_
          | (UnboundPrivate _, _) -> (-1)
          | (_, UnboundPrivate _) -> 1
          | (PrivateNotInClass, PrivateNotInClass) -> 0
          | (PrivateNotInClass, _) -> (-1)
          | (_, PrivateNotInClass) -> 1
          | (SuperPrivate, SuperPrivate) -> 0
          | (SuperPrivate, _) -> (-1)
          | (_, SuperPrivate) -> 1
          | (YieldInFormalParameters, YieldInFormalParameters) -> 0
          | (YieldInFormalParameters, _) -> (-1)
          | (_, YieldInFormalParameters) -> 1
          | (AwaitAsIdentifierReference, AwaitAsIdentifierReference) -> 0
          | (AwaitAsIdentifierReference, _) -> (-1)
          | (_, AwaitAsIdentifierReference) -> 1
          | (YieldAsIdentifierReference, YieldAsIdentifierReference) -> 0
          | (YieldAsIdentifierReference, _) -> (-1)
          | (_, YieldAsIdentifierReference) -> 1
          | (AmbiguousLetBracket, AmbiguousLetBracket) -> 0
          | (AmbiguousLetBracket, _) -> (-1)
          | (_, AmbiguousLetBracket) -> 1
          | (LiteralShorthandProperty, LiteralShorthandProperty) -> 0
          | (LiteralShorthandProperty, _) -> (-1)
          | (_, LiteralShorthandProperty) -> 1
          | (ComputedShorthandProperty, ComputedShorthandProperty) -> 0
          | (ComputedShorthandProperty, _) -> (-1)
          | (_, ComputedShorthandProperty) -> 1
          | (MethodInDestructuring, MethodInDestructuring) -> 0
          | (MethodInDestructuring, _) -> (-1)
          | (_, MethodInDestructuring) -> 1
          | (TrailingCommaAfterRestElement, TrailingCommaAfterRestElement) ->
              0
          | (TrailingCommaAfterRestElement, _) -> (-1)
          | (_, TrailingCommaAfterRestElement) -> 1
          | (OptionalChainNew, OptionalChainNew) -> 0
          | (OptionalChainNew, _) -> (-1)
          | (_, OptionalChainNew) -> 1
          | (OptionalChainTemplate, OptionalChainTemplate) -> 0
          | (OptionalChainTemplate, _) -> (-1)
          | (_, OptionalChainTemplate) -> 1
          | (NullishCoalescingUnexpectedLogical _a__059_,
             NullishCoalescingUnexpectedLogical _b__060_) ->
              compare_string _a__059_ _b__060_
          | (NullishCoalescingUnexpectedLogical _, _) -> (-1)
          | (_, NullishCoalescingUnexpectedLogical _) -> 1
          | (WhitespaceInPrivateName, WhitespaceInPrivateName) -> 0
          | (WhitespaceInPrivateName, _) -> (-1)
          | (_, WhitespaceInPrivateName) -> 1
          | (ThisParamAnnotationRequired, ThisParamAnnotationRequired) -> 0
          | (ThisParamAnnotationRequired, _) -> (-1)
          | (_, ThisParamAnnotationRequired) -> 1
          | (ThisParamMustBeFirst, ThisParamMustBeFirst) -> 0
          | (ThisParamMustBeFirst, _) -> (-1)
          | (_, ThisParamMustBeFirst) -> 1
          | (ThisParamMayNotBeOptional, ThisParamMayNotBeOptional) -> 0
          | (ThisParamMayNotBeOptional, _) -> (-1)
          | (_, ThisParamMayNotBeOptional) -> 1
          | (GetterMayNotHaveThisParam, GetterMayNotHaveThisParam) -> 0
          | (GetterMayNotHaveThisParam, _) -> (-1)
          | (_, GetterMayNotHaveThisParam) -> 1
          | (SetterMayNotHaveThisParam, SetterMayNotHaveThisParam) -> 0
          | (SetterMayNotHaveThisParam, _) -> (-1)
          | (_, SetterMayNotHaveThisParam) -> 1
          | (ThisParamBannedInArrowFunctions,
             ThisParamBannedInArrowFunctions) -> 0
          | (ThisParamBannedInArrowFunctions, _) -> (-1)
          | (_, ThisParamBannedInArrowFunctions) -> 1
          | (ThisParamBannedInConstructor, ThisParamBannedInConstructor) -> 0) :
  t -> t -> int)
let _ = compare
[@@@end]
exception Error of (Loc.t * t) * (Loc.t * t) list

let error loc e = raise (Error ((loc, e), []))

module PP = struct
  let error = function
    | EnumBooleanMemberNotInitialized { enum_name; member_name } ->
      Printf.sprintf
        "Boolean enum members need to be initialized. Use either `%s = true,` or `%s = false,` in enum `%s`."
        member_name
        member_name
        enum_name
    | EnumDuplicateMemberName { enum_name; member_name } ->
      Printf.sprintf
        "Enum member names need to be unique, but the name `%s` has already been used before in enum `%s`."
        member_name
        enum_name
    | EnumInconsistentMemberValues { enum_name } ->
      Printf.sprintf
        "Enum `%s` has inconsistent member initializers. Either use no initializers, or consistently use literals (either booleans, numbers, or strings) for all member initializers."
        enum_name
    | EnumInvalidExplicitType { enum_name; supplied_type } ->
      let suggestion =
        Printf.sprintf
          "Use one of `boolean`, `number`, `string`, or `symbol` in enum `%s`."
          enum_name
      in
      begin
        match supplied_type with
        | Some supplied_type ->
          Printf.sprintf "Enum type `%s` is not valid. %s" supplied_type suggestion
        | None -> Printf.sprintf "Supplied enum type is not valid. %s" suggestion
      end
    | EnumInvalidExport ->
      "Cannot export an enum with `export type`, try `export enum E {}` or `module.exports = E;` instead."
    | EnumInvalidInitializerSeparator { member_name } ->
      Printf.sprintf
        "Enum member names and initializers are separated with `=`. Replace `%s:` with `%s =`."
        member_name
        member_name
    | EnumInvalidMemberInitializer { enum_name; explicit_type; member_name } -> begin
      match explicit_type with
      | Some (Enum_common.Boolean as explicit_type)
      | Some (Enum_common.Number as explicit_type)
      | Some (Enum_common.String as explicit_type) ->
        let explicit_type_str = Enum_common.string_of_explicit_type explicit_type in
        Printf.sprintf
          "Enum `%s` has type `%s`, so the initializer of `%s` needs to be a %s literal."
          enum_name
          explicit_type_str
          member_name
          explicit_type_str
      | Some Enum_common.Symbol ->
        Printf.sprintf
          "Symbol enum members cannot be initialized. Use `%s,` in enum `%s`."
          member_name
          enum_name
      | None ->
        Printf.sprintf
          "The enum member initializer for `%s` needs to be a literal (either a boolean, number, or string) in enum `%s`."
          member_name
          enum_name
    end
    | EnumInvalidMemberName { enum_name; member_name } ->
      (* Based on the error condition, we will only receive member names starting with [a-z] *)
      let suggestion = String.capitalize_ascii member_name in
      Printf.sprintf
        "Enum member names cannot start with lowercase 'a' through 'z'. Instead of using `%s`, consider using `%s`, in enum `%s`."
        member_name
        suggestion
        enum_name
    | EnumInvalidMemberSeparator -> "Enum members are separated with `,`. Replace `;` with `,`."
    | EnumInvalidEllipsis { trailing_comma } ->
      if trailing_comma then
        "The `...` must come at the end of the enum body. Remove the trailing comma."
      else
        "The `...` must come after all enum members. Move it to the end of the enum body."
    | EnumNumberMemberNotInitialized { enum_name; member_name } ->
      Printf.sprintf
        "Number enum members need to be initialized, e.g. `%s = 1,` in enum `%s`."
        member_name
        enum_name
    | EnumStringMemberInconsistentlyInitailized { enum_name } ->
      Printf.sprintf
        "String enum members need to consistently either all use initializers, or use no initializers, in enum %s."
        enum_name
    | Unexpected unexpected -> Printf.sprintf "Unexpected %s" unexpected
    | UnexpectedWithExpected (unexpected, expected) ->
      Printf.sprintf "Unexpected %s, expected %s" unexpected expected
    | UnexpectedTokenWithSuggestion (token, suggestion) ->
      Printf.sprintf "Unexpected token `%s`. Did you mean `%s`?" token suggestion
    | UnexpectedReserved -> "Unexpected reserved word"
    | UnexpectedReservedType -> "Unexpected reserved type"
    | UnexpectedSuper -> "Unexpected `super` outside of a class method"
    | UnexpectedSuperCall -> "`super()` is only valid in a class constructor"
    | UnexpectedEOS -> "Unexpected end of input"
    | UnexpectedVariance -> "Unexpected variance sigil"
    | UnexpectedStatic -> "Unexpected static modifier"
    | UnexpectedProto -> "Unexpected proto modifier"
    | UnexpectedTypeAlias -> "Type aliases are not allowed in untyped mode"
    | UnexpectedOpaqueTypeAlias -> "Opaque type aliases are not allowed in untyped mode"
    | UnexpectedTypeAnnotation -> "Type annotations are not allowed in untyped mode"
    | UnexpectedTypeDeclaration -> "Type declarations are not allowed in untyped mode"
    | UnexpectedTypeImport -> "Type imports are not allowed in untyped mode"
    | UnexpectedTypeExport -> "Type exports are not allowed in untyped mode"
    | UnexpectedTypeInterface -> "Interfaces are not allowed in untyped mode"
    | UnexpectedSpreadType -> "Spreading a type is only allowed inside an object type"
    | UnexpectedExplicitInexactInObject ->
      "Explicit inexact syntax must come at the end of an object type"
    | InexactInsideExact ->
      "Explicit inexact syntax cannot appear inside an explicit exact object type"
    | InexactInsideNonObject -> "Explicit inexact syntax can only appear inside an object type"
    | NewlineAfterThrow -> "Illegal newline after throw"
    | InvalidFloatBigInt -> "A bigint literal must be an integer"
    | InvalidSciBigInt -> "A bigint literal cannot use exponential notation"
    | InvalidRegExp -> "Invalid regular expression"
    | InvalidRegExpFlags flags -> "Invalid flags supplied to RegExp constructor '" ^ flags ^ "'"
    | UnterminatedRegExp -> "Invalid regular expression: missing /"
    | InvalidLHSInAssignment -> "Invalid left-hand side in assignment"
    | InvalidLHSInExponentiation -> "Invalid left-hand side in exponentiation expression"
    | InvalidLHSInForIn -> "Invalid left-hand side in for-in"
    | InvalidLHSInForOf -> "Invalid left-hand side in for-of"
    | InvalidIndexedAccess { has_bracket } ->
      let msg =
        if has_bracket then
          "Remove the period."
        else
          "Indexed access uses bracket notation."
      in
      Printf.sprintf "Invalid indexed access. %s Use the format `T[K]`." msg
    | InvalidOptionalIndexedAccess ->
      "Invalid optional indexed access. Indexed access uses bracket notation. Use the format `T?.[K]`."
    | ExpectedPatternFoundExpression ->
      "Expected an object pattern, array pattern, or an identifier but "
      ^ "found an expression instead"
    | MultipleDefaultsInSwitch -> "More than one default clause in switch statement"
    | NoCatchOrFinally -> "Missing catch or finally after try"
    | UnknownLabel label -> "Undefined label '" ^ label ^ "'"
    | Redeclaration (what, name) -> what ^ " '" ^ name ^ "' has already been declared"
    | IllegalContinue -> "Illegal continue statement"
    | IllegalBreak -> "Illegal break statement"
    | IllegalReturn -> "Illegal return statement"
    | IllegalUnicodeEscape -> "Illegal Unicode escape"
    | StrictModeWith -> "Strict mode code may not include a with statement"
    | StrictCatchVariable -> "Catch variable may not be eval or arguments in strict mode"
    | StrictVarName -> "Variable name may not be eval or arguments in strict mode"
    | StrictParamName -> "Parameter name eval or arguments is not allowed in strict mode"
    | StrictParamDupe -> "Strict mode function may not have duplicate parameter names"
    | StrictParamNotSimple ->
      "Illegal \"use strict\" directive in function with non-simple parameter list"
    | StrictFunctionName -> "Function name may not be eval or arguments in strict mode"
    | StrictOctalLiteral -> "Octal literals are not allowed in strict mode."
    | StrictNonOctalLiteral -> "Number literals with leading zeros are not allowed in strict mode."
    | StrictDelete -> "Delete of an unqualified identifier in strict mode."
    | StrictDuplicateProperty ->
      "Duplicate data property in object literal not allowed in strict mode"
    | AccessorDataProperty ->
      "Object literal may not have data and accessor property with the same name"
    | AccessorGetSet -> "Object literal may not have multiple get/set accessors with the same name"
    | StrictLHSAssignment -> "Assignment to eval or arguments is not allowed in strict mode"
    | StrictLHSPostfix ->
      "Postfix increment/decrement may not have eval or arguments operand in strict mode"
    | StrictLHSPrefix ->
      "Prefix increment/decrement may not have eval or arguments operand in strict mode"
    | StrictReservedWord -> "Use of future reserved word in strict mode"
    | JSXAttributeValueEmptyExpression ->
      "JSX attributes must only be assigned a non-empty expression"
    | InvalidJSXAttributeValue -> "JSX value should be either an expression or a quoted JSX text"
    | ExpectedJSXClosingTag name -> "Expected corresponding JSX closing tag for " ^ name
    | NoUninitializedConst -> "Const must be initialized"
    | NoUninitializedDestructuring -> "Destructuring assignment must be initialized"
    | NewlineBeforeArrow -> "Illegal newline before arrow"
    | FunctionAsStatement { in_strict_mode } ->
      if in_strict_mode then
        "In strict mode code, functions can only be declared at top level or "
        ^ "immediately within another function."
      else
        "In non-strict mode code, functions can only be declared at top level, "
        ^ "inside a block, or as the body of an if statement."
    | AsyncFunctionAsStatement ->
      "Async functions can only be declared at top level or "
      ^ "immediately within another function."
    | GeneratorFunctionAsStatement ->
      "Generators can only be declared at top level or " ^ "immediately within another function."
    | AdjacentJSXElements ->
      "Unexpected token <. Remember, adjacent JSX "
      ^ "elements must be wrapped in an enclosing parent tag"
    | ParameterAfterRestParameter -> "Rest parameter must be final parameter of an argument list"
    | ElementAfterRestElement -> "Rest element must be final element of an array pattern"
    | PropertyAfterRestElement -> "Rest property must be final property of an object pattern"
    | DeclareAsync ->
      "async is an implementation detail and isn't necessary for your declare function statement. It is sufficient for your declare function to just have a Promise return type."
    | DeclareClassElement -> "`declare` modifier can only appear on class fields."
    | DeclareClassFieldInitializer ->
      "Unexpected token `=`. Initializers are not allowed in a `declare`."
    | DeclareOpaqueTypeInitializer ->
      "Unexpected token `=`. Initializers are not allowed in a `declare opaque type`."
    | DeclareExportLet -> "`declare export let` is not supported. Use `declare export var` instead."
    | DeclareExportConst ->
      "`declare export const` is not supported. Use `declare export var` instead."
    | DeclareExportType -> "`declare export type` is not supported. Use `export type` instead."
    | DeclareExportInterface ->
      "`declare export interface` is not supported. Use `export interface` instead."
    | DuplicateExport export -> Printf.sprintf "Duplicate export for `%s`" export
    | UnsupportedDecorator -> "Found a decorator in an unsupported position."
    | MissingTypeParamDefault ->
      "Type parameter declaration needs a default, since a preceding type parameter declaration has a default."
    | DuplicateDeclareModuleExports -> "Duplicate `declare module.exports` statement!"
    | AmbiguousDeclareModuleKind ->
      "Found both `declare module.exports` and `declare export` in the same module. Modules can only have 1 since they are either an ES module xor they are a CommonJS module."
    | GetterArity -> "Getter should have zero parameters"
    | SetterArity -> "Setter should have exactly one parameter"
    | InvalidNonTypeImportInDeclareModule ->
      "Imports within a `declare module` body must always be " ^ "`import type` or `import typeof`!"
    | ImportTypeShorthandOnlyInPureImport ->
      "The `type` and `typeof` keywords on named imports can only be used on regular `import` statements. It cannot be used with `import type` or `import typeof` statements"
    | ImportSpecifierMissingComma -> "Missing comma between import specifiers"
    | ExportSpecifierMissingComma -> "Missing comma between export specifiers"
    | MalformedUnicode -> "Malformed unicode"
    | DuplicateConstructor -> "Classes may only have one constructor"
    | DuplicatePrivateFields name ->
      "Private fields may only be declared once. `#" ^ name ^ "` is declared more than once."
    | InvalidClassMemberName { name; static; method_; private_ } ->
      let static_modifier =
        if static then
          "static "
        else
          ""
      in
      let name =
        if private_ then
          "#" ^ name
        else
          name
      in
      let category =
        if method_ then
          "methods"
        else
          "fields"
      in
      "Classes may not have " ^ static_modifier ^ category ^ " named `" ^ name ^ "`."
    | PrivateDelete -> "Private fields may not be deleted."
    | UnboundPrivate name ->
      "Private fields must be declared before they can be referenced. `#"
      ^ name
      ^ "` has not been declared."
    | PrivateNotInClass -> "Private fields can only be referenced from within a class."
    | SuperPrivate -> "You may not access a private field through the `super` keyword."
    | YieldInFormalParameters -> "Yield expression not allowed in formal parameter"
    | AwaitAsIdentifierReference -> "`await` is an invalid identifier in async functions"
    | YieldAsIdentifierReference -> "`yield` is an invalid identifier in generators"
    | AmbiguousLetBracket ->
      "`let [` is ambiguous in this position because it is "
      ^ "either a `let` binding pattern, or a member expression."
    | LiteralShorthandProperty -> "Literals cannot be used as shorthand properties."
    | ComputedShorthandProperty -> "Computed properties must have a value."
    | MethodInDestructuring -> "Object pattern can't contain methods"
    | TrailingCommaAfterRestElement -> "A trailing comma is not permitted after the rest element"
    | OptionalChainNew -> "An optional chain may not be used in a `new` expression."
    | OptionalChainTemplate -> "Template literals may not be used in an optional chain."
    | NullishCoalescingUnexpectedLogical operator ->
      Printf.sprintf
        "Unexpected token `%s`. Parentheses are required to combine `??` with `&&` or `||` expressions."
        operator
    | WhitespaceInPrivateName -> "Unexpected whitespace between `#` and identifier"
    | ThisParamAnnotationRequired -> "A type annotation is required for the `this` parameter."
    | ThisParamMustBeFirst -> "The `this` parameter must be the first function parameter."
    | ThisParamMayNotBeOptional -> "The `this` parameter cannot be optional."
    | GetterMayNotHaveThisParam -> "A getter cannot have a `this` parameter."
    | SetterMayNotHaveThisParam -> "A setter cannot have a `this` parameter."
    | ThisParamBannedInArrowFunctions ->
      "Arrow functions cannot have a `this` parameter; arrow functions automatically bind `this` when declared."
    | ThisParamBannedInConstructor ->
      "Constructors cannot have a `this` parameter; constructors don't bind `this` like other functions."
    | InvalidTypeof -> "`typeof` can only be used to get the type of variables."
end
