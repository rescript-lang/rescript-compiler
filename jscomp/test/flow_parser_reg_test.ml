module Loc
= struct
#1 "loc.ml"
(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type position = {
  line: int;
  column: int;
  offset: int;
}

type filename =
  | LibFile of string
  | SourceFile of string
  | JsonFile of string
  | Builtins

type t = {
  source: filename option;
  start: position;
  _end: position;
}

let none = {
  source = None;
  start = { line = 0; column = 0; offset = 0; };
  _end = { line = 0; column = 0; offset = 0; };
}

let from_lb_p source start _end = Lexing.(
  {
    source;
    start = {
      line = start.pos_lnum;
      column = start.pos_cnum - start.pos_bol;
      offset = start.pos_cnum;
    };
    _end = {
      line = _end.pos_lnum;
      column = max 0 (_end.pos_cnum - _end.pos_bol);
      offset = _end.pos_cnum;
    }
  }
)

(* Returns the position for the token that was just lexed *)
let from_lb source lb = Lexing.(
  let start = lexeme_start_p lb in
  let _end = lexeme_end_p lb in
  from_lb_p source start _end
)

(* Returns the position that the lexer is currently about to lex *)
let from_curr_lb source lb = Lexing.(
  let curr = lb.lex_curr_p in
  from_lb_p source curr curr
)

let btwn loc1 loc2 = {
  source = loc1.source;
  start = loc1.start;
  _end = loc2._end;
}

let btwn_exclusive loc1 loc2 = {
  source = loc1.source;
  start = loc1._end;
  _end = loc2.start;
}

(* Returns the position immediately before the start of the given loc. If the
   given loc is at the beginning of a line, return the position of the first
   char on the same line. *)
let char_before loc =
  let start =
    let { line; column; offset } = loc.start in
    let column, offset = if column > 0
    then column - 1, offset - 1
    else column, offset in
    { line; column; offset }
  in
  let _end = loc.start in
  { loc with start; _end }

(* Returns true if loc1 entirely overlaps loc2 *)
let contains loc1 loc2 =
  loc1.source = loc2.source &&
  loc1.start.offset <= loc2.start.offset &&
  loc1._end.offset >= loc2._end.offset

let string_of_filename = function
  | LibFile x | SourceFile x | JsonFile x -> x
  | Builtins -> "(global)"

let compare =
  (* builtins, then libs, then source and json files at the same priority since
     JSON files are basically source files. *)
  let order_of_filename = function
  | Builtins -> 1
  | LibFile _ -> 2
  | SourceFile _ -> 3
  | JsonFile _ -> 3
  in
  let source_cmp a b =
    match a, b with
    | Some _, None -> -1
    | None, Some _ -> 1
    | None, None -> 0
    | Some fn1, Some fn2 ->
      let k = (order_of_filename fn1) - (order_of_filename fn2) in
      if k <> 0 then k
      else String.compare (string_of_filename fn1) (string_of_filename fn2)
  in
  let pos_cmp a b = Pervasives.compare (a.line, a.column) (b.line, b.column) in
  fun loc1 loc2 ->
    let k = source_cmp loc1.source loc2.source in
    if k = 0 then
      let k = pos_cmp loc1.start loc2.start in
      if k = 0 then pos_cmp loc1._end loc2._end
      else k
    else k

(**
 * This is mostly useful for debugging purposes.
 * Please don't dead-code delete this!
 *)
let to_string ?(include_source=false) loc =
  let source =
    if include_source
    then Printf.sprintf "%S: " (
      match loc.source with
      | Some src -> string_of_filename src
      | None -> "<NONE>"
    ) else ""
  in
  let pos = Printf.sprintf "(%d, %d) to (%d, %d)"
    loc.start.line
    loc.start.column
    loc._end.line
    loc._end.column
  in
  source ^ pos

let source loc = loc.source

let source_is_lib_file = function
| LibFile _ -> true
| Builtins -> true
| SourceFile _ -> false
| JsonFile _ -> false

let filename_map f = function
  | LibFile filename -> LibFile (f filename)
  | SourceFile filename -> SourceFile (f filename)
  | JsonFile filename -> JsonFile (f filename)
  | Builtins -> Builtins

let filename_exists f = function
  | LibFile filename
  | SourceFile filename
  | JsonFile filename -> f filename
  | Builtins -> false

let check_suffix filename suffix =
  filename_exists (fun fn -> Filename.check_suffix fn suffix) filename

let chop_suffix filename suffix =
  filename_map (fun fn -> Filename.chop_suffix fn suffix) filename

let with_suffix filename suffix =
  filename_map (fun fn -> fn ^ suffix) filename

(* implements OrderedType and SharedMem.UserKeyType *)
module FilenameKey = struct
  type t = filename
  let to_string = string_of_filename
  let compare = Pervasives.compare
end

end
module Parse_error
= struct
#1 "parse_error.ml"
(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t =
  | Assertion of string
  | UnexpectedToken of string
  | UnexpectedTokenWithSuggestion of string * string
  | UnexpectedNumber
  | UnexpectedString
  | UnexpectedIdentifier
  | UnexpectedReserved
  | UnexpectedEOS
  | UnexpectedTypeAlias
  | UnexpectedTypeAnnotation
  | UnexpectedTypeDeclaration
  | UnexpectedTypeImport
  | UnexpectedTypeExport
  | UnexpectedTypeInterface
  | NewlineAfterThrow
  | InvalidRegExp
  | InvalidRegExpFlags of string
  | UnterminatedRegExp
  | InvalidLHSInAssignment
  | InvalidLHSInExponentiation
  | InvalidLHSInForIn
  | InvalidLHSInForOf
  | ExpectedPatternFoundExpression
  | MultipleDefaultsInSwitch
  | NoCatchOrFinally
  | UnknownLabel of string
  | Redeclaration of string * string
  | IllegalContinue
  | IllegalBreak
  | IllegalReturn
  | IllegalYield
  | StrictModeWith
  | StrictCatchVariable
  | StrictVarName
  | StrictParamName
  | StrictParamDupe
  | StrictFunctionName
  | StrictOctalLiteral
  | StrictDelete
  | StrictDuplicateProperty
  | AccessorDataProperty
  | AccessorGetSet
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
  | StrictFunctionStatement
  | AdjacentJSXElements
  | ParameterAfterRestParameter
  | AsyncGenerator
  | DeclareAsync
  | DeclareExportLet
  | DeclareExportConst
  | DeclareExportType
  | DeclareExportInterface
  | UnexpectedExportStarAs
  | DuplicateExport of string
  | ExportNamelessClass
  | ExportNamelessFunction
  | UnsupportedDecorator
  | MissingTypeParamDefault
  | WindowsFloatOfString
  | DuplicateDeclareModuleExports
  | AmbiguousDeclareModuleKind

exception Error of (Loc.t * t) list

let error loc e =
  raise (Error [loc, e])

module PP =
  struct
    let error = function
      | Assertion str -> "Unexpected parser state: "^str
      | UnexpectedToken token->  "Unexpected token "^token
      | UnexpectedTokenWithSuggestion (token, suggestion) ->
          Printf.sprintf "Unexpected token `%s`. Did you mean `%s`?"
            token
            suggestion
      | UnexpectedNumber ->  "Unexpected number"
      | UnexpectedString ->  "Unexpected string"
      | UnexpectedIdentifier ->  "Unexpected identifier"
      | UnexpectedReserved ->  "Unexpected reserved word"
      | UnexpectedEOS ->  "Unexpected end of input"
      | UnexpectedTypeAlias -> "Type aliases are not allowed in untyped mode"
      | UnexpectedTypeAnnotation -> "Type annotations are not allowed in untyped mode"
      | UnexpectedTypeDeclaration -> "Type declarations are not allowed in untyped mode"
      | UnexpectedTypeImport -> "Type imports are not allowed in untyped mode"
      | UnexpectedTypeExport -> "Type exports are not allowed in untyped mode"
      | UnexpectedTypeInterface -> "Interfaces are not allowed in untyped mode"
      | NewlineAfterThrow ->  "Illegal newline after throw"
      | InvalidRegExp -> "Invalid regular expression"
      | InvalidRegExpFlags flags -> "Invalid flags supplied to RegExp constructor '"^flags^"'"
      | UnterminatedRegExp ->  "Invalid regular expression: missing /"
      | InvalidLHSInAssignment ->  "Invalid left-hand side in assignment"
      | InvalidLHSInExponentiation -> "Invalid left-hand side in exponentiation expression"
      | InvalidLHSInForIn ->  "Invalid left-hand side in for-in"
      | InvalidLHSInForOf ->  "Invalid left-hand side in for-of"
      | ExpectedPatternFoundExpression -> (
          "Expected an object pattern, array pattern, or an identifier but " ^
          "found an expression instead"
        )
      | MultipleDefaultsInSwitch -> "More than one default clause in switch statement"
      | NoCatchOrFinally ->  "Missing catch or finally after try"
      | UnknownLabel label -> "Undefined label '"^label^"'"
      | Redeclaration (what, name)-> what^" '"^name^"' has already been declared"
      | IllegalContinue -> "Illegal continue statement"
      | IllegalBreak -> "Illegal break statement"
      | IllegalReturn -> "Illegal return statement"
      | IllegalYield -> "Illegal yield expression"
      | StrictModeWith ->  "Strict mode code may not include a with statement"
      | StrictCatchVariable ->  "Catch variable may not be eval or arguments in strict mode"
      | StrictVarName ->  "Variable name may not be eval or arguments in strict mode"
      | StrictParamName ->  "Parameter name eval or arguments is not allowed in strict mode"
      | StrictParamDupe -> "Strict mode function may not have duplicate parameter names"
      | StrictFunctionName ->  "Function name may not be eval or arguments in strict mode"
      | StrictOctalLiteral ->  "Octal literals are not allowed in strict mode."
      | StrictDelete ->  "Delete of an unqualified identifier in strict mode."
      | StrictDuplicateProperty ->  "Duplicate data property in object literal not allowed in strict mode"
      | AccessorDataProperty ->  "Object literal may not have data and accessor property with the same name"
      | AccessorGetSet ->  "Object literal may not have multiple get/set accessors with the same name"
      | StrictLHSAssignment ->  "Assignment to eval or arguments is not allowed in strict mode"
      | StrictLHSPostfix ->  "Postfix increment/decrement may not have eval or arguments operand in strict mode"
      | StrictLHSPrefix ->  "Prefix increment/decrement may not have eval or arguments operand in strict mode"
      | StrictReservedWord ->  "Use of future reserved word in strict mode"
      | JSXAttributeValueEmptyExpression -> "JSX attributes must only be assigned a non-empty expression"
      | InvalidJSXAttributeValue -> "JSX value should be either an expression or a quoted JSX text"
      | ExpectedJSXClosingTag name -> "Expected corresponding JSX closing tag for "^name
      | NoUninitializedConst -> "Const must be initialized"
      | NoUninitializedDestructuring -> "Destructuring assignment must be initialized"
      | NewlineBeforeArrow ->  "Illegal newline before arrow"
      | StrictFunctionStatement -> "In strict mode code, functions can only be"^
          " declared at top level or immediately within another function."
      | AdjacentJSXElements -> "Unexpected token <. Remember, adjacent JSX "^
          "elements must be wrapped in an enclosing parent tag"
      | ParameterAfterRestParameter ->
          "Rest parameter must be final parameter of an argument list"
      | AsyncGenerator -> "A function may not be both async and a generator"
      | DeclareAsync -> "async is an implementation detail and isn't necessary for your declare function statement. It is sufficient for your declare function to just have a Promise return type."
      | DeclareExportLet -> "`declare export let` is not supported. Use \
          `declare export var` instead."
      | DeclareExportConst -> "`declare export const` is not supported. Use \
          `declare export var` instead."
      | DeclareExportType -> "`declare export type` is not supported. Use \
          `export type` instead."
      | DeclareExportInterface -> "`declare export interface` is not supported. Use \
          `export interface` instead."
      | UnexpectedExportStarAs -> "`export * as` is an early-stage proposal \
          and is not enabled by default. To enable support in the parser, use \
          the `esproposal_export_star_as` option"
      | DuplicateExport export -> (Printf.sprintf "Duplicate export for `%s`" export)
      | ExportNamelessClass -> "When exporting a class as a named export, \
          you must specify a class name. Did you mean \
          `export default class ...`?"
      | ExportNamelessFunction -> "When exporting a function as a named export, \
          you must specify a function name. Did you mean \
          `export default function ...`?"
      | UnsupportedDecorator -> "Found a decorator in an unsupported position."
      | MissingTypeParamDefault -> "Type parameter declaration needs a default, \
          since a preceding type parameter declaration has a default."
      | WindowsFloatOfString -> "The Windows version of OCaml has a bug in how \
          it parses hexidecimal numbers. It is fixed in OCaml 4.03.0. Until we \
          can switch to 4.03.0, please avoid either hexidecimal notation or \
          Windows."
      | DuplicateDeclareModuleExports -> "Duplicate `declare module.exports` \
          statement!"
      | AmbiguousDeclareModuleKind -> "Found both `declare module.exports` and \
          `declare export` in the same module. Modules can only have 1 since \
          they are either an ES module xor they are a CommonJS module."
  end

end
module Spider_monkey_ast
= struct
#1 "spider_monkey_ast.ml"
(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*
 * An Ocaml implementation of the SpiderMonkey Parser API
 * https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API
 *)

module rec Identifier : sig
  type t = Loc.t * t'
  and t' = {
    name: string;
    typeAnnotation: Type.annotation option;
    optional: bool;
  }
end = Identifier

and Literal : sig
  module RegExp : sig
    type t = {
      pattern: string;
      flags: string;
    }
  end

  (* Literals also carry along their raw value *)
  type t = {
    value: value;
    raw: string;
  }
  and value =
    | String of string
    | Boolean of bool
    | Null
    | Number of float
    | RegExp of RegExp.t
end = Literal

and Type : sig
  module Function : sig
    module Param : sig
      type t = Loc.t * t'
      and t' = {
        name: Identifier.t;
        typeAnnotation: Type.t;
        optional: bool;
      }
    end

    type t = {
      params: Param.t list;
      returnType: Type.t;
      rest: Param.t option;
      typeParameters: Type.ParameterDeclaration.t option;
    }
  end

  module Object : sig
    module Property : sig
      type t' = {
        key: Expression.Object.Property.key;
        value: Type.t;
        optional: bool;
        static: bool;
        _method: bool;
      }
      type t = Loc.t * t'
    end
    module Indexer: sig
      type t' = {
        id: Identifier.t;
        key: Type.t;
        value: Type.t;
        static: bool;
      }
      and t = Loc.t * t'
    end
    module CallProperty: sig
      type t = Loc.t * t'
      and t' = {
        value: Loc.t * Function.t;
        static: bool;
      }
    end
    type t = {
      properties: Property.t list;
      indexers: Indexer.t list;
      callProperties: CallProperty.t list;
    }
  end

  module Generic : sig
    module Identifier : sig
      type t =
      | Unqualified of Identifier.t
      | Qualified of qualified
      and qualified = Loc.t * qualified'
      and qualified' = {
        qualification: t;
        id: Identifier.t
      }
    end
    type t = {
      id: Identifier.t;
      typeParameters: Type.ParameterInstantiation.t option;
    }
  end

  module StringLiteral : sig
    type t = {
      value: string;
      raw: string;
    }
  end

  module NumberLiteral : sig
    type t = {
      value: float;
      raw: string;
    }
  end

  module BooleanLiteral : sig
    type t = {
      value: bool;
      raw: string;
    }
  end

  type t = Loc.t * t'
  (* Yes, we could add a little complexity here to show that Any and Void
   * should never be declared nullable, but that check can happen later *)
  and t' =
    | Any
    | Void
    | Null
    | Number
    | String
    | Boolean
    | Nullable of t
    | Function of Function.t
    | Object of Object.t
    | Array of t
    | Generic of Generic.t
    | Union of t list
    | Intersection of t list
    | Typeof of t
    | Tuple of t list
    | StringLiteral of StringLiteral.t
    | NumberLiteral of NumberLiteral.t
    | BooleanLiteral of BooleanLiteral.t
    | Exists

  (* Type.annotation is a concrete syntax node with a location that starts at
   * the colon and ends after the type. For example, "var a: number", the
   * identifier a would have a property typeAnnotation which contains a
   * Type.annotation with a location from column 6-14 *)
  and annotation = Loc.t * t

  module ParameterDeclaration : sig
    module TypeParam : sig
      module Variance : sig
        type t = Plus | Minus
      end
      type t = Loc.t * t'
      and t' = {
        name: string;
        bound: Type.annotation option;
        variance: Variance.t option;
        default: Type.t option;
      }
    end
    type t = Loc.t * t'
    and t' = {
      params: TypeParam.t list;
    }
  end
  module ParameterInstantiation : sig
    type t = Loc.t * t'
    and t' = {
      params: Type.t list;
    }
  end
end = Type

and Statement : sig
  module Block : sig
    type t = {
      body: Statement.t list
    }
  end
  module If : sig
    type t = {
      test: Expression.t;
      consequent: Statement.t;
      alternate: Statement.t option;
    }
  end
  module Labeled : sig
    type t = {
      label: Identifier.t;
      body: Statement.t;
    }
  end
  module Break : sig
    type t = {
      label: Identifier.t option;
    }
  end
  module Continue : sig
    type t = {
      label: Identifier.t option;
    }
  end
  module With : sig
    type t = {
      _object: Expression.t;
      body: Statement.t;
    }
  end
  module TypeAlias : sig
    type t = {
      id: Identifier.t;
      typeParameters: Type.ParameterDeclaration.t option;
      right: Type.t;
    }
  end
  module Switch : sig
    module Case : sig
      type t = Loc.t * t'
      and t' = {
        test: Expression.t option;
        consequent: Statement.t list;
      }
    end
    type t = {
      discriminant: Expression.t;
      cases: Case.t list;
      lexical: bool;
    }
  end
  module Return : sig
    type t = {
      argument: Expression.t option;
    }
  end
  module Throw : sig
    type t = {
      argument: Expression.t;
    }
  end
  module Try : sig
    module CatchClause : sig
      type t = Loc.t * t'
      and t' = {
        param: Pattern.t;
        guard: Expression.t option;
        body: Loc.t * Block.t;
      }
    end
    type t = {
      block: Loc.t * Block.t;
      handler: CatchClause.t option;
      guardedHandlers: CatchClause.t list;
      finalizer: (Loc.t * Block.t) option;
    }
  end
  module VariableDeclaration : sig
    module Declarator : sig
      type t = Loc.t * t'
      and t' = {
        id: Pattern.t;
        init: Expression.t option;
      }
    end
    type kind =
      | Var
      | Let
      | Const
    type t = {
      declarations: Declarator.t list;
      kind: kind;
    }
  end
  module While : sig
    type t = {
      test: Expression.t;
      body: Statement.t;
    }
  end
  module DoWhile : sig
    type t = {
      body: Statement.t;
      test: Expression.t;
    }
  end
  module For : sig
    type init =
      | InitDeclaration of (Loc.t * VariableDeclaration.t)
      | InitExpression of Expression.t
    type t = {
      init: init option;
      test: Expression.t option;
      update: Expression.t option;
      body: Statement.t;
    }
  end
  module ForIn : sig
    type left =
      | LeftDeclaration of (Loc.t * VariableDeclaration.t)
      | LeftExpression of Expression.t
    type t = {
      left: left;
      right: Expression.t;
      body: Statement.t;
      each: bool;
    }
  end
  module ForOf : sig
    type left =
      | LeftDeclaration of (Loc.t * VariableDeclaration.t)
      | LeftExpression of Expression.t
    type t = {
      left: left;
      right: Expression.t;
      body: Statement.t;
    }
  end
  module Let : sig
    type assignment = { id: Pattern.t; init: Expression.t option; }
    type t = {
      head: assignment list;
      body: Statement.t;
    }
  end
  module Interface : sig
    type t = {
      id: Identifier.t;
      typeParameters: Type.ParameterDeclaration.t option;
      body: Loc.t * Type.Object.t;
      extends: (Loc.t * Type.Generic.t) list;
      mixins: (Loc.t * Type.Generic.t) list;
    }
  end
  module DeclareVariable : sig
    type t = {
      id: Identifier.t;
    }
  end
  module DeclareFunction : sig
    type t = {
      id: Identifier.t;
      predicate: Predicate.t option;
    }
  end
  module DeclareModule : sig
    type id =
      | Identifier of Identifier.t
      | Literal of (Loc.t * Literal.t)

    type module_kind =
      | CommonJS of Loc.t
      | ES of Loc.t

    type t = {
      id: id;
      body: Loc.t * Block.t;
      kind: module_kind;
    }
  end
  module ExportDeclaration : sig
    module Specifier : sig
      type t = Loc.t * t'
      and t' = {
        id: Identifier.t;
        name: Identifier.t option;
      }
    end
    type declaration =
      | Declaration of Statement.t
      | Expression of Expression.t
    type specifier =
      | ExportSpecifiers of Specifier.t list
      | ExportBatchSpecifier of Loc.t * Identifier.t option
    type exportKind =
      | ExportType
      | ExportValue
    type t = {
      default: bool;
      declaration: declaration option;
      specifiers: specifier option;
      source: (Loc.t * Literal.t) option; (* This will always be a string *)
      exportKind: exportKind;
    }
  end
  module DeclareExportDeclaration : sig
    type declaration =
      (* declare export var *)
      | Variable of (Loc.t * DeclareVariable.t)
      (* declare export function *)
      | Function of (Loc.t * DeclareFunction.t)
      (* declare export class *)
      | Class of (Loc.t * Interface.t)
      (* declare export default [type]
       * this corresponds to things like
       * export default 1+1; *)
      | DefaultType of Type.t
      (* declare export type *)
      | NamedType of (Loc.t * TypeAlias.t)
      (* declare export interface *)
      | Interface of (Loc.t * Interface.t)

    type t = {
      default: bool;
      declaration: declaration option;
      specifiers: ExportDeclaration.specifier option;
      source: (Loc.t * Literal.t) option; (* This will always be a string *)
    }
  end
  module ImportDeclaration : sig
    module NamedSpecifier : sig
      type t = Loc.t * t'
      and t' = {
        id: Identifier.t;
        name: Identifier.t option;
      }
    end
    type named_specifier = {
      local: Identifier.t option;
      remote: Identifier.t;
    }
    type specifier =
      | ImportNamedSpecifier of named_specifier
      | ImportDefaultSpecifier of Identifier.t
      | ImportNamespaceSpecifier of (Loc.t * Identifier.t)
    type importKind =
      | ImportType
      | ImportTypeof
      | ImportValue
    type t = {
      importKind: importKind;
      source: (Loc.t * Literal.t); (* Always a string literal *)
      specifiers: specifier list;
    }
  end
  module Expression : sig
    type t = {
      expression: Expression.t;
    }
  end

  type t = Loc.t * t'
  and t' =
    | Empty
    | Block of Block.t
    | Expression of Expression.t
    | If of If.t
    | Labeled of Labeled.t
    | Break of Break.t
    | Continue of Continue.t
    | With of With.t
    | TypeAlias of TypeAlias.t
    | Switch of Switch.t
    | Return of Return.t
    | Throw of Throw.t
    | Try of Try.t
    | While of While.t
    | DoWhile of DoWhile.t
    | For of For.t
    | ForIn of ForIn.t
    | ForOf of ForOf.t
    | Let of Let.t
    | Debugger
    | FunctionDeclaration of Function.t
    | VariableDeclaration of VariableDeclaration.t
    | ClassDeclaration of Class.t
    | InterfaceDeclaration of Interface.t
    | DeclareVariable of DeclareVariable.t
    | DeclareFunction of DeclareFunction.t
    | DeclareClass of Interface.t
    | DeclareModule of DeclareModule.t
    | DeclareModuleExports of Type.annotation
    | DeclareExportDeclaration of DeclareExportDeclaration.t
    | ExportDeclaration of ExportDeclaration.t
    | ImportDeclaration of ImportDeclaration.t
end = Statement

and Expression : sig
  module SpreadElement : sig
    type t = Loc.t * t'
    and t' = {
      argument: Expression.t;
    }
  end
  type expression_or_spread =
    | Expression of Expression.t
    | Spread of SpreadElement.t

  module Array : sig
    type t = {
      elements: expression_or_spread option list;
    }
  end
  module TemplateLiteral : sig
    module Element : sig
      type value = {
        raw: string;
        cooked: string;
      }
      type t = Loc.t * t'
      and t' = {
        value: value;
        tail: bool;
      }
    end
    type t = {
      quasis: Element.t list;
      expressions: Expression.t list;
    }
  end
  module TaggedTemplate : sig
    type t = {
      tag: Expression.t;
      quasi: Loc.t * TemplateLiteral.t;
    }
  end
  module Object : sig
    (* This is a slight deviation from the Mozilla spec. In the spec, an object
      * property is not a proper node, and lacks a location and a "type" field.
      * Esprima promotes it to a proper node and that is useful, so I'm
      * following their example *)
    module Property : sig
      type key =
        | Literal of (Loc.t * Literal.t)
        | Identifier of Identifier.t
        | Computed of Expression.t
      type kind =
        | Init
        | Get
        | Set
      type t = Loc.t * t'
      and t' = {
        key: key;
        value: Expression.t;
        kind: kind;
        _method: bool;
        shorthand: bool;
      }
    end
    module SpreadProperty : sig
      type t = Loc.t * t'
      and t' = {
        argument: Expression.t;
      }
    end
    type property =
      | Property of Property.t
      | SpreadProperty of SpreadProperty.t
    type t = {
      properties: property list;
    }
  end
  module Sequence : sig
    type t = {
      expressions: Expression.t list;
    }
  end
  module Unary : sig
    type operator =
      | Minus
      | Plus
      | Not
      | BitNot
      | Typeof
      | Void
      | Delete
      | Await
    type t = {
      operator: operator;
      prefix: bool;
      argument: Expression.t
    }
  end
  module Binary : sig
    type operator =
      | Equal
      | NotEqual
      | StrictEqual
      | StrictNotEqual
      | LessThan
      | LessThanEqual
      | GreaterThan
      | GreaterThanEqual
      | LShift
      | RShift
      | RShift3
      | Plus
      | Minus
      | Mult
      | Exp
      | Div
      | Mod
      | BitOr
      | Xor
      | BitAnd
      | In
      | Instanceof
    type t = {
      operator: operator;
      left: Expression.t;
      right: Expression.t;
    }
  end
  module Assignment : sig
    type operator =
      | Assign
      | PlusAssign
      | MinusAssign
      | MultAssign
      | ExpAssign
      | DivAssign
      | ModAssign
      | LShiftAssign
      | RShiftAssign
      | RShift3Assign
      | BitOrAssign
      | BitXorAssign
      | BitAndAssign
    type t = {
      operator: operator;
      left: Pattern.t;
      right: Expression.t;
    }
  end
  module Update : sig
    type operator =
      | Increment
      | Decrement
    type t = {
      operator: operator;
      argument: Expression.t;
      prefix: bool;
    }
  end
  module Logical : sig
    type operator =
      | Or
      | And
    type t = {
      operator: operator;
      left: Expression.t;
      right: Expression.t;
    }
  end
  module Conditional : sig
    type t = {
      test: Expression.t;
      consequent: Expression.t;
      alternate: Expression.t;
    }
  end
  module New : sig
    type t = {
      callee: Expression.t;
      arguments: expression_or_spread list;
    }
  end
  module Call : sig
    type t = {
      callee: Expression.t;
      arguments: expression_or_spread list;
    }
  end
  module Member : sig
    type property =
      | PropertyIdentifier of Identifier.t
      | PropertyExpression of Expression.t
    type t = {
      _object: Expression.t;
      property: property;
      computed: bool;
    }
  end
  module Yield : sig
    type t = {
      argument: Expression.t option;
      delegate: bool;
    }
  end
  module Comprehension : sig
    module Block : sig
      type t = Loc.t * t'
      and t' = {
        left: Pattern.t;
        right: Expression.t;
        each: bool;
      }
    end
    type t = {
      blocks: Block.t list;
      filter: Expression.t option;
    }
  end
  module Generator : sig
    type t = {
      blocks: Comprehension.Block.t list;
      filter: Expression.t option;
    }
  end
  module Let : sig
    type t = {
      head: Statement.Let.assignment list;
      body: Expression.t;
    }
  end
  module TypeCast : sig
    type t = {
      expression: Expression.t;
      typeAnnotation: Type.annotation;
    }
  end

  type t = Loc.t * t'
  and t' =
    | This
    | Array of Array.t
    | Object of Object.t
    | Function of Function.t
    | ArrowFunction of Function.t
    | Sequence of Sequence.t
    | Unary of Unary.t
    | Binary of Binary.t
    | Assignment of Assignment.t
    | Update of Update.t
    | Logical of Logical.t
    | Conditional of Conditional.t
    | New of New.t
    | Call of Call.t
    | Member of Member.t
    | Yield of Yield.t
    | Comprehension of Comprehension.t
    | Generator of Generator.t
    | Let of Let.t
    | Identifier of Identifier.t
    | Literal of Literal.t
    | TemplateLiteral of TemplateLiteral.t
    | TaggedTemplate of TaggedTemplate.t
    | JSXElement of JSX.element
    | Class of Class.t
    | TypeCast of TypeCast.t
end = Expression

and JSX : sig
  module Identifier : sig
    type t = Loc.t * t'
    and t' = {
      name: string;
    }
  end

  module NamespacedName : sig
    type t = Loc.t * t'
    and t' = {
      namespace: Identifier.t;
      name: Identifier.t;
    }
  end

  module ExpressionContainer : sig
    type t = {
      expression: expression;
    }
    and expression =
    | Expression of Expression.t
    | EmptyExpression of Loc.t
  end

  module Text : sig
    type t = {
      value: string;
      raw: string;
    }
  end

  module Attribute : sig
    type t = Loc.t * t'
    and name =
    | Identifier of Identifier.t
    | NamespacedName of NamespacedName.t
    and value =
    | Literal of Loc.t * Literal.t
    | ExpressionContainer of Loc.t * ExpressionContainer.t
    and t' = {
      name: name;
      value: value option;
    }
  end

  module SpreadAttribute : sig
    type t = Loc.t * t'
    and t' = {
      argument: Expression.t;
    }
  end

  module MemberExpression : sig
    type t = Loc.t * t'
    and _object =
    | Identifier of Identifier.t
    | MemberExpression of t
    and t' = {
      _object: _object;
      property: Identifier.t;
    }
  end

  type name =
    | Identifier of Identifier.t
    | NamespacedName of NamespacedName.t
    | MemberExpression of MemberExpression.t

  module Opening : sig
    type t = Loc.t * t'

    and attribute =
      | Attribute of Attribute.t
      | SpreadAttribute of SpreadAttribute.t

    and t' = {
      name: name;
      selfClosing: bool;
      attributes: attribute list;
    }
  end

  module Closing : sig
    type t = Loc.t * t'
    and t' = {
      name: name;
    }
  end

  type child = Loc.t * child'
  and child' =
    | Element of element
    | ExpressionContainer of ExpressionContainer.t
    | Text of Text.t

  and element = {
    openingElement: Opening.t;
    closingElement: Closing.t option;
    children: child list
  }
end = JSX

and Pattern : sig
  module Object : sig
    module Property : sig
      type key =
        | Literal of (Loc.t * Literal.t)
        | Identifier of Identifier.t
        | Computed of Expression.t
      type t = Loc.t * t'
      and t' = {
        key: key;
        pattern: Pattern.t;
        shorthand: bool;
      }
    end
    module SpreadProperty : sig
      type t = Loc.t * t'
      and t' = {
        argument: Pattern.t;
      }
    end
    type property =
      | Property of Property.t
      | SpreadProperty of SpreadProperty.t
    type t = {
      properties: property list;
      typeAnnotation: Type.annotation option;
    }
  end
  module Array : sig
    module SpreadElement : sig
      type t = Loc.t * t'
      and t' = {
        argument: Pattern.t;
      }
    end
    type element =
      | Element of Pattern.t
      | Spread of SpreadElement.t
    type t = {
      elements: element option list;
      typeAnnotation: Type.annotation option;
    }
  end
  module Assignment : sig
    type t = {
      left: Pattern.t;
      right: Expression.t;
    }
  end
  type t = Loc.t * t'
  and t' =
    | Object of Object.t
    | Array of Array.t
    | Assignment of Assignment.t
    | Identifier of Identifier.t
    | Expression of Expression.t
end = Pattern

and Comment : sig
  type t = Loc.t * t'
  and t' =
    | Block of string
    | Line of string
end = Comment

and Class : sig
  module Method : sig
    type t = Loc.t * t'
    and kind =
      | Constructor
      | Method
      | Get
      | Set
    and t' = {
      kind: kind;
      key: Expression.Object.Property.key;
      value: Loc.t * Function.t;
      static: bool;
      decorators: Expression.t list;
    }
  end
  module Property : sig
    type t = Loc.t * t'
    and t' = {
      key: Expression.Object.Property.key;
      value: Expression.t option;
      typeAnnotation: Type.annotation option;
      static: bool;
    }
  end
  module Implements : sig
    type t = Loc.t * t'
    and t' = {
      id: Identifier.t;
      typeParameters: Type.ParameterInstantiation.t option;
    }
  end
  module Body : sig
    type element =
      | Method of Method.t
      | Property of Property.t
    type t = Loc.t * t'
    and t' = {
      body: element list;
    }
  end
  type t = {
    id: Identifier.t option;
    body: Class.Body.t;
    superClass: Expression.t option;
    typeParameters: Type.ParameterDeclaration.t option;
    superTypeParameters: Type.ParameterInstantiation.t option;
    implements: Class.Implements.t list;
    classDecorators: Expression.t list;
  }
end = Class

and Function : sig
  type body =
    | BodyBlock of (Loc.t * Statement.Block.t)
    | BodyExpression of Expression.t
  type t = {
    id: Identifier.t option;
    params: Pattern.t list;
    defaults: Expression.t option list;
    rest: Identifier.t option;
    body: body;
    async: bool;
    generator: bool;
    predicate: Predicate.t option;
    expression: bool;
    returnType: Type.annotation option;
    typeParameters: Type.ParameterDeclaration.t option;
  }
end = Function

and Predicate : sig
  type t = Loc.t * t'
  and t' =
    | Declared of Expression.t
    | Inferred
end = Predicate

type program = Loc.t * Statement.t list * Comment.t list

end
module Estree_translator
= struct
#1 "estree_translator.ml"
(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Spider_monkey_ast

module type Translator = sig
  type t
  val string: string -> t
  val bool: bool -> t
  val obj: (string * t) array -> t
  val array: t array -> t
  val number: float -> t
  val null: t
  val regexp: Loc.t -> string -> string -> t
end

module Translate (Impl : Translator) : (sig
  type t
  val program:
    Loc.t * Ast.Statement.t list * (Loc.t * Ast.Comment.t') list ->
    t
  val expression: Ast.Expression.t -> t
  val errors: (Loc.t * Parse_error.t) list -> t
end with type t = Impl.t) = struct
  type t = Impl.t

  open Ast
  open Impl

  let array_of_list fn list = array (Array.of_list (List.map fn list))
  let int x = number (float x)
  let option f = function
    | Some v -> f v
    | None -> null

  let position p =
    obj [|
      "line", int p.Loc.line;
      "column", int p.Loc.column;
    |]

  let loc location =
    let source = match Loc.source location with
    | Some Loc.LibFile src
    | Some Loc.SourceFile src
    | Some Loc.JsonFile src -> string src
    | Some Loc.Builtins -> string "(global)"
    | None -> null
    in
    obj [|
      "source", source;
      "start", position location.Loc.start;
      "end", position location.Loc._end;
    |]

  let range location = Loc.(
    array [|
      int location.start.offset;
      int location._end.offset;
    |]
  )

  let node _type location props =
    obj (Array.append [|
      "type", string _type;
      "loc", loc location;
      "range", range location;
    |] props)

  let errors l =
    let error (location, e) =
      obj [|
        "loc", loc location;
        "message", string (Parse_error.PP.error e);
      |]
    in array_of_list error l

  let rec program (loc, statements, comments) =
    node "Program" loc [|
      "body", statement_list statements;
      "comments", comment_list comments;
    |]

  and statement_list statements = array_of_list statement statements
  and statement = Statement.(function
  | loc, Empty -> node "EmptyStatement" loc [||]
  | loc, Block b -> block (loc, b)
  | loc, Expression expr ->
      node "ExpressionStatement" loc [|
        "expression", expression expr.Expression.expression;
      |]
  | loc, If _if -> If.(
      node "IfStatement" loc [|
        "test", expression _if.test;
        "consequent", statement _if.consequent;
        "alternate", option statement _if.alternate;
      |]
    )
  | loc, Labeled labeled -> Labeled.(
      node "LabeledStatement" loc [|
        "label", identifier labeled.label;
        "body", statement labeled.body;
      |]
    )
  | loc, Break break ->
      node "BreakStatement" loc [|
        "label", option identifier break.Break.label;
      |]
  | loc, Continue continue ->
      node "ContinueStatement" loc [|
        "label", option identifier continue.Continue.label;
      |]
  | loc, With _with -> With.(
      node "WithStatement" loc [|
        "object", expression _with._object;
        "body", statement _with.body;
      |]
    )
  | loc, TypeAlias alias -> type_alias (loc, alias)
  | loc, Switch switch -> Switch.(
      node "SwitchStatement" loc [|
        "discriminant", expression switch.discriminant;
        "cases", array_of_list case switch.cases;
        "lexical", bool switch.lexical;
      |]
    )
  | loc, Return return ->
      node "ReturnStatement" loc [|
        "argument", option expression return.Return.argument;
      |]
  | loc, Throw throw ->
      node "ThrowStatement" loc [|
        "argument", expression throw.Throw.argument;
      |]
  | loc, Try _try -> Try.(
      node "TryStatement" loc [|
        "block", block _try.block;
        "handler", option catch _try.handler;
        "guardedHandlers", array_of_list catch _try.guardedHandlers;
        "finalizer", option block _try.finalizer;
      |]
    )
  | loc, While _while -> While.(
      node "WhileStatement" loc [|
        "test", expression _while.test;
        "body", statement _while.body;
      |]
    )
  | loc, DoWhile dowhile -> DoWhile.(
      node "DoWhileStatement" loc [|
        "body", statement dowhile.body;
        "test", expression dowhile.test;
      |]
    )
  | loc, For _for -> For.(
      let init = function
      | InitDeclaration init -> variable_declaration init
      | InitExpression expr -> expression expr
      in
      node "ForStatement" loc [|
        "init", option init _for.init;
        "test", option expression _for.test;
        "update", option expression _for.update;
        "body", statement _for.body;
      |]
    )
  | loc, ForIn forin -> ForIn.(
      let left = (match forin.left with
      | LeftDeclaration left -> variable_declaration left
      | LeftExpression left -> expression left) in
      node "ForInStatement" loc [|
        "left", left;
        "right", expression forin.right;
        "body", statement forin.body;
        "each", bool forin.each;
      |]
    )
  | loc, ForOf forof -> ForOf.(
      let left = (match forof.left with
      | LeftDeclaration left -> variable_declaration left
      | LeftExpression left -> expression left) in
      node "ForOfStatement" loc [|
        "left", left;
        "right", expression forof.right;
        "body", statement forof.body;
      |]
    )
  | loc, Let _let -> Let.(
      node "LetStatement" loc [|
        "head", array_of_list let_assignment _let.head;
        "body", statement _let.body;
      |]
    )
  | loc, Debugger -> node "DebuggerStatement" loc [||]
  | loc, ClassDeclaration c -> class_declaration (loc, c)
  | loc, InterfaceDeclaration i -> interface_declaration (loc, i)
  | loc, VariableDeclaration var -> variable_declaration (loc, var)
  | loc, FunctionDeclaration fn ->  Function.(
      (* esprima/estree hasn't come around to the idea that function decls can
       * have optional ids :( *)
      let (node_type, node_value) = (
        match fn.id with
        | Some(id) -> "FunctionDeclaration", identifier id
        | None -> "FunctionExpression", null
      ) in

      let body =  (match fn.body with
      | BodyBlock b -> block b
      | BodyExpression b -> expression b) in

      node node_type loc [|
        "id", node_value;
        "params", array_of_list pattern fn.params;
        "defaults", array_of_list (option expression) fn.defaults;
        "rest", option identifier fn.rest;
        "body", body;
        "async", bool fn.async;
        "generator", bool fn.generator;
        "expression", bool fn.expression;
        "returnType", option type_annotation fn.returnType;
        "typeParameters", option type_parameter_declaration fn.typeParameters;
      |]
    )
    | loc, DeclareVariable d -> declare_variable (loc, d)
    | loc, DeclareFunction d -> declare_function (loc, d)
    | loc, DeclareClass d -> declare_class (loc, d)
    | loc, DeclareModule m -> DeclareModule.(
        let id = match m.id with
        | Literal lit -> literal lit
        | Identifier id -> identifier id
        in
        node "DeclareModule" loc [|
          "id", id;
          "body", block m.body;
          "kind", (
            match m.kind with
            | DeclareModule.CommonJS _ -> string "CommonJS"
            | DeclareModule.ES _ -> string "ES"
          )
        |]
      )
    | loc, DeclareExportDeclaration export -> DeclareExportDeclaration.(
        let declaration = match export.declaration with
        | Some (Variable v) -> declare_variable v
        | Some (Function f) -> declare_function f
        | Some (Class c) -> declare_class c
        | Some (DefaultType t) -> _type t
        | Some (NamedType t) -> type_alias t
        | Some (Interface i) -> interface_declaration i
        | None -> null
        in
        node "DeclareExportDeclaration" loc [|
          "default", bool export.default;
          "declaration", declaration;
          "specifiers", export_specifiers export.specifiers;
          "source", option literal export.source;
        |]
      )
    | loc, DeclareModuleExports annot ->
        node "DeclareModuleExports" loc [|
          "typeAnnotation", type_annotation annot
        |]
    | loc, ExportDeclaration export -> ExportDeclaration.(
        let declaration = match export.declaration with
        | Some (Declaration stmt) -> statement stmt
        | Some (ExportDeclaration.Expression expr) -> expression expr
        | None -> null
        in
        node "ExportDeclaration" loc [|
          "default", bool export.default;
          "declaration", declaration;
          "specifiers", export_specifiers export.specifiers;
          "source", option literal export.source;
          "exportKind", string (export_kind export.exportKind);
        |]
      )
    | loc, ImportDeclaration import -> ImportDeclaration.(
        let specifiers = import.specifiers |> List.map (function
          | ImportDefaultSpecifier id ->
              import_default_specifier id
          | ImportNamedSpecifier {local; remote;} ->
              import_named_specifier local remote
          | ImportNamespaceSpecifier id ->
              import_namespace_specifier id
        ) in

        let import_kind = match import.importKind with
        | ImportType -> "type"
        | ImportTypeof -> "typeof"
        | ImportValue -> "value"
        in

        node "ImportDeclaration" loc [|
          "specifiers", array (Array.of_list specifiers);
          "source", literal import.source;
          "importKind", string (import_kind);
        |]
    )
  )

  and expression = Expression.(function
    | loc, This -> node "ThisExpression" loc [||]
    | loc, Array arr ->
        node "ArrayExpression" loc [|
          "elements", array_of_list (option expression_or_spread) arr.Array.elements;
        |]
    | loc, Object _object ->
        node "ObjectExpression" loc [|
          "properties", array_of_list object_property _object.Object.properties;
        |]
    | loc, Function _function -> function_expression (loc, _function)
    | loc, ArrowFunction arrow -> Function.(
        let body = (match arrow.body with
        | BodyBlock b -> block b
        | BodyExpression expr -> expression expr)
        in
        node "ArrowFunctionExpression" loc [|
          "id", option identifier arrow.id;
          "params", array_of_list pattern arrow.params;
          "defaults", array_of_list (option expression) arrow.defaults;
          "rest", option identifier arrow.rest;
          "body", body;
          "async", bool arrow.async;
          "generator", bool arrow.generator;
          "expression", bool arrow.expression;
          "returnType", option type_annotation arrow.returnType;
          "typeParameters", option type_parameter_declaration arrow.typeParameters;
        |]
      )
    | loc, Sequence sequence ->
        node "SequenceExpression" loc [|
          "expressions", array_of_list expression sequence.Sequence.expressions;
        |]
    | loc, Unary unary -> Unary.(
        match unary.operator with
        | Await ->
          (* await is defined as a separate expression in ast-types
           *
           * TODO
           * 1) Send a PR to ast-types
           *    (https://github.com/benjamn/ast-types/issues/113)
           * 2) Output a UnaryExpression
           * 3) Modify the esprima test runner to compare AwaitExpression and
           *    our UnaryExpression
           * *)
          node "AwaitExpression" loc [|
            "argument", expression unary.argument;
          |]
        | _ -> begin
          let operator = match unary.operator with
          | Minus -> "-"
          | Plus -> "+"
          | Not -> "!"
          | BitNot -> "~"
          | Typeof -> "typeof"
          | Void -> "void"
          | Delete -> "delete"
          | Await -> failwith "matched above"
          in
          node "UnaryExpression" loc [|
            "operator", string operator;
            "prefix", bool unary.prefix;
            "argument", expression unary.argument;
          |]
        end
      )
    | loc, Binary binary -> Binary.(
        let operator = match binary.operator with
        | Equal -> "=="
        | NotEqual -> "!="
        | StrictEqual -> "==="
        | StrictNotEqual -> "!=="
        | LessThan -> "<"
        | LessThanEqual -> "<="
        | GreaterThan -> ">"
        | GreaterThanEqual -> ">="
        | LShift -> "<<"
        | RShift -> ">>"
        | RShift3 -> ">>>"
        | Plus -> "+"
        | Minus -> "-"
        | Mult -> "*"
        | Exp -> "**"
        | Div -> "/"
        | Mod -> "%"
        | BitOr -> "|"
        | Xor -> "^"
        | BitAnd -> "&"
        | In -> "in"
        | Instanceof -> "instanceof"
        in
        node "BinaryExpression" loc [|
          "operator", string operator;
          "left", expression binary.left;
          "right", expression binary.right;
        |]
      )
    | loc, TypeCast typecast -> TypeCast.(
        node "TypeCastExpression" loc [|
          "expression", expression typecast.expression;
          "typeAnnotation", type_annotation typecast.typeAnnotation;
        |]
      )
    | loc, Assignment assignment -> Assignment.(
        let operator = match assignment.operator with
        | Assign -> "="
        | PlusAssign -> "+="
        | MinusAssign -> "-="
        | MultAssign -> "*="
        | ExpAssign -> "**="
        | DivAssign -> "/="
        | ModAssign -> "%="
        | LShiftAssign -> "<<="
        | RShiftAssign -> ">>="
        | RShift3Assign -> ">>>="
        | BitOrAssign -> "|="
        | BitXorAssign -> "^="
        | BitAndAssign -> "&="
        in
        node "AssignmentExpression" loc [|
          "operator", string operator;
          "left", pattern assignment.left;
          "right", expression assignment.right;
        |]
      )
    | loc, Update update -> Update.(
        let operator = match update.operator with
        | Increment -> "++"
        | Decrement -> "--"
        in
        node "UpdateExpression" loc [|
          "operator", string operator;
          "argument", expression update.argument;
          "prefix", bool update.prefix;
        |]
      )
    | loc, Logical logical -> Logical.(
        let operator = match logical.operator with
        | Or -> "||"
        | And -> "&&"
        in
        node "LogicalExpression" loc [|
          "operator", string operator;
          "left", expression logical.left;
          "right", expression logical.right;
        |]
      )
    | loc, Conditional conditional -> Conditional.(
        node "ConditionalExpression" loc [|
          "test", expression conditional.test;
          "consequent", expression conditional.consequent;
          "alternate", expression conditional.alternate;
        |]
      )
    | loc, New _new -> New.(
        node "NewExpression" loc [|
          "callee", expression _new.callee;
          "arguments", array_of_list expression_or_spread _new.arguments;
        |]
      )
    | loc, Call call -> Call.(
        node "CallExpression" loc [|
          "callee", expression call.callee;
          "arguments", array_of_list expression_or_spread call.arguments;
        |]
      )
    | loc, Member member -> Member.(
        let property = match member.property with
        | PropertyIdentifier id -> identifier id
        | PropertyExpression expr -> expression expr
        in
        node "MemberExpression" loc [|
          "object", expression member._object;
          "property", property;
          "computed", bool member.computed;
        |]
      )
    | loc, Yield yield -> Yield.(
        node "YieldExpression" loc [|
          "argument", option expression yield.argument;
          "delegate", bool yield.delegate;
        |]
      )
    | loc, Comprehension comp -> Comprehension.(
        node "ComprehensionExpression" loc [|
          "blocks", array_of_list comprehension_block comp.blocks;
          "filter", option expression comp.filter;
        |]
      )
    | loc, Generator gen -> Generator.(
        node "GeneratorExpression" loc [|
          "blocks", array_of_list comprehension_block gen.blocks;
          "filter", option expression gen.filter;
        |]
      )
    | loc, Let _let -> Let.(
        node "LetExpression" loc [|
          "head", array_of_list let_assignment _let.head;
          "body", expression _let.body;
        |]
      )
    | _loc, Identifier id -> identifier id
    | loc, Literal lit -> literal (loc, lit)
    | loc, TemplateLiteral lit -> template_literal (loc, lit)
    | loc, TaggedTemplate tagged -> tagged_template (loc, tagged)
    | loc, Class c -> class_expression (loc, c)
    | loc, JSXElement element -> jsx_element (loc, element))

  and function_expression (loc, _function) = Function.(
    let body = match _function.body with
    | BodyBlock b -> block b
    | BodyExpression expr -> expression expr
    in
    node "FunctionExpression" loc [|
      "id", option identifier _function.id;
      "params", array_of_list pattern _function.params;
      "defaults", array_of_list (option expression) _function.defaults;
      "rest", option identifier _function.rest;
      "body", body;
      "async", bool _function.async;
      "generator", bool _function.generator;
      "expression", bool _function.expression;
      "returnType", option type_annotation _function.returnType;
      "typeParameters", option type_parameter_declaration _function.typeParameters;
    |]
  )

  and identifier (loc, id) = Identifier.(
    node "Identifier" loc [|
      "name", string id.name;
      "typeAnnotation", option type_annotation id.typeAnnotation;
      "optional", bool id.optional;
    |]
  )

  and case (loc, c) = Statement.Switch.Case.(
    node "SwitchCase" loc [|
      "test", option expression c.test;
      "consequent", array_of_list statement c.consequent;
    |]
  )

  and catch (loc, c) = Statement.Try.CatchClause.(
    node "CatchClause" loc [|
      "param", pattern c.param;
      "guard", option expression c.guard;
      "body", block c.body;
    |]
  )

  and block (loc, b) =
    node "BlockStatement" loc [|
      "body", statement_list b.Statement.Block.body;
    |]

  and let_assignment assignment = Statement.Let.(
    obj [|
      "id", pattern assignment.id;
      "init", option expression assignment.init;
    |]
  )

  and declare_variable (loc, d) = Statement.DeclareVariable.(
    node "DeclareVariable" loc [|
      "id", identifier d.id;
    |]
  )

  and declare_function (loc, d) = Statement.DeclareFunction.(
    node "DeclareFunction" loc [|
      "id", identifier d.id;
    |]
  )

  and declare_class (loc, d) = Statement.Interface.(
    node "DeclareClass" loc [|
      "id", identifier d.id;
      "typeParameters", option type_parameter_declaration d.typeParameters;
      "body", object_type d.body;
      "extends", array_of_list interface_extends d.extends;
    |]
  )

  and export_kind = Statement.ExportDeclaration.(function
    | ExportType -> "type"
    | ExportValue -> "value"
  )

  and export_specifiers = Statement.ExportDeclaration.(function
    | Some (ExportSpecifiers specifiers) ->
        array_of_list export_specifier specifiers
    | Some (ExportBatchSpecifier (loc, name)) ->
        array [|
          node "ExportBatchSpecifier" loc [|
            "name", option identifier name
          |]
        |]
    | None ->
        array [||]
  )

  and type_alias (loc, alias) =  Statement.TypeAlias.(
    node "TypeAlias" loc [|
      "id", identifier alias.id;
      "typeParameters", option type_parameter_declaration alias.typeParameters;
      "right", _type alias.right;
    |]
  )

  and class_declaration (loc, c) = Class.(
    (* esprima/estree hasn't come around to the idea that class decls can have
     * optional ids :( *)
    let (node_type, node_value) = (
      match c.id with
      | Some(id) -> "ClassDeclaration", identifier id
      | None -> "ClassExpression", null
    ) in

    node node_type loc [|
      "id", node_value;
      "body", class_body c.body;
      "superClass", option expression c.superClass;
      "typeParameters", option type_parameter_declaration c.typeParameters;
      "superTypeParameters", option type_parameter_instantiation c.superTypeParameters;
      "implements", array_of_list class_implements c.implements;
      "decorators", array_of_list expression c.classDecorators;
    |]
  )

  and class_expression (loc, c) = Class.(
    node "ClassExpression" loc [|
      "id", option identifier c.id;
      "body", class_body c.body;
      "superClass", option expression c.superClass;
      "typeParameters", option type_parameter_declaration c.typeParameters;
      "superTypeParameters", option type_parameter_instantiation c.superTypeParameters;
      "implements", array_of_list class_implements c.implements;
      "decorators", array_of_list expression c.classDecorators;
    |]
  )

  and class_implements (loc, implements) = Class.Implements.(
    node "ClassImplements" loc [|
      "id", identifier implements.id;
      "typeParameters", option type_parameter_instantiation implements.typeParameters;
    |]
  )

  and class_body (loc, body) = Class.Body.(
    node "ClassBody" loc [|
      "body", array_of_list class_element body.body;
    |]
  )

  and class_element = Class.Body.(function
    | Method m -> class_method m
    | Property p -> class_property p)

  and class_method (loc, method_) =
    let { Class.Method.key; value; kind; static; decorators; } = method_ in
    let key, computed = Expression.Object.Property.(match key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | Computed expr -> expression expr, true) in
    let kind = Class.Method.(match kind with
      | Constructor -> "constructor"
      | Method -> "method"
      | Get -> "get"
      | Set -> "set") in
    node "MethodDefinition" loc [|
      "key", key;
      "value", function_expression value;
      "kind", string kind;
      "static", bool static;
      "computed", bool computed;
      "decorators", array_of_list expression decorators;
    |]

  and class_property (loc, prop) = Class.Property.(
    let key, computed = (match prop.key with
    | Expression.Object.Property.Literal lit -> literal lit, false
    | Expression.Object.Property.Identifier id -> identifier id, false
    | Expression.Object.Property.Computed expr -> expression expr, true) in
    node "ClassProperty" loc [|
      "key", key;
      "value", option expression prop.value;
      "typeAnnotation", option type_annotation prop.typeAnnotation;
      "computed", bool computed;
      "static", bool prop.static;
    |]
  )

  and interface_declaration (loc, i) = Statement.Interface.(
    node "InterfaceDeclaration" loc [|
      "id", identifier i.id;
      "typeParameters", option type_parameter_declaration i.typeParameters;
      "body", object_type i.body;
      "extends", array_of_list interface_extends i.extends;
    |]
  )

  and interface_extends (loc, g) = Type.Generic.(
    let id = match g.id with
    | Identifier.Unqualified id -> identifier id
    | Identifier.Qualified q -> generic_type_qualified_identifier q
    in
    node "InterfaceExtends" loc [|
      "id", id;
      "typeParameters", option type_parameter_instantiation g.typeParameters;
    |]
  )

  and pattern = Pattern.(function
    | loc, Object obj ->
        node "ObjectPattern" loc [|
          "properties", array_of_list object_pattern_property obj.Object.properties;
          "typeAnnotation", option type_annotation obj.Object.typeAnnotation;
        |]
    | loc, Array arr ->
        node "ArrayPattern" loc [|
          "elements", array_of_list (option array_pattern_element) arr.Array.elements;
          "typeAnnotation", option type_annotation arr.Array.typeAnnotation;
        |]
    | loc, Assignment { Assignment.left; right } ->
        node "AssignmentPattern" loc [|
          "left", pattern left;
          "right", expression right
        |]
    | _loc, Identifier id -> identifier id
    | _loc, Expression expr -> expression expr)

  and array_pattern_element = Pattern.Array.(function
    | Element p -> pattern p
    | Spread (loc, { SpreadElement.argument; }) ->
        node "SpreadElementPattern" loc [|
          "argument", pattern argument;
        |]
  )

  and object_property = Expression.Object.(function
    | Property (loc, prop) -> Property.(
      (* This is a slight deviation from the Mozilla spec. In the spec, an object
        * property is not a proper node, and lacks a location and a "type" field.
        * Esprima promotes it to a proper node and that is useful, so I'm
        * following their example *)
      let key, computed = (match prop.key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | Computed expr -> expression expr, true)  in
      let kind = match prop.kind with
      | Init -> "init"
      | Get -> "get"
      | Set -> "set"
      in
      node "Property" loc [|
        "key", key;
        "value", expression prop.value;
        "kind", string kind;
        "method", bool prop._method;
        "shorthand", bool prop.shorthand;
        "computed", bool computed;
      |]
    )
  | SpreadProperty(loc, prop) -> SpreadProperty.(
    node "SpreadProperty" loc [|
      "argument", expression prop.argument;
    |]
  ))

  and object_pattern_property = Pattern.Object.(function
    | Property (loc, prop) -> Property.(
      let key, computed = (match prop.key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | Computed expr -> expression expr, true) in
      node "PropertyPattern" loc [|
        "key", key;
        "pattern", pattern prop.pattern;
        "computed", bool computed;
        "shorthand", bool prop.shorthand;
      |]
    )
    | SpreadProperty (loc, prop) -> SpreadProperty.(
      node "SpreadPropertyPattern" loc [|
        "argument", pattern prop.argument;
      |]
    )
  )

  and expression_or_spread = Expression.(function
    | Expression expr -> expression expr
    | Spread (loc, { SpreadElement.argument; }) ->
        node "SpreadElement" loc [|
          "argument", expression argument;
        |]
  )

  and comprehension_block (loc, b) = Expression.Comprehension.Block.(
    node "ComprehensionBlock" loc [|
      "left", pattern b.left;
      "right", expression b.right;
      "each", bool b.each;
    |]
  )

  and literal (loc, lit) = Literal.(
    let { value; raw; } = lit in
    let value_ = match value with
    | String str -> string str
    | Boolean b -> bool b
    | Null -> null
    | Number f -> number f
    | RegExp { RegExp.pattern; flags; } -> regexp loc pattern flags
    in
    let props = match value with
    | RegExp { RegExp.pattern; flags; } ->
        let regex = obj [|
          "pattern", string pattern;
          "flags", string flags;
        |] in
        [| "value", value_; "raw", string raw; "regex", regex |]
    | _ ->
        [| "value", value_; "raw", string raw; |]
    in
    node "Literal" loc props
  )

  and template_literal (loc, value) = Expression.TemplateLiteral.(
    node "TemplateLiteral" loc [|
      "quasis", array_of_list template_element value.quasis;
      "expressions", array_of_list expression value.expressions;
    |]
  )

  and template_element (loc, element) = Expression.TemplateLiteral.Element.(
    let value = obj [|
      "raw", string element.value.raw;
      "cooked", string element.value.cooked;
    |] in
    node "TemplateElement" loc [|
      "value", value;
      "tail", bool element.tail;
    |]
  )

  and tagged_template (loc, tagged) = Expression.TaggedTemplate.(
    node "TaggedTemplateExpression" loc [|
      "tag", expression tagged.tag;
      "quasi", template_literal tagged.quasi;
    |]
  )

  and variable_declaration (loc, var) = Statement.VariableDeclaration.(
    let kind = match var.kind with
    | Var -> "var"
    | Let -> "let"
    | Const -> "const"
    in
    node "VariableDeclaration" loc [|
      "declarations", array_of_list variable_declarator var.declarations;
      "kind", string kind;
    |]
  )

  and variable_declarator (loc, declarator) =
    Statement.VariableDeclaration.Declarator.(
      node "VariableDeclarator" loc [|
        "id", pattern declarator.id;
        "init", option expression declarator.init;
      |]
    )

  and _type (loc, t) = Type.(
    match t with
    | Any -> any_type loc
    | Void -> void_type loc
    | Null -> null_type loc
    | Number -> number_type loc
    | String -> string_type loc
    | Boolean -> boolean_type loc
    | Nullable t -> nullable_type loc t
    | Function fn -> function_type (loc, fn)
    | Object o -> object_type (loc, o)
    | Array t -> array_type loc t
    | Generic g -> generic_type (loc, g)
    | Union u -> union_type (loc, u)
    | Intersection i -> intersection_type (loc, i)
    | Typeof t -> typeof_type (loc, t)
    | Tuple t -> tuple_type (loc, t)
    | StringLiteral s -> string_literal_type (loc, s)
    | NumberLiteral n -> number_literal_type (loc, n)
    | BooleanLiteral b -> boolean_literal_type (loc, b)
    | Exists -> exists_type loc
  )

  and any_type loc = node "AnyTypeAnnotation" loc [||]

  and void_type loc = node "VoidTypeAnnotation" loc [||]

  and null_type loc = node "NullTypeAnnotation" loc [||]

  and number_type loc = node "NumberTypeAnnotation" loc [||]

  and string_type loc = node "StringTypeAnnotation" loc [||]

  and boolean_type loc = node "BooleanTypeAnnotation" loc [||]

  and nullable_type loc t =
    node "NullableTypeAnnotation" loc [|
      "typeAnnotation", _type t;
    |]

  and function_type (loc, fn) = Type.Function.(
    node "FunctionTypeAnnotation" loc [|
      "params", array_of_list function_type_param fn.params;
      "returnType", _type fn.returnType;
      "rest", option function_type_param fn.rest;
      "typeParameters", option type_parameter_declaration fn.typeParameters;
    |]
  )

  and function_type_param (loc, param) = Type.Function.Param.(
    node "FunctionTypeParam" loc [|
      "name", identifier param.name;
      "typeAnnotation", _type param.typeAnnotation;
      "optional", bool param.optional;
    |]
  )

  and object_type (loc, o) = Type.Object.(
    node "ObjectTypeAnnotation" loc [|
      "properties", array_of_list object_type_property o.properties;
      "indexers", array_of_list object_type_indexer o.indexers;
      "callProperties", array_of_list object_type_call_property o.callProperties;
    |]
  )

  and object_type_property (loc, prop) = Type.Object.Property.(
    let key = match prop.key with
    | Expression.Object.Property.Literal lit -> literal lit
    | Expression.Object.Property.Identifier id -> identifier id
    | Expression.Object.Property.Computed _ ->
      failwith "There should not be computed object type property keys"
    in
    node "ObjectTypeProperty" loc [|
      "key", key;
      "value", _type prop.value;
      "optional", bool prop.optional;
      "static", bool prop.static;
    |]
  )

  and object_type_indexer (loc, indexer) = Type.Object.Indexer.(
    node "ObjectTypeIndexer" loc [|
      "id", identifier indexer.id;
      "key", _type indexer.key;
      "value", _type indexer.value;
      "static", bool indexer.static;
    |]
  )

  and object_type_call_property (loc, callProperty) = Type.Object.CallProperty.(
    node "ObjectTypeCallProperty" loc [|
      "value", function_type callProperty.value;
      "static", bool callProperty.static;
    |]
  )

  and array_type loc t =
    node "ArrayTypeAnnotation" loc [|
      "elementType", (_type t);
    |]

  and generic_type_qualified_identifier (loc, q) = Type.Generic.Identifier.(
    let qualification = match q.qualification with
    | Unqualified id -> identifier id
    | Qualified q -> generic_type_qualified_identifier q
    in
    node "QualifiedTypeIdentifier" loc [|
      "qualification", qualification;
      "id", identifier q.id;
    |]
  )

  and generic_type (loc, g) = Type.Generic.(
    let id = match g.id with
    | Identifier.Unqualified id -> identifier id
    | Identifier.Qualified q -> generic_type_qualified_identifier q
    in
    node "GenericTypeAnnotation" loc [|
      "id", id;
      "typeParameters", option type_parameter_instantiation g.typeParameters;
    |]
  )

  and union_type (loc, tl) =
    node "UnionTypeAnnotation" loc [|
      "types", array_of_list _type tl;
    |]

  and intersection_type (loc, tl) =
    node "IntersectionTypeAnnotation" loc [|
      "types", array_of_list _type tl;
    |]

  and typeof_type (loc, t) =
    node "TypeofTypeAnnotation" loc [|
      "argument", _type t;
    |]

  and tuple_type (loc, tl) =
    node "TupleTypeAnnotation" loc [|
      "types", array_of_list _type tl;
    |]

  and string_literal_type (loc, s) = Type.StringLiteral.(
    node "StringLiteralTypeAnnotation" loc [|
      "value", string s.value;
      "raw", string s.raw;
    |]
  )

  and number_literal_type (loc, s) = Type.NumberLiteral.(
    node "NumberLiteralTypeAnnotation" loc [|
      "value", number s.value;
      "raw", string s.raw;
    |]
  )

  and boolean_literal_type (loc, s) = Type.BooleanLiteral.(
    node "BooleanLiteralTypeAnnotation" loc [|
      "value", bool s.value;
      "raw", string s.raw;
    |]
  )

  and exists_type loc = node "ExistsTypeAnnotation" loc [||]

  and type_annotation (loc, ty) =
    node "TypeAnnotation" loc [|
      "typeAnnotation", _type ty;
    |]

  and type_parameter_declaration (loc, params) = Type.ParameterDeclaration.(
    node "TypeParameterDeclaration" loc [|
      "params", array_of_list type_param params.params;
    |]
  )

  and type_param (loc, tp) = Type.ParameterDeclaration.TypeParam.(
    let variance = Variance.(function
      | Plus -> string "plus"
      | Minus -> string "minus"
    ) in
    node "TypeParameter" loc [|
      "name", string tp.name;
      "bound", option type_annotation tp.bound;
      "variance", option variance tp.variance;
      "default", option _type tp.default;
    |]
  )

  and type_parameter_instantiation (loc, params) = Type.ParameterInstantiation.(
    node "TypeParameterInstantiation" loc [|
      "params", array_of_list _type params.params;
    |]
  )

  and jsx_element (loc, (element: JSX.element)) = JSX.(
    node "JSXElement" loc [|
      "openingElement", jsx_opening element.openingElement;
      "closingElement", option jsx_closing element.closingElement;
      "children", array_of_list jsx_child element.children;
    |]
  )

  and jsx_opening (loc, opening) = JSX.Opening.(
    node "JSXOpeningElement" loc [|
      "name", jsx_name opening.name;
      "attributes", array_of_list jsx_opening_attribute opening.attributes;
      "selfClosing", bool opening.selfClosing;
    |]
  )

  and jsx_opening_attribute = JSX.Opening.(function
    | Attribute attribute -> jsx_attribute attribute
    | SpreadAttribute attribute -> jsx_spread_attribute attribute
  )

  and jsx_closing (loc, closing) = JSX.Closing.(
    node "JSXClosingElement" loc [|
      "name", jsx_name closing.name;
    |]
  )

  and jsx_child = JSX.(function
    | loc, Element element -> jsx_element (loc, element)
    | loc, ExpressionContainer expr -> jsx_expression_container (loc, expr)
    | loc, Text str -> jsx_text (loc, str)
  )

  and jsx_name = JSX.(function
    | Identifier id -> jsx_identifier id
    | NamespacedName namespaced_name -> jsx_namespaced_name namespaced_name
    | MemberExpression member -> jsx_member_expression member
  )

  and jsx_attribute (loc, attribute) = JSX.Attribute.(
    let name = match attribute.name with
    | Identifier id -> jsx_identifier id
    | NamespacedName namespaced_name -> jsx_namespaced_name namespaced_name
    in
    node "JSXAttribute" loc [|
      "name", name;
      "value", option jsx_attribute_value attribute.value;
    |]
  )

  and jsx_attribute_value = JSX.Attribute.(function
    | Literal (loc, value) -> literal (loc, value)
    | ExpressionContainer (loc, expr) -> jsx_expression_container (loc, expr)
  )

  and jsx_spread_attribute (loc, attribute) = JSX.SpreadAttribute.(
    node "JSXSpreadAttribute" loc [|
      "argument", expression attribute.argument;
    |]
  )

  and jsx_expression_container (loc, expr) = JSX.ExpressionContainer.(
    let expression = match expr.expression with
    | Expression expr -> expression expr
    | EmptyExpression empty_loc ->
        node "JSXEmptyExpression" empty_loc [||]
    in
    node "JSXExpressionContainer" loc [|
      "expression", expression;
    |]
  )

  and jsx_text (loc, text) = JSX.Text.(
    let { value; raw; } = text in
    node "JSXText" loc [|
      "value", string value;
      "raw", string raw;
    |]
  )

  and jsx_member_expression (loc, member_expression) = JSX.MemberExpression.(
    let _object = match member_expression._object with
    | Identifier id -> jsx_identifier id
    | MemberExpression member -> jsx_member_expression member in
    node "JSXMemberExpression" loc [|
      "object", _object;
      "property", jsx_identifier member_expression.property;
    |]
  )

  and jsx_namespaced_name (loc, namespaced_name) = JSX.NamespacedName.(
    node "JSXNamespacedName" loc [|
      "namespace", jsx_identifier namespaced_name.namespace;
      "name", jsx_identifier namespaced_name.name;
    |]
  )

  and jsx_identifier (loc, id) = JSX.Identifier.(
    node "JSXIdentifier" loc [|
      "name", string id.name;
    |]
  )

  and export_specifier (loc, specifier) = Statement.ExportDeclaration.Specifier.(
    node "ExportSpecifier" loc [|
      "id", identifier specifier.id;
      "name", option identifier specifier.name;
    |]
  )

  and import_default_specifier id =
    node "ImportDefaultSpecifier" (fst id) [|
      "id", identifier id;
    |]

  and import_namespace_specifier (loc, id) =
    node "ImportNamespaceSpecifier" loc [|
      "id", identifier id;
    |]

  and import_named_specifier local_id remote_id =
    let span_loc =
      match local_id with
      | Some local_id -> Loc.btwn (fst remote_id) (fst local_id)
      | None -> fst remote_id
    in
    node "ImportSpecifier" span_loc [|
      "id", identifier remote_id;
      "name", option identifier local_id;
    |]

  and comment_list comments = array_of_list comment comments

  and comment (loc, c) = Comment.(
    let _type, value = match c with
      | Line s -> "Line", s
      | Block s -> "Block", s in
    node _type loc [|
      "value", string value;
    |]
  )
end

end
module Lexer_flow
= struct
#1 "lexer_flow.ml"
# 11 "lexer_flow.mll"

module Ast = Spider_monkey_ast

module Token = struct
  type t =
    | T_NUMBER of number_type
    | T_STRING of (Loc.t * string * string * bool) (* loc, value, raw, octal *)
    | T_TEMPLATE_PART of (Loc.t * template_part * bool) (* loc, value, is_tail *)
    | T_IDENTIFIER
    | T_REGEXP of (Loc.t * string * string) (* /pattern/flags *)
    (* Syntax *)
    | T_LCURLY
    | T_RCURLY
    | T_LPAREN
    | T_RPAREN
    | T_LBRACKET
    | T_RBRACKET
    | T_SEMICOLON
    | T_COMMA
    | T_PERIOD
    | T_ARROW
    | T_ELLIPSIS
    | T_AT
    (* Keywords *)
    | T_FUNCTION
    | T_IF
    | T_IN
    | T_INSTANCEOF
    | T_RETURN
    | T_SWITCH
    | T_THIS
    | T_THROW
    | T_TRY
    | T_VAR
    | T_WHILE
    | T_WITH
    | T_CONST
    | T_LET
    | T_NULL
    | T_FALSE
    | T_TRUE
    | T_BREAK
    | T_CASE
    | T_CATCH
    | T_CONTINUE
    | T_DEFAULT
    | T_DO
    | T_FINALLY
    | T_FOR
    | T_CLASS
    | T_EXTENDS
    | T_STATIC
    | T_ELSE
    | T_NEW
    | T_DELETE
    | T_TYPEOF
    | T_VOID
    | T_ENUM
    | T_EXPORT
    | T_IMPORT
    | T_SUPER
    | T_IMPLEMENTS
    | T_INTERFACE
    | T_PACKAGE
    | T_PRIVATE
    | T_PROTECTED
    | T_PUBLIC
    | T_YIELD
    | T_DEBUGGER
    | T_DECLARE
    | T_TYPE
    | T_OF
    | T_ASYNC
    | T_AWAIT
    (* Operators *)
    | T_RSHIFT3_ASSIGN
    | T_RSHIFT_ASSIGN
    | T_LSHIFT_ASSIGN
    | T_BIT_XOR_ASSIGN
    | T_BIT_OR_ASSIGN
    | T_BIT_AND_ASSIGN
    | T_MOD_ASSIGN
    | T_DIV_ASSIGN
    | T_MULT_ASSIGN
    | T_EXP_ASSIGN
    | T_MINUS_ASSIGN
    | T_PLUS_ASSIGN
    | T_ASSIGN
    | T_PLING
    | T_COLON
    | T_OR
    | T_AND
    | T_BIT_OR
    | T_BIT_XOR
    | T_BIT_AND
    | T_EQUAL
    | T_NOT_EQUAL
    | T_STRICT_EQUAL
    | T_STRICT_NOT_EQUAL
    | T_LESS_THAN_EQUAL
    | T_GREATER_THAN_EQUAL
    | T_LESS_THAN
    | T_GREATER_THAN
    | T_LSHIFT
    | T_RSHIFT
    | T_RSHIFT3
    | T_PLUS
    | T_MINUS
    | T_DIV
    | T_MULT
    | T_EXP
    | T_MOD
    | T_NOT
    | T_BIT_NOT
    | T_INCR
    | T_DECR
    (* Extra tokens *)
    | T_ERROR
    | T_EOF
    (* JSX *)
    | T_JSX_IDENTIFIER
    | T_JSX_TEXT of (Loc.t * string * string) (* loc, value, raw *)
    (* Type primitives *)
    | T_ANY_TYPE
    | T_BOOLEAN_TYPE
    | T_NUMBER_TYPE
    | T_NUMBER_SINGLETON_TYPE of number_type * float
    | T_STRING_TYPE
    | T_VOID_TYPE

  and number_type =
    | BINARY
    | LEGACY_OCTAL
    | OCTAL
    | NORMAL

  and template_part = {
    cooked: string; (* string after processing special chars *)
    raw: string; (* string as specified in source *)
    literal: string; (* same as raw, plus characters like ` and ${ *)
  }

(*****************************************************************************)
(* Pretty printer (pretty?) *)
(*****************************************************************************)
  let token_to_string = function
    | T_NUMBER _ -> "T_NUMBER"
    | T_STRING _ -> "T_STRING"
    | T_TEMPLATE_PART _ -> "T_TEMPLATE_PART"
    | T_IDENTIFIER -> "T_IDENTIFIER"
    | T_REGEXP _ -> "T_REGEXP"
    | T_FUNCTION -> "T_FUNCTION"
    | T_IF -> "T_IF"
    | T_IN -> "T_IN"
    | T_INSTANCEOF -> "T_INSTANCEOF"
    | T_RETURN -> "T_RETURN"
    | T_SWITCH -> "T_SWITCH"
    | T_THIS -> "T_THIS"
    | T_THROW -> "T_THROW"
    | T_TRY -> "T_TRY"
    | T_VAR -> "T_VAR"
    | T_WHILE -> "T_WHILE"
    | T_WITH -> "T_WITH"
    | T_CONST -> "T_CONST"
    | T_LET  -> "T_LET"
    | T_NULL -> "T_NULL"
    | T_FALSE -> "T_FALSE"
    | T_TRUE -> "T_TRUE"
    | T_BREAK -> "T_BREAK"
    | T_CASE -> "T_CASE"
    | T_CATCH -> "T_CATCH"
    | T_CONTINUE -> "T_CONTINUE"
    | T_DEFAULT -> "T_DEFAULT"
    | T_DO -> "T_DO"
    | T_FINALLY -> "T_FINALLY"
    | T_FOR -> "T_FOR"
    | T_CLASS -> "T_CLASS"
    | T_EXTENDS -> "T_EXTENDS"
    | T_STATIC -> "T_STATIC"
    | T_ELSE -> "T_ELSE"
    | T_NEW -> "T_NEW"
    | T_DELETE -> "T_DELETE"
    | T_TYPEOF -> "T_TYPEOF"
    | T_VOID -> "T_VOID"
    | T_ENUM -> "T_ENUM"
    | T_EXPORT  -> "T_EXPORT"
    | T_IMPORT -> "T_IMPORT"
    | T_SUPER  -> "T_SUPER"
    | T_IMPLEMENTS -> "T_IMPLEMENTS"
    | T_INTERFACE -> "T_INTERFACE"
    | T_PACKAGE -> "T_PACKAGE"
    | T_PRIVATE -> "T_PRIVATE"
    | T_PROTECTED -> "T_PROTECTED"
    | T_PUBLIC -> "T_PUBLIC"
    | T_YIELD -> "T_YIELD"
    | T_DEBUGGER -> "T_DEBUGGER"
    | T_DECLARE -> "T_DECLARE"
    | T_TYPE -> "T_TYPE"
    | T_OF -> "T_OF"
    | T_ASYNC -> "T_ASYNC"
    | T_AWAIT -> "T_AWAIT"
    | T_LCURLY -> "T_LCURLY"
    | T_RCURLY -> "T_RCURLY"
    | T_LPAREN -> "T_LPAREN"
    | T_RPAREN -> "T_RPAREN"
    | T_LBRACKET -> "T_LBRACKET"
    | T_RBRACKET -> "T_RBRACKET"
    | T_SEMICOLON -> "T_SEMICOLON"
    | T_COMMA -> "T_COMMA"
    | T_PERIOD -> "T_PERIOD"
    | T_ARROW -> "T_ARROW"
    | T_ELLIPSIS -> "T_ELLIPSIS"
    | T_AT -> "T_AT"
    | T_RSHIFT3_ASSIGN -> "T_RSHIFT3_ASSIGN"
    | T_RSHIFT_ASSIGN -> "T_RSHIFT_ASSIGN"
    | T_LSHIFT_ASSIGN -> "T_LSHIFT_ASSIGN"
    | T_BIT_XOR_ASSIGN -> "T_BIT_XOR_ASSIGN"
    | T_BIT_OR_ASSIGN -> "T_BIT_OR_ASSIGN"
    | T_BIT_AND_ASSIGN -> "T_BIT_AND_ASSIGN"
    | T_MOD_ASSIGN -> "T_MOD_ASSIGN"
    | T_DIV_ASSIGN -> "T_DIV_ASSIGN"
    | T_MULT_ASSIGN -> "T_MULT_ASSIGN"
    | T_EXP_ASSIGN -> "T_EXP_ASSIGN"
    | T_MINUS_ASSIGN -> "T_MINUS_ASSIGN"
    | T_PLUS_ASSIGN -> "T_PLUS_ASSIGN"
    | T_ASSIGN -> "T_ASSIGN"
    | T_PLING -> "T_PLING"
    | T_COLON -> "T_COLON"
    | T_OR -> "T_OR"
    | T_AND -> "T_AND"
    | T_BIT_OR -> "T_BIT_OR"
    | T_BIT_XOR -> "T_BIT_XOR"
    | T_BIT_AND -> "T_BIT_AND"
    | T_EQUAL -> "T_EQUAL"
    | T_NOT_EQUAL -> "T_NOT_EQUAL"
    | T_STRICT_EQUAL -> "T_STRICT_EQUAL"
    | T_STRICT_NOT_EQUAL -> "T_STRICT_NOT_EQUAL"
    | T_LESS_THAN_EQUAL -> "T_LESS_THAN_EQUAL"
    | T_GREATER_THAN_EQUAL -> "T_GREATER_THAN_EQUAL"
    | T_LESS_THAN -> "T_LESS_THAN"
    | T_GREATER_THAN -> "T_GREATER_THAN"
    | T_LSHIFT -> "T_LSHIFT"
    | T_RSHIFT -> "T_RSHIFT"
    | T_RSHIFT3 -> "T_RSHIFT3"
    | T_PLUS -> "T_PLUS"
    | T_MINUS -> "T_MINUS"
    | T_DIV -> "T_DIV"
    | T_MULT -> "T_MULT"
    | T_EXP -> "T_EXP"
    | T_MOD -> "T_MOD"
    | T_NOT -> "T_NOT"
    | T_BIT_NOT -> "T_BIT_NOT"
    | T_INCR -> "T_INCR"
    | T_DECR -> "T_DECR"
    (* Extra tokens *)
    | T_ERROR -> "T_ERROR"
    | T_EOF -> "T_EOF"
    | T_JSX_IDENTIFIER -> "T_JSX_IDENTIFIER"
    | T_JSX_TEXT _ -> "T_JSX_TEXT"
    (* Type primitives *)
    | T_ANY_TYPE -> "T_ANY_TYPE"
    | T_BOOLEAN_TYPE -> "T_BOOLEAN_TYPE"
    | T_NUMBER_TYPE -> "T_NUMBER_TYPE"
    | T_NUMBER_SINGLETON_TYPE _ -> "T_NUMBER_SINGLETON_TYPE"
    | T_STRING_TYPE -> "T_STRING_TYPE"
    | T_VOID_TYPE -> "T_VOID_TYPE"
end
open Token

(*****************************************************************************)
(* Backtracking. *)
(*****************************************************************************)
  let yyback n lexbuf =
    Lexing.(
      lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - n;
      let currp = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { currp with pos_cnum = currp.pos_cnum - n }
    )

  let back lb =
    let n = Lexing.lexeme_end lb - Lexing.lexeme_start lb in
    yyback n lb

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

let debug_string_of_lexing_position position =
  Printf.sprintf
    "{pos_fname=%S; pos_lnum=%d; pos_bol=%d; pos_cnum=%d}"
    position.Lexing.pos_fname
    position.Lexing.pos_lnum
    position.Lexing.pos_bol
    position.Lexing.pos_cnum

let debug_string_of_lexbuf (lb: Lexing.lexbuf) =
  Printf.sprintf
    "{ \
      lex_buffer = %S; \
      lex_buffer_len = %d; \
      lex_abs_pos = %d; \
      lex_start_pos = %d; \
      lex_curr_pos = %d; \
      lex_last_pos = %d; \
      lex_last_action = %d; \
      lex_eof_reached = %b; \
      lex_mem = TODO; \
      lex_start_p = %s; \
      lex_curr_p = %s; \
    }"
    (Bytes.to_string lb.Lexing.lex_buffer)
    lb.Lexing.lex_buffer_len
    lb.Lexing.lex_abs_pos
    lb.Lexing.lex_start_pos
    lb.Lexing.lex_curr_pos
    lb.Lexing.lex_last_pos
    lb.Lexing.lex_last_action
    lb.Lexing.lex_eof_reached
    (debug_string_of_lexing_position lb.Lexing.lex_start_p)
    (debug_string_of_lexing_position lb.Lexing.lex_curr_p)

module Lex_env = struct
  type t = {
    lex_source            : Loc.filename option;
    lex_lb                : Lexing.lexbuf;
    lex_in_comment_syntax : bool;
    lex_enable_comment_syntax: bool;
    lex_state             : lex_state;
  }

  and lex_state = {
    lex_errors_acc: (Loc.t * Parse_error.t) list;
    lex_comments_acc: Ast.Comment.t list;
  }

  let empty_lex_state = {
    lex_errors_acc = [];
    lex_comments_acc = [];
  }

  let new_lex_env lex_source lex_lb ~enable_types_in_comments = {
    lex_source;
    lex_lb;
    lex_in_comment_syntax = false;
    lex_enable_comment_syntax = enable_types_in_comments;
    lex_state = empty_lex_state;
  }

  let get_and_clear_state env =
    let state = env.lex_state in
    let env = if state != empty_lex_state
      then { env with lex_state = empty_lex_state }
      else env
    in
    env, state

  let lexbuf env = env.lex_lb
  let with_lexbuf ~lexbuf env = { env with lex_lb = lexbuf }
  let source env = env.lex_source
  let state env = env.lex_state
  let is_in_comment_syntax env = env.lex_in_comment_syntax
  let is_comment_syntax_enabled env = env.lex_enable_comment_syntax
  let in_comment_syntax is_in env =
    if is_in <> env.lex_in_comment_syntax
    then { env with lex_in_comment_syntax = is_in }
    else env

  let debug_string_of_lex_env (env: t) =
    let source = match (source env) with
      | None -> "None"
      | Some x -> Printf.sprintf "Some %S" (Loc.string_of_filename x)
    in
    Printf.sprintf
      "{\n  \
        lex_source = %s\n  \
        lex_lb = %s\n  \
        lex_in_comment_syntax = %b\n  \
        lex_enable_comment_syntax = %b\n  \
        lex_state = {errors = (count = %d); comments = (count = %d)}\n\
      }"
      source
      (debug_string_of_lexbuf env.lex_lb)
      (is_in_comment_syntax env)
      (is_comment_syntax_enabled env)
      (List.length (state env).lex_errors_acc)
      (List.length (state env).lex_comments_acc)
end
open Lex_env

module Lex_result = struct
  type t = {
    lex_token: Token.t;
    lex_loc: Loc.t;
    lex_value: string;
    lex_errors: (Loc.t * Parse_error.t) list;
    lex_comments: Ast.Comment.t list;
  }

  let token result = result.lex_token
  let loc result = result.lex_loc
  let value result = result.lex_value
  let comments result = result.lex_comments
  let errors result = result.lex_errors

  let debug_string_of_lex_result lex_result =
    Printf.sprintf
      "{\n  \
        lex_token = %s\n  \
        lex_value = %S\n  \
        lex_errors = (length = %d)\n  \
        lex_comments = (length = %d)\n\
      }"
    (token_to_string lex_result.lex_token)
    lex_result.lex_value
    (List.length lex_result.lex_errors)
    (List.length lex_result.lex_comments)
end

  let loc_of_lexbuf env lexbuf = Loc.from_lb (source env) lexbuf

  let get_result_and_clear_state (env, lex_token) =
    let env, state = get_and_clear_state env in
    let (lex_loc, lex_value) = match lex_token with
    | T_STRING (loc, _, raw, _) ->
        loc, raw
    | T_JSX_TEXT (loc, _, raw) -> loc, raw
    | T_TEMPLATE_PART (loc, {literal; _}, _) ->
        loc, literal
    | T_REGEXP (loc, pattern, flags) -> loc, "/" ^ pattern ^ "/" ^ flags
    | _ -> loc_of_lexbuf env env.lex_lb, Lexing.lexeme env.lex_lb in
    env, {
      Lex_result.lex_token;
      lex_loc;
      lex_value;
      lex_errors = List.rev state.lex_errors_acc;
      lex_comments = List.rev state.lex_comments_acc;
    }

  let lex_error (env: Lex_env.t) loc err: Lex_env.t =
    let lex_errors_acc = (loc, err)::env.lex_state.lex_errors_acc in
    { env with lex_state = { env.lex_state with lex_errors_acc; } }

  let unexpected_error env loc value =
    lex_error env loc (Parse_error.UnexpectedToken value)

  let unexpected_error_w_suggest (env: Lex_env.t) (loc: Loc.t) value suggest =
    lex_error env loc (Parse_error.UnexpectedTokenWithSuggestion (value, suggest))

  let illegal (env: Lex_env.t) (loc: Loc.t) =
    lex_error env loc (Parse_error.UnexpectedToken "ILLEGAL")

  let illegal_number (env: Lex_env.t) lexbuf word token =
    let loc = loc_of_lexbuf env lexbuf in
    yyback (String.length word) lexbuf;
    let env = illegal env loc in
    env, token

module FloatOfString : sig
  val float_of_string: string -> float
end = struct
  type t = {
    negative: bool;
    mantissa: int;
    exponent: int;
    decimal_exponent: int option;
    todo: char list;
  }

  exception No_good

  let eat f =
    match f.todo with
    | _::todo -> { f with todo; }
    | _ -> raise No_good

  let start str =
    let todo = ref [] in
    String.iter (fun c -> todo := c::(!todo)) str;
    {
      negative = false;
      mantissa = 0;
      exponent = 0;
      decimal_exponent = None;
      todo = List.rev (!todo);
    }

  let parse_sign f =
    match f.todo with
    | '+'::_ -> eat f
    | '-'::_ -> { (eat f) with negative = true; }
    | _ -> f

  let parse_hex_symbol f =
    match f.todo with
    | '0'::('x' | 'X')::_ -> f |> eat |> eat
    | _ -> raise No_good

  let parse_exponent f =
    let todo_str = f.todo
      |> List.map Char.escaped
      |> String.concat "" in
    let exponent =
      try int_of_string todo_str
      with Failure _ -> raise No_good in
    { f with exponent; todo = [] }

  let rec parse_body f =
    match f.todo with
    | [] -> f
    (* _ is just ignored *)
    | '_'::_ -> parse_body (eat f)
    | '.'::_ ->
        if f.decimal_exponent = None
        then parse_body { (eat f) with decimal_exponent = Some 0 }
        else raise No_good
    | ('p' | 'P')::_ ->
        parse_exponent (eat f)
    | c::_ ->
        let ref_char_code =
          if c >= '0' && c <= '9'
          then Char.code '0'
          else if c >= 'A' && c <= 'F'
          then Char.code 'A' - 10
          else if c >= 'a' && c <= 'f'
          then Char.code 'a' - 10
          else raise No_good in
        let value = (Char.code c) - ref_char_code in
        let decimal_exponent = match f.decimal_exponent with
        | None -> None
        | Some e -> Some (e - 4) in
        let mantissa = (f.mantissa lsl 4) + value in
        parse_body { (eat f) with decimal_exponent; mantissa; }

  let float_of_t f =
    assert (f.todo = []);
    let ret = float_of_int f.mantissa in
    let exponent = match f.decimal_exponent with
    | None -> f.exponent
    | Some decimal_exponent -> f.exponent + decimal_exponent in
    let ret =
      if exponent = 0
      then ret
      else ret ** (float_of_int exponent) in
    if f.negative
    then -.ret
    else ret

  let float_of_string str =
    try Pervasives.float_of_string str
    with e when Sys.win32 ->
      try
        start str
          |> parse_sign
          |> parse_hex_symbol
          |> parse_body
          |> float_of_t
      with No_good -> raise e
end

  let save_comment
    (env: Lex_env.t)
    (start: Loc.t) (_end: Loc.t)
    (buf: Buffer.t)
    (multiline: bool)
  : Lex_env.t = Ast.Comment.(
    let loc = Loc.btwn start _end in
    let s = Buffer.contents buf in
    let c = if multiline then Block s else Line s in
    let lex_comments_acc = (loc, c) :: env.lex_state.lex_comments_acc in
    { env with lex_state = { env.lex_state with lex_comments_acc; } }
  )

  let unicode_fix_cols lb =
    let rec count start stop acc =
      if start = stop then acc
      else
        let c = Char.code (Bytes.get lb.Lexing.lex_buffer start) in
        let acc = if (c land 0xC0) = 0x80
          then acc + 1
          else acc in
        count (start+1) stop acc
    in
      Lexing.(
        let bytes = count lb.lex_start_pos lb.lex_curr_pos 0 in
        let new_bol = lb.lex_curr_p.pos_bol + bytes in
        lb.lex_curr_p <- {
          lb.lex_curr_p with pos_bol = new_bol;
        }
      )

  let oct_to_int = function
    | '0'..'7' as x -> Char.code x - Char.code '0'
    | _ -> assert false

  let hexa_to_int = function
    | '0'..'9' as x -> Char.code x - Char.code '0'
    | 'a'..'f' as x -> Char.code x - Char.code 'a' + 10
    | 'A'..'F' as x -> Char.code x - Char.code 'A' + 10
    | _ -> assert false

  let utf16to8 code =
    if code >= 0x10000
    then
    (* 4 bytes *)
      [
        Char.chr (0xf0 lor (code lsr 18));
        Char.chr (0x80 lor ((code lsr 12) land 0x3f));
        Char.chr (0x80 lor ((code lsr 6) land 0x3f));
        Char.chr (0x80 lor (code land 0x3f));
      ]
    else if code >= 0x800
    then
    (* 3 bytes *)
      [
        Char.chr (0xe0 lor (code lsr 12));
        Char.chr (0x80 lor ((code lsr 6) land 0x3f));
        Char.chr (0x80 lor (code land 0x3f));
      ]
    else if code >= 0x80
    then
    (* 2 bytes *)
      [
        Char.chr (0xc0 lor (code lsr 6));
        Char.chr (0x80 lor (code land 0x3f));
      ]
    else
    (* 1 byte *)
      [
        Char.chr code;
      ]

  let mk_num_singleton number_type num neg =
    (* convert singleton number type into a float *)
    let value = match number_type with
    | LEGACY_OCTAL ->
      float (int_of_string ("0o"^num))
    | BINARY
    | OCTAL ->
      float (int_of_string num)
    | NORMAL ->
      FloatOfString.float_of_string num
    in
    let value = if neg = "" then value else ~-.value in
    T_NUMBER_SINGLETON_TYPE (number_type, value)

  type jsx_text_mode =
    | JSX_SINGLE_QUOTED_TEXT
    | JSX_DOUBLE_QUOTED_TEXT
    | JSX_CHILD_TEXT

  let keywords = Hashtbl.create 53
  let type_keywords = Hashtbl.create 53
  let _ = List.iter (fun (key, token) -> Hashtbl.add keywords key token)
    [
      "function", T_FUNCTION;
      "if", T_IF;
      "in", T_IN;
      "instanceof", T_INSTANCEOF;
      "return", T_RETURN;
      "switch", T_SWITCH;
      "this", T_THIS;
      "throw", T_THROW;
      "try", T_TRY;
      "var", T_VAR;
      "while", T_WHILE;
      "with", T_WITH;
      "const", T_CONST;
      "let", T_LET ;
      "null", T_NULL;
      "false", T_FALSE;
      "true", T_TRUE;
      "break", T_BREAK;
      "case", T_CASE;
      "catch", T_CATCH;
      "continue", T_CONTINUE;
      "default", T_DEFAULT;
      "do", T_DO;
      "finally", T_FINALLY;
      "for", T_FOR;
      "class", T_CLASS;
      "extends", T_EXTENDS;
      "static", T_STATIC;
      "else", T_ELSE;
      "new", T_NEW;
      "delete", T_DELETE;
      "typeof", T_TYPEOF;
      "void", T_VOID;
      "enum", T_ENUM;
      "export", T_EXPORT ;
      "import", T_IMPORT;
      "super", T_SUPER ;
      "implements", T_IMPLEMENTS;
      "interface", T_INTERFACE;
      "package", T_PACKAGE;
      "private", T_PRIVATE;
      "protected", T_PROTECTED;
      "public", T_PUBLIC;
      "yield", T_YIELD;
      "debugger", T_DEBUGGER;
      "declare", T_DECLARE;
      "type", T_TYPE;
      "of", T_OF;
      "async", T_ASYNC;
      "await", T_AWAIT;
    ]
  let _ = List.iter (fun (key, token) -> Hashtbl.add type_keywords key token)
    [
      "static",  T_STATIC;
      "typeof",  T_TYPEOF;
      "any",     T_ANY_TYPE;
      "bool",    T_BOOLEAN_TYPE;
      "boolean", T_BOOLEAN_TYPE;
      "true",    T_TRUE;
      "false",   T_FALSE;
      "number",  T_NUMBER_TYPE;
      "string",  T_STRING_TYPE;
      "void",    T_VOID_TYPE;
      "null",    T_NULL;
    ]

# 724 "lexer_flow.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\178\255\179\255\185\255\066\000\067\000\084\000\087\000\
    \070\000\073\000\074\000\075\000\077\000\101\000\221\255\222\255\
    \223\255\224\255\227\255\228\255\229\255\230\255\231\255\232\255\
    \192\000\076\000\101\000\023\001\110\001\246\255\247\255\108\000\
    \117\000\118\000\000\000\014\000\015\000\007\000\051\001\254\255\
    \255\255\001\000\018\000\040\000\012\000\021\000\042\000\012\000\
    \061\000\045\000\009\000\182\255\249\255\224\001\066\000\117\000\
    \015\000\048\000\052\000\023\000\229\001\040\000\056\000\026\000\
    \075\000\058\000\023\000\251\255\104\000\097\000\172\000\113\000\
    \109\000\121\000\113\000\105\000\123\000\123\000\168\000\202\255\
    \250\255\201\255\248\255\011\002\165\002\252\002\083\003\170\003\
    \001\004\088\004\175\004\006\005\093\005\180\005\011\006\098\006\
    \185\006\195\001\016\007\103\007\190\007\021\008\108\008\195\008\
    \026\009\113\009\200\009\184\000\226\255\069\002\199\255\220\255\
    \198\255\219\255\183\255\170\000\218\255\171\000\217\255\172\000\
    \216\255\210\255\173\000\215\255\176\000\208\255\207\255\204\255\
    \212\255\203\255\211\255\200\255\197\255\058\010\207\255\208\255\
    \210\255\214\255\215\255\176\000\220\255\221\255\224\255\225\255\
    \226\255\227\255\230\255\231\255\232\255\233\255\234\255\235\255\
    \148\010\250\010\214\001\081\011\168\011\026\012\249\255\204\000\
    \241\000\065\000\125\000\126\000\163\000\196\011\255\255\097\000\
    \157\000\193\000\164\000\144\000\198\000\178\000\203\009\210\000\
    \149\000\250\255\031\012\233\000\028\001\156\000\242\000\243\000\
    \249\000\036\012\231\000\247\000\245\000\223\011\021\001\215\000\
    \252\255\040\001\033\001\109\001\050\001\047\001\069\001\061\001\
    \053\001\071\001\071\001\251\255\243\001\242\000\046\001\073\001\
    \080\001\075\012\061\001\076\001\047\001\236\011\107\001\048\001\
    \120\012\255\012\086\013\173\013\000\002\004\014\091\014\178\014\
    \009\015\096\015\183\015\014\016\101\016\188\016\019\017\106\017\
    \193\017\024\018\111\018\198\018\029\019\116\019\203\019\034\020\
    \207\001\229\255\121\020\208\020\039\021\126\021\212\255\027\012\
    \252\255\253\255\254\255\255\255\207\021\238\255\001\000\239\255\
    \024\022\244\255\245\255\246\255\247\255\248\255\249\255\241\002\
    \072\003\062\022\254\255\255\255\085\022\253\255\159\003\252\255\
    \123\022\146\022\184\022\207\022\242\255\245\022\241\255\215\002\
    \251\255\210\001\254\255\255\255\207\001\253\255\252\255\059\002\
    \253\255\254\255\255\255\000\023\249\255\232\001\071\001\131\001\
    \144\001\121\001\041\012\067\021\254\255\255\255\093\001\155\001\
    \156\001\042\002\144\001\160\001\130\001\135\021\173\001\111\001\
    \251\255\252\255\011\022\248\255\004\000\249\255\250\255\056\023\
    \044\003\255\255\253\255\005\000\254\255\192\023\150\009\251\255\
    \252\255\235\001\255\255\253\255\254\255\050\024\241\255\242\255\
    \138\024\244\255\245\255\246\255\247\255\248\255\250\255\060\002\
    \127\001\201\001\231\001\043\002\136\022\055\024\254\255\255\255\
    \143\001\032\002\033\002\051\002\021\002\037\002\033\002\189\022\
    \076\002\015\002\251\255\252\255\124\012\251\255\252\255\253\255\
    \254\255\006\000\255\255\252\024\249\255\248\024\007\000\253\255\
    \254\255\255\255\079\025\223\010\095\012\132\023\156\025\252\255\
    \251\255\211\025\250\255\042\026\129\026\216\026\047\027\134\027\
    \150\002\248\027\250\255\251\255\181\002\037\002\098\002\130\002\
    \243\002\004\025\075\027\255\255\040\002\101\002\169\002\074\003\
    \114\002\133\002\140\002\201\022\183\002\121\002\252\255\253\255\
    \195\022\249\255\250\255\008\000\252\255\191\002\254\255\255\255\
    \253\255\251\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\068\000\065\000\062\000\061\000\
    \060\000\059\000\069\000\071\000\066\000\067\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \022\000\075\000\030\000\021\000\021\000\255\255\255\255\077\000\
    \063\000\074\000\077\000\077\000\077\000\077\000\002\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\003\000\255\255\004\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\064\000\255\255\
    \255\255\255\255\255\255\020\000\020\000\021\000\020\000\015\000\
    \020\000\020\000\011\000\010\000\013\000\012\000\014\000\014\000\
    \014\000\255\255\014\000\014\000\019\000\018\000\017\000\016\000\
    \021\000\019\000\018\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\041\000\255\255\042\000\255\255\046\000\
    \255\255\255\255\050\000\255\255\049\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\036\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \019\000\019\000\027\000\018\000\018\000\046\000\255\255\038\000\
    \048\000\048\000\048\000\048\000\048\000\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\002\000\255\255\003\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \018\000\017\000\017\000\016\000\255\255\016\000\015\000\015\000\
    \018\000\017\000\012\000\017\000\017\000\008\000\007\000\010\000\
    \009\000\011\000\011\000\011\000\011\000\011\000\014\000\013\000\
    \255\255\255\255\019\000\019\000\019\000\019\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\016\000\255\255\
    \015\000\255\255\255\255\255\255\255\255\255\255\255\255\012\000\
    \005\000\015\000\255\255\255\255\255\255\255\255\004\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\005\000\006\000\006\000\
    \006\000\006\000\002\000\001\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\006\000\255\255\255\255\004\000\
    \007\000\255\255\255\255\001\000\255\255\003\000\255\255\255\255\
    \255\255\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \012\000\255\255\255\255\255\255\255\255\255\255\255\255\006\000\
    \014\000\014\000\014\000\014\000\002\000\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\006\000\002\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\005\000\005\000\005\000\005\000\
    \005\000\001\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\005\000\255\255\006\000\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\255\255\000\000\255\255\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\134\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\000\000\248\000\
    \000\000\000\000\000\000\000\000\253\000\000\000\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\000\000\000\000\255\255\000\000\255\255\000\000\
    \255\255\255\255\255\255\255\255\000\000\255\255\000\000\024\001\
    \000\000\255\255\000\000\000\000\255\255\000\000\000\000\032\001\
    \000\000\000\000\000\000\036\001\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\059\001\000\000\255\255\000\000\000\000\255\255\
    \066\001\000\000\000\000\255\255\000\000\255\255\071\001\000\000\
    \000\000\255\255\000\000\000\000\000\000\078\001\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\109\001\000\000\000\000\000\000\
    \000\000\255\255\000\000\116\001\000\000\255\255\255\255\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\138\001\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\
    \161\001\000\000\000\000\255\255\000\000\255\255\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\038\000\040\000\255\000\038\000\038\000\061\001\068\001\
    \114\001\119\001\169\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \038\000\010\000\030\000\031\000\024\000\005\000\013\000\030\000\
    \021\000\020\000\032\000\007\000\016\000\006\000\026\000\033\000\
    \028\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\015\000\017\000\009\000\011\000\008\000\014\000\
    \025\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\019\000\039\000\018\000\004\000\024\000\
    \029\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\023\000\012\000\022\000\003\000\132\000\
    \131\000\130\000\128\000\123\000\122\000\119\000\120\000\117\000\
    \115\000\114\000\112\000\111\000\109\000\082\000\049\000\048\000\
    \047\000\129\000\049\000\107\000\127\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\104\000\104\000\078\000\
    \053\000\046\000\110\000\038\000\080\000\052\000\046\000\045\000\
    \048\000\047\000\038\000\038\000\045\000\038\000\068\000\067\000\
    \065\000\062\000\079\000\051\000\064\000\063\000\060\000\061\000\
    \060\000\060\000\060\000\050\000\050\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\113\000\066\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\069\000\
    \070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
    \067\000\037\000\036\000\035\000\024\000\081\000\108\000\116\000\
    \118\000\121\000\125\000\124\000\038\000\126\000\246\000\034\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\060\000\203\000\176\000\175\000\174\000\173\000\
    \002\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\178\000\176\000\175\000\165\000\024\000\
    \177\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\083\000\038\000\172\000\172\000\038\000\
    \038\000\174\000\173\000\171\000\171\000\085\000\165\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\165\000\165\000\038\000\165\000\193\000\192\000\191\000\
    \083\000\083\000\083\000\083\000\084\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\190\000\189\000\188\000\185\000\083\000\185\000\
    \083\000\083\000\083\000\083\000\084\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\187\000\185\000\185\000\185\000\194\000\
    \195\000\186\000\196\000\197\000\085\000\198\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\027\000\027\000\
    \199\000\200\000\201\000\202\000\192\000\215\000\214\000\083\000\
    \089\000\083\000\083\000\084\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\088\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\086\000\083\000\
    \083\000\213\000\212\000\209\000\209\000\083\000\209\000\083\000\
    \089\000\083\000\083\000\084\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\088\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\086\000\083\000\
    \083\000\060\000\211\000\209\000\060\000\060\000\060\000\209\000\
    \210\000\060\000\060\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\241\000\030\001\028\001\
    \060\000\029\001\055\001\054\001\240\000\060\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \053\001\052\001\056\001\051\001\044\000\043\000\042\000\057\001\
    \055\001\050\001\055\000\054\001\053\001\052\001\042\001\055\000\
    \042\001\042\001\041\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\042\001\042\001\083\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\221\000\
    \221\000\221\000\105\001\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\033\001\054\000\076\001\
    \075\001\104\001\105\001\054\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\106\001\103\001\
    \102\001\024\000\083\000\107\001\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \104\001\103\001\102\001\092\001\024\000\092\001\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \092\001\059\000\058\000\057\000\051\001\101\001\059\000\058\000\
    \057\000\083\000\050\001\100\001\092\001\101\001\092\001\056\000\
    \097\000\130\001\097\000\100\001\056\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\158\001\
    \157\001\026\001\156\001\157\001\159\001\156\001\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \145\001\025\001\155\001\154\001\083\000\145\001\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\014\001\014\001\014\001\014\001\014\001\014\001\014\001\
    \014\001\155\001\154\001\145\001\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\104\000\068\001\145\001\
    \145\001\067\001\168\001\034\001\000\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\000\000\
    \000\000\000\000\000\000\083\000\000\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \014\001\014\001\014\001\014\001\014\001\014\001\014\001\014\001\
    \000\000\000\000\000\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\153\001\000\000\000\000\
    \000\000\000\000\000\000\152\001\102\000\102\000\102\000\102\000\
    \102\000\102\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\000\000\000\000\
    \000\000\000\000\083\000\000\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\095\000\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\027\001\
    \085\000\000\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\094\000\094\000\153\001\000\000\000\000\000\000\
    \000\000\000\000\152\001\095\000\095\000\095\000\095\000\096\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\000\000\000\000\000\000\
    \000\000\095\000\000\000\095\000\095\000\095\000\095\000\096\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\083\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\083\000\083\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\000\000\000\000\000\000\000\000\
    \083\000\000\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \090\000\090\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\000\000\000\000\000\000\000\000\083\000\
    \000\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\091\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\090\000\
    \090\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\000\000\000\000\000\000\000\000\091\000\000\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\000\000\000\000\000\000\000\000\091\000\000\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\093\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\093\000\093\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \000\000\000\000\000\000\000\000\093\000\000\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\000\000\
    \000\000\000\000\000\000\093\000\000\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\095\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\085\000\000\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\095\000\095\000\095\000\095\000\
    \096\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\000\000\000\000\
    \000\000\000\000\095\000\000\000\095\000\095\000\095\000\095\000\
    \096\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\000\000\000\000\000\000\
    \000\000\095\000\000\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\097\000\000\000\097\000\000\000\
    \000\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\000\000\000\000\000\000\000\000\
    \095\000\000\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\099\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\000\000\000\000\000\000\000\000\099\000\
    \000\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\000\000\000\000\000\000\000\000\099\000\000\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\101\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\000\000\000\000\000\000\000\000\101\000\000\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \000\000\000\000\000\000\000\000\101\000\000\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \103\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\000\000\
    \000\000\000\000\000\000\103\000\000\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\000\000\000\000\
    \000\000\000\000\103\000\000\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\083\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\083\000\083\000\083\000\083\000\084\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\000\000\000\000\000\000\
    \000\000\083\000\000\000\083\000\083\000\083\000\083\000\084\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\106\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\000\000\000\000\000\000\000\000\
    \106\000\000\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\000\000\000\000\000\000\
    \000\000\000\000\073\001\072\001\000\000\000\000\000\000\000\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\000\000\000\000\000\000\000\000\106\000\
    \000\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\165\000\166\000\000\000\165\000\165\000\
    \000\000\000\000\000\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\000\000\000\000\
    \000\000\000\000\165\000\000\000\158\000\000\000\152\000\000\000\
    \137\000\158\000\147\000\146\000\159\000\136\000\144\000\157\000\
    \154\000\160\000\156\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\143\000\145\000\141\000\139\000\
    \140\000\142\000\165\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\151\000\074\001\150\000\
    \000\000\152\000\000\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\153\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\149\000\138\000\148\000\
    \152\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\000\000\
    \000\000\000\000\000\000\152\000\000\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\124\001\
    \124\001\124\001\124\001\124\001\124\001\124\001\124\001\124\001\
    \124\001\000\000\000\000\164\000\163\000\162\000\152\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\161\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\135\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\000\000\000\000\125\001\
    \000\000\152\000\000\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\242\000\152\000\217\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\224\000\
    \000\000\155\000\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\217\000\217\000\217\000\217\000\218\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\000\000\000\000\000\000\000\000\
    \217\000\000\000\217\000\217\000\217\000\217\000\218\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\165\000\000\000\000\000\
    \165\000\165\000\000\000\000\000\000\000\000\000\224\000\000\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \155\000\155\000\000\000\000\000\165\000\000\000\000\000\000\000\
    \000\000\217\000\228\000\217\000\217\000\218\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\227\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \225\000\217\000\217\000\000\000\000\000\000\000\000\000\217\000\
    \000\000\217\000\228\000\217\000\217\000\218\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\227\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \225\000\217\000\217\000\209\000\000\000\249\000\209\000\209\000\
    \185\000\000\000\000\000\185\000\185\000\185\000\000\000\000\000\
    \185\000\185\000\042\001\000\000\000\000\042\001\042\001\000\000\
    \000\000\000\000\209\000\000\000\000\000\251\000\000\000\185\000\
    \000\000\000\000\251\000\000\000\185\000\000\000\000\000\000\000\
    \204\000\042\001\156\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\209\000\000\000\000\000\209\000\
    \209\000\180\000\000\000\000\000\000\000\000\000\180\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \185\000\185\000\000\000\209\000\209\000\209\000\209\000\209\000\
    \209\000\209\000\209\000\209\000\209\000\209\000\209\000\250\000\
    \000\000\204\000\000\000\156\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\179\000\114\001\000\000\
    \000\000\113\001\179\000\000\000\000\000\000\000\185\000\124\001\
    \124\001\124\001\124\001\124\001\124\001\124\001\124\001\124\001\
    \124\001\000\000\128\001\209\000\217\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\170\000\169\000\168\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\000\000\167\000\000\000\000\000\000\000\000\000\
    \111\001\217\000\217\000\217\000\217\000\218\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\000\000\000\000\000\000\000\000\217\000\
    \000\000\217\000\217\000\217\000\217\000\218\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\000\000\000\000\000\000\000\000\110\001\
    \000\000\000\000\000\000\208\000\207\000\206\000\000\000\000\000\
    \184\000\183\000\182\000\000\000\000\000\184\000\183\000\182\000\
    \000\000\205\000\049\001\048\001\047\001\000\000\181\000\000\000\
    \000\000\000\000\000\000\181\000\000\000\000\000\000\000\000\000\
    \046\001\000\000\000\000\249\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\217\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\208\000\207\000\206\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\000\000\205\000\000\000\000\000\000\000\000\000\000\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\000\000\000\000\000\000\000\000\217\000\000\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\000\000\112\001\000\000\000\000\000\000\
    \000\000\220\000\000\000\220\000\000\000\000\000\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\219\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\000\000\000\000\000\000\000\000\217\000\000\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\223\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\219\000\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \000\000\000\000\000\000\000\000\223\000\000\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \222\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\221\000\221\000\221\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\000\000\
    \000\000\000\000\000\000\222\000\000\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\000\000\000\000\
    \000\000\000\000\222\000\000\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\223\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\000\000\000\000\000\000\
    \000\000\223\000\000\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\217\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\000\000\000\000\000\000\000\000\
    \217\000\000\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \238\000\238\000\238\000\238\000\238\000\238\000\238\000\238\000\
    \238\000\238\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\238\000\238\000\238\000\238\000\238\000\238\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\000\000\000\000\000\000\000\000\217\000\
    \000\000\238\000\238\000\238\000\238\000\238\000\238\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\234\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\224\000\000\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\233\000\
    \233\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \234\000\234\000\234\000\234\000\235\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\000\000\000\000\000\000\000\000\234\000\000\000\
    \234\000\234\000\234\000\234\000\235\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\217\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\217\000\217\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\000\000\000\000\000\000\000\000\217\000\000\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\229\000\229\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \000\000\000\000\000\000\000\000\217\000\000\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \230\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\229\000\229\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\000\000\
    \000\000\000\000\000\000\230\000\000\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\000\000\000\000\
    \000\000\000\000\230\000\000\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\232\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\232\000\232\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\000\000\000\000\000\000\
    \000\000\232\000\000\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\000\000\000\000\000\000\000\000\
    \232\000\000\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\234\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\224\000\000\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\234\000\234\000\234\000\234\000\235\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\000\000\000\000\000\000\000\000\234\000\
    \000\000\234\000\234\000\234\000\234\000\235\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\000\000\000\000\000\000\000\000\234\000\000\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\220\000\000\000\220\000\000\000\000\000\236\000\236\000\
    \236\000\236\000\236\000\236\000\236\000\236\000\236\000\236\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\000\000\000\000\000\000\000\000\234\000\000\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\237\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\236\000\236\000\236\000\
    \236\000\236\000\236\000\236\000\236\000\236\000\236\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \000\000\000\000\000\000\000\000\237\000\000\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\000\000\
    \000\000\000\000\000\000\237\000\000\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\239\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\238\000\238\000\238\000\238\000\238\000\
    \238\000\238\000\238\000\238\000\238\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\238\000\238\000\238\000\238\000\
    \238\000\238\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\000\000\000\000\
    \000\000\000\000\239\000\000\000\238\000\238\000\238\000\238\000\
    \238\000\238\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\239\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\000\000\000\000\000\000\
    \000\000\239\000\000\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\152\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\000\000\000\000\000\000\000\000\
    \152\000\000\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\243\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\000\000\000\000\000\000\000\000\152\000\
    \000\000\152\000\152\000\152\000\152\000\244\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\042\001\044\001\000\000\042\001\
    \042\001\000\000\000\000\000\000\000\000\000\000\000\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\000\000\000\000\042\001\000\000\000\000\000\000\000\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\000\000\000\000\000\000\000\000\152\000\000\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\245\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\255\000\000\000\000\000\254\000\152\000\000\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\000\000\000\000\000\000\000\000\000\000\000\000\008\001\
    \007\001\007\001\007\001\007\001\007\001\007\001\007\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\000\000\000\000\000\000\061\001\000\000\000\000\
    \060\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\049\001\048\001\047\001\000\000\
    \000\000\000\000\000\000\010\001\000\000\000\000\000\000\000\000\
    \000\000\006\001\046\001\000\000\000\000\005\001\042\001\000\000\
    \000\000\000\000\063\001\000\000\000\000\004\001\000\000\000\000\
    \000\000\003\001\000\000\002\001\000\001\001\001\000\000\009\001\
    \017\001\017\001\017\001\017\001\017\001\017\001\017\001\017\001\
    \017\001\017\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\017\001\017\001\017\001\017\001\017\001\017\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\062\001\064\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\012\001\012\001\
    \012\001\012\001\012\001\012\001\012\001\012\001\012\001\012\001\
    \000\000\017\001\017\001\017\001\017\001\017\001\017\001\012\001\
    \012\001\012\001\012\001\012\001\012\001\013\001\013\001\013\001\
    \013\001\013\001\013\001\013\001\013\001\013\001\013\001\000\000\
    \000\000\092\001\000\000\016\001\092\001\092\001\013\001\013\001\
    \013\001\013\001\013\001\013\001\000\000\000\000\000\000\012\001\
    \012\001\012\001\012\001\012\001\012\001\000\000\000\000\000\000\
    \092\001\000\000\000\000\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\000\000\013\001\013\001\
    \013\001\013\001\013\001\013\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\000\000\162\001\000\000\011\001\
    \163\001\000\000\000\000\018\001\018\001\018\001\018\001\018\001\
    \018\001\000\000\000\000\000\000\021\001\021\001\021\001\021\001\
    \021\001\021\001\000\000\000\000\000\000\000\000\000\000\165\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\000\000\018\001\018\001\018\001\018\001\018\001\
    \018\001\019\001\019\001\019\001\019\001\019\001\019\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\042\001\044\001\065\001\042\001\043\001\000\000\000\000\
    \020\001\020\001\020\001\020\001\020\001\020\001\000\000\000\000\
    \000\000\019\001\019\001\019\001\019\001\019\001\019\001\164\001\
    \042\001\000\000\000\000\166\001\000\000\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\037\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\000\000\092\001\092\001\092\001\
    \092\001\092\001\092\001\092\001\092\001\092\001\092\001\092\001\
    \000\000\145\001\145\001\145\001\145\001\145\001\145\001\145\001\
    \145\001\145\001\145\001\145\001\000\000\000\000\021\001\021\001\
    \021\001\021\001\021\001\021\001\069\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\099\001\098\001\097\001\092\001\000\000\000\000\000\000\
    \000\000\000\000\022\001\000\000\000\000\000\000\000\000\096\001\
    \145\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\000\000\000\000\000\000\000\000\069\001\
    \000\000\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\000\000\126\001\126\001\126\001\126\001\
    \126\001\126\001\126\001\126\001\126\001\126\001\000\000\000\000\
    \000\000\000\000\000\000\167\001\000\000\126\001\126\001\126\001\
    \126\001\126\001\126\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\041\001\040\001\039\001\069\001\126\001\126\001\126\001\
    \126\001\126\001\126\001\000\000\000\000\000\000\000\000\038\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \045\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\000\000\000\000\000\000\000\000\069\001\
    \000\000\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\092\001\094\001\000\000\092\001\093\001\
    \092\001\094\001\000\000\092\001\092\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\092\001\000\000\079\001\000\000\080\001\092\001\
    \000\000\079\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \082\001\087\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\083\001\000\000\086\001\081\001\
    \085\001\000\000\000\000\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\000\000\000\000\000\000\
    \000\000\080\001\000\000\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\084\001\080\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\080\001\
    \000\000\000\000\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\000\000\000\000\000\000\
    \000\000\080\001\000\000\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\000\000\119\001\000\000\
    \000\000\118\001\000\000\000\000\000\000\145\001\000\000\000\000\
    \145\001\145\001\000\000\091\001\090\001\089\001\000\000\000\000\
    \099\001\098\001\097\001\123\001\122\001\000\000\121\001\000\000\
    \000\000\088\001\117\001\121\001\145\001\000\000\096\001\000\000\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\095\001\000\000\000\000\000\000\000\000\000\000\
    \121\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\122\001\000\000\000\000\000\000\000\000\122\001\
    \000\000\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\122\001\129\001\000\000\000\000\000\000\121\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\129\001\
    \129\001\129\001\129\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \129\001\129\001\129\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\129\001\129\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\129\001\129\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\129\001\000\000\000\000\000\000\000\000\129\001\000\000\
    \129\001\129\001\129\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\129\001\129\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\129\001\129\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\129\001\000\000\000\000\126\001\126\001\126\001\126\001\
    \126\001\126\001\126\001\126\001\126\001\126\001\000\000\127\001\
    \000\000\000\000\000\000\000\000\000\000\126\001\126\001\126\001\
    \126\001\126\001\126\001\000\000\000\000\151\001\150\001\149\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\148\001\000\000\000\000\000\000\131\001\
    \000\000\000\000\000\000\000\000\120\001\126\001\126\001\126\001\
    \126\001\126\001\126\001\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\000\000\130\001\000\000\
    \000\000\000\000\000\000\000\000\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\000\000\000\000\
    \000\000\000\000\131\001\000\000\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\132\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\000\000\130\001\000\000\000\000\
    \000\000\000\000\000\000\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\000\000\000\000\000\000\
    \000\000\132\001\000\000\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\133\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\000\000\130\001\000\000\000\000\000\000\
    \000\000\000\000\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\133\001\000\000\000\000\000\000\000\000\
    \133\001\000\000\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\133\001\134\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\000\000\130\001\000\000\000\000\000\000\000\000\
    \000\000\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\134\001\000\000\000\000\000\000\000\000\134\001\
    \000\000\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\134\001\135\001\145\001\147\001\000\000\145\001\
    \145\001\000\000\000\000\000\000\000\000\000\000\000\000\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\000\000\130\001\145\001\000\000\000\000\000\000\000\000\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\135\001\000\000\000\000\000\000\000\000\135\001\000\000\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\135\001\136\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\136\001\136\001\
    \136\001\136\001\136\001\136\001\136\001\136\001\136\001\136\001\
    \000\000\130\001\000\000\000\000\000\000\000\000\000\000\136\001\
    \136\001\136\001\136\001\136\001\136\001\136\001\136\001\136\001\
    \136\001\136\001\136\001\136\001\136\001\136\001\136\001\136\001\
    \136\001\136\001\136\001\136\001\136\001\136\001\136\001\136\001\
    \136\001\000\000\000\000\000\000\000\000\136\001\000\000\136\001\
    \136\001\136\001\136\001\136\001\136\001\136\001\136\001\136\001\
    \136\001\136\001\136\001\136\001\136\001\136\001\136\001\136\001\
    \136\001\136\001\136\001\136\001\136\001\136\001\136\001\136\001\
    \136\001\145\001\147\001\000\000\145\001\146\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \145\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\140\001\
    \000\000\000\000\000\000\000\000\151\001\150\001\149\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\148\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\139\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\144\001\143\001\142\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\141\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\254\000\000\000\000\000\060\001\067\001\
    \113\001\118\001\163\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
    \005\000\006\000\007\000\008\000\008\000\009\000\009\000\010\000\
    \011\000\011\000\012\000\013\000\025\000\031\000\035\000\036\000\
    \036\000\006\000\042\000\026\000\007\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\032\000\
    \033\000\037\000\013\000\045\000\032\000\033\000\044\000\037\000\
    \043\000\043\000\046\000\047\000\044\000\049\000\054\000\055\000\
    \057\000\059\000\032\000\033\000\058\000\058\000\061\000\059\000\
    \062\000\063\000\065\000\034\000\041\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \050\000\012\000\056\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\066\000\068\000\
    \069\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
    \077\000\000\000\000\000\000\000\024\000\078\000\107\000\115\000\
    \117\000\119\000\122\000\122\000\048\000\124\000\139\000\000\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\064\000\159\000\161\000\162\000\163\000\163\000\
    \000\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\160\000\167\000\168\000\171\000\024\000\
    \160\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\027\000\038\000\164\000\170\000\038\000\
    \038\000\169\000\169\000\164\000\170\000\027\000\172\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\173\000\175\000\038\000\176\000\179\000\180\000\181\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\182\000\183\000\183\000\186\000\027\000\187\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\028\000\184\000\188\000\190\000\191\000\193\000\
    \194\000\184\000\195\000\196\000\028\000\197\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \198\000\199\000\200\000\201\000\202\000\205\000\206\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\207\000\207\000\210\000\211\000\028\000\212\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\053\000\208\000\214\000\053\000\053\000\060\000\215\000\
    \208\000\060\000\060\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\240\000\028\001\025\001\
    \053\000\025\001\038\001\039\001\154\000\060\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \040\001\040\001\037\001\041\001\038\000\038\000\038\000\037\001\
    \046\001\041\001\053\000\047\001\048\001\048\001\050\001\060\000\
    \051\001\052\001\038\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\054\001\055\001\083\000\
    \220\000\220\000\220\000\220\000\220\000\220\000\220\000\220\000\
    \220\000\220\000\088\001\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\031\001\053\000\073\001\
    \073\001\089\001\096\001\060\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\087\001\090\001\
    \090\001\109\000\083\000\087\001\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\109\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\109\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\109\000\109\000\109\000\109\000\
    \097\001\098\001\098\001\100\001\109\000\101\001\109\000\109\000\
    \109\000\109\000\109\000\109\000\109\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\109\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\109\000\109\000\109\000\109\000\
    \102\001\053\000\053\000\053\000\049\001\091\001\060\000\060\000\
    \060\000\084\000\049\001\091\001\104\001\099\001\105\001\053\000\
    \084\000\136\001\084\000\099\001\060\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\140\001\
    \141\001\023\001\142\001\148\001\140\001\149\001\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \152\001\023\001\143\001\143\001\084\000\153\001\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \085\000\007\001\007\001\007\001\007\001\007\001\007\001\007\001\
    \007\001\150\001\150\001\154\001\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\064\001\156\001\
    \157\001\064\001\165\001\031\001\255\255\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\255\255\
    \255\255\255\255\255\255\085\000\255\255\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\086\000\
    \008\001\008\001\008\001\008\001\008\001\008\001\008\001\008\001\
    \255\255\255\255\255\255\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\144\001\255\255\255\255\
    \255\255\255\255\255\255\144\001\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\255\255\255\255\
    \255\255\255\255\086\000\255\255\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\087\000\014\001\
    \014\001\014\001\014\001\014\001\014\001\014\001\014\001\023\001\
    \087\000\255\255\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\151\001\255\255\255\255\255\255\
    \255\255\255\255\151\001\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\255\255\255\255\255\255\
    \255\255\087\000\255\255\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\088\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\064\001\255\255\255\255\255\255\
    \255\255\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\255\255\255\255\255\255\255\255\
    \088\000\255\255\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\089\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\255\255\255\255\255\255\255\255\089\000\
    \255\255\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\090\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\255\255\255\255\255\255\255\255\090\000\255\255\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\091\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\255\255\255\255\255\255\255\255\091\000\255\255\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\092\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\092\000\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \255\255\255\255\255\255\255\255\092\000\255\255\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \093\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\255\255\
    \255\255\255\255\255\255\093\000\255\255\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\094\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\094\000\255\255\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\255\255\255\255\
    \255\255\255\255\094\000\255\255\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\095\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\255\255\255\255\255\255\
    \255\255\095\000\255\255\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\096\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\096\000\255\255\096\000\255\255\
    \255\255\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\255\255\255\255\255\255\255\255\
    \096\000\255\255\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\098\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\255\255\255\255\255\255\255\255\098\000\
    \255\255\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\099\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\255\255\255\255\255\255\255\255\099\000\255\255\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\100\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\255\255\255\255\255\255\255\255\100\000\255\255\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\101\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \255\255\255\255\255\255\255\255\101\000\255\255\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \102\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\255\255\
    \255\255\255\255\255\255\102\000\255\255\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\103\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\255\255\255\255\
    \255\255\255\255\103\000\255\255\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\104\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\255\255\255\255\255\255\
    \255\255\104\000\255\255\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\105\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\255\255\255\255\255\255\255\255\
    \105\000\255\255\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\106\000\255\255\255\255\255\255\
    \255\255\255\255\070\001\070\001\255\255\255\255\255\255\255\255\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\255\255\255\255\255\255\255\255\106\000\
    \255\255\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\133\000\133\000\255\255\133\000\133\000\
    \255\255\255\255\255\255\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\255\255\255\255\
    \255\255\255\255\133\000\255\255\133\000\255\255\133\000\255\255\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\174\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\070\001\133\000\
    \255\255\133\000\255\255\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \152\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\255\255\
    \255\255\255\255\255\255\152\000\255\255\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\123\001\
    \123\001\123\001\123\001\123\001\123\001\123\001\123\001\123\001\
    \123\001\255\255\255\255\133\000\133\000\133\000\153\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\133\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\133\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\255\255\255\255\123\001\
    \255\255\153\000\255\255\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\155\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\155\000\
    \255\255\155\000\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\255\255\255\255\255\255\255\255\
    \155\000\255\255\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\156\000\165\000\255\255\255\255\
    \165\000\165\000\255\255\255\255\255\255\255\255\156\000\255\255\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\255\255\255\255\165\000\255\255\255\255\255\255\
    \255\255\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\255\255\255\255\255\255\255\255\156\000\
    \255\255\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\157\000\255\255\247\000\157\000\157\000\
    \178\000\255\255\255\255\178\000\178\000\185\000\255\255\255\255\
    \185\000\185\000\042\001\255\255\255\255\042\001\042\001\255\255\
    \255\255\255\255\157\000\255\255\255\255\247\000\255\255\178\000\
    \255\255\255\255\247\000\255\255\185\000\255\255\255\255\255\255\
    \157\000\042\001\157\000\157\000\157\000\157\000\157\000\157\000\
    \157\000\157\000\157\000\157\000\209\000\255\255\255\255\209\000\
    \209\000\178\000\255\255\255\255\255\255\255\255\185\000\189\000\
    \189\000\189\000\189\000\189\000\189\000\189\000\189\000\189\000\
    \189\000\189\000\255\255\209\000\213\000\213\000\213\000\213\000\
    \213\000\213\000\213\000\213\000\213\000\213\000\213\000\247\000\
    \255\255\209\000\255\255\209\000\209\000\209\000\209\000\209\000\
    \209\000\209\000\209\000\209\000\209\000\178\000\108\001\255\255\
    \255\255\108\001\185\000\255\255\255\255\255\255\189\000\124\001\
    \124\001\124\001\124\001\124\001\124\001\124\001\124\001\124\001\
    \124\001\255\255\124\001\213\000\216\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\165\000\165\000\165\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\255\255\165\000\255\255\255\255\255\255\255\255\
    \108\001\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\255\255\255\255\255\255\255\255\216\000\
    \255\255\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\255\255\255\255\255\255\255\255\108\001\
    \255\255\255\255\255\255\157\000\157\000\157\000\255\255\255\255\
    \178\000\178\000\178\000\255\255\255\255\185\000\185\000\185\000\
    \255\255\157\000\042\001\042\001\042\001\255\255\178\000\255\255\
    \255\255\255\255\255\255\185\000\255\255\255\255\255\255\255\255\
    \042\001\255\255\255\255\247\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\217\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\209\000\209\000\209\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\255\255\209\000\255\255\255\255\255\255\255\255\255\255\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\255\255\255\255\255\255\255\255\217\000\255\255\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\218\000\255\255\108\001\255\255\255\255\255\255\
    \255\255\218\000\255\255\218\000\255\255\255\255\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\255\255\255\255\255\255\255\255\218\000\255\255\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\219\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\219\000\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\219\000\
    \255\255\255\255\255\255\255\255\219\000\255\255\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\219\000\
    \221\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\221\000\221\000\221\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\221\000\221\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\255\255\
    \255\255\255\255\255\255\221\000\255\255\221\000\221\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\222\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\255\255\255\255\
    \255\255\255\255\222\000\255\255\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\223\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\255\255\255\255\255\255\
    \255\255\223\000\255\255\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\224\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\224\000\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\255\255\255\255\255\255\255\255\
    \224\000\255\255\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\225\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \225\000\225\000\225\000\225\000\225\000\225\000\225\000\225\000\
    \225\000\225\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\225\000\225\000\225\000\225\000\225\000\225\000\225\000\
    \225\000\225\000\225\000\225\000\225\000\225\000\225\000\225\000\
    \225\000\225\000\225\000\225\000\225\000\225\000\225\000\225\000\
    \225\000\225\000\225\000\255\255\255\255\255\255\255\255\225\000\
    \255\255\225\000\225\000\225\000\225\000\225\000\225\000\225\000\
    \225\000\225\000\225\000\225\000\225\000\225\000\225\000\225\000\
    \225\000\225\000\225\000\225\000\225\000\225\000\225\000\225\000\
    \225\000\225\000\225\000\226\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\226\000\255\255\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\255\255\255\255\255\255\255\255\226\000\255\255\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\227\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\255\255\255\255\255\255\255\255\227\000\255\255\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\228\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\228\000\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\228\000\228\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\228\000\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\228\000\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\228\000\228\000\228\000\
    \255\255\255\255\255\255\255\255\228\000\255\255\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\228\000\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\228\000\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\228\000\228\000\228\000\
    \229\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\255\255\
    \255\255\255\255\255\255\229\000\255\255\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\230\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\255\255\255\255\
    \255\255\255\255\230\000\255\255\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\231\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\255\255\255\255\255\255\
    \255\255\231\000\255\255\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\232\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\255\255\255\255\255\255\255\255\
    \232\000\255\255\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\233\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\233\000\255\255\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\255\255\255\255\255\255\255\255\233\000\
    \255\255\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\234\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\255\255\255\255\255\255\255\255\234\000\255\255\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\235\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\235\000\255\255\235\000\255\255\255\255\235\000\235\000\
    \235\000\235\000\235\000\235\000\235\000\235\000\235\000\235\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\235\000\
    \235\000\235\000\235\000\235\000\235\000\235\000\235\000\235\000\
    \235\000\235\000\235\000\235\000\235\000\235\000\235\000\235\000\
    \235\000\235\000\235\000\235\000\235\000\235\000\235\000\235\000\
    \235\000\255\255\255\255\255\255\255\255\235\000\255\255\235\000\
    \235\000\235\000\235\000\235\000\235\000\235\000\235\000\235\000\
    \235\000\235\000\235\000\235\000\235\000\235\000\235\000\235\000\
    \235\000\235\000\235\000\235\000\235\000\235\000\235\000\235\000\
    \235\000\236\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\236\000\236\000\236\000\
    \236\000\236\000\236\000\236\000\236\000\236\000\236\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\236\000\236\000\
    \236\000\236\000\236\000\236\000\236\000\236\000\236\000\236\000\
    \236\000\236\000\236\000\236\000\236\000\236\000\236\000\236\000\
    \236\000\236\000\236\000\236\000\236\000\236\000\236\000\236\000\
    \255\255\255\255\255\255\255\255\236\000\255\255\236\000\236\000\
    \236\000\236\000\236\000\236\000\236\000\236\000\236\000\236\000\
    \236\000\236\000\236\000\236\000\236\000\236\000\236\000\236\000\
    \236\000\236\000\236\000\236\000\236\000\236\000\236\000\236\000\
    \237\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\255\255\
    \255\255\255\255\255\255\237\000\255\255\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\238\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\238\000\238\000\238\000\238\000\238\000\
    \238\000\238\000\238\000\238\000\238\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\238\000\238\000\238\000\238\000\
    \238\000\238\000\238\000\238\000\238\000\238\000\238\000\238\000\
    \238\000\238\000\238\000\238\000\238\000\238\000\238\000\238\000\
    \238\000\238\000\238\000\238\000\238\000\238\000\255\255\255\255\
    \255\255\255\255\238\000\255\255\238\000\238\000\238\000\238\000\
    \238\000\238\000\238\000\238\000\238\000\238\000\238\000\238\000\
    \238\000\238\000\238\000\238\000\238\000\238\000\238\000\238\000\
    \238\000\238\000\238\000\238\000\238\000\238\000\239\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\255\255\255\255\255\255\
    \255\255\239\000\255\255\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\239\000\239\000\239\000\
    \239\000\239\000\239\000\239\000\239\000\242\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\255\255\255\255\255\255\255\255\
    \242\000\255\255\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\243\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \243\000\243\000\243\000\243\000\243\000\243\000\243\000\243\000\
    \243\000\243\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\243\000\243\000\243\000\243\000\243\000\243\000\243\000\
    \243\000\243\000\243\000\243\000\243\000\243\000\243\000\243\000\
    \243\000\243\000\243\000\243\000\243\000\243\000\243\000\243\000\
    \243\000\243\000\243\000\255\255\255\255\255\255\255\255\243\000\
    \255\255\243\000\243\000\243\000\243\000\243\000\243\000\243\000\
    \243\000\243\000\243\000\243\000\243\000\243\000\243\000\243\000\
    \243\000\243\000\243\000\243\000\243\000\243\000\243\000\243\000\
    \243\000\243\000\243\000\244\000\043\001\043\001\255\255\043\001\
    \043\001\255\255\255\255\255\255\255\255\255\255\255\255\244\000\
    \244\000\244\000\244\000\244\000\244\000\244\000\244\000\244\000\
    \244\000\255\255\255\255\043\001\255\255\255\255\255\255\255\255\
    \244\000\244\000\244\000\244\000\244\000\244\000\244\000\244\000\
    \244\000\244\000\244\000\244\000\244\000\244\000\244\000\244\000\
    \244\000\244\000\244\000\244\000\244\000\244\000\244\000\244\000\
    \244\000\244\000\255\255\255\255\255\255\255\255\244\000\255\255\
    \244\000\244\000\244\000\244\000\244\000\244\000\244\000\244\000\
    \244\000\244\000\244\000\244\000\244\000\244\000\244\000\244\000\
    \244\000\244\000\244\000\244\000\244\000\244\000\244\000\244\000\
    \244\000\244\000\245\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\252\000\255\255\255\255\252\000\245\000\255\255\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\255\255\255\255\255\255\255\255\255\255\255\255\252\000\
    \252\000\252\000\252\000\252\000\252\000\252\000\252\000\053\001\
    \053\001\053\001\053\001\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\255\255\255\255\255\255\058\001\255\255\255\255\
    \058\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\043\001\043\001\043\001\255\255\
    \255\255\255\255\255\255\252\000\255\255\255\255\255\255\255\255\
    \255\255\252\000\043\001\255\255\255\255\252\000\053\001\255\255\
    \255\255\255\255\058\001\255\255\255\255\252\000\255\255\255\255\
    \255\255\252\000\255\255\252\000\252\000\252\000\255\255\252\000\
    \000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\
    \000\001\000\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\001\000\001\000\001\000\001\000\001\000\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\058\001\058\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\009\001\009\001\
    \009\001\009\001\009\001\009\001\009\001\009\001\009\001\009\001\
    \255\255\000\001\000\001\000\001\000\001\000\001\000\001\009\001\
    \009\001\009\001\009\001\009\001\009\001\012\001\012\001\012\001\
    \012\001\012\001\012\001\012\001\012\001\012\001\012\001\255\255\
    \255\255\092\001\255\255\000\001\092\001\092\001\012\001\012\001\
    \012\001\012\001\012\001\012\001\255\255\255\255\255\255\009\001\
    \009\001\009\001\009\001\009\001\009\001\255\255\255\255\255\255\
    \092\001\255\255\255\255\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\255\255\012\001\012\001\
    \012\001\012\001\012\001\012\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\017\001\017\001\017\001\017\001\017\001\017\001\
    \017\001\017\001\017\001\017\001\255\255\160\001\255\255\252\000\
    \160\001\255\255\255\255\017\001\017\001\017\001\017\001\017\001\
    \017\001\255\255\255\255\255\255\016\001\016\001\016\001\016\001\
    \016\001\016\001\255\255\255\255\255\255\255\255\255\255\160\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\255\255\017\001\017\001\017\001\017\001\017\001\
    \017\001\018\001\018\001\018\001\018\001\018\001\018\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\035\001\035\001\058\001\035\001\035\001\255\255\255\255\
    \019\001\019\001\019\001\019\001\019\001\019\001\255\255\255\255\
    \255\255\018\001\018\001\018\001\018\001\018\001\018\001\160\001\
    \035\001\255\255\255\255\160\001\255\255\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\035\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\255\255\103\001\103\001\103\001\
    \103\001\103\001\103\001\103\001\103\001\103\001\103\001\103\001\
    \255\255\155\001\155\001\155\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\255\255\255\255\021\001\021\001\
    \021\001\021\001\021\001\021\001\063\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\092\001\092\001\092\001\103\001\255\255\255\255\255\255\
    \255\255\255\255\021\001\255\255\255\255\255\255\255\255\092\001\
    \155\001\063\001\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\255\255\255\255\255\255\255\255\063\001\
    \255\255\063\001\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\255\255\125\001\125\001\125\001\125\001\
    \125\001\125\001\125\001\125\001\125\001\125\001\255\255\255\255\
    \255\255\255\255\255\255\160\001\255\255\125\001\125\001\125\001\
    \125\001\125\001\125\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\035\001\035\001\035\001\069\001\125\001\125\001\125\001\
    \125\001\125\001\125\001\255\255\255\255\255\255\255\255\035\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \035\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\255\255\255\255\255\255\255\255\069\001\
    \255\255\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\077\001\077\001\255\255\077\001\077\001\
    \093\001\093\001\255\255\093\001\093\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\077\001\255\255\077\001\255\255\077\001\093\001\
    \255\255\077\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \077\001\077\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\077\001\255\255\077\001\077\001\
    \077\001\255\255\255\255\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\255\255\255\255\255\255\
    \255\255\077\001\255\255\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\080\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\080\001\
    \255\255\255\255\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\255\255\255\255\255\255\
    \255\255\080\001\255\255\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\255\255\115\001\255\255\
    \255\255\115\001\255\255\255\255\255\255\145\001\255\255\255\255\
    \145\001\145\001\255\255\077\001\077\001\077\001\255\255\255\255\
    \093\001\093\001\093\001\117\001\117\001\255\255\115\001\255\255\
    \255\255\077\001\115\001\115\001\145\001\255\255\093\001\255\255\
    \117\001\117\001\117\001\117\001\117\001\117\001\117\001\117\001\
    \117\001\117\001\077\001\255\255\255\255\255\255\255\255\255\255\
    \115\001\117\001\117\001\117\001\117\001\117\001\117\001\117\001\
    \117\001\117\001\117\001\117\001\117\001\117\001\117\001\117\001\
    \117\001\117\001\117\001\117\001\117\001\117\001\117\001\117\001\
    \117\001\117\001\117\001\255\255\255\255\255\255\255\255\117\001\
    \255\255\117\001\117\001\117\001\117\001\117\001\117\001\117\001\
    \117\001\117\001\117\001\117\001\117\001\117\001\117\001\117\001\
    \117\001\117\001\117\001\117\001\117\001\117\001\117\001\117\001\
    \117\001\117\001\117\001\122\001\255\255\255\255\255\255\115\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\122\001\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\255\255\255\255\255\255\255\255\122\001\255\255\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\122\001\122\001\122\001\122\001\122\001\122\001\
    \122\001\122\001\255\255\255\255\126\001\126\001\126\001\126\001\
    \126\001\126\001\126\001\126\001\126\001\126\001\255\255\126\001\
    \255\255\255\255\255\255\255\255\255\255\126\001\126\001\126\001\
    \126\001\126\001\126\001\255\255\255\255\145\001\145\001\145\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\145\001\255\255\255\255\255\255\129\001\
    \255\255\255\255\255\255\255\255\115\001\126\001\126\001\126\001\
    \126\001\126\001\126\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\129\001\129\001\129\001\129\001\255\255\129\001\255\255\
    \255\255\255\255\255\255\255\255\129\001\129\001\129\001\129\001\
    \129\001\129\001\129\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\129\001\129\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\129\001\129\001\129\001\129\001\129\001\255\255\255\255\
    \255\255\255\255\129\001\255\255\129\001\129\001\129\001\129\001\
    \129\001\129\001\129\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\129\001\129\001\129\001\129\001\129\001\129\001\129\001\
    \129\001\129\001\129\001\129\001\129\001\129\001\131\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\131\001\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\255\255\131\001\255\255\255\255\
    \255\255\255\255\255\255\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\255\255\255\255\255\255\
    \255\255\131\001\255\255\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\132\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\132\001\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\255\255\132\001\255\255\255\255\255\255\
    \255\255\255\255\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\255\255\255\255\255\255\255\255\
    \132\001\255\255\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\133\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \133\001\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\255\255\133\001\255\255\255\255\255\255\255\255\
    \255\255\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\255\255\255\255\255\255\255\255\133\001\
    \255\255\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\133\001\133\001\133\001\133\001\133\001\
    \133\001\133\001\133\001\134\001\146\001\146\001\255\255\146\001\
    \146\001\255\255\255\255\255\255\255\255\255\255\255\255\134\001\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\255\255\134\001\146\001\255\255\255\255\255\255\255\255\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\255\255\255\255\255\255\255\255\134\001\255\255\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\134\001\134\001\134\001\134\001\134\001\134\001\
    \134\001\134\001\135\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\135\001\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \255\255\135\001\255\255\255\255\255\255\255\255\255\255\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\255\255\255\255\255\255\255\255\135\001\255\255\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\135\001\135\001\135\001\135\001\135\001\135\001\135\001\
    \135\001\137\001\137\001\255\255\137\001\137\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \137\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\137\001\
    \255\255\255\255\255\255\255\255\146\001\146\001\146\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\146\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\137\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\137\001\137\001\137\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\137\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \137\001";
  Lexing.lex_base_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\010\000\022\000\034\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
    \001\000\012\000\000\000\012\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\044\000\054\000\095\000\066\000\
    \118\000\076\000\078\000\000\000\129\000\000\000\152\000\000\000\
    \162\000\172\000\182\000\000\000\192\000\000\000\202\000\000\000\
    \225\000\235\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\004\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\014\001\026\001\038\001\087\001\000\000\000\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\007\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\009\000\011\000\013\000\015\000\229\000\026\000\008\000\
    \104\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\072\001\000\000\000\000\000\000\
    \000\000\121\001\013\000\028\000\016\000\026\001\029\000\069\000\
    \131\001\000\000\141\001\154\001\164\001\174\001\000\000\000\000\
    \184\001\194\001\219\001\229\001\137\000\139\000\000\000\249\001\
    \000\000\003\002\000\000\013\002\023\002\000\000\033\002\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_backtrk_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\015\000\015\000\000\000\015\000\000\000\
    \015\000\015\000\000\000\035\000\000\000\038\000\041\000\041\000\
    \041\000\000\000\041\000\041\000\000\000\044\000\000\000\047\000\
    \000\000\000\000\044\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\087\000\087\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\104\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \087\000\107\000\107\000\115\000\000\000\115\000\118\000\118\000\
    \087\000\107\000\126\000\107\000\107\000\038\000\143\000\047\000\
    \148\000\153\000\153\000\153\000\153\000\153\000\158\000\161\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_default_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_trans_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\009\000\000\000\009\000\009\000\009\000\009\000\009\000\
    \101\000\000\000\101\000\101\000\101\000\101\000\101\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \009\000\000\000\009\000\000\000\000\000\000\000\000\000\101\000\
    \000\000\101\000\009\000\101\000\000\000\000\000\000\000\000\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\000\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\000\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\001\000\001\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\000\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\001\000\001\000\032\000\032\000\032\000\032\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\101\000\009\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\101\000\101\000\050\000\050\000\050\000\000\000\009\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\101\000\050\000\
    \009\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\140\000\140\000\140\000\140\000\000\000\000\000\009\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\001\000\101\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\050\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\050\000\000\000\000\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\000\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\077\000\000\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\082\000\082\000\
    \050\000\000\000\000\000\050\000\050\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\050\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\050\000\000\000\000\000\050\000\050\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\000\000\000\000\000\000\101\000\000\000\000\000\000\000\
    \000\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\000\000\000\000\000\000\
    \000\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\112\000\112\000\000\000\
    \000\000\050\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\082\000\000\000\129\000\129\000\129\000\129\000\129\000\
    \129\000\129\000\129\000\134\000\134\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\082\000\000\000\134\000\134\000\134\000\134\000\134\000\
    \134\000\134\000\134\000\134\000\134\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\123\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check_code =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\053\000\255\255\060\000\053\000\053\000\060\000\060\000\
    \178\000\255\255\185\000\178\000\178\000\185\000\185\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \053\000\255\255\060\000\255\255\255\255\255\255\255\255\178\000\
    \255\255\185\000\033\000\160\000\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\027\000\255\255\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \028\000\255\255\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \087\000\255\255\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\089\000\089\000\090\000\090\000\
    \062\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\065\000\187\000\061\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\186\000\190\000\210\000\211\000\214\000\255\255\063\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\188\000\212\000\
    \064\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\228\000\228\000\229\000\229\000\255\255\255\255\066\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\094\000\191\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\215\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\102\000\102\000\102\000\102\000\102\000\
    \102\000\104\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\102\000\102\000\102\000\102\000\102\000\
    \102\000\133\000\255\255\255\255\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \155\000\255\255\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\156\000\255\255\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \157\000\255\255\255\255\157\000\157\000\189\000\189\000\189\000\
    \189\000\189\000\189\000\189\000\189\000\189\000\189\000\189\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\157\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\209\000\255\255\255\255\209\000\209\000\157\000\
    \157\000\157\000\157\000\157\000\157\000\157\000\157\000\157\000\
    \157\000\255\255\255\255\255\255\189\000\255\255\255\255\255\255\
    \255\255\209\000\213\000\213\000\213\000\213\000\213\000\213\000\
    \213\000\213\000\213\000\213\000\213\000\255\255\255\255\255\255\
    \255\255\209\000\209\000\209\000\209\000\209\000\209\000\209\000\
    \209\000\209\000\209\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\218\000\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\255\255\
    \255\255\213\000\219\000\219\000\219\000\219\000\219\000\219\000\
    \219\000\219\000\219\000\219\000\220\000\220\000\220\000\220\000\
    \220\000\220\000\220\000\220\000\220\000\220\000\221\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\221\000\
    \224\000\224\000\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\225\000\225\000\225\000\225\000\225\000\225\000\
    \225\000\225\000\225\000\225\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\225\000\225\000\225\000\225\000\225\000\
    \225\000\226\000\255\255\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\225\000\225\000\225\000\225\000\225\000\
    \225\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\233\000\255\255\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\235\000\235\000\235\000\
    \235\000\235\000\235\000\235\000\235\000\235\000\235\000\236\000\
    \236\000\236\000\236\000\236\000\236\000\236\000\236\000\236\000\
    \236\000\238\000\238\000\238\000\238\000\238\000\238\000\238\000\
    \238\000\238\000\238\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\238\000\238\000\238\000\238\000\238\000\238\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\238\000\238\000\238\000\238\000\238\000\238\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_code =
   "\255\001\255\255\003\255\001\255\255\002\255\255\000\002\255\000\
    \001\255\006\255\255\007\255\255\001\255\003\255\255\005\255\255\
    \004\255\255\000\004\255\000\005\255\000\003\255\000\006\255\000\
    \007\255\017\255\016\255\014\255\013\255\012\255\011\255\010\255\
    \009\255\008\255\007\255\006\255\005\255\004\255\255\019\255\018\
    \255\255\018\255\019\255\255\003\017\002\018\001\015\000\016\255\
    \022\255\019\255\255\020\255\255\000\020\255\001\019\000\014\255\
    \021\255\255\000\013\255\001\021\000\012\255\025\255\255\000\009\
    \255\019\255\022\255\255\019\255\255\024\255\255\023\255\255\001\
    \023\000\004\255\001\024\000\006\255\001\022\000\008\255\000\011\
    \255\001\025\000\010\255";
}

let rec token env lexbuf =
  lexbuf.Lexing.lex_mem <- Array.make 8 (-1) ;   __ocaml_lex_token_rec env lexbuf 0
and __ocaml_lex_token_rec env lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 786 "lexer_flow.mll"
                       (
                         Lexing.new_line lexbuf;
                         token env lexbuf
                       )
# 3142 "lexer_flow.ml"

  | 1 ->
# 790 "lexer_flow.mll"
                       ( let env = illegal env (loc_of_lexbuf env lexbuf) in
                         token env lexbuf )
# 3148 "lexer_flow.ml"

  | 2 ->
# 792 "lexer_flow.mll"
                       (
                         unicode_fix_cols lexbuf;
                         token env lexbuf )
# 3155 "lexer_flow.ml"

  | 3 ->
# 795 "lexer_flow.mll"
                       (
                         let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let env, _end = comment env buf lexbuf in
                         let env = save_comment env start _end buf true in
                         token env lexbuf
                       )
# 3166 "lexer_flow.ml"

  | 4 ->
let
# 802 "lexer_flow.mll"
                         sp
# 3172 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 2) lexbuf.Lexing.lex_mem.(0)
and
# 802 "lexer_flow.mll"
                                                             escape_type
# 3177 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos
and
# 802 "lexer_flow.mll"
                                                                             pattern
# 3182 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 803 "lexer_flow.mll"
                       (
                         if not (is_comment_syntax_enabled env) then
                           let start = loc_of_lexbuf env lexbuf in
                           let buf = Buffer.create 127 in
                           Buffer.add_string buf sp;
                           Buffer.add_string buf escape_type;
                           let env, _end = comment env buf lexbuf in
                           let env = save_comment env start _end buf true in
                           token env lexbuf
                         else
                           let env =
                             if is_in_comment_syntax env then
                               let loc = loc_of_lexbuf env lexbuf in
                               unexpected_error env loc pattern
                             else env
                           in
                           let env = in_comment_syntax true env in
                           match escape_type with
                           | ":" -> env, T_COLON
                           | _ -> token env lexbuf
                       )
# 3206 "lexer_flow.ml"

  | 5 ->
# 824 "lexer_flow.mll"
                       (
                         if is_in_comment_syntax env then
                           let env = in_comment_syntax false env in
                           token env lexbuf
                         else
                           let () = yyback 1 lexbuf in
                           env, T_MULT
                       )
# 3218 "lexer_flow.ml"

  | 6 ->
# 832 "lexer_flow.mll"
                       (
                         let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let env, _end = line_comment env buf lexbuf in
                         let env = save_comment env start _end buf false in
                         token env lexbuf
                       )
# 3229 "lexer_flow.ml"

  | 7 ->
# 841 "lexer_flow.mll"
                       ( if lexbuf.Lexing.lex_start_pos = 0
                         then begin
                           let env, _ =
                             line_comment env (Buffer.create 127) lexbuf in
                           token env lexbuf
                          end else env, T_ERROR
                       )
# 3240 "lexer_flow.ml"

  | 8 ->
let
# 849 "lexer_flow.mll"
                 quote
# 3246 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 849 "lexer_flow.mll"
                       (
                         let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let raw = Buffer.create 127 in
                         Buffer.add_char raw quote;
                         let octal = false in
                         let env, _end, octal =
                           string_quote env quote buf raw octal lexbuf in
                         env, T_STRING (Loc.btwn start _end, Buffer.contents buf, Buffer.contents raw, octal)
                       )
# 3259 "lexer_flow.ml"

  | 9 ->
# 859 "lexer_flow.mll"
                       ( let cooked = Buffer.create 127 in
                         let raw = Buffer.create 127 in
                         let literal = Buffer.create 127 in
                         Buffer.add_string literal (Lexing.lexeme lexbuf);

                         let start = loc_of_lexbuf env lexbuf in
                         let env, loc, is_tail =
                           template_part env start cooked raw literal lexbuf in
                         env, T_TEMPLATE_PART (
                           loc,
                           {
                             cooked = Buffer.contents cooked;
                             raw = Buffer.contents raw;
                             literal = Buffer.contents literal;
                           },
                           is_tail
                         )
                       )
# 3281 "lexer_flow.ml"

  | 10 ->
let
# 878 "lexer_flow.mll"
                                                     w
# 3287 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 879 "lexer_flow.mll"
                       ( illegal_number env lexbuf w (T_NUMBER BINARY) )
# 3291 "lexer_flow.ml"

  | 11 ->
# 880 "lexer_flow.mll"
                       ( env, T_NUMBER BINARY )
# 3296 "lexer_flow.ml"

  | 12 ->
let
# 881 "lexer_flow.mll"
                                                     w
# 3302 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 882 "lexer_flow.mll"
                       ( illegal_number env lexbuf w (T_NUMBER OCTAL) )
# 3306 "lexer_flow.ml"

  | 13 ->
# 883 "lexer_flow.mll"
                       ( env, T_NUMBER OCTAL )
# 3311 "lexer_flow.ml"

  | 14 ->
let
# 884 "lexer_flow.mll"
                                                           w
# 3317 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 885 "lexer_flow.mll"
                       ( illegal_number env lexbuf w (T_NUMBER LEGACY_OCTAL) )
# 3321 "lexer_flow.ml"

  | 15 ->
# 886 "lexer_flow.mll"
                       ( env, T_NUMBER LEGACY_OCTAL )
# 3326 "lexer_flow.ml"

  | 16 ->
let
# 887 "lexer_flow.mll"
                                               w
# 3332 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 888 "lexer_flow.mll"
                       ( illegal_number env lexbuf w (T_NUMBER NORMAL) )
# 3336 "lexer_flow.ml"

  | 17 ->
# 889 "lexer_flow.mll"
                       ( env, T_NUMBER NORMAL )
# 3341 "lexer_flow.ml"

  | 18 ->
let
# 890 "lexer_flow.mll"
                       w
# 3347 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 891 "lexer_flow.mll"
                       ( illegal_number env lexbuf w (T_NUMBER NORMAL) )
# 3351 "lexer_flow.ml"

  | 19 ->
# 892 "lexer_flow.mll"
                       ( env, T_NUMBER NORMAL )
# 3356 "lexer_flow.ml"

  | 20 ->
let
# 893 "lexer_flow.mll"
                                         w
# 3362 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 894 "lexer_flow.mll"
                       ( illegal_number env lexbuf w (T_NUMBER NORMAL) )
# 3366 "lexer_flow.ml"

  | 21 ->
# 896 "lexer_flow.mll"
                       ( env, T_NUMBER NORMAL )
# 3371 "lexer_flow.ml"

  | 22 ->
let
# 900 "lexer_flow.mll"
                    word
# 3377 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 901 "lexer_flow.mll"
                       (
                         unicode_fix_cols lexbuf;
                         try env, Hashtbl.find keywords word
                         with Not_found -> env, T_IDENTIFIER
                       )
# 3385 "lexer_flow.ml"

  | 23 ->
# 907 "lexer_flow.mll"
                       ( env, T_LCURLY )
# 3390 "lexer_flow.ml"

  | 24 ->
# 908 "lexer_flow.mll"
                       ( env, T_RCURLY )
# 3395 "lexer_flow.ml"

  | 25 ->
# 909 "lexer_flow.mll"
                       ( env, T_LPAREN )
# 3400 "lexer_flow.ml"

  | 26 ->
# 910 "lexer_flow.mll"
                       ( env, T_RPAREN )
# 3405 "lexer_flow.ml"

  | 27 ->
# 911 "lexer_flow.mll"
                       ( env, T_LBRACKET )
# 3410 "lexer_flow.ml"

  | 28 ->
# 912 "lexer_flow.mll"
                       ( env, T_RBRACKET )
# 3415 "lexer_flow.ml"

  | 29 ->
# 913 "lexer_flow.mll"
                       ( env, T_ELLIPSIS )
# 3420 "lexer_flow.ml"

  | 30 ->
# 914 "lexer_flow.mll"
                       ( env, T_PERIOD )
# 3425 "lexer_flow.ml"

  | 31 ->
# 915 "lexer_flow.mll"
                       ( env, T_SEMICOLON )
# 3430 "lexer_flow.ml"

  | 32 ->
# 916 "lexer_flow.mll"
                       ( env, T_COMMA )
# 3435 "lexer_flow.ml"

  | 33 ->
# 917 "lexer_flow.mll"
                       ( env, T_COLON )
# 3440 "lexer_flow.ml"

  | 34 ->
# 918 "lexer_flow.mll"
                       ( env, T_PLING )
# 3445 "lexer_flow.ml"

  | 35 ->
# 919 "lexer_flow.mll"
                       ( env, T_AND )
# 3450 "lexer_flow.ml"

  | 36 ->
# 920 "lexer_flow.mll"
                       ( env, T_OR )
# 3455 "lexer_flow.ml"

  | 37 ->
# 921 "lexer_flow.mll"
                       ( env, T_STRICT_EQUAL )
# 3460 "lexer_flow.ml"

  | 38 ->
# 922 "lexer_flow.mll"
                       ( env, T_STRICT_NOT_EQUAL )
# 3465 "lexer_flow.ml"

  | 39 ->
# 923 "lexer_flow.mll"
                       ( env, T_LESS_THAN_EQUAL )
# 3470 "lexer_flow.ml"

  | 40 ->
# 924 "lexer_flow.mll"
                       ( env, T_GREATER_THAN_EQUAL )
# 3475 "lexer_flow.ml"

  | 41 ->
# 925 "lexer_flow.mll"
                       ( env, T_EQUAL )
# 3480 "lexer_flow.ml"

  | 42 ->
# 926 "lexer_flow.mll"
                       ( env, T_NOT_EQUAL )
# 3485 "lexer_flow.ml"

  | 43 ->
# 927 "lexer_flow.mll"
                       ( env, T_INCR )
# 3490 "lexer_flow.ml"

  | 44 ->
# 928 "lexer_flow.mll"
                       ( env, T_DECR )
# 3495 "lexer_flow.ml"

  | 45 ->
# 929 "lexer_flow.mll"
                       ( env, T_LSHIFT_ASSIGN )
# 3500 "lexer_flow.ml"

  | 46 ->
# 930 "lexer_flow.mll"
                       ( env, T_LSHIFT )
# 3505 "lexer_flow.ml"

  | 47 ->
# 931 "lexer_flow.mll"
                       ( env, T_RSHIFT_ASSIGN )
# 3510 "lexer_flow.ml"

  | 48 ->
# 932 "lexer_flow.mll"
                       ( env, T_RSHIFT3_ASSIGN )
# 3515 "lexer_flow.ml"

  | 49 ->
# 933 "lexer_flow.mll"
                       ( env, T_RSHIFT3 )
# 3520 "lexer_flow.ml"

  | 50 ->
# 934 "lexer_flow.mll"
                       ( env, T_RSHIFT )
# 3525 "lexer_flow.ml"

  | 51 ->
# 935 "lexer_flow.mll"
                       ( env, T_PLUS_ASSIGN )
# 3530 "lexer_flow.ml"

  | 52 ->
# 936 "lexer_flow.mll"
                       ( env, T_MINUS_ASSIGN )
# 3535 "lexer_flow.ml"

  | 53 ->
# 937 "lexer_flow.mll"
                       ( env, T_MULT_ASSIGN )
# 3540 "lexer_flow.ml"

  | 54 ->
# 938 "lexer_flow.mll"
                       ( env, T_EXP_ASSIGN )
# 3545 "lexer_flow.ml"

  | 55 ->
# 939 "lexer_flow.mll"
                       ( env, T_MOD_ASSIGN )
# 3550 "lexer_flow.ml"

  | 56 ->
# 940 "lexer_flow.mll"
                       ( env, T_BIT_AND_ASSIGN )
# 3555 "lexer_flow.ml"

  | 57 ->
# 941 "lexer_flow.mll"
                       ( env, T_BIT_OR_ASSIGN )
# 3560 "lexer_flow.ml"

  | 58 ->
# 942 "lexer_flow.mll"
                       ( env, T_BIT_XOR_ASSIGN )
# 3565 "lexer_flow.ml"

  | 59 ->
# 943 "lexer_flow.mll"
                       ( env, T_LESS_THAN )
# 3570 "lexer_flow.ml"

  | 60 ->
# 944 "lexer_flow.mll"
                       ( env, T_GREATER_THAN )
# 3575 "lexer_flow.ml"

  | 61 ->
# 945 "lexer_flow.mll"
                       ( env, T_PLUS )
# 3580 "lexer_flow.ml"

  | 62 ->
# 946 "lexer_flow.mll"
                       ( env, T_MINUS )
# 3585 "lexer_flow.ml"

  | 63 ->
# 947 "lexer_flow.mll"
                       ( env, T_MULT )
# 3590 "lexer_flow.ml"

  | 64 ->
# 948 "lexer_flow.mll"
                       ( env, T_EXP )
# 3595 "lexer_flow.ml"

  | 65 ->
# 949 "lexer_flow.mll"
                       ( env, T_MOD )
# 3600 "lexer_flow.ml"

  | 66 ->
# 950 "lexer_flow.mll"
                       ( env, T_BIT_OR )
# 3605 "lexer_flow.ml"

  | 67 ->
# 951 "lexer_flow.mll"
                       ( env, T_BIT_AND )
# 3610 "lexer_flow.ml"

  | 68 ->
# 952 "lexer_flow.mll"
                       ( env, T_BIT_XOR )
# 3615 "lexer_flow.ml"

  | 69 ->
# 953 "lexer_flow.mll"
                       ( env, T_NOT )
# 3620 "lexer_flow.ml"

  | 70 ->
# 954 "lexer_flow.mll"
                       ( env, T_BIT_NOT )
# 3625 "lexer_flow.ml"

  | 71 ->
# 955 "lexer_flow.mll"
                       ( env, T_ASSIGN )
# 3630 "lexer_flow.ml"

  | 72 ->
# 956 "lexer_flow.mll"
                       ( env, T_ARROW )
# 3635 "lexer_flow.ml"

  | 73 ->
# 957 "lexer_flow.mll"
                       ( env, T_DIV_ASSIGN )
# 3640 "lexer_flow.ml"

  | 74 ->
# 958 "lexer_flow.mll"
                       ( env, T_DIV )
# 3645 "lexer_flow.ml"

  | 75 ->
# 959 "lexer_flow.mll"
                       ( env, T_AT )
# 3650 "lexer_flow.ml"

  | 76 ->
# 961 "lexer_flow.mll"
                       ( let env =
                           if is_in_comment_syntax env then
                             let loc = loc_of_lexbuf env lexbuf in
                             lex_error env loc Parse_error.UnexpectedEOS
                           else env
                         in
                         env, T_EOF )
# 3661 "lexer_flow.ml"

  | 77 ->
# 968 "lexer_flow.mll"
                       ( let env = illegal env (loc_of_lexbuf env lexbuf) in
                         env, T_ERROR )
# 3667 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec env lexbuf __ocaml_lex_state

and type_token env lexbuf =
  lexbuf.Lexing.lex_mem <- Array.make 26 (-1) ; (* L=14 [17] <- p ; [16] <- p ; [15] <- p ; [14] <- p ; [13] <- p ; [12] <- p ; [11] <- p ; [10] <- p ; [9] <- p ; [8] <- p ; [7] <- p ; [6] <- p ; [5] <- p ; [4] <- p ;  *)
  lexbuf.Lexing.lex_mem.(17) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(16) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(15) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(14) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(13) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(12) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(11) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(10) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(9) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(8) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(7) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(6) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(5) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
  __ocaml_lex_type_token_rec env lexbuf 133
and __ocaml_lex_type_token_rec env lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 977 "lexer_flow.mll"
                       (
                           Lexing.new_line lexbuf;
                           type_token env lexbuf
                       )
# 3697 "lexer_flow.ml"

  | 1 ->
# 981 "lexer_flow.mll"
                       (
                         unicode_fix_cols lexbuf;
                         type_token env lexbuf )
# 3704 "lexer_flow.ml"

  | 2 ->
# 984 "lexer_flow.mll"
                       ( let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let env, _end = comment env buf lexbuf in
                         let env = save_comment env start _end buf true in
                         type_token env lexbuf
                       )
# 3714 "lexer_flow.ml"

  | 3 ->
let
# 990 "lexer_flow.mll"
                         sp
# 3720 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 2) lexbuf.Lexing.lex_mem.(0)
and
# 990 "lexer_flow.mll"
                                                             escape_type
# 3725 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos
and
# 990 "lexer_flow.mll"
                                                                             pattern
# 3730 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 991 "lexer_flow.mll"
                       (
                         if not (is_comment_syntax_enabled env) then
                           let start = loc_of_lexbuf env lexbuf in
                           let buf = Buffer.create 127 in
                           Buffer.add_string buf sp;
                           Buffer.add_string buf escape_type;
                           let env, _end = comment env buf lexbuf in
                           let env = save_comment env start _end buf true in
                           type_token env lexbuf
                         else
                           let env =
                             if is_in_comment_syntax env then
                               let loc = loc_of_lexbuf env lexbuf in
                               unexpected_error env loc pattern
                             else env
                           in
                           let env = in_comment_syntax true env in
                           match escape_type with
                           | ":" -> env, T_COLON
                           | _ -> type_token env lexbuf
                       )
# 3754 "lexer_flow.ml"

  | 4 ->
# 1012 "lexer_flow.mll"
                       (
                         if is_in_comment_syntax env then
                           let env = in_comment_syntax false env in
                           type_token env lexbuf
                         else
                           let () = yyback 1 lexbuf in
                           env, T_MULT
                       )
# 3766 "lexer_flow.ml"

  | 5 ->
# 1020 "lexer_flow.mll"
                       (
                         let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let env, _end = line_comment env buf lexbuf in
                         let env = save_comment env start _end buf true in
                         type_token env lexbuf
                       )
# 3777 "lexer_flow.ml"

  | 6 ->
let
# 1027 "lexer_flow.mll"
                 quote
# 3783 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1027 "lexer_flow.mll"
                       (
                         let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let raw = Buffer.create 127 in
                         Buffer.add_char raw quote;
                         let octal = false in
                         let env, _end, octal =
                           string_quote env quote buf raw octal lexbuf in
                         env, T_STRING (Loc.btwn start _end, Buffer.contents buf, Buffer.contents raw, octal)
                       )
# 3796 "lexer_flow.ml"

  | 7 ->
let
# 1043 "lexer_flow.mll"
             neg
# 3802 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 1043 "lexer_flow.mll"
                                num
# 3807 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 1043 "lexer_flow.mll"
                                                                            w
# 3812 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_curr_pos in
# 1044 "lexer_flow.mll"
      ( illegal_number env lexbuf w (mk_num_singleton BINARY num neg) )
# 3816 "lexer_flow.ml"

  | 8 ->
let
# 1045 "lexer_flow.mll"
             neg
# 3822 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 1045 "lexer_flow.mll"
                                num
# 3827 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 1046 "lexer_flow.mll"
      ( env, mk_num_singleton BINARY num neg )
# 3831 "lexer_flow.ml"

  | 9 ->
let
# 1047 "lexer_flow.mll"
             neg
# 3837 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 1047 "lexer_flow.mll"
                                num
# 3842 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 1047 "lexer_flow.mll"
                                                                            w
# 3847 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_curr_pos in
# 1048 "lexer_flow.mll"
      ( illegal_number env lexbuf w (mk_num_singleton OCTAL num neg) )
# 3851 "lexer_flow.ml"

  | 10 ->
let
# 1049 "lexer_flow.mll"
             neg
# 3857 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 1049 "lexer_flow.mll"
                                num
# 3862 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 1050 "lexer_flow.mll"
      ( env, mk_num_singleton OCTAL num neg )
# 3866 "lexer_flow.ml"

  | 11 ->
let
# 1051 "lexer_flow.mll"
             neg
# 3872 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 1051 "lexer_flow.mll"
                                      num
# 3877 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 1051 "lexer_flow.mll"
                                                                                  w
# 3882 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_curr_pos in
# 1052 "lexer_flow.mll"
      ( illegal_number env lexbuf w (mk_num_singleton LEGACY_OCTAL num neg) )
# 3886 "lexer_flow.ml"

  | 12 ->
let
# 1053 "lexer_flow.mll"
             neg
# 3892 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 1053 "lexer_flow.mll"
                                      num
# 3897 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 1054 "lexer_flow.mll"
      ( env, mk_num_singleton LEGACY_OCTAL num neg )
# 3901 "lexer_flow.ml"

  | 13 ->
let
# 1055 "lexer_flow.mll"
             neg
# 3907 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 1055 "lexer_flow.mll"
                                num
# 3912 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 1055 "lexer_flow.mll"
                                                                      w
# 3917 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_curr_pos in
# 1056 "lexer_flow.mll"
      (
        let env, singleton =
          try env, mk_num_singleton NORMAL num neg
          with _ when Sys.win32 ->
            let loc = loc_of_lexbuf env lexbuf in
            let env = lex_error env loc Parse_error.WindowsFloatOfString in
            env, T_NUMBER_SINGLETON_TYPE (NORMAL, 789.0) in
        illegal_number env lexbuf w singleton
      )
# 3929 "lexer_flow.ml"

  | 14 ->
let
# 1065 "lexer_flow.mll"
             neg
# 3935 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 1065 "lexer_flow.mll"
                                num
# 3940 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 1066 "lexer_flow.mll"
      (
        try env, mk_num_singleton NORMAL num neg
        with _ when Sys.win32 ->
          let loc = loc_of_lexbuf env lexbuf in
          let env = lex_error env loc Parse_error.WindowsFloatOfString in
          env, T_NUMBER_SINGLETON_TYPE (NORMAL, 789.0)
      )
# 3950 "lexer_flow.ml"

  | 15 ->
let
# 1073 "lexer_flow.mll"
             neg
# 3956 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 1073 "lexer_flow.mll"
                                num
# 3961 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 1073 "lexer_flow.mll"
                                              w
# 3966 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_curr_pos in
# 1074 "lexer_flow.mll"
      ( illegal_number env lexbuf w (mk_num_singleton NORMAL num neg) )
# 3970 "lexer_flow.ml"

  | 16 ->
let
# 1075 "lexer_flow.mll"
             neg
# 3976 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 1075 "lexer_flow.mll"
                                num
# 3981 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 1076 "lexer_flow.mll"
      ( env, mk_num_singleton NORMAL num neg )
# 3985 "lexer_flow.ml"

  | 17 ->
let
# 1077 "lexer_flow.mll"
             neg
# 3991 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 1077 "lexer_flow.mll"
                                                  num
# 3996 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 1077 "lexer_flow.mll"
                                                                w
# 4001 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_curr_pos in
# 1078 "lexer_flow.mll"
      ( illegal_number env lexbuf w (mk_num_singleton NORMAL num neg) )
# 4005 "lexer_flow.ml"

  | 18 ->
let
# 1079 "lexer_flow.mll"
             neg
# 4011 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_mem.(0)
and
# 1079 "lexer_flow.mll"
                                  num
# 4016 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(3) lexbuf.Lexing.lex_mem.(2) in
# 1081 "lexer_flow.mll"
      ( env, mk_num_singleton NORMAL num neg )
# 4020 "lexer_flow.ml"

  | 19 ->
let
# 1084 "lexer_flow.mll"
            word
# 4026 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1084 "lexer_flow.mll"
                       (
                         unicode_fix_cols lexbuf;
                         try env, Hashtbl.find type_keywords word
                         with Not_found -> env, T_IDENTIFIER
                       )
# 4034 "lexer_flow.ml"

  | 20 ->
# 1090 "lexer_flow.mll"
                       ( env, T_LBRACKET )
# 4039 "lexer_flow.ml"

  | 21 ->
# 1091 "lexer_flow.mll"
                       ( env, T_RBRACKET )
# 4044 "lexer_flow.ml"

  | 22 ->
# 1092 "lexer_flow.mll"
                       ( env, T_LCURLY )
# 4049 "lexer_flow.ml"

  | 23 ->
# 1093 "lexer_flow.mll"
                       ( env, T_RCURLY )
# 4054 "lexer_flow.ml"

  | 24 ->
# 1094 "lexer_flow.mll"
                       ( env, T_LPAREN )
# 4059 "lexer_flow.ml"

  | 25 ->
# 1095 "lexer_flow.mll"
                       ( env, T_RPAREN )
# 4064 "lexer_flow.ml"

  | 26 ->
# 1096 "lexer_flow.mll"
                       ( env, T_ELLIPSIS )
# 4069 "lexer_flow.ml"

  | 27 ->
# 1097 "lexer_flow.mll"
                       ( env, T_PERIOD )
# 4074 "lexer_flow.ml"

  | 28 ->
# 1098 "lexer_flow.mll"
                       ( env, T_SEMICOLON )
# 4079 "lexer_flow.ml"

  | 29 ->
# 1099 "lexer_flow.mll"
                       ( env, T_COMMA )
# 4084 "lexer_flow.ml"

  | 30 ->
# 1100 "lexer_flow.mll"
                       ( env, T_COLON )
# 4089 "lexer_flow.ml"

  | 31 ->
# 1101 "lexer_flow.mll"
                       ( env, T_PLING )
# 4094 "lexer_flow.ml"

  | 32 ->
# 1102 "lexer_flow.mll"
                       ( env, T_LBRACKET )
# 4099 "lexer_flow.ml"

  | 33 ->
# 1103 "lexer_flow.mll"
                       ( env, T_RBRACKET )
# 4104 "lexer_flow.ml"

  | 34 ->
# 1105 "lexer_flow.mll"
                       ( env, T_LESS_THAN )
# 4109 "lexer_flow.ml"

  | 35 ->
# 1106 "lexer_flow.mll"
                       ( env, T_GREATER_THAN )
# 4114 "lexer_flow.ml"

  | 36 ->
# 1108 "lexer_flow.mll"
                       ( env, T_ASSIGN )
# 4119 "lexer_flow.ml"

  | 37 ->
# 1110 "lexer_flow.mll"
                       ( env, T_PLING )
# 4124 "lexer_flow.ml"

  | 38 ->
# 1112 "lexer_flow.mll"
                       ( env, T_MULT )
# 4129 "lexer_flow.ml"

  | 39 ->
# 1114 "lexer_flow.mll"
                       ( env, T_COLON )
# 4134 "lexer_flow.ml"

  | 40 ->
# 1116 "lexer_flow.mll"
                       ( env, T_BIT_OR )
# 4139 "lexer_flow.ml"

  | 41 ->
# 1118 "lexer_flow.mll"
                       ( env, T_BIT_AND )
# 4144 "lexer_flow.ml"

  | 42 ->
# 1120 "lexer_flow.mll"
                       ( env, T_TYPEOF )
# 4149 "lexer_flow.ml"

  | 43 ->
# 1122 "lexer_flow.mll"
                       ( env, T_ARROW )
# 4154 "lexer_flow.ml"

  | 44 ->
# 1124 "lexer_flow.mll"
                       ( env, T_ASSIGN )
# 4159 "lexer_flow.ml"

  | 45 ->
# 1126 "lexer_flow.mll"
                       ( env, T_PLUS )
# 4164 "lexer_flow.ml"

  | 46 ->
# 1127 "lexer_flow.mll"
                       ( env, T_MINUS )
# 4169 "lexer_flow.ml"

  | 47 ->
# 1130 "lexer_flow.mll"
                       ( let env =
                           if is_in_comment_syntax env then
                             let loc = loc_of_lexbuf env lexbuf in
                             lex_error env loc Parse_error.UnexpectedEOS
                           else env
                         in
                         env, T_EOF )
# 4180 "lexer_flow.ml"

  | 48 ->
# 1137 "lexer_flow.mll"
                       ( env, T_ERROR )
# 4185 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_type_token_rec env lexbuf __ocaml_lex_state

and string_quote env q buf raw octal lexbuf =
    __ocaml_lex_string_quote_rec env q buf raw octal lexbuf 247
and __ocaml_lex_string_quote_rec env q buf raw octal lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 1142 "lexer_flow.mll"
                   q'
# 4198 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1142 "lexer_flow.mll"
                         (  Buffer.add_char raw q';
                          if q = q'
                          then env, loc_of_lexbuf env lexbuf, octal
                          else begin
                            Buffer.add_char buf q';
                            string_quote env q buf raw octal lexbuf
                          end
                       )
# 4209 "lexer_flow.ml"

  | 1 ->
let
# 1150 "lexer_flow.mll"
            e
# 4215 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1150 "lexer_flow.mll"
                       ( Buffer.add_char raw e;
                         let env, octal' = string_escape env buf lexbuf in
                         let octal = octal' || octal in
                         Buffer.add_string raw (Lexing.lexeme lexbuf);
                         string_quote env q buf raw octal lexbuf )
# 4223 "lexer_flow.ml"

  | 2 ->
let
# 1155 "lexer_flow.mll"
                    x
# 4229 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1155 "lexer_flow.mll"
                       ( Buffer.add_string raw x;
                         let env = illegal env (loc_of_lexbuf env lexbuf) in
                         Buffer.add_string buf x;
                         env, loc_of_lexbuf env lexbuf, octal
                       )
# 4237 "lexer_flow.ml"

  | 3 ->
let
# 1160 "lexer_flow.mll"
         x
# 4243 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1160 "lexer_flow.mll"
                       ( Buffer.add_char raw x;
                         Buffer.add_char buf x;
                         string_quote env q buf raw octal lexbuf )
# 4249 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_string_quote_rec env q buf raw octal lexbuf __ocaml_lex_state

and string_escape env buf lexbuf =
    __ocaml_lex_string_escape_rec env buf lexbuf 252
and __ocaml_lex_string_escape_rec env buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1165 "lexer_flow.mll"
                      ( env, false )
# 4261 "lexer_flow.ml"

  | 1 ->
# 1166 "lexer_flow.mll"
                      ( Buffer.add_string buf "\\";
                        env, false )
# 4267 "lexer_flow.ml"

  | 2 ->
let
# 1168 "lexer_flow.mll"
                a
# 4273 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 1168 "lexer_flow.mll"
                           b
# 4278 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
# 1169 "lexer_flow.mll"
                      ( let code = hexa_to_int a * 16 + hexa_to_int b in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        env, false )
# 4284 "lexer_flow.ml"

  | 3 ->
let
# 1172 "lexer_flow.mll"
                  a
# 4290 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos
and
# 1172 "lexer_flow.mll"
                                   b
# 4295 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 1172 "lexer_flow.mll"
                                                    c
# 4300 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
# 1173 "lexer_flow.mll"
                      ( let code =
                          (oct_to_int a lsl 6) +
                          (oct_to_int b lsl 3) +
                          (oct_to_int c) in
                        (* If the 3 character octal code is larger than 256
                         * then it is parsed as a 2 character octal code *)
                        if code < 256
                        then List.iter (Buffer.add_char buf) (utf16to8 code)
                        else begin
                          let code =
                            (oct_to_int a lsl 3) +
                            (oct_to_int b) in
                          List.iter (Buffer.add_char buf) (utf16to8 code);
                          Buffer.add_char buf c
                        end;
                        env, true
                      )
# 4320 "lexer_flow.ml"

  | 4 ->
let
# 1190 "lexer_flow.mll"
                  a
# 4326 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos
and
# 1190 "lexer_flow.mll"
                                   b
# 4331 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 1191 "lexer_flow.mll"
                      ( let code =
                          (oct_to_int a lsl 3) +
                          (oct_to_int b) in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        env, true
                      )
# 4340 "lexer_flow.ml"

  | 5 ->
# 1197 "lexer_flow.mll"
                      ( Buffer.add_char buf (Char.chr 0x0); env, false )
# 4345 "lexer_flow.ml"

  | 6 ->
# 1198 "lexer_flow.mll"
                      ( Buffer.add_char buf (Char.chr 0x8); env, false )
# 4350 "lexer_flow.ml"

  | 7 ->
# 1199 "lexer_flow.mll"
                      ( Buffer.add_char buf (Char.chr 0xC); env, false )
# 4355 "lexer_flow.ml"

  | 8 ->
# 1200 "lexer_flow.mll"
                      ( Buffer.add_char buf (Char.chr 0xA); env, false )
# 4360 "lexer_flow.ml"

  | 9 ->
# 1201 "lexer_flow.mll"
                      ( Buffer.add_char buf (Char.chr 0xD); env, false )
# 4365 "lexer_flow.ml"

  | 10 ->
# 1202 "lexer_flow.mll"
                      ( Buffer.add_char buf (Char.chr 0x9); env, false )
# 4370 "lexer_flow.ml"

  | 11 ->
# 1203 "lexer_flow.mll"
                      ( Buffer.add_char buf (Char.chr 0xB); env, false )
# 4375 "lexer_flow.ml"

  | 12 ->
let
# 1204 "lexer_flow.mll"
                  a
# 4381 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1205 "lexer_flow.mll"
                      ( let code = oct_to_int a in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        env, true
                      )
# 4388 "lexer_flow.ml"

  | 13 ->
let
# 1209 "lexer_flow.mll"
                a
# 4394 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 1209 "lexer_flow.mll"
                           b
# 4399 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 1209 "lexer_flow.mll"
                                      c
# 4404 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and
# 1209 "lexer_flow.mll"
                                                 d
# 4409 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4) in
# 1210 "lexer_flow.mll"
                      ( let code =
                          (hexa_to_int a lsl 12) +
                          (hexa_to_int b lsl 8) +
                          (hexa_to_int c lsl 4) +
                          (hexa_to_int d) in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        env, false
                      )
# 4420 "lexer_flow.ml"

  | 14 ->
let
# 1218 "lexer_flow.mll"
                  hex_code
# 4426 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 2) (lexbuf.Lexing.lex_curr_pos + -1) in
# 1219 "lexer_flow.mll"
                      (
                        let code = int_of_string ("0x"^hex_code) in
                        (* 11.8.4.1 *)
                        let env = if code > 1114111
                          then illegal env (loc_of_lexbuf env lexbuf)
                          else env
                        in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        env, false
                      )
# 4439 "lexer_flow.ml"

  | 15 ->
let
# 1229 "lexer_flow.mll"
                       c
# 4445 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1230 "lexer_flow.mll"
                      ( let env = illegal env (loc_of_lexbuf env lexbuf) in
                        Buffer.add_char buf c;
                        env, false )
# 4451 "lexer_flow.ml"

  | 16 ->
# 1234 "lexer_flow.mll"
                      ( Lexing.new_line lexbuf; env, false )
# 4456 "lexer_flow.ml"

  | 17 ->
let
# 1235 "lexer_flow.mll"
         c
# 4462 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1235 "lexer_flow.mll"
                      ( Buffer.add_char buf c; env, false )
# 4466 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_string_escape_rec env buf lexbuf __ocaml_lex_state

and comment env buf lexbuf =
    __ocaml_lex_comment_rec env buf lexbuf 279
and __ocaml_lex_comment_rec env buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1238 "lexer_flow.mll"
                       ( let env = illegal env (loc_of_lexbuf env lexbuf) in
                         env, loc_of_lexbuf env lexbuf )
# 4479 "lexer_flow.ml"

  | 1 ->
# 1240 "lexer_flow.mll"
                       ( Lexing.new_line lexbuf;
                         Buffer.add_char buf '\n';
                         comment env buf lexbuf )
# 4486 "lexer_flow.ml"

  | 2 ->
# 1243 "lexer_flow.mll"
                       (
                         let loc = loc_of_lexbuf env lexbuf in
                         let env = if is_in_comment_syntax env
                           then unexpected_error_w_suggest env loc "*/" "*-/"
                           else env
                         in
                         env, loc
                       )
# 4498 "lexer_flow.ml"

  | 3 ->
# 1251 "lexer_flow.mll"
                       (
                         if is_in_comment_syntax env
                         then env, loc_of_lexbuf env lexbuf
                         else (
                           Buffer.add_string buf "*-/";
                           comment env buf lexbuf
                         )
                       )
# 4510 "lexer_flow.ml"

  | 4 ->
let
# 1259 "lexer_flow.mll"
          c
# 4516 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1259 "lexer_flow.mll"
                       ( Buffer.add_char buf c;
                         comment env buf lexbuf )
# 4521 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec env buf lexbuf __ocaml_lex_state

and line_comment env buf lexbuf =
    __ocaml_lex_line_comment_rec env buf lexbuf 287
and __ocaml_lex_line_comment_rec env buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1263 "lexer_flow.mll"
                       ( env, loc_of_lexbuf env lexbuf )
# 4533 "lexer_flow.ml"

  | 1 ->
# 1264 "lexer_flow.mll"
                       ( let open Loc in
                         let { source; start; _end = { line; column; offset } }
                           = loc_of_lexbuf env lexbuf in
                         Lexing.new_line lexbuf;
                         let _end = {
                           line;
                           column = column - 1;
                           offset = offset - 1;
                         } in
                         env, { source; start; _end; }
                       )
# 4548 "lexer_flow.ml"

  | 2 ->
let
# 1275 "lexer_flow.mll"
         c
# 4554 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1275 "lexer_flow.mll"
                       ( Buffer.add_char buf c;
                         line_comment env buf lexbuf )
# 4559 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_line_comment_rec env buf lexbuf __ocaml_lex_state

and regexp env lexbuf =
    __ocaml_lex_regexp_rec env lexbuf 291
and __ocaml_lex_regexp_rec env lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1279 "lexer_flow.mll"
                      ( env, T_EOF )
# 4571 "lexer_flow.ml"

  | 1 ->
# 1281 "lexer_flow.mll"
                      ( Lexing.new_line lexbuf;
                        regexp env lexbuf )
# 4577 "lexer_flow.ml"

  | 2 ->
# 1283 "lexer_flow.mll"
                      ( unicode_fix_cols lexbuf;
                        regexp env lexbuf )
# 4583 "lexer_flow.ml"

  | 3 ->
# 1285 "lexer_flow.mll"
                      ( let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = line_comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        regexp env lexbuf )
# 4592 "lexer_flow.ml"

  | 4 ->
# 1290 "lexer_flow.mll"
                      ( let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        regexp env lexbuf )
# 4601 "lexer_flow.ml"

  | 5 ->
# 1295 "lexer_flow.mll"
                      ( let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, flags = regexp_body env buf lexbuf in
                        let end_ = loc_of_lexbuf env lexbuf in
                        let loc = Loc.btwn start end_ in
                        env, T_REGEXP (loc, Buffer.contents buf, flags) )
# 4611 "lexer_flow.ml"

  | 6 ->
# 1301 "lexer_flow.mll"
                      ( let env = illegal env (loc_of_lexbuf env lexbuf) in
                        env, T_ERROR )
# 4617 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_regexp_rec env lexbuf __ocaml_lex_state

and regexp_body env buf lexbuf =
    __ocaml_lex_regexp_body_rec env buf lexbuf 314
and __ocaml_lex_regexp_body_rec env buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1305 "lexer_flow.mll"
                      ( let loc = loc_of_lexbuf env lexbuf in
                        let env = lex_error env loc Parse_error.UnterminatedRegExp in
                        env, "" )
# 4631 "lexer_flow.ml"

  | 1 ->
# 1309 "lexer_flow.mll"
                      ( let loc = loc_of_lexbuf env lexbuf in
                        let env = lex_error env loc Parse_error.UnterminatedRegExp in
                        env, "" )
# 4638 "lexer_flow.ml"

  | 2 ->
let
# 1312 "lexer_flow.mll"
                  s
# 4644 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 1312 "lexer_flow.mll"
                      ( Buffer.add_string buf s;
                        regexp_body env buf lexbuf )
# 4649 "lexer_flow.ml"

  | 3 ->
let
# 1314 "lexer_flow.mll"
                    flags
# 4655 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) lexbuf.Lexing.lex_curr_pos in
# 1315 "lexer_flow.mll"
                      ( env, flags )
# 4659 "lexer_flow.ml"

  | 4 ->
# 1316 "lexer_flow.mll"
                      ( env, "" )
# 4664 "lexer_flow.ml"

  | 5 ->
let
# 1317 "lexer_flow.mll"
           c
# 4670 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1317 "lexer_flow.mll"
                      ( Buffer.add_char buf c;
                        let env = regexp_class env buf lexbuf in
                        regexp_body env buf lexbuf )
# 4676 "lexer_flow.ml"

  | 6 ->
# 1321 "lexer_flow.mll"
                      ( let loc = loc_of_lexbuf env lexbuf in
                        let env = lex_error env loc Parse_error.UnterminatedRegExp in
                        env, "" )
# 4683 "lexer_flow.ml"

  | 7 ->
let
# 1324 "lexer_flow.mll"
         c
# 4689 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1324 "lexer_flow.mll"
                      ( Buffer.add_char buf c;
                        regexp_body env buf lexbuf )
# 4694 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_regexp_body_rec env buf lexbuf __ocaml_lex_state

and regexp_class env buf lexbuf =
    __ocaml_lex_regexp_class_rec env buf lexbuf 326
and __ocaml_lex_regexp_class_rec env buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1328 "lexer_flow.mll"
                      ( env )
# 4706 "lexer_flow.ml"

  | 1 ->
let
# 1329 "lexer_flow.mll"
              s
# 4712 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 1329 "lexer_flow.mll"
                      ( Buffer.add_string buf s;
                        regexp_class env buf lexbuf )
# 4717 "lexer_flow.ml"

  | 2 ->
let
# 1331 "lexer_flow.mll"
                    s
# 4723 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 1331 "lexer_flow.mll"
                      ( Buffer.add_string buf s;
                        regexp_class env buf lexbuf )
# 4728 "lexer_flow.ml"

  | 3 ->
let
# 1333 "lexer_flow.mll"
           c
# 4734 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1333 "lexer_flow.mll"
                      ( Buffer.add_char buf c; env )
# 4738 "lexer_flow.ml"

  | 4 ->
let
# 1334 "lexer_flow.mll"
         c
# 4744 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1334 "lexer_flow.mll"
                      ( Buffer.add_char buf c;
                        regexp_class env buf lexbuf )
# 4749 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_regexp_class_rec env buf lexbuf __ocaml_lex_state

and jsx_tag env lexbuf =
    __ocaml_lex_jsx_tag_rec env lexbuf 333
and __ocaml_lex_jsx_tag_rec env lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1338 "lexer_flow.mll"
                      ( env, T_EOF )
# 4761 "lexer_flow.ml"

  | 1 ->
# 1340 "lexer_flow.mll"
                      ( Lexing.new_line lexbuf;
                        jsx_tag env lexbuf )
# 4767 "lexer_flow.ml"

  | 2 ->
# 1342 "lexer_flow.mll"
                      ( unicode_fix_cols lexbuf;
                        jsx_tag env lexbuf )
# 4773 "lexer_flow.ml"

  | 3 ->
# 1344 "lexer_flow.mll"
                      ( let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = line_comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        jsx_tag env lexbuf )
# 4782 "lexer_flow.ml"

  | 4 ->
# 1349 "lexer_flow.mll"
                      ( let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        jsx_tag env lexbuf )
# 4791 "lexer_flow.ml"

  | 5 ->
# 1354 "lexer_flow.mll"
                      ( env, T_LESS_THAN )
# 4796 "lexer_flow.ml"

  | 6 ->
# 1355 "lexer_flow.mll"
                      ( env, T_DIV )
# 4801 "lexer_flow.ml"

  | 7 ->
# 1356 "lexer_flow.mll"
                      ( env, T_GREATER_THAN )
# 4806 "lexer_flow.ml"

  | 8 ->
# 1357 "lexer_flow.mll"
                      ( env, T_LCURLY )
# 4811 "lexer_flow.ml"

  | 9 ->
# 1358 "lexer_flow.mll"
                      ( env, T_COLON )
# 4816 "lexer_flow.ml"

  | 10 ->
# 1359 "lexer_flow.mll"
                      ( env, T_PERIOD )
# 4821 "lexer_flow.ml"

  | 11 ->
# 1360 "lexer_flow.mll"
                      ( env, T_ASSIGN )
# 4826 "lexer_flow.ml"

  | 12 ->
# 1362 "lexer_flow.mll"
                      ( unicode_fix_cols lexbuf;
                        env, T_JSX_IDENTIFIER )
# 4832 "lexer_flow.ml"

  | 13 ->
let
# 1364 "lexer_flow.mll"
                  quote
# 4838 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1365 "lexer_flow.mll"
                      (
                        let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let raw = Buffer.create 127 in
                        Buffer.add_char raw quote;
                        let mode = if quote = '\''
                          then JSX_SINGLE_QUOTED_TEXT
                          else JSX_DOUBLE_QUOTED_TEXT in
                        let env, _end = jsx_text env mode buf raw lexbuf in
                        Buffer.add_char raw quote;
                        let value = Buffer.contents buf in
                        let raw = Buffer.contents raw in
                        env, T_JSX_TEXT (Loc.btwn start _end, value, raw)
                      )
# 4855 "lexer_flow.ml"

  | 14 ->
# 1379 "lexer_flow.mll"
                      ( env, T_ERROR )
# 4860 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_jsx_tag_rec env lexbuf __ocaml_lex_state

and jsx_child env start buf raw lexbuf =
    __ocaml_lex_jsx_child_rec env start buf raw lexbuf 364
and __ocaml_lex_jsx_child_rec env start buf raw lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 1387 "lexer_flow.mll"
                                lt
# 4873 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1388 "lexer_flow.mll"
                      ( Buffer.add_string raw lt;
                        Buffer.add_string buf lt;
                        Lexing.new_line lexbuf;
                        let env, _end =
                          jsx_text env JSX_CHILD_TEXT buf raw lexbuf in
                        let value = Buffer.contents buf in
                        let raw = Buffer.contents raw in
                        env, T_JSX_TEXT (Loc.btwn start _end, value, raw)
                      )
# 4885 "lexer_flow.ml"

  | 1 ->
# 1397 "lexer_flow.mll"
                      ( env, T_EOF )
# 4890 "lexer_flow.ml"

  | 2 ->
# 1398 "lexer_flow.mll"
                      ( env, T_LESS_THAN )
# 4895 "lexer_flow.ml"

  | 3 ->
# 1399 "lexer_flow.mll"
                      ( env, T_LCURLY )
# 4900 "lexer_flow.ml"

  | 4 ->
let
# 1400 "lexer_flow.mll"
         c
# 4906 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1400 "lexer_flow.mll"
                      ( Buffer.add_char raw c;
                        Buffer.add_char buf c;
                        let env, _end =
                          jsx_text env JSX_CHILD_TEXT buf raw lexbuf in
                        let value = Buffer.contents buf in
                        let raw = Buffer.contents raw in
                        env, T_JSX_TEXT (Loc.btwn start _end, value, raw)
                      )
# 4917 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_jsx_child_rec env start buf raw lexbuf __ocaml_lex_state

and jsx_text env mode buf raw lexbuf =
    __ocaml_lex_jsx_text_rec env mode buf raw lexbuf 371
and __ocaml_lex_jsx_text_rec env mode buf raw lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 1410 "lexer_flow.mll"
                         c
# 4930 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1411 "lexer_flow.mll"
                      ( match mode, c with
                        | JSX_SINGLE_QUOTED_TEXT, '\''
                        | JSX_DOUBLE_QUOTED_TEXT, '"' ->
                            env, loc_of_lexbuf env lexbuf
                        | JSX_CHILD_TEXT, ('<' | '{') ->
                            (* Don't actually want to consume these guys
                             * yet...they're not part of the JSX text *)
                            back lexbuf;
                            env, loc_of_lexbuf env lexbuf
                        | _ ->
                            Buffer.add_char raw c;
                            Buffer.add_char buf c;
                            jsx_text env mode buf raw lexbuf
                      )
# 4947 "lexer_flow.ml"

  | 1 ->
# 1425 "lexer_flow.mll"
                      ( let env = illegal env (loc_of_lexbuf env lexbuf) in
                        env, loc_of_lexbuf env lexbuf
                      )
# 4954 "lexer_flow.ml"

  | 2 ->
let
# 1428 "lexer_flow.mll"
                                lt
# 4960 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1429 "lexer_flow.mll"
                      ( Buffer.add_string raw lt;
                        Buffer.add_string buf lt;
                        Lexing.new_line lexbuf;
                        jsx_text env mode buf raw lexbuf
                      )
# 4968 "lexer_flow.ml"

  | 3 ->
let
# 1434 "lexer_flow.mll"
                   n
# 4974 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 3) (lexbuf.Lexing.lex_curr_pos + -1)
and
# 1434 "lexer_flow.mll"
                             s
# 4979 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1435 "lexer_flow.mll"
                      ( Buffer.add_string raw s;
                        let code = int_of_string ("0x" ^ n) in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        jsx_text env mode buf raw lexbuf
                      )
# 4987 "lexer_flow.ml"

  | 4 ->
let
# 1440 "lexer_flow.mll"
                    n
# 4993 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 2) (lexbuf.Lexing.lex_curr_pos + -1)
and
# 1440 "lexer_flow.mll"
                              s
# 4998 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1441 "lexer_flow.mll"
                      ( Buffer.add_string raw s;
                        let code = int_of_string n in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        jsx_text env mode buf raw lexbuf
                      )
# 5006 "lexer_flow.ml"

  | 5 ->
let
# 1446 "lexer_flow.mll"
                       entity
# 5012 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) (lexbuf.Lexing.lex_curr_pos + -1)
and
# 1446 "lexer_flow.mll"
                                      s
# 5017 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1447 "lexer_flow.mll"
                      (
                        Buffer.add_string raw s;
                        let code = match entity with
                        | "quot" -> Some 0x0022
                        | "amp" -> Some 0x0026
                        | "apos" -> Some 0x0027
                        | "lt" -> Some 0x003C
                        | "gt" -> Some 0x003E
                        | "nbsp" -> Some 0x00A0
                        | "iexcl" -> Some 0x00A1
                        | "cent" -> Some 0x00A2
                        | "pound" -> Some 0x00A3
                        | "curren" -> Some 0x00A4
                        | "yen" -> Some 0x00A5
                        | "brvbar" -> Some 0x00A6
                        | "sect" -> Some 0x00A7
                        | "uml" -> Some 0x00A8
                        | "copy" -> Some 0x00A9
                        | "ordf" -> Some 0x00AA
                        | "laquo" -> Some 0x00AB
                        | "not" -> Some 0x00AC
                        | "shy" -> Some 0x00AD
                        | "reg" -> Some 0x00AE
                        | "macr" -> Some 0x00AF
                        | "deg" -> Some 0x00B0
                        | "plusmn" -> Some 0x00B1
                        | "sup2" -> Some 0x00B2
                        | "sup3" -> Some 0x00B3
                        | "acute" -> Some 0x00B4
                        | "micro" -> Some 0x00B5
                        | "para" -> Some 0x00B6
                        | "middot" -> Some 0x00B7
                        | "cedil" -> Some 0x00B8
                        | "sup1" -> Some 0x00B9
                        | "ordm" -> Some 0x00BA
                        | "raquo" -> Some 0x00BB
                        | "frac14" -> Some 0x00BC
                        | "frac12" -> Some 0x00BD
                        | "frac34" -> Some 0x00BE
                        | "iquest" -> Some 0x00BF
                        | "Agrave" -> Some 0x00C0
                        | "Aacute" -> Some 0x00C1
                        | "Acirc" -> Some 0x00C2
                        | "Atilde" -> Some 0x00C3
                        | "Auml" -> Some 0x00C4
                        | "Aring" -> Some 0x00C5
                        | "AElig" -> Some 0x00C6
                        | "Ccedil" -> Some 0x00C7
                        | "Egrave" -> Some 0x00C8
                        | "Eacute" -> Some 0x00C9
                        | "Ecirc" -> Some 0x00CA
                        | "Euml" -> Some 0x00CB
                        | "Igrave" -> Some 0x00CC
                        | "Iacute" -> Some 0x00CD
                        | "Icirc" -> Some 0x00CE
                        | "Iuml" -> Some 0x00CF
                        | "ETH" -> Some 0x00D0
                        | "Ntilde" -> Some 0x00D1
                        | "Ograve" -> Some 0x00D2
                        | "Oacute" -> Some 0x00D3
                        | "Ocirc" -> Some 0x00D4
                        | "Otilde" -> Some 0x00D5
                        | "Ouml" -> Some 0x00D6
                        | "times" -> Some 0x00D7
                        | "Oslash" -> Some 0x00D8
                        | "Ugrave" -> Some 0x00D9
                        | "Uacute" -> Some 0x00DA
                        | "Ucirc" -> Some 0x00DB
                        | "Uuml" -> Some 0x00DC
                        | "Yacute" -> Some 0x00DD
                        | "THORN" -> Some 0x00DE
                        | "szlig" -> Some 0x00DF
                        | "agrave" -> Some 0x00E0
                        | "aacute" -> Some 0x00E1
                        | "acirc" -> Some 0x00E2
                        | "atilde" -> Some 0x00E3
                        | "auml" -> Some 0x00E4
                        | "aring" -> Some 0x00E5
                        | "aelig" -> Some 0x00E6
                        | "ccedil" -> Some 0x00E7
                        | "egrave" -> Some 0x00E8
                        | "eacute" -> Some 0x00E9
                        | "ecirc" -> Some 0x00EA
                        | "euml" -> Some 0x00EB
                        | "igrave" -> Some 0x00EC
                        | "iacute" -> Some 0x00ED
                        | "icirc" -> Some 0x00EE
                        | "iuml" -> Some 0x00EF
                        | "eth" -> Some 0x00F0
                        | "ntilde" -> Some 0x00F1
                        | "ograve" -> Some 0x00F2
                        | "oacute" -> Some 0x00F3
                        | "ocirc" -> Some 0x00F4
                        | "otilde" -> Some 0x00F5
                        | "ouml" -> Some 0x00F6
                        | "divide" -> Some 0x00F7
                        | "oslash" -> Some 0x00F8
                        | "ugrave" -> Some 0x00F9
                        | "uacute" -> Some 0x00FA
                        | "ucirc" -> Some 0x00FB
                        | "uuml" -> Some 0x00FC
                        | "yacute" -> Some 0x00FD
                        | "thorn" -> Some 0x00FE
                        | "yuml" -> Some 0x00FF
                        | "OElig" -> Some 0x0152
                        | "oelig" -> Some 0x0153
                        | "Scaron" -> Some 0x0160
                        | "scaron" -> Some 0x0161
                        | "Yuml" -> Some 0x0178
                        | "fnof" -> Some 0x0192
                        | "circ" -> Some 0x02C6
                        | "tilde" -> Some 0x02DC
                        | "Alpha" -> Some 0x0391
                        | "Beta" -> Some 0x0392
                        | "Gamma" -> Some 0x0393
                        | "Delta" -> Some 0x0394
                        | "Epsilon" -> Some 0x0395
                        | "Zeta" -> Some 0x0396
                        | "Eta" -> Some 0x0397
                        | "Theta" -> Some 0x0398
                        | "Iota" -> Some 0x0399
                        | "Kappa" -> Some 0x039A
                        | "Lambda" -> Some 0x039B
                        | "Mu" -> Some 0x039C
                        | "Nu" -> Some 0x039D
                        | "Xi" -> Some 0x039E
                        | "Omicron" -> Some 0x039F
                        | "Pi" -> Some 0x03A0
                        | "Rho" -> Some 0x03A1
                        | "Sigma" -> Some 0x03A3
                        | "Tau" -> Some 0x03A4
                        | "Upsilon" -> Some 0x03A5
                        | "Phi" -> Some 0x03A6
                        | "Chi" -> Some 0x03A7
                        | "Psi" -> Some 0x03A8
                        | "Omega" -> Some 0x03A9
                        | "alpha" -> Some 0x03B1
                        | "beta" -> Some 0x03B2
                        | "gamma" -> Some 0x03B3
                        | "delta" -> Some 0x03B4
                        | "epsilon" -> Some 0x03B5
                        | "zeta" -> Some 0x03B6
                        | "eta" -> Some 0x03B7
                        | "theta" -> Some 0x03B8
                        | "iota" -> Some 0x03B9
                        | "kappa" -> Some 0x03BA
                        | "lambda" -> Some 0x03BB
                        | "mu" -> Some 0x03BC
                        | "nu" -> Some 0x03BD
                        | "xi" -> Some 0x03BE
                        | "omicron" -> Some 0x03BF
                        | "pi" -> Some 0x03C0
                        | "rho" -> Some 0x03C1
                        | "sigmaf" -> Some 0x03C2
                        | "sigma" -> Some 0x03C3
                        | "tau" -> Some 0x03C4
                        | "upsilon" -> Some 0x03C5
                        | "phi" -> Some 0x03C6
                        | "chi" -> Some 0x03C7
                        | "psi" -> Some 0x03C8
                        | "omega" -> Some 0x03C9
                        | "thetasym" -> Some 0x03D1
                        | "upsih" -> Some 0x03D2
                        | "piv" -> Some 0x03D6
                        | "ensp" -> Some 0x2002
                        | "emsp" -> Some 0x2003
                        | "thinsp" -> Some 0x2009
                        | "zwnj" -> Some 0x200C
                        | "zwj" -> Some 0x200D
                        | "lrm" -> Some 0x200E
                        | "rlm" -> Some 0x200F
                        | "ndash" -> Some 0x2013
                        | "mdash" -> Some 0x2014
                        | "lsquo" -> Some 0x2018
                        | "rsquo" -> Some 0x2019
                        | "sbquo" -> Some 0x201A
                        | "ldquo" -> Some 0x201C
                        | "rdquo" -> Some 0x201D
                        | "bdquo" -> Some 0x201E
                        | "dagger" -> Some 0x2020
                        | "Dagger" -> Some 0x2021
                        | "bull" -> Some 0x2022
                        | "hellip" -> Some 0x2026
                        | "permil" -> Some 0x2030
                        | "prime" -> Some 0x2032
                        | "Prime" -> Some 0x2033
                        | "lsaquo" -> Some 0x2039
                        | "rsaquo" -> Some 0x203A
                        | "oline" -> Some 0x203E
                        | "frasl" -> Some 0x2044
                        | "euro" -> Some 0x20AC
                        | "image" -> Some 0x2111
                        | "weierp" -> Some 0x2118
                        | "real" -> Some 0x211C
                        | "trade" -> Some 0x2122
                        | "alefsym" -> Some 0x2135
                        | "larr" -> Some 0x2190
                        | "uarr" -> Some 0x2191
                        | "rarr" -> Some 0x2192
                        | "darr" -> Some 0x2193
                        | "harr" -> Some 0x2194
                        | "crarr" -> Some 0x21B5
                        | "lArr" -> Some 0x21D0
                        | "uArr" -> Some 0x21D1
                        | "rArr" -> Some 0x21D2
                        | "dArr" -> Some 0x21D3
                        | "hArr" -> Some 0x21D4
                        | "forall" -> Some 0x2200
                        | "part" -> Some 0x2202
                        | "exist" -> Some 0x2203
                        | "empty" -> Some 0x2205
                        | "nabla" -> Some 0x2207
                        | "isin" -> Some 0x2208
                        | "notin" -> Some 0x2209
                        | "ni" -> Some 0x220B
                        | "prod" -> Some 0x220F
                        | "sum" -> Some 0x2211
                        | "minus" -> Some 0x2212
                        | "lowast" -> Some 0x2217
                        | "radic" -> Some 0x221A
                        | "prop" -> Some 0x221D
                        | "infin" -> Some 0x221E
                        | "ang" -> Some 0x2220
                        | "and" -> Some 0x2227
                        | "or" -> Some 0x2228
                        | "cap" -> Some 0x2229
                        | "cup" -> Some 0x222A
                        | "'int'" -> Some 0x222B
                        | "there4" -> Some 0x2234
                        | "sim" -> Some 0x223C
                        | "cong" -> Some 0x2245
                        | "asymp" -> Some 0x2248
                        | "ne" -> Some 0x2260
                        | "equiv" -> Some 0x2261
                        | "le" -> Some 0x2264
                        | "ge" -> Some 0x2265
                        | "sub" -> Some 0x2282
                        | "sup" -> Some 0x2283
                        | "nsub" -> Some 0x2284
                        | "sube" -> Some 0x2286
                        | "supe" -> Some 0x2287
                        | "oplus" -> Some 0x2295
                        | "otimes" -> Some 0x2297
                        | "perp" -> Some 0x22A5
                        | "sdot" -> Some 0x22C5
                        | "lceil" -> Some 0x2308
                        | "rceil" -> Some 0x2309
                        | "lfloor" -> Some 0x230A
                        | "rfloor" -> Some 0x230B
                        | "lang" -> Some 0x27E8 (* 0x2329 in HTML4 *)
                        | "rang" -> Some 0x27E9 (* 0x232A in HTML4 *)
                        | "loz" -> Some 0x25CA
                        | "spades" -> Some 0x2660
                        | "clubs" -> Some 0x2663
                        | "hearts" -> Some 0x2665
                        | "diams" -> Some 0x2666
                        | _ -> None in
                        (match code with
                        | Some code -> List.iter (Buffer.add_char buf) (utf16to8 code)
                        | None -> Buffer.add_string buf ("&" ^ entity ^";"));
                        jsx_text env mode buf raw lexbuf
                      )
# 5282 "lexer_flow.ml"

  | 6 ->
let
# 1709 "lexer_flow.mll"
         c
# 5288 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1709 "lexer_flow.mll"
                      ( Buffer.add_char raw c;
                        Buffer.add_char buf c;
                        jsx_text env mode buf raw lexbuf )
# 5294 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_jsx_text_rec env mode buf raw lexbuf __ocaml_lex_state

and template_tail env lexbuf =
    __ocaml_lex_template_tail_rec env lexbuf 393
and __ocaml_lex_template_tail_rec env lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1715 "lexer_flow.mll"
                      ( Lexing.new_line lexbuf;
                        template_tail env lexbuf )
# 5307 "lexer_flow.ml"

  | 1 ->
# 1717 "lexer_flow.mll"
                      ( unicode_fix_cols lexbuf;
                        template_tail env lexbuf )
# 5313 "lexer_flow.ml"

  | 2 ->
# 1719 "lexer_flow.mll"
                      ( let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = line_comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        template_tail env lexbuf )
# 5322 "lexer_flow.ml"

  | 3 ->
# 1724 "lexer_flow.mll"
                      ( let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        template_tail env lexbuf )
# 5331 "lexer_flow.ml"

  | 4 ->
# 1729 "lexer_flow.mll"
                      ( let start = loc_of_lexbuf env lexbuf in
                        let cooked = Buffer.create 127 in
                        let raw = Buffer.create 127 in
                        let literal = Buffer.create 127 in
                        Buffer.add_string literal "}";
                        let env, loc, is_tail =
                          template_part env start cooked raw literal lexbuf in
                        env, (T_TEMPLATE_PART (loc, {
                          cooked = Buffer.contents cooked;
                          raw = Buffer.contents raw;
                          literal = Buffer.contents literal;
                        }, is_tail))
                      )
# 5348 "lexer_flow.ml"

  | 5 ->
# 1742 "lexer_flow.mll"
                      ( let env = illegal env (loc_of_lexbuf env lexbuf) in
                        env, (T_TEMPLATE_PART (
                          loc_of_lexbuf env lexbuf,
                          { cooked = ""; raw = ""; literal = ""; },
                          true
                        ))
                      )
# 5359 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_template_tail_rec env lexbuf __ocaml_lex_state

and template_part env start cooked raw literal lexbuf =
    __ocaml_lex_template_part_rec env start cooked raw literal lexbuf 416
and __ocaml_lex_template_part_rec env start cooked raw literal lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1751 "lexer_flow.mll"
                      ( let env = illegal env (loc_of_lexbuf env lexbuf) in
                        env, Loc.btwn start (loc_of_lexbuf env lexbuf), true )
# 5372 "lexer_flow.ml"

  | 1 ->
# 1753 "lexer_flow.mll"
                      ( Buffer.add_char literal '`';
                        env, Loc.btwn start (loc_of_lexbuf env lexbuf), true )
# 5378 "lexer_flow.ml"

  | 2 ->
# 1755 "lexer_flow.mll"
                      ( Buffer.add_string literal "${";
                        env, Loc.btwn start (loc_of_lexbuf env lexbuf), false )
# 5384 "lexer_flow.ml"

  | 3 ->
# 1757 "lexer_flow.mll"
                      ( Buffer.add_char raw '\\';
                        Buffer.add_char literal '\\';
                        let env, _ = string_escape env cooked lexbuf in
                        let str = Lexing.lexeme lexbuf in
                        Buffer.add_string raw str;
                        Buffer.add_string literal str;
                        template_part env start cooked raw literal lexbuf )
# 5395 "lexer_flow.ml"

  | 4 ->
let
# 1767 "lexer_flow.mll"
              lf
# 5401 "lexer_flow.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 1768 "lexer_flow.mll"
                      ( Buffer.add_string raw lf;
                        Buffer.add_string literal lf;
                        Buffer.add_string cooked "\n";
                        Lexing.new_line lexbuf;
                        template_part env start cooked raw literal lexbuf )
# 5409 "lexer_flow.ml"

  | 5 ->
let
# 1773 "lexer_flow.mll"
                     lf
# 5415 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1774 "lexer_flow.mll"
                      ( Buffer.add_char raw lf;
                        Buffer.add_char literal lf;
                        Buffer.add_char cooked '\n';
                        Lexing.new_line lexbuf;
                        template_part env start cooked raw literal lexbuf )
# 5423 "lexer_flow.ml"

  | 6 ->
let
# 1779 "lexer_flow.mll"
         c
# 5429 "lexer_flow.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1779 "lexer_flow.mll"
                      ( Buffer.add_char raw c;
                        Buffer.add_char literal c;
                        Buffer.add_char cooked c;
                        template_part env start cooked raw literal lexbuf )
# 5436 "lexer_flow.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_template_part_rec env start cooked raw literal lexbuf __ocaml_lex_state

;;

# 1784 "lexer_flow.mll"

  let regexp env =
    get_result_and_clear_state (regexp env env.lex_lb)

  (* Lexing JSX children requires a string buffer to keep track of whitespace
   * *)
  let jsx_child env =
    let start = Loc.from_curr_lb (source env) env.lex_lb in
    let buf = Buffer.create 127 in
    let raw = Buffer.create 127 in
    let env, child = jsx_child env start buf raw env.lex_lb in
    get_result_and_clear_state (env, child)

  let jsx_tag env =
    get_result_and_clear_state (jsx_tag env env.lex_lb)

  let template_tail env =
    get_result_and_clear_state (template_tail env env.lex_lb)

  let type_token env =
    get_result_and_clear_state (type_token env env.lex_lb)

  let token env =
    get_result_and_clear_state (token env env.lex_lb)

# 5469 "lexer_flow.ml"

end
module Parser_env : sig
#1 "parser_env.mli"
(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module provides a layer between the lexer and the parser which includes
 * some parser state and some lexer state *)

module SSet : Set.S with type t = Set.Make(String).t

module Lex_mode : sig
  type t =
    | NORMAL
    | TYPE
    | JSX_TAG
    | JSX_CHILD
    | TEMPLATE
    | REGEXP
    | PREDICATE
  val debug_string_of_lex_mode: t -> string
end

type token_sink_result = {
  token_loc: Loc.t;
  token: Lexer_flow.Token.t;
  token_context: Lex_mode.t;
  token_value: string;
}

type parse_options = {
  esproposal_class_instance_fields: bool;
  esproposal_class_static_fields: bool;
  esproposal_decorators: bool;
  esproposal_export_star_as: bool;
  types: bool;
  use_strict: bool;
}
val default_parse_options : parse_options

type env

(* constructor: *)
val init_env :
  ?token_sink:(token_sink_result -> unit) option
  -> ?parse_options:parse_options option
  -> Loc.filename option
  -> string
  -> env

(* getters: *)
val in_strict_mode : env -> bool
val last_loc : env -> Loc.t option
val in_export : env -> bool
val labels : env -> SSet.t
val comments : env -> Spider_monkey_ast.Comment.t list
val in_loop : env -> bool
val in_switch : env -> bool
val in_function : env -> bool
val allow_yield : env -> bool
val allow_await: env -> bool
val no_in : env -> bool
val no_call : env -> bool
val no_let : env -> bool
val errors : env -> (Loc.t * Parse_error.t) list
val parse_options : env -> parse_options
val source : env -> Loc.filename option
val should_parse_types : env -> bool

(* mutators: *)
val error_at : env -> Loc.t * Parse_error.t -> unit
val error : env -> Parse_error.t -> unit
val error_unexpected : env -> unit
val error_on_decorators : env -> (Loc.t * 'a) list -> unit
val strict_error : env -> Parse_error.t -> unit
val strict_error_at : env -> Loc.t * Parse_error.t -> unit
val get_unexpected_error : Lexer_flow.Token.t * string -> Parse_error.t
val comment_list : env -> Spider_monkey_ast.Comment.t list -> unit
val error_list : env -> (Loc.t * Parse_error.t) list -> unit
val record_export: env -> Loc.t * string -> unit

(* functional operations -- these return shallow copies, so future mutations to
 * the returned env will also affect the original: *)
val with_strict : bool -> env -> env
val with_in_function : bool -> env -> env
val with_allow_yield : bool -> env -> env
val with_allow_await : bool -> env -> env
val with_no_let : bool -> env -> env
val with_in_loop : bool -> env -> env
val with_no_in : bool -> env -> env
val with_in_switch : bool -> env -> env
val with_in_export : bool -> env -> env
val with_no_call : bool -> env -> env
val with_error_callback : (env -> Parse_error.t -> unit) -> env -> env

val without_error_callback : env -> env

val add_label : env -> string -> env
val enter_function : env -> async:bool -> generator:bool -> env

val is_future_reserved : string -> bool
val is_strict_reserved : string -> bool
val is_restricted : string -> bool

module Peek : sig
  val token : ?i:int -> env -> Lexer_flow.Token.t
  val value : ?i:int -> env -> string
  val loc : ?i:int -> env -> Loc.t
  val errors : ?i:int -> env -> (Loc.t * Parse_error.t) list
  val comments : ?i:int -> env -> Spider_monkey_ast.Comment.t list
  val is_line_terminator : env -> bool
  val is_implicit_semicolon : env -> bool
  val semicolon_loc : ?i:int -> env -> Loc.t option
  val is_identifier : ?i:int -> env -> bool
  val is_function : ?i:int -> env -> bool
  val is_class : ?i:int -> env -> bool
end

module Eat : sig
  val token : env -> unit
  val push_lex_mode : env -> Lex_mode.t -> unit
  val pop_lex_mode : env -> unit
  val double_pop_lex_mode : env -> unit
  val semicolon : env -> unit
end

module Expect : sig
  val token : env -> Lexer_flow.Token.t -> unit
  val maybe : env -> Lexer_flow.Token.t -> bool
  val contextual : env -> string -> unit
end

module Try : sig
  type 'a parse_result =
    | ParsedSuccessfully of 'a
    | FailedToParse

  exception Rollback

  val to_parse: env -> (env -> 'a) -> 'a parse_result
end

end = struct
#1 "parser_env.ml"
(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Spider_monkey_ast
module Lex_env = Lexer_flow.Lex_env
module Lex_result = Lexer_flow.Lex_result
open Ast
module Error = Parse_error
module SSet = Set.Make(String)
module SMap = Map.Make(String)

module Lex_mode = struct
  type t =
    | NORMAL
    | TYPE
    | JSX_TAG
    | JSX_CHILD
    | TEMPLATE
    | REGEXP
    | PREDICATE

  let debug_string_of_lex_mode (mode: t) =
    match mode with
    | NORMAL -> "NORMAL"
    | TYPE -> "TYPE"
    | JSX_TAG -> "JSX_TAG"
    | JSX_CHILD -> "JSX_CHILD"
    | TEMPLATE -> "TEMPLATE"
    | REGEXP -> "REGEXP"
    | PREDICATE -> "PREDICATE"
end

(* READ THIS BEFORE YOU MODIFY:
 *
 * The current implementation for lookahead beyond a single token is
 * inefficient. If you believe you need to increase this constant, do one of the
 * following:
 * - Find another way
 * - Benchmark your change and provide convincing evidence that it doesn't
 *   actually have a significant perf impact.
 * - Refactor this to memoize all requested lookahead, so we aren't lexing the
 *   same token multiple times.
 *)
let maximum_lookahead = 2

module Lookahead : sig
  type t
  val create : Lex_env.t -> Lex_mode.t -> t
  val peek : t -> int -> Lex_result.t
  val lex_env : t -> int -> Lex_env.t
  val junk : t -> unit
end = struct
  type t = {
    mutable la_results    : (Lex_env.t * Lex_result.t) option array;
    mutable la_num_lexed  : int;
    la_lex_mode           : Lex_mode.t;
    mutable la_lex_env    : Lex_env.t;
  }

  let create lex_env mode =
    let lexbuf = Lex_env.lexbuf lex_env in
    (* copy all the mutable things so that we have a distinct lexing environment
     * that does not interfere with ordinary lexer operations *)
    (* lex_buffer has type bytes, which is itself mutable, but the lexer
     * promises not to change it so a shallow copy should be fine *)
    (* I don't know how to do a copy without an update *)
    let lexbuf = Lexing.({ lexbuf with lex_buffer = lexbuf.lex_buffer }) in
    let lex_env = Lex_env.with_lexbuf ~lexbuf lex_env in
    {
      la_results = [||];
      la_num_lexed = 0;
      la_lex_mode = mode;
      la_lex_env = lex_env;
    }

  let next_power_of_two n =
    let rec f i =
      if i >= n then
        i
      else
        f (i * 2) in
    f 1

  (* resize the tokens array to have at least n elements *)
  let grow t n =
    if Array.length t.la_results < n then begin
      let new_size = next_power_of_two n in
      let filler i =
        if i < Array.length t.la_results then
          t.la_results.(i)
        else
          None in
      let new_arr = Array.init new_size filler in
      t.la_results <- new_arr
    end

  (* precondition: there is enough room in t.la_results for the result *)
  let lex t =
    let lex_env = t.la_lex_env in
    let lex_env, lex_result =
      match t.la_lex_mode with
      | Lex_mode.NORMAL -> Lexer_flow.token lex_env
      | Lex_mode.TYPE -> Lexer_flow.type_token lex_env
      | Lex_mode.PREDICATE -> Lexer_flow.token lex_env
      | Lex_mode.JSX_TAG -> Lexer_flow.jsx_tag lex_env
      | Lex_mode.JSX_CHILD -> Lexer_flow.jsx_child lex_env
      | Lex_mode.TEMPLATE -> Lexer_flow.template_tail lex_env
      | Lex_mode.REGEXP -> Lexer_flow.regexp lex_env
    in
    let cloned_env =
      let lexbuf =
        let lexbuf = Lex_env.lexbuf lex_env in
        Lexing.({ lexbuf with lex_buffer = lexbuf.lex_buffer })
      in
      Lex_env.with_lexbuf ~lexbuf lex_env
    in
    t.la_lex_env <- lex_env;
    t.la_results.(t.la_num_lexed) <- Some (cloned_env, lex_result);
    t.la_num_lexed <- t.la_num_lexed + 1

  let lex_until t i =
    grow t (i + 1);
    while t.la_num_lexed <= i do
      lex t
    done

  let peek t i =
    lex_until t i;
    match t.la_results.(i) with
      | Some (_, result) -> result
      (* only happens if there is a defect in the lookahead module *)
      | None -> failwith "Lookahead.peek failed"

  let lex_env t i =
    lex_until t i;
    match t.la_results.(i) with
      | Some (lex_env, _) -> lex_env
      (* only happens if there is a defect in the lookahead module *)
      | None -> failwith "Lookahead.peek failed"

  (* Throws away the first peeked-at token, shifting any subsequent tokens up *)
  let junk t =
    lex_until t 0;
    if t.la_num_lexed > 1 then
      Array.blit t.la_results 1 t.la_results 0 (t.la_num_lexed - 1);
    t.la_results.(t.la_num_lexed - 1) <- None;
    t.la_num_lexed <- t.la_num_lexed - 1;
end

type token_sink_result = {
  token_loc: Loc.t;
  token: Lexer_flow.Token.t;
  token_context: Lex_mode.t;
  token_value: string;
}

type parse_options = {
  esproposal_class_instance_fields: bool;
  esproposal_class_static_fields: bool;
  esproposal_decorators: bool;
  esproposal_export_star_as: bool;
  types: bool;
  use_strict: bool;
}
let default_parse_options = {
  esproposal_class_instance_fields = false;
  esproposal_class_static_fields = false;
  esproposal_decorators = false;
  esproposal_export_star_as = false;
  types = true;
  use_strict = false;
}

type env = {
  errors            : (Loc.t * Error.t) list ref;
  comments          : Comment.t list ref;
  labels            : SSet.t;
  exports           : SSet.t ref;
  last_loc          : Loc.t option ref;
  in_strict_mode    : bool;
  in_export         : bool;
  in_loop           : bool;
  in_switch         : bool;
  in_function       : bool;
  no_in             : bool;
  no_call           : bool;
  no_let            : bool;
  allow_yield       : bool;
  allow_await       : bool;
  error_callback    : (env -> Error.t -> unit) option;
  lex_mode_stack    : Lex_mode.t list ref;
  (* lex_env is the lex_env after the single lookahead has been lexed *)
  lex_env           : Lex_env.t ref;
  (* This needs to be cleared whenever we advance. *)
  lookahead         : Lookahead.t ref;
  token_sink        : (token_sink_result -> unit) option ref;
  parse_options     : parse_options;
  source            : Loc.filename option;
}

(* constructor *)
let init_env ?(token_sink=None) ?(parse_options=None) source content =
  let lb = Lexing.from_string content in
  (match source with
    | None
    | Some Loc.Builtins -> ()
    | Some Loc.LibFile fn
    | Some Loc.SourceFile fn
    | Some Loc.JsonFile fn ->
      lb.Lexing.lex_curr_p <- {
        lb.Lexing.lex_curr_p with Lexing.pos_fname = fn
      });

  let parse_options =
    match parse_options with
    | Some opts -> opts
    | None -> default_parse_options
  in
  let enable_types_in_comments = parse_options.types in
  let lex_env = Lex_env.new_lex_env source lb ~enable_types_in_comments in
  {
    errors            = ref [];
    comments          = ref [];
    labels            = SSet.empty;
    exports           = ref SSet.empty;
    last_loc          = ref None;
    in_strict_mode    = parse_options.use_strict;
    in_export         = false;
    in_loop           = false;
    in_switch         = false;
    in_function       = false;
    no_in             = false;
    no_call           = false;
    no_let            = false;
    allow_yield       = true;
    allow_await       = false;
    error_callback    = None;
    lex_mode_stack    = ref [Lex_mode.NORMAL];
    lex_env           = ref lex_env;
    lookahead         = ref (Lookahead.create lex_env Lex_mode.NORMAL);
    token_sink        = ref token_sink;
    parse_options;
    source;
  }

(* getters: *)
let in_strict_mode env = env.in_strict_mode
let lex_mode env = List.hd !(env.lex_mode_stack)
let in_export env = env.in_export
let comments env = !(env.comments)
let labels env = env.labels
let in_loop env = env.in_loop
let in_switch env = env.in_switch
let in_function env = env.in_function
let allow_yield env = env.allow_yield
let allow_await env = env.allow_await
let no_in env = env.no_in
let no_call env = env.no_call
let no_let env = env.no_let
let errors env = !(env.errors)
let parse_options env = env.parse_options
let source env = env.source
let should_parse_types env = env.parse_options.types

(* mutators: *)
let error_at env (loc, e) =
  env.errors := (loc, e) :: !(env.errors);
  match env.error_callback with
  | None -> ()
  | Some callback -> callback env e
let comment_list env =
  List.iter (fun c -> env.comments := c :: !(env.comments))
let record_export env (loc, export_name) =
  let exports = !(env.exports) in
  if SSet.mem export_name exports
  then error_at env (loc, Error.DuplicateExport export_name)
  else env.exports := SSet.add export_name !(env.exports)

(* lookahead: *)
let lookahead ?(i=0) env =
  assert (i < maximum_lookahead);
  Lookahead.peek !(env.lookahead) i

(* functional operations: *)
let with_strict in_strict_mode env = { env with in_strict_mode }
let with_in_function in_function env = { env with in_function }
let with_allow_yield allow_yield env = { env with allow_yield }
let with_allow_await allow_await env = { env with allow_await }
let with_no_let no_let env = { env with no_let }
let with_in_loop in_loop env = { env with in_loop }
let with_no_in no_in env = { env with no_in }
let with_in_switch in_switch env = { env with in_switch }
let with_in_export in_export env = { env with in_export }
let with_no_call no_call env = { env with no_call }
let with_error_callback error_callback env =
  { env with error_callback = Some error_callback }

(* other helper functions: *)
let error_list env = List.iter (error_at env)
let last_loc env = !(env.last_loc)

let without_error_callback env = { env with error_callback = None }

let add_label env label = { env with labels = SSet.add label env.labels }
let enter_function env ~async ~generator = { env with
    in_function = true;
    in_loop = false;
    in_switch = false;
    labels = SSet.empty;
    allow_await = async;
    allow_yield = generator;
  }

let is_future_reserved = function
  | "enum" -> true
  | _ -> false

let is_strict_reserved = function
  | "interface"
  | "implements"
  | "package"
  | "private"
  | "protected"
  | "public"
  | "static"
  | "yield" -> true
  | _ -> false

let is_restricted = function
  | "eval"
  | "arguments" -> true
  | _ -> false

(* Answer questions about what comes next *)
module Peek = struct
  open Loc
  open Lexer_flow.Token

  let token ?(i=0) env = Lex_result.token (lookahead ~i env)
  let value ?(i=0) env = Lex_result.value (lookahead ~i env)
  let loc ?(i=0) env = Lex_result.loc (lookahead ~i env)
  let errors ?(i=0) env = Lex_result.errors (lookahead ~i env)
  let comments ?(i=0) env = Lex_result.comments (lookahead ~i env)
  let lex_env ?(i=0) env = Lookahead.lex_env !(env.lookahead) i

  (* True if there is a line terminator before the next token *)
  let is_line_terminator env =
    match last_loc env with
      | None -> false
      | Some loc' ->
          (loc env).start.line > loc'.start.line

  let is_implicit_semicolon env =
    match token env with
    | T_EOF | T_RCURLY -> true
    | T_SEMICOLON -> false
    | _ -> is_line_terminator env

  let semicolon_loc ?(i=0) env =
    if token ~i env = T_SEMICOLON
    then Some (loc ~i env)
    else None

  (* This returns true if the next token is identifier-ish (even if it is an
   * error) *)
  let is_identifier ?(i=0) env =
    let name = value ~i env in
    match token ~i env with
    | _ when
      is_strict_reserved name ||
      is_restricted name ||
      is_future_reserved name-> true
    | T_LET
    | T_TYPE
    | T_OF
    | T_DECLARE
    | T_ASYNC
    | T_AWAIT
    | T_IDENTIFIER -> true
    | _ -> false

  let is_function ?(i=0) env =
    token ~i env = T_FUNCTION ||
    (token ~i env = T_ASYNC && token ~i:(i+1) env = T_FUNCTION)

  let is_class ?(i=0) env =
    match token ~i env with
    | T_CLASS
    | T_AT -> true
    | _ -> false
end


(*****************************************************************************)
(* Errors *)
(*****************************************************************************)

(* Complains about an error at the location of the lookahead *)
let error env e =
  let loc = Peek.loc env in
  error_at env (loc, e)

let get_unexpected_error = Lexer_flow.Token.(function
  | T_EOF, _ -> Error.UnexpectedEOS
  | T_NUMBER _, _ -> Error.UnexpectedNumber
  | T_JSX_TEXT _, _
  | T_STRING _, _ -> Error.UnexpectedString
  | T_IDENTIFIER, _ -> Error.UnexpectedIdentifier
  | _, word when is_future_reserved word -> Error.UnexpectedReserved
  | _, word when is_strict_reserved word -> Error.StrictReservedWord
  | _, value -> Error.UnexpectedToken value
)

let error_unexpected env =
  (* So normally we consume the lookahead lex result when Eat.token calls
   * Parser_env.advance, which will add any lexing errors to our list of errors.
   * However, raising an unexpected error for a lookahead is kind of like
   * consuming that token, so we should process any lexing errors before
   * complaining about the unexpected token *)
  error_list env (Peek.errors env);
  error env (get_unexpected_error (Peek.token env, Peek.value env))

let error_on_decorators env = List.iter
  (fun decorator -> error_at env ((fst decorator), Error.UnsupportedDecorator))

let strict_error env e = if in_strict_mode env then error env e
let strict_error_at env (loc, e) =
  if in_strict_mode env then error_at env (loc, e)


(* Consume zero or more tokens *)
module Eat = struct
  (* Consume a single token *)
  let token env =
    (* If there's a token_sink, emit the lexed token before moving forward *)
    (match !(env.token_sink) with
      | None -> ()
      | Some token_sink ->
          let token_loc = Peek.loc env in
          let token = Peek.token env in
          let token_value = Peek.value env in
          token_sink {
            token_loc;
            token;
            (**
             * The lex mode is useful because it gives context to some
             * context-sensitive tokens.
             *
             * Some examples of such tokens include:
             *
             * `=>` - Part of an arrow function? or part of a type annotation?
             * `<`  - A less-than? Or an opening to a JSX element?
             * ...etc...
             *)
            token_context=(lex_mode env);
            token_value;
          }
    );

    env.lex_env := Peek.lex_env env;

    error_list env (Peek.errors env);
    comment_list env (Peek.comments env);
    env.last_loc := Some (Peek.loc env);

    Lookahead.junk !(env.lookahead)

  let push_lex_mode env mode =
    env.lex_mode_stack := mode :: !(env.lex_mode_stack);
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env)

  let pop_lex_mode env =
    let new_stack = match !(env.lex_mode_stack) with
    | _mode::stack -> stack
    | _ -> failwith "Popping lex mode from empty stack" in
    env.lex_mode_stack := new_stack;
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env)

  let double_pop_lex_mode env =
    let new_stack = match !(env.lex_mode_stack) with
    | _::_::stack -> stack
    | _ -> failwith "Popping lex mode from empty stack" in
    env.lex_mode_stack := new_stack;
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env)

  (* Semicolon insertion is handled here :(. There seem to be 2 cases where
  * semicolons are inserted. First, if we reach the EOF. Second, if the next
  * token is } or is separated by a LineTerminator.
  *)
  let semicolon env =
    if not (Peek.is_implicit_semicolon env)
    then
      if Peek.token env = Lexer_flow.Token.T_SEMICOLON
      then token env
      else error_unexpected env
end

module Expect = struct
  let token env t =
    if Peek.token env <> t then error_unexpected env;
    Eat.token env

  (* If the next token is t, then eat it and return true
   * else return false *)
  let maybe env t =
    if Peek.token env = t
    then begin
      Eat.token env;
      true
    end else false

  let contextual env str =
    if Peek.value env <> str
    then error_unexpected env;
    Eat.token env
end

(* This module allows you to try parsing and rollback if you need. This is not
 * cheap and its usage is strongly discouraged *)
module Try = struct
  type 'a parse_result =
    | ParsedSuccessfully of 'a
    | FailedToParse

  exception Rollback

  type saved_state = {
    saved_errors         : (Loc.t * Error.t) list;
    saved_comments       : Ast.Comment.t list;
    saved_last_loc       : Loc.t option;
    saved_lex_mode_stack : Lex_mode.t list;
    saved_lex_env        : Lex_env.t;
    token_buffer         : ((token_sink_result -> unit) * token_sink_result Queue.t) option;
  }

  let save_state env =
    let token_buffer =
      match !(env.token_sink) with
      | None -> None
      | Some orig_token_sink ->
          let buffer = Queue.create () in
          env.token_sink := Some(fun token_data ->
            Queue.add token_data buffer
          );
          Some(orig_token_sink, buffer)
    in
    {
      saved_errors         = !(env.errors);
      saved_comments       = !(env.comments);
      saved_last_loc       = !(env.last_loc);
      saved_lex_mode_stack = !(env.lex_mode_stack);
      saved_lex_env        = !(env.lex_env);
      token_buffer;
    }

  let reset_token_sink ~flush env token_buffer_info =
    match token_buffer_info with
    | None -> ()
    | Some(orig_token_sink, token_buffer) ->
        env.token_sink := Some orig_token_sink;
        if flush then Queue.iter orig_token_sink token_buffer

  let rollback_state env saved_state =
    reset_token_sink ~flush:false env saved_state.token_buffer;
    env.errors := saved_state.saved_errors;
    env.comments := saved_state.saved_comments;
    env.last_loc := saved_state.saved_last_loc;
    env.lex_mode_stack := saved_state.saved_lex_mode_stack;
    env.lex_env := saved_state.saved_lex_env;
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env);

    FailedToParse

  let success env saved_state result =
    reset_token_sink ~flush:true env saved_state.token_buffer;
    ParsedSuccessfully result

  let to_parse env parse =
    let saved_state = save_state env in
    try success env saved_state (parse env)
    with Rollback -> rollback_state env saved_state
end

end
module Parser_flow
= struct
#1 "parser_flow.ml"
(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Token = Lexer_flow.Token
open Token
open Parser_env
module Ast = Spider_monkey_ast
open Ast
module Error = Parse_error
module SSet = Set.Make(String)
module SMap = Map.Make(String)

let name_of_id id =
  let (_, {Identifier.name; _;}) = id in
  name

(* Sometimes we add the same error for multiple different reasons. This is hard
   to avoid, so instead we just filter the duplicates out. This function takes
   a reversed list of errors and returns the list in forward order with dupes
   removed. This differs from a set because the original order is preserved. *)
let filter_duplicate_errors =
  let module ErrorSet = Set.Make(struct
    type t = Loc.t * Error.t
    let compare (a_loc, a_error) (b_loc, b_error) =
      let loc = Loc.compare a_loc b_loc in
      if loc = 0
      then Pervasives.compare a_error b_error
      else loc
  end) in
  fun errs ->
    let errs = List.rev errs in
    let _, deduped = List.fold_left (fun (set, deduped) err ->
      if ErrorSet.mem err set then (set, deduped)
      else (ErrorSet.add err set, err::deduped)
    ) (ErrorSet.empty, []) errs in
    List.rev deduped

let with_loc fn env =
  let start_loc = Peek.loc env in
  let result = fn env in
  let end_loc = match last_loc env with
  | Some loc -> loc
  | None ->
      error env (Error.Assertion "did not consume any tokens");
      Peek.loc env
  in
  Loc.btwn start_loc end_loc, result

let string_starts_with long short =
  try
    let long = String.sub long 0 (String.length short) in
    long = short
  with Invalid_argument _ ->
    false

(* NOTE: temporary support for predicated function definitions requires
   that the name starts with `$pred`. In later iterations we would like to
   add support for a more compact solutions (e.g. `function?(...)`)
*)
let is_predicate = function
  | Some id -> string_starts_with (name_of_id id) "$pred$"
  | _ -> false

module rec Parse : sig
  val program : env -> Ast.program
  val statement : env -> Ast.Statement.t
  val statement_list_item : ?decorators:Ast.Expression.t list -> env -> Ast.Statement.t
  val statement_list : term_fn:(Token.t -> bool) -> env -> Ast.Statement.t list
  val statement_list_with_directives : term_fn:(Token.t -> bool) -> env -> Ast.Statement.t list * bool
  val module_body : term_fn:(Token.t -> bool) -> env -> Ast.Statement.t list
  val expression : env -> Ast.Expression.t
  val assignment : env -> Ast.Expression.t
  val object_initializer : env -> Loc.t * Ast.Expression.Object.t
  val array_initializer : env -> Loc.t * Ast.Expression.Array.t
  val identifier : ?restricted_error:Error.t -> env -> Ast.Identifier.t
  val identifier_or_reserved_keyword : env -> (Ast.Identifier.t * (Loc.t * Error.t) option)
  val identifier_with_type : env -> Error.t -> Ast.Identifier.t
  val block_body : env -> Loc.t * Ast.Statement.Block.t
  val function_block_body : env -> Loc.t * Ast.Statement.Block.t * bool
  val jsx_element : env -> Loc.t * Ast.JSX.element
  val pattern : env -> Error.t -> Ast.Pattern.t
  val pattern_from_expr : env -> Ast.Expression.t -> Ast.Pattern.t
  val object_key : env -> Loc.t * Ast.Expression.Object.Property.key
  val class_declaration : env -> Ast.Expression.t list -> Ast.Statement.t
  val class_expression : env -> Ast.Expression.t
  val is_assignable_lhs : Ast.Expression.t -> bool
  val predicate : env -> Ast.Predicate.t option
end = struct
  module Type : sig
    val _type : env -> Ast.Type.t
    val type_parameter_declaration : env -> Ast.Type.ParameterDeclaration.t option
    val type_parameter_declaration_with_defaults : env -> Ast.Type.ParameterDeclaration.t option
    val type_parameter_instantiation : env -> Ast.Type.ParameterInstantiation.t option
    val generic : env -> Loc.t * Ast.Type.Generic.t
    val _object : ?allow_static:bool -> env -> Loc.t * Type.Object.t
    val function_param_list : env -> Type.Function.Param.t option * Type.Function.Param.t list
    val annotation : env -> Ast.Type.annotation
    val annotation_opt : env -> Ast.Type.annotation option
  end = struct
    type param_list_or_type =
      | ParamList of (Type.Function.Param.t option * Type.Function.Param.t list)
      | Type of Type.t

    let rec _type env = union env

    and annotation env =
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeAnnotation;
      let start_loc = Peek.loc env in
      Expect.token env T_COLON;
      let typeAnnotation = _type env in
      let end_loc = match last_loc env with
      | Some loc -> loc
      | None -> assert false in
      Loc.btwn start_loc end_loc, typeAnnotation

    and rev_nonempty_acc acc =
      let end_loc = match acc with
      | (loc, _)::_ -> loc
      | _ -> assert false in
      let acc = List.rev acc in
      let start_loc = match acc with
      | (loc, _)::_ -> loc
      | _ -> assert false in
      Loc.btwn start_loc end_loc, acc

    and union env =
      let _ = Expect.maybe env T_BIT_OR in
      let left = intersection env in
      union_with env left

    and union_with =
      let rec unions env acc =
        match Peek.token env with
        | T_BIT_OR ->
            Expect.token env T_BIT_OR;
            unions env (intersection env::acc)
        | _ ->
            let loc, acc = rev_nonempty_acc acc in
            loc, Type.Union acc
      in fun env left ->
        if Peek.token env = T_BIT_OR
        then unions env [left]
        else left

    and intersection env =
      let _ = Expect.maybe env T_BIT_AND in
      let left = prefix env in
      intersection_with env left

    and intersection_with =
      let rec intersections env acc =
        match Peek.token env with
        | T_BIT_AND ->
            Expect.token env T_BIT_AND;
            intersections env (prefix env::acc)
        | _ ->
            let loc, acc = rev_nonempty_acc acc in
            loc, Type.Intersection acc
      in fun env left ->
        if Peek.token env = T_BIT_AND
        then intersections env [left]
        else left

    and prefix env =
      match Peek.token env with
      | T_PLING ->
          let loc = Peek.loc env in
          Expect.token env T_PLING;
          let t = prefix env in
          Loc.btwn loc (fst t), Type.Nullable t
      | _ ->
          postfix env

    and postfix env =
      let t = primary env in
      postfix_with env t

    and postfix_with env t =
      if not (Peek.is_line_terminator env) && Expect.maybe env T_LBRACKET
      then begin
        let end_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        let loc = Loc.btwn (fst t) end_loc in
        let t = loc, Type.Array t in
        postfix_with env t
      end else t

    and primary env =
      let loc = Peek.loc env in
      match Peek.token env with
      | T_MULT ->
          Expect.token env T_MULT;
          loc, Type.Exists
      | T_LESS_THAN -> _function env
      | T_LPAREN -> function_or_group env
      | T_LCURLY ->
          let loc, o = _object env in
          loc, Type.Object o
      | T_TYPEOF ->
          let start_loc = Peek.loc env in
          Expect.token env T_TYPEOF;
          let t = primary env in
          Loc.btwn start_loc (fst t), Type.Typeof t
      | T_LBRACKET -> tuple env
      | T_IDENTIFIER ->
          let loc, g = generic env in
          loc, Type.Generic g
      | T_STRING (loc, value, raw, octal)  ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (loc, value, raw, octal));
          loc, Type.StringLiteral {
            Type.StringLiteral.value;
            raw;
          }
      | T_NUMBER_SINGLETON_TYPE (number_type, value) ->
          let raw = Peek.value env in
          Expect.token env (T_NUMBER_SINGLETON_TYPE (number_type, value));
          if number_type = LEGACY_OCTAL
          then strict_error env Error.StrictOctalLiteral;
          loc, Type.NumberLiteral {
            Type.NumberLiteral.value;
            raw;
          }
      | (T_TRUE | T_FALSE) as token ->
          let raw = Peek.value env in
          Expect.token env token;
          let value = token = T_TRUE in
          loc, Type.BooleanLiteral {
            Type.BooleanLiteral.value;
            raw;
          }
      | token ->
          match primitive token with
          | Some t ->
              Expect.token env token;
              loc, t
          | None ->
              error_unexpected env;
              loc, Type.Any

    and primitive = function
      | T_ANY_TYPE     -> Some Type.Any
      | T_BOOLEAN_TYPE -> Some Type.Boolean
      | T_NUMBER_TYPE  -> Some Type.Number
      | T_STRING_TYPE  -> Some Type.String
      | T_VOID_TYPE    -> Some Type.Void
      | T_NULL         -> Some Type.Null
      | _ -> None

    and tuple =
      let rec types env acc =
        match Peek.token env with
        | T_EOF
        | T_RBRACKET -> List.rev acc
        | _ ->
            let acc = (_type env)::acc in
            (* Trailing comma support (like [number, string,]) *)
            if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
            types env acc

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LBRACKET;
        let tl = types env [] in
        let end_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        Loc.btwn start_loc end_loc, Type.Tuple tl

    and function_param_with_id env name =
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeAnnotation;
      let optional = Expect.maybe env T_PLING in
      Expect.token env T_COLON;
      let typeAnnotation = _type env in
      Loc.btwn (fst name) (fst typeAnnotation), Type.Function.Param.({
        name;
        typeAnnotation;
        optional;
      })

    and function_param_list_without_parens =
      let param env =
        let name, _ = Parse.identifier_or_reserved_keyword env in
        function_param_with_id env name

      in let rec param_list env acc =
        match Peek.token env with
        | T_EOF
        | T_ELLIPSIS
        | T_RPAREN as t ->
            let rest = if t = T_ELLIPSIS
            then begin
              Expect.token env T_ELLIPSIS;
              Some (param env)
            end else None in
            rest, List.rev acc
        | _ ->
          let acc = (param env)::acc in
          if Peek.token env <> T_RPAREN
          then Expect.token env T_COMMA;
          param_list env acc

      in fun env -> param_list env

    and function_param_list env =
        Expect.token env T_LPAREN;
        let ret = function_param_list_without_parens env [] in
        Expect.token env T_RPAREN;
        ret

    and param_list_or_type env =
      Expect.token env T_LPAREN;
      let ret = match Peek.token env with
      | T_EOF
      | T_ELLIPSIS ->
          (* (... is definitely the beginning of a param list *)
          ParamList (function_param_list_without_parens env [])
      | T_RPAREN ->
          (* () or is definitely a param list *)
          ParamList (None, [])
      | T_IDENTIFIER ->
          (* This could be a function parameter or a generic type *)
          function_param_or_generic_type env
      | token ->
          (match primitive token with
          | None ->
              (* All params start with an identifier or `...` *)
              Type (_type env)
          | Some _ ->
              (* Don't know if this is (number) or (number: number). The first
               * is a type, the second is a param. *)
              match Peek.token ~i:1 env with
              | T_PLING | T_COLON ->
                (* Ok this is definitely a parameter *)
                let name, _ = Parse.identifier_or_reserved_keyword env in
                if not (should_parse_types env)
                then error env Error.UnexpectedTypeAnnotation;
                let optional = Expect.maybe env T_PLING in
                Expect.token env T_COLON;
                let typeAnnotation = _type env in
                if Peek.token env <> T_RPAREN
                then Expect.token env T_COMMA;
                let param = Loc.btwn (fst name) (fst typeAnnotation), Type.Function.Param.({
                  name;
                  typeAnnotation;
                  optional;
                }) in
                ParamList (function_param_list_without_parens env [param])
              | _ ->
                Type (_type env)
          )
      in
      Expect.token env T_RPAREN;
      ret

    and function_param_or_generic_type env =
      let id = Parse.identifier env in
      match Peek.token env with
      | T_PLING (* optional param *)
      | T_COLON ->
          let param = function_param_with_id env id in
          ignore (Expect.maybe env T_COMMA);
          ParamList (function_param_list_without_parens env [param])
      | _ ->
          Type (union_with env
                  (intersection_with env
                    (postfix_with env (generic_type_with_identifier env id)))
                )

    and function_or_group env =
      let start_loc = Peek.loc env in
      match param_list_or_type env with
      | ParamList (rest, params) ->
        Expect.token env T_ARROW;
        let returnType = _type env in
        let end_loc = fst returnType in
        Loc.btwn start_loc end_loc, Type.(Function Function.({
          params;
          returnType;
          rest;
          typeParameters = None;
        }))
      | Type _type -> _type

    and _function env =
      let start_loc = Peek.loc env in
      let typeParameters = type_parameter_declaration ~allow_default:false env in
      let rest, params = function_param_list env in
      Expect.token env T_ARROW;
      let returnType = _type env in
      let end_loc = fst returnType in
      Loc.btwn start_loc end_loc, Type.(Function Function.({
        params;
        returnType;
        rest;
        typeParameters;
      }))

    and _object =
      let methodish env start_loc =
        let typeParameters = type_parameter_declaration ~allow_default:false env in
        let rest, params = function_param_list env in
        Expect.token env T_COLON;
        let returnType = _type env in
        let loc = Loc.btwn start_loc (fst returnType) in
        loc, Type.Function.({
          params;
          returnType;
          rest;
          typeParameters;
        })

      in let method_property env start_loc static key =
        let value = methodish env start_loc in
        let value = fst value, Type.Function (snd value) in
        fst value, Type.Object.Property.({
          key;
          value;
          optional = false;
          static;
          _method = true;
        })

      in let call_property env start_loc static =
        let value = methodish env (Peek.loc env) in
        Loc.btwn start_loc (fst value), Type.Object.CallProperty.({
          value;
          static;
        })

      in let property env start_loc static key =
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeAnnotation;
        let optional = Expect.maybe env T_PLING in
        Expect.token env T_COLON;
        let value = _type env in
        Loc.btwn start_loc (fst value), Type.Object.Property.({
          key;
          value;
          optional;
          static;
          _method = false;
        })

      in let indexer_property env start_loc static =
        Expect.token env T_LBRACKET;
        let id, _ = Parse.identifier_or_reserved_keyword env in
        Expect.token env T_COLON;
        let key = _type env in
        Expect.token env T_RBRACKET;
        Expect.token env T_COLON;
        let value = _type env in
        Loc.btwn start_loc (fst value), Type.Object.Indexer.({
          id;
          key;
          value;
          static;
        })

      in let semicolon env =
        match Peek.token env with
        | T_COMMA | T_SEMICOLON -> Eat.token env
        | T_RCURLY -> ()
        | _ -> error_unexpected env

      in let rec properties ~allow_static env (acc, indexers, callProperties) =
        let start_loc = Peek.loc env in
        let static = allow_static && Expect.maybe env T_STATIC in
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc, List.rev indexers, List.rev callProperties
        | T_LBRACKET ->
          let indexer = indexer_property env start_loc static in
          semicolon env;
          properties allow_static env (acc, indexer::indexers, callProperties)
        | T_LESS_THAN
        | T_LPAREN ->
          let call_prop = call_property env start_loc static in
          semicolon env;
          properties allow_static env (acc, indexers, call_prop::callProperties)
        | _ ->
          let static, (_, key) = match static, Peek.token env with
          | true, T_COLON ->
              strict_error_at env (start_loc, Error.StrictReservedWord);
              let static_key =
                start_loc, Expression.Object.Property.Identifier ( start_loc, {
                  Identifier.name = "static";
                  optional = false;
                  typeAnnotation = None;
                }) in
              false, static_key
          | _ ->
              Eat.push_lex_mode env Lex_mode.NORMAL;
              let key = Parse.object_key env in
              Eat.pop_lex_mode env;
              static, key
          in
          let property = match Peek.token env with
          | T_LESS_THAN
          | T_LPAREN -> method_property env start_loc static key
          | _ -> property env start_loc static key in
          semicolon env;
          properties allow_static env (property::acc, indexers, callProperties)

      in fun ?(allow_static=false) env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let properties, indexers, callProperties =
          properties ~allow_static env ([], [], []) in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        Loc.btwn start_loc end_loc, Type.Object.({
          properties;
          indexers;
          callProperties
        })

    and type_parameter_declaration =
      let rec params env ~allow_default ~require_default acc = Type.ParameterDeclaration.TypeParam.(
        let variance = match Peek.token env with
          | T_PLUS -> Eat.token env; Some Variance.Plus
          | T_MINUS -> Eat.token env; Some Variance.Minus
          | _ -> None in
        let loc, id = Parse.identifier_with_type env Error.StrictParamName in
        let default, require_default = match allow_default, Peek.token env with
        | false, _ -> None, false
        | true, T_ASSIGN ->
            Eat.token env;
            Some (_type env), true
        | true, _ ->
            if require_default
            then error_at env (loc, Error.MissingTypeParamDefault);
            None, require_default in
        let param = loc, {
          name = id.Identifier.name;
          bound = id.Identifier.typeAnnotation;
          variance;
          default;
        } in
        let acc = param::acc in
        match Peek.token env with
        | T_EOF
        | T_GREATER_THAN -> List.rev acc
        | _ ->
          Expect.token env T_COMMA;
          if Peek.token env = T_GREATER_THAN
          then List.rev acc
          else params env ~allow_default ~require_default acc
      )
      in fun ~allow_default env ->
          let start_loc = Peek.loc env in
          if Peek.token env = T_LESS_THAN
          then begin
            if not (should_parse_types env)
            then error env Error.UnexpectedTypeAnnotation;
            Expect.token env T_LESS_THAN;
            let params = params env ~allow_default ~require_default:false [] in
            let loc = Loc.btwn start_loc (Peek.loc env) in
            Expect.token env T_GREATER_THAN;
            Some (loc, Type.ParameterDeclaration.({
              params;
            }))
          end else None

    and type_parameter_instantiation =
      let rec params env acc =
        match Peek.token env with
        | T_EOF
        | T_GREATER_THAN -> List.rev acc
        | _ ->
          let acc = (_type env)::acc in
          if Peek.token env <> T_GREATER_THAN
          then Expect.token env T_COMMA;
          params env acc

      in fun env ->
          let start_loc = Peek.loc env in
          if Peek.token env = T_LESS_THAN
          then begin
            Expect.token env T_LESS_THAN;
            let params = params env [] in
            let loc = Loc.btwn start_loc (Peek.loc env) in
            Expect.token env T_GREATER_THAN;
            Some (loc, Type.ParameterInstantiation.({
              params;
            }))
          end else None

    and generic env = raw_generic_with_identifier env (Parse.identifier env)

    and raw_generic_with_identifier =
      let rec identifier env (q_loc, qualification) =
        if Peek.token env = T_PERIOD
        then begin
          Expect.token env T_PERIOD;
          let id = Parse.identifier env in
          let loc = Loc.btwn q_loc (fst id) in
          let qualification = Type.Generic.Identifier.(Qualified (loc, {
            qualification;
            id;
          })) in
          identifier env (loc, qualification)
        end else (q_loc, qualification)

      in fun env id ->
        let id = fst id, Type.Generic.Identifier.Unqualified id in
        let id_loc, id = identifier env id in
        let typeParameters = type_parameter_instantiation env in
        let loc = match typeParameters with
        | None -> id_loc
        | Some (loc, _) -> Loc.btwn id_loc loc in
        loc, Type.Generic.({
          id;
          typeParameters;
        })

    and generic_type_with_identifier env id =
      let loc, generic = raw_generic_with_identifier env id in
      loc, Type.Generic generic

    and annotation_opt env =
      match Peek.token env with
      | T_COLON -> Some (annotation env)
      | _ -> None

    let wrap f env =
      let env = env |> with_strict true in
      Eat.push_lex_mode env Lex_mode.TYPE;
      let ret = f env in
      Eat.pop_lex_mode env;
      ret

    let _type = wrap _type
    let type_parameter_declaration_with_defaults =
      wrap (type_parameter_declaration ~allow_default:true)
    let type_parameter_declaration =
      wrap (type_parameter_declaration ~allow_default:false)
    let type_parameter_instantiation = wrap type_parameter_instantiation
    let _object ?(allow_static=false) env = wrap (_object ~allow_static) env
    let function_param_list = wrap function_param_list
    let annotation = wrap annotation
    let annotation_opt = wrap annotation_opt
    let generic = wrap generic
  end

  module Declaration = struct
    let check_param =
      let rec pattern ((env, _) as check_env) (loc, p) = Pattern.(match p with
        | Object o -> _object check_env o
        | Array arr -> _array check_env arr
        | Assignment { Assignment.left; _ } -> pattern check_env left
        | Identifier id -> identifier check_env id
        | Expression _ -> (
            error_at env (loc, Error.ExpectedPatternFoundExpression);
            check_env
          )
      )

      and _object check_env o =
        List.fold_left
          object_property
          check_env
          o.Pattern.Object.properties

      and object_property check_env = Pattern.Object.(function
        | Property (_, property) -> Property.(
            let check_env = match property.key with
            | Identifier id -> identifier_no_dupe_check check_env id
            | _ -> check_env in
            pattern check_env property.pattern)
        | SpreadProperty (_, { SpreadProperty.argument; }) ->
            pattern check_env argument)

      and _array check_env arr =
        List.fold_left
        array_element
        check_env
        arr.Pattern.Array.elements

      and array_element check_env = Pattern.Array.(function
        | None -> check_env
        | Some (Element p) -> pattern check_env p
        | Some (Spread (_, { SpreadElement.argument; })) ->
            pattern check_env argument)

      and identifier (env, param_names) (loc, { Identifier.name; _ } as id) =
        if SSet.mem name param_names
        then error_at env (loc, Error.StrictParamDupe);
        let env, param_names =
          identifier_no_dupe_check (env, param_names) id in
        env, SSet.add name param_names

      and identifier_no_dupe_check (env, param_names) (loc, { Identifier.name; _ }) =
        if is_restricted name
        then strict_error_at env (loc, Error.StrictParamName);
        if is_future_reserved name || is_strict_reserved name
        then strict_error_at env (loc, Error.StrictReservedWord);
        env, param_names

      in pattern

    (* Strict is true if we were already in strict mode or if we are newly in
     * strict mode due to a directive in the function.
     * Simple is the IsSimpleParameterList thing from the ES6 spec *)
    let strict_post_check env ~strict ~simple id params =
      if strict || not simple
      then
        (* If we are doing this check due to strict mode than there are two
         * cases to consider. The first is when we were already in strict mode
         * and therefore already threw strict errors. In this case we want to
         * do these checks outside of strict mode. The other is if we
         * originally parsed in non-strict mode but now are strict. Then we
         * want to do these checks in strict mode *)
        let env =
          if strict
          then env |> with_strict (not (Parser_env.in_strict_mode env))
          else env in
        (match id with
        | Some (loc, { Identifier.name; _ }) ->
            if is_restricted name
            then strict_error_at env (loc, Error.StrictFunctionName);
            if is_future_reserved name || is_strict_reserved name
            then strict_error_at env (loc, Error.StrictReservedWord)
        | None -> ());
        ignore (List.fold_left check_param (env, SSet.empty) params)

    let function_params =
      let rec param env =
        let id = Parse.pattern env Error.StrictParamName in
        if Peek.token env = T_ASSIGN
        then begin
          Expect.token env T_ASSIGN;
          let default = Parse.assignment env in
          id, Some default
        end else
          id, None
      and param_list env (params, defaults, has_default) =
        match Peek.token env with
        | T_EOF
        | T_RPAREN
        | T_ELLIPSIS as t ->
            let rest = if t = T_ELLIPSIS
            then begin
              Expect.token env T_ELLIPSIS;
              Some (Parse.identifier_with_type env Error.StrictParamName)
            end else None in
            if Peek.token env <> T_RPAREN
            then error env Error.ParameterAfterRestParameter;
            List.rev params, (if has_default then List.rev defaults else []), rest
        | _ ->
            let param, default = param env in
            let has_default = has_default || default <> None in
            if Peek.token env <> T_RPAREN
            then Expect.token env T_COMMA;
            param_list env (param::params, default::defaults, has_default)

      in fun env ->
        Expect.token env T_LPAREN;
        let params, defaults, rest = param_list env ([], [], false) in
        Expect.token env T_RPAREN;
        params, defaults, rest

    let function_body env ~async ~generator =
      let env = enter_function env ~async ~generator in
      let loc, block, strict = Parse.function_block_body env in
      loc, Function.BodyBlock (loc, block), strict

    let concise_function_body env ~async ~generator =
      let env = env |> with_in_function true in
      match Peek.token env with
      | T_LCURLY ->
          let _, body, strict = function_body env ~async ~generator in
          body, strict
      | _ ->
          let env = enter_function env ~async ~generator in
          let expr = Parse.assignment env in
          Function.BodyExpression expr, in_strict_mode env

    let generator env is_async =
      match is_async, Expect.maybe env T_MULT with
      | true, true ->
          error env Error.AsyncGenerator;
          true
      | _, result -> result

    let async env = Expect.maybe env T_ASYNC

    let is_simple_function_params =
      let is_simple_param = function
      | _, Pattern.Identifier _ ->  true
      | _ -> false

      in fun params defaults rest ->
        defaults = [] && rest = None && List.for_all is_simple_param params

    let _function env =
      let start_loc = Peek.loc env in
      let async = async env in
      Expect.token env T_FUNCTION;
      let generator = generator env async in
      let (typeParameters, id) = (
        match in_export env, Peek.token env with
        | true, T_LPAREN -> (None, None)
        | true, T_LESS_THAN ->
          let typeParams = Type.type_parameter_declaration env in
          let id = if Peek.token env = T_LPAREN then None else Some (
            Parse.identifier ~restricted_error:Error.StrictFunctionName env
          ) in
          (typeParams, id)
        | _ ->
          let id =
            Parse.identifier ~restricted_error:Error.StrictFunctionName env
          in
          (Type.type_parameter_declaration env, Some id)
      ) in
      let params, defaults, rest = function_params env in
      let returnType = Type.annotation_opt env in
      let predicate = Parse.predicate env in
      let _, body, strict = function_body env ~async ~generator in
      let simple = is_simple_function_params params defaults rest in
      strict_post_check env ~strict ~simple id params;
      let end_loc, expression = Ast.Function.(
        match body with
        | BodyBlock (loc, _) -> loc, false
        | BodyExpression (loc, _) -> loc, true) in
      Loc.btwn start_loc end_loc, Statement.(FunctionDeclaration Function.({
        id;
        params;
        defaults;
        rest;
        body;
        generator;
        async;
        predicate;
        expression;
        returnType;
        typeParameters;
      }))

    let variable_declaration_list =
      let variable_declaration env =
        let id = Parse.pattern env Error.StrictVarName in
        let init, errs = if Peek.token env = T_ASSIGN
        then begin
          Expect.token env T_ASSIGN;
          Some (Parse.assignment env), []
        end else Ast.Pattern.(
          match id with
          | _, Identifier _ -> None, []
          | loc, _ -> None, [(loc, Error.NoUninitializedDestructuring)]
        ) in
        let end_loc = match init with
        | Some expr -> fst expr
        | _ -> fst id in
        (Loc.btwn (fst id) end_loc, Ast.Statement.VariableDeclaration.Declarator.({
          id;
          init;
        })), errs

      in let rec helper env decls errs =
        let decl, errs_ = variable_declaration env in
        let decls = decl::decls in
        let errs = errs_ @ errs in
        if Peek.token env = T_COMMA
        then begin
          Expect.token env T_COMMA;
          helper env decls errs
        end else
          let end_loc = match decls with
          | (loc, _)::_ -> loc
          | _ -> Loc.none in
          let declarations = List.rev decls in
          let start_loc = match decls with
          | (loc, _)::_ -> loc
          | _ -> Loc.none in
          Loc.btwn start_loc end_loc, declarations, List.rev errs

      in fun env -> helper env [] []

    let declarations token kind env =
      let start_loc = Peek.loc env in
      Expect.token env token;
      let loc, declarations, errs = variable_declaration_list env in
      (Loc.btwn start_loc loc, Statement.VariableDeclaration.({
        kind;
        declarations;
      })), errs

    let var = declarations T_VAR Statement.VariableDeclaration.Var

    let const env =
      let env = env |> with_no_let true in
      let (loc, variable), errs =
        declarations T_CONST Statement.VariableDeclaration.Const env in
      (* Make sure all consts defined are initialized *)
      let errs = Statement.VariableDeclaration.(
        List.fold_left (fun errs decl ->
          match decl with
          | loc, { Declarator.init = None; _ } ->
              (loc, Error.NoUninitializedConst)::errs
          | _ -> errs
        ) errs variable.declarations
      ) in
      (loc, variable), List.rev errs

    let _let env =
      let env = env |> with_no_let true in
      declarations T_LET Statement.VariableDeclaration.Let env

    let variable env =
      let start_loc = Peek.loc env in
      let (end_loc, variable), errs = match Peek.token env with
      | T_CONST -> const env
      | T_LET   -> _let env
      | T_VAR   -> var env
      | _ ->
          error_unexpected env;
          (* We need to return something. This is as good as anything else *)
          var env in
      (Loc.btwn start_loc end_loc, Statement.VariableDeclaration variable), errs
  end

  module Expression = struct
    type op_precedence = Left_assoc of int | Right_assoc of int
    let is_tighter a b =
      let a_prec = match a with Left_assoc x -> x | Right_assoc x -> x - 1 in
      let b_prec = match b with Left_assoc x -> x | Right_assoc x -> x in
      a_prec >= b_prec

    (* AssignmentExpression :
     *   ConditionalExpression
     *   LeftHandSideExpression = AssignmentExpression
     *   LeftHandSideExpression AssignmentOperator AssignmentExpression
     *   ArrowFunctionFunction
     *
     *   Originally we were parsing this without backtracking, but
     *   ArrowFunctionExpression got too tricky. Oh well.
     *)
    let rec assignment =
      let assignment_but_not_arrow_function env =
        let expr = conditional env in
        (match assignment_op env with
        | Some operator ->
          if not (is_assignable_lhs expr)
          then error_at env (fst expr, Error.InvalidLHSInAssignment);

          (match expr with
          | loc, Expression.Identifier (_, { Identifier.name = name; _ })
            when is_restricted name ->
              strict_error_at env (loc, Error.StrictLHSAssignment)
          | _ -> ());

          let left = Parse.pattern_from_expr env expr in
          let right = assignment env in
          let loc = Loc.btwn (fst left) (fst right) in

          loc, Expression.(Assignment Assignment.({
            operator;
            left;
            right;
          }))
        | _ -> expr)

      in let error_callback _ _ = raise Try.Rollback

      (* So we may or may not be parsing the first part of an arrow function
       * (the part before the =>). We might end up parsing that whole thing or
       * we might end up parsing only part of it and thinking we're done. We
       * need to look at the next token to figure out if we really parsed an
       * assignment expression or if this is just the beginning of an arrow
       * function *)
      in let try_assignment_but_not_arrow_function env =
        let env = env |> with_error_callback error_callback in
        let ret = assignment_but_not_arrow_function env in
        match Peek.token env with
        | T_ARROW (* x => 123 *)
        | T_COLON -> (* (x): number => 123 *)
          raise Try.Rollback
        | _ when Peek.is_identifier env ->
          (* (x): number checks => 123 *)
          if Peek.value env = "checks" then
            raise Try.Rollback
          (* async x => 123 -- and we've already parsed async as an identifier
           * expression *)
          else begin match snd ret with
            | Expression.Identifier (_, {Identifier.name = "async"; _ })
                when not (Peek.is_line_terminator env) ->
              raise Try.Rollback
            | _ -> ret
            end
        | _ -> ret

      in fun env ->
        match Peek.token env, Peek.is_identifier env with
        | T_YIELD, _ when (allow_yield env) -> yield env
        | T_LPAREN, _
        | T_LESS_THAN, _
        | _, true ->

          (* Ok, we don't know if this is going to be an arrow function of a
          * regular assignment expression. Let's first try to parse it as an
          * assignment expression. If that fails we'll try an arrow function.
          *)
          (match Try.to_parse env try_assignment_but_not_arrow_function with
          | Try.ParsedSuccessfully expr -> expr
          | Try.FailedToParse ->
            (match Try.to_parse env try_arrow_function with
              | Try.ParsedSuccessfully expr -> expr
              | Try.FailedToParse ->

                  (* Well shoot. It doesn't parse cleanly as a normal
                   * expression or as an arrow_function. Let's treat it as a
                   * normal assignment expression gone wrong *)
                  assignment_but_not_arrow_function env
            )
          )
        | _ -> assignment_but_not_arrow_function env

    and yield env =
      let start_loc = Peek.loc env in
      Expect.token env T_YIELD;
      if not (allow_yield env)
      then error env Error.IllegalYield;
      let delegate = Expect.maybe env T_MULT in
      let has_argument = not (
        Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
      ) in
      let argument =
        if delegate || has_argument
        then Some (assignment env)
        else None in
      let end_loc = match argument with
      | Some expr -> fst expr
      | None ->
          let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None -> start_loc in
          Eat.semicolon env;
          end_loc in
      Loc.btwn start_loc end_loc, Expression.(Yield Yield.({
        argument;
        delegate;
      }))

    and is_lhs = Expression.(function
      | _, Member _
      | _, Identifier _ -> true
      | _, Array _
      | _, Object _
      | _, Literal _
      | _, TemplateLiteral _
      | _, TaggedTemplate _
      | _, This
      | _, Class _
      | _, Function _
      | _, New _
      | _, Call _
      | _, Comprehension _
      | _, Generator _
      | _, Assignment _
      | _, Binary _
      | _, Conditional _
      | _, Logical _
      | _, Sequence _
      | _, Unary _
      | _, Update _
      | _, ArrowFunction _
      | _, Yield _
      | _, JSXElement _
      | _, Let _
      | _, TypeCast _ -> false)

    and is_assignable_lhs = Expression.(function
      | _, Array _
      | _, Object _
      | _, Member _
      | _, Identifier _ -> true
      | _, Literal _
      | _, TemplateLiteral _
      | _, TaggedTemplate _
      | _, This
      | _, Class _
      | _, Function _
      | _, New _
      | _, Call _
      | _, Comprehension _
      | _, Generator _
      | _, Assignment _
      | _, Binary _
      | _, Conditional _
      | _, Logical _
      | _, Sequence _
      | _, Unary _
      | _, Update _
      | _, ArrowFunction _
      | _, Yield _
      | _, JSXElement _
      | _, Let _
      | _, TypeCast _ -> false)


    and assignment_op env =
      let op = Expression.Assignment.(match Peek.token env with
      | T_RSHIFT3_ASSIGN -> Some RShift3Assign
      | T_RSHIFT_ASSIGN -> Some RShiftAssign
      | T_LSHIFT_ASSIGN -> Some LShiftAssign
      | T_BIT_XOR_ASSIGN -> Some BitXorAssign
      | T_BIT_OR_ASSIGN -> Some BitOrAssign
      | T_BIT_AND_ASSIGN -> Some BitAndAssign
      | T_MOD_ASSIGN -> Some ModAssign
      | T_DIV_ASSIGN -> Some DivAssign
      | T_MULT_ASSIGN -> Some MultAssign
      | T_EXP_ASSIGN -> Some ExpAssign
      | T_MINUS_ASSIGN -> Some MinusAssign
      | T_PLUS_ASSIGN -> Some PlusAssign
      | T_ASSIGN -> Some Assign
      | _ -> None) in
      if op <> None then Eat.token env;
      op

    and conditional env =
      let start_loc = Peek.loc env in
      let expr = logical env in
      if Peek.token env = T_PLING
      then begin
        Expect.token env T_PLING;
        (* no_in is ignored for the consequent *)
        let env' = env |> with_no_in false in
        let consequent = assignment env' in
        Expect.token env T_COLON;
        let end_loc, alternate = with_loc assignment env in
        let loc = Loc.btwn start_loc end_loc in
        loc, Expression.(Conditional Conditional.({
          test = expr;
          consequent;
          alternate;
        }))
      end else expr

    and logical =
      let open Expression in
      let make_logical left right operator loc =
        loc, Logical Logical.({operator; left; right;})
      in let rec logical_and env left lloc =
        match Peek.token env with
        | T_AND ->
            Expect.token env T_AND;
            let rloc, right = with_loc binary env in
            let loc = Loc.btwn lloc rloc in
            logical_and env (make_logical left right Logical.And loc) loc
        | _  -> lloc, left
      and logical_or env left lloc =
        match Peek.token env with
        | T_OR ->
            Expect.token env T_OR;
            let rloc, right = with_loc binary env in
            let rloc, right = logical_and env right rloc in
            let loc = Loc.btwn lloc rloc in
            logical_or env (make_logical left right Logical.Or loc) loc
        | _ -> lloc, left
      in fun env ->
        let loc, left = with_loc binary env in
        let loc, left = logical_and env left loc in
        let _, type_ = logical_or env left loc in
        type_

    and binary =
      let binary_op env =
        let ret = Expression.Binary.(match Peek.token env with
        (* Most BinaryExpression operators are left associative *)
        (* Lowest pri *)
        | T_BIT_OR -> Some (BitOr, Left_assoc 2)
        | T_BIT_XOR -> Some (Xor, Left_assoc 3)
        | T_BIT_AND -> Some (BitAnd, Left_assoc 4)
        | T_EQUAL -> Some (Equal, Left_assoc 5)
        | T_STRICT_EQUAL -> Some (StrictEqual, Left_assoc 5)
        | T_NOT_EQUAL -> Some (NotEqual, Left_assoc 5)
        | T_STRICT_NOT_EQUAL -> Some (StrictNotEqual, Left_assoc 5)
        | T_LESS_THAN -> Some (LessThan, Left_assoc 6)
        | T_LESS_THAN_EQUAL -> Some (LessThanEqual, Left_assoc 6)
        | T_GREATER_THAN -> Some (GreaterThan, Left_assoc 6)
        | T_GREATER_THAN_EQUAL -> Some (GreaterThanEqual, Left_assoc 6)
        | T_IN ->
            if (no_in env) then None else Some (In, Left_assoc 6)
        | T_INSTANCEOF -> Some (Instanceof, Left_assoc 6)
        | T_LSHIFT -> Some (LShift, Left_assoc 7)
        | T_RSHIFT -> Some (RShift, Left_assoc 7)
        | T_RSHIFT3 -> Some (RShift3, Left_assoc 7)
        | T_PLUS -> Some (Plus, Left_assoc 8)
        | T_MINUS -> Some (Minus, Left_assoc 8)
        | T_MULT -> Some (Mult, Left_assoc 9)
        | T_DIV -> Some (Div, Left_assoc 9)
        | T_MOD -> Some (Mod, Left_assoc 9)
        | T_EXP -> Some (Exp, Right_assoc 10)
        (* Highest priority *)
        | _ -> None)
        in if ret <> None then Eat.token env;
        ret

      in let make_binary left right operator loc =
        loc, Expression.(Binary Binary.({
          operator;
          left;
          right;
        }))

      in let rec add_to_stack right (rop, rpri) rloc = function
        | (left, (lop, lpri), lloc)::rest when is_tighter lpri rpri ->
            let loc = Loc.btwn lloc rloc in
            let right = make_binary left right lop loc in
            add_to_stack right (rop, rpri) loc rest
        | stack -> (right, (rop, rpri), rloc)::stack

      in let rec collapse_stack right rloc = function
        | [] -> right
        | (left, (lop, _), lloc)::rest ->
            let loc = Loc.btwn lloc rloc in
            collapse_stack (make_binary left right lop loc) loc rest

      in let rec helper env stack =
        let start_loc = Peek.loc env in
        let is_unary = peek_unary_op env <> None in
        let right = unary (env |> with_no_in false) in
        let end_loc = match last_loc env with
        | Some loc -> loc
        | None -> fst right
        in
        let right_loc = Loc.btwn start_loc end_loc in
        if Peek.token env = T_LESS_THAN
        then begin
          match right with
          | _, Expression.JSXElement _ ->
              error env Error.AdjacentJSXElements
          | _ -> ()
        end;
        match binary_op env with
        | None ->
          collapse_stack right right_loc stack
        | Some (rop, rpri) ->
          if is_unary && rop = Expression.Binary.Exp then
            error_at env (right_loc, Error.InvalidLHSInExponentiation);
          helper env (add_to_stack right (rop, rpri) right_loc stack)

      in fun env -> helper env []

    and peek_unary_op env =
      let open Expression.Unary in
      match Peek.token env with
      | T_NOT -> Some Not
      | T_BIT_NOT -> Some BitNot
      | T_PLUS -> Some Plus
      | T_MINUS -> Some Minus
      | T_TYPEOF -> Some Typeof
      | T_VOID -> Some Void
      | T_DELETE -> Some Delete
      (* If we are in a unary expression context, and within an async function,
       * assume that a use of "await" is intended as a keyword, not an ordinary
       * identifier. This is a little bit inconsistent, since it can be used as
       * an identifier in other contexts (such as a variable name), but it's how
       * Babel does it. *)
      | T_AWAIT when allow_await env -> Some Await
      | _ -> None

    and unary env =
      let begin_loc = Peek.loc env in
      let op = peek_unary_op env in
      match op with
      | None -> begin
          let op = Expression.Update.(match Peek.token env with
          | T_INCR -> Some Increment
          | T_DECR -> Some Decrement
          | _ -> None) in
          match op with
          | None -> postfix env
          | Some operator ->
              Eat.token env;
              let argument = unary env in
              if not (is_lhs argument)
              then error_at env (fst argument, Error.InvalidLHSInAssignment);
              (match argument with
              | _, Expression.Identifier (_, { Identifier.name; _ })
                when is_restricted name ->
                  strict_error env Error.StrictLHSPrefix
              | _ -> ());
              Loc.btwn begin_loc (fst argument), Expression.(Update Update.({
                operator;
                prefix = true;
                argument;
              }))
        end
      | Some operator ->
        Eat.token env;
        let argument = unary env in
        let loc = Loc.btwn begin_loc (fst argument) in
        Expression.(match operator, argument with
        | Unary.Delete, (_, Identifier _) ->
            strict_error_at env (loc, Error.StrictDelete)
        | _ -> ());
        loc, Expression.(Unary Unary.({
          operator;
          prefix = true;
          argument;
        }))

    and postfix env =
      let argument = left_hand_side env in
      (* No line terminator allowed before operator *)
      if Peek.is_line_terminator env
      then argument
      else let op = Expression.Update.(match Peek.token env with
      | T_INCR -> Some Increment
      | T_DECR -> Some Decrement
      | _ -> None) in
      match op with
      | None -> argument
      | Some operator ->
          if not (is_lhs argument)
          then error_at env (fst argument, Error.InvalidLHSInAssignment);
          (match argument with
          | _, Expression.Identifier (_, { Identifier.name; _ })
            when is_restricted name ->
              strict_error env Error.StrictLHSPostfix
          | _ -> ());
          let end_loc = Peek.loc env in
          Eat.token env;
          Loc.btwn (fst argument) end_loc, Expression.(Update Update.({
            operator;
            prefix = false;
            argument;
          }))

    and left_hand_side env =
      let expr = match Peek.token env with
      | T_NEW -> _new env (fun new_expr _args -> new_expr)
      | _ when Peek.is_function env -> _function env
      | _ -> primary env in
      let expr = member env expr in
      match Peek.token env with
      | T_LPAREN -> call env expr
      | T_TEMPLATE_PART part ->
          member env (tagged_template env expr part)
      | _ -> expr

    and call env left =
      match Peek.token env with
      | T_LPAREN when not (no_call env) ->
          let args_loc, arguments = arguments env in
          call env (Loc.btwn (fst left) args_loc, Expression.(Call Call.({
            callee = left;
            arguments;
          })))
      | T_LBRACKET ->
          Expect.token env T_LBRACKET;
          let expr = Parse.expression env in
          let last_loc = Peek.loc env in
          let loc = Loc.btwn (fst left) last_loc in
          Expect.token env T_RBRACKET;
          call env (loc, Expression.(Member Member.({
            _object  = left;
            property = PropertyExpression expr;
            computed = true;
          })))
      | T_PERIOD ->
          Expect.token env T_PERIOD;
          let id, _ = identifier_or_reserved_keyword env in
          call env (Loc.btwn (fst left) (fst id), Expression.(Member Member.({
            _object  = left;
            property = PropertyIdentifier id;
            computed = false;
          })))
      | T_TEMPLATE_PART part -> tagged_template env left part
      | _ -> left

    and _new env finish_fn =
      match Peek.token env with
      | T_NEW ->
          let start_loc = Peek.loc env in
          Expect.token env T_NEW;
          let finish_fn' callee args =
            let end_loc, arguments = match args with
            | Some (loc, args) -> loc, args
            | _ -> fst callee, [] in
            let callee' = Loc.btwn start_loc end_loc, Expression.(New New.({
              callee;
              arguments;
            })) in
            finish_fn callee' None in
          _new env finish_fn'
      | _ ->
          let expr = match Peek.token env with
          | _ when Peek.is_function env -> _function env
          | _ -> primary env in
          let callee = member (env |> with_no_call true) expr in
          (* You can do something like
           *   new raw`42`
           *)
          let callee = match Peek.token env with
          | T_TEMPLATE_PART part -> tagged_template env callee part
          | _ -> callee in
          let args = match Peek.token env with
          | T_LPAREN -> Some (arguments env)
          | _ -> None in
          finish_fn callee args

    and arguments =
      let argument env =
        match Peek.token env with
        | T_ELLIPSIS ->
            let start_loc = Peek.loc env in
            Expect.token env T_ELLIPSIS;
            let argument = assignment env in
            let loc = Loc.btwn start_loc (fst argument) in
            Expression.(Spread (loc, SpreadElement.({
              argument;
            })))
        | _ -> Expression.Expression (assignment env)

      in let rec arguments' env acc =
        match Peek.token env with
        | T_EOF
        | T_RPAREN -> List.rev acc
        | _ ->
            let acc = (argument env)::acc in
            if Peek.token env <> T_RPAREN
            then Expect.token env T_COMMA;
            arguments' env acc

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LPAREN;

        let args = arguments' env []

        in let end_loc = Peek.loc env in
        Expect.token env T_RPAREN;
        Loc.btwn start_loc end_loc, args

    and member env left =
      match Peek.token env with
      | T_LBRACKET ->
          Expect.token env T_LBRACKET;
          let expr = Parse.expression (env |> with_no_call false) in
          let last_loc = Peek.loc env in
          Expect.token env T_RBRACKET;
          call env (Loc.btwn (fst left) last_loc, Expression.(Member Member.({
            _object  = left;
            property = PropertyExpression expr;
            computed = true;
          })))
      | T_PERIOD ->
          Expect.token env T_PERIOD;
          let id, _ = identifier_or_reserved_keyword env in
          call env (Loc.btwn (fst left) (fst id), Expression.(Member Member.({
            _object  = left;
            property = PropertyIdentifier id;
            computed = false;
          })))
      | _ -> left

    and _function env =
      let start_loc = Peek.loc env in
      let async = Declaration.async env in
      Expect.token env T_FUNCTION;
      let generator = Declaration.generator env async in
      let id, typeParameters =
        if Peek.token env = T_LPAREN
        then None, None
        else begin
          let id = match Peek.token env with
            | T_LESS_THAN -> None
            | _ -> Some (Parse.identifier ~restricted_error:Error.StrictFunctionName env) in
          id, Type.type_parameter_declaration env
        end in
      let params, defaults, rest = Declaration.function_params env in
      let returnType = Type.annotation_opt env in
      let predicate = Parse.predicate env in
      let end_loc, body, strict =
        Declaration.function_body env ~async ~generator in
      let simple = Declaration.is_simple_function_params params defaults rest in
      Declaration.strict_post_check env ~strict ~simple id params;
      let expression = Function.(
        match body with
        | BodyBlock _ -> false
        | BodyExpression _ -> true) in
      Loc.btwn start_loc end_loc, Expression.(Function Function.({
        id;
        params;
        defaults;
        rest;
        body;
        generator;
        async;
        predicate;
        expression;
        returnType;
        typeParameters;
      }))

    and number env number_type =
      let value = Peek.value env in
      let value = match number_type with
      | LEGACY_OCTAL ->
        strict_error env Error.StrictOctalLiteral;
        float (int_of_string ("0o"^value))
      | BINARY
      | OCTAL ->
        float (int_of_string value)
      | NORMAL ->
        try Lexer_flow.FloatOfString.float_of_string value
        with _ when Sys.win32 ->
          error env Parse_error.WindowsFloatOfString;
          789.0
      in
      Expect.token env (T_NUMBER number_type);
      value

    and primary env =
      let loc = Peek.loc env in
      match Peek.token env with
      | T_THIS ->
          Expect.token env T_THIS;
          loc, Expression.This
      | T_NUMBER number_type ->
          let raw = Peek.value env in
          let value = Literal.Number (number env number_type) in
          loc, Expression.(Literal { Literal.value; raw; })
      | T_STRING (loc, value, raw, octal) ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (loc, value, raw, octal));
          let value = Literal.String value in
          loc, Expression.(Literal { Literal.value; raw; })
      | (T_TRUE | T_FALSE) as token ->
          let raw = Peek.value env in
          Expect.token env token;
          let value = (Literal.Boolean (token = T_TRUE)) in
          loc, Expression.(Literal { Literal.value; raw; })
      | T_NULL ->
          let raw = Peek.value env in
          Expect.token env T_NULL;
          let value = Literal.Null in
          loc, Expression.(Literal { Literal.value; raw; })
      | T_LPAREN -> group env
      | T_LCURLY -> object_initializer env
      | T_LBRACKET ->
          let loc, arr = array_initializer env in
          loc, Expression.Array arr
      | T_DIV
      | T_DIV_ASSIGN -> regexp env
      | T_LESS_THAN ->
          let loc, element = Parse.jsx_element env in
          loc, Expression.JSXElement element
      | T_TEMPLATE_PART part ->
          let loc, template = template_literal env part in
          loc, Expression.(TemplateLiteral template)
      | T_CLASS -> Parse.class_expression env
      | T_SUPER ->
          let loc = Peek.loc env in
          Expect.token env T_SUPER;
          let id = loc, {
            Identifier.name = "super";
            typeAnnotation = None;
            optional = false; } in
          loc, Expression.Identifier id
      | _ when Peek.is_identifier env ->
          let id = Parse.identifier env in
          fst id, Expression.Identifier id
      | t ->
          error_unexpected env;
          (* Let's get rid of the bad token *)
          if t = T_ERROR
          then Eat.token env;
          (* Really no idea how to recover from this. I suppose a null
           * expression is as good as anything *)
          let value = Literal.Null in
          let raw = "null" in
          loc, Expression.(Literal { Literal.value; raw; })

    and object_initializer env =
      let loc, obj = Parse.object_initializer env in
      loc, Expression.Object obj

    and template_literal =
      let rec template_parts env quasis expressions =
        let expr = Parse.expression env in
        let expressions = expr::expressions in
        match Peek.token env with
        | T_RCURLY ->
            Eat.push_lex_mode env Lex_mode.TEMPLATE;
            let loc, part, is_tail = match Peek.token env with
            | T_TEMPLATE_PART (loc, {cooked; raw; _}, tail) ->
                let open Ast.Expression.TemplateLiteral in
                Eat.token env;
                loc, { Element.value = { Element.cooked; raw; }; tail; }, tail
            | _ -> assert false in
            Eat.pop_lex_mode env;
            let quasis = (loc, part)::quasis in
            if is_tail
            then loc, List.rev quasis, List.rev expressions
            else template_parts env quasis expressions
        | _ ->
            (* Malformed template *)
            error_unexpected env;
            let imaginary_quasi = fst expr, { Expression.TemplateLiteral.Element.
              value = { Expression.TemplateLiteral.Element.
                raw = "";
                cooked = "";
              };
              tail = true;
            } in
            fst expr, List.rev (imaginary_quasi::quasis), List.rev expressions

      in fun env ((start_loc, {cooked; raw; _}, is_tail) as part) ->
        Expect.token env (T_TEMPLATE_PART part);
        let end_loc, quasis, expressions =
          let head = Ast.Expression.TemplateLiteral.(start_loc, {
            Element.value = { Element.cooked; raw; };
            tail = is_tail;
          }) in
          if is_tail
          then start_loc, [head], []
          else template_parts env [head] [] in
        let loc = Loc.btwn start_loc end_loc in
        loc, Expression.TemplateLiteral.({
          quasis;
          expressions;
        })

    and tagged_template env tag part =
      let quasi = template_literal env part in
      Loc.btwn (fst tag) (fst quasi), Expression.(TaggedTemplate TaggedTemplate.({
        tag;
        quasi;
      }))

    and group env =
      Expect.token env T_LPAREN;
      let expression = assignment env in
      let ret = (match Peek.token env with
      | T_COMMA -> sequence env [expression]
      | T_COLON ->
          let typeAnnotation = Type.annotation env in
          Expression.(Loc.btwn (fst expression) (fst typeAnnotation),
            TypeCast TypeCast.({
              expression;
              typeAnnotation;
            }))
      | _ -> expression) in
      Expect.token env T_RPAREN;
      ret

    and array_initializer =
      let rec elements env acc =
        match Peek.token env with
        | T_EOF
        | T_RBRACKET -> List.rev acc
        | T_COMMA ->
            Expect.token env T_COMMA;
            elements env (None::acc)
        | T_ELLIPSIS ->
            let start_loc = Peek.loc env in
            Expect.token env T_ELLIPSIS;
            let argument = assignment env in
            let loc = Loc.btwn start_loc (fst argument) in
            let elem = Expression.(Spread (loc, SpreadElement.({
              argument;
            }))) in
            elements env ((Some elem)::acc)
        | _ ->
            let elem = Expression.Expression (assignment env) in
            if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
            elements env ((Some elem)::acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LBRACKET;
        let elements = elements env [] in
        let end_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        Loc.btwn start_loc end_loc, Expression.Array.({
          elements;
        })

    and regexp env =
      Eat.push_lex_mode env Lex_mode.REGEXP;
      let loc = Peek.loc env in
      let raw, pattern, raw_flags = match Peek.token env with
        | T_REGEXP (_, pattern, flags) ->
            let raw = Peek.value env in
            Eat.token env;
            raw, pattern, flags
        | _ -> assert false in
      Eat.pop_lex_mode env;
      let filtered_flags = Buffer.create (String.length raw_flags) in
      String.iter (function
        | 'g' | 'i' | 'm' | 'y' as c -> Buffer.add_char filtered_flags c
        | _ -> ()) raw_flags;
      let flags = Buffer.contents filtered_flags in
      if flags <> raw_flags
      then error env (Error.InvalidRegExpFlags raw_flags);
      let value = Literal.(RegExp { RegExp.pattern; flags; }) in
      loc, Expression.(Literal { Literal.value; raw; })

    and try_arrow_function =
      (* Certain errors (almost all errors) cause a rollback *)
      let error_callback _ = Error.(function
        (* Don't rollback on these errors. *)
        | StrictParamName
        | ParameterAfterRestParameter
        | NewlineBeforeArrow -> ()
        (* Everything else causes a rollback *)
        | _ -> raise Try.Rollback) in

      fun env ->
        let env = env |> with_error_callback error_callback in

        let start_loc = Peek.loc env in
        (* a T_ASYNC could either be a parameter name or it could be indicating
         * that it's an async function *)
        let async = Peek.token ~i:1 env <> T_ARROW && Declaration.async env in
        let typeParameters = Type.type_parameter_declaration env in
        let params, defaults, rest, returnType =
          (* Disallow all fancy features for identifier => body *)
          if Peek.is_identifier env && typeParameters = None
          then
            let id =
              Parse.identifier ~restricted_error:Error.StrictParamName env in
            let param = fst id, Pattern.Identifier id in
            [param], [], None, None
          else
            let params, defaults, rest = Declaration.function_params env in
            params, defaults, rest, Type.annotation_opt env in

        let predicate = Parse.predicate env in

        (* It's hard to tell if an invalid expression was intended to be an
         * arrow function before we see the =>. If there are no params, that
         * implies "()" which is only ever found in arrow params. Similarly,
         * rest params indicate arrow functions. Therefore, if we see a rest
         * param or an empty param list then we can disable the rollback and
         * instead generate errors as if we were parsing an arrow function *)
        let env =
          if params = [] || rest <> None
          then without_error_callback env
          else env in

        if Peek.is_line_terminator env && Peek.token env = T_ARROW
        then error env Error.NewlineBeforeArrow;
        Expect.token env T_ARROW;

        (* Now we know for sure this is an arrow function *)
        let env = without_error_callback env in

        let end_loc, (body, strict) = with_loc
          (Declaration.concise_function_body ~async ~generator:false)
          env
        in
        let simple =
          Declaration.is_simple_function_params params defaults rest in
        Declaration.strict_post_check env ~strict ~simple None params;
        let expression = Function.(
          match body with
          | BodyBlock _ -> false
          | BodyExpression _ -> true) in
        let loc = Loc.btwn start_loc end_loc in
        loc, Expression.(ArrowFunction Function.({
          id = None;
          params;
          defaults;
          rest;
          body;
          async;
          generator = false; (* arrow functions cannot be generators *)
          (* TODO: add syntax support for arrow predicates *)
          predicate;
          expression;
          returnType;
          typeParameters;
        }))

    and sequence env acc =
      match Peek.token env with
      | T_COMMA ->
          Expect.token env T_COMMA;
          let expr = assignment env in
          sequence env (expr::acc)
      | _ ->
        let last_loc = (match acc with
          | (loc, _)::_ -> loc
          | _ -> Loc.none) in
        let expressions = List.rev acc in
        let first_loc = (match expressions with
          | (loc, _)::_ -> loc
          | _ -> Loc.none) in
        Loc.btwn first_loc last_loc, Expression.(Sequence Sequence.({
          expressions;
        }))

    (* You can do things like
     * var x = { if : 4 }
     * x.if
     *)
    and identifier_or_reserved_keyword env =
      let lex_token = Peek.token env in
      let lex_value = Peek.value env in
      let lex_loc = Peek.loc env in
      match lex_token with
      (* Anything that is a special token in Flow but not in the ES6 spec
         should be here. *)
      | T_ASYNC
      | T_DECLARE
      | T_IDENTIFIER
      | T_OF
      | T_TYPE
        -> Parse.identifier env, None
      | _ ->
        let err = match lex_token with
        | T_FUNCTION
        | T_IF
        | T_IN
        | T_INSTANCEOF
        | T_RETURN
        | T_SWITCH
        | T_THIS
        | T_THROW
        | T_TRY
        | T_VAR
        | T_WHILE
        | T_WITH
        | T_CONST
        | T_LET
        | T_NULL
        | T_FALSE
        | T_TRUE
        | T_BREAK
        | T_CASE
        | T_CATCH
        | T_CONTINUE
        | T_DEFAULT
        | T_DO
        | T_FINALLY
        | T_FOR
        | T_CLASS
        | T_EXTENDS
        | T_STATIC
        | T_ELSE
        | T_NEW
        | T_DELETE
        | T_TYPEOF
        | T_VOID
        | T_ENUM
        | T_EXPORT
        | T_IMPORT
        | T_SUPER
        | T_IMPLEMENTS
        | T_INTERFACE
        | T_PACKAGE
        | T_PRIVATE
        | T_PROTECTED
        | T_PUBLIC
        | T_YIELD
        | T_ANY_TYPE
        | T_BOOLEAN_TYPE
        | T_NUMBER_TYPE
        | T_STRING_TYPE
        | T_VOID_TYPE
        | T_AWAIT
        | T_DEBUGGER ->
            Some (lex_loc, get_unexpected_error (lex_token, lex_value))
        | _ ->
            error_unexpected env;
            None
        in
        Eat.token env;
        (lex_loc, Identifier.({
          name = lex_value;
          typeAnnotation = None;
          optional = false;
        })), err
  end

  (* A module for parsing various object related things, like object literals
   * and classes *)
  module Object : sig
    val key : env -> Loc.t * Ast.Expression.Object.Property.key
    val _initializer : env -> Loc.t * Ast.Expression.Object.t
    val class_declaration : env -> Ast.Expression.t list -> Ast.Statement.t
    val class_expression : env -> Ast.Expression.t
    val decorator_list : env -> Ast.Expression.t list
  end = struct
    let decorator_list =
      let rec decorator_list_helper env decorators =
        match Peek.token env with
        | T_AT ->
            Eat.token env;
            decorator_list_helper env ((Expression.left_hand_side env)::decorators)
        | _ ->
            decorators

      in fun env ->
        if (parse_options env).esproposal_decorators
        then List.rev (decorator_list_helper env [])
        else []

    let key env =
      Ast.Expression.Object.Property.(match Peek.token env with
      | T_STRING (loc, value, raw, octal) ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (loc, value, raw, octal));
          let value = Literal.String value in
          loc, Literal (loc, { Literal.value; raw; })
      | T_NUMBER number_type ->
          let raw = Peek.value env in
          let loc = Peek.loc env in
          let value = Expression.number env number_type in
          let value = Literal.Number value in
          loc,  Literal (loc, { Literal.value; raw; })
      | T_LBRACKET ->
          let start_loc = Peek.loc env in
          Expect.token env T_LBRACKET;
          let expr = Parse.assignment (env |> with_no_in false) in
          let end_loc = Peek.loc env in
          Expect.token env T_RBRACKET;
          Loc.btwn start_loc end_loc, Ast.Expression.Object.Property.Computed expr
      | _ ->
          let id, _ = Expression.identifier_or_reserved_keyword env in
          fst id, Identifier id)

    let _method env kind =
      (* this is a getter or setter, it cannot be async *)
      let async = false in
      let generator = Declaration.generator env async in
      let _, key = key env in
      (* It's not clear how type params on getters & setters would make sense
       * in Flow's type system. Since this is a Flow syntax extension, we might
       * as well disallow it until we need it *)
      let typeParameters = Ast.Expression.Object.Property.(match kind with
      | Get | Set -> None
      | _ -> Type.type_parameter_declaration env) in
      Expect.token env T_LPAREN;
      let params = Ast.Expression.Object.Property.(match kind with
      | Get -> []
      | Set ->
        (* TODO: support more param pattern types here *)
        let param = Parse.identifier_with_type env Error.StrictParamName in
        [ (fst param, Pattern.Identifier param) ]
      | Init -> assert false) in
      Expect.token env T_RPAREN;
      let returnType = Type.annotation_opt env in
      let _, body, strict = Declaration.function_body env ~async ~generator in
      let defaults = [] in
      let rest = None in
      let simple = Declaration.is_simple_function_params params defaults rest in
      Declaration.strict_post_check env ~strict ~simple None params;
      let end_loc, expression = Function.(
        match body with
        | BodyBlock (loc, _) -> loc, false
        | BodyExpression (loc, _) -> loc, true) in
      let value = end_loc, Function.({
        id = None;
        params;
        defaults;
        rest;
        body;
        generator;
        async;
        predicate = None; (* setters/getter are not predicates *)
        expression;
        returnType;
        typeParameters;
      }) in
      key, value

    let _initializer =
      let rec property env = Ast.Expression.Object.(
        let start_loc = Peek.loc env in
        if Peek.token env = T_ELLIPSIS
        then begin
          (* Spread property *)
          Expect.token env T_ELLIPSIS;
          let argument = Parse.assignment env in
          SpreadProperty (Loc.btwn start_loc (fst argument), SpreadProperty.({
            argument;
          }))
        end else begin
          (* look for a following identifier to tell whether to parse a function
           * or not *)
          let async = Peek.is_identifier ~i:1 env && Declaration.async env in
          Property (match async , Declaration.generator env async, key env with
          | false, false, (_, (Property.Identifier (_, { Ast.Identifier.name =
              "get"; _}) as key)) ->
              (match Peek.token env with
              | T_COLON
              | T_LESS_THAN
              | T_LPAREN -> init env start_loc key false false
              | _ -> get env start_loc)
          | false, false, (_, (Property.Identifier (_, { Ast.Identifier.name =
              "set"; _}) as key)) ->
              (match Peek.token env with
              | T_COLON
              | T_LESS_THAN
              | T_LPAREN -> init env start_loc key false false
              | _ -> set env start_loc)
          | async, generator, (_, key) ->
              init env start_loc key async generator
          )
        end
      )

      and get env start_loc =
        let key, (end_loc, fn) =
          _method env Ast.Expression.Object.Property.Get in
        let value = end_loc, Ast.Expression.Function fn in
        Loc.btwn start_loc end_loc, Ast.Expression.Object.Property.({
          key;
          value;
          kind = Get;
          _method = false;
          shorthand = false;
        })

      and set env start_loc =
        let key, (end_loc, fn) =
          _method env Ast.Expression.Object.Property.Set in
        let value = end_loc, Ast.Expression.Function fn in
        Loc.btwn start_loc end_loc, Ast.Expression.Object.Property.({
          key;
          value;
          kind = Set;
          _method = false;
          shorthand = false;
        })

      and init env start_loc key async generator =
        Ast.Expression.Object.Property.(
          let value, shorthand, _method =
            match Peek.token env with
            | T_RCURLY
            | T_COMMA ->
                (match key with
                | Literal lit -> fst lit, Ast.Expression.Literal (snd lit)
                | Identifier id -> fst id, Ast.Expression.Identifier id
                | Computed expr -> expr), true, false
            | T_LESS_THAN
            | T_LPAREN ->
                let typeParameters = Type.type_parameter_declaration env in
                let params, defaults, rest = Declaration.function_params env in
                let returnType = Type.annotation_opt env in
                let _, body, strict =
                  Declaration.function_body env ~async ~generator in
                let simple = Declaration.is_simple_function_params params defaults rest in
                Declaration.strict_post_check env ~strict ~simple None params;
                let end_loc, expression = Function.(
                  match body with
                  | BodyBlock (loc, _) -> loc, false
                  | BodyExpression (loc, _) -> loc, true) in
                let value = end_loc, Ast.Expression.(Function Function.({
                  id = None;
                  params;
                  defaults;
                  rest;
                  body;
                  generator;
                  async;
                  (* TODO: add support for object method predicates *)
                  predicate = None;
                  expression;
                  returnType;
                  typeParameters;
                })) in
                value, false, true
            | _ ->
              Expect.token env T_COLON;
              Parse.assignment env, false, false in
          Loc.btwn start_loc (fst value), {
            key;
            value;
            kind = Init;
            _method;
            shorthand;
          }
        )

      and check_property env prop_map prop = Ast.Expression.Object.(
        match prop with
        | Property (prop_loc, ({ Property.key = Property.Literal _ | Property.Identifier _; _ } as prop)) ->
            Property.(
              let key = match prop.key with
              | Literal (_, { Literal.value = Literal.String s; _; } ) -> s
              | Literal (_, { Literal.value = Literal.Boolean b; _; } ) -> string_of_bool b
              | Literal (_, { Literal.value = Literal.Null; _; } ) -> "null"
              | Literal (_, { Literal.value = Literal.Number f; _; } ) -> string_of_float f
              | Literal (_, { Literal.value = Literal.RegExp _; _; } ) ->
                  failwith "RegExp cannot be property key"
              | Identifier (_, { Identifier.name; _ }) -> name
              | Computed _ -> assert false in
              let prev_kinds =
                try SMap.find key prop_map
                with Not_found -> SSet.empty in
              let kind_string = match prop.kind with
              | Init -> "Init"
              | Get -> "Get"
              | Set -> "Set" in
              (match kind_string with
              | "Init" when SSet.mem "Init" prev_kinds ->
                  strict_error_at env (prop_loc, Error.StrictDuplicateProperty)
              | "Init" when SSet.mem "Set" prev_kinds || SSet.mem "Get" prev_kinds ->
                  error_at env (prop_loc, Error.AccessorDataProperty)
              | "Get"
              | "Set" when SSet.mem "Init" prev_kinds ->
                  error_at env (prop_loc, Error.AccessorDataProperty)
              | ("Get" | "Set") as kind when SSet.mem kind prev_kinds ->
                  error_at env (prop_loc, Error.AccessorGetSet)
              | _ -> ());
              let kinds = SSet.add kind_string prev_kinds in
              SMap.add key kinds prop_map)
        | _ -> prop_map
      )

      and properties env (prop_map, acc) =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | _ ->
            let prop = property env in
            let prop_map = check_property env prop_map prop in
            if Peek.token env <> T_RCURLY then Expect.token env T_COMMA;
            properties env (prop_map, prop::acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let props = properties env (SMap.empty, []) in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        Loc.btwn start_loc end_loc, Ast.Expression.Object.({
          properties = props;
        })

    let rec _class env =
      let superClass, superTypeParameters =
        if Peek.token env = T_EXTENDS
        then begin
          Expect.token env T_EXTENDS;
          let superClass =
            Expression.left_hand_side (env |> with_allow_yield false) in
          let superTypeParameters = Type.type_parameter_instantiation env in
          Some superClass, superTypeParameters
        end else None, None in
      let implements =
        if Peek.token env = T_IMPLEMENTS
        then begin
          if not (should_parse_types env)
          then error env Error.UnexpectedTypeInterface;
          Expect.token env T_IMPLEMENTS;
          class_implements env []
        end else [] in
      let body = class_body env in
      body, superClass, superTypeParameters, implements

    and class_implements env acc =
      let id = Parse.identifier env in
      let typeParameters = Type.type_parameter_instantiation env in
      let loc = match typeParameters with
      | None -> fst id
      | Some (loc, _) -> Loc.btwn (fst id) loc in
      let implement = loc, Ast.Class.Implements.({
        id;
        typeParameters;
      }) in
      let acc = implement::acc in
      match Peek.token env with
      | T_COMMA ->
          Expect.token env T_COMMA;
          class_implements env acc
      | _ -> List.rev acc

    and class_body =
      let rec elements env acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | T_SEMICOLON ->
            (* Skip empty elements *)
            Expect.token env T_SEMICOLON;
            elements env acc
        | _ -> elements env ((class_element env)::acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let body = elements env [] in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        Loc.btwn start_loc end_loc, Ast.Class.Body.({
          body;
        })

    (* In the ES6 draft, all elements are methods. No properties (though there
     * are getter and setters allowed *)
    and class_element =
      let get env start_loc decorators static =
        let key, (end_loc, _ as value) =
          _method env Ast.Expression.Object.Property.Get in
        Ast.Class.(Body.Method (Loc.btwn start_loc end_loc, Method.({
          key;
          value;
          kind = Get;
          static;
          decorators;
        })))

      in let set env start_loc decorators static =
        let key, (end_loc, _ as value) =
          _method env Ast.Expression.Object.Property.Set in
        Ast.Class.(Body.Method (Loc.btwn start_loc end_loc, Method.({
          key;
          value;
          kind = Set;
          static;
          decorators;
        })))

      in let init env start_loc decorators key async generator static =
        match Peek.token env with
        | T_COLON
        | T_ASSIGN
        | T_SEMICOLON when not async && not generator ->
          (* Class property with annotation *)
          let typeAnnotation = Type.annotation_opt env in
          let options = parse_options env in
          let value =
            if Peek.token env = T_ASSIGN then (
              if static && options.esproposal_class_static_fields
                 || (not static) && options.esproposal_class_instance_fields
              then begin
                Expect.token env T_ASSIGN;
                Some (Parse.expression env)
              end else None
            ) else None in
          let end_loc = Peek.loc env in
          if Expect.maybe env T_SEMICOLON then () else begin
            if Peek.token env == T_LBRACKET || Peek.token env == T_LPAREN then error_unexpected env
          end;
          let loc = Loc.btwn start_loc end_loc in
          Ast.Class.(Body.Property (loc, Property.({
            key;
            value;
            typeAnnotation;
            static;
          })))
        | _ ->
          let typeParameters = Type.type_parameter_declaration env in
          let params, defaults, rest = Declaration.function_params env in
          let returnType = Type.annotation_opt env in
          let _, body, strict =
            Declaration.function_body env ~async ~generator in
          let simple =
            Declaration.is_simple_function_params params defaults rest in
          Declaration.strict_post_check env ~strict ~simple None params;
          let end_loc, expression = Function.(
            match body with
            | BodyBlock (loc, _) -> loc, false
            | BodyExpression (loc, _) -> loc, true) in
          let value = end_loc, Function.({
            id = None;
            params;
            defaults;
            rest;
            body;
            generator;
            async;
            (* TODO: add support for method predicates *)
            predicate = None;
            expression;
            returnType;
            typeParameters;
          }) in
          let kind = Ast.(match key with
            | Expression.Object.Property.Identifier (_, {
                Identifier.name = "constructor";
                _;
              })
            | Expression.Object.Property.Literal (_, {
                Literal.value = Literal.String "constructor";
                _;
              }) ->
              Class.Method.Constructor
            | _ ->
              Class.Method.Method) in
          Ast.Class.(Body.Method (Loc.btwn start_loc end_loc, Method.({
            key;
            value;
            kind;
            static;
            decorators;
          })))

      in fun env -> Ast.Expression.Object.Property.(
        let start_loc = Peek.loc env in
        let decorators = decorator_list env in
        let static = Expect.maybe env T_STATIC in
        let async =
          Peek.token ~i:1 env <> T_LPAREN &&
          Peek.token ~i:1 env <> T_COLON &&
          Declaration.async env in
        let generator = Declaration.generator env async in
        match (async, generator, key env) with
        | false, false,
          (_, (Identifier (_, { Identifier.name = "get"; _ }) as key)) ->
            (match Peek.token env with
            | T_LESS_THAN
            | T_COLON
            | T_ASSIGN
            | T_SEMICOLON
            | T_LPAREN -> init env start_loc decorators key async generator static
            | _ -> get env start_loc decorators static )
        | false, false,
          (_, (Identifier (_, { Identifier.name = "set"; _ }) as key)) ->
            (match Peek.token env with
            | T_LESS_THAN
            | T_COLON
            | T_ASSIGN
            | T_SEMICOLON
            | T_LPAREN -> init env start_loc decorators key async generator static
            | _ -> set env start_loc decorators static)
        | _, _, (_, key) ->
            init env start_loc decorators key async generator static
      )

    let class_declaration env decorators =
      (* 10.2.1 says all parts of a class definition are strict *)
      let env = env |> with_strict true in
      let start_loc = Peek.loc env in
      let decorators = decorators @ (decorator_list env) in
      Expect.token env T_CLASS;
      let tmp_env = env |> with_no_let true in
      let id = (
        match in_export env, Peek.is_identifier tmp_env with
        | true, false -> None
        | _ -> Some(Parse.identifier tmp_env)
      ) in
      let typeParameters = Type.type_parameter_declaration_with_defaults env in
      let body, superClass, superTypeParameters, implements = _class env in
      let loc = Loc.btwn start_loc (fst body) in
      loc, Ast.Statement.(ClassDeclaration Class.({
        id;
        body;
        superClass;
        typeParameters;
        superTypeParameters;
        implements;
        classDecorators=decorators;
      }))

    let class_expression env =
      let start_loc = Peek.loc env in
      let decorators = decorator_list env in
      Expect.token env T_CLASS;
      let id, typeParameters = match Peek.token env with
        | T_EXTENDS
        | T_LESS_THAN
        | T_LCURLY -> None, None
        | _ ->
            let id = Some (Parse.identifier env) in
            let typeParameters = Type.type_parameter_declaration_with_defaults env in
            id, typeParameters in
      let body, superClass, superTypeParameters, implements = _class env in
      let loc = Loc.btwn start_loc (fst body) in
      loc, Ast.Expression.(Class Class.({
        id;
        body;
        superClass;
        typeParameters;
        superTypeParameters;
        implements;
        classDecorators=decorators;
      }))
  end

  module Statement: sig
    val _for: env -> Ast.Statement.t
    val _if: env -> Ast.Statement.t
    val _let: env -> Ast.Statement.t
    val _try: env -> Ast.Statement.t
    val _while: env -> Ast.Statement.t
    val _with: env -> Ast.Statement.t
    val block: env -> Ast.Statement.t
    val break: env -> Ast.Statement.t
    val continue: env -> Ast.Statement.t
    val debugger: env -> Ast.Statement.t
    val declare: ?in_module:bool -> env -> Ast.Statement.t
    val declare_export_declaration: ?allow_export_type:bool -> env -> Ast.Statement.t
    val do_while: env -> Ast.Statement.t
    val empty: env -> Ast.Statement.t
    val export_declaration: env -> Ast.Expression.t list -> Ast.Statement.t
    val expression: env -> Ast.Statement.t
    val import_declaration: env -> Ast.Statement.t
    val interface: env -> Ast.Statement.t
    val maybe_labeled: env -> Ast.Statement.t
    val return: env -> Ast.Statement.t
    val switch: env -> Ast.Statement.t
    val throw: env -> Ast.Statement.t
    val type_alias: env -> Ast.Statement.t
    val var_or_const: env -> Ast.Statement.t
  end = struct
    let rec empty env =
      let loc = Peek.loc env in
      Expect.token env T_SEMICOLON;
      loc, Statement.Empty

    and break env =
      let start_loc = Peek.loc env in
      Expect.token env T_BREAK;
      let label =
        if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
        then None
        else begin
          let label = Parse.identifier env in
          let name = (snd label).Identifier.name in
          if not (SSet.mem name (labels env))
          then error env (Error.UnknownLabel name);
          Some label
        end
      in
      let end_loc = match Peek.semicolon_loc env with
      | Some loc -> loc
      | None -> (match label with
        | Some id -> fst id
        | None -> start_loc) in
      let loc = Loc.btwn start_loc end_loc in
      if label = None && not (in_loop env || in_switch env)
      then error_at env (loc, Error.IllegalBreak);
      Eat.semicolon env;
      loc, Statement.Break {
        Statement.Break.label = label;
      }

    and continue env =
      let start_loc = Peek.loc env in
      Expect.token env T_CONTINUE;
      let label =
        if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
        then None
        else begin
          let (_, { Identifier.name; _ }) as label = Parse.identifier env in
          if not (SSet.mem name (labels env))
          then error env (Error.UnknownLabel name);
          Some label
        end in
      let end_loc = match Peek.semicolon_loc env with
      | Some loc -> loc
      | None -> (match label with
        | Some id -> fst id
        | None -> start_loc) in
      let loc = Loc.btwn start_loc end_loc in
      if not (in_loop env)
      then error_at env (loc, Error.IllegalContinue);
      Eat.semicolon env;
      loc, Statement.Continue {
        Statement.Continue.label = label;
      }

    and debugger env =
      let start_loc = Peek.loc env in
      Expect.token env T_DEBUGGER;
      let end_loc = match Peek.semicolon_loc env with
      | None -> start_loc
      | Some loc -> loc in
      Eat.semicolon env;
      Loc.btwn start_loc end_loc, Statement.Debugger;

    and do_while env =
      let start_loc = Peek.loc env in
      Expect.token env T_DO;
      let body = Parse.statement (env |> with_in_loop true) in
      Expect.token env T_WHILE;
      Expect.token env T_LPAREN;
      let test = Parse.expression env in
      let end_loc = Peek.loc env in
      Expect.token env T_RPAREN;
      let end_loc = match Peek.semicolon_loc env with
      | None -> end_loc
      | Some loc -> loc in
      (* The rules of automatic semicolon insertion in ES5 don't mention this,
       * but the semicolon after a do-while loop is optional. This is properly
       * specified in ES6 *)
      if Peek.token env = T_SEMICOLON
      then Eat.semicolon env;
      Loc.btwn start_loc end_loc, Statement.(DoWhile DoWhile.({
        body;
        test;
      }))

    and _for =
      let assert_can_be_forin_or_forof env err = Statement.VariableDeclaration.(function
        | Some (Statement.For.InitDeclaration (loc, {
          declarations;
          _;
        })) ->
            (* Only a single declarator is allowed, without an init. So
             * something like
             *
             * for (var x in y) {}
             *
             * is allowed, but we disallow
             *
             * for (var x, y in z) {}
             * for (var x = 42 in y) {}
             *)
            (match declarations with
            | [ (_, { Declarator.init = None; _; }) ] -> ()
            | _ -> error_at env (loc, err))
        | Some (Statement.For.InitExpression (loc, expr)) ->
            (* Only certain expressions can be the lhs of a for in or for of *)
            if not (Parse.is_assignable_lhs (loc, expr))
            then error_at env (loc, err)
        | _ -> error env err
      ) in

      fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_FOR;
        Expect.token env T_LPAREN;

        let init, errs =
          match Peek.token env with
          | T_SEMICOLON -> None, []
          | T_LET ->
              let decl, errs = Declaration._let (env |> with_no_in true) in
              Some (Statement.For.InitDeclaration decl), errs
          | T_CONST ->
              let decl, errs = Declaration.const (env |> with_no_in true) in
              Some (Statement.For.InitDeclaration decl), errs
          | T_VAR ->
              let decl, errs = Declaration.var (env |> with_no_in true) in
              Some (Statement.For.InitDeclaration decl), errs
          | _ ->
              let expr = Parse.expression (env |> with_no_in true |> with_no_let true) in
              Some (Statement.For.InitExpression expr), []
        in

        match Peek.token env with
        | T_IN ->
            assert_can_be_forin_or_forof env Error.InvalidLHSInForIn init;
            let left = Statement.(match init with
            | Some (For.InitDeclaration decl) -> ForIn.LeftDeclaration decl
            | Some (For.InitExpression expr) -> ForIn.LeftExpression expr
            | None -> assert false) in
            (* This is a for in loop *)
            Expect.token env T_IN;
            let right = Parse.expression env in
            Expect.token env T_RPAREN;
            let body = Parse.statement (env |> with_in_loop true) in
            Loc.btwn start_loc (fst body), Statement.(ForIn ForIn.({
              left;
              right;
              body;
              each = false;
            }))
        | T_OF ->
            assert_can_be_forin_or_forof env Error.InvalidLHSInForOf init;
            let left = Statement.(match init with
            | Some (For.InitDeclaration decl) -> ForOf.LeftDeclaration decl
            | Some (For.InitExpression expr) -> ForOf.LeftExpression expr
            | None -> assert false) in
            (* This is a for of loop *)
            Expect.token env T_OF;
            let right = Parse.assignment env in
            Expect.token env T_RPAREN;
            let body = Parse.statement (env |> with_in_loop true) in
            Loc.btwn start_loc (fst body), Statement.(ForOf ForOf.({
              left;
              right;
              body;
            }))
        | _ ->
            (* This is a for loop *)
            errs |> List.iter (error_at env);
            Expect.token env T_SEMICOLON;
            let test = match Peek.token env with
            | T_SEMICOLON -> None
            | _ -> Some (Parse.expression env) in
            Expect.token env T_SEMICOLON;
            let update = match Peek.token env with
            | T_RPAREN -> None
            | _ -> Some (Parse.expression env) in
            Expect.token env T_RPAREN;
            let body = Parse.statement (env |> with_in_loop true) in
            Loc.btwn start_loc (fst body), Statement.(For For.({
              init;
              test;
              update;
              body;
            }))

    and _if env =
      let start_loc = Peek.loc env in
      Expect.token env T_IF;
      Expect.token env T_LPAREN;
      let test = Parse.expression env in
      Expect.token env T_RPAREN;
      let consequent = match Peek.token env with
      | _ when Peek.is_function env ->
          strict_error env Error.StrictFunctionStatement;
          Declaration._function env
      | _ -> Parse.statement env in
      let alternate = if Peek.token env = T_ELSE
      then begin
        Expect.token env T_ELSE;
        Some (Parse.statement env)
      end else None in
      let end_loc = match alternate with
      | Some stmt -> fst stmt
      | None -> fst consequent in
      Loc.btwn start_loc end_loc, Statement.(If If.({
        test;
        consequent;
        alternate;
      }))

    and return env =
      if not (in_function env)
      then error env Error.IllegalReturn;
      let start_loc = Peek.loc env in
      Expect.token env T_RETURN;
      let argument =
        if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
        then None
        else Some (Parse.expression env) in
      let end_loc = match Peek.semicolon_loc env with
      | Some loc -> loc
      | None -> (match argument with
        | Some argument -> fst argument
        | None -> start_loc) in
      Eat.semicolon env;
      Loc.btwn start_loc end_loc, Statement.(Return Return.({
        argument;
      }))

    and switch =
      let rec case_list env (seen_default, acc) =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | _ ->
          let start_loc = Peek.loc env in
          let test = match Peek.token env with
          | T_DEFAULT ->
              if seen_default
              then error env Error.MultipleDefaultsInSwitch;
              Expect.token env T_DEFAULT; None
          | _ ->
              Expect.token env T_CASE;
              Some (Parse.expression env) in
          let seen_default = seen_default || test = None in
          let end_loc = Peek.loc env in
          Expect.token env T_COLON;
          let term_fn = function
          | T_RCURLY | T_DEFAULT | T_CASE -> true
          | _ -> false in
          let consequent =
            Parse.statement_list ~term_fn (env |> with_in_switch true) in
          let end_loc = match List.rev consequent with
          | last_stmt::_ -> fst last_stmt
          | _ -> end_loc in
          let acc = (Loc.btwn start_loc end_loc, Statement.Switch.Case.({
            test;
            consequent;
          }))::acc in
          case_list env (seen_default, acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_SWITCH;
        Expect.token env T_LPAREN;
        let discriminant = Parse.expression env in
        Expect.token env T_RPAREN;
        Expect.token env T_LCURLY;
        let cases = case_list env (false, []) in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        Loc.btwn start_loc end_loc, Statement.(Switch Switch.({
          discriminant;
          cases;
          lexical = false; (* TODO *)
        }))

    and throw env =
      let start_loc = Peek.loc env in
      Expect.token env T_THROW;
      if Peek.is_line_terminator env
      then error_at env (start_loc, Error.NewlineAfterThrow);
      let argument = Parse.expression env in
      let end_loc = match Peek.semicolon_loc env with
      | Some loc -> loc
      | None -> fst argument in
      Eat.semicolon env;
      Loc.btwn start_loc end_loc, Statement.(Throw Throw.({
        argument;
      }))

    and _try env =
      let start_loc = Peek.loc env in
      Expect.token env T_TRY;
      let block = Parse.block_body env in
      let handler = match Peek.token env with
      | T_CATCH ->
          let start_loc = Peek.loc env in
          Expect.token env T_CATCH;
          Expect.token env T_LPAREN;
          let id = Parse.identifier ~restricted_error:Error.StrictCatchVariable env in
          let param = fst id, Pattern.Identifier id in
          Expect.token env T_RPAREN;
          let body = Parse.block_body env in
          let loc = Loc.btwn start_loc (fst body) in
          Some (loc, Ast.Statement.Try.CatchClause.({
            param;
            (* This SpiderMonkey specific feature is not on track to be in a
            * standard so I'm not supporting it *)
            guard = None;
            body;
          }))
      | _ -> None in
      (* SpiderMonkey feature, not supported in ES6 *)
      let guardedHandlers = [] in
      let finalizer = match Peek.token env with
      | T_FINALLY ->
          Expect.token env T_FINALLY;
          Some (Parse.block_body env)
      | _ -> None in
      let end_loc = match finalizer with
      | Some finalizer -> fst finalizer
      | None ->
          (match handler with
          | Some handler -> fst handler
          | None ->
              (* No catch or finally? That's an error! *)
              error_at env (fst block, Error.NoCatchOrFinally);
              fst block) in
      Loc.btwn start_loc end_loc, Statement.(Try Try.({
        block;
        handler;
        guardedHandlers;
        finalizer;
      }));

    and var_or_const env =
      let (start_loc, declaration), errs = Declaration.variable env in
      let end_loc = match Peek.semicolon_loc env with
      | None -> start_loc
      | Some end_loc -> end_loc in
      Eat.semicolon env;
      errs |> List.iter (error_at env);
      Loc.btwn start_loc end_loc, declaration

    and _let env =
      let start_loc = Peek.loc env in
      Expect.token env T_LET;
      if Peek.token env = T_LPAREN
      then begin
        (* Let statement *)
        Expect.token env T_LPAREN;
        let end_loc, declarations, errs =
          Declaration.variable_declaration_list (env |> with_no_let true) in
        let head = List.map
          (fun (_, {Ast.Statement.VariableDeclaration.Declarator.id; init;}) ->
            Statement.Let.({ id; init; }))
          declarations in
        Expect.token env T_RPAREN;
        let body = Parse.statement env in
        let end_loc = match Peek.semicolon_loc env with
        | None -> end_loc
        | Some end_loc -> end_loc in
        Eat.semicolon env;
        errs |> List.iter (error_at env);
        Loc.btwn start_loc end_loc, Statement.(Let Let.({
          head;
          body;
        }))
      end else begin
        (* Let declaration *)
        let end_loc, declarations, errs =
          Declaration.variable_declaration_list (env |> with_no_let true) in
        let declaration =
          Ast.(Statement.VariableDeclaration Statement.VariableDeclaration.({
            declarations;
            kind = Let;
          })) in
        let end_loc = match Peek.semicolon_loc env with
        | None -> end_loc
        | Some end_loc -> end_loc in
        Eat.semicolon env;
        errs |> List.iter (error_at env);
        Loc.btwn start_loc end_loc, declaration
      end

    and _while env =
      let start_loc = Peek.loc env in
      Expect.token env T_WHILE;
      Expect.token env T_LPAREN;
      let test = Parse.expression env in
      Expect.token env T_RPAREN;
      let body = Parse.statement (env |> with_in_loop true) in
      Loc.btwn start_loc (fst body), Statement.(While While.({
        test;
        body;
      }));

    and _with env =
      let start_loc = Peek.loc env in
      Expect.token env T_WITH;
      Expect.token env T_LPAREN;
      let _object = Parse.expression env in
      Expect.token env T_RPAREN;
      let body = Parse.statement env in
      let loc = Loc.btwn start_loc (fst body) in
      strict_error_at env (loc, Error.StrictModeWith);
      loc, Statement.(With With.({
        _object;
        body;
      }))

    and block env =
      let loc, block = Parse.block_body env in
      loc, Statement.Block block

    and maybe_labeled env =
      let expr = Parse.expression env in
      match (expr, Peek.token env) with
      | ((loc, Ast.Expression.Identifier label), T_COLON) ->
          let { Identifier.name; _ } = snd label in
          Expect.token env T_COLON;
          if SSet.mem name (labels env)
          then error_at env (loc, Error.Redeclaration ("Label", name));
          let env = add_label env name in
          let labeled_stmt = Parse.statement env in
          Loc.btwn loc (fst labeled_stmt), Statement.Labeled {
            Statement.Labeled.label = label;
            Statement.Labeled.body = labeled_stmt;
          }
      | expression, _ ->
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None -> (fst expression) in
          Eat.semicolon env;
          Loc.btwn (fst expression) end_loc, Statement.(Expression Expression.({
            expression;
          }))

    and expression env =
      let expression = Parse.expression env in
      let end_loc = match Peek.semicolon_loc env with
      | Some loc -> loc
      | None -> fst expression in
      Eat.semicolon env;
      Loc.btwn (fst expression) end_loc, Statement.(Expression Expression.({
        expression;
      }))

    and type_alias_helper env =
      let start_loc = Peek.loc env in
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeAlias;
      Expect.token env T_TYPE;
      Eat.push_lex_mode env Lex_mode.TYPE;
      let id = Parse.identifier env in
      let typeParameters = Type.type_parameter_declaration_with_defaults env in
      Expect.token env T_ASSIGN;
      let right = Type._type env in
      let end_loc = match Peek.semicolon_loc env with
      | None -> fst right
      | Some end_loc -> end_loc in
      Eat.semicolon env;
      Eat.pop_lex_mode env;
      Loc.btwn start_loc end_loc, Statement.TypeAlias.({
        id;
        typeParameters;
        right;
      })

    and type_alias env =
      if Peek.is_identifier ~i:1 env
      then
        let loc, type_alias = type_alias_helper env in
        loc, Statement.TypeAlias type_alias
      else
        Parse.statement env

    and interface_helper =
      let rec supers env acc =
        let super = Type.generic env in
        let acc = super::acc in
        match Peek.token env with
        | T_COMMA ->
            Expect.token env T_COMMA;
            supers env acc
        | _ -> List.rev acc
      in
      fun env ->
        let start_loc = Peek.loc env in
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeInterface;
        Expect.token env T_INTERFACE;
        let id = Parse.identifier env in
        let typeParameters = Type.type_parameter_declaration_with_defaults env in
        let extends = if Peek.token env = T_EXTENDS
        then begin
          Expect.token env T_EXTENDS;
          supers env []
        end else [] in
        let body = Type._object ~allow_static:true env in
        let loc = Loc.btwn start_loc (fst body) in
        loc, Statement.Interface.({
          id;
          typeParameters;
          body;
          extends;
          mixins = [];
        })


    and interface env =
      if Peek.is_identifier ~i:1 env
      then
        let loc, iface = interface_helper env in
        loc, Statement.InterfaceDeclaration iface
      else expression env

    and declare_class =
      let rec supers env acc =
        let super = Type.generic env in
        let acc = super::acc in
        match Peek.token env with
        | T_COMMA ->
          Expect.token env T_COMMA;
          supers env acc
        | _ -> List.rev acc

      (* This is identical to `interface`, except that mixins are allowed *)
      in fun env start_loc ->
        let env = env |> with_strict true in
        Expect.token env T_CLASS;
        let id = Parse.identifier env in
        let typeParameters = Type.type_parameter_declaration_with_defaults env in
        let extends = if Peek.token env = T_EXTENDS
          then begin
            Expect.token env T_EXTENDS;
            supers env []
          end else [] in
        let mixins = if Peek.value env = "mixins"
          then begin
            Expect.contextual env "mixins";
            supers env []
          end else [] in
        let body = Type._object ~allow_static:true env in
        let loc = Loc.btwn start_loc (fst body) in
        loc, Statement.Interface.({
          id;
          typeParameters;
          body;
          extends;
          mixins;
        })

    and declare_class_statement env start_loc =
      let loc, fn = declare_class env start_loc in
      loc, Statement.DeclareClass fn

    and declare_function env start_loc =
      Expect.token env T_FUNCTION;
      let id = Parse.identifier env in
      let start_sig_loc = Peek.loc env in
      let typeParameters = Type.type_parameter_declaration env in
      let rest, params = Type.function_param_list env in
      Expect.token env T_COLON;
      let returnType = Type._type env in
      let end_loc = fst returnType in
      let loc = Loc.btwn start_sig_loc end_loc in
      let value = loc, Ast.Type.(Function {Function.
        params;
        returnType;
        rest;
        typeParameters;
      }) in
      let typeAnnotation = Some ((fst value), value) in
      let id =
        Loc.btwn (fst id) end_loc,
        Ast.Identifier.({(snd id) with typeAnnotation; })
      in
      let end_loc = match Peek.semicolon_loc env with
      | None -> end_loc
      | Some end_loc -> end_loc in
      let predicate = Parse.predicate env in
      Eat.semicolon env;
      let loc = Loc.btwn start_loc end_loc in
      loc, Statement.DeclareFunction.({ id; predicate })

    and declare_function_statement env start_loc =
      let loc, fn = declare_function env start_loc in
      loc, Statement.DeclareFunction fn

    and declare_var env start_loc =
      Expect.token env T_VAR;
      let id = Parse.identifier_with_type env Error.StrictVarName in
      let end_loc = match Peek.semicolon_loc env with
      | None -> fst id
      | Some loc -> loc in
      let loc = Loc.btwn start_loc end_loc in
      Eat.semicolon env;
      loc, Statement.DeclareVariable.({ id; })

    and declare_var_statement env start_loc =
      let loc, var = declare_var env start_loc in
      loc, Statement.DeclareVariable var

    and declare_module =
      let rec module_items env ~module_kind acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> (module_kind, List.rev acc)
        | _ ->
          let stmt = declare ~in_module:true env in
          let module_kind = Statement.(
            let (loc, stmt) = stmt in
            match (module_kind, stmt) with
            (**
             * The first time we see either a `declare export` or a
             * `declare module.exports`, we lock in the kind of the module.
             *
             * `declare export type` and `declare export interface` are the two
             * exceptions to this rule because they are valid in both CommonJS
             * and ES modules (and thus do not indicate an intent for either).
             *)
            | None, DeclareModuleExports _ -> Some (DeclareModule.CommonJS loc)
            | None, DeclareExportDeclaration {
                DeclareExportDeclaration.declaration;
                _;
              } ->
              (match declaration with
                | Some (DeclareExportDeclaration.NamedType _)
                | Some (DeclareExportDeclaration.Interface _)
                  -> module_kind
                | _ -> Some (DeclareModule.ES loc)
              )

            (**
             * There should never be more than one `declare module.exports`
             * statement *)
            | Some (DeclareModule.CommonJS _), DeclareModuleExports _ ->
              error env Parse_error.DuplicateDeclareModuleExports;
              module_kind

            (**
             * It's never ok to mix and match `declare export` and
             * `declare module.exports` in the same module because it leaves the
             * kind of the module (CommonJS vs ES) ambiguous.
             *
             * The 1 exception to this rule is that `export type/interface` are
             * both ok in CommonJS modules.
             *)
            | Some (DeclareModule.ES _), DeclareModuleExports _ ->
              error env Parse_error.AmbiguousDeclareModuleKind;
              module_kind
            | Some (DeclareModule.CommonJS _), DeclareExportDeclaration {
                DeclareExportDeclaration.declaration;
                _;
              } ->
                (match declaration with
                  | Some (DeclareExportDeclaration.NamedType _)
                  | Some (DeclareExportDeclaration.Interface _)
                    -> ()
                  | _ -> error env Parse_error.AmbiguousDeclareModuleKind
                );
                module_kind

            | _ -> module_kind
          ) in
          module_items env ~module_kind (stmt::acc)

      in fun env start_loc ->
        let id = match Peek.token env with
        | T_STRING (loc, value, raw, octal) ->
            if octal then strict_error env Error.StrictOctalLiteral;
            Expect.token env (T_STRING (loc, value, raw, octal));
            let value = Literal.String value in
            Statement.DeclareModule.Literal (loc, { Literal.value; raw; })
        | _ ->
            Statement.DeclareModule.Identifier (Parse.identifier env) in
        let body_start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let (module_kind, body) = module_items env ~module_kind:None [] in
        Expect.token env T_RCURLY;
        let body_end_loc = Peek.loc env in
        let body_loc = Loc.btwn body_start_loc body_end_loc in
        let body = body_loc, { Statement.Block.body; } in
        let loc = Loc.btwn start_loc (fst body) in
        let kind =
          match module_kind with
          | Some k -> k
          | None -> Statement.DeclareModule.CommonJS loc
        in
        loc,
        Statement.(DeclareModule DeclareModule.({ id; body; kind; }))

    and declare_module_exports env start_loc =
      Expect.token env T_PERIOD;
      Expect.contextual env "exports";
      let type_annot = Type.annotation env in
      let end_loc =
        match Peek.semicolon_loc env with
        | Some loc -> loc
        | None -> fst type_annot
      in
      Eat.semicolon env;
      let loc = Loc.btwn start_loc end_loc in
      (loc, Statement.DeclareModuleExports type_annot)

    and declare ?(in_module=false) env =
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeDeclaration;
      let start_loc = Peek.loc env in
      (* eventually, just emit a wrapper AST node *)
      (match Peek.token ~i:1 env with
        | T_CLASS ->
            Expect.token env T_DECLARE;
            declare_class_statement env start_loc
        | T_INTERFACE ->
            Expect.token env T_DECLARE;
            interface env
        | T_TYPE ->
            Expect.token env T_DECLARE;
            type_alias env;
        | T_FUNCTION ->
            Expect.token env T_DECLARE;
            declare_function_statement env start_loc
        | T_VAR ->
            Expect.token env T_DECLARE;
            declare_var_statement env start_loc
        | T_ASYNC ->
            Expect.token env T_DECLARE;
            error env Error.DeclareAsync;
            Expect.token env T_ASYNC;
            declare_function_statement env start_loc
        | T_EXPORT when in_module ->
            declare_export_declaration ~allow_export_type:in_module env
        | T_IDENTIFIER when Peek.value ~i:1 env = "module" ->
            Expect.token env T_DECLARE;
            Expect.contextual env "module";
            if in_module || Peek.token env = T_PERIOD
            then declare_module_exports env start_loc
            else declare_module env start_loc
        | _ when in_module ->
            (* Oh boy, found some bad stuff in a declare module. Let's just
              * pretend it's a declare var (arbitrary choice) *)
            Expect.token env T_DECLARE;
            declare_var_statement env start_loc
        | _ ->
            Parse.statement env
      )

    and export_source env =
      Expect.contextual env "from";
      match Peek.token env with
      | T_STRING (loc, value, raw, octal) ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (loc, value, raw, octal));
          let value = Literal.String value in
          loc, { Literal.value; raw; }
      | _ ->
          (* Just make up a string for the error case *)
          let raw = Peek.value env in
          let value = Literal.String raw in
          let ret = Peek.loc env, { Literal.value; raw; } in
          error_unexpected env;
          ret

    and extract_pattern_binding_names =
      let rec fold acc = Pattern.(function
        | (_, Object {Object.properties; _;}) ->
          List.fold_left (fun acc prop ->
            match prop with
            | Object.Property (_, {Object.Property.pattern; _;})
            | Object.SpreadProperty (_, {Object.SpreadProperty.argument = pattern;})
              -> fold acc pattern
          ) acc properties
        | (_, Array {Array.elements; _;}) ->
          List.fold_left (fun acc elem ->
            match elem with
            | Some (Array.Element pattern)
            | Some (Array.Spread (_, {Array.SpreadElement.argument = pattern;}))
              -> fold acc pattern
            | None -> acc
          ) acc elements
        | (_, Assignment {Assignment.left;_;}) -> fold acc left
        | (_, Identifier (loc, {Identifier.name; _;})) -> (loc, name)::acc
        | (_, Expression _) ->
          failwith "Parser error: No such thing as an expression pattern!"
      ) in
      List.fold_left fold

    and extract_ident_name (_, {Identifier.name; _;}) = name

    and export_specifiers_and_errs env specifiers errs =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
          List.rev specifiers, List.rev errs
      | _ ->
          let id, err = Parse.identifier_or_reserved_keyword env in
          let name, err, end_loc = if Peek.value env = "as"
          then begin
            Expect.contextual env "as";
            let name, _ = Parse.identifier_or_reserved_keyword env in
            (record_export env (fst name, extract_ident_name name));
            Some name, None, fst name
          end else begin
            let loc = fst id in
            record_export env (loc, extract_ident_name id);
            None, err, loc
          end in
          let loc = Loc.btwn (fst id) end_loc in
          let specifier = loc, {
            Statement.ExportDeclaration.Specifier.id;
            name;
          } in
          if Peek.token env = T_COMMA
          then Expect.token env T_COMMA;
          let errs = match err with
          | Some err -> err::errs
          | None -> errs in
          export_specifiers_and_errs env (specifier::specifiers) errs

    and export_declaration env decorators =
      let env = env |> with_strict true |> with_in_export true in
      let start_loc = Peek.loc env in
      Expect.token env T_EXPORT;
      Statement.ExportDeclaration.(match Peek.token env with
      | T_DEFAULT ->
          (* export default ... *)
          Expect.token env T_DEFAULT;
          record_export env (Loc.btwn start_loc (Peek.loc env), "default");
          let end_loc, declaration = match Peek.token env with
          | T_FUNCTION ->
              (* export default function foo (...) { ... } *)
              let fn = Declaration._function env in
              fst fn, Some (Declaration fn)
          | _ when Peek.is_class env ->
              (* export default class foo { ... } *)
              let _class = Object.class_declaration env decorators in
              fst _class, Some (Declaration _class)
          | _ ->
              (* export default [assignment expression]; *)
              let expr = Parse.assignment env in
              let end_loc = match Peek.semicolon_loc env with
              | Some loc -> loc
              | None -> fst expr in
              Eat.semicolon env;
              end_loc, Some (Expression expr)
            in
          Loc.btwn start_loc end_loc, Statement.ExportDeclaration {
            default = true;
            declaration;
            specifiers = None;
            source = None;
            exportKind = ExportValue;
          }
      | T_TYPE when (Peek.token env ~i:1) <> T_LCURLY ->
          (* export type ... *)
          if not (should_parse_types env)
          then error env Error.UnexpectedTypeExport;
          let type_alias = type_alias env in
          (match type_alias with
            | (loc, Statement.TypeAlias {Statement.TypeAlias.id; _;}) ->
              record_export env (loc, extract_ident_name id)
            | _ -> failwith (
                "Internal Flow Error! Parsed `export type` into something " ^
                "other than a type alias!"
              )
          );
          let end_loc = fst type_alias in
          Loc.btwn start_loc end_loc, Statement.ExportDeclaration {
            default = false;
            declaration = Some (Declaration type_alias);
            specifiers = None;
            source = None;
            exportKind = ExportType;
          }
      | T_INTERFACE ->
          (* export interface I { ... } *)
          if not (should_parse_types env)
          then error env Error.UnexpectedTypeExport;
          let interface = interface env in
          (match interface with
            | (loc, Statement.InterfaceDeclaration {Statement.Interface.id; _;}) ->
              record_export env (loc, extract_ident_name id)
            | _ -> failwith (
                "Internal Flow Error! Parsed `export interface` into something " ^
                "other than an interface declaration!"
              )
          );
          let end_loc = fst interface in
          Loc.btwn start_loc end_loc, Statement.ExportDeclaration {
            default = false;
            declaration = Some (Declaration interface);
            specifiers = None;
            source = None;
            exportKind = ExportType;
          }
      | T_LET
      | T_CONST
      | T_VAR
      (* not using Peek.is_class here because it would guard all of the
        * cases *)
      | T_AT
      | T_CLASS
      (* not using Peek.is_function here because it would guard all of the
        * cases *)
      | T_ASYNC
      | T_FUNCTION ->
          let stmt = Parse.statement_list_item env ~decorators:decorators in
          let names = Statement.(
            match stmt with
            | (_, VariableDeclaration { VariableDeclaration.declarations; _; }) ->
              List.fold_left (fun names (_, declaration) ->
                let id = declaration.VariableDeclaration.Declarator.id in
                extract_pattern_binding_names names [id]
              ) [] declarations
            | (loc, ClassDeclaration { Class.id = Some id; _; })
            | (loc, FunctionDeclaration { Function.id = Some id; _; })
              -> [(loc, extract_ident_name id)]
            | (loc, ClassDeclaration { Class.id = None; _; }) ->
              error_at env (loc, Error.ExportNamelessClass);
              []
            | (loc, FunctionDeclaration { Function.id = None; _; }) ->
              error_at env (loc, Error.ExportNamelessFunction);
              []
            | _ -> failwith "Internal Flow Error! Unexpected export statement declaration!"
          ) in
          List.iter (record_export env) names;
          let declaration = Some (Declaration stmt) in
          Loc.btwn start_loc (fst stmt), Statement.ExportDeclaration {
            default = false;
            declaration;
            specifiers = None;
            source = None;
            exportKind = ExportValue;
          }
      | T_MULT ->
          let loc = Peek.loc env in
          Expect.token env T_MULT;
          let local_name =
            let parse_export_star_as =
              (parse_options env).esproposal_export_star_as
            in
            if Peek.value env = "as"
            then (
              Expect.contextual env "as";
              if parse_export_star_as
              then Some (Parse.identifier env)
              else (error env Error.UnexpectedTypeDeclaration; None)
            ) else None
          in
          let specifiers =
            Some (ExportBatchSpecifier (loc, local_name))
          in
          let source = export_source env in
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None -> fst source in
          let source = Some source in
          Eat.semicolon env;
          Loc.btwn start_loc end_loc, Statement.ExportDeclaration {
            default = false;
            declaration = None;
            specifiers;
            source;
            exportKind = ExportValue;
          }
      | _ ->
          let exportKind = (
            match Peek.token env with
            | T_TYPE -> Eat.token env; ExportType
            | _ -> ExportValue
          ) in
          Expect.token env T_LCURLY;
          let specifiers, errs = export_specifiers_and_errs env [] [] in
          let specifiers = Some (ExportSpecifiers specifiers) in
          let end_loc = Peek.loc env in
          Expect.token env T_RCURLY;
          let source = if Peek.value env = "from"
          then Some (export_source env)
          else begin
            errs |> List.iter (error_at env);
            None
          end in
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None ->
              (match source with
              | Some source -> fst source
              | None -> end_loc) in
          Eat.semicolon env;
          Loc.btwn start_loc end_loc, Statement.ExportDeclaration {
            default = false;
            declaration = None;
            specifiers;
            source;
            exportKind;
          }
      )

    and declare_export_declaration ?(allow_export_type=false) env =
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeDeclaration;
      let start_loc = Peek.loc env in
      Expect.token env T_DECLARE;

      let env = env |> with_strict true |> with_in_export true in
      Expect.token env T_EXPORT;
      Statement.DeclareExportDeclaration.(match Peek.token env with
      | T_DEFAULT ->
          (* declare export default ... *)
          Expect.token env T_DEFAULT;
          let end_loc, declaration = match Peek.token env with
          | T_FUNCTION ->
              (* declare export default function foo (...): ...  *)
              let fn = declare_function env start_loc in
              fst fn, Some (Function fn)
          | T_CLASS ->
              (* declare export default class foo { ... } *)
              let _class = declare_class env start_loc in
              fst _class, Some (Class _class)
          | _ ->
              (* declare export default [type]; *)
              let _type = Type._type env in
              let end_loc = match Peek.semicolon_loc env with
              | Some loc -> loc
              | None -> fst _type in
              Eat.semicolon env;
              end_loc, Some (DefaultType _type)
            in
          Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
            default = true;
            declaration;
            specifiers = None;
            source = None;
          }
      | T_LET
      | T_CONST
      | T_VAR
      | T_CLASS
      | T_FUNCTION ->
          let end_loc, declaration = match Peek.token env with
          | T_FUNCTION ->
              (* declare export function foo (...): ...  *)
              let fn = declare_function env start_loc in
              fst fn, Some (Function fn)
          | T_CLASS ->
              (* declare export class foo { ... } *)
              let _class = declare_class env start_loc in
              fst _class, Some (Class _class)
          | T_LET
          | T_CONST
          | T_VAR as token ->
              (match token with
              | T_LET -> error env Error.DeclareExportLet
              | T_CONST -> error env Error.DeclareExportConst
              | _ -> ());
              (* declare export var foo: ... *)
              let var = declare_var env start_loc in
              fst var, Some (Variable var)
          | _ -> assert false in
          Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
            default = false;
            declaration;
            specifiers = None;
            source = None;
          }
      | T_MULT ->
          (* declare export * from 'foo' *)
          let loc = Peek.loc env in
          Expect.token env T_MULT;
          let parse_export_star_as =
            (parse_options env).esproposal_export_star_as
          in
          let local_name =
            if Peek.value env = "as"
            then (
              Expect.contextual env "as";
              if parse_export_star_as
              then Some (Parse.identifier env)
              else (error env Error.UnexpectedTypeDeclaration; None)
            ) else None
          in
          let specifiers = Statement.ExportDeclaration.(
            Some (ExportBatchSpecifier (loc, local_name))
          ) in
          let source = export_source env in
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None -> fst source in
          let source = Some source in
          Eat.semicolon env;
          Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
            default = false;
            declaration = None;
            specifiers;
            source;
          }
      | T_TYPE when allow_export_type ->
          (* declare export type = ... *)
          let (alias_loc, alias) = type_alias_helper env in
          let loc = Loc.btwn start_loc alias_loc in
          (loc, Statement.DeclareExportDeclaration {
            default = false;
            declaration = Some (NamedType (alias_loc, alias));
            specifiers = None;
            source = None;
          })
      | T_INTERFACE when allow_export_type ->
          (* declare export interface ... *)
          let (iface_loc, iface) = interface_helper env in
          let loc = Loc.btwn start_loc iface_loc in
          (loc, Statement.DeclareExportDeclaration {
            default = false;
            declaration = Some (Interface (iface_loc, iface));
            specifiers = None;
            source = None;
          })
      | _ ->
          (match Peek.token env with
            | T_TYPE -> error env Error.DeclareExportType
            | T_INTERFACE -> error env Error.DeclareExportInterface
            | _ -> ()
          );
          Expect.token env T_LCURLY;
          let specifiers, errs = export_specifiers_and_errs env [] [] in
          let specifiers = Some (Statement.ExportDeclaration.ExportSpecifiers specifiers) in
          let end_loc = Peek.loc env in
          Expect.token env T_RCURLY;
          let source = if Peek.value env = "from"
          then Some (export_source env)
          else begin
            errs |> List.iter (error_at env);
            None
          end in
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None ->
              (match source with
              | Some source -> fst source
              | None -> end_loc) in
          Eat.semicolon env;
          Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
            default = false;
            declaration = None;
            specifiers;
            source;
          }
      )

    and import_declaration =
      let open Statement.ImportDeclaration in

      let source env =
        Expect.contextual env "from";
        match Peek.token env with
        | T_STRING (loc, value, raw, octal) ->
            if octal then strict_error env Error.StrictOctalLiteral;
            Expect.token env (T_STRING (loc, value, raw, octal));
            let value = Literal.String value in
            loc, { Literal.value; raw; }
        | _ ->
            (* Just make up a string for the error case *)
            let raw = Peek.value env in
            let value = Literal.String raw in
            let ret = Peek.loc env, { Literal.value; raw; } in
            error_unexpected env;
            ret

      in let rec specifier_list env acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | _ ->
            let remote, err = Parse.identifier_or_reserved_keyword env in
            let specifier =
              if Peek.value env = "as" then begin
                Expect.contextual env "as";
                let local = Some (Parse.identifier env) in
                ImportNamedSpecifier { local; remote; }
              end else begin
                (match err with Some err -> error_at env err | None -> ());
                ImportNamedSpecifier { local = None; remote; }
              end
            in
            if Peek.token env = T_COMMA
            then Expect.token env T_COMMA;
            specifier_list env (specifier::acc)

      in let named_or_namespace_specifier env =
        let start_loc = Peek.loc env in
        match Peek.token env with
        | T_MULT ->
            Expect.token env T_MULT;
            Expect.contextual env "as";
            let id = Parse.identifier env in
            [ImportNamespaceSpecifier (Loc.btwn start_loc (fst id), id)]
        | _ ->
            Expect.token env T_LCURLY;
            let specifiers = specifier_list env [] in
            Expect.token env T_RCURLY;
            specifiers

      in fun env ->
        let env = env |> with_strict true in
        let start_loc = Peek.loc env in
        Expect.token env T_IMPORT;
        (* It might turn out that we need to treat this "type" token as an
         * identifier, like import type from "module" *)
        let importKind, type_ident =
          match Peek.token env with
          | T_TYPE ->
            if not (should_parse_types env)
            then error env Error.UnexpectedTypeImport;
            ImportType, Some(Parse.identifier env)
          | T_TYPEOF ->
            if not (should_parse_types env)
            then error env Error.UnexpectedTypeImport;
            Expect.token env T_TYPEOF;
            ImportTypeof, None
          | _ -> ImportValue, None
        in
        match Peek.token env, Peek.is_identifier env with
        (* import "ModuleName"; *)
        | T_STRING (str_loc, value, raw, octal), _
            when importKind = ImportValue ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (str_loc, value, raw, octal));
          let value = Literal.String value in
          let source = (str_loc, { Literal.value; raw; }) in
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None -> str_loc in
          Eat.semicolon env;
          Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
            importKind;
            source;
            specifiers = [];
          }

        (* import [type] SomeDefault ... *)
        | T_COMMA, _ (* `import type, ...` *)
        | _, true -> (* `import type Foo` or `import type from` *)
            let importKind, default_specifier = (
              match type_ident, Peek.token env, Peek.value env with
              | Some type_ident, T_COMMA, _ (* `import type,` *)
              | Some type_ident, T_IDENTIFIER, "from" -> (* `import type from` *)
                ImportValue, ImportDefaultSpecifier type_ident
              | _ -> (* Either `import type Foo` or `import Foo` *)
                importKind, ImportDefaultSpecifier (Parse.identifier env)
            ) in

            let additional_specifiers = (
              match Peek.token env with
              | T_COMMA -> (* `import Foo, ...` *)
                  Expect.token env T_COMMA;
                  named_or_namespace_specifier env
              | _ -> []
            ) in

            let source = source env in
            let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None -> fst source in
            let source = source in
            Eat.semicolon env;
            Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
              importKind;
              source;
              specifiers = default_specifier::additional_specifiers;
            }

        (* `import [type] { ... } ...` or `import [typeof] * as ...` *)
        | _ ->
            let specifiers = named_or_namespace_specifier env in
            let source = source env in
            let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None -> fst source in
            let source = source in
            Eat.semicolon env;
            Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
              importKind;
              source;
              specifiers;
            }
  end

  module Pattern = struct
    (* Reinterpret various expressions as patterns.
     * This is not the correct thing to do and is only used for assignment
     * expressions. This should be removed and replaced ASAP.
     *)
    let object_from_expr =
      let property env prop =
        Ast.Expression.Object.(match prop with
        | Property (loc, { Property.key; value; shorthand; _ }) ->
          let key = Property.(match key with
          | Literal lit -> Pattern.Object.Property.Literal lit
          | Identifier id -> Pattern.Object.Property.Identifier id
          | Computed expr -> Pattern.Object.Property.Computed expr) in
          let pattern = Parse.pattern_from_expr env value in
          Pattern.(Object.Property (loc, Object.Property.({
            key;
            pattern;
            shorthand;
          })))
        | SpreadProperty (loc, { SpreadProperty.argument; }) ->
            let argument = Parse.pattern_from_expr env argument in
            Pattern.(Object.SpreadProperty (loc, Object.SpreadProperty.({
              argument;
            }))))

      in fun env (loc, obj) ->
        let properties =
          List.map (property env) obj.Ast.Expression.Object.properties in
        loc, Pattern.(Object Object.({
          properties;
          typeAnnotation = None;
        }))

    let array_from_expr =
      let element env = Ast.Expression.(function
        | None -> None
        | Some (Spread (loc, spread)) ->
            let argument = Parse.pattern_from_expr env (spread.SpreadElement.argument) in
            Some Pattern.(Array.Spread (loc, { Array.SpreadElement.argument; }))
        | Some (Expression (loc, expr)) ->
            Some Pattern.Array.(Element (Parse.pattern_from_expr env (loc, expr)))
      )

      in fun env (loc, arr) ->
        let elements =
          List.map (element env) arr.Ast.Expression.Array.elements in
        loc, Pattern.(Array Array.({
          elements;
          typeAnnotation = None;
        }))

    let from_expr env (loc, expr) =
      Ast.Expression.(match expr with
      | Object obj -> object_from_expr env (loc, obj)
      | Array arr ->  array_from_expr env (loc, arr)
      | Identifier id -> loc, Pattern.Identifier id
      | Assignment { Assignment.operator = Assignment.Assign; left; right } ->
          loc, Pattern.Assignment { Pattern.Assignment.left; right }
      | expr -> loc, Pattern.Expression (loc, expr))

    (* Parse object destructuring pattern *)
    let rec _object restricted_error =
      let rec property env =
        let start_loc = Peek.loc env in
        if Expect.maybe env T_ELLIPSIS
        then begin
          let argument = pattern env restricted_error in
          let loc = Loc.btwn start_loc (fst argument) in
          Some Pattern.Object.(SpreadProperty (loc, SpreadProperty.({
            argument
          })))
        end else begin
          let key = Ast.Expression.Object.Property.(
            match Parse.object_key env with
            | _, Literal lit -> Pattern.Object.Property.Literal lit
            | _, Identifier id -> Pattern.Object.Property.Identifier id
            | _, Computed expr -> Pattern.Object.Property.Computed expr
          ) in
          let prop = match Peek.token env with
            | T_COLON ->
              Expect.token env T_COLON;
              Some (pattern env restricted_error, false)
            | _ ->
              (match key with
              | Pattern.Object.Property.Identifier id ->
                let pattern = (fst id, Pattern.Identifier id) in
                Some (pattern, true)
              | _ ->
                error_unexpected env; (* invalid shorthand destructuring *)
                None)
          in
          match prop with
          | Some (pattern, shorthand) ->
            let pattern = match Peek.token env with
              | T_ASSIGN ->
                Expect.token env T_ASSIGN;
                let default = Parse.assignment env in
                let loc = Loc.btwn (fst pattern) (fst default) in
                loc, Pattern.(Assignment Assignment.({
                  left = pattern;
                  right = default;
                }));
              | _ -> pattern
            in
            let loc = Loc.btwn start_loc (fst pattern) in
            Some Pattern.Object.(Property (loc, Property.({
              key;
              pattern;
              shorthand;
            })))
          | None -> None
        end

      and properties env acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | _ ->
          (match property env with
          | Some prop ->
            if Peek.token env <> T_RCURLY
            then Expect.token env T_COMMA;
            properties env (prop::acc)
          | None -> properties env acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let properties = properties env [] in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        let end_loc, typeAnnotation =
          if Peek.token env = T_COLON then
            let typeAnnotation = Type.annotation env in
            fst typeAnnotation, Some typeAnnotation
          else end_loc, None
        in
        Loc.btwn start_loc end_loc, Pattern.(Object Object.({
          properties;
          typeAnnotation;
        }))

    (* Parse array destructuring pattern *)
    and _array restricted_error =
      let rec elements env acc =
        match Peek.token env with
        | T_EOF
        | T_RBRACKET -> List.rev acc
        | T_COMMA ->
          Expect.token env T_COMMA;
          elements env (None::acc)
        | T_ELLIPSIS ->
          let start_loc = Peek.loc env in
          Expect.token env T_ELLIPSIS;
          let argument = pattern env restricted_error in
          let loc = Loc.btwn start_loc (fst argument) in
          let element = Pattern.Array.(Spread (loc, SpreadElement.({
            argument;
          }))) in
          elements env ((Some element)::acc)
        | _ ->
          let pattern = pattern env restricted_error in
          let pattern = match Peek.token env with
            | T_ASSIGN ->
              Expect.token env T_ASSIGN;
              let default = Parse.expression env in
              let loc = Loc.btwn (fst pattern) (fst default) in
              loc, Pattern.(Assignment Assignment.({
                left = pattern;
                right = default;
              }))
            | _ -> pattern
          in
          let element = Pattern.Array.(Element pattern) in
          if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
          elements env ((Some element)::acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LBRACKET;
        let elements = elements env [] in
        let end_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        let end_loc, typeAnnotation =
          if Peek.token env = T_COLON then
            let typeAnnotation = Type.annotation env in
            fst typeAnnotation, Some typeAnnotation
          else end_loc, None
        in
        Loc.btwn start_loc end_loc, Pattern.(Array Array.({
          elements;
          typeAnnotation;
        }))

    and pattern env restricted_error =
      match Peek.token env with
      | T_LCURLY ->
          _object restricted_error env
      | T_LBRACKET ->
          _array restricted_error env
      | _ ->
          let id = Parse.identifier_with_type env restricted_error in
          fst id, Pattern.Identifier id
  end

  module JSX = struct
    let spread_attribute env =
      Eat.push_lex_mode env Lex_mode.NORMAL;
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      Expect.token env T_ELLIPSIS;
      let argument = Expression.assignment env in
      let end_loc = Peek.loc env in
      Expect.token env T_RCURLY;
      Eat.pop_lex_mode env;
      Loc.btwn start_loc end_loc, JSX.SpreadAttribute.({
        argument;
      })

    let expression_container env =
      Eat.push_lex_mode env Lex_mode.NORMAL;
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      let expression = if Peek.token env = T_RCURLY
        then
          let empty_loc = Loc.btwn_exclusive start_loc (Peek.loc env) in
          JSX.ExpressionContainer.EmptyExpression empty_loc
        else JSX.ExpressionContainer.Expression (Parse.expression env) in
      let end_loc = Peek.loc env in
      Expect.token env T_RCURLY;
      Eat.pop_lex_mode env;
      Loc.btwn start_loc end_loc, JSX.ExpressionContainer.({
        expression;
      })

    let identifier env =
      let loc = Peek.loc env in
      let name = Peek.value env in
      Expect.token env T_JSX_IDENTIFIER;
      loc, JSX.Identifier.({ name; })

    let name =
      let rec member_expression env member =
        match Peek.token env with
        | T_PERIOD ->
            let _object = JSX.MemberExpression.MemberExpression member in
            Expect.token env T_PERIOD;
            let property = identifier env in
            let loc = Loc.btwn (fst member) (fst property) in
            let member = loc, JSX.MemberExpression.({
              _object;
              property;
            }) in
            member_expression env member
        | _ -> member

      in fun env ->
        let name = identifier env in
        match Peek.token env with
        | T_COLON ->
            let namespace = name in
            Expect.token env T_COLON;
            let name = identifier env in
            let loc = Loc.btwn (fst namespace) (fst name) in
            JSX.NamespacedName (loc, JSX.NamespacedName.({
              namespace;
              name;
            }))
        | T_PERIOD ->
            let _object = JSX.MemberExpression.Identifier name in
            Expect.token env T_PERIOD;
            let property = identifier env in
            let loc = Loc.btwn (fst name) (fst property) in
            let member = loc, JSX.MemberExpression.({
              _object;
              property;
            }) in
            JSX.MemberExpression (member_expression env member)
        | _ -> JSX.Identifier name


    let attribute env =
      let start_loc = Peek.loc env in
      let name = identifier env in
      let end_loc, name =
        if Peek.token env = T_COLON
        then begin
          Expect.token env T_COLON;
          let namespace = name in
          let name = identifier env in
          let loc = Loc.btwn (fst namespace) (fst name) in
          loc, JSX.Attribute.NamespacedName (loc, JSX.NamespacedName.({
            namespace;
            name;
          }))
        end else fst name, JSX.Attribute.Identifier name in
      let end_loc, value =
        if Peek.token env = T_ASSIGN
        then begin
          Expect.token env T_ASSIGN;
          match Peek.token env with
          | T_LCURLY ->
              let loc, expression_container = expression_container env in
              begin
                let open JSX.ExpressionContainer in
                match expression_container.expression with
                | EmptyExpression _ ->
                    error env Error.JSXAttributeValueEmptyExpression;
                | _ -> ()
              end;
              loc, Some (JSX.Attribute.ExpressionContainer (loc, expression_container))
          | T_JSX_TEXT (loc, value, raw) as token ->
              Expect.token env token;
              let value = Ast.Literal.String value in
              loc, Some (JSX.Attribute.Literal (loc, { Ast.Literal.value; raw;}))
          | _ ->
              error env Error.InvalidJSXAttributeValue;
              let loc = Peek.loc env in
              let raw = "" in
              let value = Ast.Literal.String "" in
              loc, Some (JSX.Attribute.Literal (loc, { Ast.Literal.value; raw;}))
        end else end_loc, None in
      Loc.btwn start_loc end_loc, JSX.Attribute.({
        name;
        value;
      })

      let opening_element_without_lt =
        let rec attributes env acc =
          match Peek.token env with
          | T_EOF
          | T_DIV
          | T_GREATER_THAN -> List.rev acc
          | T_LCURLY ->
              let attribute = JSX.Opening.SpreadAttribute (spread_attribute env) in
              attributes env (attribute::acc)
          | _ ->
              let attribute = JSX.Opening.Attribute (attribute env) in
              attributes env (attribute::acc)

        in fun env start_loc ->
          let name = name env in
          let attributes = attributes env [] in
          let selfClosing = Peek.token env = T_DIV in
          if selfClosing then Expect.token env T_DIV;
          let end_loc = Peek.loc env in
          Expect.token env T_GREATER_THAN;
          Eat.pop_lex_mode env;
          Loc.btwn start_loc end_loc, JSX.Opening.({
            name;
            selfClosing;
            attributes;
          })

      let closing_element_without_lt env start_loc =
        Expect.token env T_DIV;
        let name = name env in
        let end_loc = Peek.loc env in
        Expect.token env T_GREATER_THAN;
        (* We double pop to avoid going back to childmode and re-lexing the
         * lookahead *)
        Eat.double_pop_lex_mode env;
        Loc.btwn start_loc end_loc, JSX.Closing.({
          name;
        })

      type element_or_closing =
        | Closing of JSX.Closing.t
        | ChildElement of (Loc.t * JSX.element)


      let rec child env =
        match Peek.token env with
        | T_LCURLY ->
            let expression_container = expression_container env in
            fst expression_container, JSX.ExpressionContainer (snd expression_container)
        | T_JSX_TEXT (loc, value, raw) as token ->
            Expect.token env token;
            loc, JSX.Text { JSX.Text.value; raw; }
        | _ ->
            let element = element env in
            fst element, JSX.Element (snd element)

      and element_without_lt =
        let element_or_closing env =
          Eat.push_lex_mode env Lex_mode.JSX_TAG;
          let start_loc = Peek.loc env in
          Expect.token env T_LESS_THAN;
          match Peek.token env with
          | T_EOF
          | T_DIV -> Closing (closing_element_without_lt env start_loc)
          | _ -> ChildElement (element_without_lt env start_loc)

        in let rec children_and_closing env acc =
          match Peek.token env with
          | T_LESS_THAN -> (
              match element_or_closing env with
              | Closing closingElement ->
                  List.rev acc, Some closingElement
              | ChildElement element ->
                  let element = fst element, JSX.Element (snd element) in
                  children_and_closing env (element::acc))
          | T_EOF ->
              error_unexpected env;
              List.rev acc, None
          | _ ->
              children_and_closing env ((child env)::acc)

        in let rec normalize name = JSX.(match name with
          | Identifier (_, { Identifier.name }) -> name
          | NamespacedName (_, { NamespacedName.namespace; name; }) ->
              (snd namespace).Identifier.name ^ ":" ^ (snd name).Identifier.name
          | MemberExpression (_, { MemberExpression._object; property; }) ->
              let _object = match _object with
              | MemberExpression.Identifier id -> (snd id).Identifier.name
              | MemberExpression.MemberExpression e -> normalize (JSX.MemberExpression e) in
              _object ^ "." ^ (snd property).Identifier.name
        )

        in fun env start_loc ->
          let openingElement = opening_element_without_lt env start_loc in
          let children, closingElement =
            if (snd openingElement).JSX.Opening.selfClosing
            then [], None
            else begin
              Eat.push_lex_mode env Lex_mode.JSX_CHILD;
              let ret = children_and_closing env [] in
              ret
            end in
          let end_loc = match closingElement with
          | Some (loc, { JSX.Closing.name }) ->
              let opening_name = normalize (snd openingElement).JSX.Opening.name in
              if normalize name <> opening_name
              then error env (Error.ExpectedJSXClosingTag opening_name);
              loc
          | _ -> fst openingElement in
          Loc.btwn (fst openingElement) end_loc, JSX.({
            openingElement;
            closingElement;
            children;
          })

      and element env =
        let start_loc = Peek.loc env in
        Eat.push_lex_mode env Lex_mode.JSX_TAG;
        Expect.token env T_LESS_THAN;
        element_without_lt env start_loc
  end

  let rec program env =
    let stmts = module_body_with_directives env (fun _ -> false) in
    let end_loc = Peek.loc env in
    Expect.token env T_EOF;
    let loc = match stmts with
    | [] -> end_loc
    | _ -> Loc.btwn (fst (List.hd stmts)) (fst (List.hd (List.rev stmts))) in
    let comments = List.rev (comments env) in
    loc, stmts, comments

  and directives =
      let check env (loc, token) =
        match token with
        | T_STRING (_, _, _, octal) ->
            if octal then strict_error_at env (loc, Error.StrictOctalLiteral)
        | _ -> failwith ("Nooo: "^(token_to_string token)^"\n")

      in let rec statement_list env term_fn item_fn (string_tokens, stmts) =
        match Peek.token env with
        | T_EOF -> env, string_tokens, stmts
        | t when term_fn t -> env, string_tokens, stmts
        | _ ->
            let string_token = Peek.loc env, Peek.token env in
            let possible_directive = item_fn env in
            let stmts = possible_directive::stmts in
            (match possible_directive with
            | _, Ast.Statement.Expression {
                Ast.Statement.Expression.expression = loc, Ast.Expression.Literal {
                  Ast.Literal.value = Ast.Literal.String str;
                  _;
                }
              } ->
                (* 14.1.1 says that it has to be "use strict" without any
                  * escapes, so "use\x20strict" is disallowed. We could in theory
                  * keep the raw string around, but that's a pain. This is a hack
                  * that actually seems to work pretty well (make sure the string
                  * has the right length)
                  *)
                let len = Loc.(loc._end.column - loc.start.column) in
                let strict =
                  (in_strict_mode env) ||
                  (str = "use strict" && len = 12)
                in
                let string_tokens = string_token::string_tokens in
                statement_list
                  (env |> with_strict strict)
                  term_fn
                  item_fn
                  (string_tokens, stmts)
            | _ ->
                env, string_tokens, stmts)

      in fun env term_fn item_fn ->
        let env, string_tokens, stmts = statement_list env term_fn item_fn ([], []) in
        List.iter (check env) (List.rev string_tokens);
        env, stmts

  (* 15.2 *)
  and module_item env =
    let decorators = Object.decorator_list env in
    match Peek.token env with
    | T_EXPORT -> Statement.export_declaration env decorators
    | T_IMPORT ->
        error_on_decorators env decorators;
        Statement.import_declaration env
    | T_DECLARE when Peek.token ~i:1 env = T_EXPORT ->
        error_on_decorators env decorators;
        Statement.declare_export_declaration env
    | _ -> statement_list_item env ~decorators

  and module_body_with_directives env term_fn =
    let env, directives = directives env term_fn module_item in
    let stmts = module_body ~term_fn env in
    (* Prepend the directives *)
    List.fold_left (fun acc stmt -> stmt::acc) stmts directives

  and module_body =
    let rec module_item_list env term_fn acc =
      match Peek.token env with
      | T_EOF -> List.rev acc
      | t when term_fn t -> List.rev acc
      | _ -> module_item_list env term_fn (module_item env::acc)

    in fun ~term_fn env ->
      module_item_list env term_fn []

  and statement_list_with_directives ~term_fn env =
    let env, directives = directives env term_fn statement_list_item in
    let stmts = statement_list ~term_fn env in
    (* Prepend the directives *)
    let stmts = List.fold_left (fun acc stmt -> stmt::acc) stmts directives in
    stmts, (in_strict_mode env)

  and statement_list =
    let rec statements env term_fn acc =
      match Peek.token env with
      | T_EOF -> List.rev acc
      | t when term_fn t -> List.rev acc
      | _ -> statements env term_fn ((statement_list_item env)::acc)

    in fun ~term_fn env -> statements env term_fn []


  and statement_list_item ?(decorators=[]) env =
    if not (Peek.is_class env)
    then error_on_decorators env decorators;
    Statement.(match Peek.token env with
    (* Remember kids, these look like statements but they're not
      * statements... (see section 13) *)
    | T_LET -> _let env
    | T_CONST -> var_or_const env
    | _ when Peek.is_function env -> Declaration._function env
    | _ when Peek.is_class env -> class_declaration env decorators
    | T_INTERFACE -> interface env
    | T_DECLARE -> declare env
    | T_TYPE -> type_alias env
    | _ -> statement env)

  and statement env =
    Statement.(match Peek.token env with
    | T_EOF ->
        error_unexpected env;
        Peek.loc env, Ast.Statement.Empty
    | T_SEMICOLON -> empty env
    | T_LCURLY -> block env
    | T_VAR -> var_or_const env
    | T_BREAK -> break env
    | T_CONTINUE -> continue env
    | T_DEBUGGER -> debugger env
    | T_DO -> do_while env
    | T_FOR -> _for env
    | T_IF -> _if env
    | T_RETURN -> return env
    | T_SWITCH -> switch env
    | T_THROW -> throw env
    | T_TRY -> _try env
    | T_WHILE -> _while env
    | T_WITH -> _with env
    | _ when Peek.is_identifier env -> maybe_labeled env
    (* If we see an else then it's definitely an error, but we can probably
     * assume that this is a malformed if statement that is missing the if *)
    | T_ELSE -> _if env
    (* There are a bunch of tokens that aren't the start of any valid
     * statement. We list them here in order to skip over them, rather than
     * getting stuck *)
    | T_COLON
    | T_RPAREN
    | T_RCURLY
    | T_RBRACKET
    | T_COMMA
    | T_PERIOD
    | T_ARROW
    | T_IN
    | T_INSTANCEOF
    | T_CATCH
    | T_FINALLY
    | T_CASE
    | T_DEFAULT
    | T_EXTENDS
    | T_STATIC
    | T_IMPORT (* TODO *)
    | T_EXPORT (* TODO *)
    | T_ELLIPSIS ->
        error_unexpected env;
        Eat.token env;
        statement env
    | _ -> expression env)

  and expression env =
    let expr = Expression.assignment env in
    match Peek.token env with
    | T_COMMA -> Expression.sequence env [expr]
    | _ ->
        expr

  and assignment = Expression.assignment
  and object_initializer = Object._initializer
  and object_key = Object.key
  and class_declaration = Object.class_declaration
  and class_expression = Object.class_expression
  and array_initializer = Expression.array_initializer

  and is_assignable_lhs = Expression.is_assignable_lhs

  and identifier ?restricted_error env =
    let loc = Peek.loc env in
    let name = Peek.value env in
    (match Peek.token env with
    | T_LET ->
    (* So "let" is disallowed as an identifier in a few situations. 11.6.2.1
     * lists them out. It is always disallowed in strict mode *)
      if in_strict_mode env
      then strict_error env Error.StrictReservedWord
      else
        if no_let env
        then error env (Error.UnexpectedToken name);
      Eat.token env
    | _ when is_strict_reserved name ->
      strict_error env Error.StrictReservedWord;
      Eat.token env
    | T_DECLARE
    | T_OF
    | T_ASYNC
    | T_AWAIT
    | T_TYPE as t ->
        (* These aren't real identifiers *)
        Expect.token env t
    | _ -> Expect.token env T_IDENTIFIER);
    (match restricted_error with
    | Some err when is_restricted name -> strict_error_at env (loc, err)
    | _ -> ());
    loc, Identifier.({
      name;
      typeAnnotation = None;
      optional = false;
    })

  and identifier_or_reserved_keyword = Expression.identifier_or_reserved_keyword

  and identifier_with_type env restricted_error =
    let loc, id = identifier ~restricted_error env in
    let loc, id =
      if Peek.token env = T_PLING
      then begin
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeAnnotation;
        let loc = Loc.btwn loc (Peek.loc env) in
        Expect.token env T_PLING;
        loc, { id with Identifier.optional = true; }
      end else (loc, id) in
    if Peek.token env = T_COLON
    then begin
      let typeAnnotation = Type.annotation env in
      let loc = Loc.btwn loc (fst typeAnnotation) in
      let typeAnnotation = Some typeAnnotation in
      Identifier.(loc, { id with typeAnnotation; })
    end else loc, id

  and block_body env =
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
    let term_fn = fun t -> t = T_RCURLY in
    let body = statement_list ~term_fn env in
    let end_loc = Peek.loc env in
    Expect.token env T_RCURLY;
    Loc.btwn start_loc end_loc, { Ast.Statement.Block.body; }

  and function_block_body env =
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
    let term_fn = fun t -> t = T_RCURLY in
    let body, strict = statement_list_with_directives ~term_fn env in
    let end_loc = Peek.loc env in
    Expect.token env T_RCURLY;
    Loc.btwn start_loc end_loc, { Ast.Statement.Block.body; }, strict

  and jsx_element = JSX.element

  and pattern = Pattern.pattern
  and pattern_from_expr = Pattern.from_expr

  and predicate env =
    let checks_loc = Peek.loc env in
    if Peek.token env = T_IDENTIFIER && Peek.value env = "checks" then (
      Expect.token env T_IDENTIFIER;
      if Expect.maybe env T_LPAREN then (
        let exp = Parse.expression env in
        let rparen_loc = Peek.loc env in
        Expect.token env T_RPAREN;
        let loc = Loc.btwn checks_loc rparen_loc in
        Some (loc, Predicate.Declared exp)
      )
      else
        Some (checks_loc, Predicate.Inferred)
    )
    else
      None

end

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let do_parse env parser fail =
  let ast = parser env in
  let error_list = filter_duplicate_errors (errors env) in
  if fail && error_list <> []
  then raise (Error.Error error_list);
  ast, error_list

let parse_program fail ?(token_sink=None) ?(parse_options=None) filename content =
  let env = init_env ~token_sink ~parse_options filename content in
  do_parse env Parse.program fail

let program ?(fail=true) ?(token_sink=None) ?(parse_options=None) content =
  parse_program fail ~token_sink ~parse_options None content

let program_file ?(fail=true) ?(token_sink=None) ?(parse_options=None) content filename =
  parse_program fail ~token_sink ~parse_options filename content

(* even if fail=false, still raises an error on a totally invalid token, since
   there's no legitimate fallback. *)
let json_file ?(fail=true) ?(token_sink=None) ?(parse_options=None) content filename =
  let env = init_env ~token_sink ~parse_options filename content in
  match Peek.token env with
  | T_LBRACKET
  | T_LCURLY
  | T_STRING _
  | T_NUMBER _
  | T_TRUE
  | T_FALSE
  | T_NULL ->
    do_parse env Parse.expression fail
  | T_MINUS ->
    (match Peek.token ~i:1 env with
    | T_NUMBER _ ->
      do_parse env Parse.expression fail
    | _ ->
      error_unexpected env;
      raise (Error.Error (errors env)))
  | _ ->
    error_unexpected env;
    raise (Error.Error (errors env))

end
module Flow_parser_js
= struct
#1 "flow_parser_js.ml"
(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

external createRegex : string -> string -> 'a = "RegExp" [@@bs.new]

module JsTranslator : sig
  val translation_errors: (Loc.t * Parse_error.t) list ref
  include Estree_translator.Translator
end = struct
  type t

  let translation_errors = ref []
  let string = [%bs.raw "function (x) {return x;}"]
  let bool = [%bs.raw "function (x) {x ? 1 : 0;}"]
  let obj = [%bs.raw "function(arr) {var ret = {}; arr.forEach(function(a) {ret[a[0]]=a[1];}); return ret}"]
  let array = [%bs.raw "function (x) {return x;}"]
  let number = [%bs.raw "function (x) {return x;}"]
  let null = [%bs.raw "null"]
  let regexp loc pattern flags =
    let regexp = try
      createRegex pattern flags
    with _ ->
      translation_errors := (loc, Parse_error.InvalidRegExp)::!translation_errors;
      (* Invalid RegExp. We already validated the flags, but we've been
       * too lazy to write a JS regexp parser in Ocaml, so we didn't know
       * the pattern was invalid. We'll recover with an empty pattern.
       *)
      createRegex "" flags
    in
    regexp
end

external throw : 'a -> 'b = "throw" [@@bs.val]

(* let parse_options jsopts = Parser_env.(
  let opts = default_parse_options in

  let decorators = Js.Unsafe.get jsopts "esproposal_decorators" in
  let opts = if Js.Optdef.test decorators
    then { opts with esproposal_decorators = Js.to_bool decorators; }
    else opts in

  let class_instance_fields = Js.Unsafe.get jsopts "esproposal_class_instance_fields" in
  let opts = if Js.Optdef.test class_instance_fields
    then { opts with esproposal_class_instance_fields = Js.to_bool class_instance_fields; }
    else opts in

  let class_static_fields = Js.Unsafe.get jsopts "esproposal_class_static_fields" in
  let opts = if Js.Optdef.test class_static_fields
    then { opts with esproposal_class_static_fields = Js.to_bool class_static_fields; }
    else opts in

  let export_star_as = Js.Unsafe.get jsopts "esproposal_export_star_as" in
  let opts = if Js.Optdef.test export_star_as
    then { opts with esproposal_export_star_as = Js.to_bool export_star_as; }
    else opts in

  let types = Js.Unsafe.get jsopts "types" in
  let opts = if Js.Optdef.test types
    then { opts with types = Js.to_bool types; }
    else opts in

  opts
) *)

external setRetErrors : 'a -> string -> 'b -> unit = "" [@@bs.set_index]
external setEName : 'a -> string -> 'b -> unit = "" [@@bs.set_index]
external newError : 'a -> 'b = "Error" [@@bs.new]

let parse content options =
  (* let parse_options = Some (parse_options options) in *)
  let parse_options = None in
  try
    let (ocaml_ast, errors) = Parser_flow.program ~fail:false ~parse_options content in
    JsTranslator.translation_errors := [];
    let module Translate = Estree_translator.Translate (JsTranslator)  in
    let ret = Translate.program ocaml_ast in
    let translation_errors = !JsTranslator.translation_errors in
    setRetErrors ret "errors" (Translate.errors (errors @ translation_errors));
    ret
  with Parse_error.Error l ->
    let e = newError ((string_of_int (List.length l)) ^ " errors") in
    setEName e "name" "Parse Error";
    ignore (throw e);
    [%bs.raw "{}"]

end
module RunParser
= struct
#1 "runParser.ml"
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

let v =
  match [%node __dirname] with
  | Some f ->
    let f =  Node.Path.join [|f; "flow_parser_sample.js"|] in
    let v : < range : int * int; ..> Js.t =
      (Obj.magic (Flow_parser_js.parse (Node.Fs2.readFileSync f `utf8 ) None )) in
    eq __LOC__ (0,2842) (v## range)
  | None -> assert false

let () = Mt.from_pair_suites __MODULE__ !suites

end
