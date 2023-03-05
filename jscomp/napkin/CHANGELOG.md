## Master

> **Tags:**
>
> - :boom: [Breaking Change]
> - :eyeglasses: [Spec Compliance]
> - :rocket: [New Feature]
> - :bug: [Bug Fix]
> - :memo: [Documentation]
> - :house: [Internal]
> - :nail_care: [Polish]

#### :boom: Breaking Change

- Emit an error when a `@string` or `@int` attribute is used in a V4 component https://github.com/rescript-lang/rescript-compiler/issues/5724
- Parse the attributes of labelled argument to the pattern attributes of argument instead of function. https://github.com/rescript-lang/syntax/pull/722

#### :rocket: New Feature

- Add surface syntax for `async`/`await` https://github.com/rescript-lang/syntax/pull/600

- Initial support for JSX V4, still work in progress.

  - :boom: when V4 is activated, at most one component is allowed for each module.

- Add support for empty record literal `{}` for records with only optional fields, and type definition of empty record (e.g. `type empty = {}`) https://github.com/rescript-lang/syntax/pull/632

- Support the use of spread anywhere in list creation (e.g. `list{...x, 1, ...y, ...z}). https://github.com/rescript-lang/syntax/pull/692

- Add support for the argument of `@react.component` to set a props type from the outside. https://github.com/rescript-lang/syntax/pull/699

#### :bug: Bug Fix

- Fix issue in formatting JSX spread props https://github.com/rescript-lang/syntax/pull/644
- Fix pretty printer where it would print doc comments on the same line as other attributes https://github.com/rescript-lang/syntax/pull/642
- Fix location issue in error messages with JSX V4 where the body of the component is an application https://github.com/rescript-lang/syntax/pull/633
- Fix issue where the printer would omit attributes for `->` and `|>` https://github.com/rescript-lang/syntax/pull/629
- Fix printing of optional fields in records https://github.com/rescript-lang/rescript-compiler/issues/5654
- Fix printing of comments inside empty blocks https://github.com/rescript-lang/syntax/pull/647
- Fix location issue in error messages with JSX V4 where the multiple props types are defined https://github.com/rescript-lang/syntax/pull/655
- Fix location issue in make function in JSX V4 that breaks dead code elimination https://github.com/rescript-lang/syntax/pull/660
- Fix parsing (hence pretty printing) of expressions with underscore `_` and comments.
- Fix printing of comments inside JSX tag https://github.com/rescript-lang/syntax/pull/664
- Fix issue where formatter erases tail comments inside JSX tag https://github.com/rescript-lang/syntax/issues/663
- Fix issue where the JSX prop has type annotation of the first class module https://github.com/rescript-lang/syntax/pull/666
- Fix issue where a spread `...x` in non-last position would not be reported as syntax error https://github.com/rescript-lang/syntax/pull/673/
- Fix issue where the formatter would delete `async` in a function with labelled arguments.
- Fix several printing issues with `async` including an infinite loop https://github.com/rescript-lang/syntax/pull/680
- Fix issue where certain JSX expressions would be formatted differenctly in compiler 10.1.0-rc.1 https://github.com/rescript-lang/syntax/issues/675
- Fix issue where printing nested pipe discards await https://github.com/rescript-lang/syntax/issues/687
- Fix issue where the JSX key type is not an optional string https://github.com/rescript-lang/syntax/pull/693
- Fix issue where the JSX fragment without children build error https://github.com/rescript-lang/syntax/pull/704
- Fix issue where async as an id cannot be used with application and labelled arguments https://github.com/rescript-lang/syntax/issues/707
- Treat await as almost-unary operator weaker than pipe so `await foo->bar` means `await (foo->bar)` https://github.com/rescript-lang/syntax/pull/711
- Fix build error where aliasing arguments to `_` in the make function with JSX V4. https://github.com/rescript-lang/syntax/pull/720
- Fix parsing of spread props as an expression in JSX V4 https://github.com/rescript-lang/syntax/pull/721
- Fix dropping attributes from props in make function in JSX V4 https://github.com/rescript-lang/syntax/pull/723
- Fix an issue where error messages related to duplicate props were displayed without a loc and were unclear https://github.com/rescript-lang/syntax/pull/728
- Fix issue where error messages related to non-existent props were displayed without location information https://github.com/rescript-lang/syntax/pull/730
- Fix issue where uncurried functions were incorrectly converting the type of a prop given as a default value to curried https://github.com/rescript-lang/syntax/pull/731
- Fix issue with printing async functions with locally abstract types https://github.com/rescript-lang/syntax/pull/732
- Fix support for recursive components in JSX V4 https://github.com/rescript-lang/syntax/pull/733
- Fix issue with overlapping labelled argument with default value https://github.com/rescript-lang/syntax/pull/734
- Fix issue with using alias and default value together https://github.com/rescript-lang/syntax/pull/734
- Fix formatting of `switch` expressions that contain braced `cases` inside https://github.com/rescript-lang/syntax/pull/735
- Fix formatting of props spread for multiline JSX expression https://github.com/rescript-lang/syntax/pull/736
- Fix issue with JSX V4 and newtype https://github.com/rescript-lang/syntax/pull/737
- Fix issue with JSX V4 when components are nested https://github.com/rescript-lang/syntax/pull/738

#### :eyeglasses: Spec Compliance

- Functions with consecutive dots now print as multiple arrow functions like in JavaScript.

#### :nail_care Polish

- Change the internal representation of props for the lowercase components to record. https://github.com/rescript-lang/syntax/pull/665
- Change the payload of Pconst_char for type safety. https://github.com/rescript-lang/rescript-compiler/pull/5759
- Specialize the printing of the rhs of a record field assignment for optional values `{x: ? e}` https://github.com/rescript-lang/syntax/issues/714

## ReScript 10.0

- Fix printing for inline nullary functor types [#477](https://github.com/rescript-lang/syntax/pull/477)
- Fix stripping of quotes for empty poly variants [#474](https://github.com/rescript-lang/syntax/pull/474)
- Implement syntax for arity zero vs arity one in uncurried application in [#139](https://github.com/rescript-lang/syntax/pull/139)
- Fix parsing of first class module exprs as part of binary/ternary expr in [#256](https://github.com/rescript-lang/syntax/pull/256)
- Fix formatter hanging on deeply nested function calls [#261](https://github.com/rescript-lang/syntax/issues/261)
- Remove parsing of "import" and "export" which was never officially supported.

## ReScript 9.0.0

- Fix parsing of poly-var typexpr consisting of one tag-spec-first in [#254](https://github.com/rescript-lang/syntax/pull/254)
- Implement new syntax for guards on pattern match cases in [#248](https://github.com/rescript-lang/syntax/pull/248)
- Implement intelligent breaking for poly-var type expressions in [#246](https://github.com/rescript-lang/syntax/pull/246)
- Improve indentation of fast pipe chain in let binding in [#244](https://github.com/rescript-lang/syntax/pull/244)
- Improve printing of non-callback arguments in call expressions with callback in [#241](https://github.com/rescript-lang/syntax/pull/241/files)
- Fix printing of constrained expressions in rhs of js objects [#240](https://github.com/rescript-lang/syntax/pull/240)
- Improve printing of trailing comments under lhs of "pipe" expression in [#329](https://github.com/rescript-lang/syntax/pull/239/files)
- Improve printing of jsx children and props with leading line comment in [#236](https://github.com/rescript-lang/syntax/pull/236)
- Improve conversion of quoted strings from Reason in [#238](https://github.com/rescript-lang/syntax/pull/238)
- Print attributes/extension without bs prefix where possible in [#230](https://github.com/rescript-lang/syntax/pull/230)
- Cleanup gentype attribute printing [fe05e1051aa94b16f6993ddc5ba9651f89e86907](https://github.com/rescript-lang/syntax/commit/fe05e1051aa94b16f6993ddc5ba9651f89e86907)
- Implement light weight syntax for poly-variants [f84c5760b3f743f65e934195c87fc06bf88bff75](https://github.com/rescript-lang/syntax/commit/f84c5760b3f743f65e934195c87fc06bf88bff75)
- Fix bug in fast pipe conversion from Reason. [3d5f2daba5418b821c577ba03e2de1afb0dd66de](https://github.com/rescript-lang/syntax/commit/3d5f2daba5418b821c577ba03e2de1afb0dd66de)
- Improve parsed AST when tilde is missing in arrow expr parameters. [e52a0c89ac39b578a2062ef15fae2be625962e1f](https://github.com/rescript-lang/syntax/commit/e52a0c89ac39b578a2062ef15fae2be625962e1f)
- Improve parser diagnostics for missing tilde in labeled parameters. [a0d7689d5d2bfc31dc251e966ac33a3001200171](https://github.com/rescript-lang/syntax/commit/a0d7689d5d2bfc31dc251e966ac33a3001200171)
- Improve printing of uncurried application with a huggable expression in [c8767215186982e171fe9f9101d518150a65f0d7](https://github.com/rescript-lang/syntax/commit/c8767215186982e171fe9f9101d518150a65f0d7)
- Improve printing of uncurried arrow typexpr outcome printer in [4d953b668cf47358deccb8b730566f24de25b9ee](https://github.com/rescript-lang/syntax/commit/4d953b668cf47358deccb8b730566f24de25b9ee)
- Remove support for nativeint syntax in [72d9b7034fc28f317672c94994b322bee520acca](https://github.com/rescript-lang/syntax/commit/72d9b7034fc28f317672c94994b322bee520acca)
- Improve printing of poly variant typexprs with attributes in [bf6561b](https://github.com/rescript-lang/syntax/commit/bf6561bb5d84557b8b6cbbcd40078c39526af4af)

## ReScript 8.4.2 (December 11, 2020)

Released in https://github.com/rescript-lang/syntax/releases/tag/v8.4.2 as part of https://github.com/rescript-lang/rescript-compiler/releases/tag/8.4.2
