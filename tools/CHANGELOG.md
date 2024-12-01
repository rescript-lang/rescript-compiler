# Changelog

> **Tags:**
>
> - :boom: [Breaking Change]
> - :eyeglasses: [Spec Compliance]
> - :rocket: [New Feature]
> - :bug: [Bug Fix]
> - :memo: [Documentation]
> - :house: [Internal]
> - :nail_care: [Polish]

## master

## 0.6.4

#### :rocket: New Feature

- Add `moduletypeid` field for explicitly annotated module type. https://github.com/rescript-lang/rescript-vscode/pull/1019

### :bug: Bug Fix

- Print module structure with signature to module path. https://github.com/rescript-lang/rescript-vscode/pull/1018

## 0.6.3

#### :bug: Bug Fix

- Make sure Linux binaries are statically linked.

#### :nail_care: Polish

- Reverse order of extracted embeds, so they're in the correct order.

## 0.6.2

#### :rocket: New Feature

- Ship Linux ARM64 binaries.

## 0.6.1

#### :rocket: New Feature

- Expose `getBinaryPath` JS function that you can import to get the binary to call for the current platform.

## 0.6.0

#### :rocket: New Feature

- _internal_ Add experimental command for extracting (string) contents from extension points.

## 0.5.0

#### :rocket: New Feature

- Add `source` property to type, value, module and module alias. https://github.com/rescript-lang/rescript-vscode/pull/900.

#### :bug: Bug Fix

- Print docstrings for nested submodules. https://github.com/rescript-lang/rescript-vscode/pull/897
- Print `deprecated` field for module. https://github.com/rescript-lang/rescript-vscode/pull/897

## 0.4.0

#### :bug: Bug Fix

- Support inline record fields in constructors. https://github.com/rescript-lang/rescript-vscode/pull/889
- Fix docstrings for module alias. Get internal docstrings of module file. https://github.com/rescript-lang/rescript-vscode/pull/878
- Fix extracted docs of types include escaped linebreaks in signature. https://github.com/rescript-lang/rescript-vscode/pull/891

## 0.3.0

#### :rocket: New Feature

- Expose more `decode` functions. https://github.com/rescript-lang/rescript-vscode/pull/866

#### :house: [Internal]

- Add env var `FROM_COMPILER` to extract docstrings from compiler repo. https://github.com/rescript-lang/rescript-vscode/pull/868

#### :bug: Bug Fix

- Fix tagged variant for `Module` and add attr to interface files. https://github.com/rescript-lang/rescript-vscode/pull/866
- Fix `rescript-tools --version` command. https://github.com/rescript-lang/rescript-vscode/pull/873
- Fix output truncate when run `rescript-tools doc path/to/file.res` in a separate process. https://github.com/rescript-lang/rescript-vscode/pull/868
