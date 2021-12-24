module CssForTest = {
  include Css_Legacy_Core;
  include Css_Legacy_Core.Make({
    exception NotImplemented;

    let mergeStyles = (. _) => raise(NotImplemented);
    let make = (. _) => raise(NotImplemented);
    let injectRule = (. _) => raise(NotImplemented);
    let injectRaw = (. _) => raise(NotImplemented);
    let makeKeyFrames = (. _) => raise(NotImplemented);
  });
};

open Jest;
open Expect;
open CssForTest;

let toBeJson = x => Expect.toBe(x->Js.Json.stringifyAny);
let r = x => toJson([x]); /* simple rule for more readable tests */
let ruleSelector = display(block);
let ruleJson = {"display": "block"};

describe("Pseudo classes", () => {
  test("test selectors that have no parameters", () =>
    expect(
      (
        r(active([ruleSelector])),
        r(checked([ruleSelector])),
        r(default([ruleSelector])),
        r(defined([ruleSelector])),
        r(disabled([ruleSelector])),
        r(empty([ruleSelector])),
        r(enabled([ruleSelector])),
        r(first([ruleSelector])),
        r(firstChild([ruleSelector])),
        r(firstOfType([ruleSelector])),
        r(focus([ruleSelector])),
        r(focusWithin([ruleSelector])),
        r(hover([ruleSelector])),
        r(indeterminate([ruleSelector])),
        r(inRange([ruleSelector])),
        r(invalid([ruleSelector])),
        r(lastChild([ruleSelector])),
        r(lastOfType([ruleSelector])),
        r(link([ruleSelector])),
        r(onlyChild([ruleSelector])),
        r(onlyOfType([ruleSelector])),
        r(optional([ruleSelector])),
        r(outOfRange([ruleSelector])),
        r(readOnly([ruleSelector])),
        r(readWrite([ruleSelector])),
        r(required([ruleSelector])),
        r(root([ruleSelector])),
        r(scope([ruleSelector])),
        r(target([ruleSelector])),
        r(valid([ruleSelector])),
        r(visited([ruleSelector])),
      )
      ->Js.Json.stringifyAny,
    )
    |> toBeJson((
         {":active": ruleJson},
         {":checked": ruleJson},
         {":default": ruleJson},
         {":defined": ruleJson},
         {":disabled": ruleJson},
         {":empty": ruleJson},
         {":enabled": ruleJson},
         {":first": ruleJson},
         {":first-child": ruleJson},
         {":first-of-type": ruleJson},
         {":focus": ruleJson},
         {":focus-within": ruleJson},
         {":hover": ruleJson},
         {":indeterminate": ruleJson},
         {":in-range": ruleJson},
         {":invalid": ruleJson},
         {":last-child": ruleJson},
         {":last-of-type": ruleJson},
         {":link": ruleJson},
         {":only-child": ruleJson},
         {":only-of-type": ruleJson},
         {":optional": ruleJson},
         {":out-of-range": ruleJson},
         {":read-only": ruleJson},
         {":read-write": ruleJson},
         {":required": ruleJson},
         {":root": ruleJson},
         {":scope": ruleJson},
         {":target": ruleJson},
         {":valid": ruleJson},
         {":visited": ruleJson},
       ))
  );

  test("test host", () =>
    expect(
      (
        r(host([ruleSelector])),
        r(host(~selector=".special-custom-element", [ruleSelector])),
      )
      ->Js.Json.stringifyAny,
    )
    |> toBeJson((
         {":host": ruleJson},
         {":host(.special-custom-element)": ruleJson},
       ))
  );

  test("test not", () =>
    expect(r(not_("p", [ruleSelector]))->Js.Json.stringifyAny)
    |> toBeJson({":not(p)": ruleJson})
  );

  test("test nth-child", () =>
    expect(
      (
        r(nthChild(`odd, [ruleSelector])),
        r(nthChild(`even, [ruleSelector])),
        r(nthChild(`n(2), [ruleSelector])),
        r(nthChild(`add((3, 4)), [ruleSelector])),
      )
      ->Js.Json.stringifyAny,
    )
    |> toBeJson((
         {":nth-child(odd)": ruleJson},
         {":nth-child(even)": ruleJson},
         {":nth-child(2n)": ruleJson},
         {":nth-child(3n+4)": ruleJson},
       ))
  );

  test("test nth-last-child", () =>
    expect(
      (
        r(nthLastChild(`odd, [ruleSelector])),
        r(nthLastChild(`even, [ruleSelector])),
        r(nthLastChild(`n(2), [ruleSelector])),
        r(nthLastChild(`add((3, 4)), [ruleSelector])),
      )
      ->Js.Json.stringifyAny,
    )
    |> toBeJson((
         {":nth-last-child(odd)": ruleJson},
         {":nth-last-child(even)": ruleJson},
         {":nth-last-child(2n)": ruleJson},
         {":nth-last-child(3n+4)": ruleJson},
       ))
  );

  test("test nth-last-of-type", () =>
    expect(
      (
        r(nthLastOfType(`odd, [ruleSelector])),
        r(nthLastOfType(`even, [ruleSelector])),
        r(nthLastOfType(`n(2), [ruleSelector])),
        r(nthLastOfType(`add((3, 4)), [ruleSelector])),
      )
      ->Js.Json.stringifyAny,
    )
    |> toBeJson((
         {":nth-last-of-type(odd)": ruleJson},
         {":nth-last-of-type(even)": ruleJson},
         {":nth-last-of-type(2n)": ruleJson},
         {":nth-last-of-type(3n+4)": ruleJson},
       ))
  );

  test("test nth-of-type", () =>
    expect(
      (
        r(nthOfType(`odd, [ruleSelector])),
        r(nthOfType(`even, [ruleSelector])),
        r(nthOfType(`n(2), [ruleSelector])),
        r(nthOfType(`add((3, 4)), [ruleSelector])),
      )
      ->Js.Json.stringifyAny,
    )
    |> toBeJson((
         {":nth-of-type(odd)": ruleJson},
         {":nth-of-type(even)": ruleJson},
         {":nth-of-type(2n)": ruleJson},
         {":nth-of-type(3n+4)": ruleJson},
       ))
  );
});

describe("Pseudo classes", () =>
  test("test selectors that have no parameters", () =>
    expect(
      (
        r(after([ruleSelector])),
        r(before([ruleSelector])),
        r(firstLetter([ruleSelector])),
        r(firstLine([ruleSelector])),
        r(placeholder([ruleSelector])),
        r(selection([ruleSelector])),
      )
      ->Js.Json.stringifyAny,
    )
    |> toBeJson((
         {"::after": ruleJson},
         {"::before": ruleJson},
         {"::first-letter": ruleJson},
         {"::first-line": ruleJson},
         {"::placeholder": ruleJson},
         {"::selection": ruleJson},
       ))
  )
);

describe("Combinators", () =>
  test("test selectors", () =>
    expect(
      (
        r(child("li", [ruleSelector])),
        r(children([ruleSelector])),
        r(siblings([ruleSelector])),
        r(directSibling([ruleSelector])),
      )
      ->Js.Json.stringifyAny,
    )
    |> toBeJson((
         {" > li": ruleJson},
         {" > *": ruleJson},
         {" ~ ": ruleJson},
         {" + ": ruleJson},
       ))
  )
);