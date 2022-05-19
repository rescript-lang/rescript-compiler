// Boilerplate currently handwritten
external async0: Js.Fn.arity0<'r> => Js.Fn.arity0<Js.Promise.t<'r>> = "?async"
external async1: Js.Fn.arity1<'a1 => 'r> => Js.Fn.arity1<'a1 => Js.Promise.t<'r>> = "?async"
external async2: Js.Fn.arity2<('a1, 'a2) => 'r> => Js.Fn.arity2<('a1, 'a2) => Js.Promise.t<'r>> =
  "?async"

external await: Js.Promise.t<'a> => 'a = "?await"

type testable = (. unit) => Js.Promise.t<unit>

let tests: array<testable> = []

let addTest = t => tests->Js.Array2.push(t)->ignore
let addTest1 = (t, x) => tests->Js.Array2.push((. ()) => t(. x))->ignore

//
//
// Basic tests

let foo = async2((. x, y) => x + y)

let bar = async1((. ff) => {
  let a = await(ff(. 3, 4))
  let b = await(foo(. 5, 6))
  a + b
})

let baz = async0((. ()) => await(bar(. foo)))

let testBaz: testable = async0((. ()) => {
  let n = await(baz(.))
  Js.log2("baz returned", n)
})

testBaz->addTest

//
//
// Catching exceptions

exception E(int)

let e1: testable = async0((. ()) => raise(E(1000)))
let e2: testable = async0((. ()) => Js.Exn.raiseError("Some JS error"))
let e3: testable = async0((. ()) => await(e1(.)))
let e4: testable = async0((. ()) => await(e2(.)))
let e5: testable = %raw(`function() { return Promise.reject(new Error('fail')) }`)

let testTryCatch = async1((. fn) =>
  try await(fn(.)) catch {
  | E(n) => Js.log2("testTryCatch: E", n)
  | JsError(_) => Js.log("testTryCatch: JsError")
  }
)

testTryCatch->addTest1(e1)
testTryCatch->addTest1(e2)
testTryCatch->addTest1(e3)
testTryCatch->addTest1(e4)
testTryCatch->addTest1(e5)

//
//
// Check for nested promise

let singlePromise = async1((. x) => x + 1)

let nestedPromise = async1((. x) => {
  let resolve = x => [Js.Promise.resolve(x)]
  let _result = singlePromise(. x + 1)->resolve
  32
})

//
//
// Test error handling in fetch

let explainError: unknown => string = %raw(`(e)=>e.toString()`)

let testFetch = async1((. url) => {
  switch await(Fetch.fetch(url)) {
  | response =>
    let status = response->Fetch.Response.status
    Js.log2("Fetch returned status:", status)
  | exception JsError(e) => Js.log2("Fetch returned an error:", e->explainError)
  }
})

testFetch->addTest1("https://www.google.com/sdkjdkghdsg")
testFetch->addTest1("https://www.google.comsdkjdkghdsg")

//
//
// Run tests

let runAllTests = async0((. ()) => {
  for i in 0 to Array.length(tests) - 1 {
    await(tests[i](.))
  }
  // Note: this is no good, as await is inside a closure
  //  tests->Js.Array2.forEach(test => await(test(.)))
})

runAllTests(.)->ignore
