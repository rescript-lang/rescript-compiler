type testable = (. unit) => Js.Promise.t<unit>

let tests: array<testable> = []

let addTest = t => tests->Js.Array2.push(t)->ignore
let addTest1 = (t, x) => tests->Js.Array2.push((. ()) => t(. x))->ignore

//
//
// Basic tests

let foo = @async (. x, y) => x + y

let bar =
  @async
  (. ff) => {
    let a = @await ff(. 3, 4)
    let b = @await foo(. 5, 6)
    a + b
  }

let baz = @async (. ()) => @await bar(. foo)

let testBaz: testable =
  @async
  (. ()) => {
    let n = @await baz(.)
    Js.log2("baz returned", n)
  }

testBaz->addTest

//
//
// Catching exceptions

exception E(int)

let e1: testable = @async (. ()) => raise(E(1000))
let e2: testable = @async (. ()) => Js.Exn.raiseError("Some JS error")
let e3: testable = @async (. ()) => @await e1(.)
let e4: testable = @async (. ()) => @await e2(.)
let e5: testable = %raw(`function() { return Promise.reject(new Error('fail')) }`)

let testTryCatch =
  @async
  (. fn) =>
    try @await
    fn(.) catch {
    | E(n) => Js.log2("testTryCatch: E", n)
    | JsError(_) => Js.log("testTryCatch: JsError")
    }

testTryCatch->addTest1(e1)
testTryCatch->addTest1(e2)
testTryCatch->addTest1(e3)
testTryCatch->addTest1(e4)
testTryCatch->addTest1(e5)

//
//
// Check for nested promise

let singlePromise = @async (. x) => x + 1

let nestedPromise =
  @async
  (. x) => {
    let resolve = x => [Js.Promise.resolve(x)]
    let _result = singlePromise(. x + 1)->resolve
    32
  }

//
//
// Test error handling in fetch

let explainError: unknown => string = %raw(`(e)=>e.toString()`)

let testFetch =
  @async
  (. url) => {
    switch @await
    Fetch.fetch(url) {
    | response =>
      let status = response->Fetch.Response.status
      Js.log2("Fetch returned status:", status)
    | exception JsError(e) => Js.log2("Fetch returned an error:", e->explainError)
    }
  }

testFetch->addTest1("https://www.google.com/sdkjdkghdsg")
testFetch->addTest1("https://www.google.comsdkjdkghdsg")

//
//
// Callbacks
let withCallback =
  @async
  (. ()) => {
    let callback = @async (. x) => x + 1
    callback
  } 

let testWithCallback =
  @async (. ()) => Js.log2("callback returned", @await (@await withCallback(.))(. 3))

testWithCallback->addTest

//
//
// Run tests

let rec runAllTests =
  @async
  (. n) => {
    if n >= 0 && n < Array.length(tests) {
      @await
      tests[n](.)

      @await
      runAllTests(. n + 1)
    }
  }

runAllTests(. 0)->ignore
