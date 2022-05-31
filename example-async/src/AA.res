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
    try {@await fn(.)} catch {
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

module Fetch = {
  //@raises(JsError)
  let fetch = url => Fetch.fetch(url)

  let status = response => Fetch.Response.status(response)
}

let explainError: unknown => string = %raw(`(e)=>e.toString()`)

let testFetch =
  @async
  (. url) => {
    open Fetch
    switch {@await fetch(url)} {
    | response =>
      let status = response->status
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
    @async (. x) => @await (x->Js.Promise.resolve) + 1
  }

let testWithCallback =
  @async (. ()) => Js.log2("callback returned", @await (@await withCallback(.))(. 3))

testWithCallback->addTest

//
//
// Async list
module AsyncList = {
  let map =
    @async
    (. l, f) => {
      let rec loop =
        @async
        (. l, acc) =>
          switch l {
          | list{} => acc
          | list{p, ...rest} => @await loop(. rest, list{@await p, ...acc})
          }

      @await
      loop(. l->Belt.List.mapReverse(x => f(. x)), list{})
    }
}

let fetchAndCount = {
  let counter = ref(0)

  let ff =
    @async
    (. url) => {
      let response = @await Fetch.fetch(url)
      counter := counter.contents + 1
      (counter.contents, response->Fetch.status)
    }

  ff
}

let testFetchMany =
  @async
  (. ()) => {
    let fetchedItems =
      @await
      AsyncList.map(.
        list{
          "https://www.google.com",
          "https://www.google.com",
          "https://www.google.com",
          "https://www.google.com",
          "https://www.google.com",
        },
        fetchAndCount,
      )
    fetchedItems->Belt.List.forEach(((i, s)) => Js.log3("Fetched", i, s))
  }
testFetchMany->addTest

//
//
// Fetch with Result type
module FetchResult = {
  let fetch =
    @async
    (. url) => {
      switch {@await Fetch.fetch(url)} {
      | response => Ok(response)
      | exception JsError(e) => Error(e)
      }
    }
}

let nextFetch = (. _response) => Some("https://github.com/")

let testFetchWithResult =
  @async
  (. ()) => {
    switch @await
    FetchResult.fetch(. "https://www.google.com") {
    | Ok(response1) =>
      Js.log2("FetchResult response1", response1->Fetch.status)
      switch nextFetch(. response1) {
      | None => ()
      | Some(url) =>
        switch @await
        FetchResult.fetch(. url) {
        | Ok(response2) => Js.log2("FetchResult response2", response2->Fetch.status)
        | Error(_) => ()
        }
      }
    | Error(_) => ()
    }
  }

// // imaginary syntax
// let testFetchWithResult = async () =>
//   if let Ok(response1) = await FetchResult.fetch("https://www.google.com")
//      and Some(url) = nextFetch(response1)
//      and Ok(response2) = await FetchResult.fetch(url) {
//     Js.log2("FetchResult response2", response2->Fetch.Response.status)
//   }

testFetchWithResult->addTest

//
//
// Run tests

let rec runAllTests =
  @async
  (. n) => {
    if n >= 0 && n < Array.length(tests) {
      @await
      (@doesNotRaise tests[n])(.)

      @await
      runAllTests(. n + 1)
    }
  }

runAllTests(. 0)->ignore

//
//
// Curried functions

let bb = @async x => @await x

let cc = @async (x, ~y=x, z) => @await x + @await y + @await z

let dd = @async x => {y => @await x + @await y}

let ee = @async (. x) => {y => @await x + @await y}

//
//
// Errors

// let aa =
//   @async
//   (. x) => {
//     let cb = (. _) => @await x // Error: Await on expression not in an async context
//     cb
//   }

// let _ = @async (_, . x) => @await x // Error: Await on expression not in an async context
