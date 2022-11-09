let willBeInlined = async () => 3

let inlined = willBeInlined ()

let wrapSomethingAsync: unit => unit = () => {
  let _ = (
    async (_) => {
      let test = await Js.Promise.resolve("Test")
      Js.log(test)
    }
  )(777)
}

module M: {
  let broken: (unit => promise<'a>) => promise<'a>
} = {
  let doSomethingAsync = async (someAsyncFunction) => {
    await someAsyncFunction()
  }

  let broken = someAsyncFunction => doSomethingAsync(someAsyncFunction)
}

let broken = async (someAsyncFunction) => {
  await someAsyncFunction()
}

let broken = someAsyncFunction => broken(someAsyncFunction)
