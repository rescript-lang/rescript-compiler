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