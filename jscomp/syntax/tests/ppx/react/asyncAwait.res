let f = a => Js.Promise.resolve(a + a)

@react.component
let make = async (~a) => {
  let a = await f(a)
  <div> {React.int(a)} </div>
}
