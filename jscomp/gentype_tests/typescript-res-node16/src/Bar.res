@genType
let makeFoo = (~bar) => {
  open Foo
  {...make(), bar}
}

@genType let jsonStringify = Js.Json.stringify
