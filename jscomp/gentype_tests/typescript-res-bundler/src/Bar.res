@genType
let makeFoo = (~bar) => {
  open Foo
  {...make(), bar}
}
