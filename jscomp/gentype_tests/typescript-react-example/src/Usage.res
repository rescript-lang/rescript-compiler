module Wrapper = {
  @genType
  module MyModuleAlias = MyModule
}

let a = 5
@genType
let b = Wrapper.MyModuleAlias.add(a, 3)
