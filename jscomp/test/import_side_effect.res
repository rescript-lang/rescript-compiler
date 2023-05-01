// Import_lazy_component is not a pure module
// It is supposed not to be required by dynamic import
let a = Js.import(Side_effect2.a)

module M = await Side_effect
