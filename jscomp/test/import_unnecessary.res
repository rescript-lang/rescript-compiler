// Import_lazy_component is not a pure module
// It is supposed not to be required by dynamic import
let a = Js.import(Import_lazy_component.a)

module LazyC = await Import_lazy_component
