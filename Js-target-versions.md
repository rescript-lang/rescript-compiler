bucklescript targets to ES5,

Below is a list of polyfills from ES6 needed:

1. `Math.imul` for int 32 multiplication

2. `Int8Array`, `Uint8Array`, `Int16Array`, `Uinit16Array`, `Int32Array`, `Uint32Array`, `Float32Array`, `Float64Array` for library bigarray support. (the support of bigarray has to be done in the compiler level, since its instructions are hard coded in the compiler itself)

