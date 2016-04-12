[js_of_ocaml](https://github.com/ocsigen/js_of_ocaml) is a compiler which compiles OCaml's bytecode into Javascript, it has been developed for several years and ready for production while bucklescript is still a very young project(but moving very fast!).

There is [an issue](https://github.com/ocsigen/js_of_ocaml/issues/338) about why I originally started this project.

The major advantage of js_of_ocaml is that it is non intrusive, you can reuse existing OCaml build system to have its bytecode output and get a javascript output. With bucklescript, users have to adapt its build system for existing OCaml code base.

In my opinion, if you have some OCaml code and happen to want it runnable inside the browser, than js_of_ocaml is currently better, however, if you want to treat javascript(browser/nodejs) as its first class backend and compile it into native code when you want best performance, then you can give bucklescript a try, I believe both projects would help each other and learn from each other.


## Debuggable output

Bucklescript has a debuggable output, its output is pretty close to original javascript if you use features which are also available in javascript, we will still try to make its output readable even for features not available in Javascript.

On our roadmap we plan to introduce a debug mode, which will instrument more debug data(combined with chrome formatter tools) so that user can just debug the generated javascript without pain.

It is impossible to debug js_of_ocaml output without sourcemap, even with sourcemap (note that we plan to support sourcemap as well), it can not go very far, since sourcemap can only point locations correctly, the `symbol names` are still gone.

## Separate and faster compilation, potential hot module reloading support

Bucklescript compiles one ocaml module into one Javascript module, it does less work than js_of_ocaml. js_of_ocaml first compile and link to low level byte code and then compile it back to Javascript, which means some unnecessary redundant work.

## Runtime representations && FFI

Bucklescript tries to stay close to javascript, for example, Buckelescript's array is Javascript's array, while js_of_ocaml introduces one field difference. Bucklescript uses simple attributes to support FFI while js_of_ocaml introduces a fairly sophisticated syntax extension and type tricks.

Since bucklescript's excellent support of FFI, its runtime libraries are written in OCaml as well which means more maintainable and better optimizations opportunity.

## Performance

We did not do exhaustive benchmarks currently, our initial results shows they are similar (using Google Closure Simple mode for bucklescript), in some cases bucklescript are marginally better. We will focus on better optimizations later.