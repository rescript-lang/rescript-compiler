# 3.1.0

Features:

- #2809, #2806 Sync up with latest reason 
- #2805, allow `x |. Some `
- #2797, #2771 `-bs-g`
- #2793 more bindings to Js.Console
- #2621 add Belt.Result module
- #2760 add Belt.Array.unzip

Perf:

- #2808 better code gen for if then else
- #2804 make HashSet.String size smaller

Fixes:

- #2812, compatibility with Node 10
- #2789, fix Weak.length
- #2790, fix Belt.Set performance issue
- #2786, fix polymorphic compare on nullables
- #2781, improve location info on bs.deriving abstract
- #2776, lift the function limitation on bs.deriving abstract
- #2752, fix binding Buffer.toString
# 3.0.0

Features:

- #2740, #2726 Generalized safe/cleaner embedding raw function (https://bucklescript.github.io/docs/en/interop-cheatsheet.html#generaizlied-raw-js-since-300)

- #2687, #2665, #2663 bs.deriving abstract type, a powerful way for idiomatic JS and FFI
  (https://bucklescript.github.io/docs/en/generate-converters-accessors.html#abstract-type)
  
- #2696, #2742, #2706, #2711, compile OCaml boolean as JS boolean 
  Breaking change to your code path relying on `Obj.magic` and `bs.raw`



- #2741  add Node.Buffer.fromStringWithEncoding 
- #2737 add Js.Json.stringifyWithSpace 
- #2728 add console.error and console.trace 
- #2666 add List.sort to belt
- #2664 pipe syntax support tuple 
            `obj |. method a b ` and `obj |. (m1 , m2)`


Perf:
- #2676 beter optimizations for tuple allocation

Docs:
- #2730, #2729, #2733, #2722, #2701, #2699 More docs to Belt 

Fixes:
- #2657 more intuitive polymorphic comparison with objects
- #2686 playground unicode
- #2720, #2719, #2718, error message enhancment
- #2731 not inlining function contains `raw`
- #2672 Fix ci
- #2652 fix Buffer name mangling on Node
- #2642 weird indentation in generated code 

# 2.2.3

Features:
- #2646,#2622 Adding Belt.Option
- #2342, #2624 (|.) pipe syntax for t first convention
- #2589 Expose Id.MakeComparable functor
- #2587 Added production ready settings for react theme
- * remove refmt syntax version 2

Performances:
- #2438 using concrete predicates for integer comparison
- #2584 better handling of if then else common sub expression

Fixes:
- #2303 defining or using a module named "Block" causes runtime errors
- #2616,#2612,#2554 better error message
- #2352 return value of assignment expression
- #2413 no break generated after return statement in some code branches
- #2633 [@bs.string] in FFI
- #2608 short-circuiting of && fails due to extraction of variable
- #2559 fix Bytes.create semantics
- #2638, bsb -w on windows freez
- #2448, js_int.ml not installed on windows

# 2.2.2

Features:
- Upgrade with latest reason syntax (native unucurry support etc)
- #2531 add missing functions in Js.Nullable fromoption/toOption
- #2527 Belt.List.shuffle

Fixes:
- #2503 bs.string and bs.obj interaction
- #2549, #2548, $2548, #2542, #2541 improve error message

# 2.2.0
Features:
- A beta release for the new stdlib called Belt
- #2436, #2381, #2377, #2353 bs.deriving abstract support


Performances:
- #2452 specialized comparison with Js.null, Js.undefined, Js.boolean
- #2412, provide specialized primitives for comparison with null/undefined
- #2361, better  optimization for temporary tuple
Fixes:
- #2451 better error message when arity mismatch for reason syntax
- #2399, turn partial application warnings to error in react template
- #2465 build on FreeBSD
- #2450 ignore bsb.lock
- #2356 ship build-schema.json
- #2489, #2464  capitalize names in combination of '/'
- #2459 subdirs:true by default for templates
- #2428, fix trailing space on react-jsx
- #2401, stop tab-aligning imports for smaller diff
- #2383, drop bs.deriving attribute after post-processing
# 2.1.0
Features:
- #2282, #2280,#2272,#2271,#2270,#2262,#2260,#2255,#2253
  Automatically derive js converter between ocaml and Js values
  see docs: https://bucklescript.github.io/bucklescript/Manual.html#_mapping_between_js_values_and_ocaml_values_since_2_1_0
- #2238, #2225, #2221
  Make the compiler relocatable
  prebuilt compiler (this release for Mac/Win)
- #2276  update reason syntax@d0d18
- #2229 improve error message with regard to `@bs`
- #2266, add Js_global.(encode|decode)URI(Component) bindings
- #2220 make watcher mode in linux accept ninja progress animation
- #2163 better hints for binding module name
- #2187, #2186, #2179 add two warning numbers 101, 102 for polymorhic comparisons
  and unused bs attributes

Performance:
- #2269 type specialized comparison also applied to nullable polymorphic variants
- #2208 type specialized comparison allpied to nullable variants
- #2213 refine caml_obj_dump into caml_array_dup better array initialization code

Fixes:
- #2316 Pattern match with exception case and a single catch-all pattern is optimized incorrectly
- #2318 no absname in Match_failure
- #2250 #1285, fix code gen for object oriented code
- #2278, #2274 fix fatal errors regression and syntactice fatal errors(-werror A) don't stop building
- #2259 fix fatal errors don't stop generating cmj file
- #1972 bsb -init does not rely on `npm link` on *nix platform
- #2207 nop rebuild to work around yarn bug
- #2226 kill bsb -w when stdin is closed
- #2184 bsb should exclude -I empty dirs

# 2.0
Features
- update reason3 syntax

# 1.10.3
Features:
- #2112, introduced a key `suffix`, so that user can
  choose `suffix : ".bs.js"`
- #2138, in combination of `.bs.js` suffix and `in-source` build,
  bsb is able to remove staled build artifact
- #2091 bsc xx.cmi will print xx.mli so users can generate
  mli in the beginning. `bsc -output-re xx.cmi` will print
  it in reason syntax
- #2096, clorized ninja build output

- #2120 better error message in the location of `{json||json}`
- #2123 avoid namespace leaking in types
- #2130 make Sys module not break React Native bindings
- #2159, #2165 enhance user expereince of bsb (less verbose, status bar when failed)
- #2134, allow people to make customized playground via a plugin

Fixes:
- #2157, fix unnecessary rebuilding when adding files with namespace option on
- #2145, fix bsb unnecessary rebuild issues
- #2150, fix an edge case of comparison between cyclic value
- #2140 tweak invalid npm package error message
- #2080, #2094 bsb -w can detect multiple processes running
  so no race condition
- #2097 default warning with "-30"
- #2101 fix bs.splice error message
- #2102 fix reason printer bug
- #2089 more logs when ocaml fails to build
- e16cbd03c64eb2b7e99570abdc29f5799778835f fix a binding of `Js.Re.matches`

# 1.9.3

Features:
- #2049, add a dedicated `warning` field, so that it is easy to override, enable `warn-error` and customize it.
- React JSX PPX V3 is in. This allows a custom component's `children` to be of any type. When the child's a single non-JSX item, it's not wrapped in an array anymore.
- #2016, add a flag `bsb -v` to tell the version number
- 64ea144746f998955f69d8eb4ec2b0179ce2d5b4 add Js.Typed_array subarray API
- #2030, error message: better unbound value message
- #2041, add a flag `bsb -verbose`, by default it is less noisy
- #2047, make bsb error message more professional
- #2048, turn `-bs-super-error` by default for reason files


Deprecations:
- React JSX PPX V1 has been removed, and the bsconfig `"react-jsx": true` is removed too (use `2` or `3`). JSX only accept a version number
- #1666, remove deprecated API

Fixes:
- #2029, error message: fix display line and column
- #2033, #2040 error message: trim output when too many lines
- #2042, better react error message
- #2053, better error message when file name is non-existent
- #2066, add a missing escaped char ':'
- #2063, add Js.Float for windows, remove deprecated Js.Float API
- #2070, fix -warn-error interaction with -bs-super-error

# 1.9.2

Fixes:
- #1943, Wrong name mangling for properties "_50"
- #1029, tree shaking in playground
- #1946, Fix invalid JS output
- #1965, more JS modules exported in playground
- #1559, add a comment when no js output produced
- #1989, fix a bug for exit inlining
- #2002, make default exports work with transpiled babel ES6 import
- A bunch of improvements for better error message by Cheng Lou and Cristiano Calcagno

Features:
- #1944, bspack support -main-export
- #1990, better optimizations for pattern match compilation
- #1991, Accept `bs.deriving accessors` applies to `bs.config` as well for single field
- #2001, improve global module compilation
- #2006, `"subdirs": true` will traverse the directory recursively
- #1964, for `Capital_file.ml` generate `Capital_file.js` instead of `capital_file.js`

Deprecations:
- #1968, remove support for Google module system
# 1.9.1 (Recovery 1.9.0)
Fixes
- #1933 hyphen directory name fixes
# 1.9.0

Features:
- Namespace support in the build system
- #1839, #1653, allow in-source build in package-specs , allow single package-spec element in package-specs
- #1802 introduce [@bs.unwrap] for polymorphic variant as external argument
- Improve error message via -bs-super-errors
- Reason syntax error message  for .re/.rei files
- #1908 two APIs for Js.Re
- #1905, #1906, simplify the workflow of handling null or undefined (via nullable)
Optimizations:
- #1918, better code gen for pattern match
- #1865, Add Js.logN

Fixes
- #1925, fix missing js modules in playground
- #1893, fix Js.Array.append (Js.Vector.append)

# 1.8.2
Features:
- #1798 make `default` the same semantics as ES6 exports
- #1785 upgrade playground
- #1758 [generator support](https://bucklescript.github.io/bucklescript/Manual.html#_customize_rules_generators_support_since_1_7_4)
- #1826 add `bsb -where` support so that bsb.exe can be located and cached in a more robust way

Optimizations:
- #1796, #1793 improve submodule arity inference
- #1810 don't rebuild ninja if binary already exists in bin folder

Fixes:
- #1811 add relative PPX paths to .merlin correctly
- #1822, fix an optimization bug

Internal:
- add a tool cmjdump.exe

# 1.8.1
Fixes:
- #1762, discard effectful unused code
- #1760, div by zero effect analysis

# 1.8.0
Fixes:
- #1573, don't include `-bs` flags in `.merlin`
- #1716, fix wrong optimization of recursive values
- #1728, bad inlining
- #1409, make sure when optional is None, bs.obj will not introduce such label
- #1737, same as #1409
- #1746, a corner case when mixing recursive value and functions, should make markup.ml work
- #1749, fix nested if, code block discarded issue

# 1.7.5

Fixes:
- #1676, `bsb -w` will always build regardless of filetype when fs.watch doesn't send a filename
- #1655, fix #1653 Js.Promise.all[n] interfaces
- #1658, fix typeof = "null" issue
- #1656, bs.get/set/get_index/set_index respects bs.ignore
- #1654, `bsb -init` fails if package or current dir has space (parent dir can have spaces)
- #1678, bs.get{null;undefined}  in object type
- #1692, fix invalid js syntax output
- #1701, fix tailcall handling interaction with  exception handler
- #1666, fix misuse of GADT API

Features:
- #1648, exposed `bsc` in the npm environment
- #1647, special handling `bsb -init .` to reuse current directory
- #1667, fix an optimization bug
- #1698, fix exit code incorrectly aggregated issue
- #1666, add Js.Json.classify and Js.Types.classify
- #1705, add DOM storage API
- #1672, sync up with new reason
- #1696, provide reason-react template

# 1.7.4

Internal:
- #1583, add -U -D support for bspack

Features:
- #1630, add modules Option, Result, List, and Vector into Js namespace, update docs

- #1613, allow bs.scope with bs.send/bs.send.pipe/bs.set/bs.get/bs.set_index/bs.get_index
- #1604, add the functions `entries`, `values`, `fromList`, `fromArray` and `map` to `Js.Dict`

- #1632, bsb themes support

Fixes:
- #1581, more error checking
- #1633, fix missing installations
- #1581, more error checking %identity

# 1.7.3

Fixes:
- #1556, fix duplicated requires of runtime (report by Cheng Lou)
- #1568, internal compiler error

Features:
- #1564: scoped values in FF, see `bs.scope` in the Manual

# 1.3.2

Features:
- Significantly improve bsb experience

## 1.2.1 + dev

Features:
- #861, add `-bs-assume-no-mli` and `-bs-no-implicit-include` for deterministic build
- #851, add -bs-D -bs-list-conditionals flags
- add `-bs-syntax-only`
- #854, add `-bs-binary-ast`

# 1.1.2

Fixes:
- #831, bug fix with opam issues

Features:
- Provide `bspp.exe` for the official compiler

# 1.1.1

Features:

- #822, add `bsdep.exe`
- #820, conditional compilation support
- #793, relax syntactic restrictions for all extension point so that `bs.obj`, `obj`, `bs.raw`, `raw`, etc. will both work. Note that all attributes will still be qualified
- #793, support `bs.splice` in `bs.new`
- #798, complete `bs.splice` support and documentation

# 1.0.3

Incompatible changes (to better support Windows):
- `bsc`, `bspack` and `bsppx` are renamed into `bsc.exe`, `bspack.exe` and `bsppx.exe`
- No symlink from .bin any more.

  **Old symlinks**:

  ```sh
  tmp > ls -al node_modules/.bin/
  total 96
  drwxr-xr-x  14 hzhang295  staff  476 Sep 20 17:26 .
  drwxr-xr-x   4 hzhang295  staff  136 Sep 20 17:27 ..
  lrwxr-xr-x   1 hzhang295  staff   22 Sep 20 17:26 bsc -> ../bs-platform/bin/bsc
  lrwxr-xr-x   1 hzhang295  staff   25 Sep 20 17:26 bspack -> ../bs-platform/bin/bspack
  lrwxr-xr-x   1 hzhang295  staff   24 Sep 20 17:26 bsppx -> ../bs-platform/bin/bsppx
  ```

  Now these symlinks are removed. You have to refer to `bs-platform/bin/bsc.exe`.

Features:
- #787, add an option `-bs-no-warn-unused-bs-attribute`

# 1.0.2

Fixes:
- #743, Fix Bytes.blit when `src == dst`

Features:
- #783, by default, `bsc.exe` will warn when it detect some OCaml data types are passed from/to external FFI
- #784, add an option `-bs-eval`

# 1.0.1

Fixes:
- #718, Enforce `#=` always return unit for better error messages

Features:
- FFI
  - #694, support fields and mutable fields in JS object creation and private method
  - #686, introduce phantom arguments (`bs.ignore`) for ad-hoc polymorphism

# 1.0.0

Initial release
