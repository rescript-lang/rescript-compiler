# 1.9.2

Fixes:
- #1943, Wrong name mangling for properties "_50"
- #1029, tree shaking in playground
- #1946, Fix invalid JS output
- #1965, more JS modules exported in playground
- #1559, add a comment when no js output produced
- #1989, fix a bug for exit inlining
- #2002, make default exports work with transpiled babel es6 import
- A bunch of improvements for better error message by Cheng Lou and Cristiano Calcagno

Features:
- #1944, bspack support -main-export
- #1990, better optimizations for pattern match compilation
- #1991, Accept `bs.deriving accessors` applies to `bs.config` as well for single field
- #2001, improve global module compilation
- #2006, "subdirs" : true will traverse the directory recursively
- #1964, for `Capital_file.ml` generate `Capital_file.js` instead of `capital_file.js`

Deprecations:
- #1968, remove support for google module system
# 1.9.1 (Recovery 1.9.0)
Fixes
- #1933 hyphen directory name fixes
# 1.9.0

Features:
- Namespace support in the build system
- #1839, #1653, allow in-source build in package-specs , allow single pacakge-spec element in package-specs
- #1802 introduce [@bs.unwrap] for polymorphic variant as external argument
- Improve error message via -bs-super-errors
- Reason syntax error message  for .re/rei files
- #1908 two APIs for Js.Re
- #1905, #1906, simplify the workflow of handlding null or undefined (via nullable)
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
- #1811 add relative ppx paths to .merlin correctly
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
- #1666, fix misue of GADT api

Features:
- #1648, exposed `bsc` in the npm environment
- #1647, special handling `bsb -init .` to reuse current directory
- #1667, fix an optimization bug
- #1698, fix exit code incorrectly aggregated issue
- #1666, add Js.Json.classify and Js.Types.classify
- #1705, add dom storage api
- #1672, sync up with new reason
- #1696, provide reason-react template

# 1.7.4(May 24, 2017):

Internal:
- #1583, add -U -D support for bspack

Features:
- #1630, add modules Option, Result, List, and Vector into Js namesapace, update docs

- #1613, allow bs.scope with bs.send/bs.send.pipe/bs.set/bs.get/bs.set_index/bs.get_index
- #1604, add the functions `entries`, `values`, `fromList`, `fromArray` and `map` to `Js.Dict`

- #1632, bsb themes support

Fixes:
- #1581, more error checking
- #1633, fix missing installations
- #1581, more error checking %identity

# 1.7.3:

Fixes:
- #1556, fix duplicated requires of runtime (report by Chenglou) 

- #1568, internal compiler error

Features:
- #1564: scoped values in FF, see `bs.scope` in the Manual



