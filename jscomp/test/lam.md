

== optimizations done in `Lam.convert`

1. Specialize `CamlinternalMod`, `Pervasives.(^)` functions for recursive module
2. Alias removing

[source,ocaml]
--------------
let x = y (x/y)
let u = x (u/x, x/y -> u/y)
--------------

3. extract `Psj_unsafe_downgrade`

4. scc for recursive functions