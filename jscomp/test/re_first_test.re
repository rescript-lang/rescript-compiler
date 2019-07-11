// ocamldep.opt -modules -pp '../../lib/refmt.exe --print=binary' -impl re_first_test.re

let suites = ref([]);
let test_id = ref(0);

let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y);

let x = List.length([1, 2, 3]);
let u = 3;

[@bs.deriving jsConverter]
type adapter = [ | `idb | `leveldb | `http];

eq(__LOC__, adapterToJs(`idb), "idb");

Mt.from_pair_suites(__MODULE__, suites^);
