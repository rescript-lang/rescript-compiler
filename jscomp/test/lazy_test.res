let u = ref(3)
let v = lazy (u := 32)

let lazy_test = () => {
  let h = u.contents
  let g = {
    Lazy.force(v)
    u.contents
  }
  (h, g)
}

let f = x =>
  switch x {
  | (lazy (), _, {contents: None}) => 0
  | (_, lazy (), {contents: Some(x)}) => 1
  }

/* PR #5992 */
/* Was segfaulting */
let s = ref(None)
let set_true = lazy (s := Some(1))
let set_false = lazy (s := None)

let h = try f((set_true, set_false, s)) catch {
| Match_failure(_) => 2
}

let u_v = ref(0)
let u = lazy (u_v := 2)
let () = Lazy.force(u)

/* module Mt = Mock_mt */

let exotic = x =>
  switch x {
  /* Lazy in a pattern. (used in advi) */
  | lazy y => y
  }

/* let l_from_val = Lazy.from_val 3 */

let l_from_fun = Lazy.from_fun(_ => 3)
let forward_test = lazy {
  let u = ref(3)
  incr(u)
  u.contents
}
/* module Mt = Mock_mt */

let f005 = lazy (1 + 2 + 3)

let f006 = lazy {
  let x = 3
  _ => x
}

let f007 = lazy raise(Not_found)
let f008 = lazy {
  print_endline("hi")
  raise(Not_found)
}

let a2 = x => lazy x

let a3 = lazy 3
let a4 = a2(3)
let a5 = lazy None
let a6 = lazy ()

let lazy a7 = a5
let lazy a8 = a6

Mt.from_pair_suites(
  __MODULE__,
  {
    open Mt
    list{
      ("simple", _ => Eq(lazy_test(), (3, 32))),
      ("lazy_match", _ => Eq(h, 2)),
      ("lazy_force", _ => Eq(u_v.contents, 2)),
      ("lazy_from_fun", _ => Eq(Lazy.force(l_from_fun), 3)),
      ("lazy_from_val", _ => Eq(Lazy.force(Lazy.from_val(3)), 3)),
      ("lazy_from_val2", _ => Eq(\"@@"(Lazy.force, Lazy.force(Lazy.from_val(lazy 3))), 3)),
      (
        "lazy_from_val3",
        _ => Eq(
          {
            %debugger
            \"@@"(Lazy.force, Lazy.force(Lazy.from_val(forward_test)))
          },
          4,
        ),
      ),
      (__FILE__, _ => Eq(a3, a4)),
      (__FILE__, _ => Eq(a7, None)),
      (__FILE__, _ => Eq(a8, ())),
      (__LOC__, _ => Ok(Lazy.is_val(lazy 3))),
      (__LOC__, _ => Ok(\"@@"(not, Lazy.is_val(lazy raise(Not_found))))),
    }
  },
)
