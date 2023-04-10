type rec t<'a> = Fix(lazy_t<t<'a>>)

let rec fix = () => Fix(lazy fix())

let rec unfixLeak = (Fix(f)) => \"@@"(unfixLeak, Lazy.force(f))

let unfix = p =>
  while true {
    p :=
      switch p.contents {
      | Fix(lazy h) => h
      }
  }
/* ;; unfixLeak (fix ()) */

/* ;; unfix (ref (fix ())) */
