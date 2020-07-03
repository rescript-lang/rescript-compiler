switch x {
| Blue | Red => ()
| (Blue | Red) as colour => ()
| Blue as c1 | Red as c2 => ()
| (Blue as c1) | (Red as c2) => ()
| exception Exit | exception Continue => ()
| exception (Exit | exception Continue) => ()
| lazy x | lazy y => ()
| lazy (x | lazy y) => ()
}
