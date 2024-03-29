type t0 = A | B | C
/* https://github.com/ocaml/ocaml/pull/632 */

let f = x =>
  switch x {
  | B => 2
  | A => 1
  | C => 3
  }

let f_0 = x =>
  switch x {
  | A => -1
  | B => 0
  | C => 1
  }

type t1 =
  | T000
  | T001
  | T002
  | T003

let f2 = x =>
  switch x {
  | ' ' => T000
  | '' => T001
  | '' => T002
  | _ => T003
  }

type t2 =
  | X0
  | X1
  | X2
  | X3
  | X4

type t3 =
  | Y0
  | Y1
  | Y2
  | Y3
  | Y4
  | Y5(int)

let f3 = x =>
  switch x {
  | X0 => Y0
  | X1 => Y1
  | X2 => Y2
  | X3 => Y3
  | X4 => Y4
  }

type t4 = T400

let f4 = x =>
  switch x {
  | T400 => 3
  }

type t5 =
  | A
  | B
  | F
  | C(int)
  | D(string)
  | E(int)

let f5 = x =>
  switch x {
  | A => 1
  | B => 3
  | F => 4
  | C(_) | D(_) => 1
  | _ => 2
  }

let f6 = x =>
  switch x {
  | A | B => 0
  | C(_) | D(_) | E(_) => 1
  | _ => 2
  }

let f7 = x =>
  switch x {
  | A => 1
  | B => 2
  | C(_) => 3
  | D(_) => 4
  | _ => -1
  }

type t6 =
  | T60
  | T61
  | T62
  | T63
  | T64(int)
  | T65(int)
  | T66(int)
  | T68(int)

let f8 = x =>
  switch x {
  | T60
  | T61 => 1
  | T64(_)
  | T65(_) => 2
  | _ => 3
  }

/*
ocaml402
{[
  (function param/1098
          (catch
            (catch
              (catch
                (switch param/1098
                 case int 0: (exit 11)
                 case int 1: (exit 11)
                 case int 2: (exit 11)
                 case tag 0: (exit 12)
                 case tag 1: (exit 12)
                 default: (exit 13))
               with (13) 3)
             with (11) 1)
           with (12) 2))
]}

ocaml406
{[
   (function param/1069
         (catch
           (catch
             (switch param/1069
              case int 3: (exit 10)
              case tag 0: (exit 9)
              case tag 1: (exit 9)
              case tag 2: (exit 10)
              case tag 3: (exit 10)
              default: 1)
            with (10) 3)
          with (9) 2))
]}


*/
let f9 = x =>
  switch x {
  | T60
  | T61
  | T62 => 1
  | T64(_)
  | T65(_) => 2
  | _ => 3
  }

let f10 = x =>
  switch x {
  | T60
  | T61
  | T62
  | T63 => 1
  | T64(_)
  | T65(_) => 2
  | _ => 3
  }

let f10 = x =>
  switch x {
  | T60 => 0
  | T61 => 2
  | T62 => 4
  | T63 => 1
  | T64(_)
  | T65(_) => 2
  | _ => 3
  }

/* let f11 = function 
  | T60 -> 0
  | T61 -> 2 
  | T62  -> 4 
  | T63 -> 1 
  | T64 _ 
  | T65 _ -> 2
  | _ -> 3 */

type t11 = A | B | C | D(int) | E(char)

let f11 = (x: t11) =>
  switch x {
  | D(_) => 1
  | A
  | B
  | C => 2

  | _ => assert(false)
  }
