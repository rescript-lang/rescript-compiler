
type r =
  { a : unit;
    b : int;
    c : char;
    d : float; }

let r1 =
  {
    c = (print_endline "c1"; 'c');
    a = print_endline "a1";
    d = (print_endline "d1"; 1.);
    b = (print_endline "b1"; 2);
  }

let r2 =
  {
    b = (print_endline "b2"; 2);
    d = (print_endline "d2"; 1.);
    a = print_endline "a2";
    c = (print_endline "c2"; 'c');
  }

let r3 =
  { (print_endline "default"; r1) with
    d = (print_endline "d3"; 1.);
    c = (print_endline "c3"; 'c');
    a = print_endline "a3";
  }

let () = print_endline ""

type r2 =
  { x1 : unit;
    x2 : unit;
    x3 : unit;
    x4 : unit;
    x5 : unit;
    x6 : unit;
    x7 : unit;
    x8 : unit;
    x9 : unit; }

let a =
  {
    x5 = print_endline "x5";
    x6 = print_endline "x6";
    x1 = print_endline "x1";
    x3 = print_endline "x3";
    x4 = print_endline "x4";
    x9 = print_endline "x9";
    x7 = print_endline "x7";
    x8 = print_endline "x8";
    x2 = print_endline "x2";
  }

let () = print_endline ""

let b =
  { a with
    x7 = print_endline "x7";
    x2 = print_endline "x2";
  }

let () = print_endline ""

let c =
  { a with
    x2 = print_endline "x2";
    x7 = print_endline "x7";
  }

let () = print_endline ""

let c =
  { a with
    x2 = print_endline "x2";
    x7 = print_endline "x7";
    x5 = print_endline "x5";
  }

let () = print_endline ""

let d =
  { a with
    x5 = print_endline "x5";
    x7 = print_endline "x7";
    x2 = print_endline "x2";
  }
