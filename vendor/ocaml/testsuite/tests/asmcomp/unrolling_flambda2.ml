
type t = { fn : t -> t -> int -> unit -> unit }

let rec foo f b n x =
  if n < 0 then ()
  else begin
    foo f b (n - 1) x;
    b.fn f b (n - 1) x
  end
[@@specialise always]

let rec bar f b n x =
  if n < 0 then ()
  else begin
    bar f b (n - 1) x;
    f.fn f b (n - 1) x
  end
[@@specialise always]

let () = foo {fn = foo} {fn = bar} 10 ()
