[@@@config
{
  flags =
    [|
      "-w";
      "@A";
      "-drawlambda";
      (* "-dtypedtree"; *)
      "-bs-diagnose"
      (* "-dparsetree"; *)
      (* "-dsource"; *)
      (* "-bs-no-builtin-ppx"; *);
    |];
}]

type t = A of (t -> int) [@@unboxed]

let g x = match x with A v -> v x

let loop = g (A g)
