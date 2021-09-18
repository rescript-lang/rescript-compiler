[@@@config
{
  flags =
    [|
      "-w";
      "@A";
      (* "-drawlambda"; *)
      (* "-dtypedtree"; *)
      "-bs-diagnose"
      (* "-dparsetree"; *)
      (* "-dsource"; *)
      (* "-bs-no-builtin-ppx"; *);
    |];
}]

type color = Orange | Color of string

let a = Color "#ffff"

let c =
  match a with Orange -> "orange" | Color "#ffff" -> "white" | Color s -> s
