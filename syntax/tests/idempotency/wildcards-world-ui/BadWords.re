type t;
[@bs.new] [@bs.module "bad-words"] external make: unit => t = "default";
[@bs.send] external clean: (t, string) => string = "clean";
