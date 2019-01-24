module A' = A (* missing a.cmi *)
module B' = B (* broken b.cmi *)
module C' = C (* valid c.cmi *)
module D' = D (* valid d.cmi *)
let () = print_int D'.something
