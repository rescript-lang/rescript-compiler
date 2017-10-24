
(* Test file for module [Nonnegative] *)

open Printf
open Graph
open Pack.NonnegDigraph
open Nonnegative

let test () =
  let v = Array.init 10 V.create in
  let g = create () in

  let test result op =
    begin
      try
	op g;
      with
	| Assert_failure _ -> print_string "bug bug bug?"; assert false
	| exc ->
	  let exc_name = Printexc.to_string exc in
	  if exc_name = "Nonnegative.Imperative(G)(W).Negative_cycle(_)" then
	    assert (not result)
	  else begin
	    print_endline exc_name;
	    Printexc.print_backtrace stdout;
	    assert false
	  end
    end;
    flush_all ();
    display_with_gv g
  in

  let add   s   t g = add_edge      g           v.(s)   v.(t)  in
  let add_e s w t g = add_edge_e    g (E.create v.(s) w v.(t)) in
  let rem   s   t g = remove_edge   g           v.(s)   v.(t)  in
  let rem_v s     g = remove_vertex g           v.(s)          in
  let rem_e s w t g = remove_edge_e g (E.create v.(s) w v.(t)) in

  test true  (add    0        1);
  test true  (rem    0        1);
  clear g;

  test true  (add_e  0    1   1);
  test true  (add_e  0    1   1);
  test true  (rem    0        1);
  clear g;

  test true  (add    0        1);
  test true  (add_e  0    1   1);
  test true  (rem    0        1);
  test true  (rem_v  0         );
  test true  (rem_v  1         );
  clear g;

  test true  (add_e  2    1   3);
  test true  (add_e  3    2   4);
  test true  (add_e  0  (-1)  2);
  test true  (add_e  3    1   4);
  test false (add_e  4  (-4)  2);
  test false (add_e  4  (-3)  2);
  test true  (add_e  4  (-2)  2);
  test true  (add_e  1    3   2);
  test true  (add_e  5  (-2)  4);
  test false (add    2        5);
  test true  (add_e  2    4   5);
  test true  (rem    2        3);
  test true  (rem_v  2         );
  clear g;

  test false (add_e  1  (-1)  1);
  test true  (add    1        1);
  test true  (add_e  1    1   1);
  test true  (add_e  1    1   2);
  test false (add_e  2  (-2)  1);
  test true  (rem_v  1         );
  clear g;

  test true  (add    1        2);
  test true  (add_e  2    1   3);
  test true  (add_e  3    2   4);
  test true  (add_e  4    3   1);
  test true  (add    5        6);
  test true  (add_e  6    1   7);
  test true  (add_e  7    2   8);
  test true  (add_e  8    3   5);
  test true  (add_e  1  (-3)  5);
  test false (add_e  8  (-4)  4);
  test true  (add_e  8  (-3)  4);
  test true  (rem    1        5);
  assert false; (* FIXME: Bug in choosing new source *)
  clear g;

  printf "All test succeeded.\n";
  ()

let _ = test ()
