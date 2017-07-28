(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* To run this example:
   ********************
   1. Select all the text in this window.
   2. Drag it to the toplevel window.
   3. Watch the colors.
   4. Drag the mouse over the graphics window and click here and there.
   5. Type any key to the graphics window to stop the program.
*)

open Graphics;;
open_graph " 480x270";;

let xr = size_x () / 2 - 30
and yr = size_y () / 2 - 26
and xg = size_x () / 2 + 30
and yg = size_y () / 2 - 26
and xb = size_x () / 2
and yb = size_y () / 2 + 26
;;

let point x y =
  let dr = (x-xr)*(x-xr) + (y-yr)*(y-yr)
  and dg = (x-xg)*(x-xg) + (y-yg)*(y-yg)
  and db = (x-xb)*(x-xb) + (y-yb)*(y-yb)
  in
  if dr > dg && dr > db then set_color (rgb 255 (255*dg/dr) (255*db/dr))
  else if dg > db then set_color (rgb (255*dr/dg) 255 (255*db/dg))
  else set_color (rgb (255*dr/db) (255*dg/db) 255);
  fill_rect x y 2 2;
;;

for y = (size_y () - 1) / 2 downto 0 do
  for x = 0 to (size_x () - 1) / 2 do
    point (2*x) (2*y);
  done
done
;;

let n = 0x000000
and w = 0xFFFFFF
and b = 0xFFCC99
and y = 0xFFFF00
and o = 0xCC9966
and v = 0x00BB00
and g = 0x888888
and c = 0xDDDDDD
and t = transp
;;

let caml = make_image [|
  [|t;t;t;t;t;t;t;t;t;t;t;n;n;n;n;n;n;t;t;t;t;t;t;t;t;t;t;t;t;t;t;t;|];
  [|t;t;t;t;t;t;t;t;t;t;n;n;n;n;n;n;n;n;n;t;t;t;t;t;t;t;t;t;t;t;t;t;|];
  [|t;t;t;t;t;t;t;t;n;n;n;n;n;n;n;n;n;n;n;n;t;t;t;t;t;t;t;t;t;t;t;t;|];
  [|n;n;n;n;n;n;t;n;n;n;n;n;b;b;b;b;b;b;b;n;n;t;t;t;t;t;n;n;n;n;n;t;|];
  [|n;o;o;o;o;o;n;n;n;n;b;b;b;b;b;b;b;b;b;b;b;n;n;n;n;n;n;n;n;n;n;t;|];
  [|n;o;o;o;o;o;o;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;t;|];
  [|n;o;o;o;o;o;o;o;n;n;n;g;g;g;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;t;t;|];
  [|n;n;o;o;o;o;o;o;o;n;n;n;c;c;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;t;t;|];
  [|t;n;n;o;o;o;o;o;o;o;n;n;n;c;n;n;n;n;n;n;n;b;b;n;n;n;n;n;n;t;t;t;|];
  [|t;t;n;n;n;o;o;o;o;o;o;n;n;n;n;n;n;n;n;n;b;b;b;b;n;n;n;n;t;t;t;t;|];
  [|t;t;t;t;n;n;o;o;o;o;o;o;n;n;n;n;n;n;n;n;b;b;b;b;b;b;n;n;t;t;t;t;|];
  [|t;t;t;t;t;n;n;o;o;o;o;o;o;n;n;n;n;n;n;o;o;b;b;b;b;b;b;n;n;t;t;t;|];
  [|t;t;t;t;t;n;n;o;o;o;o;o;o;b;b;b;b;b;n;n;o;o;b;b;b;b;b;b;n;n;t;t;|];
  [|t;t;t;t;n;n;n;o;o;o;o;o;b;b;b;b;b;b;b;n;n;o;o;b;b;b;b;b;b;n;n;t;|];
  [|t;t;t;t;n;n;n;o;o;o;o;b;b;b;b;b;b;b;b;b;n;n;o;o;b;b;b;b;b;b;n;n;|];
  [|t;t;t;t;n;n;n;o;o;o;o;b;b;b;b;b;n;n;b;b;b;n;n;o;o;b;b;b;b;b;n;n;|];
  [|t;t;t;t;n;n;n;o;o;o;o;b;b;b;b;b;n;n;b;b;b;b;n;n;o;o;b;o;b;b;n;n;|];
  [|t;t;t;t;n;n;n;o;o;o;o;b;b;b;b;b;n;n;b;b;b;b;b;n;n;o;o;o;o;o;n;n;|];
  [|t;t;t;t;n;n;n;o;o;o;o;b;b;b;b;b;n;n;b;b;b;b;b;b;n;n;o;o;o;o;n;n;|];
  [|t;t;t;t;n;n;n;o;o;o;o;o;b;b;b;b;n;n;b;b;b;b;b;b;b;n;n;o;o;n;n;n;|];
  [|t;t;t;t;n;n;n;n;o;o;o;o;o;b;b;b;n;n;n;b;b;b;b;b;b;b;n;n;o;n;b;n;|];
  [|t;t;t;t;t;n;n;n;o;o;o;o;o;o;b;b;n;n;n;b;b;b;b;b;b;b;b;n;n;n;b;n;|];
  [|t;t;t;t;t;t;n;n;o;o;o;o;o;o;o;y;v;y;n;b;b;b;b;b;b;b;b;n;n;b;b;n;|];
  [|t;t;t;t;t;t;t;n;o;o;o;o;o;v;y;o;o;n;n;n;b;b;b;b;b;b;b;n;n;b;b;n;|];
  [|t;t;t;t;t;t;t;n;o;o;o;y;v;o;o;o;o;n;n;n;n;b;b;b;b;b;b;n;n;b;b;n;|];
  [|t;t;t;t;t;t;n;n;o;v;y;o;y;o;o;o;o;o;o;n;n;n;b;b;b;b;b;n;n;b;b;n;|];
  [|t;t;t;t;t;t;n;o;y;y;o;o;v;o;o;o;o;o;o;o;n;n;n;b;b;b;n;n;n;b;n;t;|];
  [|t;t;t;t;t;n;n;v;o;v;o;o;o;o;o;o;o;o;o;o;o;n;n;n;b;n;n;n;n;b;n;t;|];
  [|t;t;t;t;t;n;v;o;o;v;o;o;o;o;o;o;o;o;o;o;o;o;n;n;n;n;n;n;n;n;t;t;|];
  [|t;t;t;t;n;n;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;n;n;n;n;n;n;t;t;t;t;t;|];
  [|t;t;t;t;n;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;n;n;t;t;t;t;t;t;t;t;t;t;|];
  [|t;t;t;t;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;n;t;t;t;t;t;t;t;t;t;t;t;|];
|];;

(*
let x = ref 0 and y = ref 0;;
let bg = get_image !x !y 32 32;;
while true do
  let st = wait_next_event [Mouse_motion; Button_down] in
  if not st.button then draw_image bg !x !y;
  x := st.mouse_x;
  y := st.mouse_y;
  blit_image bg !x !y;
  draw_image caml !x !y;
done;;
*)
set_color (rgb 0 0 0);
remember_mode false;
try while true do
  let st = wait_next_event [Mouse_motion; Button_down; Key_pressed] in
  synchronize ();
  if st.keypressed then raise Exit;
  if st.button then begin
    remember_mode true;
    draw_image caml st.mouse_x st.mouse_y;
    remember_mode false;
  end;
  let x = st.mouse_x + 16 and y = st.mouse_y + 16 in

  moveto 0 y;
  lineto (x - 25) y;
  moveto 10000 y;
  lineto (x + 25) y;

  moveto x 0;
  lineto x (y - 25);
  moveto x 10000;
  lineto x (y + 25);

  draw_image caml st.mouse_x st.mouse_y;
done with Exit -> ()
;;

(* To run this example:
   ********************
   1. Select all the text in this window.
   2. Drag it to the toplevel window.
   3. Watch the colors.
   4. Drag the mouse over the graphics window and click here and there.
   5. Type any key to the graphics window to stop the program.
*)
