(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis, projet Cristal, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* graph_test.ml : tests various drawing and filling primitives of the
   Graphics library. *)

(* To run this example just load this file into a suitable toplevel.
   Alternatively execute
   ocamlc graphics.cma graph_test.ml *)

open Graphics;;

auto_synchronize false;;
display_mode false;;
remember_mode true;;

let sz = 450;;

open_graph (Printf.sprintf " %ix%i" sz sz);;

(* To be defined for older versions of OCaml
   Lineto, moveto and draw_rect.

let rlineto x y =
 let xc, yc = current_point () in
 lineto (x + xc) (y + yc);;

let rmoveto x y =
 let xc, yc = current_point () in
 moveto (x + xc) (y + yc);;

let draw_rect x y w h =
 let x0, y0 = current_point () in
 moveto x y;
 rlineto w 0;
 rlineto 0 h;
 rlineto (- w) 0;
 rlineto 0 (-h);
 moveto x0 y0;;
*)

(* A set of points. *)

set_color foreground;;

let dashes y =
 for i = 1 to 100 do
  plot y (2 * i);
  plot y (3 * i);
  plot y (4 * i);
 done;;

dashes 3;;

set_line_width 20;;
dashes (sz - 20);;

(* Drawing chars *)

draw_char 'C';
draw_char 'a';
draw_char 'm';
draw_char 'l';;

(* More and more red enlarging squares *)
moveto 10 10;;
set_line_width 5;;

let carre c =
 rlineto 0 c;
 rlineto c 0;
 rlineto 0 (- c);
 rlineto (- c) 0;;

for i = 1 to 10 do
 moveto (10 * i) (10 * i);
 set_color (rgb (155 + 10 * i) 0 0);
 carre (10 * i)
done;;

(* Blue squares in arithmetic progression *)
moveto 10 210;;
set_color blue;;
set_line_width 1;;

for i = 1 to 10 do
 carre (10 * i)
done;;

(* Tiny circles filled or not *)
rmoveto 0 120;;
(* Must not change the current point *)
fill_circle 20 190 10;;
set_color green;;
rlineto 0 10;;
rmoveto 50 10;;
let x, y = current_point () in
(* Must not change the current point *)
draw_circle x y 20;;
set_color black;;
rlineto 0 20;;

(* Cyan rectangles as a kind of graphical representation *)
set_color cyan;;

let lw = 15;;
set_line_width lw;;
let go_caption l = moveto 210 (130 - lw + l);;
let go_legend () = go_caption (- 3 * lw);;

go_caption 0;;
fill_rect 210 130 5 10;;
fill_rect 220 130 10 20;;
fill_rect 235 130 15 40;;
fill_rect 255 130 20 80;;
fill_rect 280 130 25 160;;
(* A green rectangle below the graph. *)
set_color green;;
rlineto 50 0;;

(* A black frame for each of our rectangles *)
set_color black;;
set_line_width (lw / 4);;

draw_rect 210 130 5 10;;
draw_rect 220 130 10 20;;
draw_rect 235 130 15 40;;
draw_rect 255 130 20 80;;
draw_rect 280 130 25 160;;

(* A black rectangle after the green one, below the graph. *)
set_line_width lw;;
rlineto 50 0;;

(* Write a text in yellow on a blue background. *)
(* x = 210, y = 70 *)
go_legend ();;
set_text_size 10;;
set_color (rgb 150 100 250);;
let x,y = current_point () in
fill_rect x (y - 5) (8 * 20) 25;;
set_color yellow;;
go_legend ();;
draw_string "Graphics (OCaml)";;

(* Pie parts in different colors. *)
let draw_green_string s = set_color green; draw_string s;;
let draw_red_string s = set_color red; draw_string s;;

moveto 120 210;;
set_color red;;
fill_arc 150 260 25 25 60 300;
draw_green_string "A ";
draw_red_string "red";
draw_green_string " pie.";

set_text_size 5;
moveto 180 240;
draw_red_string "A "; draw_green_string "green"; draw_red_string " slice.";;
set_color green;
fill_arc 200 260 25 25 0 60;
set_color black;
set_line_width 2;
draw_arc 200 260 27 27 0 60;;

(* Should do nothing since this is a line *)
set_color red;;
fill_poly [| (40, 10); (150, 70); (150, 10); (40, 10) |];;
set_color blue;;

(* Drawing polygones. *)
(* Redefining the draw_poly primitive for the usual library. *)
let draw_poly v =
 let l = Array.length v in
  if l > 0 then begin
  let x0, y0 = current_point () in
  let p0 = v.(0) in
  let x, y = p0 in moveto x y;
  for i = 1 to l - 1 do
   let x, y = v.(i) in lineto x y
  done;
  lineto x y;
  moveto x0 y0
 end;;

draw_poly [| (150, 10); (150, 70); (260, 10); (150, 10) |];;

(* Filling polygones. *)
(* Two equilateral triangles, one red and one blue, and their inside
   filled in black. *)
let equi x y l =
 [| (x - l / 2, y);
    (x, y + int_of_float (float_of_int l *. (sqrt 3.0 /. 2.0)));
    (x + l / 2, y) |];;

set_color black;;
fill_poly (Array.append (equi 300 20 40) (equi 300 44 (- 40)));;

set_line_width 1;;
set_color cyan;;
draw_poly (equi 300 20 40);;
set_color red;;
draw_poly (equi 300 44 (- 40));;

(* Drawing and filling ellipses. *)
let x, y = current_point () in
rlineto 10 10; moveto x y;

moveto 395 100;;

let x, y = current_point () in
fill_ellipse x y 25 15;;

set_color (rgb 0xFF 0x00 0xFF);;
rmoveto 0 (- 50);;

let x, y = current_point () in
fill_ellipse x y 15 30;;

rmoveto (- 45) 0;;
let x, y = current_point () in
draw_ellipse x y 25 10;;

(* Drawing and filling arcs. *)

let draw_arc_ellipse x y r1 r2 =
  set_color green;
  draw_arc x y r1 r2 60 120;
  set_color black;
  draw_arc x y r1 r2 120 420;;

set_line_width 3;;

let draw_arc_ellipses x y r1 r2 =
  let step = 5 in
  for i = 0 to (r1 - step) / (2 * step) do
   for j = 0 to (r2 - step) / (2 * step) do
    draw_arc_ellipse x y (3 * i * step) (3 * j * step)
   done
  done;;

draw_arc_ellipses 20 128 15 50;;

let fill_arc_ellipse x y r1 r2 c1 c2 =
  set_color c1;
  fill_arc x y r1 r2 60 120;
  set_color c2;
  fill_arc x y r1 r2 120 420;;

let fill_arc_ellipses x y r1 r2 =
  let step = 3 in
  let c1 = ref black
  and c2 = ref yellow in
  let exchange r1 r2 = let tmp = !r1 in r1 := !r2; r2 := tmp in
  for i = r1 / (2 * step) downto 10 do
   for j = r2 / (2 * step) downto 30 do
    exchange c1 c2;
    fill_arc_ellipse x y (3 * i) (3 * j) !c1 !c2
   done
  done;;

fill_arc_ellipses 400 240 150 200;;


synchronize ();;

(* transparent color drawing *)
set_color transp;;
draw_circle 400 240 50;;
draw_circle 400 240 40;;
draw_circle 400 240 30;;
(* try to go back a normal color *)
set_color red;;
draw_circle 400 240 20;;

synchronize ();;

ignore (wait_next_event [Key_pressed])
