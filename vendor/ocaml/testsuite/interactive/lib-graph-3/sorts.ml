(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Animation of sorting algorithms. *)

open Graphics

(* Information on a given sorting process *)

type graphic_context =
  { array: int array;                   (* Data to sort *)
    x0: int;                            (* X coordinate, lower left corner *)
    y0: int;                            (* Y coordinate, lower left corner *)
    width: int;                         (* Width in pixels *)
    height: int;                        (* Height in pixels *)
    nelts: int;                         (* Number of elements in the array *)
    maxval: int;                        (* Max val in the array + 1 *)
    rad: int                            (* Dimension of the rectangles *)
  }

(* Array assignment and exchange with screen update *)

let screen_mutex = Mutex.create()

let draw gc i v =
  fill_rect (gc.x0 + (gc.width * i) / gc.nelts)
            (gc.y0 + (gc.height * v) / gc.maxval)
            gc.rad gc.rad

let assign gc i v =
  Mutex.lock screen_mutex;
  set_color background; draw gc i gc.array.(i);
  set_color foreground; draw gc i v;
  gc.array.(i) <- v;
  Mutex.unlock screen_mutex

let exchange gc i j =
  let val_i = gc.array.(i) in
  assign gc i gc.array.(j);
  assign gc j val_i

(* Construction of a graphic context *)

let initialize name array maxval x y w h =
  let (_, label_height) = text_size name in
  let rad = (w - 2) / (Array.length array) - 1 in
  let gc =
    { array = Array.copy array;
      x0 = x + 1;                       (* Leave one pixel left for Y axis *)
      y0 = y + 1;                       (* Leave one pixel below for X axis *)
      width = w - 2;                    (* 1 pixel left, 1 pixel right *)
      height = h - 1 - label_height - rad;
      nelts = Array.length array;
      maxval = maxval;
      rad = rad } in
  moveto (gc.x0 - 1) (gc.y0 + gc.height);
  lineto (gc.x0 - 1) (gc.y0 - 1);
  lineto (gc.x0 + gc.width) (gc.y0 - 1);
  moveto (gc.x0 - 1) (gc.y0 + gc.height);
  draw_string name;
  for i = 0 to Array.length array - 1 do
    draw gc i array.(i)
  done;
  gc

(* Main animation function *)

let display functs nelts maxval =
  let a = Array.make nelts 0 in
  for i = 0 to nelts - 1 do
    a.(i) <- Random.int maxval
  done;
  let num_finished = ref 0 in
  let lock_finished = Mutex.create() in
  let cond_finished = Condition.create() in
  for i = 0 to Array.length functs - 1 do
    let (name, funct, x, y, w, h) = functs.(i) in
    let gc = initialize name a maxval x y w h in
    Thread.create
      (fun () ->
        funct gc;
        Mutex.lock lock_finished;
        incr num_finished;
        Mutex.unlock lock_finished;
        Condition.signal cond_finished)
      ()
  done;
  Mutex.lock lock_finished;
  while !num_finished < Array.length functs do
    Condition.wait cond_finished lock_finished
  done;
  Mutex.unlock lock_finished;
  read_key()

(*****
  let delay = ref 0 in
  try
    while true do
      let gc = Queue.take q in
        begin match gc.action with
          Finished -> ()
        | Pause f ->
            gc.action <- f ();
            for i = 0 to !delay do () done;
            Queue.add gc q
        end;
      if key_pressed() then begin
        match read_key() with
          'q'|'Q' ->
            raise Exit
        | '0'..'9' as c ->
            delay := (Char.code c - 48) * 500
        | _ ->
            ()
      end
    done
  with Exit -> ()
     | Queue.Empty -> read_key(); ()
*****)

(* The sorting functions. *)

(* Bubble sort *)

let bubble_sort gc =
  let ordered = ref false in
  while not !ordered do
    ordered := true;
    for i = 0 to Array.length gc.array - 2 do
      if gc.array.(i+1) < gc.array.(i) then begin
        exchange gc i (i+1);
        ordered := false
      end
    done
  done

(* Insertion sort *)

let insertion_sort gc =
  for i = 1 to Array.length gc.array - 1 do
    let val_i = gc.array.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && val_i < gc.array.(!j) do
      assign gc (!j + 1) gc.array.(!j);
      decr j
    done;
    assign gc (!j + 1) val_i
  done

(* Selection sort *)

let selection_sort gc =
  for i = 0 to Array.length gc.array - 1 do
    let min = ref i in
    for j = i+1 to Array.length gc.array - 1 do
      if gc.array.(j) < gc.array.(!min) then min := j
    done;
    exchange gc i !min
  done

(* Quick sort *)

let quick_sort gc =
  let rec quick lo hi =
    if lo < hi then begin
      let i = ref lo in
      let j = ref hi in
      let pivot = gc.array.(hi) in
      while !i < !j do
        while !i < hi && gc.array.(!i) <= pivot do incr i done;
        while !j > lo && gc.array.(!j) >= pivot do decr j done;
        if !i < !j then exchange gc !i !j
      done;
      exchange gc !i hi;
      quick lo (!i-1);
      quick (!i+1) hi
    end
  in quick 0 (Array.length gc.array - 1)

(* Merge sort *)

let merge_sort gc =
  let rec merge i l1 l2 =
    match (l1, l2) with
      ([], []) ->
        ()
    | ([], v2::r2) ->
        assign gc i v2; merge (i+1) l1 r2
    | (v1::r1, []) ->
        assign gc i v1; merge (i+1) r1 l2
    | (v1::r1, v2::r2) ->
        if v1 < v2
        then begin assign gc i v1; merge (i+1) r1 l2 end
        else begin assign gc i v2; merge (i+1) l1 r2 end in
  let rec msort start len =
    if len < 2 then () else begin
      let m = len / 2 in
      msort start m;
      msort (start+m) (len-m);
      merge start
            (Array.to_list (Array.sub gc.array start m))
            (Array.to_list (Array.sub gc.array (start+m) (len-m)))
    end in
  msort 0 (Array.length gc.array)

(* Main program *)

let animate() =
  open_graph "";
  moveto 0 0; draw_string "Press a key to start...";
  let seed = ref 0 in
  while not (key_pressed()) do incr seed done;
  read_key();
  Random.init !seed;
  clear_graph();
  let prompt = "0: fastest ... 9: slowest, press 'q' to quit" in
  moveto 0 0; draw_string prompt;
  let (_, h) = text_size prompt in
  let sx = size_x() / 2 and sy = (size_y() - h) / 3 in
  display [| "Bubble", bubble_sort, 0, h, sx, sy;
             "Insertion", insertion_sort, 0, h+sy, sx, sy;
             "Selection", selection_sort, 0, h+2*sy, sx, sy;
             "Quicksort", quick_sort, sx, h, sx, sy;
             (** "Heapsort", heap_sort, sx, h+sy, sx, sy; **)
             "Mergesort", merge_sort, sx, h+2*sy, sx, sy |]
          100 1000;
  close_graph()

let _ = if !Sys.interactive then () else begin animate(); exit 0 end

;;
