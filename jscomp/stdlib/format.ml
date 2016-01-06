(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* A pretty-printing facility and definition of formatters for 'parallel'
   (i.e. unrelated or independent) pretty-printing on multiple out channels. *)

(**************************************************************

  Data structures definitions.

 **************************************************************)

type size;;

external size_of_int : int -> size = "%identity"
;;
external int_of_size : size -> int = "%identity"
;;

(* Tokens are one of the following : *)

type block_type
       = CamlinternalFormatBasics.block_type
       = Pp_hbox | Pp_vbox | Pp_hvbox | Pp_hovbox | Pp_box | Pp_fits

type pp_token =
| Pp_text of string            (* normal text *)
| Pp_break of int * int        (* complete break *)
| Pp_tbreak of int * int       (* go to next tabulation *)
| Pp_stab                      (* set a tabulation *)
| Pp_begin of int * block_type (* beginning of a block *)
| Pp_end                       (* end of a block *)
| Pp_tbegin of tblock          (* beginning of a tabulation block *)
| Pp_tend                      (* end of a tabulation block *)
| Pp_newline                   (* to force a newline inside a block *)
| Pp_if_newline                (* to do something only if this very
                                  line has been broken *)
| Pp_open_tag of tag           (* opening a tag name *)
| Pp_close_tag                 (* closing the most recently opened tag *)

and tag = string

and tblock = Pp_tbox of int list ref  (* Tabulation box *)

(* The Queue:
   contains all formatting elements.
   elements are tuples (size, token, length), where
   size is set when the size of the block is known
   len is the declared length of the token. *)
type pp_queue_elem = {
  mutable elem_size : size;
  token : pp_token;
  length : int;
}
;;

(* Scan stack:
   each element is (left_total, queue element) where left_total
   is the value of pp_left_total when the element has been enqueued. *)
type pp_scan_elem = Scan_elem of int * pp_queue_elem;;

(* Formatting stack:
   used to break the lines while printing tokens.
   The formatting stack contains the description of
   the currently active blocks. *)
type pp_format_elem = Format_elem of block_type * int;;

(* General purpose queues, used in the formatter. *)
type 'a queue_elem =
   | Nil
   | Cons of 'a queue_cell

and 'a queue_cell = {
  mutable head : 'a;
  mutable tail : 'a queue_elem;
}
;;

type 'a queue = {
  mutable insert : 'a queue_elem;
  mutable body : 'a queue_elem;
}
;;

(* The formatter specific tag handling functions. *)
type formatter_tag_functions = {
  mark_open_tag : tag -> string;
  mark_close_tag : tag -> string;
  print_open_tag : tag -> unit;
  print_close_tag : tag -> unit;
}
;;

(* A formatter with all its machinery. *)
type formatter = {
  mutable pp_scan_stack : pp_scan_elem list;
  mutable pp_format_stack : pp_format_elem list;
  mutable pp_tbox_stack : tblock list;
  mutable pp_tag_stack : tag list;
  mutable pp_mark_stack : tag list;
  (* Global variables: default initialization is
     set_margin 78
     set_min_space_left 0. *)
  (* Value of right margin. *)
  mutable pp_margin : int;
  (* Minimal space left before margin, when opening a block. *)
  mutable pp_min_space_left : int;
  (* Maximum value of indentation:
     no blocks can be opened further. *)
  mutable pp_max_indent : int;
  (* Space remaining on the current line. *)
  mutable pp_space_left : int;
  (* Current value of indentation. *)
  mutable pp_current_indent : int;
  (* True when the line has been broken by the pretty-printer. *)
  mutable pp_is_new_line : bool;
  (* Total width of tokens already printed. *)
  mutable pp_left_total : int;
  (* Total width of tokens ever put in queue. *)
  mutable pp_right_total : int;
  (* Current number of opened blocks. *)
  mutable pp_curr_depth : int;
  (* Maximum number of blocks which can be simultaneously opened. *)
  mutable pp_max_boxes : int;
  (* Ellipsis string. *)
  mutable pp_ellipsis : string;
  (* Output function. *)
  mutable pp_out_string : string -> int -> int -> unit;
  (* Flushing function. *)
  mutable pp_out_flush : unit -> unit;
  (* Output of new lines. *)
  mutable pp_out_newline : unit -> unit;
  (* Output of indentation spaces. *)
  mutable pp_out_spaces : int -> unit;
  (* Are tags printed ? *)
  mutable pp_print_tags : bool;
  (* Are tags marked ? *)
  mutable pp_mark_tags : bool;
  (* Find opening and closing markers of tags. *)
  mutable pp_mark_open_tag : tag -> string;
  mutable pp_mark_close_tag : tag -> string;
  mutable pp_print_open_tag : tag -> unit;
  mutable pp_print_close_tag : tag -> unit;
  (* The pretty-printer queue. *)
  mutable pp_queue : pp_queue_elem queue;
}
;;

(**************************************************************

  Auxilliaries and basic functions.

 **************************************************************)


(* Queues auxilliaries. *)
let make_queue () = { insert = Nil; body = Nil; };;

let clear_queue q = q.insert <- Nil; q.body <- Nil;;

let add_queue x q =
  let c = Cons { head = x; tail = Nil; } in
  match q with
  | { insert = Cons cell; body = _; } ->
    q.insert <- c; cell.tail <- c
  (* Invariant: when insert is Nil body should be Nil. *)
  | { insert = Nil; body = _; } ->
    q.insert <- c; q.body <- c
;;

exception Empty_queue;;

let peek_queue = function
  | { body = Cons { head = x; tail = _; }; _ } -> x
  | { body = Nil; insert = _; } -> raise Empty_queue
;;

let take_queue = function
  | { body = Cons { head = x; tail = tl; }; _ } as q ->
    q.body <- tl;
    if tl = Nil then q.insert <- Nil; (* Maintain the invariant. *)
    x
  | { body = Nil; insert = _; } -> raise Empty_queue
;;

(* Enter a token in the pretty-printer queue. *)
let pp_enqueue state ({ length = len; _} as token) =
  state.pp_right_total <- state.pp_right_total + len;
  add_queue token state.pp_queue
;;

let pp_clear_queue state =
  state.pp_left_total <- 1; state.pp_right_total <- 1;
  clear_queue state.pp_queue
;;

(* Pp_infinity: large value for default tokens size.

   Pp_infinity is documented as being greater than 1e10; to avoid
   confusion about the word 'greater', we choose pp_infinity greater
   than 1e10 + 1; for correct handling of tests in the algorithm,
   pp_infinity must be even one more than 1e10 + 1; let's stand on the
   safe side by choosing 1.e10+10.

   Pp_infinity could probably be 1073741823 that is 2^30 - 1, that is
   the minimal upper bound for integers; now that max_int is defined,
   this limit could also be defined as max_int - 1.

   However, before setting pp_infinity to something around max_int, we
   must carefully double-check all the integer arithmetic operations
   that involve pp_infinity, since any overflow would wreck havoc the
   pretty-printing algorithm's invariants. Given that this arithmetic
   correctness check is difficult and error prone and given that 1e10
   + 1 is in practice large enough, there is no need to attempt to set
   pp_infinity to the theoretically maximum limit. It is not worth the
   burden ! *)

let pp_infinity = 1000000010;;

(* Output functions for the formatter. *)
let pp_output_string state s = state.pp_out_string s 0 (String.length s)
and pp_output_newline state = state.pp_out_newline ()
and pp_output_spaces state n = state.pp_out_spaces n

(* To format a break, indenting a new line. *)
let break_new_line state offset width =
  pp_output_newline state;
  state.pp_is_new_line <- true;
  let indent = state.pp_margin - width + offset in
  (* Don't indent more than pp_max_indent. *)
  let real_indent = min state.pp_max_indent indent in
  state.pp_current_indent <- real_indent;
  state.pp_space_left <- state.pp_margin - state.pp_current_indent;
  pp_output_spaces state state.pp_current_indent
;;

(* To force a line break inside a block: no offset is added. *)
let break_line state width = break_new_line state 0 width;;

(* To format a break that fits on the current line. *)
let break_same_line state width =
  state.pp_space_left <- state.pp_space_left - width;
  pp_output_spaces state width
;;

(* To indent no more than pp_max_indent, if one tries to open a block
   beyond pp_max_indent, then the block is rejected on the left
   by simulating a break. *)
let pp_force_break_line state =
  match state.pp_format_stack with
  | Format_elem (bl_ty, width) :: _ ->
    if width > state.pp_space_left then
      (match bl_ty with
       | Pp_fits -> () | Pp_hbox -> ()
       | Pp_vbox | Pp_hvbox | Pp_hovbox | Pp_box ->
         break_line state width)
  | [] -> pp_output_newline state
;;

(* To skip a token, if the previous line has been broken. *)
let pp_skip_token state =
  (* When calling pp_skip_token the queue cannot be empty. *)
  match take_queue state.pp_queue with
  | { elem_size = size; length = len; token = _; } ->
    state.pp_left_total <- state.pp_left_total - len;
    state.pp_space_left <- state.pp_space_left + int_of_size size
;;

(**************************************************************

  The main pretty printing functions.

 **************************************************************)

(* To format a token. *)
let format_pp_token state size = function

  | Pp_text s ->
    state.pp_space_left <- state.pp_space_left - size;
    pp_output_string state s;
    state.pp_is_new_line <- false

  | Pp_begin (off, ty) ->
    let insertion_point = state.pp_margin - state.pp_space_left in
    if insertion_point > state.pp_max_indent then
      (* can't open a block right there. *)
      begin pp_force_break_line state end;
    let offset = state.pp_space_left - off in
    let bl_type =
      begin match ty with
      | Pp_vbox -> Pp_vbox
      | Pp_hbox | Pp_hvbox | Pp_hovbox | Pp_box | Pp_fits ->
        if size > state.pp_space_left then ty else Pp_fits
      end in
    state.pp_format_stack <-
      Format_elem (bl_type, offset) :: state.pp_format_stack

  | Pp_end ->
    begin match state.pp_format_stack with
    | _ :: ls -> state.pp_format_stack <- ls
    | [] -> () (* No more block to close. *)
    end

  | Pp_tbegin (Pp_tbox _ as tbox) ->
    state.pp_tbox_stack <- tbox :: state.pp_tbox_stack

  | Pp_tend ->
    begin match state.pp_tbox_stack with
    | _ :: ls -> state.pp_tbox_stack <- ls
    | [] -> () (* No more tabulation block to close. *)
    end

  | Pp_stab ->
    begin match state.pp_tbox_stack with
    | Pp_tbox tabs :: _ ->
      let rec add_tab n = function
        | [] -> [n]
        | x :: l as ls -> if n < x then n :: ls else x :: add_tab n l in
      tabs := add_tab (state.pp_margin - state.pp_space_left) !tabs
    | [] -> () (* No opened tabulation block. *)
    end

  | Pp_tbreak (n, off) ->
    let insertion_point = state.pp_margin - state.pp_space_left in
    begin match state.pp_tbox_stack with
    | Pp_tbox tabs :: _ ->
      let rec find n = function
        | x :: l -> if x >= n then x else find n l
        | [] -> raise Not_found in
      let tab =
        match !tabs with
        | x :: _ ->
          begin
            try find insertion_point !tabs with
            | Not_found -> x
          end
        | _ -> insertion_point in
      let offset = tab - insertion_point in
      if offset >= 0
      then break_same_line state (offset + n)
      else break_new_line state (tab + off) state.pp_margin
    | [] -> () (* No opened tabulation block. *)
    end

  | Pp_newline ->
    begin match state.pp_format_stack with
    | Format_elem (_, width) :: _ -> break_line state width
    | [] -> pp_output_newline state (* No opened block. *)
    end

  | Pp_if_newline ->
    if state.pp_current_indent != state.pp_margin - state.pp_space_left
    then pp_skip_token state

  | Pp_break (n, off) ->
    begin match state.pp_format_stack with
    | Format_elem (ty, width) :: _ ->
      begin match ty with
      | Pp_hovbox ->
        if size > state.pp_space_left
        then break_new_line state off width
        else break_same_line state n
      | Pp_box ->
        (* Have the line just been broken here ? *)
        if state.pp_is_new_line then break_same_line state n else
        if size > state.pp_space_left
         then break_new_line state off width else
        (* break the line here leads to new indentation ? *)
        if state.pp_current_indent > state.pp_margin - width + off
        then break_new_line state off width
        else break_same_line state n
      | Pp_hvbox -> break_new_line state off width
      | Pp_fits -> break_same_line state n
      | Pp_vbox -> break_new_line state off width
      | Pp_hbox -> break_same_line state n
      end
    | [] -> () (* No opened block. *)
    end

   | Pp_open_tag tag_name ->
     let marker = state.pp_mark_open_tag tag_name in
     pp_output_string state marker;
     state.pp_mark_stack <- tag_name :: state.pp_mark_stack

   | Pp_close_tag ->
     begin match state.pp_mark_stack with
     | tag_name :: tags ->
       let marker = state.pp_mark_close_tag tag_name in
       pp_output_string state marker;
       state.pp_mark_stack <- tags
     | [] -> () (* No more tag to close. *)
     end
;;

(* Print if token size is known or printing is delayed.
   Size is known when not negative.
   Printing is delayed when the text waiting in the queue requires
   more room to format than exists on the current line.

   Note: [advance_loop] must be tail recursive to prevent stack overflows. *)
let rec advance_loop state =
  match peek_queue state.pp_queue with
  | {elem_size = size; token = tok; length = len} ->
    let size = int_of_size size in
    if not
         (size < 0 &&
          (state.pp_right_total - state.pp_left_total < state.pp_space_left))
    then begin
      ignore (take_queue state.pp_queue);
      format_pp_token state (if size < 0 then pp_infinity else size) tok;
      state.pp_left_total <- len + state.pp_left_total;
      advance_loop state
    end
;;

let advance_left state =
  try advance_loop state with
  | Empty_queue -> ()
;;

let enqueue_advance state tok = pp_enqueue state tok; advance_left state;;

(* To enqueue a string : try to advance. *)
let make_queue_elem size tok len =
  { elem_size = size; token = tok; length = len; };;

let enqueue_string_as state size s =
  let len = int_of_size size in
  enqueue_advance state (make_queue_elem size (Pp_text s) len)
;;

let enqueue_string state s =
  let len = String.length s in
  enqueue_string_as state (size_of_int len) s
;;

(* Routines for scan stack
   determine sizes of blocks. *)

(* The scan_stack is never empty. *)
let scan_stack_bottom =
  let q_elem = make_queue_elem (size_of_int (-1)) (Pp_text "") 0 in
  [Scan_elem (-1, q_elem)]
;;

(* Set size of blocks on scan stack:
   if ty = true then size of break is set else size of block is set;
   in each case pp_scan_stack is popped. *)
let clear_scan_stack state = state.pp_scan_stack <- scan_stack_bottom;;

(* Pattern matching on scan stack is exhaustive,
   since scan_stack is never empty.
   Pattern matching on token in scan stack is also exhaustive,
   since scan_push is used on breaks and opening of boxes. *)
let set_size state ty =
  match state.pp_scan_stack with
  | Scan_elem
      (left_tot,
       ({ elem_size = size; token = tok; length = _; } as queue_elem)) :: t ->
    let size = int_of_size size in
    (* test if scan stack contains any data that is not obsolete. *)
    if left_tot < state.pp_left_total then clear_scan_stack state else
      begin match tok with
      | Pp_break (_, _) | Pp_tbreak (_, _) ->
        if ty then
        begin
          queue_elem.elem_size <- size_of_int (state.pp_right_total + size);
          state.pp_scan_stack <- t
        end
      | Pp_begin (_, _) ->
        if not ty then
        begin
          queue_elem.elem_size <- size_of_int (state.pp_right_total + size);
          state.pp_scan_stack <- t
        end
      | Pp_text _ | Pp_stab | Pp_tbegin _ | Pp_tend | Pp_end
      | Pp_newline | Pp_if_newline
      | Pp_open_tag _ | Pp_close_tag ->
        () (* scan_push is only used for breaks and boxes. *)
      end
  | [] -> () (* scan_stack is never empty. *)
;;

(* Push a token on scan stack. If b is true set_size is called. *)
let scan_push state b tok =
  pp_enqueue state tok;
  if b then set_size state true;
  state.pp_scan_stack <-
    Scan_elem (state.pp_right_total, tok) :: state.pp_scan_stack
;;

(* To open a new block :
   the user may set the depth bound pp_max_boxes
   any text nested deeper is printed as the ellipsis string. *)
let pp_open_box_gen state indent br_ty =
  state.pp_curr_depth <- state.pp_curr_depth + 1;
  if state.pp_curr_depth < state.pp_max_boxes then
    let elem =
      make_queue_elem
        (size_of_int (- state.pp_right_total))
        (Pp_begin (indent, br_ty))
        0 in
    scan_push state false elem else
  if state.pp_curr_depth = state.pp_max_boxes
  then enqueue_string state state.pp_ellipsis
;;

(* The box which is always opened. *)
let pp_open_sys_box state = pp_open_box_gen state 0 Pp_hovbox;;

(* Close a block, setting sizes of its sub blocks. *)
let pp_close_box state () =
  if state.pp_curr_depth > 1 then
  begin
    if state.pp_curr_depth < state.pp_max_boxes then
    begin
      pp_enqueue state
        { elem_size = size_of_int 0; token = Pp_end; length = 0; };
      set_size state true; set_size state false
    end;
    state.pp_curr_depth <- state.pp_curr_depth - 1;
  end
;;

(* Open a tag, pushing it on the tag stack. *)
let pp_open_tag state tag_name =
  if state.pp_print_tags then
  begin
    state.pp_tag_stack <- tag_name :: state.pp_tag_stack;
    state.pp_print_open_tag tag_name
  end;
  if state.pp_mark_tags then
    pp_enqueue state {
      elem_size = size_of_int 0;
      token = Pp_open_tag tag_name;
      length = 0;
    }
;;

(* Close a tag, popping it from the tag stack. *)
let pp_close_tag state () =
  if state.pp_mark_tags then
    pp_enqueue state {
      elem_size = size_of_int 0;
      token = Pp_close_tag;
      length = 0;
    };
  if state.pp_print_tags then
  begin
    match state.pp_tag_stack with
    | tag_name :: tags ->
      state.pp_print_close_tag tag_name;
      state.pp_tag_stack <- tags
    | _ -> () (* No more tag to close. *)
  end
;;

let pp_set_print_tags state b = state.pp_print_tags <- b;;
let pp_set_mark_tags state b = state.pp_mark_tags <- b;;
let pp_get_print_tags state () = state.pp_print_tags;;
let pp_get_mark_tags state () = state.pp_mark_tags;;
let pp_set_tags state b = pp_set_print_tags state b; pp_set_mark_tags state b;;

let pp_get_formatter_tag_functions state () = {
  mark_open_tag = state.pp_mark_open_tag;
  mark_close_tag = state.pp_mark_close_tag;
  print_open_tag = state.pp_print_open_tag;
  print_close_tag = state.pp_print_close_tag;
}
;;

let pp_set_formatter_tag_functions state {
     mark_open_tag = mot;
     mark_close_tag = mct;
     print_open_tag = pot;
     print_close_tag = pct;
  } =
   state.pp_mark_open_tag <- mot;
   state.pp_mark_close_tag <- mct;
   state.pp_print_open_tag <- pot;
   state.pp_print_close_tag <- pct
;;

(* Initialize pretty-printer. *)
let pp_rinit state =
  pp_clear_queue state;
  clear_scan_stack state;
  state.pp_format_stack <- [];
  state.pp_tbox_stack <- [];
  state.pp_tag_stack <- [];
  state.pp_mark_stack <- [];
  state.pp_current_indent <- 0;
  state.pp_curr_depth <- 0;
  state.pp_space_left <- state.pp_margin;
  pp_open_sys_box state;;

(* Flushing pretty-printer queue. *)
let pp_flush_queue state b =
  while state.pp_curr_depth > 1 do
    pp_close_box state ()
  done;
  state.pp_right_total <- pp_infinity;
  advance_left state;
  if b then pp_output_newline state;
  pp_rinit state
;;

(**************************************************************

  Procedures to format objects, and use boxes

 **************************************************************)

(* To format a string. *)
let pp_print_as_size state size s =
  if state.pp_curr_depth < state.pp_max_boxes
  then enqueue_string_as state size s
;;

let pp_print_as state isize s =
  pp_print_as_size state (size_of_int isize) s
;;

let pp_print_string state s =
  pp_print_as state (String.length s) s
;;

(* To format an integer. *)
let pp_print_int state i = pp_print_string state (string_of_int i);;

(* To format a float. *)
let pp_print_float state f = pp_print_string state (string_of_float f);;

(* To format a boolean. *)
let pp_print_bool state b = pp_print_string state (string_of_bool b);;

(* To format a char. *)
let pp_print_char state c =
  pp_print_as state 1 (String.make 1 c)
;;

(* Opening boxes. *)
let pp_open_hbox state () = pp_open_box_gen state 0 Pp_hbox
and pp_open_vbox state indent = pp_open_box_gen state indent Pp_vbox

and pp_open_hvbox state indent = pp_open_box_gen state indent Pp_hvbox
and pp_open_hovbox state indent = pp_open_box_gen state indent Pp_hovbox
and pp_open_box state indent = pp_open_box_gen state indent Pp_box;;

(* Print a new line after printing all queued text
   (same for print_flush but without a newline). *)
let pp_print_newline state () =
  pp_flush_queue state true; state.pp_out_flush ()
and pp_print_flush state () =
  pp_flush_queue state false; state.pp_out_flush ();;

(* To get a newline when one does not want to close the current block. *)
let pp_force_newline state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state (make_queue_elem (size_of_int 0) Pp_newline 0)
;;

(* To format something if the line has just been broken. *)
let pp_print_if_newline state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state (make_queue_elem (size_of_int 0) Pp_if_newline 0)
;;

(* Breaks: indicate where a block may be broken.
   If line is broken then offset is added to the indentation of the current
   block else (the value of) width blanks are printed.
   To do (?) : add a maximum width and offset value. *)
let pp_print_break state width offset =
  if state.pp_curr_depth < state.pp_max_boxes then
    let elem =
      make_queue_elem
        (size_of_int (- state.pp_right_total))
        (Pp_break (width, offset))
        width in
    scan_push state true elem
;;

let pp_print_space state () = pp_print_break state 1 0
and pp_print_cut state () = pp_print_break state 0 0
;;

(* Tabulation boxes. *)
let pp_open_tbox state () =
  state.pp_curr_depth <- state.pp_curr_depth + 1;
  if state.pp_curr_depth < state.pp_max_boxes then
    let elem =
      make_queue_elem (size_of_int 0) (Pp_tbegin (Pp_tbox (ref []))) 0 in
    enqueue_advance state elem
;;

(* Close a tabulation block. *)
let pp_close_tbox state () =
  if state.pp_curr_depth > 1 then
  begin
   if state.pp_curr_depth < state.pp_max_boxes then
     let elem = make_queue_elem (size_of_int 0) Pp_tend 0 in
     enqueue_advance state elem;
     state.pp_curr_depth <- state.pp_curr_depth - 1
  end
;;

(* Print a tabulation break. *)
let pp_print_tbreak state width offset =
  if state.pp_curr_depth < state.pp_max_boxes then
    let elem =
      make_queue_elem
        (size_of_int (- state.pp_right_total))
        (Pp_tbreak (width, offset))
        width in
    scan_push state true elem
;;

let pp_print_tab state () = pp_print_tbreak state 0 0;;

let pp_set_tab state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    let elem =
      make_queue_elem (size_of_int 0) Pp_stab 0 in
    enqueue_advance state elem
;;


(* Convenience functions *)

(* To format a list *)
let rec pp_print_list ?(pp_sep = pp_print_cut) pp_v ppf = function
  | [] -> ()
  | [v] -> pp_v ppf v
  | v :: vs ->
    pp_v ppf v;
    pp_sep ppf ();
    pp_print_list ~pp_sep pp_v ppf vs

(* To format free-flowing text *)
let pp_print_text ppf s =
  let len = String.length s in
  let left = ref 0 in
  let right = ref 0 in
  let flush () =
    pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    match s.[!right] with
      | '\n' ->
        flush ();
        pp_force_newline ppf ()
      | ' ' ->
        flush (); pp_print_space ppf ()
      (* there is no specific support for '\t'
         as it is unclear what a right semantics would be *)
      | _ -> incr right
  done;
  if !left <> len then flush ()


(**************************************************************

  Procedures to control the pretty-printers

 **************************************************************)

(* Fit max_boxes. *)
let pp_set_max_boxes state n = if n > 1 then state.pp_max_boxes <- n;;

(* To know the current maximum number of boxes allowed. *)
let pp_get_max_boxes state () = state.pp_max_boxes;;

let pp_over_max_boxes state () = state.pp_curr_depth = state.pp_max_boxes;;

(* Ellipsis. *)
let pp_set_ellipsis_text state s = state.pp_ellipsis <- s
and pp_get_ellipsis_text state () = state.pp_ellipsis
;;

(* To set the margin of pretty-printer. *)
let pp_limit n =
  if n < pp_infinity then n else pred pp_infinity
;;

let pp_set_min_space_left state n =
  if n >= 1 then
    let n = pp_limit n in
    state.pp_min_space_left <- n;
    state.pp_max_indent <- state.pp_margin - state.pp_min_space_left;
    pp_rinit state
;;

(* Initially, we have :
  pp_max_indent = pp_margin - pp_min_space_left, and
  pp_space_left = pp_margin. *)
let pp_set_max_indent state n =
  pp_set_min_space_left state (state.pp_margin - n)
;;
let pp_get_max_indent state () = state.pp_max_indent;;

let pp_set_margin state n =
  if n >= 1 then
    let n = pp_limit n in
    state.pp_margin <- n;
    let new_max_indent =
      (* Try to maintain max_indent to its actual value. *)
      if state.pp_max_indent <= state.pp_margin
      then state.pp_max_indent else
      (* If possible maintain pp_min_space_left to its actual value,
         if this leads to a too small max_indent, take half of the
         new margin, if it is greater than 1. *)
       max (max (state.pp_margin - state.pp_min_space_left)
                (state.pp_margin / 2)) 1 in
    (* Rebuild invariants. *)
    pp_set_max_indent state new_max_indent
;;

let pp_get_margin state () = state.pp_margin;;

type formatter_out_functions = {
  out_string : string -> int -> int -> unit;
  out_flush : unit -> unit;
  out_newline : unit -> unit;
  out_spaces : int -> unit;
}
;;

let pp_set_formatter_out_functions state {
      out_string = f;
      out_flush = g;
      out_newline = h;
      out_spaces = i;
    } =
  state.pp_out_string <- f;
  state.pp_out_flush <- g;
  state.pp_out_newline <- h;
  state.pp_out_spaces <- i;
;;

let pp_get_formatter_out_functions state () = {
  out_string = state.pp_out_string;
  out_flush = state.pp_out_flush;
  out_newline = state.pp_out_newline;
  out_spaces = state.pp_out_spaces;
}
;;

let pp_set_formatter_output_functions state f g =
  state.pp_out_string <- f; state.pp_out_flush <- g;;
let pp_get_formatter_output_functions state () =
  (state.pp_out_string, state.pp_out_flush)
;;

let pp_set_all_formatter_output_functions state
    ~out:f ~flush:g ~newline:h ~spaces:i =
  pp_set_formatter_output_functions state f g;
  state.pp_out_newline <- h;
  state.pp_out_spaces <- i;
;;
let pp_get_all_formatter_output_functions state () =
  (state.pp_out_string, state.pp_out_flush,
   state.pp_out_newline, state.pp_out_spaces)
;;

(* Default function to output new lines. *)
let display_newline state () = state.pp_out_string "\n" 0  1;;

(* Default function to output spaces. *)
let blank_line = String.make 80 ' ';;
let rec display_blanks state n =
  if n > 0 then
  if n <= 80 then state.pp_out_string blank_line 0 n else
  begin
    state.pp_out_string blank_line 0 80;
    display_blanks state (n - 80)
  end
;;

let pp_set_formatter_out_channel state os =
  state.pp_out_string <- output_substring os;
  state.pp_out_flush <- (fun () -> flush os);
  state.pp_out_newline <- display_newline state;
  state.pp_out_spaces <- display_blanks state;
;;

(**************************************************************

  Creation of specific formatters

 **************************************************************)

let default_pp_mark_open_tag s = "<" ^ s ^ ">";;
let default_pp_mark_close_tag s = "</" ^ s ^ ">";;

let default_pp_print_open_tag = ignore;;
let default_pp_print_close_tag = ignore;;

let pp_make_formatter f g h i =
  (* The initial state of the formatter contains a dummy box. *)
  let pp_q = make_queue () in
  let sys_tok =
    make_queue_elem (size_of_int (-1)) (Pp_begin (0, Pp_hovbox)) 0 in
  add_queue sys_tok pp_q;
  let sys_scan_stack =
      (Scan_elem (1, sys_tok)) :: scan_stack_bottom in
  {
   pp_scan_stack = sys_scan_stack;
   pp_format_stack = [];
   pp_tbox_stack = [];
   pp_tag_stack = [];
   pp_mark_stack = [];
   pp_margin = 78;
   pp_min_space_left = 10;
   pp_max_indent = 78 - 10;
   pp_space_left = 78;
   pp_current_indent = 0;
   pp_is_new_line = true;
   pp_left_total = 1;
   pp_right_total = 1;
   pp_curr_depth = 1;
   pp_max_boxes = max_int;
   pp_ellipsis = ".";
   pp_out_string = f;
   pp_out_flush = g;
   pp_out_newline = h;
   pp_out_spaces = i;
   pp_print_tags = false;
   pp_mark_tags = false;
   pp_mark_open_tag = default_pp_mark_open_tag;
   pp_mark_close_tag = default_pp_mark_close_tag;
   pp_print_open_tag = default_pp_print_open_tag;
   pp_print_close_tag = default_pp_print_close_tag;
   pp_queue = pp_q;
  }
;;

(* Make a formatter with default functions to output spaces and new lines. *)
let make_formatter output flush =
  let ppf = pp_make_formatter output flush ignore ignore in
  ppf.pp_out_newline <- display_newline ppf;
  ppf.pp_out_spaces <- display_blanks ppf;
  ppf
;;

let formatter_of_out_channel oc =
  make_formatter (output_substring oc) (fun () -> flush oc)
;;

let formatter_of_buffer b =
  make_formatter (Buffer.add_substring b) ignore
;;

let stdbuf = Buffer.create 512;;

(* Predefined formatters. *)
let std_formatter = formatter_of_out_channel Pervasives.stdout
and err_formatter = formatter_of_out_channel Pervasives.stderr
and str_formatter = formatter_of_buffer stdbuf
;;

let flush_str_formatter () =
  pp_flush_queue str_formatter false;
  let s = Buffer.contents stdbuf in
  Buffer.reset stdbuf;
  s
;;

let flush_buf_formatter buf ppf =
  pp_flush_queue ppf false;
  let s = Buffer.contents buf in
  Buffer.reset buf;
  s

(**************************************************************

  Basic functions on the standard formatter

 **************************************************************)

let open_hbox = pp_open_hbox std_formatter
and open_vbox = pp_open_vbox std_formatter
and open_hvbox = pp_open_hvbox std_formatter
and open_hovbox = pp_open_hovbox std_formatter
and open_box = pp_open_box std_formatter
and close_box = pp_close_box std_formatter
and open_tag = pp_open_tag std_formatter
and close_tag = pp_close_tag std_formatter
and print_as = pp_print_as std_formatter
and print_string = pp_print_string std_formatter
and print_int = pp_print_int std_formatter
and print_float = pp_print_float std_formatter
and print_char = pp_print_char std_formatter
and print_bool = pp_print_bool std_formatter
and print_break = pp_print_break std_formatter
and print_cut = pp_print_cut std_formatter
and print_space = pp_print_space std_formatter
and force_newline = pp_force_newline std_formatter
and print_flush = pp_print_flush std_formatter
and print_newline = pp_print_newline std_formatter
and print_if_newline = pp_print_if_newline std_formatter

and open_tbox = pp_open_tbox std_formatter
and close_tbox = pp_close_tbox std_formatter
and print_tbreak = pp_print_tbreak std_formatter

and set_tab = pp_set_tab std_formatter
and print_tab = pp_print_tab std_formatter

and set_margin = pp_set_margin std_formatter
and get_margin = pp_get_margin std_formatter

and set_max_indent = pp_set_max_indent std_formatter
and get_max_indent = pp_get_max_indent std_formatter

and set_max_boxes = pp_set_max_boxes std_formatter
and get_max_boxes = pp_get_max_boxes std_formatter
and over_max_boxes = pp_over_max_boxes std_formatter

and set_ellipsis_text = pp_set_ellipsis_text std_formatter
and get_ellipsis_text = pp_get_ellipsis_text std_formatter

and set_formatter_out_channel =
  pp_set_formatter_out_channel std_formatter

and set_formatter_out_functions =
  pp_set_formatter_out_functions std_formatter
and get_formatter_out_functions =
  pp_get_formatter_out_functions std_formatter

and set_formatter_output_functions =
  pp_set_formatter_output_functions std_formatter
and get_formatter_output_functions =
  pp_get_formatter_output_functions std_formatter

and set_all_formatter_output_functions =
  pp_set_all_formatter_output_functions std_formatter
and get_all_formatter_output_functions =
  pp_get_all_formatter_output_functions std_formatter

and set_formatter_tag_functions =
  pp_set_formatter_tag_functions std_formatter
and get_formatter_tag_functions =
  pp_get_formatter_tag_functions std_formatter
and set_print_tags =
  pp_set_print_tags std_formatter
and get_print_tags =
  pp_get_print_tags std_formatter
and set_mark_tags =
  pp_set_mark_tags std_formatter
and get_mark_tags =
  pp_get_mark_tags std_formatter
and set_tags =
  pp_set_tags std_formatter
;;

 (**************************************************************)

let compute_tag output tag_acc =
  let buf = Buffer.create 16 in
  let ppf = formatter_of_buffer buf in
  let () = output ppf tag_acc in
  let () = pp_print_flush ppf () in
  let len = Buffer.length buf in
  if len < 2 then Buffer.contents buf
  else Buffer.sub buf 1 (len - 2)

 (**************************************************************

  Defining continuations to be passed as arguments of
  CamlinternalFormat.make_printf.

  **************************************************************)

open CamlinternalFormatBasics
open CamlinternalFormat

(* Interpret a formatting entity on a formatter. *)
let output_formatting_lit ppf fmting_lit = match fmting_lit with
  | Close_box                 -> pp_close_box ppf ()
  | Close_tag                 -> pp_close_tag ppf ()
  | Break (_, width, offset)  -> pp_print_break ppf width offset
  | FFlush                    -> pp_print_flush ppf ()
  | Force_newline             -> pp_force_newline ppf ()
  | Flush_newline             -> pp_print_newline ppf ()
  | Magic_size (_, _)         -> ()
  | Escaped_at                -> pp_print_char ppf '@'
  | Escaped_percent           -> pp_print_char ppf '%'
  | Scan_indic c              -> pp_print_char ppf '@'; pp_print_char ppf c

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in an output_stream. *)
(* Differ from Printf.output_acc by the interpretation of formatting. *)
(* Used as a continuation of CamlinternalFormat.make_printf. *)
let rec output_acc ppf acc = match acc with
  | Acc_string_literal (Acc_formatting_lit (p, Magic_size (_, size)), s)
  | Acc_data_string (Acc_formatting_lit (p, Magic_size (_, size)), s) ->
    output_acc ppf p;
    pp_print_as_size ppf (size_of_int size) s;
  | Acc_char_literal (Acc_formatting_lit (p, Magic_size (_, size)), c)
  | Acc_data_char (Acc_formatting_lit (p, Magic_size (_, size)), c) ->
    output_acc ppf p;
    pp_print_as_size ppf (size_of_int size) (String.make 1 c);
  | Acc_formatting_lit (p, f) ->
    output_acc ppf p;
    output_formatting_lit ppf f;
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
    output_acc ppf p;
    pp_open_tag ppf (compute_tag output_acc acc')
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    let () = output_acc ppf p in
    let (indent, bty) = open_box_of_string (compute_tag output_acc acc') in
    pp_open_box_gen ppf indent bty
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   -> output_acc ppf p; pp_print_string ppf s;
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> output_acc ppf p; pp_print_char ppf c;
  | Acc_delay (p, f)         -> output_acc ppf p; f ppf;
  | Acc_flush p              -> output_acc ppf p; pp_print_flush ppf ();
  | Acc_invalid_arg (p, msg) -> output_acc ppf p; invalid_arg msg;
  | End_of_acc               -> ()

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in a buffer. *)
(* Differ from Printf.bufput_acc by the interpretation of formatting. *)
(* Used as a continuation of CamlinternalFormat.make_printf. *)
let rec strput_acc ppf acc = match acc with
  | Acc_string_literal (Acc_formatting_lit (p, Magic_size (_, size)), s)
  | Acc_data_string (Acc_formatting_lit (p, Magic_size (_, size)), s) ->
    strput_acc ppf p;
    pp_print_as_size ppf (size_of_int size) s;
  | Acc_char_literal (Acc_formatting_lit (p, Magic_size (_, size)), c)
  | Acc_data_char (Acc_formatting_lit (p, Magic_size (_, size)), c) ->
    strput_acc ppf p;
    pp_print_as_size ppf (size_of_int size) (String.make 1 c);
  | Acc_delay (Acc_formatting_lit (p, Magic_size (_, size)), f) ->
    strput_acc ppf p;
    pp_print_as_size ppf (size_of_int size) (f ());
  | Acc_formatting_lit (p, f) ->
    strput_acc ppf p;
    output_formatting_lit ppf f;
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
    strput_acc ppf p;
    pp_open_tag ppf (compute_tag strput_acc acc')
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    let () = strput_acc ppf p in
    let (indent, bty) = open_box_of_string (compute_tag strput_acc acc') in
    pp_open_box_gen ppf indent bty
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   -> strput_acc ppf p; pp_print_string ppf s;
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> strput_acc ppf p; pp_print_char ppf c;
  | Acc_delay (p, f)         -> strput_acc ppf p; pp_print_string ppf (f ());
  | Acc_flush p              -> strput_acc ppf p; pp_print_flush ppf ();
  | Acc_invalid_arg (p, msg) -> strput_acc ppf p; invalid_arg msg;
  | End_of_acc               -> ()

(**************************************************************

  Defining [fprintf] and various flavors of [fprintf].

 **************************************************************)

let kfprintf k o (Format (fmt, _)) =
  make_printf (fun o acc -> output_acc o acc; k o) o End_of_acc fmt
let ikfprintf k x (Format (fmt, _)) =
  make_printf (fun _ _ -> k x) x End_of_acc fmt

let fprintf ppf fmt = kfprintf ignore ppf fmt
let ifprintf ppf fmt = ikfprintf ignore ppf fmt
let printf fmt = fprintf std_formatter fmt
let eprintf fmt = fprintf err_formatter fmt

let ksprintf k (Format (fmt, _)) =
  let b = Buffer.create 512 in
  let ppf = formatter_of_buffer b in
  let k' () acc =
    strput_acc ppf acc;
    k (flush_buf_formatter b ppf) in
  make_printf k' () End_of_acc fmt

let sprintf fmt =
  ksprintf (fun s -> s) fmt

let asprintf (Format (fmt, _)) =
  let b = Buffer.create 512 in
  let ppf = formatter_of_buffer b in
  let k' : (formatter -> (formatter, unit) acc -> string)
    = fun ppf acc ->
      output_acc ppf acc;
      pp_flush_queue ppf false;
      flush_buf_formatter b ppf in
  make_printf k' ppf End_of_acc fmt

(**************************************************************

  Deprecated stuff.

 **************************************************************)

(* Deprecated error prone function bprintf. *)
let bprintf b (Format (fmt, _) : ('a, formatter, unit) format) =
  let k ppf acc = output_acc ppf acc; pp_flush_queue ppf false in
  make_printf k (formatter_of_buffer b) End_of_acc fmt

(* Deprecated alias for ksprintf. *)
let kprintf = ksprintf;;

(* Output everything left in the pretty printer queue at end of execution. *)
at_exit print_flush
;;
