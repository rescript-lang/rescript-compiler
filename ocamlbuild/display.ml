(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Berke Durak *)
(* Display *)
open My_std;;

open My_unix;;

let fp = Printf.fprintf;;

(*** ANSI *)
module ANSI =
  struct
    let up oc n = fp oc "\027[%dA" n;;
    let clear_to_eol oc () = fp oc "\027[K";;
    let bol oc () = fp oc "\r";;
    let get_columns () =
      if Sys.os_type = "Unix" then
        try
          int_of_string (String.chomp (My_unix.run_and_read "tput cols"))
        with
        | Failure _ -> 80
      else 80
  end
;;
(* ***)
(*** tagline_description *)
type tagline_description = (string * char) list;;
(* ***)
(*** sophisticated_display *)
type sophisticated_display = {
          ds_channel         : out_channel;            (** Channel for writing *)
          ds_start_time      : float;                  (** When was compilation started *)
  mutable ds_last_update     : float;                  (** When was the display last updated *)
  mutable ds_last_target     : string;                 (** Last target built *)
  mutable ds_last_cached     : bool;                   (** Was the last target cached or really built ? *)
  mutable ds_last_tags       : Tags.t;                 (** Tags of the last command *)
  mutable ds_changed         : bool;                   (** Does the tag line need recomputing ? *)
          ds_update_interval : float;                  (** Minimum interval between updates *)
          ds_columns         : int;                    (** Number of columns in dssplay *)
  mutable ds_jobs            : int;                    (** Number of jobs launched or cached *)
  mutable ds_jobs_cached     : int;                    (** Number of jobs cached *)
          ds_tagline         : bytes;                  (** Current tagline *)
  mutable ds_seen_tags       : Tags.t;                 (** Tags that we have encountered *)
          ds_pathname_length : int;                    (** How much space for displaying pathnames ? *)
          ds_tld             : tagline_description;    (** Description for the tagline *)
};;
(* ***)
(*** display_line, display *)
type display_line =
| Classic
| Sophisticated of sophisticated_display

type display = {
          di_log_level    : int;
  mutable di_log_channel  : (Format.formatter * out_channel) option;
          di_channel      : out_channel;
          di_formatter    : Format.formatter;
          di_display_line : display_line;
  mutable di_finished     : bool;
}
;;
(* ***)
(*** various defaults *)
let default_update_interval = 0.05;;
let default_tagline_description = [
  "ocaml",     'O';
  "native",    'N';
  "byte",      'B';
  "program",   'P';
  "pp",        'R';
  "debug",     'D';
  "interf",    'I';
  "link",      'L';
];;

(* NOT including spaces *)
let countdown_chars = 8;;
let jobs_chars = 3;;
let jobs_cached_chars = 5;;
let dots = "...";;
let start_target = "STARTING";;
let finish_target = "FINISHED";;
let ticker_chars = 3;;
let ticker_period = 0.25;;
let ticker_animation = [|
  "\\";
  "|";
  "/";
  "-";
|];;
let cached = "*";;
let uncached = " ";;
let cache_chars = 1;;
(* ***)
(*** create_tagline *)
let create_tagline description = Bytes.make (List.length description) '-';;
(* ***)
(*** create *)
let create
  ?(channel=stdout)
  ?(mode:[`Classic|`Sophisticated] = `Sophisticated)
  ?columns:(_columns=75)
  ?(description = default_tagline_description)
  ?log_file
  ?(log_level=1)
  ()
  =
  let log_channel =
    match log_file with
    | None -> None
    | Some fn ->
        let oc = open_out_gen [Open_text; Open_wronly; Open_creat; Open_trunc] 0o666 fn in
        let f = Format.formatter_of_out_channel oc in
        Format.fprintf f "### Starting build.\n";
        Some (f, oc)
  in

  let display_line =
    match mode with
    | `Classic -> Classic
    | `Sophisticated ->
      (* We assume Unix is not degraded. *)
      let n = ANSI.get_columns () in
      let tag_chars = List.length description in
      Sophisticated
        { ds_channel         = stdout;
          ds_start_time      = gettimeofday ();
          ds_last_update     = 0.0;
          ds_last_target     = start_target;
          ds_last_tags       = Tags.empty;
          ds_last_cached     = false;
          ds_changed         = false;
          ds_update_interval = default_update_interval;
          ds_columns         = n;
          ds_jobs            = 0;
          ds_jobs_cached     = 0;
          ds_tagline         = create_tagline description;
          ds_seen_tags       = Tags.empty;
          ds_pathname_length = n -
                                 (countdown_chars + 1 + jobs_chars + 1 + jobs_cached_chars + 1 +
                                  cache_chars + 1 + tag_chars + 1 + ticker_chars + 2);
          ds_tld             = description }
  in
  { di_log_level    = log_level;
    di_log_channel  = log_channel;
    di_channel      = channel;
    di_formatter    = Format.formatter_of_out_channel channel;
    di_display_line = display_line;
    di_finished     = false }
;;
(* ***)
(*** print_time *)
let print_time oc t =
  let t = int_of_float t in
  let s = t mod 60 in
  let m = (t / 60) mod 60 in
  let h = t / 3600 in
  fp oc "%02d:%02d:%02d" h m s
;;
(* ***)
(*** print_shortened_pathname *)
let print_shortened_pathname length oc u =
  assert(length >= 3);
  let m = String.length u in
  if m <= length then
    begin
      output_string oc u;
      fp oc "%*s" (length - m) ""
    end
  else
    begin
      let n = String.length dots in
      let k = length - n in
      output_string oc dots;
      output_substring oc u (m - k) k;
    end
(* ***)
(*** Layout

00000000001111111111222222222233333333334444444444555555555566666666667777777777
01234567890123456789012345678901234567890123456789012345678901234567890123456789
HH MM SS XXXX        PATHNAME
00:12:31   32 (  26) ...lp4Filters/Camlp4LocationStripper.cmo * OBn-------------
|          |  |      |                                        | \ tags
|          |  |      \ last target built                      \ cached ?
|          |  |
|          |  \ number of jobs cached
|          \ number of jobs
\ elapsed time
cmo mllib
***)
(*** redraw_sophisticated *)
let redraw_sophisticated ds =
  let t = gettimeofday () in
  let oc = ds.ds_channel in
  let dt = t -. ds.ds_start_time in
  ds.ds_last_update <- t;
  fp oc "%a" ANSI.bol ();
  let ticker_phase = (abs (int_of_float (ceil (dt /. ticker_period)))) mod (Array.length ticker_animation) in
  let ticker = ticker_animation.(ticker_phase) in
  fp oc "%a %-4d (%-4d) %a %s %s %s"
    print_time dt
    ds.ds_jobs
    ds.ds_jobs_cached
    (print_shortened_pathname ds.ds_pathname_length) ds.ds_last_target
    (if ds.ds_last_cached then cached else uncached)
    (Bytes.to_string ds.ds_tagline)
    ticker;
  fp oc "%a%!" ANSI.clear_to_eol ()
;;
(* ***)
(*** redraw *)
let redraw = function
  | Classic -> ()
  | Sophisticated ds -> redraw_sophisticated ds
;;
(* ***)
(*** finish_sophisticated *)
let finish_sophisticated ?(how=`Success) ds =
  let t = gettimeofday () in
  let oc = ds.ds_channel in
  let dt = t -. ds.ds_start_time in
  match how with
  | `Success|`Error ->
    fp oc "%a" ANSI.bol ();
    fp oc "%s %d target%s (%d cached) in %a."
      (if how = `Error then
        "Compilation unsuccessful after building"
       else
         "Finished,")
      ds.ds_jobs
      (if ds.ds_jobs = 1 then "" else "s")
      ds.ds_jobs_cached
      print_time dt;
    fp oc "%a\n%!" ANSI.clear_to_eol ()
  | `Quiet ->
    fp oc "%a%a%!" ANSI.bol () ANSI.clear_to_eol ();
;;
(* ***)
(*** sophisticated_display *)
let sophisticated_display ds f =
  fp ds.ds_channel "%a%a%!" ANSI.bol () ANSI.clear_to_eol ();
  f ds.ds_channel
;;
(* ***)
(*** call_if *)
let call_if log_channel f =
  match log_channel with
  | None -> ()
  | Some x -> f x
;;
(* ***)
(*** display *)
let display di f =
  call_if di.di_log_channel (fun (_, oc) -> f oc);
  match di.di_display_line with
  | Classic -> f di.di_channel
  | Sophisticated ds -> sophisticated_display ds f
;;
(* ***)
(*** finish *)
let finish ?(how=`Success) di =
  if not di.di_finished then begin
    di.di_finished <- true;
    call_if di.di_log_channel
      begin fun (fmt, oc) ->
        Format.fprintf fmt "# Compilation %ssuccessful.@." (if how = `Error then "un" else "");
        close_out oc;
        di.di_log_channel <- None
      end;
    match di.di_display_line with
    | Classic -> ()
    | Sophisticated ds -> finish_sophisticated ~how ds
  end
;;
(* ***)
(*** update_tagline_from_tags *)
let update_tagline_from_tags ds =
  let tagline = ds.ds_tagline in
  let tags = ds.ds_last_tags in
  let rec loop i = function
    | [] ->
        for j = i to Bytes.length tagline - 1 do
          Bytes.set tagline j '-'
        done
    | (tag, c) :: rest ->
        if Tags.mem tag tags then
          Bytes.set tagline i (Char.uppercase c)
        else
          if Tags.mem tag ds.ds_seen_tags then
            Bytes.set tagline i (Char.lowercase c)
          else
            Bytes.set tagline i '-';
        loop (i + 1) rest
  in
  loop 0 ds.ds_tld;
;;
(* ***)
(*** update_sophisticated *)
let update_sophisticated ds =
  let t = gettimeofday () in
  let dt = t -. ds.ds_last_update in
  if dt > ds.ds_update_interval then
    begin
      if ds.ds_changed then
        begin
          update_tagline_from_tags ds;
          ds.ds_changed <- false
        end;
      redraw_sophisticated ds
    end
  else
    ()
;;
(* ***)
(*** set_target_sophisticated *)
let set_target_sophisticated ds target tags cached =
  ds.ds_changed <- true;
  ds.ds_last_target <- target;
  ds.ds_last_tags <- tags;
  ds.ds_jobs <- 1 + ds.ds_jobs;
  if cached then ds.ds_jobs_cached <- 1 + ds.ds_jobs_cached;
  ds.ds_last_cached <- cached;
  ds.ds_seen_tags <- Tags.union ds.ds_seen_tags ds.ds_last_tags;
  update_sophisticated ds
;;

let print_tags f tags =
  let first = ref true in
  Tags.iter begin fun tag ->
    if !first then begin
      first := false;
      Format.fprintf f "%s" tag
    end else Format.fprintf f ", %s" tag
  end tags
;;
(* ***)
(*** update *)
let update di =
  match di.di_display_line with
  | Classic -> ()
  | Sophisticated ds -> update_sophisticated ds
;;
(* ***)
(*** event *)
let event di ?(pretend=false) command target tags =
  call_if di.di_log_channel
    (fun (fmt, _) ->
      Format.fprintf fmt "# Target: %s, tags: { %a }\n" target print_tags tags;
      Format.fprintf fmt "%s%s@." command (if pretend then " # cached" else ""));
  match di.di_display_line with
  | Classic ->
      if pretend then
        begin
          (* This should work, even on Windows *)
          let command = Filename.basename command in
          if di.di_log_level >= 2 then Format.fprintf di.di_formatter "[cache hit] %s\n%!" command
        end
      else
        (if di.di_log_level >= 1 then Format.fprintf di.di_formatter "%s\n%!" command)
  | Sophisticated ds ->
      set_target_sophisticated ds target tags pretend;
      update_sophisticated ds
;;
(* ***)
(*** dprintf *)
let dprintf ?(log_level=1) di fmt =
  if log_level > di.di_log_level then Discard_printf.discard_printf fmt else
  match di.di_display_line with
  | Classic -> Format.fprintf di.di_formatter fmt
  | Sophisticated _ ->
      if log_level < 0 then
        begin
          display di ignore;
          Format.fprintf di.di_formatter fmt
        end
      else
        match di.di_log_channel with
        | Some (f, _) -> Format.fprintf f fmt
        | None -> Discard_printf.discard_printf fmt
(* ***)
