(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external spacetime_enabled : unit -> bool
  = "caml_spacetime_enabled" [@@noalloc]

let enabled = spacetime_enabled ()

let if_spacetime_enabled f =
  if enabled then f () else ()

module Series = struct
  type t = {
    channel : out_channel;
    mutable closed : bool;
  }

  external write_magic_number : out_channel -> unit
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_write_magic_number"

  external register_channel_for_spacetime : out_channel -> unit
    = "caml_register_channel_for_spacetime"

  let create ~path =
    if spacetime_enabled () then begin
      let channel = open_out path in
      register_channel_for_spacetime channel;
      let t =
        { channel = channel;
          closed = false;
        }
      in
      write_magic_number t.channel;
      t
    end else begin
      { channel = stdout;  (* arbitrary value *)
        closed = true;
      }
    end

  external save_event : ?time:float -> out_channel -> event_name:string -> unit
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_save_event"

  let save_event ?time t ~event_name =
    if_spacetime_enabled (fun () ->
      save_event ?time t.channel ~event_name)

  external save_trie : ?time:float -> out_channel -> unit
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_save_trie"

  let save_and_close ?time t =
    if_spacetime_enabled (fun () ->
      if t.closed then failwith "Series is closed";
      save_trie ?time t.channel;
      close_out t.channel;
      t.closed <- true)
end

module Snapshot = struct
  external take : ?time:float -> out_channel -> unit
    = "caml_spacetime_only_works_for_native_code"
      "caml_spacetime_take_snapshot"

  let take ?time { Series.closed; channel } =
    if_spacetime_enabled (fun () ->
      if closed then failwith "Series is closed";
      Gc.minor ();
      take ?time channel)
end

external save_event_for_automatic_snapshots : event_name:string -> unit
  = "caml_spacetime_only_works_for_native_code"
    "caml_spacetime_save_event_for_automatic_snapshots"

let save_event_for_automatic_snapshots ~event_name =
  if_spacetime_enabled (fun () ->
    save_event_for_automatic_snapshots ~event_name)
