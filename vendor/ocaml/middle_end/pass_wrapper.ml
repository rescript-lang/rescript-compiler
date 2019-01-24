(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let register ~pass_name =
  Clflags.all_passes := pass_name :: !Clflags.all_passes

let with_dump ~pass_name ~f ~input ~print_input ~print_output =
  let dump = Clflags.dumped_pass pass_name in
  let result = f () in
  match result with
  | None ->
    if dump then Format.eprintf "%s: no-op.\n\n%!" pass_name;
    None
  | Some result ->
    if dump then begin
      Format.eprintf "Before %s:@ %a@.@." pass_name print_input input;
      Format.eprintf "After %s:@ %a@.@." pass_name print_output result
    end;
    Some result
