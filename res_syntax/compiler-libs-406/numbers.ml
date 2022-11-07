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

module Int_base = Identifiable.Make (struct
  type t = int

  let compare x y = x - y
  let output oc x = Printf.fprintf oc "%i" x
  let hash i = i
  let equal (i : int) j = i = j
  let print = Format.pp_print_int
end)

module Int = struct
  type t = int

  include Int_base

  let rec zero_to_n n =
    if n < 0 then Set.empty else Set.add n (zero_to_n (n-1))
end

module Int8 = struct
  type t = int

  let zero = 0
  let one = 1

  let of_int_exn i =
    if i < -(1 lsl 7) || i > ((1 lsl 7) - 1) then
      Misc.fatal_errorf "Int8.of_int_exn: %d is out of range" i
    else
      i

  let to_int i = i
end

module Int16 = struct
  type t = int

  let of_int_exn i =
    if i < -(1 lsl 15) || i > ((1 lsl 15) - 1) then
      Misc.fatal_errorf "Int16.of_int_exn: %d is out of range" i
    else
      i

  let lower_int64 = Int64.neg (Int64.shift_left Int64.one 15)
  let upper_int64 = Int64.sub (Int64.shift_left Int64.one 15) Int64.one

  let of_int64_exn i =
    if Int64.compare i lower_int64 < 0
        || Int64.compare i upper_int64 > 0
    then
      Misc.fatal_errorf "Int16.of_int64_exn: %Ld is out of range" i
    else
      Int64.to_int i

  let to_int t = t
end

module Float = struct
  type t = float

  include Identifiable.Make (struct
    type t = float

    let compare x y = Stdlib.compare x y
    let output oc x = Printf.fprintf oc "%f" x
    let hash f = Hashtbl.hash f
    let equal (i : float) j = i = j
    let print = Format.pp_print_float
  end)
end
