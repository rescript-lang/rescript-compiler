(**************************************************************************)
(*                                                                        *)
(*  Crude slicer for preprocessing reachability verification tasks        *)
(*                                                                        *)
(*  Copyright (C) 2016-2017 Mikhail Mandrykin, ISP RAS                    *)
(*                                                                        *)
(**************************************************************************)

module type Analysis = sig
  type t
  type 'a maybe_region =
    [< `Location of t
    |  `Value of t
    |  `None ] as 'a
  val of_var : ?f:string -> string -> [ `Location of _ | `Value of _  | `None ] maybe_region
end

module Make (Analysis : Analysis) = struct
  include Analysis
  let of_var  = of_var ~f:""
end

