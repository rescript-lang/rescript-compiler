module Std = struct module Hash = Hashtbl end;;

open Std;;
module Hash1 : module type of Hash = Hash;;
module Hash2 : sig include (module type of Hash) end = Hash;;
let f1 (x : (_,_) Hash1.t) = (x : (_,_) Hashtbl.t);;
let f2 (x : (_,_) Hash2.t) = (x : (_,_) Hashtbl.t);;

(* Another case, not using include *)

module Std2 = struct module M = struct type t end end;;
module Std' = Std2;;
module M' : module type of Std'.M = Std2.M;;
let f3 (x : M'.t) = (x : Std2.M.t);;

(* original report required Core_kernel:
module type S = sig
open Core_kernel.Std

module Hashtbl1 : module type of Hashtbl
module Hashtbl2 : sig
  include (module type of Hashtbl)
end

module Coverage : Core_kernel.Std.Hashable

type types = unit constraint 'a Coverage.Table.t = (Coverage.t, 'a) Hashtbl1.t
type doesnt_type = unit
  constraint 'a Coverage.Table.t = (Coverage.t, 'a) Hashtbl2.t
end
*)
