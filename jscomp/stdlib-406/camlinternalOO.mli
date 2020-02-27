(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Run-time support for objects and classes.
    All functions in this module are for system use only, not for the
    casual user. *)

(** {1 Classes} *)

type tag
type label
type table
type meth
type t
type obj
type closure
val public_method_label : string -> tag
val new_method : table -> label (* [@@dead "new_method"] *)
val new_variable : table -> string -> int (* [@@dead "new_variable"] *)
val new_methods_variables : (* [@@dead "new_methods_variables"] *)
    table -> string array -> string array -> label array
val get_variable : table -> string -> int (* [@@dead "get_variable"] *)
val get_variables : table -> string array -> int array (* [@@dead "get_variables"] *)
val get_method_label : table -> string -> label (* [@@dead "get_method_label"] *)
val get_method_labels : table -> string array -> label array (* [@@dead "get_method_labels"] *)
val get_method : table -> label -> meth (* [@@dead "get_method"] *)
val set_method : table -> label -> meth -> unit (* [@@dead "set_method"] *)
val set_methods : table -> label array -> unit (* [@@dead "set_methods"] *)
val narrow : table -> string array -> string array -> string array -> unit (* [@@dead "narrow"] *)
val widen : table -> unit (* [@@dead "widen"] *)
val add_initializer : table -> (obj -> unit) -> unit (* [@@dead "add_initializer"] *)
val dummy_table : table (* [@@dead "dummy_table"] *)
val create_table : string array -> table (* [@@dead "create_table"] *)
val init_class : table -> unit (* [@@dead "init_class"] *)
val inherits : (* [@@dead "inherits"] *)
    table -> string array -> string array -> string array ->
    (t * (table -> obj -> Obj.t) * t * obj) -> bool -> Obj.t array
val make_class : (* [@@dead "make_class"] *)
    string array -> (table -> Obj.t -> t) ->
    (t * (table -> Obj.t -> t) * (Obj.t -> t) * Obj.t)
type init_table
val make_class_store : (* [@@dead "make_class_store"] *)
    string array -> (table -> t) -> init_table -> unit
#if BS then    
#else
val dummy_class :
    string * int * int ->
    (t * (table -> Obj.t -> t) * (Obj.t -> t) * Obj.t)
#end
(** {1 Objects} *)

val copy : (< .. > as 'a) -> 'a
val create_object : table -> obj (* [@@dead "create_object"] *)
val create_object_opt : obj -> table -> obj (* [@@dead "create_object_opt"] *)
val run_initializers : obj -> table -> unit (* [@@dead "run_initializers"] *)
val run_initializers_opt : obj -> obj -> table -> obj (* [@@dead "run_initializers_opt"] *)
val create_object_and_run_initializers : obj -> table -> obj (* [@@dead "create_object_and_run_initializers"] *)
#if BS then
#else
external send : obj -> tag -> t = "%send"
#end
external sendcache : obj -> tag -> t -> int -> t = "%sendcache"
external sendself : obj -> label -> t = "%sendself"
external get_public_method : obj -> tag -> closure
    = "caml_get_public_method" [@@noalloc]

(** {1 Table cache} *)

type tables
val lookup_tables : tables -> closure array -> tables (* [@@dead "lookup_tables"] *)

(** {1 Builtins to reduce code size} *)

(*
val get_const : t -> closure
val get_var : int -> closure
val get_env : int -> int -> closure
val get_meth : label -> closure
val set_var : int -> closure
val app_const : (t -> t) -> t -> closure
val app_var : (t -> t) -> int -> closure
val app_env : (t -> t) -> int -> int -> closure
val app_meth : (t -> t) -> label -> closure
val app_const_const : (t -> t -> t) -> t -> t -> closure
val app_const_var : (t -> t -> t) -> t -> int -> closure
val app_const_env : (t -> t -> t) -> t -> int -> int -> closure
val app_const_meth : (t -> t -> t) -> t -> label -> closure
val app_var_const : (t -> t -> t) -> int -> t -> closure
val app_env_const : (t -> t -> t) -> int -> int -> t -> closure
val app_meth_const : (t -> t -> t) -> label -> t -> closure
val meth_app_const : label -> t -> closure
val meth_app_var : label -> int -> closure
val meth_app_env : label -> int -> int -> closure
val meth_app_meth : label -> label -> closure
val send_const : tag -> obj -> int -> closure
val send_var : tag -> int -> int -> closure
val send_env : tag -> int -> int -> int -> closure
val send_meth : tag -> label -> int -> closure
*)

type impl =
    GetConst (* [@@dead "impl.GetConst"] *)
  | GetVar (* [@@dead "impl.GetVar"] *)
  | GetEnv (* [@@dead "impl.GetEnv"] *)
  | GetMeth (* [@@dead "impl.GetMeth"] *)
  | SetVar (* [@@dead "impl.SetVar"] *)
  | AppConst (* [@@dead "impl.AppConst"] *)
  | AppVar (* [@@dead "impl.AppVar"] *)
  | AppEnv (* [@@dead "impl.AppEnv"] *)
  | AppMeth (* [@@dead "impl.AppMeth"] *)
  | AppConstConst (* [@@dead "impl.AppConstConst"] *)
  | AppConstVar (* [@@dead "impl.AppConstVar"] *)
  | AppConstEnv (* [@@dead "impl.AppConstEnv"] *)
  | AppConstMeth (* [@@dead "impl.AppConstMeth"] *)
  | AppVarConst (* [@@dead "impl.AppVarConst"] *)
  | AppEnvConst (* [@@dead "impl.AppEnvConst"] *)
  | AppMethConst (* [@@dead "impl.AppMethConst"] *)
  | MethAppConst (* [@@dead "impl.MethAppConst"] *)
  | MethAppVar (* [@@dead "impl.MethAppVar"] *)
  | MethAppEnv (* [@@dead "impl.MethAppEnv"] *)
  | MethAppMeth (* [@@dead "impl.MethAppMeth"] *)
  | SendConst (* [@@dead "impl.SendConst"] *)
  | SendVar (* [@@dead "impl.SendVar"] *)
  | SendEnv (* [@@dead "impl.SendEnv"] *)
  | SendMeth (* [@@dead "impl.SendMeth"] *)
  | Closure of closure (* [@@dead "impl.Closure"] *)

(** {1 Parameters} *)

(* currently disabled *)
type params =
  { mutable compact_table : bool; (* [@@dead "params.compact_table"] *)
    mutable copy_parent : bool; (* [@@dead "params.copy_parent"] *)
    mutable clean_when_copying : bool; (* [@@dead "params.clean_when_copying"] *)
    mutable retry_count : int; (* [@@dead "params.retry_count"] *)
    mutable bucket_small_size : int } (* [@@dead "params.bucket_small_size"] *)

val params : params (* [@@dead "params"] *)

(** {1 Statistics} *)

type stats =
  { classes : int; (* [@@dead "stats.classes"] *)
    methods : int; (* [@@dead "stats.methods"] *)
    inst_vars : int } (* [@@dead "stats.inst_vars"] *)
val stats : unit -> stats (* [@@dead "stats"] *)