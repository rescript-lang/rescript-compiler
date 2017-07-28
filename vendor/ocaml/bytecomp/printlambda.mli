(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Lambda

open Format

val structured_constant: formatter -> structured_constant -> unit

val env_lambda : Env.t -> formatter -> lambda -> unit
val lambda : formatter -> lambda -> unit
val primitive: formatter -> primitive -> unit

val lambda_as_module : Env.t  -> Format.formatter -> Lambda.lambda -> unit


val seriaize: Env.t -> string -> lambda -> unit

val serialize_raw_js: 
    (Env.t -> Types.signature ->  string  -> lambda  -> unit) ref
val serialize_js: (Env.t -> string  -> lambda  -> unit) ref
