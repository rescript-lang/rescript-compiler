(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** The types and functions to create a html table representing a dag.
   Thanks to Daniel de Rauglaudre. *)

type 'a dag = { mutable dag : 'a node array }
and 'a node =
  { mutable pare : idag list; valu : 'a; mutable chil : idag list }
and idag = int

(** This function returns the html code to represent the given dag. *)
val html_of_dag : string dag -> string

(** This function takes a list of classes and a list of class types and creates the associate dag. *)
val create_class_dag :
    Odoc_info.Class.t_class list ->
      Odoc_info.Class.t_class_type list ->
        (Odoc_info.Name.t * Odoc_info.Class.cct option) dag
