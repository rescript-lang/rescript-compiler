(* Values *)

module X : sig
  val x : int [@@deprecated "DEPRECATED"]
end = struct
  let x = 7
end

module Y : sig val x : int end = X

module Z : sig val x : int [@@deprecated "..."] end = X

module F(A : sig val x : int end) = struct end

module B = F(X)



module XX = struct let x = 7 end
module YY : sig val x : int [@@deprecated "..."] end = XX


(* Constructors *)

module CSTR : sig type t = A | B end = struct type t = A [@deprecated] | B end

module CSTR1 = struct
  type t = A [@deprecated] | B
  type s = t = A | B
end


(* Fields *)

module FIELD :
sig type t = {mutable x: int} end =
struct type t = {mutable x: int [@deprecated_mutable]} end

module FIELD1 = struct
  type t = {mutable x: int [@deprecated_mutable]}
  type s = t = {mutable x: int}
end

(* Types *)

module TYPE : sig type t = int end = struct type t = int [@@deprecated] end

(* Class, class types *)

module CL :
sig class c : object end end =
struct class c = object end [@@deprecated "FOO"] end

module CLT :
sig class type c = object end end =
struct class type c = object end [@@deprecated "FOO"] end


(* Module types *)

module MT :
sig module type S = sig end end =
struct module type S = sig end [@@deprecated "FOO"] end

module MT_OK :
sig module type S = sig end [@@deprecated] end =
struct module type S = sig end [@@deprecated "FOO"] end


(* Modules *)

module MD :
sig module M : sig end end =
struct module M = struct end [@@deprecated "FOO"] end

module MD_OK :
sig module M : sig end [@@deprecated] end =
struct module M = struct end [@@deprecated "FOO"] end
