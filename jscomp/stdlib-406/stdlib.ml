include Stdlib__no_aliases.Stdlib

(*MODULE_ALIASES*)
module Arg          = Arg
module Array        = Array
module ArrayLabels  = ArrayLabels
module Atomic       = Atomic
#if BS then
#else
module Bigarray     = Bigarray
#end
module Bool         = Bool
module Buffer       = Buffer
module Bytes        = Bytes
module BytesLabels  = BytesLabels
module Callback     = Callback
module Char         = Char
module Complex      = Complex
module Digest       = Digest
module Either       = Either
#if BS then
#else
module Ephemeron    = Ephemeron
#end
module Filename     = Filename
module Float        = Float
module Format       = Format
module Fun          = Fun
module Gc           = Gc
module Genlex       = Genlex
module Hashtbl      = Hashtbl
module In_channel   = In_channel
module Int          = Int
module Int32        = Int32
module Int64        = Int64
module Lazy         = Lazy
module Lexing       = Lexing
module List         = List
module ListLabels   = ListLabels
module Map          = Map
module Marshal      = Marshal
module MoreLabels   = MoreLabels
#if BS then
#else
module Nativeint    = Nativeint
#end
module Obj          = Obj
module Oo           = Oo
module Option       = Option
module Out_channel  = Out_channel
module Parsing      = Parsing
module Pervasives   = Pervasives
module Printexc     = Printexc
module Printf       = Printf
module Queue        = Queue
module Random       = Random
module Result       = Result
module Scanf        = Scanf
module Seq          = Seq
module Set          = Set
module Stack        = Stack
module StdLabels    = StdLabels
module Stream       = Stream
module String       = String
module StringLabels = StringLabels
module Sys          = Sys
module Uchar        = Uchar
module Unit         = Unit
module Weak         = Weak
