

external (^) : string -> string -> string = "#string_append"
external ( = ) : 'a -> 'a -> bool = "%equal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( ~- ) : int -> int = "%negint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external ( lsl ) : int -> int -> int = "%lslint"
external ( lor ) : int -> int -> int = "%orint"
external ( land ) : int -> int -> int = "%andint"
external ( mod ) : int -> int -> int = "%modint"

type 'a ref = { mutable contents : 'a }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"

external ( || ) : bool -> bool -> bool = "%sequor"
external ( && ) : bool -> bool -> bool = "%sequand"
external not : bool -> bool = "%boolnot"

external raise : exn -> 'a = "%raise"
external ignore : 'a -> unit = "%ignore"
external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

external ( ** ) : float -> float -> float =  "pow" [@@bs.val] [@@bs.scope "Math"]
external ( ~-. ) : float -> float = "%negfloat"
external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"
external mod_float : float -> float -> float = "caml_fmod_float" "fmod" "float"
external log : float -> float =  "log" [@@bs.val] [@@bs.scope "Math"]
external ceil : float -> float =  "ceil" [@@bs.val] [@@bs.scope "Math"]
external floor : float -> float =  "floor" [@@bs.val] [@@bs.scope "Math"]
external exp : float -> float =  "exp" [@@bs.val] [@@bs.scope "Math"]
external sqrt : float -> float =  "sqrt" [@@bs.val] [@@bs.scope "Math"]

type in_channel

type fpclass =
    FP_normal           (** Normal number, none of the below *)
  | FP_subnormal        (** Number very close to 0.0, has reduced precision *)
  | FP_zero             (** Number is 0.0 or -0.0 *)
  | FP_infinite         (** Number is positive or negative infinity *)
  | FP_nan              (** Not a number: result of an undefined operation *)

external float : int -> float = "%floatofint"

external int_of_float : float -> int = "%intoffloat"

module Bytes : sig
  external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
  external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
  external length : bytes -> int = "%bytes_length"
end

module Array : sig 
  external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
  external length : 'a array -> int = "%array_length"
  external make : int -> 'a -> 'a array = "caml_make_vect"
end 

module String : sig 
  external unsafe_get : string -> int -> char = "%string_unsafe_get"
  external length : string -> int = "%string_length"
end 

module Char : sig 
  external code : char -> int = "%identity"
  external unsafe_chr : int -> char = "%identity"
end 

module Obj : sig 
  type t 
  external magic : 'a -> 'b = "%identity"
  external repr : 'a -> t = "%identity"
  external field : t -> int -> t = "%obj_field"
  external set_field : t -> int -> t -> unit = "%obj_set_field"
  external tag : t -> int = "caml_obj_tag"
  external dup : t -> t = "caml_obj_dup"
end 

module Nativeint : sig 
  external add : nativeint -> nativeint -> nativeint = "%nativeint_add"
  external div : nativeint -> nativeint -> nativeint = "%nativeint_div"
  external rem : nativeint -> nativeint -> nativeint = "%nativeint_mod"
  external logor : nativeint -> nativeint -> nativeint = "%nativeint_or"
  external shift_left : nativeint -> int -> nativeint = "%nativeint_lsl"
  external logand : nativeint -> nativeint -> nativeint = "%nativeint_and"
  external shift_right_logical : nativeint -> int -> nativeint = "%nativeint_lsr"
  external shift_right : nativeint -> int -> nativeint = "%nativeint_asr"
  external mul : nativeint -> nativeint -> nativeint = "%nativeint_mul"
  external logxor : nativeint -> nativeint -> nativeint = "%nativeint_xor"
  external to_float : nativeint -> float = "caml_nativeint_to_float"
  external of_float : float -> nativeint = "caml_nativeint_of_float"
  external to_int : nativeint -> int = "%nativeint_to_int"
  external to_int32 : nativeint -> int32 = "%nativeint_to_int32"
  external of_int : int -> nativeint = "%nativeint_of_int"
  external neg : nativeint -> nativeint = "%nativeint_neg"
end

module Pervasives : sig 
  external compare : 'a -> 'a -> int = "%compare"
  external not : bool -> bool = "%boolnot"
  external min : 'a -> 'a -> 'a = "%bs_min"
  external max : 'a -> 'a -> 'a = "%bs_max"
  external ( = ) : 'a -> 'a -> bool = "%equal"
end

module Int32 : sig 
  external to_int : int32 -> int = "%int32_to_int"
  external add : int32 -> int32 -> int32 = "%int32_add"
  external shift_left : int32 -> int -> int32 = "%int32_lsl"
  external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"
  external shift_right : int32 -> int -> int32 = "%int32_asr"
  external logand : int32 -> int32 -> int32 = "%int32_and"
  external logxor : int32 -> int32 -> int32 = "%int32_xor"
  external logor : int32 -> int32 -> int32 = "%int32_or"
  external of_int : int -> int32 = "%int32_of_int"
  external mul : int32 -> int32 -> int32 = "%int32_mul"
end




module Lexing : sig 
  type lex_tables
  type lexbuf
end 

module Parsing : sig 
  type parse_tables 
  type parser_env
end 

module Int64 : sig 
  external of_int : int -> int64 = "%int64_of_int"
  external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
  external add : int64 -> int64 -> int64 = "%int64_add"
  external sub : int64 -> int64 -> int64 = "%int64_sub"
  external mul : int64 -> int64 -> int64 = "%int64_mul"
  external div : int64 -> int64 -> int64 = "%int64_div"
  external logor : int64 -> int64 -> int64 = "%int64_or"
  external neg : int64 -> int64 = "%int64_neg"
  external to_int : int64 -> int = "%int64_to_int"
end 

module Oo : sig 
  external id : < .. > -> int = "%field1"
end 

module Printexc : sig 
  type raw_backtrace_slot
  type backtrace_slot
end 

module Gc : sig 
  type stat =
    { minor_words : float;
      (** Number of words allocated in the minor heap since
          the program was started.  This number is accurate in
          byte-code programs, but only an approximation in programs
          compiled to native code. *)

      promoted_words : float;
      (** Number of words allocated in the minor heap that
          survived a minor collection and were moved to the major heap
          since the program was started. *)

      major_words : float;
      (** Number of words allocated in the major heap, including
          the promoted words, since the program was started. *)

      minor_collections : int;
      (** Number of minor collections since the program was started. *)

      major_collections : int;
      (** Number of major collection cycles completed since the program
          was started. *)

      heap_words : int;
      (** Total size of the major heap, in words. *)

      heap_chunks : int;
      (** Number of contiguous pieces of memory that make up the major heap. *)

      live_words : int;
      (** Number of words of live data in the major heap, including the header
          words. *)

      live_blocks : int;
      (** Number of live blocks in the major heap. *)

      free_words : int;
      (** Number of words in the free list. *)

      free_blocks : int;
      (** Number of blocks in the free list. *)

      largest_free : int;
      (** Size (in words) of the largest block in the free list. *)

      fragments : int;
      (** Number of wasted words due to fragmentation.  These are
          1-words free blocks placed between two live blocks.  They
          are not available for allocation. *)

      compactions : int;
      (** Number of heap compactions since the program was started. *)

      top_heap_words : int;
      (** Maximum size reached by the major heap, in words. *)

      stack_size: int;
      (** Current size of the stack, in words. @since 3.12.0 *)
    }
  type control =
    { mutable minor_heap_size : int;
      (** The size (in words) of the minor heap.  Changing
          this parameter will trigger a minor collection.  Default: 256k. *)

      mutable major_heap_increment : int;
      (** How much to add to the major heap when increasing it. If this
          number is less than or equal to 1000, it is a percentage of
          the current heap size (i.e. setting it to 100 will double the heap
          size at each increase). If it is more than 1000, it is a fixed
          number of words that will be added to the heap. Default: 15. *)

      mutable space_overhead : int;
      (** The major GC speed is computed from this parameter.
          This is the memory that will be "wasted" because the GC does not
          immediatly collect unreachable blocks.  It is expressed as a
          percentage of the memory used for live data.
          The GC will work more (use more CPU time and collect
          blocks more eagerly) if [space_overhead] is smaller.
          Default: 80. *)

      mutable verbose : int;
      (** This value controls the GC messages on standard error output.
          It is a sum of some of the following flags, to print messages
          on the corresponding events:
          - [0x001] Start of major GC cycle.
          - [0x002] Minor collection and major GC slice.
          - [0x004] Growing and shrinking of the heap.
          - [0x008] Resizing of stacks and memory manager tables.
          - [0x010] Heap compaction.
          - [0x020] Change of GC parameters.
          - [0x040] Computation of major GC slice size.
          - [0x080] Calling of finalisation functions.
          - [0x100] Bytecode executable and shared library search at start-up.
          - [0x200] Computation of compaction-triggering condition.
          Default: 0. *)

      mutable max_overhead : int;
      (** Heap compaction is triggered when the estimated amount
          of "wasted" memory is more than [max_overhead] percent of the
          amount of live data.  If [max_overhead] is set to 0, heap
          compaction is triggered at the end of each major GC cycle
          (this setting is intended for testing purposes only).
          If [max_overhead >= 1000000], compaction is never triggered.
          If compaction is permanently disabled, it is strongly suggested
          to set [allocation_policy] to 1.
          Default: 500. *)

      mutable stack_limit : int;
      (** The maximum size of the stack (in words).  This is only
          relevant to the byte-code runtime, as the native code runtime
          uses the operating system's stack.  Default: 1024k. *)

      mutable allocation_policy : int;
      (** The policy used for allocating in the heap.  Possible
          values are 0 and 1.  0 is the next-fit policy, which is
          quite fast but can result in fragmentation.  1 is the
          first-fit policy, which can be slower in some cases but
          can be better for programs with fragmentation problems.
          Default: 0. @since 3.11.0 *)
    }
end 

module CamlinternalOO : sig 
  type obj
  type closure
end 

module CamlinternalMod : sig 
  type shape =
    | Function
    | Lazy
    | Class
    | Module of shape array
    | Value of Obj.t
end 

