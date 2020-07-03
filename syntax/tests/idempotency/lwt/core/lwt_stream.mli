(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Data streams *)

type 'a t
(** A stream holding values of type ['a].

    Naming convention: in this module, all functions applying a function
    to each element of a stream are suffixed by:

    - [_s] when the function returns a thread and calls are serialised
    - [_p] when the function returns a thread and calls are parallelised *)

(** {2 Construction} *)

val from : (unit -> 'a option Lwt.t) -> 'a t
(** [from f] creates a stream from the given input function. [f] is
    called each time more input is needed, and the stream ends when
    [f] returns [None].

    If [f], or the thread produced by [f], raises an exception, that exception
    is forwarded to the consumer of the stream (for example, a caller of
    {!get}). Note that this does not end the stream. A subsequent attempt to
    read from the stream will cause another call to [f], which may succeed
    with a value. *)

val from_direct : (unit -> 'a option) -> 'a t
(** [from_direct f] does the same as {!from} but with a function
    that does not return a thread. It is preferred that this
    function be used rather than wrapping [f] into a function which
    returns a thread.

    The behavior when [f] raises an exception is the same as for {!from},
    except that [f] does not produce a thread. *)

exception Closed
(** Exception raised by the push function of a push-stream when
    pushing an element after the end of stream ([= None]) has been
    pushed. *)

val create : unit -> 'a t * ('a option -> unit)
(** [create ()] returns a new stream and a push function.

    To notify the stream's consumer of errors, either use a separate
    communication channel, or use a
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#TYPEresult}
    [result]} stream. There is no way to push an exception into a
    push-stream. *)

val create_with_reference : unit -> 'a t * ('a option -> unit) * ('b -> unit)
(** [create_with_reference ()] returns a new stream and a push
    function. The last function allows a reference to be set to an
    external source. This prevents the external source from being
    garbage collected.

    For example, to convert a reactive event to a stream:

    {[
      let stream, push, set_ref = Lwt_stream.create_with_reference () in
      set_ref (map_event push event)
    ]}
*)

exception Full
(** Exception raised by the push function of a bounded push-stream
    when the stream queue is full and a thread is already waiting to
    push an element. *)

(** Type of sources for bounded push-streams. *)
val create_bounded : int -> 'a t * 'a bounded_push
(** [create_bounded size] returns a new stream and a bounded push
    source. The stream can hold a maximum of [size] elements.  When
    this limit is reached, pushing a new element will block until
    one is consumed.

    Note that you cannot clone or parse (with {!parse}) a bounded
    stream. These functions will raise [Invalid_argument] if you try
    to do so.

    It raises [Invalid_argument] if [size < 0]. *)

val of_seq : 'a Seq.t -> 'a t
(** [of_seq s] creates a stream returning all elements of [s]. The elements are
    evaluated from [s] and pushed onto the stream as the stream is consumed.

    @since 4.2.0 *)

val of_list : 'a list -> 'a t
(** [of_list l] creates a stream returning all elements of [l]. The elements are
    pushed into the stream immediately, resulting in a closed stream (in the
    sense of {!is_closed}). *)

val of_array : 'a array -> 'a t
(** [of_array a] creates a stream returning all elements of [a]. The elements
    are pushed into the stream immediately, resulting in a closed stream (in the
    sense of {!is_closed}). *)

val of_string : string -> char t
(** [of_string str] creates a stream returning all characters of [str]. The
    characters are pushed into the stream immediately, resulting in a closed
    stream (in the sense of {!is_closed}). *)

val clone : 'a t -> 'a t
(** [clone st] clone the given stream. Operations on each stream
    will not affect the other.

    For example:

    {[
      # let st1 = Lwt_stream.of_list [1; 2; 3];;
      val st1 : int Lwt_stream.t = <abstr>
      # let st2 = Lwt_stream.clone st1;;
      val st2 : int Lwt_stream.t = <abstr>
      # lwt x = Lwt_stream.next st1;;
      val x : int = 1
      # lwt y = Lwt_stream.next st2;;
      val y : int = 1
    ]}

    It raises [Invalid_argument] if [st] is a bounded
    push-stream. *)

(** {2 Destruction} *)

val to_list : 'a t -> 'a list Lwt.t
(** Returns the list of elements of the given stream *)

val to_string : char t -> string Lwt.t
(** Returns the word composed of all characters of the given
    stream *)

(** {2 Data retrieval} *)

exception Empty
(** Exception raised when trying to retrieve data from an empty
    stream. *)

val peek : 'a t -> 'a option Lwt.t
(** [peek st] returns the first element of the stream, if any,
    without removing it. *)

val npeek : int -> 'a t -> 'a list Lwt.t
(** [npeek n st] returns at most the first [n] elements of [st],
    without removing them. *)

val get : 'a t -> 'a option Lwt.t
(** [get st] removes and returns the first element of the stream, if
    any. *)

val nget : int -> 'a t -> 'a list Lwt.t
(** [nget n st] removes and returns at most the first [n] elements of
    [st]. *)

val get_while : ('a -> bool) -> 'a t -> 'a list Lwt.t
val get_while_s : ('a -> bool Lwt.t) -> 'a t -> 'a list Lwt.t
(** [get_while f st] returns the longest prefix of [st] where all
    elements satisfy [f]. *)

val next : 'a t -> 'a Lwt.t
(** [next st] removes and returns the next element of the stream or
    fails with {!Empty}, if the stream is empty. *)

val last_new : 'a t -> 'a Lwt.t
(** [last_new st] returns the last element that can be obtained
    without sleeping, or wait for one if none is available.

    It fails with {!Empty} if the stream has no more elements. *)

val junk : 'a t -> unit Lwt.t
(** [junk st] removes the first element of [st]. *)

val njunk : int -> 'a t -> unit Lwt.t
(** [njunk n st] removes at most the first [n] elements of the
    stream. *)

val junk_while : ('a -> bool) -> 'a t -> unit Lwt.t
val junk_while_s : ('a -> bool Lwt.t) -> 'a t -> unit Lwt.t
(** [junk_while f st] removes all elements at the beginning of the
    streams which satisfy [f]. *)

val junk_old : 'a t -> unit Lwt.t
(** [junk_old st] removes all elements that are ready to be read
    without yielding from [st].

    For example, the [read_password] function of [Lwt_read_line]
    uses it to flush keys previously typed by the user.
*)

val get_available : 'a t -> 'a list
(** [get_available st] returns all available elements of [l] without
    blocking. *)

val get_available_up_to : int -> 'a t -> 'a list
(** [get_available_up_to n st] returns up to [n] elements of [l]
    without blocking. *)

val is_empty : 'a t -> bool Lwt.t
(** [is_empty st] returns whether the given stream is empty. *)

val is_closed : 'a t -> bool
(** [is_closed st] returns whether the given stream has been closed. A closed
    stream is not necessarily empty. It may still contain unread elements. If
    [is_closed s = true], then all subsequent reads until the end of the
    stream are guaranteed not to block.

    @since 2.6.0 *)

val closed : 'a t -> unit Lwt.t
(** [closed st] returns a thread that will sleep until the stream has been
    closed.

    @since 2.6.0 *)

val on_termination : 'a t -> (unit -> unit) -> unit
[@@ocaml.deprecated " Bind on Lwt_stream.closed."]
(** [on_termination st f] executes [f] when the end of the stream [st]
    is reached. Note that the stream may still contain elements if
    {!peek} or similar was used.

    @deprecated Use {!closed}. *)

val on_terminate : 'a t -> (unit -> unit) -> unit
[@@ocaml.deprecated " Bind on Lwt_stream.closed."]
(** Same as {!on_termination}.

    @deprecated Use {!closed}. *)

(** {2 Stream transversal} *)

(** Note: all the following functions are destructive.

    For example:

    {[
      # let st1 = Lwt_stream.of_list [1; 2; 3];;
      val st1 : int Lwt_stream.t = <abstr>
      # let st2 = Lwt_stream.map string_of_int st1;;
      val st2 : string Lwt_stream.t = <abstr>
      # lwt x = Lwt_stream.next st1;;
      val x : int = 1
      # lwt y = Lwt_stream.next st2;;
      val y : string = "2"
    ]}
*)

val choose : 'a t list -> 'a t
(** [choose l] creates an stream from a list of streams. The
    resulting stream will return elements returned by any stream of
    [l] in an unspecified order. *)

val map : ('a -> 'b) -> 'a t -> 'b t
val map_s : ('a -> 'b Lwt.t) -> 'a t -> 'b t
(** [map f st] maps the value returned by [st] with [f] *)

val filter : ('a -> bool) -> 'a t -> 'a t
val filter_s : ('a -> bool Lwt.t) -> 'a t -> 'a t
(** [filter f st] keeps only values, [x], such that [f x] is [true] *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val filter_map_s : ('a -> 'b option Lwt.t) -> 'a t -> 'b t
(** [filter_map f st] filter and map [st] at the same time *)

val map_list : ('a -> 'b list) -> 'a t -> 'b t
val map_list_s : ('a -> 'b list Lwt.t) -> 'a t -> 'b t
(** [map_list f st] applies [f] on each element of [st] and flattens
    the lists returned *)

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b Lwt.t
val fold_s : ('a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t
(** [fold f s x] fold_like function for streams. *)

val iter : ('a -> unit) -> 'a t -> unit Lwt.t
val iter_p : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
val iter_s : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
(** [iter f s] iterates over all elements of the stream. *)

val iter_n : ?max_concurrency:int -> ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
(** [iter_n ?max_concurrency f s] iterates over all elements of the stream [s].
    Iteration is performed concurrently with up to [max_threads] concurrent
    instances of [f].

    Iteration is {b not} guaranteed to be in order as this function will
    attempt to always process [max_concurrency] elements from [s] at once.

    @param max_concurrency defaults to [1].
    @raise Invalid_argument if [max_concurrency < 1].
    @since 3.3.0 *)

val find : ('a -> bool) -> 'a t -> 'a option Lwt.t
val find_s : ('a -> bool Lwt.t) -> 'a t -> 'a option Lwt.t
(** [find f s] find an element in a stream. *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option Lwt.t
val find_map_s : ('a -> 'b option Lwt.t) -> 'a t -> 'b option Lwt.t
(** [find_map f s] find and map at the same time. *)

val combine : 'a t -> 'b t -> ('a * 'b) t
(** [combine s1 s2] combines two streams. The stream will end when
    either stream ends. *)

val append : 'a t -> 'a t -> 'a t
(** [append s1 s2] returns a stream which returns all elements of
    [s1], then all elements of [s2] *)

val concat : 'a t t -> 'a t
(** [concat st] returns the concatenation of all streams of [st]. *)

val flatten : 'a list t -> 'a t
(** [flatten st = map_list (fun l -> l) st] *)

val wrap_exn : 'a t -> 'a Lwt.result t
(** [wrap_exn s] is a stream [s'] such that each time [s] yields a value [v],
    [s'] yields [Result.Ok v], and when the source of [s] raises an exception
    [e], [s'] yields [Result.Error e].

    Note that push-streams (as returned by {!create}) never raise exceptions.

    If the stream source keeps raising the same exception [e] each time the
    stream is read, [s'] is unbounded. Reading it will produce [Result.Error e]
    indefinitely.

    @since 2.7.0 *)

(** {2 Parsing} *)

val parse : 'a t -> ('a t -> 'b Lwt.t) -> 'b Lwt.t
(** [parse st f] parses [st] with [f]. If [f] raise an exception,
    [st] is restored to its previous state.

    It raises [Invalid_argument] if [st] is a bounded
    push-stream. *)

(** {2 Misc} *)

val hexdump : char t -> string t
(** [hexdump byte_stream] returns a stream which is the same as the
    output of [hexdump -C].

    Basically, here is a simple implementation of [hexdump -C]:

    {[
      let () = Lwt_main.run (Lwt_io.write_lines Lwt_io.stdout (Lwt_stream.hexdump (Lwt_io.read_lines Lwt_io.stdin)))
    ]}
*)

(** {2 Deprecated} *)

type 'a result =
  | Value of 'a
  | Error of exn
[@@ocaml.deprecated
  " This type is being replaced by Lwt.result and the corresponding function
 Lwt_stream.wrap_exn."]
(** A value or an error.

    @deprecated Replaced by {!wrap_exn}, which uses {!Lwt.result}. *)

[@@@ocaml.warning "-3"]
val map_exn : 'a t -> 'a result t
[@@ocaml.deprecated " Use Lwt_stream.wrap_exn"]
(** [map_exn s] returns a stream that captures all exceptions raised
    by the source of the stream (the function passed to {!from}).

    Note that for push-streams (as returned by {!create}) all
    elements of the mapped streams are values.

    If the stream source keeps raising the same exception [e] each time the
    stream is read, the stream produced by [map_exn] is unbounded. Reading it
    will produce [Lwt_stream.Error e] indefinitely.

    @deprecated Use {!wrap_exn}. *)

[@@@ocaml.warning "+3"]
