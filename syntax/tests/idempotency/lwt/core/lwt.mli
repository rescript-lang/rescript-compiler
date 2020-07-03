(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Asynchronous programming with promises.

    A {b promise} is a placeholder for a single value which might take a long
    time to compute. Speaking roughly, a promise is a [ref] that can be filled
    in later. To make that precise, here is how promises differ from [ref]s:

    - A promise might not have a value yet. A promise in this state is called a
      {e pending} promise.
    - Writing a value into a promise is called {e resolving} it. A promise with
      a value is called a {e resolved} promise.
    - Each promise can be resolved only once. After a promise has a value, the
      promise is immutable.
    - It's possible to attach {b callbacks} to a promise. They will run when the
      promise has a value, i.e. is resolved. If the promise is already resolved
      when a callback is attached, the callback is run (almost) right away. If
      the promise is pending, the callback is put into a list and waits.

    So, promises are optional, write-once references, and when they don't yet
    have a value, they store a list of callbacks that are waiting for the value.

    The waiting callbacks make promises a natural data type for asynchronous
    programming. For example, you can ask Lwt to [read] a file. Lwt immediately
    returns you only a {e promise} for the data.

    You can neglect this promise for a while. You can do some other computation,
    request more I/O, etc. At some point, you might decide to attach a callback
    to the [read] promise, maybe several callbacks.

    In the meantime, the [read] operation is running in the background. Once it
    finishes, Lwt {e resolves} the [read] promise by putting the data into it.
    Lwt then runs the callbacks you attached.

    One of those might take the data, and ask Lwt to [write] it to STDOUT. Lwt
    gives you a promise for that, too, and the process repeats.

    Lwt has a small amount of syntactic sugar to make this look as natural as
    possible:

{[
let () =
  Lwt_main.run begin
    let%lwt data = Lwt_io.(read_line stdin) in
    let%lwt () = Lwt_io.printl data in
    Lwt.return ()
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix echo.ml && ./a.out *)
]}

    This is all explained in the next sections:

    - {{: #3_Quickstart} Quick start} links these concepts to actual functions
      in Lwt – the most fundamental ones.
    - {{: #3_Tutorial} Tutorial} shows how to write examples like the above, and
      how concurrency happens.
    - {{: #3_Executionmodel} Execution model} clarifies control flow when using
      Lwt.
    - {{: #3_GuidetotherestofLwt} Guide to the rest of Lwt} shows how
      {e everything} else in Lwt fits into this framework.

    After that is the {{: #2_Fundamentals} reference proper}, which goes into
    {e painful} levels of detail on every single type and value in this module,
    [Lwt]. Please be safe, and read only what you need from it :)

    Happy asynchronous programming!



    {3 Quick start}

    {e All} of Lwt is variations on:

    - {b Promises} of type ['a ]{!Lwt.t} are placeholders for values of type
      ['a].
    - {!Lwt.bind} attaches {b callbacks} to promises. When a promise gets a
      value, its callbacks are called.
    - Separate {b resolvers} of type ['a ]{!Lwt.u} are used to write values into
      promises, through {!Lwt.wakeup_later}.
    - Promises and resolvers are created in pairs using {!Lwt.wait}. Lwt I/O
      functions call {!Lwt.wait} internally, but return only the promise.
    - {!Lwt_main.run} is used to wait on one “top-level” promise. When that
      promise gets a value, the program terminates.



    {3 Tutorial}

    Let's read  from STDIN. The first version is written using ordinary values
    from the OCaml standard library. This makes the program block until the user
    enters a line:

{[
let () =
  let line : string = read_line () in
  print_endline "Now unblocked!";
  ignore line

(* ocamlfind opt -linkpkg code.ml && ./a.out *)
]}

    If we use a promise instead, execution continues immediately:

{[
let () =
  let line_promise : string Lwt.t =
    Lwt_io.(read_line stdin) in
  print_endline "Execution just continues...";
  ignore line_promise

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    Indeed, this program is a little {e too} asynchronous – it exits right away!
    Let's force it to wait for [line_promise] at the end by calling
    {!Lwt_main.run}:

{[
let () =
  let line_promise : string Lwt.t =
    Lwt_io.(read_line stdin) in
  print_endline "Execution just continues...";

  let line : string =
    Lwt_main.run line_promise in
  ignore line

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    {!Lwt_main.run} should only be called once, on one promise, at the top level
    of your program. Most of the time, waiting for promises is done using
    [let%lwt]. That is the recommended syntactic sugar for {!Lwt.bind}, and is
    pronounced “bind”:

{[
let () =
  let p : unit Lwt.t =
    let%lwt line_1 = Lwt_io.(read_line stdin) in
    let%lwt line_2 = Lwt_io.(read_line stdin) in
    Lwt_io.printf "%s and %s\n" line_1 line_2
  in

  Lwt_main.run p

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    The way that works is everything in scope after the “[in]” in
    “[let%lwt x =] ... [in] ...” goes into a callback, and “[x]” is that
    callback's argument. So, we could have been very explicit, and written the
    code like this:

{[
let () =
  let p : unit Lwt.t =
    let line_1_promise : string Lwt.t = Lwt_io.(read_line stdin) in
    Lwt.bind line_1_promise (fun (line_1 : string) ->

      let line_2_promise : string Lwt.t = Lwt_io.(read_line stdin) in
      Lwt.bind line_2_promise (fun (line_2 : string) ->

        Lwt_io.printf "%s and %s\n" line_1 line_2))
  in

  Lwt_main.run p

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    But, as you can see, this is verbose, and the indentation gets a bit crazy.
    So, we will always use [let%lwt].

    The code above reads two lines in sequence, because we ask Lwt to wait for
    [line_1], before calling the second {!Lwt_io.read_line} in the callback, to
    start the second I/O.

    We could also run I/O {e concurrently}. All we have to do is not start the
    second I/O in a callback of the first. Because it doesn't make sense to read
    two lines from STDIN concurrently, let's start two waits instead:

{[
let () =
  Lwt_main.run begin
    let three_seconds : unit Lwt.t = Lwt_unix.sleep 3. in
    let five_seconds : unit Lwt.t = Lwt_unix.sleep 5. in
    let%lwt () = three_seconds in
    let%lwt () = Lwt_io.printl "3 seconds passed" in
    let%lwt () = five_seconds in
    Lwt_io.printl "Only 2 more seconds passed"
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    This program takes about five seconds to run. We are still new to [let%lwt],
    so let's desugar it:

{[
let () =
  Lwt_main.run begin
    let three_seconds : unit Lwt.t = Lwt_unix.sleep 3. in
    let five_seconds : unit Lwt.t = Lwt_unix.sleep 5. in

    (* Both waits have already been started at this point! *)

    Lwt.bind three_seconds (fun () ->
      (* This is 3 seconds later. *)
      Lwt.bind (Lwt_io.printl "3 seconds passed") (fun () ->
        Lwt.bind five_seconds (fun () ->
          (* Only 2 seconds were left in the 5-second wait, so
              this callback runs 2 seconds after the first callback. *)
          Lwt_io.printl "Only 2 more seconds passed")))
  end

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    And that's it! Concurrency in Lwt is simply a matter of whether you start an
    operation in the callback of another one or not. As a convenience, Lwt
    provides a few {{: #2_Concurrency} helpers} for common concurrency patterns.



    {3 Execution model}

    It's important to understand that promises are a pure-OCaml data type. They
    don't do any fancy scheduling or I/O. They are just lists of callbacks (if
    pending), or containers for one value (if resolved).

    The interesting function is {!Lwt_main.run}. It's a wrapper around
    {{: http://man7.org/linux/man-pages/man2/select.2.html} [select(2)]},
    {{: http://man7.org/linux/man-pages/man7/epoll.7.html} [epoll(7)]},
    {{: https://www.freebsd.org/cgi/man.cgi?query=kqueue&sektion=2}
    [kqueue(2)]}, or whatever asynchronous I/O API your system provides. On
    browsers, the work of {!Lwt_main.run} is done by the surrounding JavaScript
    engine, so you don't call {!Lwt_main.run} from inside your program. But the
    execution model is still the same, and the description below applies!

    To avoid writing out “underlying asynchronous I/O API,” we'll assume, in
    this section, that the API is [select(2)]. That's just for the sake of
    abbreviation. It doesn't actually matter, for most purposes, what the
    underlying I/O API is.

    Let's use the program from the tutorial that reads two lines as an example.
    Here it is, again, in its desugared form:

{[
let () =
  let p : unit Lwt.t =
    let line_1_promise : string Lwt.t = Lwt_io.(read_line stdin) in
    Lwt.bind line_1_promise (fun (line_1 : string) ->

      let line_2_promise : string Lwt.t = Lwt_io.(read_line stdin) in
      Lwt.bind line_2_promise (fun (line_2 : string) ->

        Lwt_io.printf "%s and %s\n" line_1 line_2))
  in

  Lwt_main.run p

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    {!Lwt_main.run} is your program's main I/O loop. You pass it a single
    promise, and it:

    + Uses [select(2)] to put your process to sleep until the next I/O
      completes.
    + That next I/O happens to be the one that reads [line_1]. {!Lwt_main.run}
      knows that I/O is supposed to resolve [line_1_promise], so it puts
      [line_1] into the promise and resolves it.
    + Resolving is an ordinary OCaml operation. It causes all the callbacks of
      [line_1_promise] to run, one after another. Each callback is also ordinary
      OCaml code. In our case, there is only one callback, but in general, there
      might be several, and they might also resolve additional promises. So,
      promise resolution triggers a “cascade” of callbacks. Eventually, however,
      we should run out of callbacks, and control will return to
      {!Lwt_main.run}.
    + In our example, our one callback registers a second I/O with
      {!Lwt_main.run} – the one that will read [line_2]. There are no callbacks
      left to run after that, so control returns to {!Lwt_main.run}.
    + {!Lwt_main.run} goes back to sleep again by calling [select(2)], now
      waiting for the second I/O that we just registered. The loop repeats
      itself from step 1.

    This has two major implications, one good and one bad. Let's start with the
    bad one.

    {b (1)} If one of your callbacks enters an infinite loop, calls an
    Lwt-unfriendly blocking I/O, or just runs for a really long time, it won't
    return control to {!Lwt_main.run} anytime soon. That means {!Lwt_main.run}
    won't get a chance to resolve any other Lwt I/O promises, even if the
    underlying I/O operations complete.

    In case your callback is just using the CPU for a really long time, you can
    insert a few calls to {!Lwt_main.yield} into it, and resume your computation
    in callbacks of [yield]. This is basically the same as
    {!Lwt_unix.sleep}[ 0.] – it's a promise that will be resolved by
    {!Lwt_main.run} {e after} any other I/O resolutions that are already in its
    queue.

    {b (2)} The good implication is that all your callbacks run in a single
    thread. This means that in most situations, you don't have to worry about
    locks, synchronization, etc. Anything that is in the same callback is
    guaranteed to run without interruption. Lwt programs are often {e much}
    easier to write and refactor, than equivalent programs written with threads
    – but both are concurrent!



    {3 Guide to the rest of Lwt}

    This module [Lwt] is the pure-OCaml definition of promises and
    callback-calling. It has a few extras on top of what's described above:

    - {{: #2_Rejection} Rejection}. Lwt promises can actually be resolved in two
      ways: {e fulfilled} with a value, or {e rejected} with an exception. There
      is nothing conceptually special about rejection – it's just that you can
      ask for callbacks to run only on fulfillment, only on rejection, etc.
    - {{: #2_Cancelation} Cancellation}. This is a special case of rejection,
      specifically with exception {!Lwt.Canceled}. It has extra helpers in the
      Lwt API.
    - {{: #2_Concurrency} Concurrency helpers}. All of these could be
      implemented on top of {!Lwt.bind}. As we saw, Lwt concurrency requires
      only deciding whether to run something inside a callback, or outside it.
      These functions just implement common patterns, and make intent explicit.
    - Miscellaneous {{: #2_Convenience} helpers}, and {{: #2_Deprecated}
      deprecated} APIs.

    The next layer above module [Lwt] is the pure-OCaml Lwt “core” library,
    which provides some promise-friendly patterns, like streams and mvars. This
    consists of the modules {!Lwt_list}, {!Lwt_stream}, {!Lwt_result},
    {!Lwt_mutex}, {!Lwt_condition}, {!Lwt_mvar}, {!Lwt_pool}, and {!Lwt_switch}.

    Above that is the Lwt Unix binding, where I/O begins. This includes the
    module {!Lwt_main}, including the all-important {!Lwt_main.run}. The rest of
    the Unix binding consists of functions, each one of which...

    - ...starts a background I/O operation,
    - creates a promise for it and gives it to you,
    - registers with {!Lwt_main.run}, so if you attach callbacks to the promise,
      they will be called when the I/O operation completes.

    The functions are grouped into modules:

    - {!Lwt_unix} for Unix system calls.
    - {!Lwt_bytes} for Unix system calls on bigarrays.
    - {!Lwt_io} for [Pervasives]-like high-level channels, TCP servers, etc.
    - {!Lwt_process} for managing subprocesses.
    - {!Lwt_preemptive} for spawning system threads.
    - Miscellaneous modules {!Lwt_gc}, {!Lwt_engine}, {!Lwt_throttle},
      {!Lwt_timeout}, {!Lwt_sys}.

    Warning! Introductory material ends and detailed reference begins! *)



(** {2 Fundamentals} *)

(** {3 Promises} *)

type +'a t
(** Promises for values of type ['a].

    A {b promise} is a memory cell that is always in one of three {b states}:

    - {e fulfilled}, and containing one value of type ['a],
    - {e rejected}, and containing one exception, or
    - {e pending}, in which case it may become fulfilled or rejected later.

    A {e resolved} promise is one that is either fulfilled or rejected, i.e. not
    pending. Once a promise is resolved, its content cannot change. So, promises
    are {e write-once references}. The only possible state changes are (1) from
    pending to fulfilled and (2) from pending to rejected.

    Promises are typically “read” by attaching {b callbacks} to them. The most
    basic functions for that are {!Lwt.bind}, which attaches a callback that is
    called when a promise becomes fulfilled, and {!Lwt.catch}, for rejection.

    Promise variables of this type, ['a Lwt.t], are actually {b read-only} in
    Lwt. Separate {e resolvers} of type ['a ]{!Lwt.u} are used to write to them.
    Promises and their resolvers are created together by calling {!Lwt.wait}.
    There is one exception to this: most promises can be {e canceled} by calling
    {!Lwt.cancel}, without going through a resolver. *)

type -'a u
(** Resolvers for promises of type ['a ]{!Lwt.t}.

    Each resolver can be thought of as the {b write end} of one promise. It can
    be passed to {!Lwt.wakeup_later}, {!Lwt.wakeup_later_exn}, or
    {!Lwt.wakeup_later_result} to resolve that promise. *)

val wait : unit -> ('a t * 'a u)
(** Creates a new pending {{: #TYPEt} promise}, paired with its {{: #TYPEu}
    resolver}.

    It is rare to use this function directly. Many helpers in Lwt, and Lwt-aware
    libraries, call it internally, and return only the promise. You then chain
    the promises together using {!Lwt.bind}.

    However, it is important to understand [Lwt.wait] as the fundamental promise
    “constructor.” All other functions that evaluate to a promise can be, or
    are, eventually implemented in terms of it. *)



(** {3 Resolving} *)

val wakeup_later : 'a u -> 'a -> unit
(** [Lwt.wakeup_later r v] {e fulfills}, with value [v], the {e pending}
    {{: #TYPEt} promise} associated with {{: #TYPEu} resolver} [r]. This
    triggers callbacks attached to the promise.

    If the promise is not pending, [Lwt.wakeup_later] raises
    {{: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Stdlib.html#VALinvalid_arg}
    [Invalid_argument]}, unless the promise is {{: #VALcancel} canceled}. If the
    promise is canceled, [Lwt.wakeup_later] has no effect.

    If your program has multiple threads, it is important to make sure that
    [Lwt.wakeup_later] (and any similar function) is only called from the main
    thread. [Lwt.wakeup_later] can trigger callbacks attached to promises
    by the program, and these assume they are running in the main thread. If you
    need to communicate from a worker thread to the main thread running Lwt, see
    {!Lwt_preemptive} or {!Lwt_unix.send_notification}. *)

val wakeup_later_exn : _ u -> exn -> unit
(** [Lwt.wakeup_later_exn r exn] is like {!Lwt.wakeup_later}, except, if the
    associated {{: #TYPEt} promise} is {e pending}, it is {e rejected} with
    [exn]. *)

val return : 'a -> 'a t
(** [Lwt.return v] creates a new {{: #TYPEt} promise} that is {e already
    fulfilled} with value [v].

    This is needed to satisfy the type system in some cases. For example, in a
    [match] expression where one case evaluates to a promise, the other cases
    have to evaluate to promises as well:

{[
match need_input with
| true -> Lwt_io.(read_line stdin)   (* Has type string Lwt.t... *)
| false -> Lwt.return ""             (* ...so wrap empty string in a promise. *)
]}

    Another typical usage is in {{: #VALbind} [let%lwt]}. The expression after
    the “[in]” has to evaluate to a promise. So, if you compute an ordinary
    value instead, you have to wrap it:

{[
let%lwt line = Lwt_io.(read_line stdin) in
Lwt.return (line ^ ".")
]} *)

val fail : exn -> _ t
(** [Lwt.fail exn] is like {!Lwt.return}, except the new {{: #TYPEt} promise}
    that is {e already rejected} with [exn].

    Whenever possible, it is recommended to use [raise exn] instead, as [raise]
    captures a backtrace, while [Lwt.fail] does not. If you call [raise exn] in
    a callback that is expected by Lwt to return a promise, Lwt will
    automatically wrap [exn] in a rejected promise, but the backtrace will have
    been recorded by the OCaml runtime. Use [Lwt.fail] only when you
    specifically want to create a rejected promise, to pass to another function,
    or store in a data structure. *)



(** {3 Callbacks} *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [Lwt.bind p_1 f] makes it so that [f] will run when [p_1] is {{: #TYPEt}
    {e fulfilled}}.

    When [p_1] is fulfilled with value [v_1], the callback [f] is called with
    that same value [v_1]. Eventually, after perhaps starting some I/O or other
    computation, [f] returns promise [p_2].

    [Lwt.bind] itself returns immediately. It only attaches the callback [f] to
    [p_1] – it does not wait for [p_2]. {e What} [Lwt.bind] returns is yet a
    third promise, [p_3]. Roughly speaking, fulfillment of [p_3] represents both
    [p_1] and [p_2] becoming fulfilled, one after the other.

    A minimal example of this is an echo program:

{[
let () =
  let p_3 =
    Lwt.bind
      Lwt_io.(read_line stdin)
      (fun line -> Lwt_io.printl line)
  in
  Lwt_main.run p_3

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    Rejection of [p_1] and [p_2], and raising an exception in [f], are all
    forwarded to rejection of [p_3].

    {b Precise behavior}

    [Lwt.bind] returns a promise [p_3] immediately. [p_3] starts out pending,
    and is resolved as follows:

    - The first condition to wait for is that [p_1] becomes resolved. It does
      not matter whether [p_1] is already resolved when [Lwt.bind] is called, or
      becomes resolved later – the rest of the behavior is the same.
    - If and when [p_1] becomes resolved, it will, by definition, be either
      fulfilled or rejected.
    - If [p_1] is rejected, [p_3] is rejected with the same exception.
    - If [p_1] is fulfilled, with value [v], [f] is applied to [v].
    - [f] may finish by returning the promise [p_2], or raising an exception.
    - If [f] raises an exception, [p_3] is rejected with that exception.
    - Finally, the remaining case is when [f] returns [p_2]. From that point on,
      [p_3] is effectively made into a reference to [p_2]. This means they have
      the same state, undergo the same state changes, and performing any
      operation on one is equivalent to performing it on the other.

    {b Syntactic sugar}

    [Lwt.bind] is almost never written directly, because sequences of [Lwt.bind]
    result in growing indentation and many parentheses:

{[
let () =
  Lwt_main.run begin
    Lwt.bind Lwt_io.(read_line stdin) (fun line ->
      Lwt.bind (Lwt_unix.sleep 1.) (fun () ->
        Lwt_io.printf "One second ago, you entered %s\n" line))
  end

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    The recommended way to write [Lwt.bind] is using the [let%lwt] syntactic
    sugar:

{[
let () =
  Lwt_main.run begin
    let%lwt line = Lwt_io.(read_line stdin) in
    let%lwt () = Lwt_unix.sleep 1. in
    Lwt_io.printf "One second ago, you entered %s\n" line
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    This uses the Lwt {{: Ppx_lwt.html} PPX} (preprocessor). Note that we had to
    add package [lwt_ppx] to the command line for building this program. We will
    do that throughout this manual.

    Another way to write [Lwt.bind], that you may encounter while reading code,
    is with the [>>=] operator:

{[
open Lwt.Infix

let () =
  Lwt_main.run begin
    Lwt_io.(read_line stdin) >>= fun line ->
    Lwt_unix.sleep 1. >>= fun () ->
    Lwt_io.printf "One second ago, you entered %s\n" line
  end

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    The [>>=] operator comes from the module {!Lwt.Infix}, which is why we
    opened it at the beginning of the program.

    See also {!Lwt.map}. *)



(** {2 Rejection} *)

val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
(** [Lwt.catch f h] applies [f ()], which returns a promise, and then makes it
    so that [h] (“handler”) will run when that promise is {{: #TYPEt}
    {e rejected}}.

{[
let () =
  Lwt_main.run begin
    Lwt.catch
      (fun () -> Lwt.fail Exit)
      (function
      | Exit -> Lwt_io.printl "Got Stdlib.Exit"
      | exn -> Lwt.fail exn)
  end

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    Despite the above code, the recommended way to write [Lwt.catch] is using
    the [try%lwt] syntactic sugar from the {{: Ppx_lwt.html} PPX}. Here is an
    equivalent example:

{[
let () =
  Lwt_main.run begin
    try%lwt Lwt.fail Exit
    with Exit -> Lwt_io.printl "Got Stdlb.Exit"
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    A particular advantage of the PPX syntax is that it is not necessary to
    artificially insert a catch-all [exn -> Lwt.fail exn] case. Like in the core
    language's [try] expression, the catch-all case is implied in [try%lwt].

    [Lwt.catch] is a counterpart to {!Lwt.bind} – {!Lwt.bind} is for
    fulfillment, and {!Lwt.catch} is for rejection.

    As with {!Lwt.bind}, three promises are involved:

    - [p_1], the promise returned from applying [f ()].
    - [p_2], the promise returned from applying [h exn].
    - [p_3], the promise returned by [Lwt.catch] itself.

    The remainder is (1) a precise description of how [p_3] is resolved, and
    (2) a warning about accidentally using ordinary [try] for exception handling
    in asynchronous code.

    {b (1)} [Lwt.catch] first applies [f ()]. It then returns [p_3] immediately.
    [p_3] starts out pending. It is resolved as follows:

    - If [f ()] returned a promise [p_1], and [p_1] becomes fulfilled, [p_3] is
      fulfilled with the same value.
    - [p_1] can instead become rejected. There is one other possibility: [f ()]
      itself raised an exception, instead of returning a promise. The behavior
      of [Lwt.catch] is the same whether [f ()] raised an exception, or returned
      a promise that is later rejected with an exception. Let's call the
      exception [exn].
    - [h exn] is applied.
    - [h exn] may return a promise, or might itself raise an exception. The
      first case is the interesting one, but the exception case is simple, so we
      cover the exception case first.
    - If [h exn] raises another exception [exn'], [p_3] is rejected with [exn'].
    - If [h exn] instead returns the promise [p_2], [p_3] is effectively made
      into a reference to [p_2]. This means [p_3] and [p_2] have the same state,
      undergo the same state changes, and performing any operation one is
      equivalent to performing it on the other.

    {b (2)} {b Warning}: it may be tempting to write this code, which differs
    from the second example above only in that [try] is used instead of
    [try%lwt]:

{[
let () =
  Lwt_main.run begin
    try Lwt.fail Exit
    with Exit -> Lwt_io.printl "Got Stdlib.Exit"
  end

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    This does {e not} handle the exception and does not print the message.
    Instead, it terminates the program with an unhandled [Stdlib.Exit].

    This is because the call to {!Lwt.fail} creates a rejected promise. The
    promise is still an ordinary OCaml value, though, and not a {e raised}
    exception. So, [try] considers that code to have succeeded, and doesn't run
    the handler. When that rejected promise reaches {!Lwt_main.run},
    it is {!Lwt_main.run} that raises the exception.

    Basically, the rule is: if the code inside [try] evaluates to a promise
    (has type [_ Lwt.t]), replace [try] by [try%lwt]. *)

val finalize : (unit -> 'a t) -> (unit -> unit t) -> 'a t
(** [Lwt.finalize f c] applies [f ()], which returns a promise, and then makes
    it so [c] (“cleanup”) will run when that promise is {{: #TYPEt}
    {e resolved}}.

    In other words, [c] runs no matter whether promise [f ()] is fulfilled or
    rejected. As the names suggest, [Lwt.finalize] corresponds to the [finally]
    construct found in many programming languages, and [c] is typically used for
    cleaning up resources:

{[
let () =
  Lwt_main.run begin
    let%lwt file = Lwt_io.(open_file ~mode:Input "code.ml") in
    Lwt.finalize
      (fun () ->
        let%lwt content = Lwt_io.read file in
        Lwt_io.print content)
      (fun () ->
        Lwt_io.close file)
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    As with {!Lwt.bind} and {!Lwt.catch}, there is a syntactic sugar for
    [Lwt.finalize], though it is not as often used:

{[
let () =
  Lwt_main.run begin
    let%lwt file = Lwt_io.(open_file ~mode:Input "code.ml") in
    begin
      let%lwt content = Lwt_io.read file in
      Lwt_io.print content
    end
    [%lwt.finally
      Lwt_io.close file]
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    Also as with {!Lwt.bind} and {!Lwt.catch}, three promises are involved:

    - [p_1], the promise returned from applying [f ()].
    - [p_2], the promise returned from applying [c ()].
    - [p_3], the promise returned by [Lwt.finalize] itself.

    [p_3] is returned immediately. It starts out pending, and is resolved as
    follows:

    - [f ()] is applied. If it finishes, it will either return a promise [p_1],
      or raise an exception.
    - If [f ()] raises an exception, [p_1] is created artificially as a promise
      rejected with that exception. So, no matter how [f ()] finishes, there is
      a promise [p_1] representing the outcome.
    - After [p_1] is resolved (fulfilled or rejected), [c ()] is applied. This
      is meant to be the cleanup code.
    - If [c ()] finishes, it will also either return a promise, [p_2], or raise
      an exception.
    - If [c ()] raises an exception, [p_2] is created artificially as a promise
      rejected with that exception. Again, no matter how [c ()] finishes, there
      is a promise [p_2] representing the outcome of cleanup.
    - If [p_2] is fulfilled, [p_3] is resolved the same way [p_1] had been
      resolved. In other words, [p_1] is forwarded to [p_2] when cleanup is
      successful.
    - If [p_2] is rejected, [p_3] is rejected with the same exception. In other
      words, when cleanup fails, [p_3] is rejected. Note this means that if
      {e both} the protected code and the cleanup fail, the cleanup exception
      has precedence. *)

val try_bind : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
(** [Lwt.try_bind f g h] applies [f ()], and then makes it so that:

    - [g] will run when promise [f ()] is {{: #TYPEt} {e fulfilled}},
    - [h] will run when promise [f ()] is {{: #TYPEt} {e rejected}}.

    [Lwt.try_bind] is a generalized {!Lwt.finalize}. The difference is that
    [Lwt.try_bind] runs different callbacks depending on {e how} [f ()] is
    resolved. This has two main implications:

    - The cleanup functions [g] and [h] each “know” whether [f ()] was fulfilled
      or rejected.
    - The cleanup functions [g] and [h] are passed the value [f ()] was
      fulfilled with, and, respectively, the exception [f ()] was rejected
      with.

    The rest is a detailed description of the promises involved.

    As with {!Lwt.finalize} and the several preceding functions, three promises
    are involved.

    - [p_1] is the promise returned from applying [f ()].
    - [p_2] is the promise returned from applying [h] or [g], depending on which
      one is chosen.
    - [p_3] is the promise returned by [Lwt.try_bind] itself.

    [Lwt.try_bind] returns [p_3] immediately. [p_3] starts out pending, and is
    resolved as follows:

    - [f ()] is applied. If it finishes, it either returns [p_1], or raises an
      exception.
    - If [f ()] raises an exception, [p_1] is created artificially as a promise
      rejected with that exception. So, no matter how [f ()] finishes, there is
      a promise [p_1] representing the outcome.
    - If [p_1] is fulfilled, [g] is applied to the value [p_1] is fulfilled
      with.
    - If [p_1] is rejected, [h] is applied to the exception [p_1] is rejected
      with.
    - So, in either case, a callback is applied. The rest of the procedure is
      the same no matter which callback was chosen, so we will refer to it as
      “the callback.”
    - If the callback finishes, it either returns [p_2], or raises an exception.
    - If the callback raises an exception, [p_3] is rejected with that
      exception.
    - If the callback returns [p_2], [p_3] is effectively made into an reference
      to [p_2]. They have the same state, including any state changes, and
      performing any operation on one is equivalent to performing it on the
      other. *)

val async : (unit -> unit t) -> unit
(** [Lwt.async f] applies [f ()], which returns a promise, and then makes it so
    that if the promise is {{: #TYPEt} {e rejected}}, the exception is passed to
    [!]{!Lwt.async_exception_hook}.

    In addition, if [f ()] raises an exception, it is also passed to
    [!]{!Lwt.async_exception_hook}.

    [!]{!Lwt.async_exception_hook} typically prints an error message and
    terminates the program.

    [Lwt.async] is misleadingly named. Itself, it has nothing to do with
    asynchronous execution. It's actually a safety function for making Lwt
    programs more debuggable.

    For example, take this program, which prints messages in a loop, while
    waiting for one line of user input:

{[
let () =
  let rec show_nag () : _ Lwt.t =
    let%lwt () = Lwt_io.printl "Please enter a line" in
    let%lwt () = Lwt_unix.sleep 1. in
    show_nag ()
  in
  ignore (show_nag ());     (* Bad – see note for (1)! *)

  Lwt_main.run begin
    let%lwt line = Lwt_io.(read_line stdin) in
    Lwt_io.printl line
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    If one of the I/O operations in [show_nag] were to fail, the promise
    representing the whole loop would get rejected. However, since we are
    ignoring that promise at {b (1)}, we never find out about the rejection. If
    this failure and resulting rejection represents a bug in the program, we
    have a harder time finding out about the bug.

    A safer version differs only in using [Lwt.async] instead of
    [Stdlib.ignore]:

{[
let () =
  let rec show_nag () : _ Lwt.t =
    let%lwt () = Lwt_io.printl "Please enter a line" in
    let%lwt () = Lwt_unix.sleep 1. in
    show_nag ()
  in
  Lwt.async (fun () -> show_nag ());

  Lwt_main.run begin
    let%lwt line = Lwt_io.(read_line stdin) in
    Lwt_io.printl line
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    In this version, if I/O in [show_nag] fails with an exception, the exception
    is printed by [Lwt.async], and then the program exits.

    The general rule for when to use [Lwt.async] is:

    - Promises which are {e not} passed {e to} {!Lwt.bind}, {!Lwt.catch},
      {!Lwt.join}, etc., are {b top-level} promises.
    - One top-level promise is passed to {!Lwt_main.run}, as can be seen in most
      examples in this manual.
    - Every other top-level promise should be wrapped in [Lwt.async]. *)

val async_exception_hook : (exn -> unit) ref
(** Reference to a function, to be called on an "unhandled" exception.

    This reference is used by {!Lwt.async}, {!Lwt.on_cancel}, {!Lwt.on_success},
    {!Lwt.on_failure}, {!Lwt.on_termination}, {!Lwt.on_any}, and the deprecated
    {!Lwt.ignore_result}.

    The initial, default implementation prints the exception, then terminates
    the process with non-zero exit status, as if the exception had reached the
    top level of the program:

{[
let () = Lwt.async (fun () -> Lwt.fail Exit)

(* ocamlfind opt -linkpkg -package lwt code.ml && ./a.out *)
]}

    produces in the output:

{v
Fatal error: exception Stdlib.Exit
v}

    If you are writing an application, you are welcome to reassign the
    reference, and replace the function with something more appropriate for your
    needs.

    If you are writing a library, you should leave this reference alone. Its
    behavior should be determined by the application. *)



(** {2 Concurrency} *)

(** {3 Multiple wait} *)

val both : 'a t -> 'b t -> ('a * 'b) t
(** [Lwt.both p_1 p_2] returns a promise that is pending until {e both} promises
    [p_1] and [p_2] become {{: #TYPEt} {e resolved}}.

{[
let () =
  let p_1 =
    let%lwt () = Lwt_unix.sleep 3. in
    Lwt_io.printl "Three seconds elapsed"
  in

  let p_2 =
    let%lwt () = Lwt_unix.sleep 5. in
    Lwt_io.printl "Five seconds elapsed"
  in

  let p_3 = Lwt.both p_1 p_2 in
  Lwt_main.run p_3

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    If both [p_1] and [p_2] become fulfilled, [Lwt.both p_1 p_2] is also
    fulfilled, with the pair of their final values. Otherwise, if at least one
    of the two promises becomes rejected, [Lwt.both p_1 p_2] is rejected with
    the same exception as one such promise, chosen arbitrarily. Note that this
    occurs only after both promises are resolved, not immediately when the first
    promise is rejected.

    @since 4.2.0 *)

val join : (unit t) list -> unit t
(** [Lwt.join ps] returns a promise that is pending until {e all} promises in
    the list [ps] become {{: #TYPEt} {e resolved}}.

{[
let () =
  let p_1 =
    let%lwt () = Lwt_unix.sleep 3. in
    Lwt_io.printl "Three seconds elapsed"
  in

  let p_2 =
    let%lwt () = Lwt_unix.sleep 5. in
    Lwt_io.printl "Five seconds elapsed"
  in

  let p_3 = Lwt.join [p_1; p_2] in
  Lwt_main.run p_3

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    If all of the promises in [ps] become fulfilled, [Lwt.join ps] is also
    fulfilled. Otherwise, if at least one promise in [ps] becomes rejected,
    [Lwt.join ps] is rejected with the same exception as one such promise,
    chosen arbitrarily. Note that this occurs only after all the promises are
    resolved, not immediately when the first promise is rejected. *)

val all : ('a t) list -> ('a list) t
(** [Lwt.all ps] is like {!Lwt.join}[ ps]: it waits for all promises in the list
    [ps] to become {{: #TYPEt} {e resolved}}.

    It then resolves the returned promise with the list of all resulting values.

    Note that if any of the promises in [ps] is rejected, the returned promise
    is also rejected. This means that none of the values will be available, even
    if some of the promises in [ps] were already resolved when one of them is
    rejected. For more fine-grained handling of rejection, structure the program
    with {!Lwt_stream} or {!Lwt_list}, handle rejections explicitly, or use
    {!Lwt.join} and collect values manually.

    @since 5.1.0 *)



(** {3 Racing} *)

val pick : ('a t) list -> 'a t
(** [Lwt.pick ps] returns a promise that is pending until {e one} promise in
    the list [ps] becomes {{: #TYPEt} {e resolved}}.

    When at least one promise in [ps] is resolved, [Lwt.pick] tries to cancel
    all other promises that are still pending, using {!Lwt.cancel}.

{[
let () =
  let echo =
    let%lwt line = Lwt_io.(read_line stdin) in
    Lwt_io.printl line
  in

  let timeout = Lwt_unix.sleep 5. in

  Lwt_main.run (Lwt.pick [echo; timeout])

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    If the first promise in [ps] to become resolved is fulfilled, the result
    promise [p] is also fulfilled, with the same value. Likewise, if the first
    promise in [ps] to become resolved is rejected, [p] is rejected with the
    same exception.

    If [ps] has no promises (if it is the empty list), [Lwt.pick ps] raises
    [Stdlib.Invalid_argument _].

    It's possible for multiple promises in [ps] to become resolved
    simultaneously. This happens most often when some promises [ps] are already
    resolved at the time [Lwt.pick] is called.

    In that case, if at least one of the promises is rejected, the result
    promise [p] is rejected with the same exception as one such promise, chosen
    arbitrarily. If all promises are fulfilled, [p] is fulfilled with the value
    of one of the promises, also chosen arbitrarily.

    The remaining functions in this section are variations on [Lwt.pick]. *)

val choose : ('a t) list -> 'a t
(** [Lwt.choose ps] is the same as {!Lwt.pick}[ ps], except that it does not try
    to cancel pending promises in [ps]. *)

val npick : ('a t) list -> ('a list) t
(** [Lwt.npick ps] is similar to {!Lwt.pick}[ ps], the difference being that
    when multiple promises in [ps] are fulfilled simultaneously (and none are
    rejected), the result promise is fulfilled with the {e list} of values the
    promises were fulfilled with.

    When at least one promise is rejected, [Lwt.npick] still rejects the result
    promise with the same exception. *)

val nchoose : ('a t) list -> ('a list) t
(** [Lwt.nchoose ps] is the same as {!Lwt.npick}[ ps], except that it does not
    try to cancel pending promises in [ps]. *)

val nchoose_split : ('a t) list -> ('a list * ('a t) list) t
(** [Lwt.nchoose_split ps] is the same as {!Lwt.nchoose}[ ps], except that when
    multiple promises in [ps] are fulfilled simultaneously (and none are
    rejected), the result promise is fulfilled with {e both} the list of values
    of the fulfilled promises, and the list of promises that are still
    pending. *)



(** {2 Cancellation}

    Note: cancelation has proved difficult to understand, explain, and maintain,
    so use of these functions is discouraged in new code. See
    {{:https://github.com/ocsigen/lwt/issues/283#issuecomment-518014539}
    ocsigen/lwt#283}. *)

exception Canceled
(** Canceled promises are those rejected with this exception, [Lwt.Canceled].
    See {!Lwt.cancel}. *)

val task : unit -> ('a t * 'a u)
(** [Lwt.task] is the same as {!Lwt.wait}, except the resulting promise [p] is
    {{: #VALcancel} cancelable}.

    This is significant, because it means promises created by [Lwt.task] can be
    resolved (specifically, rejected) by canceling them directly, in addition to
    being resolved through their paired resolvers.

    In contrast, promises returned by {!Lwt.wait} can only be resolved through
    their resolvers. *)

val cancel : _ t -> unit
(** [Lwt.cancel p] attempts to {e cancel} the pending promise [p], without
    needing access to its resolver.

    It is recommended to avoid [Lwt.cancel], and handle cancelation by tracking
    the needed extra state explicitly within your library or application.

    A {b canceled} promise is one that has been rejected with exception
    {!Lwt.Canceled}.

    There are straightforward ways to make promises canceled. One could create a
    promise that {e starts out} canceled, with {!Lwt.fail}[ Lwt.Canceled]. It's
    also possible to {e make} a promise canceled through its resolver, by
    calling {!Lwt.wakeup_later_exn}[ r Lwt.Canceled].

    This function, [Lwt.cancel], provides another method, which can cancel
    pending promises {e without} going through their resolvers – it acts
    directly on promises.

    Like any other promise rejection, the canceled state of a promise is
    propagated “forwards” by {!Lwt.bind}, {!Lwt.join}, etc., as described in the
    documentation of those functions.

    {b Cancellation} is a separate phase, triggered only by {!Lwt.cancel}, that
    searches {e backwards}, strating from [p], for promises to reject with
    {!Lwt.Canceled}. Once those promises are found, they are canceled, and then
    ordinary, forwards rejection propagation takes over.

    All of this will be made precise, but first let's have an example:

{[
let () =
  let p =
    let%lwt () = Lwt_unix.sleep 5. in
    Lwt_io.printl "Slept five seconds"
  in

  Lwt.cancel p;

  Lwt_main.run p

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    At the time [Lwt.cancel] is called, [p] “depends” on the [sleep] promise
    (the [printl] is not yet called, so its promise hasn't been created).

    So, {!Lwt.cancel} recursively tries to cancel the [sleep] promise. That is
    an example of the backwards search. The [sleep] promise is a pending promise
    that doesn't depend on anything, so backwards search stops at it. The state
    of the [sleep] promise is set to {e rejected} with {!Lwt.Canceled}.

    {!Lwt.bind} then propagates the rejection forwards to [p], so [p] also
    becomes canceled.

    Eventually, this rejection reaches {!Lwt_main.run}, which raises the
    {!Lwt.Canceled} as an ordinary exception. The [sleep] does not complete, and
    the [printl] is never started.

    Promises, like the [sleep] promise above, that can be rejected by
    [Lwt.cancel] are {b cancelable}. Most promises in Lwt are either cancelable,
    or depend on cancelable promises. The functions {!Lwt.wait} and
    {!Lwt.no_cancel} create promises that are {e not} cancelable.

    The rest is a detailed description of how the [Lwt.cancel] backwards search
    works.

    - If [p] is already resolved, [Lwt.cancel] does nothing.
    - If [p] was created by {!Lwt.wait} or {!Lwt.no_cancel}, [Lwt.cancel] does
      nothing.
    - If [p] was created by {!Lwt.task} or {!Lwt.protected}, [Lwt.cancel]
      rejects it with [Lwt.Canceled]. This rejection then propagates normally
      through any Lwt calls that depend on [p]. Most I/O promises are internally
      created by calling {!Lwt.task}.
    - Suppose [p_3] was returned by {!Lwt.bind}, {!Lwt.map}, {!Lwt.catch},
      {!Lwt.finalize}, or {!Lwt.try_bind}. Then, see those functions for the
      naming of the other promises involved. If [p_3] is pending, then either
      [p_1] is pending, or [p_2] is pending. [Lwt.cancel p_3] then tries
      recursively to cancel whichever of these two is still pending. If that
      succeeds, [p_3] {e may} be canceled later by the normal propagation of
      rejection.
    - Suppose [p] was returned by {!Lwt.join}, {!Lwt.pick}, or similar function,
      which was applied to the promise list [ps]. {!Lwt.cancel} then recursively
      tries to cancel each promise in [ps]. If one of those cancellations
      succeeds, [p] {e may} be canceled later by the normal propagation of
      rejection. *)

val on_cancel : _ t -> (unit -> unit) -> unit
(** [Lwt.on_cancel p f] makes it so that [f] will run when [p] becomes
    {{: #EXCEPTIONCanceled} {e canceled}}.

    Callbacks scheduled with [on_cancel] are guaranteed to run before any other
    callbacks that are triggered by rejection, such as those added by
    {!Lwt.catch}.

    Note that this does not interact directly with the {e cancellation}
    mechanism, the backwards search described in {!Lwt.cancel}. For example,
    manually rejecting a promise with {!Lwt.Canceled} is sufficient to trigger
    [f].

    [f] should not raise exceptions. If it does, they are passed to
    [!]{!Lwt.async_exception_hook}, which terminates the process by default. *)

val protected : 'a t -> 'a t
(** [Lwt.protected p] creates a {{: #VALcancel} cancelable} promise [p'] with
    the same state as [p]. However, cancellation, the backwards search described
    in {!Lwt.cancel}, stops at [p'], and does not continue to [p]. *)

val no_cancel : 'a t -> 'a t
(** [Lwt.no_cancel p] creates a non-{{: #VALcancel}cancelable} promise [p'],
    with the same state as [p]. Cancellation, the backwards search described in
    {!Lwt.cancel}, stops at [p'], and does not continue to [p].

    Note that [p'] can still be canceled if [p] is canceled. [Lwt.no_cancel]
    only prevents cancellation of [p] and [p'] through [p']. *)



(** {2 Convenience} *)

(** {3 Callback helpers} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [Lwt.map f p_1] is similar to {!Lwt.bind}[ p_1 f], but [f] is not expected
    to return a promise.

    This function is more convenient that {!Lwt.bind} when [f] inherently does
    not return a promise. An example is [Stdlib.int_of_string]:

{[
let read_int : unit -> int Lwt.t = fun () ->
  Lwt.map
    int_of_string
    Lwt_io.(read_line stdin)

let () =
  Lwt_main.run begin
    let%lwt number = read_int () in
    Lwt_io.printf "%i\n" number
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    By comparison, the {!Lwt.bind} version is more awkward:

{[
let read_int : unit -> int Lwt.t = fun () ->
  Lwt.bind
    Lwt_io.(read_line stdin)
    (fun line -> Lwt.return (int_of_string line))
]}

    As with {!Lwt.bind}, sequences of calls to [Lwt.map] result in excessive
    indentation and parentheses. The recommended syntactic sugar for avoiding
    this is the {{: #VAL(>|=)} [>|=]} operator, which comes from module
    [Lwt.Infix]:

{[
open Lwt.Infix

let read_int : unit -> int Lwt.t = fun () ->
  Lwt_io.(read_line stdin) >|= int_of_string
]}

    The detailed operation follows. For consistency with the promises in
    {!Lwt.bind}, the {e two} promises involved are named [p_1] and [p_3]:

    - [p_1] is the promise passed to [Lwt.map].
    - [p_3] is the promise returned by [Lwt.map].

    [Lwt.map] returns a promise [p_3]. [p_3] starts out pending. It is resolved
    as follows:

    - [p_1] may be, or become, resolved. In that case, by definition, it will
      become fulfilled or rejected. Fulfillment is the interesting case, but the
      behavior on rejection is simpler, so we focus on rejection first.
    - When [p_1] becomes rejected, [p_3] is rejected with the same exception.
    - When [p_1] instead becomes fulfilled, call the value it is fulfilled with
      [v].
    - [f v] is applied. If this finishes, it may either return another value, or
      raise an exception.
    - If [f v] returns another value [v'], [p_3] is fulfilled with [v'].
    - If [f v] raises exception [exn], [p_3] is rejected with [exn]. *)

val on_success : 'a t -> ('a -> unit) -> unit
(** [Lwt.on_success p f] makes it so that [f] will run when [p] is {{: #TYPEt}
    {e fulfilled}}.

    It is similar to {!Lwt.bind}, except no new promises are created. [f] is a
    plain, arbitrary function attached to [p], to perform some side effect.

    If [f] raises an exception, it is passed to [!]{!Lwt.async_exception_hook}.
    By default, this will terminate the process. *)

val on_failure : _ t -> (exn -> unit) -> unit
(** [Lwt.on_failure p f] makes it so that [f] will run when [p] is {{: #TYPEt}
    {e rejected}}.

    It is similar to {!Lwt.catch}, except no new promises are created.

    If [f] raises an exception, it is passed to [!]{!Lwt.async_exception_hook}.
    By default, this will terminate the process. *)

val on_termination : _ t -> (unit -> unit) -> unit
(** [Lwt.on_termination p f] makes it so that [f] will run when [p] is
    {{: #TYPEt} {e resolved}} – that is, fulfilled {e or} rejected.

    It is similar to {!Lwt.finalize}, except no new promises are created.

    If [f] raises an exception, it is passed to [!]{!Lwt.async_exception_hook}.
    By default, this will terminate the process. *)

val on_any : 'a t -> ('a -> unit) -> (exn -> unit) -> unit
(** [Lwt.on_any p f g] makes it so that:

    - [f] will run when [p] is {{: #TYPEt} {e fulfilled}},
    - [g] will run when [p] is, alternatively, {{: #TYPEt} {e rejected}}.

    It is similar to {!Lwt.try_bind}, except no new promises are created.

    If [f] or [g] raise an exception, the exception is passed to
    [!]{!Lwt.async_exception_hook}. By default, this will terminate the
    process. *)



(** {3 Infix operators} *)

(** This module provides several infix operators for making programming with
    Lwt more convenient.

    To use it, open [Lwt.Infix].

    Of the operators declared in this module, only [>|=] is recommended for new
    code. The only other commonly-used operator is [>>=]. *)
module Infix :
sig
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** [p >>= f] is the same as {!Lwt.bind}[ p f]. It requires [Lwt.Infix] to be
      opened in scope:

{[
open Lwt.Infix

let () =
  Lwt_main.run
    (Lwt_io.(read_line stdin) >>= Lwt_io.printl)

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

      It is recommended to use the PPX [let%lwt] syntax instead. This operator
      is the next-best choice. It is frequently found while reading existing
      Lwt code. *)

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** [p >|= f] is the same as {!Lwt.map}[ f p]. It requires [Lwt.Infix] to be
      opened in scope.

{[
open Lwt.Infix

let () =
  Lwt_main.run
    (Lwt_io.(read_line stdin) >|= ignore)

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]} *)

  val (<&>) : unit t -> unit t -> unit t
  (** [p1 <&> p2] is the same as {!Lwt.join}[ [p1; p2]]. It requires [Lwt.Infix]
      to be opened in scope.

      Unlike with {!Lwt.bind} and {!Lwt.map}, there are no problems with
      explicit {!Lwt.join} syntax, so using this operator is not recommended. *)

  val (<?>) : 'a t -> 'a t -> 'a t
  (** [p1 <?> p2] is the same as {!Lwt.choose}[ [p1; p2]]. It requires
      [Lwt.Infix] to be opened in scope.

      Unlike with {!Lwt.bind} and {!Lwt.join}, there are no problems with
      explicit {!Lwt.choose} syntax, so using this operator is not
      recommended.

      Furthermore, most users actually need {!Lwt.pick} instead of
      {!Lwt.choose}. *)

  val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
  (** [f =<< p] is the same as {!Lwt.bind}[ p f]. It requires [Lwt.Infix] to be
      opened in scope.

      This operator is obscure and its use is discouraged. It is the same as
      [p >>= f]. *)

  val (=|<) : ('a -> 'b) -> 'a t -> 'b t
  (** [f =|< p] is the same as {!Lwt.map}[ f p]. It requires [Lwt.Infix] to be
      opened in scope.

      This operator is obscure and its use is discouraged. It is the same as
      [p >|= f]. *)

  (** This module provides support for {{:https://github.com/janestreet/ppx_let}
      ppx_let}.

      @since 4.2.0 *)
  module Let_syntax :
  sig
    val return : 'a -> 'a t
    (** See {!Lwt.return}. *)

    val map : 'a t -> f:('a -> 'b) -> 'b t
    (** See {!Lwt.map}. *)

    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    (** See {!Lwt.bind}. *)

    val both : 'a t -> 'b t -> ('a * 'b) t
    (** See {!Lwt.both}. *)

    module Open_on_rhs :
    sig
    end
  end
end

(** {3 Pre-allocated promises} *)

val return_unit : unit t
(** [Lwt.return_unit] is defined as {!Lwt.return}[ ()], but this definition is
    evaluated only once, during initialization of module [Lwt], at the beginning
    of your program.

    This means the promise is allocated only once. By contrast, each time
    {!Lwt.return}[ ()] is evaluated, it allocates a new promise.

    It is recommended to use [Lwt.return_unit] only where you know the
    allocations caused by an instance of {!Lwt.return}[ ()] are a performance
    bottleneck. Generally, the cost of I/O tends to dominate the cost of
    {!Lwt.return}[ ()] anyway.

    In future Lwt, we hope to perform this optimization, of using a single,
    pre-allocated promise, automatically, wherever {!Lwt.return}[ ()] is
    written. *)

val return_none : (_ option) t
(** [Lwt.return_none] is like {!Lwt.return_unit}, but for
    {!Lwt.return}[ None]. *)

val return_nil : (_ list) t
(** [Lwt.return_nil] is like {!Lwt.return_unit}, but for {!Lwt.return}[ []]. *)

val return_true : bool t
(** [Lwt.return_true] is like {!Lwt.return_unit}, but for
    {!Lwt.return}[ true]. *)

val return_false : bool t
(** [Lwt.return_false] is like {!Lwt.return_unit}, but for
    {!Lwt.return}[ false]. *)



(** {3 Result type} *)

type +'a result = ('a, exn) Result.result
(** Representation of the content of a resolved promise of type
    ['a ]{!Lwt.t}.

    This type is effectively

{[
type +'a Lwt.result =
  | Ok of 'a
  | Error of exn
]}

    or, on OCaml 4.02:

{[
type +'a Lwt.result =
  | Result.Ok of 'a
  | Result.Error of exn
]}

    A resolved promise of type ['a ]{!Lwt.t} is either fulfilled with a value of
    type ['a], or rejected with an exception.

    This corresponds to the cases of a
    [('a, exn)]{{: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Stdlib.html#TYPEresult}[Stdlib.result]}:
    fulfilled corresponds to [Ok of 'a], and rejected corresponds to
    [Error of exn].

    It's important to note that this type constructor, [Lwt.result], is
    different from [Stdlib.result]. It is a specialization of [Stdlib.result] so
    that the [Error] constructor always carries [exn].

    For Lwt programming with [result] where the [Error] constructor can carry
    arbitrary error types, see module {!Lwt_result}.

    The naming conflict between [Lwt.result] and [Stdlib.result] is an
    unfortunate historical accident. [Stdlib.result] did not exist when
    [Lwt.result] was created.

    The type [Result.result] is equivalent to [Stdlib.result] starting from
    OCaml 4.03. If you need compatibility with OCaml 4.02, refer to
    [Stdlib.result] as [Result.result], and prefix the constructor names with
    [Result], as shown in the second example. *)

val of_result : 'a result -> 'a t
(** [Lwt.of_result r] converts an r to a resolved promise.

    - If [r] is [Ok v], [Lwt.of_result r] is [Lwt.return v], i.e. a promise
      fulfilled with [v].
    - If [r] is [Error exn], [Lwt.of_result r] is [Lwt.fail exn], i.e. a promise
      rejected with [exn]. *)

val wakeup_later_result : 'a u -> 'a result -> unit
(** [Lwt.wakeup_later_result r result] resolves the pending promise [p]
    associated to resolver [r], according to [result]:

    - If [result] is [Ok v], [p] is fulfilled with [v].
    - If [result] is [Error exn], [p] is rejected with [exn].

    If [p] is not pending, [Lwt.wakeup_later_result] raises
    [Stdlib.Invalid_argument _], except if [p] is {{: #VALcancel} canceled}. If
    [p] is canceled, [Lwt.wakeup_later_result] has no effect. *)



(** {3 State query} *)

type 'a state =
  | Return of 'a
  | Fail of exn
  | Sleep

val state : 'a t -> 'a state
(** [Lwt.state p] evaluates to the current state of promise [p]:

    - If [p] is {{: #TYPEt} fulfilled} with value [v], the result is
      [Lwt.Return v].
    - If [p] is {{: #TYPEt} rejected} with exception [exn], the result is
      [Lwt.Fail exn].
    - If [p] is {{: #TYPEt} pending}, the result is [Lwt.Sleep].

    The constructor names are historical holdovers. *)



(** {2 Deprecated} *)

(** {3 Implicit callback arguments}

    Using this mechanism is discouraged, because it is non-syntactic, and
    because it manipulates hidden state in module [Lwt]. It is recommended
    instead to pass additional values explicitly in tuples, or maintain explicit
    associative maps for them. *)

type 'a key
(** Keys into the implicit callback argument map, for implicit arguments of type
    ['a option].

    The keys are abstract, but they are basically integers that are all distinct
    from each other.

    See {!Lwt.with_value}. *)

val new_key : unit -> 'a key
(** Creates a fresh implicit callback argument key.

    The key is distinct from any other key created by the current process. The
    value [None] of type ['a option] is immediately associated with the key.

    See {!Lwt.with_value}. *)

val get : 'a key -> 'a option
(** Retrieves the value currently associated with the given implicit callback
    argument key.

    See {!Lwt.with_value}. *)

val with_value : 'a key -> 'a option -> (unit -> 'b) -> 'b
(** [Lwt.with_value k v f] sets [k] to [v] in Lwt's internal implicit callback
    argument map, then runs [f ()], then restores the previous value associated
    with [k].

    Lwt maintains a single, global map, that can be used to “pass” extra
    arguments to callbacks:

{[
let () =
  let k : string Lwt.key = Lwt.new_key () in

  let say_hello () =
    match Lwt.get k with
    | None -> assert false
    | Some s -> Lwt_io.printl s
  in

  Lwt_main.run begin
    Lwt.with_value k (Some "Hello world!") begin fun () ->
      Lwt.bind
        (Lwt_unix.sleep 1.)
        (fun () -> say_hello ())
    end
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    Note that the string [Hello world!] was passed to [say_hello] through the
    key [k]. Meanwhile, the only {e explicit} argument of the callback
    [say_hello] is [()].

    The way this works is functions like {!Lwt.bind} take a {b snapshot} of the
    implicit argument map. Later, right before the callback is run, the map is
    {e restored} to that snapshot. In other words, the map has the same state
    inside the callback as it did at the time the callback was registered.

    To be more precise:

    - [Lwt.with_value] associates [Some "Hello world!"] with [k], and runs the
      function passed to it.
    - This function contains the {!Lwt.bind}.
    - OCaml's eager evaluation means the arguments are evaluated first. In
      particular, the [Lwt_unix.sleep 1.] promise is created.
    - {!Lwt.bind} then attaches the callback in its second argument, the one
      which calls [say_hello], to that [sleep] promise.
    - {!Lwt.bind} also takes a snapshot of the current state of the implicit
      argument map, and pairs the callback with that snapshot.
    - The callback will not run for another second or so, when the [sleep]
      promise will be resolved.
    - Instead, {!Lwt.bind} returns its result promise [p_3]. This causes
      [Lwt.with_value] to also return [p_3], first restoring [k] to be
      associated with [None].
    - {!Lwt_main.run} gets the pending [p_3], and blocks the whole process, with
      [k] associated with [None].
    - One second later, the [sleep] I/O completes, resolving the [sleep]
      promise.
    - This triggers the [say_hello] callback. Right before the callback is
      called, the implicit argument map is restored to its snapshot, so [k] is
      associated with [Some "Hello world!"].
    - After the callback completes, Lwt again restores [k] to be associated with
      [None].

    The Lwt functions that take snapshots of the implicit callback argument map
    are exactly those which attach callbacks to promises: {!Lwt.bind} and its
    variants [>>=] and [let%lwt], {!Lwt.map} and its variant [>|=], {!Lwt.catch}
    and its variant [try%lwt], {!Lwt.finalize} and its variant [%lwt.finally],
    {!Lwt.try_bind}, {!Lwt.on_success}, {!Lwt.on_failure},
    {!Lwt.on_termination}, and {!Lwt.on_any}.

    [Lwt.with_value] should only be called in the main thread, i.e. do not call
    it inside {!Lwt_preemptive.detach}. *)



(** {3 Immediate resolving} *)

val wakeup : 'a u -> 'a -> unit
(** [Lwt.wakeup r v] is like {!Lwt.wakeup_later}[ r v], except it guarantees
    that callbacks associated with [r] will be called immediately, deeper on the
    current stack.

    In contrast, {!Lwt.wakeup_later} {e may} call callbacks immediately, or may
    queue them for execution on a shallower stack – though still before the next
    time Lwt blocks the process on I/O.

    Using this function is discouraged, because calling it in a loop can exhaust
    the stack. The loop might be difficult to detect or predict, due to combined
    mutually-recursive calls between multiple modules and libraries.

    Also, trying to use this function to guarantee the timing of callback calls
    for synchronization purposes is discouraged. This synchronization effect is
    obscure to readers. It is better to use explicit promises, or {!Lwt_mutex},
    {!Lwt_condition}, and/or {!Lwt_mvar}. *)

val wakeup_exn : _ u -> exn -> unit
(** [Lwt.wakeup_exn r exn] is like {!Lwt.wakeup_later_exn}[ r exn], but has
    the same problems as {!Lwt.wakeup}. *)

val wakeup_result : 'a u -> 'a result -> unit
(** [Lwt.wakeup_result r result] is like {!Lwt.wakeup_later_result}[ r result],
    but has the same problems as {!Lwt.wakeup}. *)



(** {3 Helpers for resolving} *)

val make_value : 'a -> 'a result
  [@@ocaml.deprecated
    " Use Result.Ok, which is the same as Ok since OCaml 4.03."]
(** [Lwt.make_value v] is equivalent to
    {{: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Stdlib.html#TYPEresult}
    [Ok v]} since OCaml 4.03. If you need compatibility with OCaml 4.02, use
    [Result.Ok] and depend on opam package
    {{: https://opam.ocaml.org/packages/result/} [result]}. *)

val make_error : exn -> _ result
  [@@ocaml.deprecated
    " Use Result.Error, which is the same as Error since OCaml 4.03."]
(** [Lwt.make_error exn] is equivalent to
    {{: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Stdlib.html#TYPEresult}
    [Error exn]} since OCaml 4.03. If you need compatibility with OCaml 4.02,
    use [Result.Error] and depend on opam package
    {{: https://opam.ocaml.org/packages/result/} [result]}. *)

val waiter_of_wakener : 'a u -> 'a t
  [@@ocaml.deprecated
" This function should be avoided, because it makes subtyping of resolvers
 unsound. See
  https://github.com/ocsigen/lwt/issues/458"]
(** [Lwt.waiter_of_wakener r] evaluates to the promise associated with resolver
    [r].

    It is recommended to explicitly keep the reference to the promise
    instead. *)



(** {3 Linked lists of promises} *)

[@@@ocaml.warning "-3"]

val add_task_r : ('a u) Lwt_sequence.t -> 'a t
  [@@ocaml.deprecated
" Deprecated because Lwt_sequence is an implementation detail of Lwt. See
  https://github.com/ocsigen/lwt/issues/361"]
(** [Lwt.add_task_r sequence] is equivalent to

{[
let p, r = Lwt.task () in
let node = Lwt_sequence.add_r r sequence in
Lwt.on_cancel p (fun () -> Lwt_sequence.remove node);
p
]}

    Use of this function is discouraged for two reasons:

    - {!Lwt_sequence} should not be used outside Lwt.
    - This function only exists because it performs a minor internal
      optimization, which may be removed. *)

val add_task_l : ('a u) Lwt_sequence.t -> 'a t
  [@@ocaml.deprecated
" Deprecated because Lwt_sequence is an implementation detail of Lwt. See
  https://github.com/ocsigen/lwt/issues/361"]
(** Like {!Lwt.add_task_r}, but the equivalent code calls {!Lwt_sequence.add_l}
    instead. *)

[@@@ocaml.warning "+3"]



(** {3 Yielding} *)

val pause : unit -> unit t
(** [Lwt.pause ()] creates a pending promise that is fulfilled after Lwt
    finishes calling all currently ready callbacks, i.e. it is fulfilled on the
    next “tick.”

    Putting the rest of your computation into a callback of [Lwt.pause ()]
    creates a “yield” that gives other callbacks a chance to run first.

    For example, to break up a long-running computation, allowing I/O to be
    handled between chunks:

{[
let () =
  let rec handle_io () =
    let%lwt () = Lwt_io.printl "Handling I/O" in
    let%lwt () = Lwt_unix.sleep 0.1 in
    handle_io ()
  in

  let rec compute n =
    if n = 0 then
      Lwt.return ()
    else
      let%lwt () =
        if n mod 1_000_000 = 0 then
          Lwt.pause ()
        else
          Lwt.return ()
      in
      compute (n - 1)
  in

  Lwt.async handle_io;
  Lwt_main.run (compute 100_000_000)

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

  If you replace the call to [Lwt.pause] by [Lwt.return] in the program above,
  ["Handling I/O"] is printed only once. With [Lwt.pause], it is printed several
  times, depending on the speed of your machine.

  An alternative way to handle long-running computations is to detach them to
  preemptive threads using {!Lwt_preemptive}. *)

(**/**)

val wakeup_paused : unit -> unit
(** [Lwt.wakeup_paused ()] fulfills all promises created with {!Lwt.pause} since
    the last time [Lwt.wakeup_paused] was called, or since the process was
    started.

    This function is intended for internal use by Lwt. *)

val paused_count : unit -> int
(** Returns the number of promises that would be fulfilled by calling
    {!Lwt.wakeup_paused} right now.

    This function is intended for internal use by Lwt. *)

val register_pause_notifier : (int -> unit) -> unit
(** [Lwt.register_pause_notifier f] causes [f] to be called every time
    {!Lwt.pause} is called. The result of {!Lwt.paused_count}[ ()] is passed to
    [f].

    Only one such function can be registered at a time. There is only a single
    internal reference cell available for this purpose.

    This function is intended for internal use by Lwt. *)

(**/**)



(** {3 Function lifters} *)

val wrap : (unit -> 'a) -> 'a t
(** [Lwt.wrap f] applies [f ()]. If [f ()] returns a value [v], [Lwt.wrap]
    returns {!Lwt.return}[ v]. If [f ()] raises an exception exn, [Lwt.wrap]
    returns {!Lwt.fail}[ exn]. *)

val wrap1 :
  ('a -> 'b) ->
    ('a -> 'b t)
val wrap2 :
  ('a -> 'b -> 'c) ->
    ('a -> 'b -> 'c t)
val wrap3 :
  ('a -> 'b -> 'c -> 'd) ->
    ('a -> 'b -> 'c -> 'd t)
val wrap4 :
  ('a -> 'b -> 'c -> 'd -> 'e) ->
    ('a -> 'b -> 'c -> 'd -> 'e t)
val wrap5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f t)
val wrap6 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g t)
val wrap7 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h t)
(** As a “prototype,” [Lwt_wrap1 f] creates a promise-valued function [g]:

{[
let g v =
  try
    let v' = f v in
    Lwt.return v'
  with exn ->
    Lwt.fail exn
]}

    The remainder of the functions work analogously – they just work on [f] with
    larger numbers of arguments.

    Note that there is an important difference to {!Lwt.wrap}. These functions
    don't run [f], nor create the final promise, immediately. In contrast,
    {!Lwt.wrap} runs its argument [f] eagerly.

    To get a suspended function instead of the eager execution of {!Lwt.wrap},
    use [Lwt.wrap1]. *)



(** {3 Trivial promises} *)

val return_some : 'a -> ('a option) t
(** Counterpart to {!Lwt.return_none}. However, unlike {!Lwt.return_none}, this
    function performs no {{: #VALreturn_unit} optimization}. This is because it
    takes an argument, so it cannot be evaluated at initialization time, at
    which time the argument is not yet available. *)

val return_ok : 'a -> (('a, _) Result.result) t
(** Like {!Lwt.return_some}, this function performs no optimization.

    @since Lwt 2.6.0 *)

val return_error : 'e -> ((_, 'e) Result.result) t
(** Like {!Lwt.return_some}, this function performs no optimization.

    @since Lwt 2.6.0 *)

val fail_with : string -> _ t
(** [Lwt.fail_with s] is an abbreviation for

{[
Lwt.fail (Stdlib.Failure s)
]}

    In most cases, it is better to use [failwith s] from the standard library.
    See {!Lwt.fail} for an explanation. *)

val fail_invalid_arg : string -> _ t
(** [Lwt.invalid_arg s] is an abbreviation for

{[
Lwt.fail (Stdlib.Invalid_argument s)
]}

    In most cases, it is better to use [invalid_arg s] from the standard
    library. See {!Lwt.fail} for an explanation. *)



(** {3 Unscoped infix operators} *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>|=) : 'a t -> ('a -> 'b) -> 'b t
val (<?>) : 'a t -> 'a t -> 'a t
val (<&>) : unit t -> unit t -> unit t
val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
val (=|<) : ('a -> 'b) -> 'a t -> 'b t
(** Use the operators in module {{: #MODULEInfix} [Lwt.Infix]} instead. Using
    these instances of the operators directly requires opening module [Lwt],
    which brings an excessive number of other names into scope. *)



(** {3 Miscellaneous} *)

val is_sleeping : _ t -> bool
(** [Lwt.is_sleeping p] is equivalent to {!Lwt.state}[ p = Lwt.Sleep]. *)

val ignore_result : _ t -> unit
(** An obsolete variant of {!Lwt.async}.

    [Lwt.ignore_result p] behaves as follows:

    - If [p] is already fulfilled, [Lwt.ignore_result p] does nothing.
    - If [p] is already rejected with [exn], [Lwt.ignore_result p] raises [exn]
      immedaitely.
    - If [p] is pending, [Lwt.ignore_result p] does nothing, but if [p] becomes
      rejected later, the exception is passed to [!]{!Lwt.async_exception_hook}.

    Use of this function is discouraged for two reasons:

    - The behavior is different depending on whether [p] is rejected now or
      later.
    - The name is misleading, and has led to users thinking this function is
      analogous to [Stdlib.ignore], i.e. that it waits for [p] to become
      resolved, completing any associated side effects along the way. In fact,
      the function that does {e that} is ordinary {!Lwt.bind}. *)



(**/**)

val poll : 'a t -> 'a option
val apply : ('a -> 'b t) -> 'a -> 'b t

val backtrace_bind :
  (exn -> exn) -> 'a t -> ('a -> 'b t) -> 'b t
val backtrace_catch :
  (exn -> exn) -> (unit -> 'a t) -> (exn -> 'a t) -> 'a t
val backtrace_finalize :
  (exn -> exn) -> (unit -> 'a t) -> (unit -> unit t) -> 'a t
val backtrace_try_bind :
  (exn -> exn) -> (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t

val abandon_wakeups : unit -> unit

val debug_state_is : 'a state -> 'a t -> bool t
