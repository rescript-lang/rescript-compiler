(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(* Reading guide

   Welcome to the implementation of the Lwt core! This is a big file, but we
   hope that reading it (parts at a time!) will not be scary :) Here is why:


   * Sectioning

   The code is broken up into sections, each one of which is an internal module.
   Most of the modules have a signature, which serves as a neat table of
   contents.

   It is recommended that you read this file with code folding enabled. If you
   fold all the modules, you can visualize the logical structure of Lwt quite
   easily. You can then expand modules as needed, depending on what part of the
   implementation you are interested in. Without code folding, you face an
   intimidating wall of code :( You can still visually parse the file, however,
   because there are plenty of blank lines to help section things off. You can
   also view this file folded online:

     https://gist.github.com/aantron/9fab0bdead98a60fccf06e0189186863
     https://gist.github.com/aantron/97b58520d5bb4858ccac6f54700a24d7

   The signatures are unusual: big comments are absent. They are moved into the
   modules, so that they are hidden by code folding when you (the reader!) are
   not interested in those modules.


   * Documentation

   The documentation begins with an overview of major concepts and components.
   This overview puts everything into context. You don't have to read the whole
   thing. The overview begins with basic concepts, moves on to advanced ones,
   and then gets into the truly esoteric. You can read about each concept on an
   as-needed basis. However, once you have read the whole overview, you will be
   aware of *everything* that is needed to understand, and work with, the core
   of Lwt.

   Littered in the code are additional comments, that go in-depth on various
   local implementation details, opportunities, regrets, and the like.

   The sections (modules) of the code correspond closely to sections of the
   overview.


   * Whitespace

   The total line count of this file may seem frightening, but one third of it
   is whitespace and comments, both there to help you read the remaining two
   thirds!

   Also, within those two thirds, there are large groups of functions that are
   repetitive and formulaic, so there is much less conceptually-unique code in
   Lwt than you might think at first.


   * Please edit the code and the docs!

   This code is meant to be readable, and to be edited. If you are reading
   something, and think there is a better way to express it, please go ahead and
   open a pull request to the Lwt repository at

     https://github.com/ocsigen/lwt

   Even if your pull request somehow doesn't get merged, you will have educated
   the maintainers, not to mention other contributors, and users. This is true
   even if the change is trivial -- sometimes, maintainers just need to be
   educated multiple times before they see the wisdom of it :/

   Likewise, if you would like to make a code contribution to the Lwt core, it
   is quite welcome, and we hope that this code is readable enough for you to be
   able to make it!


   Enjoy! *)



(* Overview

   In this file, there is a "model" function -- [Lwt.bind] -- which pulls
   together many (though not all) of the concepts and helpers discussed in this
   overview. To find it, search for "let bind," and you can examine it while
   reading the overview. The authors of this file intend to put extra effort
   into writing nice comments inside [Lwt.bind] :)


   0. Main mechanism and two aspects

   The Lwt interface ([lwt.mli]) provides one main mechanism, promises, and two
   "aspects," which are *not* necessary to understand the main mechanism
   promises, but they are still there:

   - promise cancellation
   - sequence-associated storage

   If you are not interested in cancellation or storage, you can ignore these
   two complications, and still get a pretty good understanding of the code. To
   help, all identifiers related to cancellation contain the string "cancel,"
   and all identifiers related to storage contain "storage."


   1. Promises

   A promise is a cell that can be in one of two states: "resolved" or
   "pending."

   - Resolved promises

     A resolved promise is either "fulfilled" with a value, or "rejected" with
     an exception. The state of a resolved promise will never change again: a
     resolved promise is immutable. A resolved promise is basically equivalent
     to an [('a, exn) Pervasives.result]. Resolved promises are produced in two
     ways:

     - [Lwt.return], [Lwt.fail], and related functions, produce "trivial"
       promises that are resolved from the start.
     - The other way is to resolve a promise that started out pending.

     Note that rejected promises have nothing to do with unhandled exceptions.

   - Pending promises

     ...are those that may become resolved in the future. Each pending promise
     carries a list of callbacks. These callbacks are added by functions like
     [Lwt.bind], and called by Lwt if/when the promise is resolved. These
     callbacks typically end up resolving additional promises; see section
     "Resolution loop" below.

     Pending promises are produced in three ways, according to how they can be
     resolved:

     - Initial promises

       ...are created by [Lwt.wait] and [Lwt.task]. The user of Lwt resolves
       these promises manually, through the resolvers returned by those
       functions.

     - Sequential composition

       For example, [Lwt.bind]. These promises only are only resolved when the
       preceding sequence of promises resolves. The user cannot resolve these
       promises directly (but see the section on cancellation below).

     - Concurrent composition

       For example, [Lwt.join] or [Lwt.choose]. These promises are only resolved
       when all or one of a set of "preceding" promises resolve. The user cannot
       resolve these promises directly (but see the section on cancellation
       below).


   2. Resolvers

   Resolvers are given to the user by [Lwt.wait] and [Lwt.task], and can be used
   by the user to resolve the corresponding promises. Note that this means the
   user only ever gets resolvers for initial promises.

   Internally, resolvers are the exact same objects as the promises they
   resolve, even though the resolver is exposed as a reference of a different
   type by [lwt.mli]. For details on why, see section "Type system abuse" below.


   3. Callbacks

   ...are attached by Lwt to pending promises, and are run by Lwt if/when those
   promises are resolved. These callbacks are not directly exposed through
   [lwt.mli] -- they are a low-level mechanism. For example, to implement
   [Lwt.bind p f], Lwt attaches a callback to [p] that does some internal Lwt
   book-keeping, and then calls [f] if [p] is fulfilled, and does something else
   if [p] is rejected.

   Callbacks come in two flavors: regular callbacks and cancel callbacks. The
   only material differences between them are that:

   - regular callbacks are always called when a promise is resolved, but cancel
     callbacks are called, in addition, only if the promise is canceled, and
   - all cancel callbacks of a promise are called before any regular callback
     is called.

   Cancellation is a special case of resolution, in particular, a special case
   of rejection, but see the section on cancellation later below.


   4. Resolution loop

   Resolving a pending promise triggers its callbacks, and those might resolve
   more pending promises, triggering more callbacks, etc. This behavior is the
   *resolution loop*. Lwt has some machinery to avoid stack overflow and other
   unfortunate situations during this loop.

   This chaining of promise resolutions through callbacks can be seen as a kind
   of promise dependency graph, in which the nodes are pending promises, and the
   edges are callbacks. During the resolution loop, Lwt starts at some initial
   promise that is getting resolved by the user, and recursively resolves all
   dependent promises. The graph is modified: resolved promises are no longer
   pending, so they are no longer part of the graph.

   Some of these dependencies are explicit to Lwt, e.g. the callbacks registered
   by [Lwt.bind]. Others are not visible to Lwt, because the user can always
   register a callback using a function like [Lwt.on_success], and use that
   callback to resolve another initial promise. All the explicit dependencies
   are created by Lwt's own sequential and concurrent composition functions
   (so, [Lwt.bind], [Lwt.join], etc). Whether dependencies are explicit or not
   is relevant only to cancellation.


   5. Cancellation

   As described above, ordinary promise resolution proceeds from an initial
   promise, forward along callbacks through the dependency graph. Since it
   starts from an initial promise, it can only be triggered using a resolver.

   Cancellation is a sort of dual to ordinary resolution. Instead of starting at
   an initial promise/resolver, cancellation starts at *any* promise. It then
   goes *backwards* through the explicit dependency graph, looking for
   cancelable initial promises to cancel -- those that were created by
   [Lwt.task]. After finding them, cancellation resolves them normally with
   [Rejected Lwt.Canceled], causing an ordinary promise resolution process.

   To summarize, cancellation is a way to trigger an *ordinary* resolution of
   promises created with [Lwt.task], by first searching for them in the promise
   dependency graph (which is assembled by [Lwt.bind], [Lwt.join], etc).

   This backwards search is triggered only by [Lwt.cancel]. It is also possible
   for the user to cancel a promise directly by rejecting it with
   [Lwt.Canceled], but in all cases where the user can do so, the search would
   be redundant anyway -- the user has only two ways of directly rejecting a
   promise with [Lwt.Canceled] (or any exception, for that matter):

   - The user can create an initial promise, then reject it through its
     resolver. The search is redundant because it would find only the same
     initial promise to cancel.
   - The user can create a trivial promise by calling [Lwt.fail Lwt.Canceled].
     The search is again redundant; in this case it would find nothing to
     cancel.

   Note that there is a quirk: only promises created by [Lwt.task] are
   susceptible to being canceled by [Lwt.cancel], but the user can manually
   cancel initial promises created by both [Lwt.task] and [Lwt.wait].

   Due to [Lwt.cancel], promise cancellation, and therefore resolution, can be
   initiated by the user without access to a resolver. This is important for
   reasoning about state changes in the implementation of Lwt, and is referenced
   in some implementation detail comments.


   6. No I/O

   The Lwt core deliberately doesn't do I/O. The resolution loop stops running
   once no promises can be resolved immediately. It has to be restarted later
   by some surrounding I/O loop. This I/O loop typically keeps track of pending
   promises that represent blocked or in-progress I/O; other pending promises
   that indirectly depend on I/O are not explicitly tracked. They are retained
   in memory by references captured inside callbacks.

   On Unix and Windows, a separate top-level loop, typically [Lwt_main.run], is
   necessary to repeatedly call [select], [epoll], or [kevent], and resolve
   blocked I/O promises.

   In JavaScript, references to promises are retained by JavaScript code, which
   is, in turn, triggered by the JS engine. In other words, the top-level loop
   is buried inside the JS engine.

   This separation of the Lwt core from the top-level I/O loop keeps the core
   portable.


   7. Promise "proxying"

   In [Lwt.bind : 'a t -> ('a -> 'b t) -> 'b t], the outer ['b t] is created by
   [bind] first, and returned to the user. The inner ['b t] is created by the
   user later, and then returned to [bind]. At that point, [bind] needs to make
   the inner and outer ['b t]s behave identically.

   This is accomplished by making one of the promises point to the other. The
   first of the promises thus becomes a "proxy," and the other is its
   "underlying" promise.

   After that, all operations that would be performed by Lwt on the proxy are
   instead performed on the underlying promise. This is ensured by the numerous
   calls to the internal function [underlying] in this file.

   Because of the pervasive use of [underlying], proxies can be more or less
   ignored on a first reading the code. However, becoming a proxy is a kind of
   state change, and any promise that is returned by a callback to [bind], or to
   a similar Lwt function, might become a proxy. That means: just about any
   promise that is handed to the user, might become a proxy promise by the next
   time Lwt sees it. This is important for reasoning about possible state
   changes in implementation of Lwt, and is referenced in some implementation
   detail comments.


   8. Sequence-associated storage

   Lwt has a global key-value map. The map can be preserved across sequential
   composition functions, so that it has the same state in the user's callback
   [f] as it did at the time the user called [Lwt.bind p f].

   The details are pretty straightforward, and discussed in module
   [Sequence_associated_storage]. The main thing to be aware of is the many
   references to [current_storage] throughout Lwt, which are needed to properly
   save and restore the mapping.


   9. Type system abuse

   The implementation uses the type system somewhat extensively. Gentle
   introductions can be found here:

     https://discuss.ocaml.org/t/161/7
     https://discuss.ocaml.org/t/161/16

   A short summary follows.

   The promise state is, internally, a GADT which encodes the state in its type
   parameters. Thus, if you do [let p = underlying p], the shadowing reference
   [p] is statically known *not* to be a proxy, and the compiler knows that the
   corresponding match case [Proxy _] is impossible.

   The external promise type, ['a t], and the external resolver type, ['a u],
   are not GADTs. Furthermore, they are, respectively, covariant and
   contravariant in ['a], while the internal promise type is invariant in ['a].
   For these reasons, there are nasty casts between ['a t], ['a u], and the
   internal promise type. The implementation is, of course, written in terms of
   the internal type.

   Casting from an ['a t] to an internal promise produces a reference for
   which the state is "unknown": this is simulated with a helper GADT, which
   encodes existential types. There are several similar casts, which are used
   to document possible state changes between the time a promise is created,
   and the later time it is used in a callback. You can see these casts in
   action in [Lwt.bind]. The cast syntax is pretty light, and, besides being
   commented in [bind], all such casts are documented in modules [Public_types]
   and [Basic_helpers].


   If you've made it this far, you are an Lwt expert! Rejoice! *)



(* Suppress warning 4, "fragile pattern matching," in this file only, due to

     https://caml.inria.fr/mantis/view.php?id=7451

   This can be removed if/when Lwt requires a minimum OCaml version 4.05. *)
[@@@ocaml.warning "-4"]



(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
[@@@ocaml.warning "-3"]
module Lwt_sequence = Lwt_sequence
[@@@ocaml.warning "+3"]



(* Some sequence-associated storage types

   Sequence-associated storage is defined and documented later, in module
   [Sequence_associated_storage]. However, the following types are mentioned in
   the definition of [promise], so they must be defined here first. *)
module Storage_map =
  Map.Make
    (struct
      type t = int
      let compare = compare
    end)
type storage = (unit -> unit) Storage_map.t



module Main_internal_types =
struct
  (* Phantom types for use with types [promise] and [state]. These are never
     constructed; the purpose of the constructors is to prove to the type
     checker that these types are distinct from each other. Warning 37, "unused
     constructor," therefore has to be temporarily suppressed. *)

  [@@@ocaml.warning "-37"]

  type underlying = private Underlying_and_this_constructor_is_not_used
  type proxy = private Proxy_and_this_constructor_is_not_used

  type resolved = private Resolved_and_this_constructor_is_not_used
  type pending = private Pending_and_this_constructor_is_not_used

  [@@@ocaml.warning "+37"]



  (* Promises proper. *)

  type ('a, 'u, 'c) promise = {
    mutable state : ('a, 'u, 'c) state;
  }

  and (_, _, _) state =
    | Fulfilled : 'a                  -> ('a, underlying, resolved) state
    | Rejected  : exn                 -> ( _, underlying, resolved) state
    | Pending   : 'a callbacks        -> ('a, underlying, pending)  state
    | Proxy     : ('a, _, 'c) promise -> ('a, proxy,      'c)       state

  (* Note:

     A promise whose state is [Proxy _] is a "proxy" promise. A promise whose
     state is *not* [Proxy _] is an "underlying" promise.

     The "underlying promise of [p]" is:

     - [p], if [p] is itself underlying.
     - Otherwise, [p] is a proxy and has state [Proxy p']. The underlying
       promise of [p] is the underlying promise of [p'].

     In other words, to find the underlying promise of a proxy, Lwt follows the
     [Proxy _] links to the end. *)

  (* Note:

     When a promise is resolved, or becomes a proxy, its state field is
     mutated. This invalidates the type invariants on the promise. See internal
     function [set_promise_state] for details about that.

     When an Lwt function has a reference to a promise, and also registers a
     callback that has a reference to the same promise, the invariants on the
     reference may become invalid by the time the callback is called. All such
     callbacks have comments explaining what the valid invariants are at that
     point, and/or casts to (1) get the correct typing and (2) document the
     potential state change for readers of the code. *)



  (* Callback information for pending promises. *)

  and 'a callbacks = {
    mutable regular_callbacks : 'a regular_callback_list;
    mutable cancel_callbacks  : 'a cancel_callback_list;
    mutable how_to_cancel     : how_to_cancel;
    mutable cleanups_deferred : int;
  }

  and 'a regular_callback = 'a resolved_state -> unit

  and cancel_callback = unit -> unit

  and 'a resolved_state = ('a, underlying, resolved) state

  and how_to_cancel =
    | Not_cancelable              :                           how_to_cancel
    | Cancel_this_promise         :                           how_to_cancel
    | Propagate_cancel_to_one     : (_, _, _) promise      -> how_to_cancel
    | Propagate_cancel_to_several : (_, _, _) promise list -> how_to_cancel

  and 'a regular_callback_list =
    | Regular_callback_list_empty
    | Regular_callback_list_concat of
      'a regular_callback_list * 'a regular_callback_list
    | Regular_callback_list_implicitly_removed_callback of
      'a regular_callback
    | Regular_callback_list_explicitly_removable_callback of
      'a regular_callback option ref

  and _ cancel_callback_list =
    | Cancel_callback_list_empty :
      _ cancel_callback_list
    | Cancel_callback_list_concat :
      'a cancel_callback_list * 'a cancel_callback_list ->
        'a cancel_callback_list
    | Cancel_callback_list_callback :
      storage * cancel_callback ->
        _ cancel_callback_list
    | Cancel_callback_list_remove_sequence_node :
      ('a, _, _) promise Lwt_sequence.node ->
        'a cancel_callback_list

  (* Notes:

     These type definitions are guilty of performing several optimizations,
     without which they would be much easier to understand.

     - The type parameters of ['a resolved_state] guarantee that it is either
       [Fulfilled _] or [Rejected _]. So, it is equivalent to
       [('a, exn) Pervasives.result], and, indeed, should have an identical
       memory representation.

     - As per the Overview, there are regular callbacks and cancel callbacks.
       Cancel callbacks are called only on cancellation, and, then, before any
       regular callbacks are called.

       Despite the different types for the two kinds of callbacks, they are
       otherwise the same. Cancel callbacks just don't need a result state
       argument, because it is known to be [Rejected Canceled].

     - Regular callbacks are not allowed to raise exceptions. All regular
       callbacks are created in this file, so this can be checked.

       Cancel callbacks can raise exceptions, but if they do so, the exceptions
       are passed to [async_exception_hook].

     - [how_to_cancel] implements the dependency graph mentioned in the
       Overview. It is traversed backwards during [Lwt.cancel]. It is a GADT
       because we don't care about the actual types of the promise references
       stored, or their invariants. The constructors correspond to pending
       promise kinds as follows:
         - [Not_cancelable]: initial, [Lwt.wait].
         - [Cancel_this_promise]: initial, [Lwt.task].
         - [Propagate_cancel_to_one]: sequential composition, e.g. [Lwt.bind].
         - [Propagate_cancel_to_several]: concurrent composition, e.g.
           [Lwt.join].

     - The two callback list types are ordinary append-friendly lists, with two
       optimizations inlined:

       - ['a regular_callback_list] apparently has two "kinds" of regular
         callbacks, implicitly removed and explicitly removable. All callbacks
         are removable. It's just that, for some callbacks, they will only be
         removed at the same time that the promise they are attached to becomes
         resolved. When that happens, the entire state of that promise changes
         to [Fulfilled _] or [Rejected _], and the reference to the whole
         callback list is simply lost. This "removes" the callback. For these
         callbacks, ['a regular_callback_list] attempts to trim an option and a
         reference cell with the
         [Regular_callback_list_implicitly_removed_callback] constructor.

       - ['a cancel_callback_list] has
         [Cancel_callback_list_remove_sequence_node node], which is the same as
         [Cancel_callback_list_callback (_, (fun _ ->
           Lwt_sequence.remove node))].
         This was probably done to avoid a closure allocation.

     - The [cleanups_deferred] field is explained in module
       [Pending_callbacks]. *)
end
open Main_internal_types



module Public_types =
struct
  type +'a t
  type -'a u
  (* The contravariance of resolvers is, technically, unsound due to the
     existence of [Lwt.waiter_of_wakener]. That is why that function is
     deprecated. See

       https://github.com/ocsigen/lwt/issues/458 *)

  let to_public_promise : ('a, _, _) promise -> 'a t = Obj.magic
  let to_public_resolver : ('a, _, _) promise -> 'a u = Obj.magic

  type _ packed_promise =
    | Internal : ('a, _, _) promise -> 'a packed_promise
    [@@ocaml.unboxed]

  let to_internal_promise (p : 'a t) : 'a packed_promise =
    Internal (Obj.magic p)
  let to_internal_resolver (r : 'a u) : 'a packed_promise =
    Internal (Obj.magic r)

  (* Most functions that take a public promise (['a t]) convert it to an
     internal promise as follows:

       (* p : 'a t *)

       let Internal p = to_internal_promise p in

       (* p : ('a, u, c) promise, where u and c are fresh types, i.e. the
          invariants on p are unknown. *)

     This cast is a no-op cast. It only produces a reference with a different
     type. The introduction and immediate elimination of [Internal _] seems to
     be optimized away even on older versions of OCaml that don't have Flambda
     and don't support [[@@ocaml.unboxed]]. *)



  (* Internal name of the public [+'a Lwt.result]. The public name is defined
     later in the module. This is to avoid potential confusion with
     [Pervasives.result]/[Result.result], as the public name would not be
     prefixed with [Lwt.] inside this file. *)
  type +'a lwt_result = ('a, exn) Result.result

  (* This could probably save an allocation by using [Obj.magic]. *)
  let state_of_result = function
    | Result.Ok x -> Fulfilled x
    | Result.Error exn -> Rejected exn
end
include Public_types



module Basic_helpers :
sig
  val identical : ('a, _, _) promise -> ('a, _, _) promise -> bool
  val underlying : ('a, 'u, 'c) promise -> ('a, underlying, 'c) promise

  type ('a, 'u, 'c) state_changed =
    | State_may_have_changed of ('a, 'u, 'c) promise
    [@@ocaml.unboxed]
  val set_promise_state :
    ('a, _, _) promise -> ('a, 'u, 'c) state -> ('a, 'u, 'c) state_changed

  (* type 'a may_now_be_proxy = *)
    (* | State_may_now_be_pending_proxy : *)
      (* ('a, _, pending) promise -> 'a may_now_be_proxy *)
    (* [@@ocaml.unboxed] *)
  val may_now_be_proxy :
    ('a, underlying, pending) promise -> 'a may_now_be_proxy
end =
struct
  (* Checks physical equality ([==]) of two internal promises. Unlike [==], does
     not force unification of their invariants. *)
  let identical p1 p2 =
    (to_public_promise p1) == (to_public_promise p2)

  (* [underlying p] evaluates to the underlying promise of [p].

     If multiple [Proxy _] links are traversed, [underlying] updates all the
     proxies to point immediately to their final underlying promise. *)
  let rec underlying
      : 'u 'c. ('a, 'u, 'c) promise -> ('a, underlying, 'c) promise =
    fun
      (type u)
      (type c)
      (p : ('a, u, c) promise) ->

    match p.state with
    | Fulfilled _ -> (p : (_, underlying, _) promise)
    | Rejected _ -> p
    | Pending _ -> p
    | Proxy p' ->
      let p'' = underlying p' in
      if not (identical p'' p') then
        p.state <- Proxy p'';
      p''



  type ('a, 'u, 'c) state_changed =
    | State_may_have_changed of ('a, 'u, 'c) promise
    [@@ocaml.unboxed]

  let set_promise_state p state =
    let p : (_, _, _) promise = Obj.magic p in
    p.state <- state;
    State_may_have_changed p

  (* [set_promise_state p state] mutates the state of [p], and evaluates to a
     (wrapped) reference to [p] with the same invariants as on [state]. The
     original reference [p] should be shadowed when calling this function:

       let State_may_have_changed p = set_promise_state p (Fulfilled 42) in ...

     This is a kind of cheap imitation of linear typing, which is good enough
     for the needs of [lwt.ml].

     Internal functions that transitively call [set_promise_state] likewise
     return the new reference. This ends at some top-level function, typically
     either a callback or a function in the public API. There, the new reference
     is still bound, but is then explicitly ignored.

     The state of a promise is never updated directly outside this module
     [Basic_helpers]. All updates elsewhere are done through
     [set_promise_state].

     To avoid problems with type-level invariants not matching reality, data
     structures do not store promises with concrete invariants -- except
     resolved promises, which are immutable. Indeed, if one looks at
     definitions of data structures that can store pending promises, e.g. the
     [how_to_cancel] graph, the invariants are existentially quantified.

     Note: it's possible to statically disallow the setting of the [state] field
     by making type [promise] private. However, that seems to require writing a
     signature that is a near-duplicate of [Main_internal_types], or some abuse
     of functors. *)



  (* type 'a may_now_be_proxy = *)
    (* | State_may_now_be_pending_proxy : *)
      (* ('a, _, pending) promise -> 'a may_now_be_proxy *)
    (* [@@ocaml.unboxed] *)

  let may_now_be_proxy p = State_may_now_be_pending_proxy p

  (* Many functions, for example [Lwt.bind] and [Lwt.join], create a fresh
     pending promise [p] and return it to the user.

     They do not return a corresponding resolver. That means that only the
     function itself (typically, a callback registered by it) can resolve [p].
     The only thing the user can do directly is try to cancel [p], but, since
     [p] is not an initial promise, the cancellation attempt simply propagates
     past [p] to [p]'s predecessors. If that eventually results in canceling
     [p], it will be through the normal mechanisms of the function (e.g.
     [Lwt.bind]'s callback).

     As a result, the only possible state change, before the callback, is that
     [p] may have become a proxy. Now,

     - If [p] does not undergo this state change and become a proxy, it remains
       an underlying, pending promise.
     - If [p] does become a proxy, it will be a proxy for another promise [p']
       created fresh by [Lwt.bind], to which this same argument applies. See
       [make_into_proxy].

     So, by induction on the length of the proxy ([Proxy _]) chain, at the time
     the callback is called, [p] is either an underlying, pending promise, or a
     proxy for a pending promise.

     The cast

       let State_may_now_be_pending_proxy p = may_now_be_proxy p in ...

     encodes the possibility of this state change. It replaces a reference

       p : ('a, underlying, pending)

     with

       p : ('a, $Unknown, pending)

     and is typically seen at the beginning of callbacks registered by
     [Lwt.bind] and similar functions.

     The cast is a no-op cast. The introduction and immediate elimination of
     [State_may_have_changed _] seems to be optimized away even on old versions
     of OCaml. *)
end
open Basic_helpers



module Sequence_associated_storage :
sig
  (* Public interface *)
  type 'v key
  val new_key : unit -> _ key
  val get : 'v key -> 'v option
  val with_value : 'v key -> 'v option -> (unit -> 'b) -> 'b

  (* Internal interface *)
  val current_storage : storage ref
end =
struct
  (* The idea behind sequence-associated storage is to preserve some values
     during a call to [bind] or other sequential composition operation, and
     restore those values in the callback function:

       Lwt.with_value my_key (Some "foo") (fun () ->
       p >|= fun () ->
       assert (Lwt.get my_key = Some "foo"))
         (* Will succeed even if this callback is called later. *)

     Note that it does not matter that the callback is defined within an
     argument of [with_value], i.e., this does the same:

       let f = fun () -> assert (Lwt.get my_key = Some "foo") in
       Lwt.with_value my_key (Some "foo") (fun () -> p >|= f)

     All that matters is that the top-most sequencing operation (in this case,
     map) is executed by that argument.

     This is implemented using a single global heterogeneous key-value map.
     Sequential composition functions snapshot this map when they are called,
     and restore the snapshot right before calling the user's callback. The same
     happens for cancel triggers added by [on_cancel].

     Maintainer's note: I think using this mechanism should be discouraged in
     new code. *)

  type 'v key = {
    id : int;
    mutable value : 'v option;
  }

  let next_key_id = ref 0

  let new_key () =
    let id = !next_key_id in
    next_key_id := id + 1;
    {id = id; value = None}

  let current_storage = ref Storage_map.empty

  let get key =
    if Storage_map.mem key.id !current_storage then begin
      let refresh = Storage_map.find key.id !current_storage in
      refresh ();
      let value = key.value in
      key.value <- None;
      value
    end
    else
      None

  let with_value key value f =
    let new_storage =
      match value with
      | Some _ ->
        let refresh = fun () -> key.value <- value in
        Storage_map.add key.id refresh !current_storage
      | None ->
        Storage_map.remove key.id !current_storage
    in

    let saved_storage = !current_storage in
    current_storage := new_storage;
    try
      let result = f () in
      current_storage := saved_storage;
      result
    with exn ->
      current_storage := saved_storage;
      raise exn
end
include Sequence_associated_storage



module Pending_callbacks :
sig
  (* Mutating callback lists attached to pending promises *)
  val add_implicitly_removed_callback :
    'a callbacks -> 'a regular_callback -> unit
  val add_explicitly_removable_callback_to_each_of :
    'a t list -> 'a regular_callback -> unit
  val add_explicitly_removable_callback_and_give_remove_function :
    'a t list -> 'a regular_callback -> (unit -> unit)
  val add_cancel_callback : 'a callbacks -> (unit -> unit) -> unit
  val merge_callbacks : from:'a callbacks -> into:'a callbacks -> unit
end =
struct
  let concat_regular_callbacks l1 l2 =
    begin match l1, l2 with
    | Regular_callback_list_empty, _ -> l2
    | _, Regular_callback_list_empty -> l1
    | _, _ -> Regular_callback_list_concat (l1, l2)
    end [@ocaml.warning "-4"]

  let concat_cancel_callbacks l1 l2 =
    begin match l1, l2 with
    | Cancel_callback_list_empty, _ -> l2
    | _, Cancel_callback_list_empty -> l1
    | _, _ -> Cancel_callback_list_concat (l1, l2)
    end [@ocaml.warning "-4"]

  (* In a callback list, filters out cells of explicitly removable callbacks
     that have been removed. *)
  let rec clean_up_callback_cells = function
    | Regular_callback_list_explicitly_removable_callback {contents = None} ->
      Regular_callback_list_empty

    | Regular_callback_list_explicitly_removable_callback {contents = Some _}
    | Regular_callback_list_implicitly_removed_callback _
    | Regular_callback_list_empty as callbacks ->
      callbacks

    | Regular_callback_list_concat (l1, l2) ->
      let l1 = clean_up_callback_cells l1 in
      let l2 = clean_up_callback_cells l2 in
      concat_regular_callbacks l1 l2

  (* See [clear_explicitly_removable_callback_cell] and [merge_callbacks]. *)
  let cleanup_throttle = 42

  (* Explicitly removable callbacks are added (mainly) by [Lwt.choose] and its
     similar functions. In [Lwt.choose [p; p']], if [p'] resolves first, the
     callback added by [Lwt.choose] to [p] is removed.

     The removal itself is accomplished when this function clears the reference
     cell [cell], which contains the reference to that callback.

     If [p] is a long-pending promise that repeatedly participates in
     [Lwt.choose], perhaps in a loop, it will accumulate a large number of
     cleared reference cells in this fashion. To avoid a memory leak, they must
     be cleaned up. However, the cells are not cleaned up on *every* removal,
     presumably because scanning the callback list that often, and rebuilding
     it, can get expensive.

     Cleanup is throttled by maintaining a counter, [cleanups_deferred], on each
     pending promise. The counter is incremented each time this function wants
     to clean the callback list (right after clearing a cell). When the counter
     reaches [cleanup_throttle], the callback list is actually scanned and
     cleared callback cells are removed. *)
  let clear_explicitly_removable_callback_cell cell ~originally_added_to:ps =
    cell := None;

    (* Go through the promises the cell had originally been added to, and either
       defer a cleanup, or actually clean up their callback lists. *)
    ps |> List.iter (fun p ->
      let Internal p = to_internal_promise p in
      match (underlying p).state with
      (* Some of the promises may already have been resolved at the time this
         function is called. *)
      | Fulfilled _ -> ()
      | Rejected _ -> ()

      | Pending callbacks ->
        match callbacks.regular_callbacks with
        (* If the promise has only one regular callback, and it is removable, it
           must have been the cell cleared in this function, above. In that
           case, just set its callback list to empty. *)
        | Regular_callback_list_explicitly_removable_callback _ ->
          callbacks.regular_callbacks <- Regular_callback_list_empty

        (* Maintainer's note: I think this function shouldn't try to trigger a
           cleanup in the first two cases, but I am preserving them for now, as
           this is how the code was written in the past. *)
        | Regular_callback_list_empty
        | Regular_callback_list_implicitly_removed_callback _
        | Regular_callback_list_concat _ ->
          let cleanups_deferred = callbacks.cleanups_deferred + 1 in
          if cleanups_deferred > cleanup_throttle then begin
            callbacks.cleanups_deferred <- 0;
            callbacks.regular_callbacks <-
              clean_up_callback_cells callbacks.regular_callbacks
          end else
            callbacks.cleanups_deferred <- cleanups_deferred)

  (* Concatenates both kinds of callbacks on [~from] to the corresponding lists
     of [~into]. The callback lists on [~from] are *not* then cleared, because
     this function is called only by [Sequential_composition.make_into_proxy],
     which immediately changes the state of [~from] and loses references to the
     original callback lists.

     The [cleanups_deferred] fields of both promises are summed, and if the sum
     exceeds [cleanup_throttle], a cleanup of regular callbacks is triggered.
     This is to prevent memory leaks; see
     [clear_explicitly_removable_callback_cell]. *)
  let merge_callbacks ~from ~into =
    let regular_callbacks =
      concat_regular_callbacks into.regular_callbacks from.regular_callbacks in
    let cleanups_deferred = into.cleanups_deferred + from.cleanups_deferred in

    let regular_callbacks, cleanups_deferred =
      if cleanups_deferred > cleanup_throttle then
        clean_up_callback_cells regular_callbacks, 0
      else
        regular_callbacks, cleanups_deferred
    in

    let cancel_callbacks =
      concat_cancel_callbacks into.cancel_callbacks from.cancel_callbacks in

    into.regular_callbacks <- regular_callbacks;
    into.cancel_callbacks <- cancel_callbacks;
    into.cleanups_deferred <- cleanups_deferred



  (* General, internal, function for adding a regular callback. *)
  let add_regular_callback_list_node callbacks node =
    callbacks.regular_callbacks <-
      match callbacks.regular_callbacks with
      | Regular_callback_list_empty ->
        node
      | Regular_callback_list_implicitly_removed_callback _
      | Regular_callback_list_explicitly_removable_callback _
      | Regular_callback_list_concat _ as existing ->
        Regular_callback_list_concat (node, existing)

  let add_implicitly_removed_callback callbacks f =
    add_regular_callback_list_node
      callbacks (Regular_callback_list_implicitly_removed_callback f)

  (* Adds [callback] as removable to each promise in [ps]. The first promise in
     [ps] to trigger [callback] removes [callback] from the other promises; this
     guarantees that [callback] is called at most once. All the promises in [ps]
     must be pending.

     This is an internal function, indirectly used by the implementations of
     [Lwt.choose] and related functions. *)
  let add_explicitly_removable_callback_and_give_cell ps f =
    let rec cell = ref (Some self_removing_callback_wrapper)
    and self_removing_callback_wrapper result =
      clear_explicitly_removable_callback_cell cell ~originally_added_to:ps;
      f result
    in

    let node = Regular_callback_list_explicitly_removable_callback cell in
    ps |> List.iter (fun p ->
      let Internal p = to_internal_promise p in
      match (underlying p).state with
      | Pending callbacks -> add_regular_callback_list_node callbacks node
      | Fulfilled _ -> assert false
      | Rejected _ -> assert false);

    cell

  let add_explicitly_removable_callback_to_each_of ps f =
    ignore (add_explicitly_removable_callback_and_give_cell ps f)

  (* This is basically just to support [Lwt.protected], which needs to remove
     the callback in circumstances other than the callback being called. *)
  let add_explicitly_removable_callback_and_give_remove_function ps f =
    let cell = add_explicitly_removable_callback_and_give_cell ps f in
    fun () ->
      clear_explicitly_removable_callback_cell cell ~originally_added_to:ps

  let add_cancel_callback callbacks f =
    (* Ugly cast :( *)
    let cast_cancel_callback : (unit -> unit) -> cancel_callback = Obj.magic in
    let f = cast_cancel_callback f in

    let node = Cancel_callback_list_callback (!current_storage, f) in

    callbacks.cancel_callbacks <-
      match callbacks.cancel_callbacks with
      | Cancel_callback_list_empty ->
        node

      | Cancel_callback_list_callback _
      | Cancel_callback_list_remove_sequence_node _
      | Cancel_callback_list_concat _ ->
        Cancel_callback_list_concat (node, callbacks.cancel_callbacks)
end
open Pending_callbacks



module Resolution_loop :
sig
  (* All user-provided callbacks are called by Lwt only through this module. It
     tracks the current callback stack depth, and decides whether each callback
     call should be deferred or not. *)

  (* Internal interface used only in this module Lwt *)
  val resolve :
    ?allow_deferring:bool ->
    ?maximum_callback_nesting_depth:int ->
    ('a, underlying, pending) promise ->
    'a resolved_state ->
      ('a, underlying, resolved) state_changed

  val run_callbacks_or_defer_them :
    ?allow_deferring:bool ->
    ?maximum_callback_nesting_depth:int ->
    ('a callbacks) ->
    'a resolved_state ->
      unit

  val run_callback_or_defer_it :
    ?run_immediately_and_ensure_tail_call:bool ->
    callback:(unit -> 'a) ->
    if_deferred:(unit -> 'a * 'b regular_callback * 'b resolved_state) ->
      'a

  val handle_with_async_exception_hook : ('a -> unit) -> 'a -> unit

  (* Internal interface exposed to other modules in Lwt *)
  val abandon_wakeups : unit -> unit

  (* Public interface *)
  exception Canceled

  val async_exception_hook : (exn -> unit) ref
end =
struct
  (* When Lwt needs to call a callback, it enters the resolution loop. This
     typically happens when Lwt sets the state of one promise to [Fulfilled _]
     or [Rejected _]. The callbacks that were attached to the promise when it
     was pending must then be called.

     This also happens in a few other situations. For example, when [Lwt.bind]
     is called on a promise, but that promise is already resolved, the callback
     passed to [bind] must be called.

     The callbacks triggered during the resolution loop might resolve more
     promises, triggering more callbacks, and so on. This is what makes the
     resolution loop a {e loop}.

     Lwt generally tries to call each callback immediately. However, this can
     lead to a progressive deepening of the call stack, until there is a stack
     overflow. This can't be avoided by doing tail calls, because Lwt always
     needs to do exception handling around callbacks calls: each callback call
     is followed by an exception handler. Instead, what Lwt does is track the
     current callback call depth. Once that depth reaches a certain number,
     [default_maximum_callback_nesting_depth], defined below, further callbacks
     are deferred into a queue instead. That queue is drained when Lwt exits
     from the top-most callback call that triggered the resolution loop in the
     first place.

     To ensure that this deferral mechanism is always properly invoked, all
     callbacks called by Lwt are called through one of three functions provided
     by this module:

     - [resolve], which calls all the callbacks associated to a pending promise
       (and resolves it, changing its state).
     - [run_callbacks_or_defer_them], which is internally used by [resolve] to
       call callbacks that are in a record of type ['a callbacks], which records
       are associated with pending promises. This function is exposed because
       the current implementation of [Lwt.cancel] needs to call it directly.
       Promise resolution and callback calling are separated in a unique way in
       [cancel].
     - [run_callback_or_defer_it], which is used by [Lwt.bind] and similar
       functions to call single callbacks when the promises passed to
       [Lwt.bind], etc., are already resolved.

     Current Lwt actually has a messy mix of callback-calling behaviors. For
     example, [Lwt.bind] is expected to always call its callback immediately,
     while [Lwt.wakeup_later] is expected to defer all callbacks of the promise
     resolved, {e unless} Lwt is not already inside the resolution loop.

     We planned to make these behaviors uniform in Lwt 4.0.0, but decided
     against it due to the risk of breaking users. See

     - https://github.com/ocsigen/lwt/pull/500
     - https://github.com/ocsigen/lwt/pull/519

     As part of the preparation for the change, the above callback-invoking
     functions support several optional arguments to emulate the various
     behaviors. We decided not to remove this machinery, because we might want
     to expose different APIs to Lwt in the future.

     - [~allow_deferring:false] allows ignoring the callback stack depth, and
       calling the callbacks immediately. This emulates the old resolution
       behavior.
     - [~maximum_callback_nesting_depth:1] allows limiting the depth which
       triggers deferral on a per-call-site basis. This is used by
       [Lwt.wakeup_later].
     - [~run_immediately_and_ensure_tail_call:true] is like
       [~allow_deferring:false], which ignores the callback stack depth.
       However, to ensure that the callback is tail-called, Lwt doesn't even
       update the callback stack depth for the benefit of *other* callback
       calls. It just blindly calls the callback.

     See discussion of callback-calling semantics in:

       https://github.com/ocsigen/lwt/issues/329

     * Context

     The resolution loop effectively handles all promises that can be resolved
     immediately, without blocking on I/O. A complete program that does I/O
     calls [Lwt_main.run]. See "No I/O" in the Overview. *)



  let async_exception_hook =
    ref (fun exn ->
      prerr_string "Fatal error: exception ";
      prerr_string (Printexc.to_string exn);
      prerr_char '\n';
      Printexc.print_backtrace stderr;
      flush stderr;
      exit 2)

  let handle_with_async_exception_hook f v =
    (* Note that this function does not care if [f] evaluates to a promise. In
       particular, if [f v] evaluates to [p] and [p] is already rejected or will
       be reject later, it is not the responsibility of this function to pass
       the exception to [!async_exception_hook]. *)
    try f v
    with exn -> !async_exception_hook exn



  exception Canceled



  (* Runs the callbacks (formerly) associated to a promise. Cancel callbacks are
     run first, if the promise was canceled. These are followed by regular
     callbacks.

     The reason for the "formerly" is that the promise's state has already been
     set to [Fulfilled _] or [Rejected _], so the callbacks are no longer
     reachable through the promise reference. This is why the direct [callbacks]
     record must be given to this function. *)
  let run_callbacks
      (callbacks : 'a callbacks)
      (result : 'a resolved_state) : unit =

    let run_cancel_callbacks fs =
      let rec iter_callback_list fs rest =
        match fs with
        | Cancel_callback_list_empty ->
          iter_list rest
        | Cancel_callback_list_callback (storage, f) ->
          current_storage := storage;
          handle_with_async_exception_hook f ();
          iter_list rest
        | Cancel_callback_list_remove_sequence_node node ->
          Lwt_sequence.remove node;
          iter_list rest
        | Cancel_callback_list_concat (fs, fs') ->
          iter_callback_list fs (fs'::rest)

      and iter_list rest =
        match rest with
        | [] -> ()
        | fs::rest -> iter_callback_list fs rest

      in

      iter_callback_list fs []
    in

    let run_regular_callbacks fs =
      let rec iter_callback_list fs rest =
        match fs with
        | Regular_callback_list_empty ->
          iter_list rest
        | Regular_callback_list_implicitly_removed_callback f ->
          f result;
          iter_list rest
        | Regular_callback_list_explicitly_removable_callback
            {contents = None} ->
          iter_list rest
        | Regular_callback_list_explicitly_removable_callback
            {contents = Some f} ->
          f result;
          iter_list rest
        | Regular_callback_list_concat (fs, fs') ->
          iter_callback_list fs (fs'::rest)

      and iter_list rest =
        match rest with
        | [] -> ()
        | fs::rest -> iter_callback_list fs rest

      in

      iter_callback_list fs []
    in

    (* Pattern matching is much faster than polymorphic comparison. *)
    let is_canceled =
      match result with
      | Rejected Canceled -> true
      | Rejected _ -> false
      | Fulfilled _ -> false
    in
    if is_canceled then
      run_cancel_callbacks callbacks.cancel_callbacks;
    run_regular_callbacks callbacks.regular_callbacks



  let default_maximum_callback_nesting_depth = 42

  let current_callback_nesting_depth = ref 0

  type deferred_callbacks =
    Deferred : ('a callbacks * 'a resolved_state) -> deferred_callbacks
    [@@ocaml.unboxed]

  let deferred_callbacks : deferred_callbacks Queue.t = Queue.create ()

  (* Before entering a resolution loop, it is necessary to take a snapshot of
     the current state of sequence-associated storage. This is because many of
     the callbacks that will be run will modify the storage. The storage is
     restored to the snapshot when the resolution loop is exited. *)
  let enter_resolution_loop () =
    current_callback_nesting_depth := !current_callback_nesting_depth + 1;
    let storage_snapshot = !current_storage in
    storage_snapshot

  let leave_resolution_loop (storage_snapshot : storage) : unit =
    if !current_callback_nesting_depth = 1 then begin
      while not (Queue.is_empty deferred_callbacks) do
        let Deferred (callbacks, result) = Queue.pop deferred_callbacks in
        run_callbacks callbacks result
      done
    end;
    current_callback_nesting_depth := !current_callback_nesting_depth - 1;
    current_storage := storage_snapshot

  let run_in_resolution_loop f =
    let storage_snapshot = enter_resolution_loop () in
    let result = f () in
    leave_resolution_loop storage_snapshot;
    result

  (* This is basically a hack to fix https://github.com/ocsigen/lwt/issues/48.
     If currently resolving promises, it immediately exits all recursive
     entries of the resolution loop, goes to the top level, runs any deferred
     callbacks, and exits the top-level resolution loop.

     The name should probably be [abaondon_resolution_loop]. *)
  let abandon_wakeups () =
    if !current_callback_nesting_depth <> 0 then
      leave_resolution_loop Storage_map.empty



  let run_callbacks_or_defer_them
      ?(allow_deferring = true)
      ?(maximum_callback_nesting_depth = default_maximum_callback_nesting_depth)
      callbacks result =

    let should_defer =
      allow_deferring
      && !current_callback_nesting_depth >= maximum_callback_nesting_depth
    in

    if should_defer then
      Queue.push (Deferred (callbacks, result)) deferred_callbacks
    else
      run_in_resolution_loop (fun () ->
        run_callbacks callbacks result)

  let resolve ?allow_deferring ?maximum_callback_nesting_depth p result =
    let Pending callbacks = p.state in
    let p = set_promise_state p result in

    run_callbacks_or_defer_them
      ?allow_deferring ?maximum_callback_nesting_depth callbacks result;

    p

  let run_callback_or_defer_it
      ?(run_immediately_and_ensure_tail_call = false)
      ~callback:f
      ~if_deferred =

    if run_immediately_and_ensure_tail_call then
      f ()

    else
      let should_defer =
        !current_callback_nesting_depth
          >= default_maximum_callback_nesting_depth
      in

      if should_defer then begin
        let immediate_result, deferred_callback, deferred_result =
          if_deferred () in
        let deferred_record =
          {
            regular_callbacks =
              Regular_callback_list_implicitly_removed_callback
                deferred_callback;
            cancel_callbacks = Cancel_callback_list_empty;
            how_to_cancel = Not_cancelable;
            cleanups_deferred = 0
          }
        in
        Queue.push
          (Deferred (deferred_record, deferred_result)) deferred_callbacks;
        immediate_result
      end
      else
        run_in_resolution_loop (fun () ->
          f ())
end
include Resolution_loop



module Resolving :
sig
  val wakeup_later_result : 'a u -> 'a lwt_result -> unit
  val wakeup_later : 'a u -> 'a -> unit
  val wakeup_later_exn : _ u -> exn -> unit

  val wakeup_result : 'a u -> 'a lwt_result -> unit
  val wakeup : 'a u -> 'a -> unit
  val wakeup_exn : _ u -> exn -> unit

  val cancel : 'a t -> unit
end =
struct
  (* Note that this function deviates from the "ideal" callback deferral
     behavior: it runs callbacks directly on the current stack. It should
     therefore be possible to cause a stack overflow using this function. *)
  let wakeup_general api_function_name r result =
    let Internal p = to_internal_resolver r in
    let p = underlying p in

    match p.state with
    | Rejected Canceled ->
      ()
    | Fulfilled _ ->
      Printf.ksprintf invalid_arg "Lwt.%s" api_function_name
    | Rejected _ ->
      Printf.ksprintf invalid_arg "Lwt.%s" api_function_name

    | Pending _ ->
      let result = state_of_result result in
      let State_may_have_changed p = resolve ~allow_deferring:false p result in
      ignore p

  let wakeup_result r result = wakeup_general "wakeup_result" r result
  let wakeup r v = wakeup_general "wakeup" r (Result.Ok v)
  let wakeup_exn r exn = wakeup_general "wakeup_exn" r (Result.Error exn)

  let wakeup_later_general api_function_name r result =
    let Internal p = to_internal_resolver r in
    let p = underlying p in

    match p.state with
    | Rejected Canceled ->
      ()
    | Fulfilled _ ->
      Printf.ksprintf invalid_arg "Lwt.%s" api_function_name
    | Rejected _ ->
      Printf.ksprintf invalid_arg "Lwt.%s" api_function_name

    | Pending _ ->
      let result = state_of_result result in
      let State_may_have_changed p =
        resolve ~maximum_callback_nesting_depth:1 p result in
      ignore p

  let wakeup_later_result r result =
    wakeup_later_general "wakeup_later_result" r result
  let wakeup_later r v =
    wakeup_later_general "wakeup_later" r (Result.Ok v)
  let wakeup_later_exn r exn =
    wakeup_later_general "wakeup_later_exn" r (Result.Error exn)



  type packed_callbacks =
    | Packed : _ callbacks -> packed_callbacks
    [@@ocaml.unboxed]

  (* Note that this function deviates from the "ideal" callback deferral
     behavior: it runs callbacks directly on the current stack. It should
     therefore be possible to cause a stack overflow using this function. *)
  let cancel p =
    let canceled_result = Rejected Canceled in

    (* Walks the promise dependency graph backwards, looking for cancelable
       initial promises, and cancels (only) them.

       Found initial promises are canceled immediately, as they are found, by
       setting their state to [Rejected Canceled]. This is to prevent them from
       being "found twice" if they are reachable by two or more distinct paths
       through the promise dependency graph.

       The callbacks of these initial promises are then run, in a separate
       phase. These callbacks propagate cancellation forwards to any dependent
       promises. See "Cancellation" in the Overview. *)
    let propagate_cancel : (_, _, _) promise -> packed_callbacks list =
        fun p ->
      let rec cancel_and_collect_callbacks :
          'a 'u 'c. packed_callbacks list -> ('a, 'u, 'c) promise ->
            packed_callbacks list =
          fun (type c) callbacks_accumulator (p : (_, _, c) promise) ->
        let p = underlying p in
        match p.state with
        | Fulfilled _ ->
          callbacks_accumulator
        | Rejected _ ->
          callbacks_accumulator

        | Pending callbacks ->
          match callbacks.how_to_cancel with
          | Not_cancelable ->
            callbacks_accumulator
          | Cancel_this_promise ->
            let State_may_have_changed p =
              set_promise_state p canceled_result in
            ignore p;
            (Packed callbacks)::callbacks_accumulator
          | Propagate_cancel_to_one p' ->
            cancel_and_collect_callbacks callbacks_accumulator p'
          | Propagate_cancel_to_several ps ->
            List.fold_left cancel_and_collect_callbacks callbacks_accumulator ps
      in
      cancel_and_collect_callbacks [] p
    in

    let Internal p = to_internal_promise p in
    let callbacks = propagate_cancel p in

    callbacks |> List.iter (fun (Packed callbacks) ->
      run_callbacks_or_defer_them
        ~allow_deferring:false callbacks canceled_result)
end
include Resolving



module Trivial_promises :
sig
  val return : 'a -> 'a t
  val fail : exn -> _ t
  val of_result : 'a lwt_result -> 'a t

  val return_unit : unit t
  val return_true : bool t
  val return_false : bool t
  val return_none : _ option t
  val return_some : 'a -> 'a option t
  val return_ok : 'a -> ('a, _) Result.result t
  val return_error : 'e -> (_, 'e) Result.result t
  val return_nil : _ list t

  val fail_with : string -> _ t
  val fail_invalid_arg : string -> _ t
end =
struct
  let return v =
    to_public_promise {state = Fulfilled v}

  let of_result result =
    to_public_promise {state = state_of_result result}

  let fail exn =
    to_public_promise {state = Rejected exn}

  let return_unit = return ()
  let return_none = return None
  let return_some x = return (Some x)
  let return_nil = return []
  let return_true = return true
  let return_false = return false
  let return_ok x = return (Result.Ok x)
  let return_error x = return (Result.Error x)

  let fail_with msg =
    to_public_promise {state = Rejected (Failure msg)}

  let fail_invalid_arg msg =
    to_public_promise {state = Rejected (Invalid_argument msg)}
end
include Trivial_promises



module Pending_promises :
sig
  (* Internal *)
  val new_pending :
    how_to_cancel:how_to_cancel -> ('a, underlying, pending) promise
  val propagate_cancel_to_several : _ t list -> how_to_cancel

  (* Initial pending promises (public) *)
  val wait : unit -> 'a t * 'a u
  val task : unit -> 'a t * 'a u

  val waiter_of_wakener : 'a u -> 'a t

  val add_task_r : 'a u Lwt_sequence.t -> 'a t
  val add_task_l : 'a u Lwt_sequence.t -> 'a t

  val protected : 'a t -> 'a t
  val no_cancel : 'a t -> 'a t
end =
struct
  let new_pending ~how_to_cancel =
    let state =
      Pending {
        regular_callbacks = Regular_callback_list_empty;
        cancel_callbacks = Cancel_callback_list_empty;
        how_to_cancel;
        cleanups_deferred = 0;
      }
    in
    {state}

  let propagate_cancel_to_several ps =
    (* Using a dirty cast here to avoid rebuilding the list :( Not bothering
       with the invariants, because [Propagate_cancel_to_several] packs them,
       and code that matches on [Propagate_cancel_to_several] doesn't care about
       them anyway. *)
    let cast_promise_list : 'a t list -> ('a, _, _) promise list = Obj.magic in
    Propagate_cancel_to_several (cast_promise_list ps)



  let wait () =
    let p = new_pending ~how_to_cancel:Not_cancelable in
    to_public_promise p, to_public_resolver p

  let task () =
    let p = new_pending ~how_to_cancel:Cancel_this_promise in
    to_public_promise p, to_public_resolver p



  let waiter_of_wakener r =
    let Internal r = to_internal_resolver r in
    let p = r in
    to_public_promise p



  let cast_sequence_node
      (node : 'a u Lwt_sequence.node)
      (_actual_content:('a, 'u, 'c) promise)
        : ('a, 'u, 'c) promise Lwt_sequence.node =
    Obj.magic node

  let add_task_r sequence =
    let p = new_pending ~how_to_cancel:Cancel_this_promise in
    let node = Lwt_sequence.add_r (to_public_resolver p) sequence in
    let node = cast_sequence_node node p in

    let Pending callbacks = p.state in
    callbacks.cancel_callbacks <-
      Cancel_callback_list_remove_sequence_node node;

    to_public_promise p

  let add_task_l sequence =
    let p = new_pending ~how_to_cancel:Cancel_this_promise in
    let node = Lwt_sequence.add_l (to_public_resolver p) sequence in
    let node = cast_sequence_node node p in

    let Pending callbacks = p.state in
    callbacks.cancel_callbacks <-
      Cancel_callback_list_remove_sequence_node node;

    to_public_promise p



  let protected p =
    let Internal p_internal = to_internal_promise p in
    match (underlying p_internal).state with
    | Fulfilled _ -> p
    | Rejected _ -> p

    | Pending _ ->
      let p' = new_pending ~how_to_cancel:Cancel_this_promise in

      let callback p_result =
        let State_may_now_be_pending_proxy p' = may_now_be_proxy p' in
        let p' = underlying p' in
        (* In this callback, [p'] will either still itself be pending, or it
           will have become a proxy for a pending promise. The reasoning for
           this is almost the same as in the comment at [may_now_be_proxy]. The
           differences are:

           - [p'] *is* an initial promise, so it *can* get canceled. However, if
             it does, the [on_cancel] handler installed below will remove this
             callback.
           - [p'] never gets passed to [make_into_proxy], the only effect of
             which is that it cannot be the underlying promise of another
             (proxy) promise. So, [p'] can only appear at the head of a chain of
             [Proxy _] links, and it's not necessary to worry about whether the
             inductive reasoning at [may_now_be_proxy] applies. *)

        let State_may_have_changed p' =
          resolve ~allow_deferring:false p' p_result in
        ignore p'
      in

      let remove_the_callback =
        add_explicitly_removable_callback_and_give_remove_function
          [p] callback
      in

      let Pending p'_callbacks = p'.state in
      add_cancel_callback p'_callbacks remove_the_callback;

      to_public_promise p'

  let no_cancel p =
    let Internal p_internal = to_internal_promise p in
    match (underlying p_internal).state with
    | Fulfilled _ -> p
    | Rejected _ -> p

    | Pending p_callbacks ->
      let p' = new_pending ~how_to_cancel:Not_cancelable in

      let callback p_result =
        let State_may_now_be_pending_proxy p' = may_now_be_proxy p' in
        let p' = underlying p' in
        (* In this callback, [p'] will either still itself be pending, or it
           will have become a proxy for a pending promise. The reasoning for
           this is as in [protected] and [may_now_be_proxy], but even simpler,
           because [p'] is not cancelable. *)

        let State_may_have_changed p' =
          resolve ~allow_deferring:false p' p_result in
        ignore p'
      in
      add_implicitly_removed_callback p_callbacks callback;

      to_public_promise p'
end
include Pending_promises



module Sequential_composition :
sig
  (* Main interface (public) *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  val finalize : (unit -> 'a t) -> (unit -> unit t) -> 'a t
  val try_bind : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t

  (* Cancel callbacks (public). *)
  val on_cancel : 'a t -> (unit -> unit) -> unit

  (* Non-promise callbacks (public) *)
  val on_success : 'a t -> ('a -> unit) -> unit
  val on_failure : _ t -> (exn -> unit) -> unit
  val on_termination : _ t -> (unit -> unit) -> unit
  val on_any : 'a t -> ('a -> unit) -> (exn -> unit) -> unit

  (* Backtrace support (internal; for use by the PPX) *)
  val backtrace_bind :
    (exn -> exn) -> 'a t -> ('a -> 'b t) -> 'b t
  val backtrace_catch :
    (exn -> exn) -> (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  val backtrace_finalize :
    (exn -> exn) -> (unit -> 'a t) -> (unit -> unit t) -> 'a t
  val backtrace_try_bind :
    (exn -> exn) -> (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
end =
struct
  (* There are five primary sequential composition functions: [bind], [map],
     [catch], [finalize], and [try_bind]. Of these, [try_bind] is the most
     general -- all the others can be implemented in terms of it.

     Lwt conflates concurrency with error propagation. If Lwt did not do this,
     there would be only two primary functions: [bind] and [map], and, of these
     two, [bind] is the most general. Since [bind] is the most relevant
     specifically to concurrency, and is also the most familiar function in Lwt,
     its implementation serves as a kind of "model" for the rest. It is the most
     commented, and all the other functions follow a similar pattern to [bind].

     Four of the primary functions have [backtrace_*] versions, which are not
     truly public, and exist to support the PPX. [backtrace_map] does not exist
     because the PPX does not need it.

     The remaining four functions in this section attach "lower-level-ish"
     non-promise-producing callbacks to promises: these are the [on_*]
     functions. Of these, [on_any] is the most general. If Lwt did not conflate
     concurrency with error handling, there would only be one: [on_success]. *)



  (* Makes [~user_provided_promise] into a proxy of [~outer_promise]. After
     [make_into_proxy], these two promise references "behave identically."

     Note that this is not symmetric: [user_provided_promise] always becomes the
     proxy. [make_into_proxy] is called only by [bind] and similar functions in
     this module. This means that:

     - the only way for a promise to become a proxy is by being returned from
       the callback given by the user to [bind], or a similar function, and
     - the only way for a promise to become underlying for a promise other than
       itself is to be the outer promise originally returned to the user from
       [bind], or a similar function.

     These two facts are important for reasoning about how and which promises
     can become proxies, underlying, etc.; in particular, it is used in the
     argument in [may_now_be_proxy] for correct predictions about state changes.

     [~outer_promise] is always a pending promise when [make_into_proxy] is
     called; for the explanation, see [may_now_be_proxy] (though the caller of
     [make_into_proxy] always calls [underlying] first to pass the underlying
     pending promise to [make_into_proxy]).

     The reasons proxying is used, instead of adding a callback to
     [~user_provided_promise] to resolve [~outer_promise] when the former
     becomes resolved probably are:

     - Promises have more behaviors than resolution. One would have to add a
       cancellation handler to [~outer_promise] to propagate the cancellation
       back to [~user_provided_promise], for example. It may be easier to just
       think of them as the same promise.
     - If using callbacks, resolving [~user_provided_promise] would not
       immediately resolve [~outer_promise]. Another callback added to
       [~user_provided_promise] might see [~user_provided_promise] resolved,
       but [~outer_promise] still pending, depending on the order in which
       callbacks are run. *)
  let make_into_proxy
      (type c)
      ~(outer_promise : ('a, underlying, pending) promise)
      ~(user_provided_promise : ('a, _, c) promise)
        : ('a, underlying, c) state_changed =

    (* Using [p'] as it's the name used inside [bind], etc., for promises with
       this role -- [p'] is the promise returned by the user's function. *)
    let p' = underlying user_provided_promise in

    if identical p' outer_promise then
      State_may_have_changed p'
      (* We really want to return [State_may_have_changed outer_promise], but
         the reference through [p'] has the right type. *)

    else
      match p'.state with
      | Fulfilled _ ->
        resolve ~allow_deferring:false outer_promise p'.state
      | Rejected _ ->
        resolve ~allow_deferring:false outer_promise p'.state

      | Pending p'_callbacks ->
        let Pending outer_callbacks = outer_promise.state in

        merge_callbacks ~from:p'_callbacks ~into:outer_callbacks;
        outer_callbacks.how_to_cancel <- p'_callbacks.how_to_cancel;

        let State_may_have_changed p' =
          set_promise_state p' (Proxy outer_promise) in
        ignore p';

        State_may_have_changed outer_promise
        (* The state hasn't actually changed, but we still have to wrap
           [outer_promise] for type checking. *)

        (* The state of [p'] may instead have changed -- it may have become a
           proxy. However, callers of [make_into_proxy] don't know if
           [user_provided_promise] was a proxy or not (that's why we call
           underlying on it at the top of this function, to get [p']). We can
           therefore take a dangerous shortcut and not bother returning a new
           reference to [user_provided_promise] for shadowing. *)



  (* Maintainer's note: a lot of the code below can probably be deduplicated in
     some way, especially if assuming Flambda. *)

  let bind p f =
    let Internal p = to_internal_promise p in
    let p = underlying p in

    (* In case [Lwt.bind] needs to defer the call to [f], this function will be
       called to create:

       1. The promise, [p''], that must be returned to the caller immediately.
       2. The callback that resolves [p''].

       [Lwt.bind] defers the call to [f] in two circumstances:

       1. The promise [p] is pending.
       2. The promise [p] is fulfilled, but the current callback call nesting
          depth is such that the call to [f] must go into the callback queue, in
          order to avoid stack overflow.

      Mechanism (2) is currently disabled. It may be used in an alternative Lwt
      API.

      Functions other than [Lwt.bind] have analogous deferral behavior. *)
    let create_result_promise_and_callback_if_deferred () =
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in
      (* The result promise is a fresh pending promise.

         Initially, trying to cancel this fresh pending promise [p''] will
         propagate the cancellation attempt to [p] (backwards through the
         promise dependency graph). If/when [p] is fulfilled, Lwt will call the
         user's callback [f] below, which will provide a new promise [p'], and
         [p'] will become a proxy of [p'']. At that point, trying to cancel
         [p''] will be equivalent to trying to cancel [p'], so the behavior will
         depend on how the user obtained [p']. *)

      let saved_storage = !current_storage in

      let callback p_result =
        match p_result with
        | Fulfilled v ->
          current_storage := saved_storage;

          let p' = try f v with exn -> fail exn in
          let Internal p' = to_internal_promise p' in
          (* Run the user's function [f]. *)

          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in
          (* [p''] was an underlying promise when it was created above, but it
             may have become a proxy by the time this code is being executed.
             However, it is still either an underlying pending promise, or a
             proxy for a pending promise. Therefore, [may_now_be_proxy] produces
             a reference with the right type variables. We immediately get
             [p'']'s current underlying promise. *)

          let State_may_have_changed p'' =
            make_into_proxy ~outer_promise:p'' ~user_provided_promise:p' in
          ignore p''
          (* Make the outer promise [p''] behaviorally identical to the promise
             [p'] returned by [f] by making [p'] into a proxy of [p'']. *)

        | Rejected _ as p_result ->
          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            resolve ~allow_deferring:false p'' p_result in
          ignore p''
      in

      (to_public_promise p'', callback)
    in

    match p.state with
    | Fulfilled v ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> f v)
        ~if_deferred:(fun () ->
          let (p'', callback) =
            create_result_promise_and_callback_if_deferred () in
          (p'', callback, p.state))

    | Rejected _ as result ->
      to_public_promise {state = result}

    | Pending p_callbacks ->
      let (p'', callback) = create_result_promise_and_callback_if_deferred () in
      add_implicitly_removed_callback p_callbacks callback;
      p''

  let backtrace_bind add_loc p f =
    let Internal p = to_internal_promise p in
    let p = underlying p in

    let create_result_promise_and_callback_if_deferred () =
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in

      let saved_storage = !current_storage in

      let callback p_result =
        match p_result with
        | Fulfilled v ->
          current_storage := saved_storage;

          let p' = try f v with exn -> fail (add_loc exn) in
          let Internal p' = to_internal_promise p' in

          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            make_into_proxy ~outer_promise:p'' ~user_provided_promise:p' in
          ignore p''

        | Rejected exn ->
          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            resolve ~allow_deferring:false p'' (Rejected (add_loc exn)) in
          ignore p''
      in

      (to_public_promise p'', callback)
    in

    match p.state with
    | Fulfilled v ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> f v)
        ~if_deferred:(fun () ->
          let (p'', callback) =
            create_result_promise_and_callback_if_deferred () in
          (p'', callback, p.state))

    | Rejected exn ->
      to_public_promise {state = Rejected (add_loc exn)}

    | Pending p_callbacks ->
      let (p'', callback) = create_result_promise_and_callback_if_deferred () in
      add_implicitly_removed_callback p_callbacks callback;
      p''

  let map f p =
    let Internal p = to_internal_promise p in
    let p = underlying p in

    let create_result_promise_and_callback_if_deferred () =
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in

      let saved_storage = !current_storage in

      let callback p_result =
        match p_result with
        | Fulfilled v ->
          current_storage := saved_storage;

          let p''_result = try Fulfilled (f v) with exn -> Rejected exn in

          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            resolve ~allow_deferring:false p'' p''_result in
          ignore p''

        | Rejected _ as p_result ->
          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            resolve ~allow_deferring:false p'' p_result in
          ignore p''
      in

      (to_public_promise p'', callback)
    in

    match p.state with
    | Fulfilled v ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () ->
          to_public_promise
            {state = try Fulfilled (f v) with exn -> Rejected exn})
        ~if_deferred:(fun () ->
          let (p'', callback) =
            create_result_promise_and_callback_if_deferred () in
          (p'', callback, p.state))

    | Rejected _ as result ->
      to_public_promise {state = result}

    | Pending p_callbacks ->
      let (p'', callback) = create_result_promise_and_callback_if_deferred () in
      add_implicitly_removed_callback p_callbacks callback;
      p''

  let catch f h =
    let p = try f () with exn -> fail exn in
    let Internal p = to_internal_promise p in
    let p = underlying p in

    let create_result_promise_and_callback_if_deferred () =
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in

      let saved_storage = !current_storage in

      let callback p_result =
        match p_result with
        | Fulfilled _ as p_result ->
          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            resolve ~allow_deferring:false p'' p_result in
          ignore p''

        | Rejected exn ->
          current_storage := saved_storage;

          let p' = try h exn with exn -> fail exn in
          let Internal p' = to_internal_promise p' in

          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            make_into_proxy ~outer_promise:p'' ~user_provided_promise:p' in
          ignore p''
      in

      (to_public_promise p'', callback)
    in

    match p.state with
    | Fulfilled _ ->
      to_public_promise p

    | Rejected exn ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> h exn)
        ~if_deferred:(fun () ->
          let (p'', callback) =
            create_result_promise_and_callback_if_deferred () in
          (p'', callback, p.state))

    | Pending p_callbacks ->
      let (p'', callback) = create_result_promise_and_callback_if_deferred () in
      add_implicitly_removed_callback p_callbacks callback;
      p''

  let backtrace_catch add_loc f h =
    let p = try f () with exn -> fail exn in
    let Internal p = to_internal_promise p in
    let p = underlying p in

    let create_result_promise_and_callback_if_deferred () =
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in

      let saved_storage = !current_storage in

      let callback p_result =
        match p_result with
        | Fulfilled _ as p_result ->
          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            resolve ~allow_deferring:false p'' p_result in
          ignore p''

        | Rejected exn ->
          current_storage := saved_storage;

          let p' = try h exn with exn -> fail (add_loc exn) in
          let Internal p' = to_internal_promise p' in

          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            make_into_proxy ~outer_promise:p'' ~user_provided_promise:p' in
          ignore p''
      in

      (to_public_promise p'', callback)
    in

    match p.state with
    | Fulfilled _ ->
      to_public_promise p

    | Rejected exn ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> h (add_loc exn))
        ~if_deferred:(fun () ->
          let (p'', callback) =
            create_result_promise_and_callback_if_deferred () in
          (p'', callback, p.state))

    | Pending p_callbacks ->
      let (p'', callback) = create_result_promise_and_callback_if_deferred () in
      add_implicitly_removed_callback p_callbacks callback;
      p''

  let try_bind f f' h =
    let p = try f () with exn -> fail exn in
    let Internal p = to_internal_promise p in
    let p = underlying p in

    let create_result_promise_and_callback_if_deferred () =
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in

      let saved_storage = !current_storage in

      let callback p_result =
        match p_result with
        | Fulfilled v ->
          current_storage := saved_storage;

          let p' = try f' v with exn -> fail exn in
          let Internal p' = to_internal_promise p' in

          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            make_into_proxy ~outer_promise:p'' ~user_provided_promise:p' in
          ignore p''

        | Rejected exn ->
          current_storage := saved_storage;

          let p' = try h exn with exn -> fail exn in
          let Internal p' = to_internal_promise p' in

          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            make_into_proxy ~outer_promise:p'' ~user_provided_promise:p' in
          ignore p''
      in

      (to_public_promise p'', callback)
    in

    match p.state with
    | Fulfilled v ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> f' v)
        ~if_deferred:(fun () ->
          let (p'', callback) =
            create_result_promise_and_callback_if_deferred () in
          (p'', callback, p.state))

    | Rejected exn ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> h exn)
        ~if_deferred:(fun () ->
          let (p'', callback) =
            create_result_promise_and_callback_if_deferred () in
          (p'', callback, p.state))

    | Pending p_callbacks ->
      let (p'', callback) = create_result_promise_and_callback_if_deferred () in
      add_implicitly_removed_callback p_callbacks callback;
      p''

  let backtrace_try_bind add_loc f f' h =
    let p = try f () with exn -> fail exn in
    let Internal p = to_internal_promise p in
    let p = underlying p in

    let create_result_promise_and_callback_if_deferred () =
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in

      let saved_storage = !current_storage in

      let callback p_result =
        match p_result with
        | Fulfilled v ->
          current_storage := saved_storage;

          let p' = try f' v with exn -> fail (add_loc exn) in
          let Internal p' = to_internal_promise p' in

          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            make_into_proxy ~outer_promise:p'' ~user_provided_promise:p' in
          ignore p''

        | Rejected exn ->
          current_storage := saved_storage;

          let p' = try h exn with exn -> fail (add_loc exn) in
          let Internal p' = to_internal_promise p' in

          let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
          let p'' = underlying p'' in

          let State_may_have_changed p'' =
            make_into_proxy ~outer_promise:p'' ~user_provided_promise:p' in
          ignore p''
      in

      (to_public_promise p'', callback)
    in

    match p.state with
    | Fulfilled v ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> f' v)
        ~if_deferred:(fun () ->
          let (p'', callback) =
            create_result_promise_and_callback_if_deferred () in
          (p'', callback, p.state))

    | Rejected exn ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> h (add_loc exn))
        ~if_deferred:(fun () ->
          let (p'', callback) =
            create_result_promise_and_callback_if_deferred () in
          (p'', callback, p.state))

    | Pending p_callbacks ->
      let (p'', callback) = create_result_promise_and_callback_if_deferred () in
      add_implicitly_removed_callback p_callbacks callback;
      p''

  let finalize f f' =
    try_bind f
      (fun x -> bind (f' ()) (fun () -> return x))
      (fun e -> bind (f' ()) (fun () -> fail e))

  let backtrace_finalize add_loc f f' =
    backtrace_try_bind add_loc f
      (fun x -> bind (f' ()) (fun () -> return x))
      (fun e -> bind (f' ()) (fun () -> fail (add_loc e)))



  let on_cancel p f =
    let Internal p = to_internal_promise p in
    let p = underlying p in

    match p.state with
    | Rejected Canceled ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> handle_with_async_exception_hook f ())
        ~if_deferred:(fun () ->
          ((), (fun _ -> handle_with_async_exception_hook f ()), Fulfilled ()))

    | Rejected _ ->
      ()

    | Fulfilled _ ->
      ()

    | Pending callbacks ->
      add_cancel_callback callbacks f



  let on_success p f =
    let Internal p = to_internal_promise p in
    let p = underlying p in

    let callback_if_deferred () =
      let saved_storage = !current_storage in

      fun result ->
        match result with
        | Fulfilled v ->
          current_storage := saved_storage;
          handle_with_async_exception_hook f v

        | Rejected _ ->
          ()
    in

    match p.state with
    | Fulfilled v ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> handle_with_async_exception_hook f v)
        ~if_deferred:(fun () ->
          let callback = callback_if_deferred () in
          ((), callback, p.state))

    | Rejected _ ->
      ()

    | Pending p_callbacks ->
      let callback = callback_if_deferred () in
      add_implicitly_removed_callback p_callbacks callback

  let on_failure p f =
    let Internal p = to_internal_promise p in
    let p = underlying p in

    let callback_if_deferred () =
      let saved_storage = !current_storage in

      fun result ->
        match result with
        | Fulfilled _ ->
          ()

        | Rejected exn ->
          current_storage := saved_storage;
          handle_with_async_exception_hook f exn
    in

    match p.state with
    | Fulfilled _ ->
      ()

    | Rejected exn ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> handle_with_async_exception_hook f exn)
        ~if_deferred:(fun () ->
          let callback = callback_if_deferred () in
          ((), callback, p.state))

    | Pending p_callbacks ->
      let callback = callback_if_deferred () in
      add_implicitly_removed_callback p_callbacks callback

  let on_termination p f =
    let Internal p = to_internal_promise p in
    let p = underlying p in

    let callback_if_deferred () =
      let saved_storage = !current_storage in

      fun _result ->
        current_storage := saved_storage;
        handle_with_async_exception_hook f ()
    in

    match p.state with
    | Fulfilled _ ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> handle_with_async_exception_hook f ())
        ~if_deferred:(fun () ->
          let callback = callback_if_deferred () in
          ((), callback, p.state))

    | Rejected _ ->
      run_callback_or_defer_it
      ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> handle_with_async_exception_hook f ())
        ~if_deferred:(fun () ->
          let callback = callback_if_deferred () in
          ((), callback, p.state))

    | Pending p_callbacks ->
      let callback = callback_if_deferred () in
      add_implicitly_removed_callback p_callbacks callback

  let on_any p f g =
    let Internal p = to_internal_promise p in
    let p = underlying p in

    let callback_if_deferred () =
      let saved_storage = !current_storage in

      fun result ->
        match result with
        | Fulfilled v ->
          current_storage := saved_storage;
          handle_with_async_exception_hook f v

        | Rejected exn ->
          current_storage := saved_storage;
          handle_with_async_exception_hook g exn
    in

    match p.state with
    | Fulfilled v ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> handle_with_async_exception_hook f v)
        ~if_deferred:(fun () ->
          let callback = callback_if_deferred () in
          ((), callback, p.state))

    | Rejected exn ->
      run_callback_or_defer_it
        ~run_immediately_and_ensure_tail_call:true
        ~callback:(fun () -> handle_with_async_exception_hook g exn)
        ~if_deferred:(fun () ->
          let callback = callback_if_deferred () in
          ((), callback, p.state))

    | Pending p_callbacks ->
      let callback = callback_if_deferred () in
      add_implicitly_removed_callback p_callbacks callback
end
include Sequential_composition



module Concurrent_composition :
sig
  val async : (unit -> _ t) -> unit
  val ignore_result : _ t -> unit

  val both : 'a t -> 'b t -> ('a * 'b) t
  val join : unit t list -> unit t
  val all : ('a t) list -> ('a list) t

  val choose : 'a t list -> 'a t
  val pick : 'a t list -> 'a t

  val nchoose : 'a t list -> 'a list t
  val npick : 'a t list -> 'a list t

  val nchoose_split : 'a t list -> ('a list * 'a t list) t
end =
struct
  external reraise : exn -> 'a = "%reraise"

  let async f =
    let p = try f () with exn -> fail exn in
    let Internal p = to_internal_promise p in

    match (underlying p).state with
    | Fulfilled _ ->
      ()
    | Rejected exn ->
      !async_exception_hook exn

    | Pending p_callbacks ->
      let callback result =
        match result with
        | Fulfilled _ ->
          ()
        | Rejected exn ->
          !async_exception_hook exn
      in
      add_implicitly_removed_callback p_callbacks callback

  let ignore_result p =
    let Internal p = to_internal_promise p in

    match (underlying p).state with
    | Fulfilled _ ->
      ()
    | Rejected exn ->
      reraise exn

    | Pending p_callbacks ->
      let callback result =
        match result with
        | Fulfilled _ ->
          ()
        | Rejected exn ->
          !async_exception_hook exn
      in
      add_implicitly_removed_callback p_callbacks callback



  let join ps =
    let p' = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in

    let number_pending_in_ps = ref 0 in
    let join_result = ref (Fulfilled ()) in

    (* Callback attached to each promise in [ps] that is still pending at the
       time [join] is called. *)
    let callback new_result =
      let State_may_now_be_pending_proxy p' = may_now_be_proxy p' in

      begin match new_result with
      | Fulfilled () -> ()
      | Rejected _ ->
      (* For the first promise in [ps] to be rejected, set the result of the
         [join] to rejected with the same exception.. *)
        match !join_result with
        | Fulfilled () -> join_result := new_result
        | Rejected _ -> ()
      end;

      (* In all cases, decrement the number of promises still pending, and
         resolve the [join] once all promises resolve. *)
      number_pending_in_ps := !number_pending_in_ps - 1;
      if !number_pending_in_ps = 0 then begin
        let p' = underlying p' in
        let State_may_have_changed p' =
          resolve ~allow_deferring:false (underlying p') !join_result in
        ignore p'
      end
    in

    (* Attach the above callback. Simultaneously count how many pending promises
       there are in [ps] (initially). If that number is zero, the [join] must
       resolve immediately. *)
    let rec attach_callback_or_resolve_immediately ps =
      match ps with
      | [] ->
        if !number_pending_in_ps = 0 then
          to_public_promise {state = !join_result}
        else
          to_public_promise p'

      | p::ps ->
        let Internal p = to_internal_promise p in

        match (underlying p).state with
        | Pending p_callbacks ->
          number_pending_in_ps := !number_pending_in_ps + 1;
          add_implicitly_removed_callback p_callbacks callback;
          attach_callback_or_resolve_immediately ps

        | Rejected _ as p_result ->
          (* As in the callback above, but for already-resolved promises in
             [ps]: reject the [join] with the same exception as in the first
             rejected promise found. [join] still waits for any pending promises
             before actually resolving, though. *)
          begin match !join_result with
          | Fulfilled () -> join_result := p_result;
          | Rejected _ -> ()
          end;
          attach_callback_or_resolve_immediately ps

        | Fulfilled () ->
          attach_callback_or_resolve_immediately ps
    in

    attach_callback_or_resolve_immediately ps

  let both p1 p2 =
    let v1 = ref None in
    let v2 = ref None in
    let p1' = bind p1 (fun v -> v1 := Some v; return_unit) in
    let p2' = bind p2 (fun v -> v2 := Some v; return_unit) in
    join [p1'; p2'] |> map (fun () ->
      match !v1, !v2 with
      | Some v1, Some v2 -> v1, v2
      | _ -> assert false)

  let all ps =
    let vs = Array.make (List.length ps) None in
    ps
    |> List.mapi (fun index p ->
      bind p (fun v -> vs.(index) <- Some v; return_unit))
    |> join
    |> map (fun () ->
      vs
      |> Array.map (fun v ->
        match v with
        | Some v -> v
        | None -> assert false)
      |> Array.to_list)



  (* Maintainer's note: the next few functions are helpers for [choose] and
     [pick]. Perhaps they should be factored into some kind of generic
     [choose]/[pick] implementation, which may actually be optimal anyway with
     Flambda. *)

  let count_resolved_promises_in (ps : _ t list) =
    let accumulate total p =
      let Internal p = to_internal_promise p in
      match (underlying p).state with
      | Fulfilled _ -> total + 1
      | Rejected _ -> total + 1
      | Pending _ -> total
    in
    List.fold_left accumulate 0 ps

  (* Evaluates to the [n]th promise in [ps], among only those promises in [ps]
     that are resolved. The caller is expected to ensure that there are at
     least [n] resolved promises in [ps]. *)
  let rec nth_resolved (ps : 'a t list) (n : int) : 'a t =
    match ps with
    | [] ->
      assert false

    | p::ps ->
      let Internal p' = to_internal_promise p in
      match (underlying p').state with
      | Pending _ ->
        nth_resolved ps n

      | Fulfilled _ ->
        if n <= 0 then p
        else nth_resolved ps (n - 1)
      | Rejected _ ->
        if n <= 0 then p
        else nth_resolved ps (n - 1)

  (* Like [nth_resolved], but cancels all pending promises found while
     traversing [ps]. *)
  let rec nth_resolved_and_cancel_pending (ps : 'a t list) (n : int) : 'a t =
    match ps with
    | [] ->
      assert false

    | p::ps ->
      let Internal p' = to_internal_promise p in
      match (underlying p').state with
      | Pending _ ->
        cancel p;
        nth_resolved_and_cancel_pending ps n

      | Fulfilled _ ->
        if n <= 0 then (List.iter cancel ps; p)
        else nth_resolved_and_cancel_pending ps (n - 1)
      | Rejected _ ->
        if n <= 0 then (List.iter cancel ps; p)
        else nth_resolved_and_cancel_pending ps (n - 1)

  (* The PRNG state is initialized with a constant to make non-IO-based programs
     deterministic. *)
  (* Maintainer's note: is this necessary? *)
  let prng = lazy (Random.State.make [||])

  let choose ps =
    if ps = [] then
      invalid_arg
        "Lwt.choose [] would return a promise that is pending forever";
    match count_resolved_promises_in ps with
    | 0 ->
      let p = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in

      let callback result =
        let State_may_now_be_pending_proxy p = may_now_be_proxy p in
        let p = underlying p in
        let State_may_have_changed p =
          resolve ~allow_deferring:false p result in
        ignore p
      in
      add_explicitly_removable_callback_to_each_of ps callback;

      to_public_promise p

    | 1 ->
      nth_resolved ps 0

    | n ->
      nth_resolved ps (Random.State.int (Lazy.force prng) n)

  let pick ps =
    if ps = [] then
      invalid_arg "Lwt.pick [] would return a promise that is pending forever";
    match count_resolved_promises_in ps with
    | 0 ->
      let p = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in

      let callback result =
        let State_may_now_be_pending_proxy p = may_now_be_proxy p in
        List.iter cancel ps;
        let p = underlying p in
        let State_may_have_changed p =
          resolve ~allow_deferring:false p result in
        ignore p
      in
      add_explicitly_removable_callback_to_each_of ps callback;

      to_public_promise p

    | 1 ->
      nth_resolved_and_cancel_pending ps 0

    | n ->
      nth_resolved_and_cancel_pending ps
        (Random.State.int (Lazy.force prng) n)



  (* If [nchoose ps] or [npick ps] found all promises in [ps] pending, the
     callback added to each promise in [ps] eventually calls this function. The
     function collects promises in [ps] that have become fulfilled, or finds one
     promise in [ps] that has been rejected. It then returns the desired state
     of the final promise: either the list of results collected, or the
     exception found. *)
  let rec collect_fulfilled_promises_after_pending
      (results : 'a list)
      (ps : 'a t list) :
        ('a list resolved_state) =

    match ps with
    | [] ->
      Fulfilled (List.rev results)

    | p::ps ->
      let Internal p = to_internal_promise p in

      match (underlying p).state with
      | Fulfilled v ->
        collect_fulfilled_promises_after_pending (v::results) ps

      | Rejected _ as result ->
        result

      | Pending _ ->
        collect_fulfilled_promises_after_pending results ps

  let nchoose ps =
    (* If at least one promise in [ps] is found fulfilled, this function is
       called to find all such promises. *)
    if ps = [] then
      invalid_arg
        "Lwt.nchoose [] would return a promise that is pending forever";
    let rec collect_already_fulfilled_promises_or_find_rejected acc ps =
      match ps with
      | [] ->
        return (List.rev acc)

      | p::ps ->
        let Internal p = to_internal_promise p in
        match (underlying p).state with
        | Fulfilled v ->
          collect_already_fulfilled_promises_or_find_rejected (v::acc) ps

        | Rejected _ as result ->
          to_public_promise {state = result}

        | Pending _ ->
          collect_already_fulfilled_promises_or_find_rejected acc ps
    in

    (* Looks for already-resolved promises in [ps]. If none are fulfilled or
       rejected, adds a callback to all promises in [ps] (all of which are
       pending). *)
    let rec check_for_already_resolved_promises ps' =
      match ps' with
      | [] ->
        let p = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in

        let callback _result =
          let State_may_now_be_pending_proxy p = may_now_be_proxy p in
          let p = underlying p in
          let result = collect_fulfilled_promises_after_pending [] ps in
          let State_may_have_changed p =
            resolve ~allow_deferring:false p result in
          ignore p
        in
        add_explicitly_removable_callback_to_each_of ps callback;

        to_public_promise p

      | p::ps ->
        let Internal p = to_internal_promise p in
        match (underlying p).state with
        | Fulfilled v ->
          collect_already_fulfilled_promises_or_find_rejected [v] ps

        | Rejected _ as result ->
          to_public_promise {state = result}

        | Pending _ ->
          check_for_already_resolved_promises ps
    in

    let p = check_for_already_resolved_promises ps in
    p

  (* See [nchoose]. This function differs only in having additional calls to
     [cancel]. *)
  let npick ps =
    if ps = [] then
      invalid_arg "Lwt.npick [] would return a promise that is pending forever";
    let rec collect_already_fulfilled_promises_or_find_rejected acc ps' =
      match ps' with
      | [] ->
        List.iter cancel ps;
        return (List.rev acc)

      | p::ps' ->
        let Internal p = to_internal_promise p in
        match (underlying p).state with
        | Fulfilled v ->
          collect_already_fulfilled_promises_or_find_rejected (v::acc) ps'

        | Rejected _ as result ->
          List.iter cancel ps;
          to_public_promise {state = result}

        | Pending _ ->
          collect_already_fulfilled_promises_or_find_rejected acc ps'
    in

    let rec check_for_already_resolved_promises ps' =
      match ps' with
      | [] ->
        let p = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in

        let callback _result =
          let State_may_now_be_pending_proxy p = may_now_be_proxy p in
          let p = underlying p in
          let result = collect_fulfilled_promises_after_pending [] ps in
          List.iter cancel ps;
          let State_may_have_changed p =
            resolve ~allow_deferring:false p result in
          ignore p
        in
        add_explicitly_removable_callback_to_each_of ps callback;

        to_public_promise p

      | p::ps' ->
        let Internal p = to_internal_promise p in
        match (underlying p).state with
        | Fulfilled v ->
          collect_already_fulfilled_promises_or_find_rejected [v] ps'

        | Rejected _ as result ->
          List.iter cancel ps;
          to_public_promise {state = result}

        | Pending _ ->
          check_for_already_resolved_promises ps'
    in

    let p = check_for_already_resolved_promises ps in
    p



  (* Same general pattern as [npick] and [nchoose]. *)
  let nchoose_split ps =
    if ps = [] then
      invalid_arg
        "Lwt.nchoose_split [] would return a promise that is pending forever";
    let rec finish
        (to_resolve : ('a list * 'a t list, underlying, pending) promise)
        (fulfilled : 'a list)
        (pending : 'a t list)
        (ps : 'a t list)
          : ('a list * 'a t list, underlying, resolved) state_changed =

      match ps with
      | [] ->
        resolve ~allow_deferring:false to_resolve
          (Fulfilled (List.rev fulfilled, List.rev pending))

      | p::ps ->
        let Internal p_internal = to_internal_promise p in
        match (underlying p_internal).state with
        | Fulfilled v ->
          finish to_resolve (v::fulfilled) pending ps

        | Rejected _ as result ->
          resolve ~allow_deferring:false to_resolve result

        | Pending _ ->
          finish to_resolve fulfilled (p::pending) ps
    in

    let rec collect_already_resolved_promises results pending ps =
      match ps with
      | [] ->
        (* Maintainer's note: should the pending promise list also be
           reversed? It is reversed in finish. *)
        return (List.rev results, pending)

      | p::ps ->
        let Internal p_internal = to_internal_promise p in
        match (underlying p_internal).state with
        | Fulfilled v ->
          collect_already_resolved_promises (v::results) pending ps

        | Rejected _ as result ->
          to_public_promise {state = result}

        | Pending _ ->
          collect_already_resolved_promises results (p::pending) ps
    in

    let rec check_for_already_resolved_promises pending_acc ps' =
      match ps' with
      | [] ->
        let p = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in

        let callback _result =
          let State_may_now_be_pending_proxy p = may_now_be_proxy p in
          let p = underlying p in
          let State_may_have_changed p = finish p [] [] ps in
          ignore p
        in
        add_explicitly_removable_callback_to_each_of ps callback;

        to_public_promise p

      | p::ps' ->
        let Internal p_internal = to_internal_promise p in
        match (underlying p_internal).state with
        | Fulfilled v ->
          collect_already_resolved_promises [v] pending_acc ps'

        | Rejected _ as result ->
          to_public_promise {state = result}

        | Pending _ ->
          check_for_already_resolved_promises (p::pending_acc) ps'
    in

    let p = check_for_already_resolved_promises [] ps in
    p
end
include Concurrent_composition



module Miscellaneous :
sig
  (* Promise state query *)
  type 'a state =
    | Return of 'a
    | Fail of exn
    | Sleep

  val state : 'a t -> 'a state
  val is_sleeping : 'a t -> bool
  val debug_state_is : 'a state -> 'a t -> bool t

  (* Function lifters *)
  val apply : ('a -> 'b t) -> 'a -> 'b t

  val wrap :
    (unit -> 'b) ->
    'b t
  val wrap1 :
    ('a1 -> 'b) ->
    ('a1 -> 'b t)
  val wrap2 :
    ('a1 -> 'a2 -> 'b) ->
    ('a1 -> 'a2 -> 'b t)
  val wrap3 :
    ('a1 -> 'a2 -> 'a3 -> 'b) ->
    ('a1 -> 'a2 -> 'a3 -> 'b t)
  val wrap4 :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b t)
  val wrap5 :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) ->
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b t)
  val wrap6 :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b) ->
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b t)
  val wrap7 :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b) ->
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b t)

  (* Paused promises *)
  val pause : unit -> unit t
  val wakeup_paused : unit -> unit
  val paused_count : unit -> int
  val register_pause_notifier : (int -> unit) -> unit

  (* Internal interface for other modules in Lwt *)
  val poll : 'a t -> 'a option
end =
struct
  type 'a state =
    | Return of 'a
    | Fail of exn
    | Sleep

  external reraise : exn -> 'a = "%reraise"

  let state p =
    let Internal p = to_internal_promise p in
    match (underlying p).state with
    | Fulfilled v -> Return v
    | Rejected exn -> Fail exn
    | Pending _ -> Sleep

  let debug_state_is expected_state p =
    return (state p = expected_state)

  let is_sleeping p =
    let Internal p = to_internal_promise p in
    match (underlying p).state with
    | Fulfilled _ -> false
    | Rejected _ -> false
    | Pending _ -> true

  let poll p =
    let Internal p = to_internal_promise p in
    match (underlying p).state with
    | Rejected e -> reraise e
    | Fulfilled v -> Some v
    | Pending _ -> None



  let apply f x = try f x with exn -> fail exn

  let wrap f = try return (f ()) with exn -> fail exn

  let wrap1 f x1 =
    try return (f x1)
    with exn -> fail exn

  let wrap2 f x1 x2 =
    try return (f x1 x2)
    with exn -> fail exn

  let wrap3 f x1 x2 x3 =
    try return (f x1 x2 x3)
    with exn -> fail exn

  let wrap4 f x1 x2 x3 x4 =
    try return (f x1 x2 x3 x4)
    with exn -> fail exn

  let wrap5 f x1 x2 x3 x4 x5 =
    try return (f x1 x2 x3 x4 x5)
    with exn -> fail exn

  let wrap6 f x1 x2 x3 x4 x5 x6 =
    try return (f x1 x2 x3 x4 x5 x6)
    with exn -> fail exn

  let wrap7 f x1 x2 x3 x4 x5 x6 x7 =
    try return (f x1 x2 x3 x4 x5 x6 x7)
    with exn -> fail exn



  let pause_hook = ref ignore

  let paused = Lwt_sequence.create ()
  let paused_count = ref 0

  let pause () =
    let p = add_task_r paused in
    incr paused_count;
    !pause_hook !paused_count;
    p

  let wakeup_paused () =
    if Lwt_sequence.is_empty paused then
      paused_count := 0
    else begin
      let tmp = Lwt_sequence.create () in
      Lwt_sequence.transfer_r paused tmp;
      paused_count := 0;
      Lwt_sequence.iter_l (fun r -> wakeup r ()) tmp
    end

  let register_pause_notifier f = pause_hook := f

  let paused_count () = !paused_count
end
include Miscellaneous



module Infix =
struct
  let (>>=) = bind
  let (=<<) f p = bind p f
  let (>|=) p f = map f p
  let (=|<) = map
  let (<&>) p p' = join [p; p']
  let (<?>) p p' = choose [p; p']

  module Let_syntax =
  struct
    let return = return
    let map t ~f = map f t
    let bind t ~f = bind t f
    let both = both

    module Open_on_rhs =
    struct
    end
  end
end
include Infix

module Lwt_result_type =
struct
  type +'a result = 'a lwt_result

  (* Deprecated. *)
  let make_value v = Result.Ok v
  let make_error exn = Result.Error exn
end
include Lwt_result_type
