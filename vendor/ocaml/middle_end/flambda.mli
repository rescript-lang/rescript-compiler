(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Intermediate language used for tree-based analysis and optimization. *)

(** Whether the callee in a function application is known at compile time. *)
type call_kind =
  | Indirect
  | Direct of Closure_id.t

(** Simple constants.  ("Structured constants" are rewritten to invocations
    of [Pmakeblock] so that they easily take part in optimizations.) *)
type const =
  | Int of int
  | Char of char
  (** [Char] is kept separate from [Int] to improve printing *)
  | Const_pointer of int
  (** [Const_pointer] is an immediate value of a type whose values may be
     boxed (typically a variant type with both constant and non-constant
     constructors). *)

(** The application of a function to a list of arguments. *)
type apply = {
  (* CR-soon mshinwell: rename func -> callee, and
     lhs_of_application -> callee *)
  func : Variable.t;
  args : Variable.t list;
  kind : call_kind;
  dbg : Debuginfo.t;
  inline : Lambda.inline_attribute;
  (** Instructions from the source code as to whether the callee should
      be inlined. *)
  specialise : Lambda.specialise_attribute;
  (** Instructions from the source code as to whether the callee should
      be specialised. *)
}

(** The update of a mutable variable.  Mutable variables are distinct from
    immutable variables in Flambda. *)
type assign = {
  being_assigned : Mutable_variable.t;
  new_value : Variable.t;
}

(** The invocation of a method. *)
type send = {
  kind : Lambda.meth_kind;
  meth : Variable.t;
  obj : Variable.t;
  args : Variable.t list;
  dbg : Debuginfo.t;
}

(** For details on these types, see projection.mli. *)
type project_closure = Projection.project_closure
type move_within_set_of_closures = Projection.move_within_set_of_closures
type project_var = Projection.project_var

(** See [free_vars] and [specialised_args], below. *)
(* CR-someday mshinwell: move to separate module and make [Identifiable].
  (Or maybe nearly Identifiable; having a special map that enforces invariants
  might be good.) *)
type specialised_to = {
  var : Variable.t;
  (** The "outer variable". *)
  projection : Projection.t option;
  (** The [projecting_from] value (see projection.mli) of any [projection]
      must be another free variable or specialised argument (depending on
      whether this record type is involved in [free_vars] or
      [specialised_args] respectively) in the same set of closures.
      As such, this field describes a relation of projections between
      either the [free_vars] or the [specialised_args]. *)
}

(** Flambda terms are partitioned in a pseudo-ANF manner; many terms are
    required to be [let]-bound.  This in particular ensures there is always
    a variable name for an expression that may be lifted out (for example
    if it is found to be constant).
    Note: All bound variables in Flambda terms must be distinct.
    [Flambda_invariants] verifies this. *)
type t =
  | Var of Variable.t
  | Let of let_expr
  | Let_mutable of let_mutable
  | Let_rec of (Variable.t * named) list * t
  (** CR-someday lwhite: give Let_rec the same fields as Let. *)
  | Apply of apply
  | Send of send
  | Assign of assign
  | If_then_else of Variable.t * t * t
  | Switch of Variable.t * switch
  | String_switch of Variable.t * (string * t) list * t option
  (** Restrictions on [Lambda.Lstringswitch] also apply to [String_switch]. *)
  | Static_raise of Static_exception.t * Variable.t list
  | Static_catch of Static_exception.t * Variable.t list * t * t
  | Try_with of t * Variable.t * t
  | While of t * t
  | For of for_loop
  | Proved_unreachable

(** Values of type [named] will always be [let]-bound to a [Variable.t]. *)
and named =
  | Symbol of Symbol.t
  | Const of const
  | Allocated_const of Allocated_const.t
  | Read_mutable of Mutable_variable.t
  | Read_symbol_field of Symbol.t * int
  (** During the lifting of [let] bindings to [program] constructions after
      closure conversion, we generate symbols and their corresponding
      definitions (which may or may not be constant), together with field
      accesses to such symbols.  We would like it to be the case that such
      field accesses are simplified to the relevant component of the
      symbol concerned.  (The rationale is to generate efficient code and
      share constants as expected: see e.g. tests/asmcomp/staticalloc.ml.)
      The components of the symbol would be identified by other symbols.
      This sort of access pattern is feasible because the top-level structure
      of symbols is statically allocated and fixed at compile time.
      It may seem that [Prim (Pfield, ...)] expressions could be used to
      perform the field accesses.  However for simplicity, to avoid having to
      keep track of properties of individual fields of blocks,
      [Inconstant_idents] never deems a [Prim (Pfield, ...)] expression to be
      constant.  This would in general prevent field accesses to symbols from
      being simplified in the way we would like, since [Lift_constants] would
      not assign new symbols (i.e. the things we would like to simplify to)
      to the various projections from the symbols in question.
      To circumvent this problem we use [Read_symbol_field] when generating
      projections from the top level of symbols.  Owing to the properties of
      symbols described above, such expressions may be eligible for declaration
      as constant by [Inconstant_idents] (and thus themselves lifted to another
      symbol), without any further complication.
      [Read_symbol_field] may only be used when the definition of the symbol
      is in scope in the [program].  For external unresolved symbols, [Pfield]
      may still be used; it will be changed to [Read_symbol_field] by
      [Inline_and_simplify] when (and if) the symbol is imported. *)
  | Set_of_closures of set_of_closures
  | Project_closure of project_closure
  | Move_within_set_of_closures of move_within_set_of_closures
  | Project_var of project_var
  | Prim of Lambda.primitive * Variable.t list * Debuginfo.t
  | Expr of t  (** ANF escape hatch. *)

(* CR-someday mshinwell: use [letcont]-style construct to remove e.g.
   [While] and [For]. *)
(* CR-someday mshinwell: try to produce a tighter definition of a "switch"
   (and translate to that earlier) so that middle- and back-end code for
   these can be reduced. *)
(* CR-someday mshinwell: remove [Expr], but to do this easily would probably
   require a continuation-binding construct. *)
(* CR-someday mshinwell: Since we lack expression identifiers on every term,
   we should probably introduce [Mutable_var] into [named] if we introduce
   more complicated analyses on these in the future.  Alternatively, maybe
   consider removing mutable variables altogether. *)

and let_expr = private {
  var : Variable.t;
  defining_expr : named;
  body : t;
  (* CR-someday mshinwell: we could consider having these be keys into some
     kind of global cache, to reduce memory usage. *)
  free_vars_of_defining_expr : Variable.Set.t;
  (** A cache of the free variables in the defining expression of the [let]. *)
  free_vars_of_body : Variable.Set.t;
  (** A cache of the free variables of the body of the [let].  This is an
      important optimization. *)
}

and let_mutable = {
  var : Mutable_variable.t;
  initial_value : Variable.t;
  contents_kind : Lambda.value_kind;
  body : t;
}

(** The representation of a set of function declarations (possibly mutually
    recursive).  Such a set encapsulates the declarations themselves,
    information about their defining environment, and information used
    specifically for optimization.
    Before a function can be applied it must be "projected" from a set of
    closures to yield a "closure".  This is done using [Project_closure]
    (see above).  Given a closure, not only can it be applied, but information
    about its defining environment can be retrieved (using [Project_var],
    see above).
    At runtime, a [set_of_closures] corresponds to an OCaml value with tag
    [Closure_tag] (possibly with inline [Infix_tag](s)).  As an optimization,
    an operation ([Move_within_set_of_closures]) is provided (see above)
    which enables one closure within a set to be located given another
    closure in the same set.  This avoids keeping a pointer to the whole set
    of closures alive when compiling, for example, mutually-recursive
    functions.
*)
and set_of_closures = private {
  function_decls : function_declarations;
  (* CR-soon mshinwell: consider renaming [free_vars].  Also, it's still really
     confusing which side of this map to use when.  "Vars bound by the
     closure" is the domain.
     Another example of when this is confusing:
      let bound_vars_approx =
        Variable.Map.map (Env.find_approx env) set.free_vars
      in
     in [Build_export_info]. *)
  (* CR-soon mshinwell: I'd like to arrange these maps so that it's impossible
     to put invalid projection information into them (in particular, so that
     we enforce that the relation stays within the domain of the map). *)
  free_vars : specialised_to Variable.Map.t;
  (** Mapping from all variables free in the body of the [function_decls] to
      variables in scope at the definition point of the [set_of_closures].
      The domain of this map is sometimes known as the "variables bound by
      the closure". *)
  specialised_args : specialised_to Variable.Map.t;
  (** Parameters whose corresponding arguments are known to always alias a
      particular value.  These are the only parameters that may, during
      [Inline_and_simplify], have non-unknown approximations.

      An argument may only be specialised to a variable in the scope of the
      corresponding set of closures declaration.  Usually, that variable
      itself also appears in the position of the specialised argument at
      all call sites of the function.  However it may also be the case (for
      example in code generated as a result of [Augment_specialised_args])
      that the various call sites of such a function have differing
      variables in the position of the specialised argument.  This is
      permissible *so long as it is certain they all alias the same value*.
      Great care must be taken in transformations that result in this
      situation since there are no invariant checks for correctness.

      As an example, supposing all call sites of f are represented here:
        [let x = ... in
         let f a b c = ... in
         let y = ... in
         f x y 1;
         f x y 1]
      the specialised arguments of f can (but does not necessarily) contain
      the association [a] -> [x], but cannot contain [b] -> [y] because [f]
      is not in the scope of [y]. If f were the recursive function
      [let rec f a b c = f a 1 2 in], [a] -> [x] would still be a valid
      specialised argument because all recursive calls maintain the invariant.

      This information is used for optimization purposes, if such a binding is
      known, it is possible to specialise the body of the function according
      to its parameter. This is usually introduced when specialising a
      recursive function, for instance.
        [let rec map f = function
           | [] -> []
           | h :: t -> f h :: map f t
         let map_succ l =
           let succ x = x + 1 in
           map succ l]
      [map] can be duplicated in [map_succ] to be specialised for the argument
      [f]. This will result in
        [let map_succ l =
           let succ x = x + 1 in
           let rec map f = function
             | [] -> []
             | h :: t -> f h :: map f t in
           map succ l]
      with map having [f] -> [succ] in its [specialised_args] field.

      Specialised argument information for arguments that are used must
      never be erased.  This ensures that specialised arguments whose
      approximations describe closures maintain those approximations, which
      is essential to transport the closure freshening information to the
      point of use (e.g. a [Project_var] from such an argument).
  *)
  direct_call_surrogates : Variable.t Variable.Map.t;
  (** If [direct_call_surrogates] maps [fun_var1] to [fun_var2] then direct
      calls to [fun_var1] should be redirected to [fun_var2].  This is used
      to reduce the overhead of transformations that introduce wrapper
      functions (which will be inlined at direct call sites, but will
      penalise indirect call sites).
      [direct_call_surrogates] may not be transitively closed. *)
}

and function_declarations = private {
  set_of_closures_id : Set_of_closures_id.t;
  (** An identifier (unique across all Flambda trees currently in memory)
      of the set of closures associated with this set of function
      declarations. *)
  set_of_closures_origin : Set_of_closures_origin.t;
  (** An identifier of the original set of closures on which this set of
      function declarations is based.  Used to prevent different
      specialisations of the same functions from being inlined/specialised
      within each other. *)
  funs : function_declaration Variable.Map.t;
  (** The function(s) defined by the set of function declarations.  The
      keys of this map are often referred to in the code as "fun_var"s. *)
}

and function_declaration = private {
  params : Parameter.t list;
  body : t;
  (* CR-soon mshinwell: inconsistent naming free_variables/free_vars here and
     above *)
  free_variables : Variable.Set.t;
  (** All variables free in the *body* of the function.  For example, a
      variable that is bound as one of the function's parameters will still
      be included in this set.  This field is present as an optimization. *)
  free_symbols : Symbol.Set.t;
  (** All symbols that occur in the function's body.  (Symbols can never be
      bound in a function's body; the only thing that binds symbols is the
      [program] constructions below.) *)
  stub : bool;
  (** A stub function is a generated function used to prepare arguments or
      return values to allow indirect calls to functions with a special calling
      convention.  For instance indirect calls to tuplified functions must go
      through a stub.  Stubs will be unconditionally inlined. *)
  dbg : Debuginfo.t;
  (** Debug info for the function declaration. *)
  inline : Lambda.inline_attribute;
  (** Inlining requirements from the source code. *)
  specialise : Lambda.specialise_attribute;
  (** Specialising requirements from the source code. *)
  is_a_functor : bool;
  (** Whether the function is known definitively to be a functor. *)
}

(** Equivalent to the similar type in [Lambda]. *)
and switch = {
  numconsts : Numbers.Int.Set.t; (** Integer cases *)
  consts : (int * t) list; (** Integer cases *)
  numblocks : Numbers.Int.Set.t; (** Number of tag block cases *)
  blocks : (int * t) list; (** Tag block cases *)
  failaction : t option; (** Action to take if none matched *)
}

(** Equivalent to the similar type in [Lambda]. *)
and for_loop = {
  bound_var : Variable.t;
  from_value : Variable.t;
  to_value : Variable.t;
  direction : Asttypes.direction_flag;
  body : t
}

(** Like a subset of [Flambda.named], except that instead of [Variable.t]s we
    have [Symbol.t]s, and everything is a constant (i.e. with a fixed value
    known at compile time).  Values of this type describe constants that will
    be directly assigned to symbols in the object file (see below). *)
and constant_defining_value =
  | Allocated_const of Allocated_const.t
    (** A single constant.  These are never "simple constants" (type [const])
        but instead more complicated constructions. *)
  | Block of Tag.t * constant_defining_value_block_field list
    (** A pre-allocated block full of constants (either simple constants
        or references to other constants, see below). *)
  | Set_of_closures of set_of_closures
    (** A closed (and thus constant) set of closures.  (That is to say,
        [free_vars] must be empty.) *)
  | Project_closure of Symbol.t * Closure_id.t
    (** Selection of one closure from a constant set of closures.
        Analogous to the equivalent operation on expressions. *)

and constant_defining_value_block_field =
  | Symbol of Symbol.t
  | Const of const

module Constant_defining_value :
  Identifiable.S with type t = constant_defining_value

type expr = t

(** A "program" is the contents of one compilation unit.  It describes the
    various values that are assigned to symbols (and in some cases fields of
    such symbols) in the object file.  As such, it is closely related to
    the compilation of toplevel modules. *)
type program_body =
  | Let_symbol of Symbol.t * constant_defining_value * program_body
  (** Define the given symbol to have the given constant value. *)
  | Let_rec_symbol of (Symbol.t * constant_defining_value) list * program_body
  (** As for [Let_symbol], but recursive.  This is needed to treat examples
      like this, where a constant set of closures is lifted to toplevel:

        let rec f x = f x

      After lifting this produces (in pseudo-Flambda):

        Let_rec_symbol set_of_closures_symbol =
          (Set_of_closures { f x ->
            let applied_function = Symbol f_closure in
            Apply (applied_function, x) })
        and f_closure = Project_closure (set_of_closures_symbol, f)

      Use of [Let_rec_symbol], by virtue of the special handling in
      [Inline_and_simplify.define_let_rec_symbol_approx], enables the
      approximation of the set of closures to be present in order to
      correctly simplify the [Project_closure] construction.  (See
      [Inline_and_simplify.simplify_project_closure] for that part.) *)
  | Initialize_symbol of Symbol.t * Tag.t * t list * program_body
  (** Define the given symbol as a constant block of the given size and
      tag; but with a possibly non-constant initializer.  The initializer
      will be executed at most once (from the entry point of the compilation
      unit). *)
  | Effect of t * program_body
  (** Cause the given expression, which may have a side effect, to be
      executed.  The resulting value is discarded.  [Effect] constructions
      are never re-ordered. *)
  | End of Symbol.t
  (** [End] accepts the root symbol: the only symbol that can never be
      eliminated. *)

type program = {
  imported_symbols : Symbol.Set.t;
  program_body : program_body;
}

(** Compute the free variables of a term.  (This is O(1) for [Let]s).
    If [ignore_uses_as_callee], all free variables inside [Apply] expressions
    are ignored.  Likewise [ignore_uses_in_project_var] for [Project_var]
    expressions.
*)
val free_variables
   : ?ignore_uses_as_callee:unit
  -> ?ignore_uses_as_argument:unit
  -> ?ignore_uses_in_project_var:unit
  -> t
  -> Variable.Set.t

(** Compute the free variables of a named expression. *)
val free_variables_named
   : ?ignore_uses_in_project_var:unit
  -> named
  -> Variable.Set.t

(** Compute _all_ variables occurring inside an expression. *)
val used_variables
   : ?ignore_uses_as_callee:unit
  -> ?ignore_uses_as_argument:unit
  -> ?ignore_uses_in_project_var:unit
  -> t
  -> Variable.Set.t

(** Compute _all_ variables occurring inside a named expression. *)
val used_variables_named
   : ?ignore_uses_in_project_var:unit
  -> named
  -> Variable.Set.t

val free_symbols : expr -> Symbol.Set.t

val free_symbols_named : named -> Symbol.Set.t

val free_symbols_program : program -> Symbol.Set.t

(** Used to avoid exceeding the stack limit when handling expressions with
    multiple consecutive nested [Let]-expressions.  This saves rewriting large
    simplification functions in CPS.  This function provides for the
    rewriting or elimination of expressions during the fold. *)
val fold_lets_option
   : t
  -> init:'a
  -> for_defining_expr:('a -> Variable.t -> named -> 'a * Variable.t * named)
  -> for_last_body:('a -> t -> t * 'b)
  (* CR-someday mshinwell: consider making [filter_defining_expr]
     optional *)
  -> filter_defining_expr:('b -> Variable.t -> named -> Variable.Set.t ->
                           'b * Variable.t * named option)
  -> t * 'b

(** Like [fold_lets_option], but just a map. *)
val map_lets
   : t
  -> for_defining_expr:(Variable.t -> named -> named)
  -> for_last_body:(t -> t)
  -> after_rebuild:(t -> t)
  -> t

(** Like [map_lets], but just an iterator. *)
val iter_lets
   : t
  -> for_defining_expr:(Variable.t -> named -> unit)
  -> for_last_body:(t -> unit)
  -> for_each_let:(t -> unit)
  -> unit

(** Creates a [Let] expression.  (This computes the free variables of the
    defining expression and the body.) *)
val create_let : Variable.t -> named -> t -> t

(** Apply the specified function [f] to the defining expression of the given
    [Let]-expression, returning a new [Let]. *)
val map_defining_expr_of_let : let_expr -> f:(named -> named) -> t

(** A module for the manipulation of terms where the recomputation of free
    variable sets is to be kept to a minimum. *)
module With_free_variables : sig
  type 'a t

  (** O(1) time. *)
  val of_defining_expr_of_let : let_expr -> named t

  (** O(1) time. *)
  val of_body_of_let : let_expr -> expr t

  (** Takes the time required to calculate the free variables of the given
      term (proportional to the size of the term, except that the calculation
      for [Let] is O(1)). *)
  val of_expr : expr -> expr t

  val of_named : named -> named t

  (** Takes the time required to calculate the free variables of the given
      [expr]. *)
  val create_let_reusing_defining_expr
     : Variable.t
    -> named t
    -> expr
    -> expr

  (** Takes the time required to calculate the free variables of the given
      [named]. *)
  val create_let_reusing_body
     : Variable.t
    -> named
    -> expr t
    -> expr

  (** O(1) time. *)
  val create_let_reusing_both
     : Variable.t
    -> named t
    -> expr t
    -> expr

  (** The equivalent of the [Expr] constructor. *)
  val expr : expr t -> named t

  val contents : 'a t -> 'a

  (** O(1) time. *)
  val free_variables : _ t -> Variable.Set.t
end

(** Create a function declaration.  This calculates the free variables and
    symbols occurring in the specified [body]. *)
val create_function_declaration
   : params:Parameter.t list
  -> body:t
  -> stub:bool
  -> dbg:Debuginfo.t
  -> inline:Lambda.inline_attribute
  -> specialise:Lambda.specialise_attribute
  -> is_a_functor:bool
  -> function_declaration

(** Create a set of function declarations given the individual declarations. *)
val create_function_declarations
   : funs:function_declaration Variable.Map.t
  -> function_declarations

(** Create a set of function declarations based on another set of function
    declarations. *)
val update_function_declarations
   : function_declarations
  -> funs:function_declaration Variable.Map.t
  -> function_declarations

val import_function_declarations_for_pack
   : function_declarations
  -> (Set_of_closures_id.t -> Set_of_closures_id.t)
  -> (Set_of_closures_origin.t -> Set_of_closures_origin.t)
  -> function_declarations

(** Create a set of closures.  Checks are made to ensure that [free_vars]
    and [specialised_args] are reasonable. *)
val create_set_of_closures
   : function_decls:function_declarations
  -> free_vars:specialised_to Variable.Map.t
  -> specialised_args:specialised_to Variable.Map.t
  -> direct_call_surrogates:Variable.t Variable.Map.t
  -> set_of_closures

(** Given a function declaration, find which of its parameters (if any)
    are used in the body. *)
val used_params : function_declaration -> Variable.Set.t

type maybe_named =
  | Is_expr of t
  | Is_named of named

(** This function is designed for the internal use of [Flambda_iterators].
    See that module for iterators to be used over Flambda terms. *)
val iter_general
   : toplevel:bool
  -> (t -> unit)
  -> (named -> unit)
  -> maybe_named
  -> unit

val print : Format.formatter -> t -> unit

val print_named : Format.formatter -> named -> unit

val print_program : Format.formatter -> program -> unit

val print_const : Format.formatter -> const -> unit

val print_constant_defining_value
   : Format.formatter
  -> constant_defining_value
  -> unit

val print_function_declaration
   : Format.formatter
  -> Variable.t * function_declaration
  -> unit

val print_function_declarations
   : Format.formatter
  -> function_declarations
  -> unit

val print_project_closure
   : Format.formatter
  -> project_closure
  -> unit

val print_move_within_set_of_closures
   : Format.formatter
  -> move_within_set_of_closures
  -> unit

val print_project_var
   : Format.formatter
  -> project_var
  -> unit

val print_set_of_closures
   : Format.formatter
  -> set_of_closures
  -> unit

val print_specialised_to
   : Format.formatter
  -> specialised_to
  -> unit

val equal_specialised_to
   : specialised_to
  -> specialised_to
  -> bool

val compare_project_var : project_var -> project_var -> int

val compare_move_within_set_of_closures
   : move_within_set_of_closures
  -> move_within_set_of_closures
  -> int

val compare_project_closure : project_closure -> project_closure -> int
