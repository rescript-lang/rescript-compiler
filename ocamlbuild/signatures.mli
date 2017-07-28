(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
(** This module contains all module signatures that the user
    could use to build an ocamlbuild plugin. *)

module type OrderedTypePrintable = sig
  type t
  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
end

module type SET = sig
  include Set.S
  val find_elt : (elt -> bool) -> t -> elt
  val map : (elt -> elt) -> t -> t
  val of_list : elt list -> t
  val print : Format.formatter -> t -> unit
end

module type LIST = sig
  (* Added functions *)
  val print : (Format.formatter -> 'a -> 'b) -> Format.formatter -> 'a list -> unit
  val filter_opt : ('a -> 'b option) -> 'a list -> 'b list
  val union : 'a list -> 'a list -> 'a list
  val ordered_unique : 'a list -> 'a list
  (* Original functions *)
  include module type of List
end

module type STRING = sig
  val print : Format.formatter -> string -> unit
  val chomp : string -> string

  (** [before s n] returns the substring of all characters of [s]
     that precede position [n] (excluding the character at
     position [n]).
     This is the same function as {!Str.string_before}. *)
  val before : string -> int -> string

  (** [after s n] returns the substring of all characters of [s]
     that follow position [n] (including the character at
     position [n]).
     This is the same function as {!Str.string_after}. *)
  val after : string -> int -> string

  val first_chars : string -> int -> string
  (** [first_chars s n] returns the first [n] characters of [s].
     This is the same function as {!before} ant {!Str.first_chars}. *)

  val last_chars : string -> int -> string
  (** [last_chars s n] returns the last [n] characters of [s].
      This is the same function as {!Str.last_chars}. *)

  val eq_sub_strings : string -> int -> string -> int -> int -> bool

  (** [is_prefix u v] is u a prefix of v ? *)
  val is_prefix : string -> string -> bool
  (** [is_suffix u v] : is v a suffix of u ? *)
  val is_suffix : string -> string -> bool

  (** [contains_string s1 p2 s2] Search in [s1] starting from [p1] if it
      contains the [s2] string. Returns [Some position] where [position]
      is the begining of the string [s2] in [s1], [None] otherwise. *)
  val contains_string : string -> int -> string -> int option

  (** [subst patt repl text] *)
  val subst : string -> string -> string -> string

  (** [tr patt repl text] *)
  val tr : char -> char -> string -> string

  val rev : string -> string

  (* Convert a character list into a character string *)
  val implode : char list -> string

  (* Convert a character string into a character list *)
  val explode : string -> char list

  (** The following are original functions from the [String] module. *)
  include module type of String
end

module type TAGS = sig
  include Set.S with type elt = string
  (** [Tags.elt] represents a tag, which is simply a string, usually
      lowercase, for example "ocaml" or "native".  The set of tags
      attached to a file is computed by applying the tagging rules to
      the filename.  Tagging rules are defined in _tags files in any
      parent directory of a file, up to the main project directory. *)

  val of_list : string list -> t
  val print : Format.formatter -> t -> unit
  val does_match : t -> t -> bool
  module Operators : sig
    val ( ++ ) : t -> elt -> t
    val ( -- ) : t -> elt -> t
    val ( +++ ) : t -> elt option -> t
    val ( --- ) : t -> elt option -> t
  end
end

module type PATHNAME = sig
  type t = string
  val concat : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val exists : t -> bool
  val mk : string -> t
  val define_context : string -> string list -> unit
  val include_dirs_of : string -> string list
  val copy : t -> t -> unit
  val to_string : t -> string
  val print : Format.formatter -> t -> unit
  val current_dir_name : t
  val parent_dir_name : t
  val read : t -> string
  val same_contents : t -> t -> bool
  val basename : t -> t
  val dirname : t -> t
  val is_relative : t -> bool
  val readlink : t -> t
  val readdir : t -> t array
  val is_link : t -> bool
  val is_directory : t -> bool

  val add_extension : string -> t -> t
  val check_extension : t -> string -> bool

  val get_extension : t -> string
  val remove_extension : t -> t
  val update_extension : string -> t -> t

  val get_extensions : t -> string
  val remove_extensions : t -> t
  val update_extensions : string -> t -> t

  val print_path_list : Format.formatter -> t list -> unit
  val pwd : t
  val parent : t -> t
  (** [is_prefix x y] is [x] a pathname prefix of [y] *)
  val is_prefix : t -> t -> bool
  val is_implicit : t -> bool
  module Operators : sig
    val ( / ) : t -> t -> t
    val ( -.- ) : t -> string -> t
  end
end

(** Provides an abstract type for easily building complex shell
    commands without making quotation mistakes.  *)
module type COMMAND = sig
  type tags
  type pathname

  (** The type [t] provides some basic combinators and command primitives.
      Other commands can be made of command specifications ([spec]). *)
  type t =
    | Seq of t list (** A sequence of commands (like the `;' in shell) *)
    | Cmd of spec   (** A command is made of command specifications ([spec]) *)
    | Echo of string list * pathname
    (** Write the given strings (w/ any formatting) to the given file *)
    | Nop           (** The command that does nothing *)

  (** The type for command specifications. That is pieces of command. *)
  and spec =
    | N              (** No operation. *)
    | S of spec list (** A sequence.  This gets flattened in the last stages *)
    | A of string    (** An atom. *)
    | P of pathname  (** A pathname. *)
    | Px of pathname (** A pathname, that will also be given to the
                         call_with_target hook. *)
    | Sh of string   (** A bit of raw shell code, that will not be escaped. *)
    | T of tags      (** A set of tags, that describe properties and
                         some semantics information about the
                         command, afterward these tags will be
                         replaced by command [spec]s (flags for
                         instance). *)
    | V of string    (** A virtual command, that will be resolved at
                         execution using [resolve_virtuals] *)
    | Quote of spec  (** A string that should be quoted like a
                           filename but isn't really one. *)

  (*type v = [ `Seq of v list | `Cmd of vspec | `Nop ]
    and vspec =
    [ `N
    | `S of vspec list
    | `A of string
    | `P of pathname
    | `Px of pathname
    | `Sh of string
    | `Quote of vspec ]

    val spec_of_vspec : vspec -> spec
    val vspec_of_spec : spec -> vspec
    val t_of_v : v -> t
    val v_of_t : t -> v*)

  (** Will convert a string list to a list of atoms by adding [A] constructors. *)
  val atomize : string list -> spec

  (** Will convert a string list to a list of paths by adding [P] constructors. *)
  val atomize_paths : string list -> spec

  (** Run the command. *)
  val execute : ?quiet:bool -> ?pretend:bool -> t -> unit

  (** Run the commands in the given list, if possible in parallel.
      See the module [Ocamlbuild_executor]. *)
  val execute_many : ?quiet:bool -> ?pretend:bool -> t list -> (bool list * exn) option

  (** [setup_virtual_command_solver virtual_command solver]
        the given solver can raise Not_found if it fails to find a valid
        command for this virtual command. *)
  val setup_virtual_command_solver : string -> (unit -> spec) -> unit

  (** Search the given command in the command path and return its absolute
      pathname. *)
  val search_in_path : string -> string

  (** Simplify a command by flattening the sequences and resolving the tags
      into command-line options. *)
  val reduce : spec -> spec

  (** Print a command (the format is not suitable to running the command). *)
  val print : Format.formatter -> t -> unit

  (** Convert a command to a string (same format as print). *)
  val to_string : t -> string

  (** Build a string representation of a command that can be passed to the
      system calls. *)
  val string_of_command_spec : spec -> string
end

(** A self-contained module implementing extended shell glob patterns who have an expressive power
    equal to boolean combinations of regular expressions.  *)
module type GLOB = sig

  (** A globber is a boolean combination of basic expressions indented to work on
      pathnames.  Known operators
      are [or], [and] and [not], which may also be written [|], [&] and [~].  There are
      also constants [true] and [false] (or [1] and [0]).  Expression can be grouped
      using parentheses.
      - [true] matches anything,
      - [false] matches nothing,
      - {i basic} [or] {i basic} matches strings matching either one of the basic expressions,
      - {i basic} [and] {i basic} matches strings matching both basic expressions,
      - not {i basic} matches string that don't match the basic expression,
      - {i basic} matches strings that match the basic expression.

      A basic expression can be a constant string enclosed in double quotes, in which
      double quotes must be preceded by backslashes, or a glob pattern enclosed between a [<] and a [>],
      - ["]{i string}["] matches the literal string {i string},
      - [<]{i glob}[>] matches the glob pattern {i glob}.

      A glob pattern is an anchored regular expression in a shell-like syntax.  Most characters stand for themselves.
      Character ranges are given in usual shell syntax between brackets.  The star [*] stands for any sequence of
      characters.  The joker '?' stands for exactly one, unspecified character.  Alternation is achieved using braces [{].
      - {i glob1}{i glob2} matches strings who have a prefix matching {i glob1} and the corresponding suffix
        matching {i glob2}.
      - [a] matches the string consisting of the single letter [a].
      - [{]{i glob1},{i glob2}[}] matches strings matching {i glob1} or {i glob2}.
      - [?] any one-letter string, excluding the slash.
      - [*] matches all strings not containing a slash, including the empty one.
      - [**/] the empty string, or any string ending with a slash.
      - [/**] any string starting with a slash, or the empty string.
      - [/**/] any string starting and ending with a slash.
      - [\[]{i c1}-{i c2}{i c3}-{i c4}...[\]] matches characters in the range {i c1} to {i c2} inclusive,
        or in the range {i c3} to {i c4} inclusive.  For instance [\[a-fA-F0-9\]] matches hexadecimal digits.
        To match the dash, put it at the end.
  *)

  (** The type representing globbers.  Do not attempt to compare them, as they get on-the-fly optimizations. *)
  type globber

  (** [parse ~dir pattern] will parse the globber pattern [pattern], optionally prefixing its patterns with [dir]. *)
  val parse : ?dir:string -> string -> globber

  (** A descriptive exception raised when an invalid glob pattern description is given. *)
  exception Parse_error of string

  (** [eval g u] returns [true] if and only if the string [u] matches the given glob expression.  Avoid reparsing
      the same pattern, since the automaton implementing the pattern is optimized on the fly.  The first few evaluations
      are done using a time-inefficient but memory-efficient algorithm.  It then compiles the pattern into an efficient
      but more memory-hungry data structure. *)
  val eval : globber -> string -> bool
end

(** Module for modulating the logging output with the logging level. *)
module type LOG = sig
  (** Current logging (debugging) level. *)
  val level : int ref

  (** [dprintf level fmt args...] formats the logging information [fmt]
      with the arguments [args...] on the logging output if the logging
      level is greater than or equal to [level]. The default level is 1.
      More obscure debugging information should have a higher logging
      level. Youre formats are wrapped inside these two formats
      ["@\[<2>"] and ["@\]@."]. *)
  val dprintf : int -> ('a, Format.formatter, unit) format -> 'a

  (** Equivalent to calling [dprintf] with a level [< 0]. *)
  val eprintf : ('a, Format.formatter, unit) format -> 'a

  (** Same as dprintf but without the format wrapping. *)
  val raw_dprintf : int -> ('a, Format.formatter, unit) format -> 'a
end

module type OUTCOME = sig
  type ('a,'b) t =
    | Good of 'a
    | Bad of 'b

  val wrap : ('a -> 'b) -> 'a -> ('b, exn) t
  val ignore_good : ('a, exn) t -> unit
  val good : ('a, exn) t -> 'a
end

module type MISC = sig
  val opt_print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
  val the : 'a option -> 'a
  val getenv : ?default:string -> string -> string
  val with_input_file : ?bin:bool -> string -> (in_channel -> 'a) -> 'a
  val with_output_file : ?bin:bool -> string -> (out_channel -> 'a) -> 'a
  val with_temp_file : string -> string -> (string -> 'a) -> 'a
  val read_file : string -> string
  val copy_chan : in_channel -> out_channel -> unit
  val copy_file : string -> string -> unit
  val print_string_list : Format.formatter -> string list -> unit

  (** A shortcut to force lazy value (See {Lazy.force}). *)
  val ( !* ) : 'a Lazy.t -> 'a

  (** The right associative application.
      Useful when writing to much parentheses:
      << f (g x ... t) >> becomes << f& g x ... t >>
      << f (g (h x)) >>   becomes << f& g& h x >> *)
  val ( & ) : ('a -> 'b) -> 'a -> 'b

  (** The reversed application combinator.
      Useful to describe some operations chaining.
      << f x (g y (h z)) >> becomes << z |> h |> g y |> f x >> *)
  val ( |> ) : 'a -> ('a -> 'b) -> 'b

  (** [r @:= l] is equivalent to [r := !r @ l] *)
  val ( @:= ) : 'a list ref -> 'a list -> unit

  val memo : ('a -> 'b) -> ('a -> 'b)

  val memo2 : ('a -> 'b -> 'c) -> ('a -> 'b -> 'c)

  val memo3 : ('a -> 'b -> 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
end

module type OPTIONS = sig
  type command_spec

  val build_dir : string ref
  val include_dirs : string list ref
  val exclude_dirs : string list ref
  val nothing_should_be_rebuilt : bool ref
  val ocamlc : command_spec ref
  val ocamlopt : command_spec ref
  val ocamldep : command_spec ref
  val ocamldoc : command_spec ref
  val ocamlyacc : command_spec ref
  val ocamllex : command_spec ref
  val ocamlrun : command_spec ref
  val ocamlmklib : command_spec ref
  val ocamlmktop : command_spec ref
  val hygiene : bool ref
  val sanitize : bool ref
  val sanitization_script : string ref
  val ignore_auto : bool ref
  val plugin : bool ref
  val just_plugin : bool ref
  val native_plugin : bool ref
  val make_links : bool ref
  val nostdlib : bool ref
  val program_to_execute : bool ref
  val must_clean : bool ref
  val catch_errors : bool ref
  val use_menhir : bool ref
  val show_documentation : bool ref
  val recursive : bool ref
  val use_ocamlfind : bool ref

  val targets : string list ref
  val ocaml_libs : string list ref
  val ocaml_mods : string list ref
  val ocaml_pkgs : string list ref
  val ocaml_syntax : string option ref
  val ocaml_cflags : string list ref
  val ocaml_lflags : string list ref
  val ocaml_ppflags : string list ref
  val ocaml_docflags : string list ref
  val ocaml_yaccflags : string list ref
  val ocaml_lexflags : string list ref
  val program_args : string list ref
  val ignore_list : string list ref
  val tags : string list ref
  val tag_lines : string list ref
  val show_tags : string list ref

  val ext_obj : string ref
  val ext_lib : string ref
  val ext_dll : string ref
  val exe : string ref

  val add : string * Arg.spec * string -> unit
end

module type ARCH = sig
  type 'a arch = private
    | Arch_dir of string * 'a * 'a arch list
    | Arch_dir_pack of string * 'a * 'a arch list
    | Arch_file of string * 'a

  val dir : string -> unit arch list -> unit arch
  val dir_pack : string -> unit arch list -> unit arch
  val file : string -> unit arch

  type info = private {
    current_path : string;
    include_dirs : string list;
    for_pack : string;
  }

  val annotate : 'a arch -> info arch

  val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a arch -> unit
  val print_include_dirs : Format.formatter -> string list -> unit
  val print_info : Format.formatter -> info -> unit

  val iter_info : ('a -> unit) -> 'a arch -> unit
  val fold_info : ('a -> 'b -> 'b) -> 'a arch -> 'b -> 'b

  val iter_include_dirs : info arch -> (string -> unit) -> unit

  val mk_tables :
    info arch -> (string, string list) Hashtbl.t * (string, string) Hashtbl.t
  val print_table :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> (string, 'a) Hashtbl.t -> unit
end

module type FINDLIB = sig
  (** Findlib / Ocamlfind tools. *)

  type command_spec

  type error =
    | Cannot_run_ocamlfind
    | Dependency_not_found of string * string (* package, dependency *)
    | Package_not_found of string
    | Cannot_parse_query of string * string (* package, explaination *)

  exception Findlib_error of error

  val string_of_error: error -> string
    (** Return a string message describing an error. *)

  val report_error: error -> 'a
    (** Report an error on the standard error and exit with code 2. *)

  type package = {
    name: string;
    description: string;
    version: string;
    archives_byte: string;
      (** Archive names, with the .cma extension, but without the directory. *)
    archives_native: string;
      (** Archive names, with the .cmxa extension, but without the directory. *)
    link_options: string;
    location: string;
    dependencies: package list;
      (** Transitive closure, as returned by [ocamlfind query -r]. *)
  }
    (** Package information. *)

  val query: string -> package
    (** Query information about a package, given its name. May raise
[Not_found]. *)

  val list: unit -> string list
    (** Return the names of all known packages. *)

  val topological_closure: package list -> package list
    (** Computes the transitive closure of a list of
packages and sort them in topological order.
Given any list of package [l], [topological_closure l] returns a list of
packages including [l] and their dependencies, in an order where any element
may only depend on the previous ones. *)

  val compile_flags_byte: package list -> command_spec
    (** Return the flags to add when compiling in byte mode (include
directories). *)

  val compile_flags_native: package list -> command_spec
    (** Same as [link_flags_byte] but for native mode. *)

  val link_flags_byte: package list -> command_spec
    (** Return the flags to add when linking in byte mode. It includes:
include directories, libraries and special link options. *)

  val link_flags_native: package list -> command_spec
    (** Same as [link_flags_byte] but for native mode. *)
end

(** This module contains the functions and values that can be used by
    plugins. *)
module type PLUGIN = sig
  module Pathname  : PATHNAME
  module Tags      : TAGS
  module Command   : COMMAND with type tags = Tags.t and type pathname = Pathname.t
  module Outcome   : OUTCOME
  module String    : STRING
  module List      : LIST
  module StringSet : Set.S with type elt = String.t
  module Options   : OPTIONS with type command_spec = Command.spec
  module Arch      : ARCH
  module Findlib   : FINDLIB with type command_spec = Command.spec
  include MISC

  (** See {!COMMAND.t} for the description of this type. *)
  type command = Command.t =
    | Seq of command list
    | Cmd of spec
    | Echo of string list * Pathname.t
    | Nop

  (** See {!COMMAND.spec} for the description of this type. *)
  and spec = Command.spec =
    | N | S of spec list | A of string | P of string | Px of string
    | Sh of string | T of Tags.t | V of string | Quote of spec

  (** [path1/path2] Join the given path names. *)
  val ( / ) : Pathname.t -> Pathname.t -> Pathname.t

  (** [path-.-extension] Add the given extension to the given pathname. *)
  val ( -.- ) : Pathname.t -> string -> Pathname.t

  (** [tags++tag] Add the given tag to the given set of tags. *)
  val ( ++ ) : Tags.t -> Tags.elt -> Tags.t

  (** [tags--tag] Remove the given tag to the given set of tags. *)
  val ( -- ) : Tags.t -> Tags.elt -> Tags.t

  (** [tags+++optional_tag] Add the given optional tag to the given set of tags
      if the given option is Some. *)
  val ( +++ ) : Tags.t -> Tags.elt option -> Tags.t

  (** [tags---optional_tag] Remove the given optional tag to the given
      set of tags if the given option is Some. *)
  val ( --- ) : Tags.t -> Tags.elt option -> Tags.t

  (** The type of the builder environments. Here an environment is just the
      lookup function of it. Basically this function will resolve path variables
      like % or more generally %(var_name). *)
  type env = Pathname.t -> Pathname.t

  (** A builder is a function that waits for conjonction of alternative targets.
      The alternatives are here to support some choices, for instance for an
      OCaml module an alternatives can be foo.cmo, foo.cmi, Foo.cmo, Foo.cmi.
      Conjonctions are here to help making parallelism, indeed commands that are
      independant will be run concurently. *)
  type builder = Pathname.t list list -> (Pathname.t, exn) Outcome.t list

  (** This is the type for rule actions. An action receive as argument, the
      environment lookup function (see {!env}), and a function to dynamically
      build more targets (see {!builder}). An action should return the command
      to run in order to build the rule productions using the rule dependencies.
  *)
  type action = env -> builder -> Command.t

  (** This is the main function for adding a rule to the ocamlbuild engine.
      - The first argument is the name of the rule (should be unique).
      - It takes files that the rule produces.
        Use ~prod for one file, ~prods for list of files.
      - It also takes files that the rule uses.
        Use ~dep for one file, ~deps for list of files.
      - It finally takes the action to perform in order to produce the
        productions files using the dependencies (see [action]).

      There are some more optional parameters:
      - The ~insert argument allow to insert the rules precisely between other
        rules.
      - The ~stamp argument specify the name of a file that will be
        automatically produced by ocamlbuild. This file can serve as a virtual
        target (or phony target), since it will be filled up by a digest of
        it dependencies.
      - The ~tags argument in deprecated, don't use it.
      
      Finally, the optional ~doc argument allows to give an informal
      explanation of the rule purpose and behavior, that will be
      displayed by [ocamlbuild -documentation]. For example, it is
      a good place to specify the commands that will be called, any
      new tags introduced by the rule, and dynamic dependencies.
  *)
  val rule : string ->
    ?tags:string list ->
    ?prods:string list ->
    ?deps:string list ->
    ?prod:string ->
    ?dep:string ->
    ?stamp:string ->
    ?insert:[`top | `before of string | `after of string | `bottom] ->
    ?doc:string ->
    action -> unit

  (** [copy_rule name ?insert source destination] *)
  val copy_rule : string ->
    ?insert:[`top | `before of string | `after of string | `bottom] ->
    string -> string -> unit

  (** Empties the list of rules of the ocamlbuild engine. *)
  val clear_rules : unit -> unit

  (** [dep tags deps] Will build [deps] when all [tags] will be activated.
      If you do not know which tags to use, have a look to the file
      _build/_log after trying to compile your code. *)
  val dep : Tags.elt list -> Pathname.t list -> unit

  (** [pdep tags ptag deps] is equivalent to [dep tags deps], with an
      additional parameterized tag [ptag]. [deps] is now a function
      which takes the parameter of the tag [ptag] as an argument.

      Example:
        [pdep ["ocaml"; "compile"] "autodep" (fun param -> param)]
      says that the tag [autodep(file)] can now be used to automatically
      add [file] as a dependency when compiling an OCaml program. *)
  val pdep : Tags.elt list -> Tags.elt -> (string -> Pathname.t list) -> unit

  (** [flag tags command_spec] Will inject the given piece of command
      ([command_spec]) when all [tags] will be activated.
      If you do not know which tags to use, have a look to the file
      _build/_log after trying to compile your code. *)
  val flag : Tags.elt list -> Command.spec -> unit

  (** Allows to use [flag] with a parametrized tag (as [pdep] for [dep]).

      Example:
        [pflag ["ocaml"; "compile"] "inline"
           (fun count -> S [A "-inline"; A count])]
      says that command line option ["-inline 42"] should be added
      when compiling OCaml modules tagged with ["inline(42)"]. *)
  val pflag : Tags.elt list -> Tags.elt -> (string -> Command.spec) -> unit

  (** [flag_and_dep tags command_spec]
      Combines [flag] and [dep] function.
      Basically it calls [flag tags command_spec], and calls [dep tags files]
      where [files] is the list of all pathnames in [command_spec].
      Pathnames selected are those in the constructor [P] or [Px], or the
      pathname argument of builtins like [Echo]. *)
  val flag_and_dep : Tags.elt list -> Command.spec -> unit

  (** Allows to use [flag_and_dep] with a parameterized tag
      (as [pdep] for [dep]). *)
  val pflag_and_dep : Tags.elt list -> Tags.elt ->
    (string -> Command.spec) -> unit

  (** manually mark the tag as "useful" to silence the warning about
      tags that are not part of any flag declaration.

      This is useful,
      for example, if the tag is used in a flag declaration that is
      only perfored in a conditional branch:
      [if we_are_on_Windows then flag ["libfoo"] (A "bar");]

      When [we_are_on_Windows] is not true, you could get a warning about
      "libfoo" not used in any flag declaration.
     *)
  val mark_tag_used : Tags.elt -> unit

  (** [non_dependency module_path module_name]
      Example:
         [non_dependency "foo/bar/baz" "Goo"]
      Says that the module [Baz] in the file [foo/bar/baz.*] does
      not depend on [Goo]. *)
  val non_dependency : Pathname.t -> string -> unit

  (** [use_lib module_path lib_path]*)
  val use_lib : Pathname.t -> Pathname.t -> unit

  (** [ocaml_lib <options> library_pathname] Declare an ocaml library.
      This informs ocamlbuild and produce tags to use the library;
      they are named by default use_#{library_name}.

      Example: [ocaml_lib "foo/bar"] will setup the tag use_bar.
        At link time it will include:
          foo/bar.cma or foo/bar.cmxa
      @param dir supply the [~dir:"boo"] option to add '-I boo'
             at link and compile time.
      @param extern use ~extern:true for non-ocamlbuild handled libraries.
             Set this to add libraries whose sources are not in your project.
      @param byte use ~byte:false to disable byte mode.
      @param native use ~native:false to disable native mode.
      @param tag_name Use ~tag_name:"usebar" to override the default
             tag name. *)
  val ocaml_lib :
    ?extern:bool ->
    ?byte:bool ->
    ?native:bool ->
    ?dir:Pathname.t ->
    ?tag_name:string ->
    Pathname.t -> unit

  (** [expand_module include_dirs module_name extensions]
      Example:
      [expand_module ["a";"b";"c"] "Foo" ["cmo";"cmi"] =
      ["a/foo.cmo"; "a/Foo.cmo"; "a/foo.cmi"; "a/Foo.cmi";
      "b/foo.cmo"; "b/Foo.cmo"; "b/foo.cmi"; "b/Foo.cmi";
      "c/foo.cmo"; "c/Foo.cmo"; "c/foo.cmi"; "c/Foo.cmi"]] *)
  val expand_module :
    Pathname.t list -> Pathname.t -> string list -> Pathname.t list

  (** Reads the given file, parse it has list of words separated by blanks.
      It ignore lines that begins with a '#' character. *)
  val string_list_of_file : Pathname.t -> string list

  (** Takes a pathname and returns an OCaml module name. Basically it will
      remove directories and extensions, and then capitalize the string. *)
  val module_name_of_pathname : Pathname.t -> string

  (** The Unix mv command. *)
  val mv : Pathname.t -> Pathname.t -> Command.t

  (** The Unix cp command. *)
  val cp : Pathname.t -> Pathname.t -> Command.t

  (** The Unix ln -f command. *)
  val ln_f : Pathname.t -> Pathname.t -> Command.t

  (** The Unix ln -s command. *)
  val ln_s : Pathname.t -> Pathname.t -> Command.t

  (** The Unix rm -f command. *)
  val rm_f : Pathname.t -> Command.t

  (** The Unix chmod command (almost deprecated). *)
  val chmod : Command.spec -> Pathname.t -> Command.t

  (** The Unix cmp command (almost deprecated). *)
  val cmp : Pathname.t -> Pathname.t -> Command.t

  (** [hide_package_contents pack_name]
      Don't treat the given package as an open package.
      So a module will not be replaced during linking by
      this package even if it contains that module. *)
  val hide_package_contents : string -> unit

  (** [tag_file filename tag_list] Tag the given filename with all
      given tags.  Prefix a tag with the minus sign to remove it.
      This is usually used as an [After_rules] hook.
      For example [tag_file "bla.ml" ["use_unix"]] tags the file
      "bla.ml" with "use_unix" and [tag_file "bla.ml" ["-use_unix"]]
      removes the tag "use_unix" from the file "bla.ml". *)
  val tag_file : Pathname.t -> Tags.elt list -> unit

  (** [tag_any tag_list] Tag anything with all given tags. *)
  val tag_any : Tags.elt list -> unit

  (** Returns the set of tags that applies to the given pathname. *)
  val tags_of_pathname : Pathname.t -> Tags.t

  (** Run the given command and returns it's output as a string. *)
  val run_and_read : string -> string

  (** Here is the list of hooks that the dispatch function have to handle.
      Generally one responds to one or two hooks (like After_rules) and do
      nothing in the default case. *)
  type hook =
    | Before_hygiene
    | After_hygiene
    | Before_options
    | After_options
    | Before_rules
    | After_rules

  (** [dispatch hook_handler] Is the entry point for ocamlbuild plugins. Every
      plugin must call it with a [hook_handler] where all calls to plugin
      functions lives. *)
  val dispatch : (hook -> unit) -> unit
end
