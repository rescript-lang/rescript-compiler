[@@@warning "-a"]
[@@@ocaml.doc
  "\n BuckleScript compiler\n Copyright (C) 2015-2016 Bloomberg Finance L.P.\n\n This program is free software; you can redistribute it and/or modify\n it under the terms of the GNU Lesser General Public License as published by\n the Free Software Foundation, with linking exception;\n either version 2.1 of the License, or (at your option) any later version.\n\n This program is distributed in the hope that it will be useful,\n but WITHOUT ANY WARRANTY; without even the implied warranty of\n MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n GNU Lesser General Public License for more details.\n\n You should have received a copy of the GNU Lesser General Public License\n along with this program; if not, write to the Free Software\n Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.\n\n\n Author: Hongbo Zhang  \n\n"]
[@@@ocaml.doc "04/26-11:18"]
include
  struct
    module Literals :
      sig
        val js_array_ctor : string
        val js_type_number : string
        val js_type_string : string
        val js_type_object : string
        val js_undefined : string
        val js_prop_length : string
        val param : string
        val partial_arg : string
        val prim : string
        val tmp : string[@@ocaml.doc
                          "temporary varaible used in {!Js_ast_util} "]
        val create : string
        val app : string
        val app_array : string
        val runtime : string
        val stdlib : string
        val imul : string
      end =
      struct
        let js_array_ctor = "Array"
        let js_type_number = "number"
        let js_type_string = "string"
        let js_type_object = "object"
        let js_undefined = "undefined"
        let js_prop_length = "length"
        let prim = "prim"
        let param = "param"
        let partial_arg = "partial_arg"
        let tmp = "tmp"
        let create = "create"
        let app = "_"
        let app_array = "app"
        let runtime = "runtime"
        let stdlib = "stdlib"
        let imul = "imul"
      end 
    module Js_op =
      struct
        [@@@ocaml.text " Define some basic types used in JS IR "]
        type binop =
          | Eq
          | Or
          | And
          | EqEqEq
          | NotEqEq
          | InstanceOf
          | Lt
          | Le
          | Gt
          | Ge
          | Bor
          | Bxor
          | Band
          | Lsl
          | Lsr
          | Asr
          | Plus
          | Minus
          | Mul
          | Div
          | Mod
        type int_op =
          | Bor
          | Bxor
          | Band
          | Lsl
          | Lsr
          | Asr
          | Plus
          | Minus
          | Mul
          | Div
          | Mod[@@ocaml.doc
                 "\nnote that we don't need raise [Div_by_zero] in BuckleScript\n\n{[\nlet add x y = x + y  (* | 0 *)\nlet minus x y = x - y (* | 0 *)\nlet mul x y = x * y   (* caml_mul | Math.imul *)\nlet div x y = x / y (* caml_div (x/y|0)*)\nlet imod x y = x mod y  (* caml_mod (x%y) (zero_divide)*)\n\nlet bor x y = x lor y   (* x  | y *)\nlet bxor x y = x lxor y (* x ^ y *)\nlet band x y = x land y (* x & y *)\nlet ilnot  y  = lnot y (* let lnot x = x lxor (-1) *)\nlet ilsl x y = x lsl y (* x << y*)\nlet ilsr x y = x lsr y  (* x >>> y | 0 *)\nlet iasr  x y = x asr y (* x >> y *)\n]}\n\n\nNote that js treat unsigned shift 0 bits in a special way\n   Unsigned shifts convert their left-hand side to Uint32, \n   signed shifts convert it to Int32.\n   Shifting by 0 digits returns the converted value.\n   {[\n    function ToUint32(x) {\n        return x >>> 0;\n    }\n    function ToInt32(x) {\n        return x >> 0;\n    }\n   ]}\n   So in Js, [-1 >>>0] will be the largest Uint32, while [-1>>0] will remain [-1]\n   and [-1 >>> 0 >> 0 ] will be [-1]\n"]
        type level =
          | Log
          | Info
          | Warn
          | Error
        type kind =
          | Ml
          | Runtime
          | External of string
        type property = Lambda.let_kind =
          | Strict
          | Alias
          | StrictOpt
          | Variable
        type property_name =
          | Key of string
          | Int_key of int
          | Tag
          | Length
        type 'a access =
          | Getter
          | Setter
        type jsint = Int32.t
        type int_or_char = {
          i: jsint;
          c: char option;}
        type float_lit = {
          f: string;}
        type number =
          | Float of float_lit
          | Int of int_or_char
          | Uint of int32
          | Nint of nativeint
        type mutable_flag =
          | Mutable
          | Immutable
          | NA
        type recursive_info =
          | SingleRecursive
          | NonRecursie
          | NA
        type used_stats =
          | Dead_pure
          | Dead_non_pure
          | Exported
          | Once_pure
          | Used
          | Scanning_pure
          | Scanning_non_pure
          | NA
        type ident_info = {
          mutable used_stats: used_stats;}
        type exports = Ident.t list
        type required_modules = (Ident.t* string) list
        type tag_info = Lambda.tag_info =
          | Blk_constructor of string* int
          | Blk_tuple
          | Blk_array
          | Blk_variant of string
          | Blk_record of string array
          | Blk_module of string list option
          | Blk_na
        type length_object =
          | Array
          | String
          | Bytes
          | Function
          | Caml_block
        type code_info =
          | Exp of int option
          |
          Stmt[@ocaml.doc
                " TODO: define constant - for better constant folding  "]
      end
    module Ext_format :
      sig
        [@@@ocaml.text
          " Simplified wrapper module for the standard library [Format] module. \n  "]
        type t = private Format.formatter
        val string : t -> string -> unit
        val break : t -> unit
        val break1 : t -> unit
        val space : t -> unit
        val group : t -> int -> (unit -> 'a) -> 'a[@@ocaml.doc
                                                    " [group] will record current indentation \n    and indent futher\n "]
        val vgroup : t -> int -> (unit -> 'a) -> 'a
        val paren : t -> (unit -> 'a) -> 'a
        val paren_group : t -> int -> (unit -> 'a) -> 'a
        val brace_group : t -> int -> (unit -> 'a) -> 'a
        val brace_vgroup : t -> int -> (unit -> 'a) -> 'a
        val bracket_group : t -> int -> (unit -> 'a) -> 'a
        val newline : t -> unit
        val to_out_channel : out_channel -> t
        val flush : t -> unit -> unit
      end =
      struct
        open Format
        type t = formatter
        let string = pp_print_string
        let break fmt = pp_print_break fmt 0 0
        let break1 fmt = pp_print_break fmt 0 1
        let space fmt = pp_print_break fmt 1 0
        let vgroup fmt indent u =
          pp_open_vbox fmt indent; (let v = u () in pp_close_box fmt (); v)
        let group fmt indent u =
          pp_open_hovbox fmt indent; (let v = u () in pp_close_box fmt (); v)
        let paren fmt u = string fmt "("; (let v = u () in string fmt ")"; v)
        let brace fmt u = string fmt "{"; (let v = u () in string fmt "}"; v)
        let bracket fmt u =
          string fmt "["; (let v = u () in string fmt "]"; v)
        let paren_group st n action = group st n (fun _  -> paren st action)
        let brace_group st n action = group st n (fun _  -> brace st action)
        let brace_vgroup st n action =
          vgroup st n
            (fun _  ->
               string st "{";
               pp_print_break st 0 2;
               (let v = vgroup st 0 action in
                pp_print_break st 0 0; string st "}"; v))
        let bracket_group st n action =
          group st n (fun _  -> bracket st action)
        let newline fmt = pp_print_newline fmt ()
        let to_out_channel = formatter_of_out_channel
        let flush = pp_print_flush
        let list = pp_print_list
      end 
    module Ident_set :
      sig
        [@@@ocaml.text " Set with key specialized as [Ident.t] type\n "]
        [@@@ocaml.text
          " Original set module enhanced with some utilities, \n    note that it's incompatible with [Lambda.IdentSet] \n "]
        include (Set.S with type  elt =  Ident.t)
        val print : Ext_format.t -> t -> unit
      end =
      struct
        include
          Set.Make(struct
                     type t = Ident.t
                     let compare = Pervasives.compare
                   end)
        let print ppf v =
          let open Ext_format in
            brace_vgroup ppf 0
              (fun _  ->
                 iter
                   (fun v  ->
                      string ppf
                        (Printf.sprintf "%s/%d" v.Ident.name v.stamp);
                      Ext_format.space ppf) v)
      end 
    module Ext_list :
      sig
        [@@@ocaml.text " Extension to the standard library [List] module "]
        [@@@ocaml.text
          " TODO some function are no efficiently implemented. "]
        val filter_map : ('a -> 'b option) -> 'a list -> 'b list
        val same_length : 'a list -> 'b list -> bool
        val init : int -> (int -> 'a) -> 'a list
        val take : int -> 'a list -> ('a list* 'a list)
        val try_take : int -> 'a list -> ('a list* int* 'a list)
        val exclude_tail : 'a list -> 'a list
        val filter_map2 :
          ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list
        val filter_map2i :
          (int -> 'a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list
        val filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b list
        val flat_map2 :
          ('a -> 'b -> 'c list) -> 'a list -> 'b list -> 'c list
        val flat_map : ('a -> 'b list) -> 'a list -> 'b list
        val flat_map2_last :
          (bool -> 'a -> 'b -> 'c list) -> 'a list -> 'b list -> 'c list
        val map_last : (bool -> 'a -> 'b) -> 'a list -> 'b list
        val stable_group : ('a -> 'a -> bool) -> 'a list -> 'a list list
        val drop : int -> 'a list -> 'a list
        val for_all_ret : ('a -> bool) -> 'a list -> 'a option
        val for_all_opt : ('a -> 'b option) -> 'a list -> 'b option[@@ocaml.doc
                                                                    " [for_all_opt f l] returns [None] if all return [None],  \n    otherwise returns the first one. \n "]
        val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b[@@ocaml.doc
                                                            " same as [List.fold_left]. \n    Provide an api so that list can be easily swapped by other containers  \n "]
        val rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list
        val rev_map_acc : 'a list -> ('b -> 'a) -> 'b list -> 'a list
        val rev_iter : ('a -> unit) -> 'a list -> unit
        val for_all2_no_exn :
          ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        val find_opt : ('a -> 'b option) -> 'a list -> 'b option
        val split_map : ('a -> ('b* 'c)) -> 'a list -> ('b list* 'c list)
        [@@ocaml.doc " [f] is applied follow the list order "]
      end =
      struct
        let rec filter_map (f : 'a -> 'b option) xs =
          match xs with
          | [] -> []
          | y::ys ->
              (match f y with
               | None  -> filter_map f ys
               | Some z -> z :: (filter_map f ys))
        let rec same_length xs ys =
          match (xs, ys) with
          | ([],[]) -> true
          | (_::xs,_::ys) -> same_length xs ys
          | (_,_) -> false
        let filter_mapi (f : int -> 'a -> 'b option) xs =
          let rec aux i xs =
            match xs with
            | [] -> []
            | y::ys ->
                (match f i y with
                 | None  -> aux (i + 1) ys
                 | Some z -> z :: (aux (i + 1) ys)) in
          aux 0 xs
        let rec filter_map2 (f : 'a -> 'b -> 'c option) xs ys =
          match (xs, ys) with
          | ([],[]) -> []
          | (u::us,v::vs) ->
              (match f u v with
               | None  -> filter_map2 f us vs
               | Some z -> z :: (filter_map2 f us vs))
          | _ -> invalid_arg "Ext_list.filter_map2"
        let filter_map2i (f : int -> 'a -> 'b -> 'c option) xs ys =
          let rec aux i xs ys =
            match (xs, ys) with
            | ([],[]) -> []
            | (u::us,v::vs) ->
                (match f i u v with
                 | None  -> aux (i + 1) us vs
                 | Some z -> z :: (aux (i + 1) us vs))
            | _ -> invalid_arg "Ext_list.filter_map2i" in
          aux 0 xs ys
        let rec rev_map_append f l1 l2 =
          match l1 with | [] -> l2 | a::l -> rev_map_append f l ((f a) :: l2)
        let flat_map2 f lx ly =
          let rec aux acc lx ly =
            match (lx, ly) with
            | ([],[]) -> List.rev acc
            | (x::xs,y::ys) -> aux (List.rev_append (f x y) acc) xs ys
            | (_,_) -> invalid_arg "Ext_list.flat_map2" in
          aux [] lx ly
        let flat_map f lx =
          let rec aux acc lx =
            match lx with
            | [] -> List.rev acc
            | y::ys -> aux (List.rev_append (f y) acc) ys in
          aux [] lx
        let rec map2_last f l1 l2 =
          match (l1, l2) with
          | ([],[]) -> []
          | (u::[],v::[]) -> [f true u v]
          | (a1::l1,a2::l2) ->
              let r = f false a1 a2 in r :: (map2_last f l1 l2)
          | (_,_) -> invalid_arg "List.map2_last"
        let rec map_last f l1 =
          match l1 with
          | [] -> []
          | u::[] -> [f true u]
          | a1::l1 -> let r = f false a1 in r :: (map_last f l1)
        let flat_map2_last f lx ly = List.concat @@ (map2_last f lx ly)
        let init n f = Array.to_list (Array.init n f)
        let take n l =
          let arr = Array.of_list l in
          let arr_length = Array.length arr in
          if arr_length < n
          then invalid_arg "Ext_list.take"
          else
            ((Array.to_list (Array.sub arr 0 n)),
              (Array.to_list (Array.sub arr n (arr_length - n))))
        let try_take n l =
          let arr = Array.of_list l in
          let arr_length = Array.length arr in
          if arr_length <= n
          then (l, arr_length, [])
          else
            ((Array.to_list (Array.sub arr 0 n)), n,
              (Array.to_list (Array.sub arr n (arr_length - n))))
        let exclude_tail (x : 'a list) =
          (let rec aux acc x =
             match x with
             | [] -> invalid_arg "Ext_list.exclude_tail"
             | _::[] -> List.rev acc
             | y0::ys -> aux (y0 :: acc) ys in
           aux [] x : 'a list)
        let rec group (cmp : 'a -> 'a -> bool) (lst : 'a list) =
          (match lst with | [] -> [] | x::xs -> aux cmp x (group cmp xs) : 
          'a list list)
        and aux cmp (x : 'a) (xss : 'a list list) =
          (match xss with
           | [] -> [[x]]
           | y::ys ->
               if cmp x (List.hd y)
               then (x :: y) :: ys
               else y :: (aux cmp x ys) : 'a list list)
        let stable_group cmp lst = (group cmp lst) |> List.rev
        let rec drop n h =
          if n < 0
          then invalid_arg "Ext_list.drop"
          else
            if n = 0
            then h
            else
              if h = []
              then invalid_arg "Ext_list.drop"
              else drop (n - 1) (List.tl h)
        let rec for_all_ret p =
          function
          | [] -> None
          | a::l -> if p a then for_all_ret p l else Some a
        let rec for_all_opt p =
          function
          | [] -> None
          | a::l -> (match p a with | None  -> for_all_opt p l | v -> v)
        let fold f l init =
          List.fold_left (fun acc  -> fun i  -> f i init) init l
        let rev_map_acc acc f l =
          let rec rmap_f accu =
            function | [] -> accu | a::l -> rmap_f ((f a) :: accu) l in
          rmap_f acc l
        let rec rev_iter f xs =
          match xs with | [] -> () | y::ys -> (rev_iter f ys; f y)
        let rec for_all2_no_exn p l1 l2 =
          match (l1, l2) with
          | ([],[]) -> true
          | (a1::l1,a2::l2) -> (p a1 a2) && (for_all2_no_exn p l1 l2)
          | (_,_) -> false
        let rec find_no_exn p =
          function
          | [] -> None
          | x::l -> if p x then Some x else find_no_exn p l
        let rec find_opt p =
          function
          | [] -> None
          | x::l ->
              (match p x with | Some _ as v -> v | None  -> find_opt p l)
        let split_map (f : 'a -> ('b* 'c)) (xs : 'a list) =
          (let rec aux bs cs xs =
             match xs with
             | [] -> ((List.rev bs), (List.rev cs))
             | u::us -> let (b,c) = f u in aux (b :: bs) (c :: cs) us in
           aux [] [] xs : ('b list* 'c list))
      end 
    module Js_fun_env :
      sig
        [@@@ocaml.text
          " Define type t used in JS IR to collect some meta data for a function, like its closures, etc \n  "]
        type t
        val empty : ?immutable_mask:bool array -> int -> t
        val is_tailcalled : t -> bool
        val is_empty : t -> bool
        val set_bound : t -> Ident_set.t -> unit
        val get_bound : t -> Ident_set.t
        val set_lexical_scope : t -> Ident_set.t -> unit
        val get_lexical_scope : t -> Ident_set.t
        val to_string : t -> string
        val mark_unused : t -> int -> unit
        val get_unused : t -> int -> bool
        val get_mutable_params : Ident.t list -> t -> Ident.t list
        val get_bound : t -> Ident_set.t
        val get_length : t -> int
      end =
      struct
        type immutable_mask =
          |
          All_immutable_and_no_tail_call[@ocaml.doc
                                          " iff not tailcalled \n         if tailcalled, in theory, it does not need change params, \n         for example\n         {[\n         let rec f  (n : int ref) = \n            if !n > 0 then decr n; print_endline \"hi\"\n            else  f n\n         ]}\n         in this case, we still create [Immutable_mask], \n         since the inline behavior is slightly different\n      "]
          | Immutable_mask of bool array
        type t =
          {
          mutable bound: Ident_set.t;
          mutable bound_loop_mutable_values: Ident_set.t;
          used_mask: bool array;
          immutable_mask: immutable_mask;}[@@ocaml.doc
                                            " Invariant: unused param has to be immutable "]
        let empty ?immutable_mask  n =
          {
            bound = Ident_set.empty;
            used_mask = (Array.make n false);
            immutable_mask =
              (match immutable_mask with
               | Some x -> Immutable_mask x
               | None  -> All_immutable_and_no_tail_call);
            bound_loop_mutable_values = Ident_set.empty
          }
        let is_tailcalled x =
          x.immutable_mask <> All_immutable_and_no_tail_call
        let mark_unused t i = (t.used_mask).(i) <- true
        let get_unused t i = (t.used_mask).(i)
        let get_length t = Array.length t.used_mask
        let to_string env =
          String.concat ","
            (List.map
               (fun (id : Ident.t)  ->
                  Printf.sprintf "%s/%d" id.name id.stamp)
               (Ident_set.elements env.bound))
        let get_mutable_params (params : Ident.t list) (x : t) =
          match x.immutable_mask with
          | All_immutable_and_no_tail_call  -> []
          | Immutable_mask xs ->
              Ext_list.filter_mapi
                (fun i  -> fun p  -> if not (xs.(i)) then Some p else None)
                params
        let get_bound t = t.bound
        let set_bound env v = env.bound <- v
        let set_lexical_scope env bound_loop_mutable_values =
          env.bound_loop_mutable_values <- bound_loop_mutable_values
        let get_lexical_scope env = env.bound_loop_mutable_values
        let is_empty t = Ident_set.is_empty t.bound
      end 
    module Js_closure :
      sig
        [@@@ocaml.text
          " Define a type used in JS IR to help convert lexical scope to JS [var] \n    based scope convention\n "]
        type t = {
          mutable outer_loop_mutable_values: Ident_set.t;}
        val empty : unit -> t
        val get_lexical_scope : t -> Ident_set.t
        val set_lexical_scope : t -> Ident_set.t -> unit
      end =
      struct
        type t = {
          mutable outer_loop_mutable_values: Ident_set.t;}
        let empty () = { outer_loop_mutable_values = Ident_set.empty }
        let set_lexical_scope t v = t.outer_loop_mutable_values <- v
        let get_lexical_scope t = t.outer_loop_mutable_values
      end 
    module Js_call_info :
      sig
        [@@@ocaml.text
          " Type for collecting call site information, used in JS IR "]
        type arity =
          | Full
          | NA
        type call_info =
          | Call_ml
          | Call_builtin_runtime
          | Call_na
        type t = {
          call_info: call_info;
          arity: arity;}
        val dummy : t
        val builtin_runtime_call : t
        val ml_full_call : t
      end =
      struct
        type arity =
          | Full
          | NA
        type call_info =
          | Call_ml
          | Call_builtin_runtime
          | Call_na
        type t = {
          call_info: call_info;
          arity: arity;}
        let dummy = { arity = NA; call_info = Call_na }
        let builtin_runtime_call =
          { arity = Full; call_info = Call_builtin_runtime }
        let ml_full_call = { arity = Full; call_info = Call_ml }
      end 
    module J =
      struct
        [@@@ocaml.text
          " Javascript IR\n  \n    It's a subset of Javascript AST specialized for OCaml lambda backend\n\n    Note it's not exactly the same as Javascript, the AST itself follows lexical\n    convention and [Block] is just a sequence of statements, which means it does \n    not introduce new scope\n"]
        type label = string
        and binop = Js_op.binop
        and int_op = Js_op.int_op
        and kind = Js_op.kind
        and property = Js_op.property
        and number = Js_op.number
        and mutable_flag = Js_op.mutable_flag
        and ident_info = Js_op.ident_info
        and exports = Js_op.exports
        and tag_info = Js_op.tag_info
        and required_modules = Js_op.required_modules
        and code_info = Js_op.code_info[@@ocaml.doc
                                         " object literal, if key is ident, in this case, it might be renamed by \n    Google Closure  optimizer,\n    currently we always use quote\n "]
        and property_name = Js_op.property_name[@@ocaml.doc
                                                 " object literal, if key is ident, in this case, it might be renamed by \n    Google Closure  optimizer,\n    currently we always use quote\n "]
        and jsint = Js_op.jsint
        and ident = Ident.t
        and vident =
          | Id of ident
          | Qualified of ident* kind* string option
        and exception_ident = ident
        and for_ident = ident
        and for_direction = Asttypes.direction_flag
        and property_map = (property_name* expression) list
        and length_object = Js_op.length_object
        and expression_desc =
          | Math of string* expression list
          | Length of expression* length_object
          | Char_of_int of expression
          | Char_to_int of expression
          | Array_of_size of expression
          | Array_copy of expression
          | Array_append of expression* expression
          | String_append of expression* expression
          | Int_of_boolean of expression
          | Anything_to_number of expression
          | Bool of bool
          | Typeof of expression
          | Not of expression
          | String_of_small_int_array of expression
          | Json_stringify of expression
          | Anything_to_string of expression
          | Dump of Js_op.level* expression list
          | Seq of expression* expression
          | Cond of expression* expression* expression
          | Bin of binop* expression* expression
          | FlatCall of expression* expression
          | Bind of expression* expression
          | Call of expression* expression list* Js_call_info.t
          | String_access of expression* expression
          | Access of expression* expression
          | Dot of expression* string* bool
          | New of expression* expression list option
          | Var of vident
          | Fun of ident list* block* Js_fun_env.t
          | Str of bool* string
          | Raw_js_code of string* code_info
          | Array of expression list* mutable_flag
          | Caml_block of expression list* mutable_flag* expression*
          tag_info
          | Caml_uninitialized_obj of expression* expression
          | Caml_block_tag of expression
          | Caml_block_set_tag of expression* expression
          | Caml_block_set_length of expression* expression
          | Number of number
          | Object of property_map
        and for_ident_expression = expression
        and finish_ident_expression = expression
        and statement_desc =
          | Block of block
          | Variable of variable_declaration
          | Exp of expression
          | If of expression* block* block option
          | While of label option* expression* block* Js_closure.t
          | ForRange of for_ident_expression option* finish_ident_expression*
          for_ident* for_direction* block* Js_closure.t
          | Continue of label
          | Break
          | Return of return_expression
          | Int_switch of expression* int case_clause list* block option
          | String_switch of expression* string case_clause list* block
          option
          | Throw of expression
          | Try of block* (exception_ident* block) option* block option
          | Debugger
        and return_expression = {
          return_value: expression;}
        and expression =
          {
          expression_desc: expression_desc;
          comment: string option;}
        and statement =
          {
          statement_desc: statement_desc;
          comment: string option;}
        and variable_declaration =
          {
          ident: ident;
          value: expression option;
          property: property;
          ident_info: ident_info;}
        and 'a case_clause = {
          case: 'a;
          body: (block* bool);}
        and block = statement list
        and program =
          {
          name: string;
          block: block;
          exports: exports;
          export_set: Ident_set.t;}
        and deps_program =
          {
          program: program;
          modules: required_modules;
          side_effect: string option;}
      end
    module Lam_module_ident :
      sig
        [@@@ocaml.text " A type for qualified identifiers in Lambda IR \n "]
        type t = private {
          id: Ident.t;
          kind: Js_op.kind;}
        val id : t -> Ident.t
        val name : t -> string
        val mk : J.kind -> Ident.t -> t
        val of_ml : Ident.t -> t
        val of_external : Ident.t -> string -> t
        val of_runtime : Ident.t -> t
      end =
      struct
        type t = {
          id: Ident.t;
          kind: Js_op.kind;}
        let id x = x.id
        let of_ml id = { id; kind = Ml }
        let of_external id name = { id; kind = (External name) }
        let of_runtime id = { id; kind = Runtime }
        let mk kind id = { id; kind }
        let name x =
          (match (x.kind : J.kind) with
           | Ml |Runtime  -> (x.id).name
           | External v -> v : string)
        type module_property = bool
      end 
    module Hash_set :
      sig
        [@@@ocaml.text
          " A naive hashset implementation on top of [hashtbl], the value is [unit]"]
        type 'a hashset
        val create : ?random:bool -> int -> 'a hashset
        val clear : 'a hashset -> unit
        val reset : 'a hashset -> unit
        val copy : 'a hashset -> 'a hashset
        val add : 'a hashset -> 'a -> unit
        val mem : 'a hashset -> 'a -> bool
        val iter : ('a -> unit) -> 'a hashset -> unit
        val elements : 'a hashset -> 'a list
      end =
      struct
        include Hashtbl
        type 'a hashset = ('a,unit) Hashtbl.t
        let add tbl k = replace tbl k ()
        let iter f = iter (fun k  -> fun _  -> f k)
        let elements set =
          fold (fun k  -> fun _  -> fun acc  -> k :: acc) set []
      end 
    module Lam_stats :
      sig
        [@@@ocaml.text " Types defined for lambda analysis "]
        type function_arities =
          | Determin of bool* (int* Ident.t list option) list*
          bool[@ocaml.doc
                " when the first argument is true, it is for sure \n\n      approximation sound but not complete \n      the last one means it can take any params later, \n      for an exception: it is (Determin (true,[], true))\n   "]
          | NA
        type alias_tbl = (Ident.t,Ident.t) Hashtbl.t[@@ocaml.doc
                                                      " Keep track of which identifiers are aliased\n  "]
        type state =
          | Live[@ocaml.doc " Globals are always live "]
          | Dead[@ocaml.doc " removed after elimination "]
          | NA
        type function_kind =
          | Functor
          | Function
          | NA
        type rec_flag =
          | Rec
          | Non_rec
        type function_id =
          {
          kind: function_kind;
          mutable arity: function_arities;
          lambda: Lambda.lambda;
          rec_flag: rec_flag;}
        type element =
          | NA
          | SimpleForm of Lambda.lambda
        type boxed_nullable =
          | Undefined
          | Null
          | Null_undefined
          | Normal
        type kind =
          | ImmutableBlock of element array* boxed_nullable
          | MutableBlock of element array
          | Constant of Lambda.structured_constant
          | Module of
          Ident.t[@ocaml.doc " TODO: static module vs first class module "]
          | Function of function_id
          | Exception
          |
          Parameter[@ocaml.doc
                     " For this case, it can help us determine whether it should be inlined or not "]
          |
          NA[@ocaml.doc
              " Not such information is associated with an identifier, it is immutable, \n           if you only associate a property to an identifier \n           we should consider [Lassign]\n        "]
        type ident_tbl = (Ident.t,kind) Hashtbl.t
        type ident_info = {
          kind: kind;
          state: state;}
        type meta =
          {
          env: Env.t;
          filename: string;
          export_idents: Ident_set.t;
          exports: Ident.t list;
          alias_tbl: alias_tbl;
          exit_codes: int Hash_set.hashset;
          ident_tbl:
            ident_tbl[@ocaml.doc
                       " we don't need count arities for all identifiers, for identifiers\n      for sure it's not a function, there is no need to count them\n   "];
          mutable required_modules: Lam_module_ident.t list;}
      end =
      struct
        type function_arities =
          | Determin of bool* (int* Ident.t list option) list* bool
          | NA
        type alias_tbl = (Ident.t,Ident.t) Hashtbl.t
        type function_kind =
          | Functor
          | Function
          | NA
        type rec_flag =
          | Rec
          | Non_rec
        type function_id =
          {
          kind: function_kind;
          mutable arity: function_arities;
          lambda: Lambda.lambda;
          rec_flag: rec_flag;}
        type element =
          | NA
          | SimpleForm of Lambda.lambda
        type boxed_nullable =
          | Undefined
          | Null
          | Null_undefined
          | Normal
        type kind =
          | ImmutableBlock of element array* boxed_nullable
          | MutableBlock of element array
          | Constant of Lambda.structured_constant
          | Module of
          Ident.t[@ocaml.doc
                   " Global module, local module is treated as an array\n         "]
          | Function of function_id[@ocaml.doc " True then functor "]
          | Exception
          |
          Parameter[@ocaml.doc
                     " For this case, it can help us determine whether it should be inlined or not "]
          |
          NA[@ocaml.doc
              " \n       {[ let v/2 =  js_from_nullable u]} \n\n       {[ let v/2 = js_from_nullable exp]}\n       can be translated into \n       {[\n         let v/1 = exp in \n         let v/2 =a js_from_nullable exp \n       ]}\n       so that [Pfield v/2 0] will be replaced by [v/1], \n       [Lif(v/1)] will be translated into [Lif (v/2 === undefined )]\n    "]
        type ident_tbl = (Ident.t,kind) Hashtbl.t
        type state =
          | Live[@ocaml.doc " Globals are always live "]
          | Dead[@ocaml.doc " removed after elimination "]
          | NA
        type ident_info = {
          kind: kind;
          state: state;}
        type meta =
          {
          env: Env.t;
          filename: string;
          export_idents: Ident_set.t;
          exports: Ident.t list;
          alias_tbl: alias_tbl;
          exit_codes: int Hash_set.hashset;
          ident_tbl:
            ident_tbl[@ocaml.doc
                       " we don't need count arities for all identifiers, for identifiers\n      for sure it's not a function, there is no need to count them\n  "];
          mutable required_modules: Lam_module_ident.t list;}
      end 
    module Lam_current_unit :
      sig
        val set_file : string -> unit
        val get_file : unit -> string
        val get_module_name : unit -> string
        val iset_debug_file : string -> unit
        val set_debug_file : string -> unit
        val get_debug_file : unit -> string
        val is_same_file : unit -> bool
      end =
      struct
        let file = ref ""
        let debug_file = ref ""
        let set_file f = file := f
        let get_file () = !file
        let get_module_name () =
          Filename.chop_extension (String.uncapitalize (!file))
        let iset_debug_file _ = ()
        let set_debug_file f = debug_file := f
        let get_debug_file () = !debug_file
        let is_same_file () =
          ((!debug_file) <> "") && ((!debug_file) = (!file))
      end 
    module Lam_comb :
      sig
        type t = Lambda.lambda
        val if_ : t -> t -> t -> t
        val switch : t -> Lambda.lambda_switch -> t
        val stringswitch : t -> (string* t) list -> t option -> t
      end =
      struct
        type t = Lambda.lambda
        let if_ a (b : t) c =
          match a with
          | Lambda.Lconst v ->
              (match v with
               | Const_pointer (x,_)|Lambda.Const_base (Const_int x) ->
                   if x <> 0 then b else c
               | Const_base (Const_char x) ->
                   if (Char.code x) <> 0 then b else c
               | Const_base (Const_int32 x) -> if x <> 0l then b else c
               | Const_base (Const_int64 x) -> if x <> 0L then b else c
               | Const_base (Const_nativeint x) -> if x <> 0n then b else c
               | Const_base (Const_string _|Const_float _) -> b
               | Const_block _|Const_float_array _|Const_immstring _ -> b)
          | _ -> Lambda.Lifthenelse (a, b, c)
        let switch lam lam_switch = Lambda.Lswitch (lam, lam_switch)
        let stringswitch lam cases default =
          match lam with
          | Lambda.Lconst (Lambda.Const_base (Const_string (a,_))) ->
              (try List.assoc a cases
               with
               | Not_found  ->
                   (match default with | Some x -> x | None  -> assert false))
          | _ -> Lambda.Lstringswitch (lam, cases, default)
      end 
    module Ident_map =
      struct
        [@@@ocaml.text
          " Map with key specialized as [Ident] type, enhanced with some utilities "]
        include
          Map.Make(struct
                     type t = Ident.t
                     let compare = Pervasives.compare[@@ocaml.doc
                                                       "TODO: fix me"]
                   end)
        let of_list lst =
          List.fold_left (fun acc  -> fun (k,v)  -> add k v acc) empty lst
        let keys map = fold (fun k  -> fun _  -> fun acc  -> k :: acc) map []
        let add_if_not_exist key v m = if mem key m then m else add key v m
        let merge_disjoint m1 m2 =
          merge
            (fun k  ->
               fun x0  ->
                 fun y0  ->
                   match (x0, y0) with
                   | (None ,None ) -> None
                   | (None ,Some v)|(Some v,None ) -> Some v
                   | (_,_) ->
                       invalid_arg "merge_disjoint: maps are not disjoint")
            m1 m2
      end
    module Lam_analysis :
      sig
        [@@@ocaml.text
          " A module which provides some basic analysis over lambda expression "]
        val no_side_effects : Lambda.lambda -> bool[@@ocaml.doc
                                                     " No side effect, but it might depend on data store "]
        val size : Lambda.lambda -> int
        val eq_lambda : Lambda.lambda -> Lambda.lambda -> bool[@@ocaml.doc
                                                                " a conservative version of comparing two lambdas, mostly \n    for looking for similar cases in switch\n "]
        val is_closed_by : Ident_set.t -> Lambda.lambda -> bool[@@ocaml.doc
                                                                 " [is_closed_by map lam]\n    return [true] if all unbound variables\n    belongs to the given [map] "]
        val is_closed : Lambda.lambda -> bool
        type stats = {
          mutable top: bool;
          mutable times: int;}
        val is_closed_with_map :
          Ident_set.t ->
            Ident.t list -> Lambda.lambda -> (bool* stats Ident_map.t)
        val param_map_of_list : Ident.t list -> stats Ident_map.t
        val free_variables :
          Ident_set.t ->
            stats Ident_map.t -> Lambda.lambda -> stats Ident_map.t
        val small_inline_size : int
        val exit_inline_size : int
        val safe_to_inline : Lambda.lambda -> bool
      end =
      struct
        let rec no_side_effects (lam : Lambda.lambda) =
          (match lam with
           | Lvar _|Lconst _|Lfunction _ -> true
           | Lprim (primitive,args) ->
               (List.for_all no_side_effects args) &&
                 ((match primitive with
                   | Pccall { prim_name;_} ->
                       (match (prim_name, args) with
                        | (("caml_register_named_value"|"caml_set_oo_id"
                            |"caml_is_js"|"caml_int64_float_of_bits"
                            |"caml_sys_get_config"|"caml_sys_get_argv"
                            |"caml_create_string"|"caml_make_vect"
                            |"caml_obj_dup"|"caml_obj_block"
                            |"js_from_nullable"|"js_from_def"
                            |"js_from_nullable_def"),_) -> true
                        | ("caml_ml_open_descriptor_in",(Lconst (Const_base
                           (Const_int 0)))::[]) -> true
                        | ("caml_ml_open_descriptor_out",(Lconst (Const_base
                           (Const_int (1|2))))::[]) -> true
                        | (_,_) -> false)
                   | Pidentity |Pbytes_to_string |Pbytes_of_string 
                     |Pchar_to_int |Pchar_of_int |Ploc _|Pgetglobal _
                     |Pmakeblock _|Pfield _|Pfloatfield _|Pduprecord _
                     |Psequand |Psequor |Pnot |Pnegint |Paddint |Psubint 
                     |Pmulint |Pdivint |Pmodint |Pandint |Porint |Pxorint 
                     |Plslint |Plsrint |Pasrint |Pintcomp _|Pintoffloat 
                     |Pfloatofint |Pnegfloat |Pabsfloat |Paddfloat |Psubfloat 
                     |Pmulfloat |Pdivfloat |Pfloatcomp _|Pstringlength 
                     |Pstringrefu |Pstringrefs |Pbyteslength |Pbytesrefu 
                     |Pbytesrefs |Pmakearray _|Parraylength _|Parrayrefu _
                     |Parrayrefs _|Pisint |Pisout |Pbintofint _|Pintofbint _
                     |Pcvtbint _|Pnegbint _|Paddbint _|Psubbint _|Pmulbint _
                     |Pdivbint _|Pmodbint _|Pandbint _|Porbint _|Pxorbint _
                     |Plslbint _|Plsrbint _|Pasrbint _|Pbintcomp _
                     |Pbigarrayref _|Pctconst _|Pint_as_pointer |Poffsetint _
                       -> true
                   | Pignore |Prevapply _|Pdirapply _|Pstringsetu 
                     |Pstringsets |Pbytessetu |Pbytessets |Pbittest 
                     |Parraysets _|Pbigarrayset _|Pbigarraydim _
                     |Pstring_load_16 _|Pstring_load_32 _|Pstring_load_64 _
                     |Pstring_set_16 _|Pstring_set_32 _|Pstring_set_64 _
                     |Pbigstring_load_16 _|Pbigstring_load_32 _
                     |Pbigstring_load_64 _|Pbigstring_set_16 _
                     |Pbigstring_set_32 _|Pbigstring_set_64 _|Pbswap16 
                     |Pbbswap _|Parraysetu _|Poffsetref _|Praise _|Plazyforce 
                     |Pmark_ocaml_object |Psetfield _|Psetfloatfield _
                     |Psetglobal _ -> false))
           | Llet (_,_,arg,body) ->
               (no_side_effects arg) && (no_side_effects body)
           | Lswitch (_,_) -> false
           | Lstringswitch (_,_,_) -> false
           | Lstaticraise _ -> false
           | Lstaticcatch _ -> false
           | Ltrywith
               (Lprim
                (Pccall { prim_name = "caml_sys_getenv" },(Lconst _)::[]),exn,Lifthenelse
                (Lprim
                 (_,(Lvar exn1)::(Lprim
                  (Pgetglobal { name = "Not_found" },[]))::[]),then_,_))
               when Ident.same exn1 exn -> no_side_effects then_
           | Ltrywith (body,exn,handler) ->
               (no_side_effects body) && (no_side_effects handler)
           | Lifthenelse (a,b,c) ->
               (no_side_effects a) &&
                 ((no_side_effects b) && (no_side_effects c))
           | Lsequence (e0,e1) ->
               (no_side_effects e0) && (no_side_effects e1)
           | Lwhile (a,b) -> (no_side_effects a) && (no_side_effects b)
           | Lfor _ -> false
           | Lassign _ -> false
           | Lsend _ -> false
           | Levent (e,_) -> no_side_effects e
           | Lifused _ -> false
           | Lapply _ -> false
           | Lletrec (bindings,body) ->
               (List.for_all (fun (_,b)  -> no_side_effects b) bindings) &&
                 (no_side_effects body) : bool)
        exception Too_big_to_inline
        let really_big () = raise Too_big_to_inline
        let big_lambda = 1000
        let rec size (lam : Lambda.lambda) =
          try
            match lam with
            | Lvar _ -> 1
            | Lconst c -> size_constant c
            | Llet (_,_,l1,l2) -> (1 + (size l1)) + (size l2)
            | Lletrec _ -> really_big ()
            | Lprim (Pfield _,(Lprim (Pgetglobal _,[]))::[]) -> 1
            | Lprim (Praise _,l::[]) -> size l
            | Lprim (_,ll) -> size_lams 1 ll
            | Lapply (f,args,_) -> size_lams (size f) args
            | Lfunction (_,_params,body) -> size body
            | Lswitch (_,_) -> really_big ()
            | Lstringswitch (_,_,_) -> really_big ()
            | Lstaticraise (i,ls) ->
                List.fold_left (fun acc  -> fun x  -> (size x) + acc) 1 ls
            | Lstaticcatch (l1,(i,x),l2) -> really_big ()
            | Ltrywith (l1,v,l2) -> really_big ()
            | Lifthenelse (l1,l2,l3) ->
                ((1 + (size l1)) + (size l2)) + (size l3)
            | Lsequence (l1,l2) -> (size l1) + (size l2)
            | Lwhile (l1,l2) -> really_big ()
            | Lfor (flag,l1,l2,dir,l3) -> really_big ()
            | Lassign (_,v) -> 1 + (size v)
            | Lsend _ -> really_big ()
            | Levent (l,_) -> size l
            | Lifused (v,l) -> size l
          with | Too_big_to_inline  -> 1000
        and size_constant x =
          match x with
          | Const_base _|Const_immstring _|Const_pointer _ -> 1
          | Const_block (_,_,str) ->
              List.fold_left (fun acc  -> fun x  -> acc + (size_constant x))
                0 str
          | Const_float_array xs -> List.length xs
        and size_lams acc (lams : Lambda.lambda list) =
          List.fold_left (fun acc  -> fun l  -> acc + (size l)) acc lams
        let exit_inline_size = 7
        let small_inline_size = 5
        let rec eq_lambda (l1 : Lambda.lambda) (l2 : Lambda.lambda) =
          match (l1, l2) with
          | (Lvar i1,Lvar i2) -> Ident.same i1 i2
          | (Lconst c1,Lconst c2) -> c1 = c2
          | (Lapply (l1,args1,_),Lapply (l2,args2,_)) ->
              (eq_lambda l1 l2) && (List.for_all2 eq_lambda args1 args2)
          | (Lfunction _,Lfunction _) -> false
          | (Lassign (v0,l0),Lassign (v1,l1)) ->
              (Ident.same v0 v1) && (eq_lambda l0 l1)
          | (Lstaticraise (id,ls),Lstaticraise (id1,ls1)) ->
              (id = id1) && (List.for_all2 eq_lambda ls ls1)
          | (Llet (_,_,_,_),Llet (_,_,_,_)) -> false
          | (Lletrec _,Lletrec _) -> false
          | (Lprim (p,ls),Lprim (p1,ls1)) ->
              (eq_primitive p p1) && (List.for_all2 eq_lambda ls ls1)
          | (Lswitch _,Lswitch _) -> false
          | (Lstringswitch _,Lstringswitch _) -> false
          | (Lstaticcatch _,Lstaticcatch _) -> false
          | (Ltrywith _,Ltrywith _) -> false
          | (Lifthenelse (a,b,c),Lifthenelse (a0,b0,c0)) ->
              (eq_lambda a a0) && ((eq_lambda b b0) && (eq_lambda c c0))
          | (Lsequence (a,b),Lsequence (a0,b0)) ->
              (eq_lambda a a0) && (eq_lambda b b0)
          | (Lwhile (p,b),Lwhile (p0,b0)) ->
              (eq_lambda p p0) && (eq_lambda b b0)
          | (Lfor (_,_,_,_,_),Lfor (_,_,_,_,_)) -> false
          | (Lsend _,Lsend _) -> false
          | (Levent (v,_),Levent (v0,_)) -> eq_lambda v v0
          | (Lifused _,Lifused _) -> false
          | (_,_) -> false
        and eq_primitive (p : Lambda.primitive) (p1 : Lambda.primitive) =
          match (p, p1) with
          | (Pccall { prim_name = n0; prim_attributes = [] },Pccall
             { prim_name = n1; prim_attributes = [] }) -> n0 = n1
          | (Pfield (n0,_dbg_info0),Pfield (n1,_dbg_info1)) -> n0 = n1
          | (Psetfield (i0,b0,_dbg_info0),Psetfield (i1,b1,_dbg_info1)) ->
              (i0 = i1) && (b0 = b1)
          | (_,_) -> (try p = p1 with | _ -> false)
        type stats = {
          mutable top: bool;
          mutable times: int;}
        type env = {
          top: bool;
          loop: bool;}
        let no_substitute = { top = false; loop = true }
        let fresh_env = { top = true; loop = false }
        let fresh_stats () = { top = true; times = 0 }
        let param_map_of_list lst =
          List.fold_left
            (fun acc  -> fun l  -> Ident_map.add l (fresh_stats ()) acc)
            Ident_map.empty lst
        [@@@ocaml.text
          " Sanity check, remove all varaibles in [local_set] in the last pass "]
        let free_variables (export_idents : Ident_set.t)
          (params : stats Ident_map.t) lam =
          let fv = ref params in
          let local_set = ref export_idents in
          let local_add k = local_set := (Ident_set.add k (!local_set)) in
          let local_add_list ks =
            local_set :=
              (List.fold_left (fun acc  -> fun k  -> Ident_set.add k acc)
                 (!local_set) ks) in
          let loop_use = 100 in
          let map_use { top; loop } v =
            let times = if loop then loop_use else 1 in
            if Ident_set.mem v (!local_set)
            then ()
            else
              (match Ident_map.find v (!fv) with
               | exception Not_found  ->
                   fv := (Ident_map.add v { top; times } (!fv))
               | v -> (v.times <- v.times + times; v.top <- v.top && top)) in
          let new_env lam (env : env) =
            if env.top
            then
              (if no_side_effects lam then env else { env with top = false })
            else env in
          let rec iter (top : env) (lam : Lambda.lambda) =
            match lam with
            | Lvar v -> map_use top v
            | Lconst _ -> ()
            | Lapply (fn,args,_) ->
                (iter top fn;
                 (let top = new_env fn top in List.iter (iter top) args))
            | Lprim (_p,args) -> List.iter (iter top) args
            | Lfunction (_kind,params,body) ->
                (local_add_list params; iter no_substitute body)
            | Llet (_let_kind,id,arg,body) ->
                (local_add id; iter top arg; iter no_substitute body)
            | Lletrec (decl,body) ->
                (local_set :=
                   (List.fold_left
                      (fun acc  -> fun (id,_)  -> Ident_set.add id acc)
                      (!local_set) decl);
                 List.iter (fun (_,exp)  -> iter no_substitute exp) decl;
                 iter no_substitute body)
            | Lswitch (arg,sw) ->
                (iter top arg;
                 (let top = new_env arg top in
                  List.iter (fun (key,case)  -> iter top case) sw.sw_consts;
                  List.iter (fun (key,case)  -> iter top case) sw.sw_blocks;
                  (match sw.sw_failaction with
                   | None  -> ()
                   | Some x ->
                       let nconsts = List.length sw.sw_consts in
                       let nblocks = List.length sw.sw_blocks in
                       if
                         (nconsts < sw.sw_numconsts) &&
                           (nblocks < sw.sw_numblocks)
                       then iter no_substitute x
                       else iter top x)))
            | Lstringswitch (arg,cases,default) ->
                (iter top arg;
                 (let top = new_env arg top in
                  List.iter (fun (_,act)  -> iter top act) cases;
                  (match default with | None  -> () | Some x -> iter top x)))
            | Lstaticraise (_,args) -> List.iter (iter no_substitute) args
            | Lstaticcatch (e1,(_,vars),e2) ->
                (iter no_substitute e1;
                 local_add_list vars;
                 iter no_substitute e2)
            | Ltrywith (e1,exn,e2) -> (iter top e1; iter no_substitute e2)
            | Lifthenelse (e1,e2,e3) ->
                (iter top e1;
                 (let top = new_env e1 top in iter top e2; iter top e3))
            | Lsequence (e1,e2) -> (iter top e1; iter no_substitute e2)
            | Lwhile (e1,e2) ->
                (iter no_substitute e1; iter no_substitute e2)
            | Lfor (v,e1,e2,dir,e3) ->
                (local_add v;
                 iter no_substitute e1;
                 iter no_substitute e2;
                 iter no_substitute e3)
            | Lassign (id,e) -> (map_use top id; iter top e)
            | Lsend (_k,met,obj,args,_) ->
                (iter no_substitute met;
                 iter no_substitute obj;
                 List.iter (iter no_substitute) args)
            | Levent (lam,evt) -> iter top lam
            | Lifused (v,e) -> iter no_substitute e in
          iter fresh_env lam; !fv[@@ocaml.text
                                   " Sanity check, remove all varaibles in [local_set] in the last pass "]
        let is_closed_by set lam =
          Ident_map.is_empty (free_variables set Ident_map.empty lam)
        let is_closed lam =
          Ident_map.for_all (fun k  -> fun _  -> Ident.global k)
            (free_variables Ident_set.empty Ident_map.empty lam)[@@ocaml.doc
                                                                  " A bit consverative , it should be empty "]
        let is_closed_with_map exports params body =
          let param_map =
            free_variables exports (param_map_of_list params) body in
          let old_count = List.length params in
          let new_count = Ident_map.cardinal param_map in
          ((old_count = new_count), param_map)
        let safe_to_inline (lam : Lambda.lambda) =
          match lam with
          | Lfunction _ -> true
          | Lconst (Const_pointer _|Const_immstring _) -> true
          | _ -> false
      end 
    module Js_fold =
      struct
        open J[@@ocaml.doc
                " GENERATED CODE for fold visitor patten of JS IR  "]
        class virtual fold =
          object (o : 'self_type)
            method string : string -> 'self_type= o#unknown
            method option :
              'a .
                ('self_type -> 'a -> 'self_type) -> 'a option -> 'self_type=
              fun _f_a  ->
                function | None  -> o | Some _x -> let o = _f_a o _x in o
            method list :
              'a . ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type=
              fun _f_a  ->
                function
                | [] -> o
                | _x::_x_i1 ->
                    let o = _f_a o _x in let o = o#list _f_a _x_i1 in o
            method int : int -> 'self_type= o#unknown
            method bool : bool -> 'self_type=
              function | false  -> o | true  -> o
            method vident : vident -> 'self_type=
              function
              | Id _x -> let o = o#ident _x in o
              | Qualified (_x,_x_i1,_x_i2) ->
                  let o = o#ident _x in
                  let o = o#kind _x_i1 in
                  let o = o#option (fun o  -> o#string) _x_i2 in o
            method variable_declaration : variable_declaration -> 'self_type=
              fun
                { ident = _x; value = _x_i1; property = _x_i2;
                  ident_info = _x_i3 }
                 ->
                let o = o#ident _x in
                let o = o#option (fun o  -> o#expression) _x_i1 in
                let o = o#property _x_i2 in let o = o#ident_info _x_i3 in o
            method tag_info : tag_info -> 'self_type= o#unknown
            method statement_desc : statement_desc -> 'self_type=
              function
              | Block _x -> let o = o#block _x in o
              | Variable _x -> let o = o#variable_declaration _x in o
              | Exp _x -> let o = o#expression _x in o
              | If (_x,_x_i1,_x_i2) ->
                  let o = o#expression _x in
                  let o = o#block _x_i1 in
                  let o = o#option (fun o  -> o#block) _x_i2 in o
              | While (_x,_x_i1,_x_i2,_x_i3) ->
                  let o = o#option (fun o  -> o#label) _x in
                  let o = o#expression _x_i1 in
                  let o = o#block _x_i2 in let o = o#unknown _x_i3 in o
              | ForRange (_x,_x_i1,_x_i2,_x_i3,_x_i4,_x_i5) ->
                  let o = o#option (fun o  -> o#for_ident_expression) _x in
                  let o = o#finish_ident_expression _x_i1 in
                  let o = o#for_ident _x_i2 in
                  let o = o#for_direction _x_i3 in
                  let o = o#block _x_i4 in let o = o#unknown _x_i5 in o
              | Continue _x -> let o = o#label _x in o
              | Break  -> o
              | Return _x -> let o = o#return_expression _x in o
              | Int_switch (_x,_x_i1,_x_i2) ->
                  let o = o#expression _x in
                  let o =
                    o#list (fun o  -> o#case_clause (fun o  -> o#int)) _x_i1 in
                  let o = o#option (fun o  -> o#block) _x_i2 in o
              | String_switch (_x,_x_i1,_x_i2) ->
                  let o = o#expression _x in
                  let o =
                    o#list (fun o  -> o#case_clause (fun o  -> o#string))
                      _x_i1 in
                  let o = o#option (fun o  -> o#block) _x_i2 in o
              | Throw _x -> let o = o#expression _x in o
              | Try (_x,_x_i1,_x_i2) ->
                  let o = o#block _x in
                  let o =
                    o#option
                      (fun o  ->
                         fun (_x,_x_i1)  ->
                           let o = o#exception_ident _x in
                           let o = o#block _x_i1 in o) _x_i1 in
                  let o = o#option (fun o  -> o#block) _x_i2 in o
              | Debugger  -> o
            method statement : statement -> 'self_type=
              fun { statement_desc = _x; comment = _x_i1 }  ->
                let o = o#statement_desc _x in
                let o = o#option (fun o  -> o#string) _x_i1 in o
            method return_expression : return_expression -> 'self_type=
              fun { return_value = _x }  -> let o = o#expression _x in o
            method required_modules : required_modules -> 'self_type=
              o#unknown
            method property_name : property_name -> 'self_type= o#unknown
            method property_map : property_map -> 'self_type=
              o#list
                (fun o  ->
                   fun (_x,_x_i1)  ->
                     let o = o#property_name _x in
                     let o = o#expression _x_i1 in o)
            method property : property -> 'self_type= o#unknown
            method program : program -> 'self_type=
              fun
                { name = _x; block = _x_i1; exports = _x_i2;
                  export_set = _x_i3 }
                 ->
                let o = o#string _x in
                let o = o#block _x_i1 in
                let o = o#exports _x_i2 in let o = o#unknown _x_i3 in o
            method number : number -> 'self_type= o#unknown
            method mutable_flag : mutable_flag -> 'self_type= o#unknown
            method length_object : length_object -> 'self_type= o#unknown
            method label : label -> 'self_type= o#string
            method kind : kind -> 'self_type= o#unknown
            method jsint : jsint -> 'self_type= o#unknown
            method int_op : int_op -> 'self_type= o#unknown
            method ident_info : ident_info -> 'self_type= o#unknown
            method ident : ident -> 'self_type= o#unknown
            method for_ident_expression : for_ident_expression -> 'self_type=
              o#expression
            method for_ident : for_ident -> 'self_type= o#ident
            method for_direction : for_direction -> 'self_type= o#unknown
            method finish_ident_expression :
              finish_ident_expression -> 'self_type= o#expression
            method expression_desc : expression_desc -> 'self_type=
              function
              | Math (_x,_x_i1) ->
                  let o = o#string _x in
                  let o = o#list (fun o  -> o#expression) _x_i1 in o
              | Length (_x,_x_i1) ->
                  let o = o#expression _x in
                  let o = o#length_object _x_i1 in o
              | Char_of_int _x -> let o = o#expression _x in o
              | Char_to_int _x -> let o = o#expression _x in o
              | Array_of_size _x -> let o = o#expression _x in o
              | Array_copy _x -> let o = o#expression _x in o
              | Array_append (_x,_x_i1) ->
                  let o = o#expression _x in let o = o#expression _x_i1 in o
              | String_append (_x,_x_i1) ->
                  let o = o#expression _x in let o = o#expression _x_i1 in o
              | Int_of_boolean _x -> let o = o#expression _x in o
              | Anything_to_number _x -> let o = o#expression _x in o
              | Bool _x -> let o = o#bool _x in o
              | Typeof _x -> let o = o#expression _x in o
              | Not _x -> let o = o#expression _x in o
              | String_of_small_int_array _x -> let o = o#expression _x in o
              | Json_stringify _x -> let o = o#expression _x in o
              | Anything_to_string _x -> let o = o#expression _x in o
              | Dump (_x,_x_i1) ->
                  let o = o#unknown _x in
                  let o = o#list (fun o  -> o#expression) _x_i1 in o
              | Seq (_x,_x_i1) ->
                  let o = o#expression _x in let o = o#expression _x_i1 in o
              | Cond (_x,_x_i1,_x_i2) ->
                  let o = o#expression _x in
                  let o = o#expression _x_i1 in
                  let o = o#expression _x_i2 in o
              | Bin (_x,_x_i1,_x_i2) ->
                  let o = o#binop _x in
                  let o = o#expression _x_i1 in
                  let o = o#expression _x_i2 in o
              | FlatCall (_x,_x_i1) ->
                  let o = o#expression _x in let o = o#expression _x_i1 in o
              | Bind (_x,_x_i1) ->
                  let o = o#expression _x in let o = o#expression _x_i1 in o
              | Call (_x,_x_i1,_x_i2) ->
                  let o = o#expression _x in
                  let o = o#list (fun o  -> o#expression) _x_i1 in
                  let o = o#unknown _x_i2 in o
              | String_access (_x,_x_i1) ->
                  let o = o#expression _x in let o = o#expression _x_i1 in o
              | Access (_x,_x_i1) ->
                  let o = o#expression _x in let o = o#expression _x_i1 in o
              | Dot (_x,_x_i1,_x_i2) ->
                  let o = o#expression _x in
                  let o = o#string _x_i1 in let o = o#bool _x_i2 in o
              | New (_x,_x_i1) ->
                  let o = o#expression _x in
                  let o =
                    o#option (fun o  -> o#list (fun o  -> o#expression))
                      _x_i1 in
                  o
              | Var _x -> let o = o#vident _x in o
              | Fun (_x,_x_i1,_x_i2) ->
                  let o = o#list (fun o  -> o#ident) _x in
                  let o = o#block _x_i1 in let o = o#unknown _x_i2 in o
              | Str (_x,_x_i1) ->
                  let o = o#bool _x in let o = o#string _x_i1 in o
              | Raw_js_code (_x,_x_i1) ->
                  let o = o#string _x in let o = o#code_info _x_i1 in o
              | Array (_x,_x_i1) ->
                  let o = o#list (fun o  -> o#expression) _x in
                  let o = o#mutable_flag _x_i1 in o
              | Caml_block (_x,_x_i1,_x_i2,_x_i3) ->
                  let o = o#list (fun o  -> o#expression) _x in
                  let o = o#mutable_flag _x_i1 in
                  let o = o#expression _x_i2 in let o = o#tag_info _x_i3 in o
              | Caml_uninitialized_obj (_x,_x_i1) ->
                  let o = o#expression _x in let o = o#expression _x_i1 in o
              | Caml_block_tag _x -> let o = o#expression _x in o
              | Caml_block_set_tag (_x,_x_i1) ->
                  let o = o#expression _x in let o = o#expression _x_i1 in o
              | Caml_block_set_length (_x,_x_i1) ->
                  let o = o#expression _x in let o = o#expression _x_i1 in o
              | Number _x -> let o = o#number _x in o
              | Object _x -> let o = o#property_map _x in o
            method expression : expression -> 'self_type=
              fun { expression_desc = _x; comment = _x_i1 }  ->
                let o = o#expression_desc _x in
                let o = o#option (fun o  -> o#string) _x_i1 in o
            method exports : exports -> 'self_type= o#unknown
            method exception_ident : exception_ident -> 'self_type= o#ident
            method deps_program : deps_program -> 'self_type=
              fun { program = _x; modules = _x_i1; side_effect = _x_i2 }  ->
                let o = o#program _x in
                let o = o#required_modules _x_i1 in
                let o = o#option (fun o  -> o#string) _x_i2 in o
            method code_info : code_info -> 'self_type= o#unknown
            method case_clause :
              'a .
                ('self_type -> 'a -> 'self_type) ->
                  'a case_clause -> 'self_type=
              fun _f_a  ->
                fun { case = _x; body = _x_i1 }  ->
                  let o = _f_a o _x in
                  let o =
                    (fun (_x,_x_i1)  ->
                       let o = o#block _x in let o = o#bool _x_i1 in o) _x_i1 in
                  o
            method block : block -> 'self_type=
              o#list (fun o  -> o#statement)
            method binop : binop -> 'self_type= o#unknown
            method unknown : 'a . 'a -> 'self_type= fun _  -> o
          end
      end
    module String_set : sig include (Set.S with type  elt =  string) end =
      struct include Set.Make(String) end 
    module Ext_bytes :
      sig
        [@@@ocaml.text
          " Port the {!Bytes.escaped} from trunk to make it not locale sensitive "]
        val escaped : bytes -> bytes
      end =
      struct
        external char_code : char -> int = "%identity"
        external char_chr : int -> char = "%identity"
        let escaped s =
          let n = ref 0 in
          for i = 0 to (Bytes.length s) - 1 do
            n :=
              ((!n) +
                 ((match Bytes.unsafe_get s i with
                   | '"'|'\\'|'\n'|'\t'|'\r'|'\b' -> 2
                   | ' '..'~' -> 1
                   | _ -> 4)))
          done;
          if (!n) = (Bytes.length s)
          then Bytes.copy s
          else
            (let s' = Bytes.create (!n) in
             n := 0;
             for i = 0 to (Bytes.length s) - 1 do
               ((match Bytes.unsafe_get s i with
                 | '"'|'\\' as c ->
                     (Bytes.unsafe_set s' (!n) '\\';
                      incr n;
                      Bytes.unsafe_set s' (!n) c)
                 | '\n' ->
                     (Bytes.unsafe_set s' (!n) '\\';
                      incr n;
                      Bytes.unsafe_set s' (!n) 'n')
                 | '\t' ->
                     (Bytes.unsafe_set s' (!n) '\\';
                      incr n;
                      Bytes.unsafe_set s' (!n) 't')
                 | '\r' ->
                     (Bytes.unsafe_set s' (!n) '\\';
                      incr n;
                      Bytes.unsafe_set s' (!n) 'r')
                 | '\b' ->
                     (Bytes.unsafe_set s' (!n) '\\';
                      incr n;
                      Bytes.unsafe_set s' (!n) 'b')
                 | ' '..'~' as c -> Bytes.unsafe_set s' (!n) c
                 | c ->
                     let a = char_code c in
                     (Bytes.unsafe_set s' (!n) '\\';
                      incr n;
                      Bytes.unsafe_set s' (!n) (char_chr (48 + (a / 100)));
                      incr n;
                      Bytes.unsafe_set s' (!n)
                        (char_chr (48 + ((a / 10) mod 10)));
                      incr n;
                      Bytes.unsafe_set s' (!n) (char_chr (48 + (a mod 10)))));
                incr n)
             done;
             s')
      end 
    module Ext_string :
      sig
        [@@@ocaml.text
          " Extension to the standard library [String] module, avoid locale sensitivity "]
        val split_by :
          ?keep_empty:bool -> (char -> bool) -> string -> string list
        [@@ocaml.doc " default is false "]
        val split : ?keep_empty:bool -> string -> char -> string list
        [@@ocaml.doc " default is false "]
        val starts_with : string -> string -> bool
        val ends_with : string -> string -> bool
        val escaped : string -> string
        val for_all : (char -> bool) -> string -> bool
        val is_empty : string -> bool
        val repeat : int -> string -> string
        val equal : string -> string -> bool
        val find : ?start:int -> sub:string -> string -> int
        val rfind : sub:string -> string -> int
        val tail_from : string -> int -> string
        val digits_of_str : string -> offset:int -> int -> int
        val starts_with_and_number : string -> offset:int -> string -> int
      end =
      struct
        let split_by ?(keep_empty= false)  is_delim str =
          let len = String.length str in
          let rec loop acc last_pos pos =
            if pos = (-1)
            then (String.sub str 0 last_pos) :: acc
            else
              if is_delim (str.[pos])
              then
                (let new_len = (last_pos - pos) - 1 in
                 if (new_len <> 0) || keep_empty
                 then
                   let v = String.sub str (pos + 1) new_len in
                   loop (v :: acc) pos (pos - 1)
                 else loop acc pos (pos - 1))
              else loop acc last_pos (pos - 1) in
          loop [] len (len - 1)
        let split ?keep_empty  str on =
          if str = ""
          then []
          else split_by ?keep_empty (fun x  -> (x : char) = on) str
        let starts_with s beg =
          let beg_len = String.length beg in
          let s_len = String.length s in
          (beg_len <= s_len) &&
            (let i = ref 0 in
             while
               ((!i) < beg_len) &&
                 ((String.unsafe_get s (!i)) = (String.unsafe_get beg (!i)))
               do incr i done;
             (!i) = beg_len)
        let ends_with s beg =
          let s_finish = (String.length s) - 1 in
          let s_beg = (String.length beg) - 1 in
          if s_beg > s_finish
          then false
          else
            (let rec aux j k =
               if k < 0
               then true
               else
                 if (String.unsafe_get s j) = (String.unsafe_get beg k)
                 then aux (j - 1) (k - 1)
                 else false in
             aux s_finish s_beg)
        let escaped s =
          let rec needs_escape i =
            if i >= (String.length s)
            then false
            else
              (match String.unsafe_get s i with
               | '"'|'\\'|'\n'|'\t'|'\r'|'\b' -> true
               | ' '..'~' -> needs_escape (i + 1)
               | _ -> true) in
          if needs_escape 0
          then
            Bytes.unsafe_to_string
              (Ext_bytes.escaped (Bytes.unsafe_of_string s))
          else s[@@ocaml.doc
                  "  In OCaml 4.02.3, {!String.escaped} is locale senstive, \n     this version try to make it not locale senstive, this bug is fixed\n     in the compiler trunk     \n"]
        let for_all (p : char -> bool) s =
          let len = String.length s in
          let rec aux i =
            if i >= len
            then true
            else (p (String.unsafe_get s i)) && (aux (i + 1)) in
          aux 0
        let is_empty s = (String.length s) = 0
        let repeat n s =
          let len = String.length s in
          let res = Bytes.create (n * len) in
          for i = 0 to pred n do String.blit s 0 res (i * len) len done;
          Bytes.to_string res
        let equal (x : string) y = x = y
        let _is_sub ~sub  i s j ~len  =
          let rec check k =
            if k = len
            then true
            else
              ((String.unsafe_get sub (i + k)) =
                 (String.unsafe_get s (j + k)))
                && (check (k + 1)) in
          ((j + len) <= (String.length s)) && (check 0)
        let find ?(start= 0)  ~sub  s =
          let n = String.length sub in
          let i = ref start in
          let module M = struct exception Exit end in
            try
              while ((!i) + n) <= (String.length s) do
                (if _is_sub ~sub 0 s (!i) ~len:n then raise M.Exit; incr i)
                done;
              (-1)
            with | M.Exit  -> !i
        let rfind ~sub  s =
          let n = String.length sub in
          let i = ref ((String.length s) - n) in
          let module M = struct exception Exit end in
            try
              while (!i) >= 0 do
                (if _is_sub ~sub 0 s (!i) ~len:n then raise M.Exit; decr i)
                done;
              (-1)
            with | M.Exit  -> !i
        let tail_from s x =
          let len = String.length s in
          if x > len
          then
            invalid_arg
              ("Ext_string.tail_from " ^ (s ^ (" : " ^ (string_of_int x))))
          else String.sub s x (len - x)
        let digits_of_str s ~offset  x =
          let rec aux i acc s x =
            if i >= x
            then acc
            else
              aux (i + 1) (((10 * acc) + (Char.code (s.[offset + i]))) - 48)
                s x in
          aux 0 0 s x[@@ocaml.doc
                       "\n   {[ \n     digits_of_str \"11_js\" 2 == 11     \n   ]}\n"]
        let starts_with_and_number s ~offset  beg =
          let beg_len = String.length beg in
          let s_len = String.length s in
          let finish_delim = offset + beg_len in
          if finish_delim > s_len
          then (-1)
          else
            (let i = ref offset in
             while
               ((!i) < finish_delim) &&
                 ((String.unsafe_get s (!i)) =
                    (String.unsafe_get beg ((!i) - offset)))
               do incr i done;
             if (!i) = finish_delim
             then digits_of_str ~offset:finish_delim s 2
             else (-1))
      end 
    module Js_config :
      sig
        type env =
          | Browser
          | NodeJS
          | AmdJS
          | Goog of string option
        val get_env : unit -> env
        val get_ext : unit -> string
        val get_goog_package_name : unit -> string option
        val set_env : env -> unit
        val cmd_set_module : string -> unit
        val default_gen_tds : bool ref
        val runtime_set : String_set.t
        val stdlib_set : String_set.t
        val block : string
        val int32 : string
        val gc : string
        val backtrace : string
        val prim : string
        val builtin_exceptions : string
        val exceptions : string
        val io : string
        val oo : string
        val sys : string
        val lexer : string
        val parser : string
        val obj_runtime : string
        val array : string
        val format : string
        val string : string
        val float : string
        val curry : string
        val bigarray : string
        val unix : string
        val int64 : string
        val md5 : string
        val hash : string
        val weak : string
        val js_primitive : string
      end =
      struct
        type env =
          | Browser
          | NodeJS
          | AmdJS
          | Goog of string option
        let default_env = ref NodeJS
        let ext = ref ".js"
        let get_ext () = !ext
        let get_env () = !default_env
        let set_env env = default_env := env
        let cmd_set_module str =
          match str with
          | "commonjs" -> default_env := NodeJS
          | "amdjs" -> default_env := AmdJS
          | "browser-internal" -> default_env := Browser
          | _ ->
              if Ext_string.starts_with str "goog"
              then
                let len = String.length str in
                (if len = 4
                 then (default_env := (Goog (Some "")); ext := ".g.js")
                 else
                   if ((str.[4]) = ':') && (len > 5)
                   then
                     (default_env :=
                        (Goog (Some (Ext_string.tail_from str 5)));
                      ext := ".g.js")
                   else
                     raise
                       (Arg.Bad
                          (Printf.sprintf "invalid module system %s" str)))
              else
                raise
                  (Arg.Bad (Printf.sprintf "invalid module system %s" str))
        let get_goog_package_name () =
          match !default_env with
          | Goog x -> x
          | Browser |AmdJS |NodeJS  -> None
        let default_gen_tds = ref false
        let stdlib_set =
          String_set.of_list
            ["arg";
            "gc";
            "printexc";
            "array";
            "genlex";
            "printf";
            "arrayLabels";
            "hashtbl";
            "queue";
            "buffer";
            "int32";
            "random";
            "bytes";
            "int64";
            "scanf";
            "bytesLabels";
            "lazy";
            "set";
            "callback";
            "lexing";
            "sort";
            "camlinternalFormat";
            "list";
            "stack";
            "camlinternalFormatBasics";
            "listLabels";
            "stdLabels";
            "camlinternalLazy";
            "map";
            "camlinternalMod";
            "marshal";
            "stream";
            "camlinternalOO";
            "moreLabels";
            "string";
            "char";
            "nativeint";
            "stringLabels";
            "complex";
            "obj";
            "sys";
            "digest";
            "oo";
            "weak";
            "filename";
            "parsing";
            "format";
            "pervasives"]
        let prim = "Caml_primitive"
        let builtin_exceptions = "Caml_builtin_exceptions"
        let exceptions = "Caml_exceptions"
        let io = "Caml_io"
        let sys = "Caml_sys"
        let lexer = "Caml_lexer"
        let parser = "Caml_parser"
        let obj_runtime = "Caml_obj"
        let array = "Caml_array"
        let format = "Caml_format"
        let string = "Caml_string"
        let float = "Caml_float"
        let hash = "Caml_hash"
        let oo = "Caml_oo"
        let curry = "Curry"
        let bigarray = "Caml_bigarray"
        let unix = "Caml_unix"
        let int64 = "Caml_int64"
        let md5 = "Caml_md5"
        let weak = "Caml_weak"
        let backtrace = "Caml_backtrace"
        let gc = "Caml_gc"
        let int32 = "Caml_int32"
        let block = "Block"
        let js_primitive = "Js_primitive"
        let runtime_set =
          [js_primitive;
          block;
          int32;
          gc;
          backtrace;
          prim;
          builtin_exceptions;
          exceptions;
          io;
          sys;
          lexer;
          parser;
          obj_runtime;
          array;
          format;
          string;
          float;
          hash;
          oo;
          curry;
          bigarray;
          unix;
          int64;
          md5;
          weak] |>
            (List.fold_left
               (fun acc  ->
                  fun x  -> String_set.add (String.uncapitalize x) acc)
               String_set.empty)
      end 
    module Ext_ident :
      sig
        [@@@ocaml.text " A wrapper around [Ident] module in compiler-libs"]
        val is_js : Ident.t -> bool
        val is_js_object : Ident.t -> bool
        val create_js : string -> Ident.t[@@ocaml.doc
                                           " create identifiers for predefined [js] global variables "]
        val create : string -> Ident.t
        val create_js_module : string -> Ident.t
        val make_js_object : Ident.t -> unit
        val reset : unit -> unit
        val gen_js : ?name:string -> unit -> Ident.t
        val make_unused : unit -> Ident.t
        val is_unused_ident : Ident.t -> bool
        val convert : string -> string
        val undefined : Ident.t
        val is_js_or_global : Ident.t -> bool
        val nil : Ident.t
      end =
      struct
        let js_flag = 8
        let js_module_flag = 16
        let js_object_flag = 32
        let is_js (i : Ident.t) = (i.flags land js_flag) <> 0
        let is_js_or_global (i : Ident.t) = (i.flags land (8 lor 1)) <> 0
        let is_js_module (i : Ident.t) = (i.flags land js_module_flag) <> 0
        let is_js_object (i : Ident.t) = (i.flags land js_object_flag) <> 0
        let make_js_object (i : Ident.t) =
          i.flags <- i.flags lor js_object_flag
        let create_js (name : string) =
          ({ name; flags = js_flag; stamp = 0 } : Ident.t)
        let js_module_table = Hashtbl.create 31
        let create_js_module (name : string) =
          (let name =
             (String.concat "") @@
               ((List.map String.capitalize) @@ (Ext_string.split name '-')) in
           match Hashtbl.find js_module_table name with
           | exception Not_found  ->
               let v = Ident.create name in
               let ans = { v with flags = js_module_flag } in
               (Hashtbl.add js_module_table name ans; ans)
           | v -> v : Ident.t)
        let create = Ident.create
        let gen_js ?(name= "$js")  () = create name
        let reserved_words =
          ["break";
          "case";
          "catch";
          "continue";
          "debugger";
          "default";
          "delete";
          "do";
          "else";
          "finally";
          "for";
          "function";
          "if";
          "in";
          "instanceof";
          "new";
          "return";
          "switch";
          "this";
          "throw";
          "try";
          "typeof";
          "var";
          "void";
          "while";
          "with";
          "class";
          "enum";
          "export";
          "extends";
          "import";
          "super";
          "implements";
          "interface";
          "let";
          "package";
          "private";
          "protected";
          "public";
          "static";
          "yield";
          "null";
          "true";
          "false";
          "NaN";
          "undefined";
          "this";
          "abstract";
          "boolean";
          "byte";
          "char";
          "const";
          "double";
          "final";
          "float";
          "goto";
          "int";
          "long";
          "native";
          "short";
          "synchronized";
          "transient";
          "volatile";
          "await";
          "event";
          "location";
          "window";
          "document";
          "eval";
          "navigator";
          "Array";
          "Date";
          "Math";
          "JSON";
          "Object";
          "RegExp";
          "String";
          "Boolean";
          "Number";
          "Map";
          "Set";
          "Infinity";
          "isFinite";
          "ActiveXObject";
          "XMLHttpRequest";
          "XDomainRequest";
          "DOMException";
          "Error";
          "SyntaxError";
          "arguments";
          "decodeURI";
          "decodeURIComponent";
          "encodeURI";
          "encodeURIComponent";
          "escape";
          "unescape";
          "isNaN";
          "parseFloat";
          "parseInt";
          "require";
          "exports";
          "module"]
        let reserved_map =
          List.fold_left (fun acc  -> fun x  -> String_set.add x acc)
            String_set.empty reserved_words
        let convert (name : string) =
          let module E = struct exception Not_normal_letter of int end in
            let len = String.length name in
            if String_set.mem name reserved_map
            then "$$" ^ name
            else
              (try
                 for i = 0 to len - 1 do
                   (let c = String.unsafe_get name i in
                    if
                      not
                        (((c >= 'a') && (c <= 'z')) ||
                           (((c >= 'A') && (c <= 'Z')) ||
                              ((c = '_') || (c = '$'))))
                    then raise (E.Not_normal_letter i)
                    else ())
                 done;
                 name
               with
               | E.Not_normal_letter i ->
                   (String.sub name 0 i) ^
                     (let buffer = Buffer.create len in
                      (for j = i to len - 1 do
                         (let c = String.unsafe_get name j in
                          match c with
                          | '*' -> Buffer.add_string buffer "$star"
                          | '\'' -> Buffer.add_string buffer "$prime"
                          | '!' -> Buffer.add_string buffer "$bang"
                          | '>' -> Buffer.add_string buffer "$great"
                          | '<' -> Buffer.add_string buffer "$less"
                          | '=' -> Buffer.add_string buffer "$eq"
                          | '+' -> Buffer.add_string buffer "$plus"
                          | '-' -> Buffer.add_string buffer "$neg"
                          | '@' -> Buffer.add_string buffer "$at"
                          | '^' -> Buffer.add_string buffer "$caret"
                          | '/' -> Buffer.add_string buffer "$slash"
                          | '|' -> Buffer.add_string buffer "$pipe"
                          | '.' -> Buffer.add_string buffer "$dot"
                          | '%' -> Buffer.add_string buffer "$percent"
                          | '~' -> Buffer.add_string buffer "$tilde"
                          | 'a'..'z'|'A'..'Z'|'_'|'$'|'0'..'9' ->
                              Buffer.add_char buffer c
                          | _ -> Buffer.add_string buffer "$unknown")
                       done;
                       Buffer.contents buffer)))
        let make_unused () = create "_"
        let is_unused_ident i = (Ident.name i) = "_"
        let reset () = Hashtbl.clear js_module_table
        let undefined = create_js "undefined"
        let nil = create_js "null"
      end 
    module Js_fold_basic :
      sig
        [@@@ocaml.text
          " A module to calculate hard dependency based on JS IR in module [J] "]
        val depends_j : J.expression -> Ident_set.t -> Ident_set.t
        val calculate_hard_dependencies :
          J.block -> Lam_module_ident.t Hash_set.hashset
      end =
      struct
        class count_deps (add : Ident.t -> unit) =
          object (self)
            inherit  Js_fold.fold as super
            method! expression lam =
              match lam.expression_desc with
              | Fun (_,block,_) -> self#block block
              | _ -> super#expression lam
            method! ident x = add x; self
          end
        class count_hard_dependencies =
          object (self)
            inherit  Js_fold.fold as super
            val hard_dependencies = Hash_set.create 17
            method! vident vid =
              match vid with
              | Qualified (id,kind,_) ->
                  (Hash_set.add hard_dependencies
                     (Lam_module_ident.mk kind id);
                   self)
              | Id id -> self
            method! expression x =
              match x with
              | { expression_desc = Call (_,_,{ arity = NA  });_} ->
                  (Hash_set.add hard_dependencies
                     (Lam_module_ident.of_runtime
                        (Ext_ident.create_js Js_config.curry));
                   super#expression x)
              | { expression_desc = Caml_block (_,_,tag,tag_info);_} ->
                  ((match ((tag.expression_desc), tag_info) with
                    | (Number (Int
                       { i = 0l;_}),(Blk_tuple |Blk_array |Blk_variant _
                                     |Blk_record _|Blk_na |Blk_module _
                                     |Blk_constructor (_,1)))
                        -> ()
                    | (_,_) ->
                        Hash_set.add hard_dependencies
                          (Lam_module_ident.of_runtime
                             (Ext_ident.create_js Js_config.block)));
                   super#expression x)
              | _ -> super#expression x
            method get_hard_dependencies = hard_dependencies
          end
        let calculate_hard_dependencies block =
          ((new count_hard_dependencies)#block block)#get_hard_dependencies
        let depends_j (lam : J.expression) (variables : Ident_set.t) =
          let v = ref Ident_set.empty in
          let add id =
            if Ident_set.mem id variables then v := (Ident_set.add id (!v)) in
          ignore @@ (((new count_deps) add)#expression lam); !v
      end 
    module Ext_filename :
      sig
        [@@@ocaml.text
          " An extension module to calculate relative path follow node/npm style. \n    TODO : this short name will have to change upon renaming the file.\n "]
        [@@@ocaml.text
          " Js_output is node style, which means \n    separator is only '/'\n    TODO: handle [node_modules]\n "]
        val node_relative_path : string -> string -> string[@@ocaml.doc
                                                             " TODO Change the module name, this code is not really an extension of the standard \n    library but rather specific to JS Module name convention. \n  "]
        val chop_extension : ?loc:string -> string -> string
      end =
      struct
        let node_sep = "/"[@@ocaml.doc
                            " Used when produce node compatible paths "]
        let node_parent = ".."
        let node_current = "."
        let absolute_path s =
          let s =
            if Filename.is_relative s
            then Filename.concat (Sys.getcwd ()) s
            else s in
          let rec aux s =
            let base = Filename.basename s in
            let dir = Filename.dirname s in
            if dir = s
            then dir
            else
              if base = Filename.current_dir_name
              then aux dir
              else
                if base = Filename.parent_dir_name
                then Filename.dirname (aux dir)
                else Filename.concat (aux dir) base in
          aux s
        let chop_extension ?(loc= "")  name =
          try Filename.chop_extension name
          with
          | Invalid_argument _ ->
              invalid_arg
                ("Filename.chop_extension (" ^ (loc ^ (":" ^ (name ^ ")"))))
        let try_chop_extension s =
          try Filename.chop_extension s with | _ -> s
        let relative_path file1 file2 =
          let dir1 =
            Ext_string.split (Filename.dirname file1) (Filename.dir_sep.[0]) in
          let dir2 =
            Ext_string.split (Filename.dirname file2) (Filename.dir_sep.[0]) in
          let rec go (dir1 : string list) (dir2 : string list) =
            match (dir1, dir2) with
            | (x::xs,y::ys) when x = y -> go xs ys
            | (_,_) -> (List.map (fun _  -> node_parent) dir2) @ dir1 in
          match go dir1 dir2 with
          | x::_ as ys when x = node_parent -> String.concat node_sep ys
          | ys -> (String.concat node_sep) @@ (node_current :: ys)[@@ocaml.doc
                                                                    " example\n    {[\n    \"/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj\"\n    \"/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml\"\n    ]}\n\n    The other way\n    {[\n    \n    \"/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml\"\n    \"/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj\"\n    ]}\n    {[\n    \"/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib//ocaml_array.ml\"\n    ]}\n    {[\n    /a/b\n    /c/d\n    ]}\n "]
        let node_relative_path path1 path2 =
          (relative_path (try_chop_extension (absolute_path path2))
             (try_chop_extension (absolute_path path1)))
            ^ (node_sep ^ (try_chop_extension (Filename.basename path2)))
          [@@ocaml.doc
            " path2: a/b \n    path1: a \n    result:  ./b \n    TODO: [Filename.concat] with care\n "]
      end 
    module Lam_util :
      sig
        val string_of_lambda : Lambda.lambda -> string
        val string_of_primitive : Lambda.primitive -> string
        val kind_of_lambda_block :
          Lam_stats.boxed_nullable -> Lambda.lambda list -> Lam_stats.kind
        val get :
          Lambda.lambda ->
            Ident.t -> int -> Lam_stats.ident_tbl -> Lambda.lambda
        val add_required_module : Ident.t -> Lam_stats.meta -> unit
        val add_required_modules : Ident.t list -> Lam_stats.meta -> unit
        val alias :
          Lam_stats.meta ->
            Ident.t -> Ident.t -> Lam_stats.kind -> Lambda.let_kind -> unit
        val refine_let :
          ?kind:Lambda.let_kind ->
            Ident.t -> Lambda.lambda -> Lambda.lambda -> Lambda.lambda
        val generate_label : ?name:string -> unit -> J.label
        val sort_dag_args : J.expression Ident_map.t -> Ident.t list option
        [@@ocaml.doc
          " if [a] depends on [b] a is ahead of [b] as [a::b]\n\n    TODO: make it a stable sort \n "]
        val dump : Env.t -> string -> Lambda.lambda -> Lambda.lambda[@@ocaml.doc
                                                                    " [dump] when {!Lam_current_unit.is_same_file}"]
        val ident_set_of_list : Ident.t list -> Ident_set.t
        val print_ident_set : Format.formatter -> Ident_set.t -> unit
        val mk_apply_info :
          ?loc:Location.t -> Lambda.apply_status -> Lambda.apply_info
        val lam_true : Lambda.lambda
        val lam_false : Lambda.lambda
        val lam_unit : Lambda.lambda
        val not_function : Lambda.lambda -> bool
        val is_function : Lambda.lambda -> bool
        val eta_conversion :
          int ->
            Lambda.apply_info ->
              Lambda.lambda -> Lambda.lambda list -> Lambda.lambda
        val default_apply_info : Lambda.apply_info
        val js_is_nil_primitive : Lambda.primitive
        val js_is_undef_primitive : Lambda.primitive
        val js_is_nil_undef_primitive : Lambda.primitive
      end =
      struct
        let string_of_lambda = Format.asprintf "%a" Printlambda.lambda
        let string_of_primitive = Format.asprintf "%a" Printlambda.primitive
        exception Cyclic
        let toplogical (get_deps : Ident.t -> Ident_set.t)
          (libs : Ident.t list) =
          (let rec aux acc later todo round_progress =
             match (todo, later) with
             | ([],[]) -> acc
             | ([],_) ->
                 if round_progress
                 then aux acc todo later false
                 else raise Cyclic
             | (x::xs,_) ->
                 if
                   Ident_set.for_all
                     (fun dep  -> (x == dep) || (List.mem dep acc))
                     (get_deps x)
                 then aux (x :: acc) later xs true
                 else aux acc (x :: later) xs round_progress in
           let (starts,todo) =
             List.partition
               (fun lib  -> Ident_set.is_empty @@ (get_deps lib)) libs in
           aux starts [] todo false : Ident.t list)
        let sort_dag_args param_args =
          let todos = Ident_map.keys param_args in
          let idents = Ident_set.of_list todos in
          let dependencies: Ident_set.t Ident_map.t =
            Ident_map.mapi
              (fun param  -> fun arg  -> Js_fold_basic.depends_j arg idents)
              param_args in
          try
            Some (toplogical (fun k  -> Ident_map.find k dependencies) todos)
          with | Cyclic  -> None
        let add_required_module (x : Ident.t) (meta : Lam_stats.meta) =
          if not @@ (Ident.is_predef_exn x)
          then
            meta.required_modules <- (Lam_module_ident.of_ml x) ::
              (meta.required_modules)
        let add_required_modules (x : Ident.t list) (meta : Lam_stats.meta) =
          let required_modules =
            (Ext_list.filter_map
               (fun x  ->
                  if Ident.is_predef_exn x
                  then None
                  else Some (Lam_module_ident.of_ml x)) x)
              @ meta.required_modules in
          meta.required_modules <- required_modules
        let subst_lambda s lam =
          let rec subst (x : Lambda.lambda) =
            match x with
            | Lvar id as l ->
                (try Ident_map.find id s with | Not_found  -> l)
            | Lconst sc as l -> l
            | Lapply (fn,args,loc) ->
                Lapply ((subst fn), (List.map subst args), loc)
            | Lfunction (kind,params,body) ->
                Lfunction (kind, params, (subst body))
            | Llet (str,id,arg,body) ->
                Llet (str, id, (subst arg), (subst body))
            | Lletrec (decl,body) ->
                Lletrec ((List.map subst_decl decl), (subst body))
            | Lprim (p,args) -> Lprim (p, (List.map subst args))
            | Lswitch (arg,sw) ->
                Lswitch
                  ((subst arg),
                    {
                      sw with
                      sw_consts = (List.map subst_case sw.sw_consts);
                      sw_blocks = (List.map subst_case sw.sw_blocks);
                      sw_failaction = (subst_opt sw.sw_failaction)
                    })
            | Lstringswitch (arg,cases,default) ->
                Lam_comb.stringswitch (subst arg)
                  (List.map subst_strcase cases) (subst_opt default)
            | Lstaticraise (i,args) ->
                Lstaticraise (i, (List.map subst args))
            | Lstaticcatch (e1,io,e2) ->
                Lstaticcatch ((subst e1), io, (subst e2))
            | Ltrywith (e1,exn,e2) -> Ltrywith ((subst e1), exn, (subst e2))
            | Lifthenelse (e1,e2,e3) ->
                Lam_comb.if_ (subst e1) (subst e2) (subst e3)
            | Lsequence (e1,e2) -> Lsequence ((subst e1), (subst e2))
            | Lwhile (e1,e2) -> Lwhile ((subst e1), (subst e2))
            | Lfor (v,e1,e2,dir,e3) ->
                Lfor (v, (subst e1), (subst e2), dir, (subst e3))
            | Lassign (id,e) -> Lassign (id, (subst e))
            | Lsend (k,met,obj,args,loc) ->
                Lsend
                  (k, (subst met), (subst obj), (List.map subst args), loc)
            | Levent (lam,evt) -> Levent ((subst lam), evt)
            | Lifused (v,e) -> Lifused (v, (subst e))
          and subst_decl (id,exp) = (id, (subst exp))
          and subst_case (key,case) = (key, (subst case))
          and subst_strcase (key,case) = (key, (subst case))
          and subst_opt = function | None  -> None | Some e -> Some (subst e) in
          subst lam
        let refine_let ?kind  param (arg : Lambda.lambda) (l : Lambda.lambda)
          =
          (match ((kind : Lambda.let_kind option), arg, l) with
           | (_,_,Lvar w) when Ident.same w param -> arg
           | (_,_,Lprim (fn,(Lvar w)::[])) when
               (Ident.same w param) &&
                 ((function | Lambda.Pmakeblock _ -> false | _ -> true) fn)
               -> Lprim (fn, [arg])
           | (_,_,Lapply (fn,(Lvar w)::[],info)) when Ident.same w param ->
               Lapply (fn, [arg], info)
           | ((Some (Strict |StrictOpt )|None ),(Lvar _|Lconst _|Lprim
                                                 (Pfield _,(Lprim
                                                  (Pgetglobal _,[]))::[])),_)
               -> Llet (Alias, param, arg, l)
           | ((Some (Strict |StrictOpt )|None ),Lfunction _,_) ->
               Llet (StrictOpt, param, arg, l)
           | (Some (Strict ),_,_) when Lam_analysis.no_side_effects arg ->
               Llet (StrictOpt, param, arg, l)
           | (Some (Variable ),_,_) -> Llet (Variable, param, arg, l)
           | (Some kind,_,_) -> Llet (kind, param, arg, l)
           | (None ,_,_) -> Llet (Strict, param, arg, l) : Lambda.lambda)
        let alias (meta : Lam_stats.meta) (k : Ident.t) (v : Ident.t)
          (v_kind : Lam_stats.kind) (let_kind : Lambda.let_kind) =
          (match v_kind with
           | NA  ->
               (match Hashtbl.find meta.ident_tbl v with
                | exception Not_found  -> ()
                | ident_info -> Hashtbl.add meta.ident_tbl k ident_info)
           | ident_info -> Hashtbl.add meta.ident_tbl k ident_info);
          (match let_kind with
           | Alias  ->
               if not @@ (Ident_set.mem k meta.export_idents)
               then Hashtbl.add meta.alias_tbl k v
           | Strict |StrictOpt |Variable  -> ())
        let element_of_lambda (lam : Lambda.lambda) =
          (match lam with
           | Lvar _|Lconst _|Lprim (Pfield _,(Lprim (Pgetglobal _,[]))::[])
               -> SimpleForm lam
           | _ -> NA : Lam_stats.element)
        let kind_of_lambda_block kind (xs : Lambda.lambda list) =
          ((xs |> (List.map element_of_lambda)) |>
             (fun ls  -> Lam_stats.ImmutableBlock ((Array.of_list ls), kind)) : 
          Lam_stats.kind)
        let get lam v i tbl =
          (match (Hashtbl.find tbl v : Lam_stats.kind) with
           | Module g ->
               Lprim
                 ((Pfield (i, Lambda.Fld_na)), [Lprim ((Pgetglobal g), [])])
           | ImmutableBlock (arr,_) ->
               (match arr.(i) with | NA  -> lam | SimpleForm l -> l)
           | Constant (Const_block (_,_,ls)) -> Lconst (List.nth ls i)
           | _ -> lam
           | exception Not_found  -> lam : Lambda.lambda)
        let count = ref 0
        let generate_label ?(name= "")  () =
          incr count; Printf.sprintf "%s_tailcall_%04d" name (!count)
        let log_counter = ref 0
        let dump env ext lam =
          incr log_counter;
          if
            ((Js_config.get_env ()) <> Browser) &&
              (Lam_current_unit.is_same_file ())
          then
            Printlambda.seriaize env
              ((Ext_filename.chop_extension ~loc:__LOC__
                  (Lam_current_unit.get_file ()))
                 ^ (Printf.sprintf ".%02d%s.lam" (!log_counter) ext)) lam;
          lam
        let ident_set_of_list ls =
          List.fold_left (fun acc  -> fun k  -> Ident_set.add k acc)
            Ident_set.empty ls
        let print_ident_set fmt s =
          Format.fprintf fmt "@[<v>{%a}@]@."
            (fun fmt  ->
               fun s  ->
                 Ident_set.iter
                   (fun e  -> Format.fprintf fmt "@[<v>%a@],@ " Ident.print e)
                   s) s
        let mk_apply_info ?(loc= Location.none)  apply_status =
          ({ apply_loc = loc; apply_status } : Lambda.apply_info)
        let lam_true: Lambda.lambda =
          Lconst (Const_pointer (1, (Pt_constructor "true")))
        let lam_false: Lambda.lambda =
          Lconst (Const_pointer (0, (Pt_constructor "false")))
        let lam_unit: Lambda.lambda =
          Lconst (Const_pointer (0, (Pt_constructor "()")))
        let is_function (lam : Lambda.lambda) =
          match lam with | Lfunction _ -> true | _ -> false
        let not_function (lam : Lambda.lambda) =
          match lam with | Lfunction _ -> false | _ -> true
        let lapply (fn : Lambda.lambda) args info =
          Lambda.Lapply (fn, args, info)
        let eta_conversion n info fn args =
          let extra_args =
            Ext_list.init n (fun _  -> Ident.create Literals.param) in
          let extra_lambdas = List.map (fun x  -> Lambda.Lvar x) extra_args in
          match List.fold_right
                  (fun lam  ->
                     fun (acc,bind)  ->
                       match lam with
                       | Lambda.Lvar _|Lconst
                         (Const_base _|Const_pointer _|Const_immstring _)
                         |Lprim
                         (Lambda.Pfield _,(Lprim
                          (Lambda.Pgetglobal _,_))::[])|Lfunction _ ->
                           ((lam :: acc), bind)
                       | _ ->
                           let v = Ident.create Literals.partial_arg in
                           (((Lambda.Lvar v) :: acc), ((v, lam) :: bind)))
                  (fn :: args) ([], [])
          with
          | (fn::args,bindings) ->
              let rest: Lambda.lambda =
                Lfunction
                  (Curried, extra_args,
                    (lapply fn (args @ extra_lambdas) info)) in
              List.fold_left
                (fun lam  -> fun (id,x)  -> Lambda.Llet (Strict, id, x, lam))
                rest bindings
          | (_,_) -> assert false
        let default_apply_info: Lambda.apply_info =
          { apply_status = App_na; apply_loc = Location.none }
        let js_is_nil_primitive =
          Lambda.Pccall
            {
              prim_name = "js_is_nil";
              prim_arity = 1;
              prim_alloc = false;
              prim_native_name = "js_is_nil";
              prim_native_float = false;
              prim_attributes = [];
              prim_ty = None
            }
        let js_is_undef_primitive =
          Lambda.Pccall
            {
              prim_name = "js_is_undef";
              prim_arity = 1;
              prim_alloc = false;
              prim_native_name = "js_is_undef";
              prim_native_float = false;
              prim_attributes = [];
              prim_ty = None
            }
        let js_is_nil_undef_primitive =
          Lambda.Pccall
            {
              prim_name = "js_is_nil_undef";
              prim_arity = 1;
              prim_alloc = false;
              prim_native_name = "js_is_nil_undef";
              prim_native_float = false;
              prim_attributes = [];
              prim_ty = None
            }
      end 
    module Js_number :
      sig
        type t = float
        val to_string : t -> string
        val caml_float_literal_to_js_string : string -> string
      end =
      struct
        type t = float
        let to_string v =
          if v = infinity
          then "Infinity"
          else
            if v = neg_infinity
            then "-Infinity"
            else
              if v <> v
              then "NaN"
              else
                (let vint = int_of_float v in
                 if (float_of_int vint) = v
                 then string_of_int vint
                 else
                   (let s1 = Printf.sprintf "%.12g" v in
                    if v = (float_of_string s1)
                    then s1
                    else
                      (let s2 = Printf.sprintf "%.15g" v in
                       if v = (float_of_string s2)
                       then s2
                       else Printf.sprintf "%.18g" v)))
        let caml_float_literal_to_js_string v =
          let len = String.length v in
          if
            (len >= 2) &&
              (((v.[0]) = '0') && (((v.[1]) = 'x') || ((v.[1]) = 'X')))
          then assert false
          else
            (let rec aux buf i =
               if i >= len
               then buf
               else
                 (let x = v.[i] in
                  if x = '_'
                  then aux buf (i + 1)
                  else
                    if (x = '.') && (i = (len - 1))
                    then buf
                    else (Buffer.add_char buf x; aux buf (i + 1))) in
             Buffer.contents (aux (Buffer.create len) 0))
      end 
    module Lam_group :
      sig
        type t =
          | Single of Lambda.let_kind* Ident.t* Lambda.lambda
          | Recursive of (Ident.t* Lambda.lambda) list
          | Nop of Lambda.lambda
        val flatten : t list -> Lambda.lambda -> (Lambda.lambda* t list)
        val lambda_of_groups : Lambda.lambda -> t list -> Lambda.lambda
        val deep_flatten : Lambda.lambda -> Lambda.lambda[@@ocaml.doc
                                                           " Tricky to be complete "]
        val pp_group : Env.t -> Format.formatter -> t -> unit
      end =
      struct
        type t =
          | Single of Lambda.let_kind* Ident.t* Lambda.lambda
          | Recursive of (Ident.t* Lambda.lambda) list
          | Nop of Lambda.lambda
        let pp = Format.fprintf
        let str_of_kind (kind : Lambda.let_kind) =
          match kind with
          | Alias  -> "a"
          | Strict  -> ""
          | StrictOpt  -> "o"
          | Variable  -> "v"
        let pp_group env fmt (x : t) =
          match x with
          | Single (kind,id,lam) ->
              Format.fprintf fmt "@[let@ %a@ =%s@ @[<hv>%a@]@ @]" Ident.print
                id (str_of_kind kind) (Printlambda.env_lambda env) lam
          | Recursive lst ->
              List.iter
                (fun (id,lam)  ->
                   Format.fprintf fmt "@[let %a@ =r@ %a@ @]" Ident.print id
                     (Printlambda.env_lambda env) lam) lst
          | Nop lam -> Printlambda.env_lambda env fmt lam
        let rec flatten (acc : t list) (lam : Lambda.lambda) =
          (match lam with
           | Levent (e,_) -> flatten acc e
           | Llet (str,id,arg,body) ->
               let (res,l) = flatten acc arg in
               flatten ((Single (str, id, res)) :: l) body
           | Lletrec (bind_args,body) ->
               flatten
                 ((Recursive
                     (List.map (fun (id,arg)  -> (id, arg)) bind_args)) ::
                 acc) body
           | Lsequence (l,r) ->
               let (res,l) = flatten acc l in flatten ((Nop res) :: l) r
           | x -> (x, acc) : (Lambda.lambda* t list))
        let lambda_of_groups result groups =
          List.fold_left
            (fun acc  ->
               fun x  ->
                 match x with
                 | Nop l -> Lambda.Lsequence (l, acc)
                 | Single (kind,ident,lam) ->
                     Lam_util.refine_let ~kind ident lam acc
                 | Recursive bindings -> Lletrec (bindings, acc)) result
            groups
        let deep_flatten (lam : Lambda.lambda) =
          (let rec flatten (acc : t list) (lam : Lambda.lambda) =
             (match lam with
              | Levent (e,_) -> flatten acc e
              | Llet
                  (str,id,(Lprim
                             (Pccall
                              {
                                prim_name =
                                  ("js_from_nullable"|"js_from_def"
                                   |"js_from_nullable_def");_},(Lvar
                              _)::[])
                             as arg),body)
                  -> flatten ((Single (str, id, (aux arg))) :: acc) body
              | Llet
                  (str,id,Lprim
                   (Pccall
                    ({
                       prim_name =
                         ("js_from_nullable"|"js_from_def"
                          |"js_from_nullable_def");_}
                       as p),arg::[]),body)
                  ->
                  let id' = Ident.rename id in
                  flatten acc
                    (Llet
                       (str, id', arg,
                         (Llet
                            (Alias, id, (Lprim ((Pccall p), [Lvar id'])),
                              body))))
              | Llet (str,id,arg,body) ->
                  let (res,l) = flatten acc arg in
                  flatten ((Single (str, id, res)) :: l) body
              | Lletrec (bind_args,body) ->
                  flatten
                    ((Recursive
                        (List.map (fun (id,arg)  -> (id, (aux arg)))
                           bind_args)) :: acc) body
              | Lsequence (l,r) ->
                  let (res,l) = flatten acc l in flatten ((Nop res) :: l) r
              | x -> ((aux x), acc) : (Lambda.lambda* t list))
           and aux (lam : Lambda.lambda) =
             (match lam with
              | Levent (e,_) -> aux e
              | Llet _ ->
                  let (res,groups) = flatten [] lam in
                  lambda_of_groups res groups
              | Lletrec (bind_args,body) ->
                  let module Ident_set = Lambda.IdentSet in
                    let rec iter bind_args acc =
                      match bind_args with
                      | [] -> acc
                      | (id,arg)::rest ->
                          let (groups,set) = acc in
                          let (res,groups) = flatten groups (aux arg) in
                          iter rest
                            (((Recursive [(id, res)]) :: groups),
                              (Ident_set.add id set)) in
                    let (groups,collections) =
                      iter bind_args ([], Ident_set.empty) in
                    let (result,_,wrap) =
                      List.fold_left
                        (fun (acc,set,wrap)  ->
                           fun g  ->
                             match g with
                             | Recursive ((id,Lconst _)::[])|Single
                               (Alias ,id,Lconst _)|Single
                               ((Alias |Strict |StrictOpt ),id,Lfunction _)
                                 -> (acc, set, (g :: wrap))
                             | Single (_,id,Lvar bid) ->
                                 (acc,
                                   (if Ident_set.mem bid set
                                    then Ident_set.add id set
                                    else set), (g :: wrap))
                             | Single (_,id,lam) ->
                                 let variables = Lambda.free_variables lam in
                                 if
                                   let open Ident_set in
                                     is_empty (inter variables collections)
                                 then (acc, set, (g :: wrap))
                                 else
                                   (((id, lam) :: acc),
                                     (Ident_set.add id set), wrap)
                             | Recursive us ->
                                 ((us @ acc),
                                   (List.fold_left
                                      (fun acc  ->
                                         fun (id,_)  -> Ident_set.add id acc)
                                      set us), wrap)
                             | Nop _ -> assert false) ([], collections, [])
                        groups in
                    lambda_of_groups (Lletrec (result, (aux body)))
                      (List.rev wrap)
              | Lsequence (l,r) -> Lsequence ((aux l), (aux r))
              | Lconst _ -> lam
              | Lvar _ -> lam
              | Lapply (l1,ll,info) ->
                  Lapply ((aux l1), (List.map aux ll), info)
              | Lprim (Pidentity ,l::[]) -> l
              | Lprim
                  (Pccall { prim_name = "caml_int64_float_of_bits";_},(Lconst
                   (Const_base (Const_int64 i)))::[])
                  ->
                  Lconst
                    (Const_base
                       (Const_float
                          (Js_number.to_string (Int64.float_of_bits i))))
              | Lprim
                  (Pccall { prim_name = "caml_int64_to_float";_},(Lconst
                   (Const_base (Const_int64 i)))::[])
                  ->
                  Lconst
                    (Const_base
                       (Const_float (Js_number.to_string (Int64.to_float i))))
              | Lprim (p,ll) ->
                  let ll = List.map aux ll in
                  (match (p, ll) with
                   | (Prevapply loc,x::(Lapply (f,args,_))::[])
                     |(Prevapply loc,x::(Levent (Lapply (f,args,_),_))::[])
                       ->
                       Lapply
                         (f, (args @ [x]),
                           (Lambda.default_apply_info ~loc ()))
                   | (Prevapply loc,x::f::[]) ->
                       Lapply (f, [x], (Lambda.default_apply_info ~loc ()))
                   | (Pdirapply loc,(Lapply (f,args,_))::x::[])
                     |(Pdirapply loc,(Levent (Lapply (f,args,_),_))::x::[])
                       ->
                       Lapply
                         (f, (args @ [x]),
                           (Lambda.default_apply_info ~loc ()))
                   | (Pdirapply loc,f::x::[]) ->
                       Lapply (f, [x], (Lambda.default_apply_info ~loc ()))
                   | _ -> Lprim (p, ll))
              | Lfunction (kind,params,l) ->
                  Lfunction (kind, params, (aux l))
              | Lswitch
                  (l,{ sw_failaction; sw_consts; sw_blocks; sw_numblocks;
                       sw_numconsts })
                  ->
                  Lswitch
                    ((aux l),
                      {
                        sw_consts =
                          (List.map (fun (v,l)  -> (v, (aux l))) sw_consts);
                        sw_blocks =
                          (List.map (fun (v,l)  -> (v, (aux l))) sw_blocks);
                        sw_numconsts;
                        sw_numblocks;
                        sw_failaction =
                          ((match sw_failaction with
                            | None  -> None
                            | Some x -> Some (aux x)))
                      })
              | Lstringswitch (l,sw,d) ->
                  Lam_comb.stringswitch (aux l)
                    (List.map (fun (i,l)  -> (i, (aux l))) sw)
                    (match d with | Some d -> Some (aux d) | None  -> None)
              | Lstaticraise (i,ls) -> Lstaticraise (i, (List.map aux ls))
              | Lstaticcatch (l1,(i,x),l2) ->
                  Lstaticcatch ((aux l1), (i, x), (aux l2))
              | Ltrywith (l1,v,l2) -> Ltrywith ((aux l1), v, (aux l2))
              | Lifthenelse (l1,l2,l3) ->
                  Lam_comb.if_ (aux l1) (aux l2) (aux l3)
              | Lwhile (l1,l2) -> Lwhile ((aux l1), (aux l2))
              | Lfor (flag,l1,l2,dir,l3) ->
                  Lfor (flag, (aux l1), (aux l2), dir, (aux l3))
              | Lassign (v,l) -> Lassign (v, (aux l))
              | Lsend (u,m,o,ll,v) ->
                  Lsend (u, (aux m), (aux o), (List.map aux ll), v)
              | Lifused (v,l) -> Lifused (v, (aux l)) : Lambda.lambda) in
           aux lam : Lambda.lambda)
      end 
    module Ext_log :
      sig
        [@@@ocaml.text
          " A Poor man's logging utility\n    \n    Example:\n    {[ \n    err __LOC__ \"xx\"\n    ]}\n "]
        type 'a logging =
          ('a,Format.formatter,unit,unit,unit,unit) format6 -> 'a
        val err : string -> 'a logging
        val ierr : bool -> string -> 'a logging
        val warn : string -> 'a logging
        val iwarn : bool -> string -> 'a logging
        val dwarn : string -> 'a logging
        val info : string -> 'a logging
        val iinfo : bool -> string -> 'a logging
      end =
      struct
        type 'a logging =
          ('a,Format.formatter,unit,unit,unit,unit) format6 -> 'a
        let err str f =
          Format.fprintf Format.err_formatter ("%s " ^^ (f ^^ "@.")) str
        let ierr b str f =
          if b
          then Format.fprintf Format.err_formatter ("%s " ^^ f) str
          else Format.ifprintf Format.err_formatter ("%s " ^^ f) str
        let warn str f =
          Format.fprintf Format.err_formatter ("WARN: %s " ^^ (f ^^ "@."))
            str
        let iwarn b str f =
          if b
          then Format.fprintf Format.err_formatter ("WARN: %s " ^^ f) str
          else Format.ifprintf Format.err_formatter ("WARN: %s " ^^ f) str
        let dwarn str f =
          if Lam_current_unit.is_same_file ()
          then
            Format.fprintf Format.err_formatter ("WARN: %s " ^^ (f ^^ "@."))
              str
          else
            Format.ifprintf Format.err_formatter ("WARN: %s " ^^ (f ^^ "@."))
              str
        let info str f =
          Format.fprintf Format.err_formatter ("INFO: %s " ^^ f) str
        let iinfo b str f =
          if b
          then Format.fprintf Format.err_formatter ("INFO: %s " ^^ f) str
          else Format.fprintf Format.err_formatter ("INFO: %s " ^^ f) str
      end 
    module Idents_analysis :
      sig
        [@@@ocaml.text
          " A simple algorithm to calcuate [used] idents given its dependencies and \n    initial list.\n\n    TODO needs improvement\n "]
        val calculate_used_idents :
          (Ident.t,Lambda.IdentSet.t) Hashtbl.t ->
            Ident.t list -> Lambda.IdentSet.t
      end =
      struct
        module Ident_set = Lambda.IdentSet
        let calculate_used_idents
          (ident_free_vars : (Ident.t,Ident_set.t) Hashtbl.t)
          (initial_idents : Ident.t list) =
          let s = Ident_set.of_list initial_idents in
          let current_ident_sets = ref s in
          let delta = ref s in
          while
            let open Ident_set in
              (delta :=
                 (diff
                    (fold
                       (fun id  ->
                          fun acc  ->
                            if Ext_ident.is_js_or_global id
                            then acc
                            else
                              union acc
                                (match Hashtbl.find ident_free_vars id with
                                 | exception Not_found  ->
                                     (Ext_log.err __LOC__
                                        "%s/%d when compiling %s" id.name
                                        id.stamp
                                        (Lam_current_unit.get_file ());
                                      assert false)
                                 | e -> e)) (!delta) empty)
                    (!current_ident_sets));
               not (is_empty (!delta)))
            do
            current_ident_sets :=
              (let open Ident_set in union (!current_ident_sets) (!delta))
            done;
          !current_ident_sets
      end 
    module Lam_dce :
      sig
        [@@@ocaml.text " Dead code eliminatiion on the lambda layer \n"]
        val remove : Ident.t list -> Lam_group.t list -> Lam_group.t list
      end =
      struct
        module I = Lambda.IdentSet
        let remove export_idents (rest : Lam_group.t list) =
          (let ident_free_vars = Hashtbl.create 17 in
           let initial_idents =
             (Ext_list.flat_map
                (fun (x : Lam_group.t)  ->
                   match x with
                   | Single (kind,id,lam) ->
                       (Hashtbl.add ident_free_vars id
                          (Lambda.free_variables lam);
                        (match kind with
                         | Alias |StrictOpt  -> []
                         | Strict |Variable  -> [id]))
                   | Recursive bindings ->
                       bindings |>
                         (Ext_list.flat_map
                            (fun (id,lam)  ->
                               Hashtbl.add ident_free_vars id
                                 (Lambda.free_variables lam);
                               (match (lam : Lambda.lambda) with
                                | Lfunction _ -> []
                                | _ -> [id])))
                   | Nop lam ->
                       if Lam_analysis.no_side_effects lam
                       then []
                       else I.elements (Lambda.free_variables lam)) rest)
               @ export_idents in
           let current_ident_sets =
             Idents_analysis.calculate_used_idents ident_free_vars
               initial_idents in
           rest |>
             (Ext_list.filter_map
                (fun (x : Lam_group.t)  ->
                   match x with
                   | Single (_,id,_) ->
                       if I.mem id current_ident_sets then Some x else None
                   | Nop _ -> Some x
                   | Recursive bindings ->
                       let b =
                         bindings |>
                           (Ext_list.filter_map
                              (fun ((id,_) as v)  ->
                                 if I.mem id current_ident_sets
                                 then Some v
                                 else None)) in
                       (match b with | [] -> None | _ -> Some (Recursive b)))) : 
          Lam_group.t list)
      end 
    module Type_util :
      sig
        [@@@ocaml.text
          " Utilities for quering typing inforaation from {!Env.t}, this part relies\n    on compiler API\n"]
        val query : Path.t -> Env.t -> Types.signature option
        val name_of_signature_item : Types.signature_item -> Ident.t
        val get_name : Types.signature -> int -> string
        val filter_serializable_signatures :
          Types.signature -> Types.signature
        val find_serializable_signatures_by_path :
          Path.t -> Env.t -> Types.signature option
        val list_of_arrow :
          Types.type_expr ->
            (Types.type_desc* (string* Types.type_expr) list)
        val label_name :
          string -> [ `Label of string  | `Optional of string ]
        val is_unit : Types.type_desc -> bool
      end =
      struct
        let rec query (v : Path.t) (env : Env.t) =
          (match Env.find_modtype_expansion v env with
           | Mty_alias v1|Mty_ident v1 -> query v1 env
           | Mty_functor _ -> assert false
           | Mty_signature s -> Some s
           | exception _ -> None : Types.signature option)
        let name_of_signature_item (x : Types.signature_item) =
          match x with
          | Sig_value (i,_)|Sig_module (i,_,_) -> i
          | Sig_typext (i,_,_) -> i
          | Sig_modtype (i,_) -> i
          | Sig_class (i,_,_) -> i
          | Sig_class_type (i,_,_) -> i
          | Sig_type (i,_,_) -> i
        let get_name (serializable_sigs : Types.signature) (pos : int) =
          let ident =
            name_of_signature_item @@ (List.nth serializable_sigs pos) in
          ident.name[@@ocaml.doc " Used in [Pgetglobal] "]
        let string_of_value_description id =
          Format.asprintf "%a" (Printtyp.value_description id)
        [@@@ocaml.text
          " It should be safe to replace Pervasives[], \n    we should test cases  like module Pervasives = List "]
        let filter_serializable_signatures (signature : Types.signature) =
          (List.filter
             (fun x  ->
                match (x : Types.signature_item) with
                | Sig_value (_,{ val_kind = Val_prim _ }) -> false
                | Sig_typext _|Sig_module _|Sig_class _|Sig_value _ -> true
                | _ -> false) signature : Types.signature)[@@ocaml.text
                                                            " It should be safe to replace Pervasives[], \n    we should test cases  like module Pervasives = List "]
        let find_serializable_signatures_by_path (v : Path.t) (env : Env.t) =
          (match Env.find_module v env with
           | exception Not_found  -> None
           | { md_type = Mty_signature signature;_} ->
               Some (filter_serializable_signatures signature)
           | _ ->
               (Ext_log.err __LOC__ "@[impossible path %s@]@." (Path.name v);
                assert false) : Types.signature option)
        let rec dump_summary fmt (x : Env.summary) =
          match x with
          | Env_empty  -> ()
          | Env_value (s,id,value_description) ->
              (dump_summary fmt s;
               Printtyp.value_description id fmt value_description)
          | _ -> ()
        let query_type (id : Ident.t) (env : Env.t) =
          match Env.find_value (Pident id) env with
          | exception Not_found  -> ""
          | { val_type } ->
              Format.asprintf "%a" (!Oprint.out_type)
                (Printtyp.tree_of_type_scheme val_type)
        let list_of_arrow ty =
          let rec aux (ty : Types.type_expr) acc =
            match (Ctype.repr ty).desc with
            | Tarrow (label,t1,t2,_) -> aux t2 ((label, t1) :: acc)
            | return_type -> (return_type, (List.rev acc)) in
          aux ty []
        let is_optional l = ((String.length l) > 0) && ((l.[0]) = '?')
        let label_name l =
          if is_optional l
          then `Optional (String.sub l 1 ((String.length l) - 1))
          else `Label l
        let is_unit (x : Types.type_desc) =
          match x with
          | Tconstr (p,_,_) when Path.same p Predef.path_unit -> true
          | _ -> false
      end 
    module String_map :
      sig
        include (Map.S with type  key =  string)
        val of_list : (key* 'a) list -> 'a t
      end =
      struct
        include Map.Make(String)
        let of_list (xs : ('a* 'b) list) =
          List.fold_left (fun acc  -> fun (k,v)  -> add k v acc) empty xs
      end 
    module Lam_compile_util :
      sig
        [@@@ocaml.text " Some utilities for lambda compilation"]
        val jsop_of_comp : Lambda.comparison -> Js_op.binop
        val comment_of_tag_info : Lambda.tag_info -> string option
        val comment_of_pointer_info : Lambda.pointer_info -> string option
      end =
      struct
        let jsop_of_comp (cmp : Lambda.comparison) =
          (match cmp with
           | Ceq  -> EqEqEq
           | Cneq  -> NotEqEq
           | Clt  -> Lt
           | Cgt  -> Gt
           | Cle  -> Le
           | Cge  -> Ge : Js_op.binop)
        let comment_of_tag_info (x : Lambda.tag_info) =
          match x with
          | Blk_constructor (n,_) -> Some n
          | Blk_tuple  -> Some "tuple"
          | Blk_variant x -> Some ("`" ^ x)
          | Blk_record _ -> Some "record"
          | Blk_array  -> Some "array"
          | Blk_module _ -> Some "module"
          | Blk_na  -> None
        let comment_of_pointer_info (x : Lambda.pointer_info) =
          match x with
          | Pt_constructor x -> Some x
          | Pt_variant x -> Some x
          | Lambda.Pt_module_alias  -> None
          | Pt_na  -> None
      end 
    module Js_op_util :
      sig
        [@@@ocaml.text " Some basic utilties around {!Js_op} module "]
        val op_prec : Js_op.binop -> (int* int* int)
        val op_str : Js_op.binop -> string
        val op_int_prec : Js_op.int_op -> (int* int* int)
        val op_int_str : Js_op.int_op -> string
        val str_of_used_stats : Js_op.used_stats -> string
        val update_used_stats : J.ident_info -> Js_op.used_stats -> unit
        val same_vident : J.vident -> J.vident -> bool
        val of_lam_mutable_flag : Asttypes.mutable_flag -> Js_op.mutable_flag
      end =
      struct
        let op_prec (op : Js_op.binop) =
          match op with
          | Eq  -> (1, 13, 1)
          | Or  -> (3, 3, 3)
          | And  -> (4, 4, 4)
          | EqEqEq |NotEqEq  -> (8, 8, 9)
          | Gt |Ge |Lt |Le |InstanceOf  -> (9, 9, 10)
          | Bor  -> (5, 5, 5)
          | Bxor  -> (6, 6, 6)
          | Band  -> (7, 7, 7)
          | Lsl |Lsr |Asr  -> (10, 10, 11)
          | Plus |Minus  -> (11, 11, 12)
          | Mul |Div |Mod  -> (12, 12, 13)
        let op_int_prec (op : Js_op.int_op) =
          match op with
          | Bor  -> (5, 5, 5)
          | Bxor  -> (6, 6, 6)
          | Band  -> (7, 7, 7)
          | Lsl |Lsr |Asr  -> (10, 10, 11)
          | Plus |Minus  -> (11, 11, 12)
          | Mul |Div |Mod  -> (12, 12, 13)
        let op_str (op : Js_op.binop) =
          match op with
          | Bor  -> "|"
          | Bxor  -> "^"
          | Band  -> "&"
          | Lsl  -> "<<"
          | Lsr  -> ">>>"
          | Asr  -> ">>"
          | Plus  -> "+"
          | Minus  -> "-"
          | Mul  -> "*"
          | Div  -> "/"
          | Mod  -> "%"
          | Eq  -> "="
          | Or  -> "||"
          | And  -> "&&"
          | EqEqEq  -> "==="
          | NotEqEq  -> "!=="
          | Lt  -> "<"
          | Le  -> "<="
          | Gt  -> ">"
          | Ge  -> ">="
          | InstanceOf  -> "instanceof"
        let op_int_str (op : Js_op.int_op) =
          match op with
          | Bor  -> "|"
          | Bxor  -> "^"
          | Band  -> "&"
          | Lsl  -> "<<"
          | Lsr  -> ">>>"
          | Asr  -> ">>"
          | Plus  -> "+"
          | Minus  -> "-"
          | Mul  -> "*"
          | Div  -> "/"
          | Mod  -> "%"
        let str_of_used_stats x =
          match (x : Js_op.used_stats) with
          | Js_op.Dead_pure  -> "Dead_pure"
          | Dead_non_pure  -> "Dead_non_pure"
          | Exported  -> "Exported"
          | Once_pure  -> "Once_pure"
          | Used  -> "Used"
          | Scanning_pure  -> "Scanning_pure"
          | Scanning_non_pure  -> "Scanning_non_pure"
          | NA  -> "NA"
        let update_used_stats (ident_info : J.ident_info) used_stats =
          match ident_info.used_stats with
          | Dead_pure |Dead_non_pure |Exported  -> ()
          | Scanning_pure |Scanning_non_pure |Used |Once_pure |NA  ->
              ident_info.used_stats <- used_stats
        let same_kind (x : Js_op.kind) (y : Js_op.kind) =
          match (x, y) with
          | (Ml ,Ml )|(Runtime ,Runtime ) -> true
          | (External (u : string),External v) -> u = v
          | (_,_) -> false
        let same_str_opt (x : string option) (y : string option) =
          match (x, y) with
          | (None ,None ) -> true
          | (Some x0,Some y0) -> x0 = y0
          | (None ,Some _)|(Some _,None ) -> false
        let same_vident (x : J.vident) (y : J.vident) =
          match (x, y) with
          | (Id x0,Id y0) -> Ident.same x0 y0
          | (Qualified (x0,k0,str_opt0),Qualified (y0,k1,str_opt1)) ->
              (Ident.same x0 y0) &&
                ((same_kind k0 k1) && (same_str_opt str_opt0 str_opt1))
          | (Id _,Qualified _)|(Qualified _,Id _) -> false
        let of_lam_mutable_flag (x : Asttypes.mutable_flag) =
          (match x with | Immutable  -> Immutable | Mutable  -> Mutable : 
          Js_op.mutable_flag)
      end 
    module Js_analyzer :
      sig
        [@@@ocaml.text " Analyzing utilities for [J] module "]
        [@@@ocaml.text " for example, whether it has side effect or not.\n "]
        val free_variables_of_statement :
          Ident_set.t -> Ident_set.t -> J.statement -> Ident_set.t
        val free_variables_of_expression :
          Ident_set.t ->
            Ident_set.t -> J.finish_ident_expression -> Ident_set.t
        val no_side_effect_expression_desc : J.expression_desc -> bool
        val no_side_effect_expression : J.expression -> bool[@@ocaml.doc
                                                              " [no_side_effect] means this expression has no side effect, \n    but it might *depend on value store*, so you can not just move it around,\n\n    for example,\n    when you want to do a deep copy, the expression passed to you is pure\n    but you still have to call the function to make a copy, \n    since it maybe changed later\n "]
        val no_side_effect_statement : J.statement -> bool[@@ocaml.doc
                                                            " \n    here we say \n    {[ var x = no_side_effect_expression ]}\n    is [no side effect], but it is actually side effect, \n    since  we are defining a variable, however, if it is not exported or used, \n    then it's fine, so we delay this check later\n "]
        val eq_expression : J.expression -> J.expression -> bool
        val eq_statement : J.statement -> J.statement -> bool
        val rev_flatten_seq : J.expression -> J.block
        val rev_toplevel_flatten : J.block -> J.block[@@ocaml.doc
                                                       " return the block in reverse order "]
        val is_constant : J.expression -> bool
      end =
      struct
        let free_variables used_idents defined_idents =
          object (self)
            inherit  Js_fold.fold as super
            val defined_idents = defined_idents
            val used_idents = used_idents
            method! variable_declaration st =
              match st with
              | { ident; value = None  } ->
                  {<defined_idents = Ident_set.add ident defined_idents>}
              | { ident; value = Some v } ->
                  ({<defined_idents = Ident_set.add ident defined_idents>})#expression
                    v
            method! ident id =
              if Ident_set.mem id defined_idents
              then self
              else {<used_idents = Ident_set.add id used_idents>}
            method! expression exp =
              match exp.expression_desc with
              | Fun (_,_,env) ->
                  {<used_idents =
                      Ident_set.union (Js_fun_env.get_bound env) used_idents>}
              | _ -> super#expression exp
            method get_depenencies =
              Ident_set.diff used_idents defined_idents
            method get_used_idents = used_idents
            method get_defined_idents = defined_idents
          end
        let free_variables_of_statement used_idents defined_idents st =
          ((free_variables used_idents defined_idents)#statement st)#get_depenencies
        let free_variables_of_expression used_idents defined_idents st =
          ((free_variables used_idents defined_idents)#expression st)#get_depenencies
        let rec no_side_effect_expression_desc (x : J.expression_desc) =
          match x with
          | Bool _ -> true
          | Var _ -> true
          | Access (a,b) -> (no_side_effect a) && (no_side_effect b)
          | Str (b,_) -> b
          | Fun _ -> true
          | Number _ -> true
          | Array (xs,_mutable_flag)|Caml_block (xs,_mutable_flag,_,_) ->
              List.for_all no_side_effect xs
          | Bind (fn,obj) -> (no_side_effect fn) && (no_side_effect obj)
          | Object kvs ->
              List.for_all (fun (_property_name,y)  -> no_side_effect y) kvs
          | Array_append (a,b)|String_append (a,b)|Seq (a,b) ->
              (no_side_effect a) && (no_side_effect b)
          | Length (e,_)|Char_of_int e|Char_to_int e|Caml_block_tag e|Typeof
            e -> no_side_effect e
          | Bin (op,a,b) ->
              (op <> Eq) && ((no_side_effect a) && (no_side_effect b))
          | Math _|Array_of_size _|Array_copy _|Int_of_boolean _
            |J.Anything_to_number _|Not _|String_of_small_int_array _
            |Json_stringify _|Anything_to_string _|Dump _|Cond _|FlatCall _
            |Call _|Dot _|New _|Caml_uninitialized_obj _|String_access _
            |Raw_js_code _|Caml_block_set_tag _|Caml_block_set_length _ ->
              false
        and no_side_effect (x : J.expression) =
          no_side_effect_expression_desc x.expression_desc
        let no_side_effect_expression (x : J.expression) = no_side_effect x
        let no_side_effect init =
          object (self)
            inherit  Js_fold.fold as super
            val no_side_effect = init
            method get_no_side_effect = no_side_effect
            method! statement s =
              if not no_side_effect
              then self
              else
                (match s.statement_desc with
                 | Throw _|Debugger |Break |Variable _|Continue _ ->
                     {<no_side_effect = false>}
                 | Exp e -> self#expression e
                 | Int_switch _|String_switch _|ForRange _|If _|While _|Block
                   _|Return _|Try _ -> super#statement s)
            method! list f x =
              if not self#get_no_side_effect then self else super#list f x
            method! expression s =
              if not no_side_effect
              then self
              else {<no_side_effect = no_side_effect_expression s>}
            [@@@ocaml.text " only expression would cause side effec "]
          end
        let no_side_effect_statement st =
          ((no_side_effect true)#statement st)#get_no_side_effect
        let rec eq_expression (x : J.expression) (y : J.expression) =
          match ((x.expression_desc), (y.expression_desc)) with
          | (Number (Int i),Number (Int j)) -> i = j
          | (Number (Float i),Number (Float j)) -> false
          | (Math (name00,args00),Math (name10,args10)) ->
              (name00 = name10) && (eq_expression_list args00 args10)
          | (Access (a0,a1),Access (b0,b1)) ->
              (eq_expression a0 b0) && (eq_expression a1 b1)
          | (Call (a0,args00,_),Call (b0,args10,_)) ->
              (eq_expression a0 b0) && (eq_expression_list args00 args10)
          | (Var (Id i),Var (Id j)) -> Ident.same i j
          | (Bin (op0,a0,b0),Bin (op1,a1,b1)) ->
              (op0 = op1) && ((eq_expression a0 a1) && (eq_expression b0 b1))
          | (_,_) -> false
        and eq_expression_list xs ys =
          let rec aux xs ys =
            match (xs, ys) with
            | ([],[]) -> true
            | ([],_) -> false
            | (_,[]) -> false
            | (x::xs,y::ys) -> (eq_expression x y) && (aux xs ys) in
          aux xs ys
        and eq_statement (x : J.statement) (y : J.statement) =
          match ((x.statement_desc), (y.statement_desc)) with
          | (Exp a,Exp b)
            |(Return { return_value = a;_},Return { return_value = b;_}) ->
              eq_expression a b
          | (_,_) -> false
        let rev_flatten_seq (x : J.expression) =
          let rec aux acc (x : J.expression) =
            (match x.expression_desc with
             | Seq (a,b) -> aux (aux acc a) b
             | _ -> { statement_desc = (Exp x); comment = None } :: acc : 
            J.block) in
          aux [] x
        let rev_toplevel_flatten block =
          let rec aux acc (xs : J.block) =
            (match xs with
             | [] -> acc
             | {
                 statement_desc = Variable
                   ({ ident_info = { used_stats = Dead_pure  };_}
                    |{ ident_info = { used_stats = Dead_non_pure  };
                       value = None  })
                 }::xs -> aux acc xs
             | { statement_desc = Block b;_}::xs -> aux (aux acc b) xs
             | x::xs -> aux (x :: acc) xs : J.block) in
          aux [] block
        let rec is_constant (x : J.expression) =
          match x.expression_desc with
          | Access (a,b) -> (is_constant a) && (is_constant b)
          | Str (b,_) -> b
          | Number _ -> true
          | Array (xs,_mutable_flag) -> List.for_all is_constant xs
          | Caml_block (xs,Immutable ,tag,_) ->
              (List.for_all is_constant xs) && (is_constant tag)
          | Bin (op,a,b) -> (is_constant a) && (is_constant b)
          | _ -> false
      end 
    module Ext_pervasives :
      sig
        [@@@ocaml.text
          " Extension to standard library [Pervavives] module, safe to open \n  "]
        external reraise : exn -> 'a = "%reraise"
        val finally : 'a -> ('a -> 'b) -> ('a -> 'c) -> 'b
        val with_file_as_chan : string -> (out_channel -> 'a) -> 'a
        val with_file_as_pp : string -> (Format.formatter -> 'a) -> 'a
        val is_pos_pow : Int32.t -> int
      end =
      struct
        external reraise : exn -> 'a = "%reraise"
        let finally v f action =
          match f v with
          | exception e -> (action v; reraise e)
          | e -> (action v; e)
        let with_file_as_chan filename f =
          let chan = open_out filename in finally chan f close_out
        let with_file_as_pp filename f =
          let chan = open_out filename in
          finally chan
            (fun chan  ->
               let fmt = Format.formatter_of_out_channel chan in
               let v = f fmt in Format.pp_print_flush fmt (); v) close_out
        let is_pos_pow n =
          let module M = struct exception E end in
            let rec aux c (n : Int32.t) =
              if n <= 0l
              then (-2)
              else
                if n = 1l
                then c
                else
                  if (Int32.logand n 1l) = 0l
                  then aux (c + 1) (Int32.shift_right n 1)
                  else raise M.E in
            try aux 0 n with | M.E  -> (-1)
      end 
    module Js_exp_make :
      sig
        [@@@ocaml.text " Creator utilities for the [J] module "]
        [@@@ocaml.text
          " check if a javascript ast is constant \n\n    The better signature might be \n    {[\n      J.expresssion -> Js_output.t\n    ]}\n    for exmaple\n    {[\n      e ?print_int(3) :  0\n                         --->\n                         if(e){print_int(3)}\n    ]}\n"]
        val extract_non_pure : J.expression -> J.expression option
        type binary_op =
          ?comment:string -> J.expression -> J.expression -> J.expression
        type unary_op = ?comment:string -> J.expression -> J.expression
        type t = J.expression
        val mk : ?comment:string -> J.expression_desc -> t
        val access : binary_op
        val string_access : binary_op
        val var : ?comment:string -> J.ident -> t
        val runtime_var_dot : ?comment:string -> string -> string -> t
        val runtime_var_vid : string -> string -> J.vident
        val ml_var_dot : ?comment:string -> Ident.t -> string -> t
        val external_var_dot :
          ?comment:string -> Ident.t -> string -> string -> t
        val ml_var : ?comment:string -> Ident.t -> t
        val runtime_call : ?comment:string -> string -> string -> t list -> t
        val public_method_call : string -> t -> t -> Int32.t -> t list -> t
        val runtime_ref : string -> string -> t
        val str : ?pure:bool -> ?comment:string -> string -> t
        val fun_ :
          ?comment:string ->
            ?immutable_mask:bool array -> J.ident list -> J.block -> t
        val econd : ?comment:string -> t -> t -> t -> t
        val int : ?comment:string -> ?c:char -> int32 -> t
        val nint : ?comment:string -> nativeint -> t
        val small_int : int -> t
        val float : ?comment:string -> string -> t
        val zero_int_literal : t
        val zero_float_lit : t
        val obj_int_tag_literal : t
        val is_out : binary_op[@@ocaml.doc
                                " [is_out e range] is equivalent to [e > range or e <0]\n\n"]
        val dot : ?comment:string -> t -> string -> t
        val array_length : unary_op
        val string_length : unary_op
        val string_of_small_int_array : unary_op
        val bytes_length : unary_op
        val function_length : unary_op
        val char_of_int : unary_op
        val char_to_int : unary_op
        val array_append : binary_op
        val array_copy : unary_op
        val string_append : binary_op[@@ocaml.doc
                                       "\n   When in ES6 mode, we can use Symbol to guarantee its uniquess,\n   we can not tag [js] object, since it can be frozen \n"]
        val var_dot : ?comment:string -> Ident.t -> string -> t
        val bind_var_call :
          ?comment:string -> Ident.t -> string -> t list -> t
        val bind_call :
          ?comment:string -> J.expression -> string -> J.expression list -> t
        val js_global_dot : ?comment:string -> string -> string -> t
        val index : ?comment:string -> t -> Int32.t -> t
        val assign : binary_op
        val triple_equal : binary_op
        val float_equal : binary_op
        val int_equal : binary_op
        val string_equal : binary_op
        val is_type_number : unary_op
        val typeof : unary_op
        val to_int32 : unary_op
        val to_uint32 : unary_op
        val unchecked_int32_add : binary_op
        val int32_add : binary_op
        val unchecked_int32_minus : binary_op
        val int32_minus : binary_op
        val int32_mul : binary_op
        val unchecked_int32_mul : binary_op
        val int32_div : binary_op
        val int32_lsl : binary_op
        val int32_lsr : binary_op
        val int32_asr : binary_op
        val int32_mod : binary_op
        val int32_bxor : binary_op
        val int32_band : binary_op
        val int32_bor : binary_op
        val float_add : binary_op
        val float_minus : binary_op
        val float_mul : binary_op
        val float_div : binary_op
        val float_notequal : binary_op
        val float_mod : binary_op
        val int_comp : Lambda.comparison -> binary_op
        val string_comp : Js_op.binop -> binary_op
        val float_comp : Lambda.comparison -> binary_op
        val not : t -> t
        val call : ?comment:string -> info:Js_call_info.t -> t -> t list -> t
        val flat_call : binary_op
        val dump : ?comment:string -> Js_op.level -> t list -> t
        val anything_to_string : unary_op
        val to_number : unary_op[@@ocaml.doc
                                  " see {!https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Arithmetic_Operators#Unary_plus}"]
        val int_to_string : unary_op
        val to_json_string : unary_op
        val new_ : ?comment:string -> J.expression -> J.expression list -> t
        val arr : ?comment:string -> J.mutable_flag -> J.expression list -> t
        val make_block :
          ?comment:string ->
            J.expression ->
              J.tag_info -> J.expression list -> J.mutable_flag -> t
        val uninitialized_object :
          ?comment:string -> J.expression -> J.expression -> t
        val uninitialized_array : unary_op
        val seq : binary_op
        val obj : ?comment:string -> J.property_map -> t
        val caml_true : t
        val caml_false : t
        val bool : bool -> t
        val unit : t[@@ocaml.doc
                      " [unit] in ocaml will be compiled into [0]  in js "]
        val js_var : ?comment:string -> string -> t
        val js_global : ?comment:string -> string -> t
        val undefined : t
        val is_caml_block : ?comment:string -> t -> t
        val math : ?comment:string -> string -> t list -> t[@@ocaml.doc
                                                             " [math \"abs\"] --> Math[\"abs\"] "]
        val tag : ?comment:string -> J.expression -> t
        val set_tag : ?comment:string -> J.expression -> J.expression -> t
        [@@@ocaml.text
          " Note that this is coupled with how we encode block, if we use the \n    `Object.defineProperty(..)` since the array already hold the length,\n    this should be a nop \n"]
        val set_length : ?comment:string -> J.expression -> J.expression -> t
        val obj_length : ?comment:string -> J.expression -> t
        val to_ocaml_boolean : unary_op
        val and_ : binary_op
        val or_ : binary_op
        val is_instance_array : unary_op[@@ocaml.doc
                                          " we don't expose a general interface, since a general interface is generally not safe "]
        [@@ocaml.doc " used combined with [caml_update_dummy]"]
        val dummy_obj : ?comment:string -> unit -> t[@@ocaml.doc
                                                      " used combined with [caml_update_dummy]"]
        val of_block :
          ?comment:string -> J.statement list -> J.expression -> t[@@ocaml.doc
                                                                    " convert a block to expresion by using IIFE "]
        val bind : binary_op
        val raw_js_code : ?comment:string -> J.code_info -> string -> t
        val nil : t
        val is_nil : unary_op
        val js_bool : ?comment:string -> bool -> t
        val is_undef : unary_op
      end =
      struct
        let no_side_effect = Js_analyzer.no_side_effect_expression
        type binary_op =
          ?comment:string -> J.expression -> J.expression -> J.expression
        type unary_op = ?comment:string -> J.expression -> J.expression
        let rec extract_non_pure (x : J.expression) =
          match x.expression_desc with
          | Var _|Str _|Number _ -> None
          | Access (a,b) ->
              (match ((extract_non_pure a), (extract_non_pure b)) with
               | (None ,None ) -> None
               | (_,_) -> Some x)
          | Array (xs,_mutable_flag) ->
              if List.for_all (fun x  -> (extract_non_pure x) = None) xs
              then None
              else Some x
          | Seq (a,b) ->
              (match ((extract_non_pure a), (extract_non_pure b)) with
               | (None ,None ) -> None
               | (Some u,Some v) ->
                   Some { x with expression_desc = (Seq (u, v)) }
               | (None ,(Some _ as v)) -> v
               | ((Some _ as u),None ) -> u)
          | _ -> Some x
        type t = J.expression
        let mk ?comment  exp = ({ expression_desc = exp; comment } : t)
        let var ?comment  id =
          ({ expression_desc = (Var (Id id)); comment } : t)
        let runtime_var_dot ?comment  (x : string) (e1 : string) =
          ({
             expression_desc =
               (Var (Qualified ((Ext_ident.create_js x), Runtime, (Some e1))));
             comment
           } : J.expression)
        let runtime_var_vid x e1 =
          (Qualified ((Ext_ident.create_js x), Runtime, (Some e1)) : 
          J.vident)
        let ml_var_dot ?comment  (id : Ident.t) e =
          ({ expression_desc = (Var (Qualified (id, Ml, (Some e)))); comment
           } : J.expression)
        let external_var_dot ?comment  (id : Ident.t) name fn =
          ({
             expression_desc =
               (Var (Qualified (id, (External name), (Some fn))));
             comment
           } : t)
        let ml_var ?comment  (id : Ident.t) =
          ({ expression_desc = (Var (Qualified (id, Ml, None))); comment } : 
          t)
        let str ?(pure= true)  ?comment  s =
          ({ expression_desc = (Str (pure, s)); comment } : t)
        let raw_js_code ?comment  info s =
          ({ expression_desc = (Raw_js_code (s, info)); comment } : t)
        let anything_to_string ?comment  (e : t) =
          (match e.expression_desc with
           | Str _ -> e
           | _ -> { expression_desc = (Anything_to_string e); comment } : 
          t)
        let arr ?comment  mt es =
          ({ expression_desc = (Array (es, mt)); comment } : t)
        let make_block ?comment  tag tag_info es mutable_flag =
          ({
             expression_desc = (Caml_block (es, mutable_flag, tag, tag_info));
             comment =
               (match comment with
                | None  -> Lam_compile_util.comment_of_tag_info tag_info
                | _ -> comment)
           } : t)
        let uninitialized_object ?comment  tag size =
          ({ expression_desc = (Caml_uninitialized_obj (tag, size)); comment
           } : t)
        let uninitialized_array ?comment  (e : t) =
          (match e.expression_desc with
           | Number (Int { i = 0l;_}) -> arr ?comment NA []
           | _ -> { comment; expression_desc = (Array_of_size e) } : 
          t)
        module L = Literals
        let typeof ?comment  (e : t) =
          (match e.expression_desc with
           | Number _|Length _ -> str ?comment L.js_type_number
           | Str _ -> str ?comment L.js_type_string
           | Array _ -> str ?comment L.js_type_object
           | _ -> { expression_desc = (Typeof e); comment } : t)
        let new_ ?comment  e0 args =
          ({ expression_desc = (New (e0, (Some args))); comment } : t)
        let unit: t =
          {
            expression_desc = (Number (Int { i = 0l; c = None }));
            comment = (Some "()")
          }
        let math ?comment  v args =
          ({ comment; expression_desc = (Math (v, args)) } : t)
        let int_to_string ?comment  (e : t) =
          (anything_to_string ?comment e : t)
        let fun_ ?comment  ?immutable_mask  params block =
          (let len = List.length params in
           {
             expression_desc =
               (Fun (params, block, (Js_fun_env.empty ?immutable_mask len)));
             comment
           } : t)
        let dummy_obj ?comment  () =
          ({ comment; expression_desc = (Object []) } : t)
        let is_instance_array ?comment  e =
          ({
             comment;
             expression_desc = (Bin (InstanceOf, e, (str L.js_array_ctor)))
           } : t)
        let rec seq ?comment  (e0 : t) (e1 : t) =
          (match ((e0.expression_desc), (e1.expression_desc)) with
           | ((Seq (a,{ expression_desc = Number _ })|Seq
               ({ expression_desc = Number _ },a)),_) -> seq ?comment a e1
           | (_,Seq ({ expression_desc = Number _ },a)) -> seq ?comment e0 a
           | (_,Seq (a,({ expression_desc = Number _ } as v))) ->
               seq ?comment (seq e0 a) v
           | _ -> { expression_desc = (Seq (e0, e1)); comment } : t)
        let zero_int_literal: t =
          {
            expression_desc = (Number (Int { i = 0l; c = None }));
            comment = None
          }
        let one_int_literal: t =
          {
            expression_desc = (Number (Int { i = 1l; c = None }));
            comment = None
          }
        let two_int_literal: t =
          {
            expression_desc = (Number (Int { i = 2l; c = None }));
            comment = None
          }
        let three_int_literal: t =
          {
            expression_desc = (Number (Int { i = 3l; c = None }));
            comment = None
          }
        let four_int_literal: t =
          {
            expression_desc = (Number (Int { i = 4l; c = None }));
            comment = None
          }
        let five_int_literal: t =
          {
            expression_desc = (Number (Int { i = 5l; c = None }));
            comment = None
          }
        let six_int_literal: t =
          {
            expression_desc = (Number (Int { i = 6l; c = None }));
            comment = None
          }
        let seven_int_literal: t =
          {
            expression_desc = (Number (Int { i = 7l; c = None }));
            comment = None
          }
        let eight_int_literal: t =
          {
            expression_desc = (Number (Int { i = 8l; c = None }));
            comment = None
          }
        let nine_int_literal: t =
          {
            expression_desc = (Number (Int { i = 9l; c = None }));
            comment = None
          }
        let obj_int_tag_literal: t =
          {
            expression_desc = (Number (Int { i = 248l; c = None }));
            comment = None
          }
        let int ?comment  ?c  i =
          ({ expression_desc = (Number (Int { i; c })); comment } : t)
        let small_int i =
          (match i with
           | 0 -> zero_int_literal
           | 1 -> one_int_literal
           | 2 -> two_int_literal
           | 3 -> three_int_literal
           | 4 -> four_int_literal
           | 5 -> five_int_literal
           | 6 -> six_int_literal
           | 7 -> seven_int_literal
           | 8 -> eight_int_literal
           | 9 -> nine_int_literal
           | 248 -> obj_int_tag_literal
           | i -> int (Int32.of_int i) : t)
        let access ?comment  (e0 : t) (e1 : t) =
          (match ((e0.expression_desc), (e1.expression_desc)) with
           | (Array (l,_mutable_flag),Number (Int { i;_})) when
               no_side_effect e0 -> List.nth l (Int32.to_int i)
           | _ -> { expression_desc = (Access (e0, e1)); comment } : 
          t)
        let string_access ?comment  (e0 : t) (e1 : t) =
          (match ((e0.expression_desc), (e1.expression_desc)) with
           | (Str (_,s),Number (Int { i;_})) ->
               let i = Int32.to_int i in
               if (i >= 0) && (i < (String.length s))
               then str (String.make 1 (s.[i]))
               else { expression_desc = (String_access (e0, e1)); comment }
           | _ -> { expression_desc = (String_access (e0, e1)); comment } : 
          t)
        let index ?comment  (e0 : t) e1 =
          (match e0.expression_desc with
           | Array (l,_mutable_flag) when no_side_effect e0 ->
               List.nth l (Int32.to_int e1)
           | Caml_block (l,_mutable_flag,_,_) when no_side_effect e0 ->
               List.nth l (Int32.to_int e1)
           | _ ->
               {
                 expression_desc = (Access (e0, (int ?comment e1)));
                 comment = None
               } : t)
        let call ?comment  ~info  e0 args =
          ({ expression_desc = (Call (e0, args, info)); comment } : t)
        let flat_call ?comment  e0 es =
          ({ expression_desc = (FlatCall (e0, es)); comment } : t)
        let runtime_call ?comment  module_name fn_name args =
          call ?comment ~info:Js_call_info.builtin_runtime_call
            (runtime_var_dot module_name fn_name) args
        let runtime_ref module_name fn_name =
          runtime_var_dot module_name fn_name
        let js_var ?comment  (v : string) =
          var ?comment (Ext_ident.create_js v)
        let js_global ?comment  (v : string) =
          var ?comment (Ext_ident.create_js v)
        let dot ?comment  (e0 : t) (e1 : string) =
          ({ expression_desc = (Dot (e0, e1, true)); comment } : t)[@@ocaml.doc
                                                                    " used in normal property\n    like [e.length], no dependency introduced\n"]
        let undefined = var Ext_ident.undefined
        let nil = var Ext_ident.nil
        let is_caml_block ?comment  (e : t) =
          ({
             expression_desc =
               (Bin (NotEqEq, (dot e L.js_prop_length), undefined));
             comment
           } : t)[@@ocaml.doc " coupled with the runtime "]
        let array_length ?comment  (e : t) =
          (match e.expression_desc with
           | Array (l,_)|Caml_block (l,_,_,_) when no_side_effect e ->
               int ?comment (Int32.of_int (List.length l))
           | _ -> { expression_desc = (Length (e, Array)); comment } : 
          t)
        let string_length ?comment  (e : t) =
          (match e.expression_desc with
           | Str (_,v) -> int ?comment (Int32.of_int (String.length v))
           | _ -> { expression_desc = (Length (e, String)); comment } : 
          t)
        let bytes_length ?comment  (e : t) =
          (match e.expression_desc with
           | Array (l,_) -> int ?comment (Int32.of_int (List.length l))
           | Str (_,v) -> int ?comment (Int32.of_int @@ (String.length v))
           | _ -> { expression_desc = (Length (e, Bytes)); comment } : 
          t)
        let function_length ?comment  (e : t) =
          (match e.expression_desc with
           | Fun (params,_,_) ->
               int ?comment (Int32.of_int @@ (List.length params))
           | _ -> { expression_desc = (Length (e, Function)); comment } : 
          t)
        let js_global_dot ?comment  (x : string) (e1 : string) =
          ({ expression_desc = (Dot ((js_var x), e1, true)); comment } : 
          t)[@@ocaml.doc " no dependency introduced "]
        let char_of_int ?comment  (v : t) =
          (match v.expression_desc with
           | Number (Int { i;_}) ->
               str (String.make 1 (Char.chr (Int32.to_int i)))
           | Char_to_int v -> v
           | _ -> { comment; expression_desc = (Char_of_int v) } : t)
        let char_to_int ?comment  (v : t) =
          (match v.expression_desc with
           | Str (_,x) ->
               (assert ((String.length x) = 1);
                int ~comment:(Printf.sprintf "%S" x)
                  (Int32.of_int @@ (Char.code (x.[0]))))
           | Char_of_int v -> v
           | _ -> { comment; expression_desc = (Char_to_int v) } : t)
        let array_append ?comment  e el =
          ({ comment; expression_desc = (Array_append (e, el)) } : t)
        let array_copy ?comment  e =
          ({ comment; expression_desc = (Array_copy e) } : t)
        let dump ?comment  level el =
          ({ comment; expression_desc = (Dump (level, el)) } : t)
        let to_json_string ?comment  e =
          ({ comment; expression_desc = (Json_stringify e) } : t)
        let rec string_append ?comment  (e : t) (el : t) =
          (match ((e.expression_desc), (el.expression_desc)) with
           | (Str (_,a),String_append ({ expression_desc = Str (_,b) },c)) ->
               string_append ?comment (str (a ^ b)) c
           | (String_append (c,{ expression_desc = Str (_,b) }),Str (_,a)) ->
               string_append ?comment c (str (b ^ a))
           | (String_append (a,{ expression_desc = Str (_,b) }),String_append
              ({ expression_desc = Str (_,c) },d)) ->
               string_append ?comment (string_append a (str (b ^ c))) d
           | (Str (_,a),Str (_,b)) -> str ?comment (a ^ b)
           | (_,Anything_to_string b) -> string_append ?comment e b
           | (Anything_to_string b,_) -> string_append ?comment b el
           | (_,_) -> { comment; expression_desc = (String_append (e, el)) } : 
          t)
        let obj ?comment  properties =
          ({ expression_desc = (Object properties); comment } : t)
        let var_dot ?comment  (x : Ident.t) (e1 : string) =
          ({ expression_desc = (Dot ((var x), e1, true)); comment } : 
          t)
        let bind_call ?comment  obj (e1 : string) args =
          (call ~info:Js_call_info.dummy
             {
               expression_desc =
                 (Bind
                    ({ expression_desc = (Dot (obj, e1, true)); comment },
                      obj));
               comment = None
             } args : t)
        let bind_var_call ?comment  (x : Ident.t) (e1 : string) args =
          (let obj = var x in
           call ~info:Js_call_info.dummy
             {
               expression_desc =
                 (Bind
                    ({ expression_desc = (Dot (obj, e1, true)); comment },
                      obj));
               comment = None
             } args : t)
        let assign ?comment  e0 e1 =
          ({ expression_desc = (Bin (Eq, e0, e1)); comment } : t)
        let to_ocaml_boolean ?comment  (e : t) =
          (match e.expression_desc with
           | Int_of_boolean _|Number _ -> e
           | _ -> { comment; expression_desc = (Int_of_boolean e) } : 
          t)[@@ocaml.doc
              " Convert a javascript boolean to ocaml boolean\n    It's necessary for return value\n     this should be optmized away for [if] ,[cond] to produce \n    more readable code\n"]
        let to_number ?comment  (e : t) =
          (match e.expression_desc with
           | Int_of_boolean _|Anything_to_number _|Number _ -> e
           | _ -> { comment; expression_desc = (Anything_to_number e) } : 
          t)
        let caml_true = int ~comment:"true" 1l
        let caml_false = int ~comment:"false" 0l
        let bool v = if v then caml_true else caml_false
        let rec triple_equal ?comment  (e0 : t) (e1 : t) =
          (match ((e0.expression_desc), (e1.expression_desc)) with
           | (Var (Id
              ({ name = ("undefined"|"null") } as id)),(Char_of_int _
                                                        |Char_to_int _|Bool _
                                                        |Number _|Typeof _
                                                        |Int_of_boolean _|Fun
                                                        _|Array _|Caml_block
                                                        _))
               when (Ext_ident.is_js id) && (no_side_effect e1) -> caml_false
           | ((Char_of_int _|Char_to_int _|Bool _|Number _|Typeof _
               |Int_of_boolean _|Fun _|Array _|Caml_block _),Var
              (Id ({ name = ("undefined"|"null") } as id))) when
               (Ext_ident.is_js id) && (no_side_effect e0) -> caml_false
           | (Str (_,x),Str (_,y)) -> bool (Ext_string.equal x y)
           | (Char_to_int a,Char_to_int b) -> triple_equal ?comment a b
           | (Char_to_int a,Number (Int { i; c = Some v }))
             |(Number (Int { i; c = Some v }),Char_to_int a) ->
               triple_equal ?comment a (str (String.make 1 v))
           | (Number (Int { i = i0;_}),Number (Int { i = i1;_})) ->
               bool (i0 = i1)
           | (Char_of_int a,Char_of_int b) -> triple_equal ?comment a b
           | _ ->
               to_ocaml_boolean
                 { expression_desc = (Bin (EqEqEq, e0, e1)); comment } : 
          t)
        [@@@ocaml.text " Arith operators "]
        let float ?comment  f =
          ({ expression_desc = (Number (Float { f })); comment } : t)
          [@@ocaml.text " Arith operators "]
        let zero_float_lit: t =
          { expression_desc = (Number (Float { f = "0." })); comment = None }
        let float_mod ?comment  e1 e2 =
          ({ comment; expression_desc = (Bin (Mod, e1, e2)) } : J.expression)
        let bin ?comment  (op : J.binop) e0 e1 =
          (match op with
           | EqEqEq  -> triple_equal ?comment e0 e1
           | _ -> { expression_desc = (Bin (op, e0, e1)); comment } : 
          t)
        let rec and_ ?comment  (e1 : t) (e2 : t) =
          match ((e1.expression_desc), (e2.expression_desc)) with
          | (Int_of_boolean e1,Int_of_boolean e2) -> and_ ?comment e1 e2
          | (Int_of_boolean e1,_) -> and_ ?comment e1 e2
          | (_,Int_of_boolean e2) -> and_ ?comment e1 e2
          | (Var i,Var j) when Js_op_util.same_vident i j ->
              to_ocaml_boolean e1
          | (Var
             i,(Bin (And ,{ expression_desc = Var j;_},_)|Bin
                (And ,_,{ expression_desc = Var j;_})))
              when Js_op_util.same_vident i j -> to_ocaml_boolean e2
          | (_,_) ->
              to_ocaml_boolean
                { expression_desc = (Bin (And, e1, e2)); comment }
        let rec or_ ?comment  (e1 : t) (e2 : t) =
          match ((e1.expression_desc), (e2.expression_desc)) with
          | (Int_of_boolean e1,Int_of_boolean e2) -> or_ ?comment e1 e2
          | (Int_of_boolean e1,_) -> or_ ?comment e1 e2
          | (_,Int_of_boolean e2) -> or_ ?comment e1 e2
          | (Var i,Var j) when Js_op_util.same_vident i j ->
              to_ocaml_boolean e1
          | (Var
             i,(Bin (Or ,{ expression_desc = Var j;_},_)|Bin
                (Or ,_,{ expression_desc = Var j;_})))
              when Js_op_util.same_vident i j -> to_ocaml_boolean e2
          | (_,_) ->
              to_ocaml_boolean
                { expression_desc = (Bin (Or, e1, e2)); comment }
        let rec not (({ expression_desc; comment } as e) : t) =
          (match expression_desc with
           | Bin (EqEqEq ,e0,e1) ->
               { expression_desc = (Bin (NotEqEq, e0, e1)); comment }
           | Bin (NotEqEq ,e0,e1) ->
               { expression_desc = (Bin (EqEqEq, e0, e1)); comment }
           | Bin (Lt ,a,b) -> { e with expression_desc = (Bin (Ge, a, b)) }
           | Bin (Ge ,a,b) -> { e with expression_desc = (Bin (Lt, a, b)) }
           | Bin (Le ,a,b) -> { e with expression_desc = (Bin (Gt, a, b)) }
           | Bin (Gt ,a,b) -> { e with expression_desc = (Bin (Le, a, b)) }
           | Number (Int { i;_}) -> if i <> 0l then caml_false else caml_true
           | Int_of_boolean e -> not e
           | Not e -> e
           | x -> { expression_desc = (Not e); comment = None } : t)
        let rec econd ?comment  (b : t) (t : t) (f : t) =
          (match ((b.expression_desc), (t.expression_desc),
                   (f.expression_desc))
           with
           | (Number (Int { i = 0l;_}),_,_) -> f
           | ((Number _|Array _|Caml_block _),_,_) when no_side_effect b -> t
           | (Bool (true ),_,_) -> t
           | (Bool (false ),_,_) -> f
           | (Bin
              (Bor ,v,{ expression_desc = Number (Int { i = 0l;_}) }),_,_) ->
               econd v t f
           | ((Bin
               (EqEqEq ,{ expression_desc = Number (Int { i = 0l;_});_},x)
               |Bin
               (EqEqEq ,x,{ expression_desc = Number (Int { i = 0l;_});_})),_,_)
               -> econd ?comment x f t
           | (Bin
              (Ge
               ,{ expression_desc = Length _;_},{
                                                  expression_desc = Number
                                                    (Int { i = 0l;_})
                                                  }),_,_)
               -> f
           | (Bin
              (Gt
               ,({ expression_desc = Length _;_} as pred),{
                                                            expression_desc =
                                                              Number (Int
                                                              { i = 0l })
                                                            }),_,_)
               -> econd ?comment pred t f
           | (_,Cond (p1,branch_code0,branch_code1),_) when
               Js_analyzer.eq_expression branch_code1 f ->
               econd (and_ b p1) branch_code0 f
           | (_,Cond (p1,branch_code0,branch_code1),_) when
               Js_analyzer.eq_expression branch_code0 f ->
               econd (and_ b (not p1)) branch_code1 f
           | (_,_,Cond (p1',branch_code0,branch_code1)) when
               Js_analyzer.eq_expression t branch_code0 ->
               econd (or_ b p1') t branch_code1
           | (_,_,Cond (p1',branch_code0,branch_code1)) when
               Js_analyzer.eq_expression t branch_code1 ->
               econd (or_ b (not p1')) t branch_code0
           | (Not e,_,_) -> econd ?comment e f t
           | (Int_of_boolean b,_,_) -> econd ?comment b t f
           | _ ->
               if Js_analyzer.eq_expression t f
               then (if no_side_effect b then t else seq ?comment b t)
               else { expression_desc = (Cond (b, t, f)); comment } : 
          t)
        let rec float_equal ?comment  (e0 : t) (e1 : t) =
          (match ((e0.expression_desc), (e1.expression_desc)) with
           | (Number (Int { i = i0;_}),Number (Int { i = i1 })) ->
               bool (i0 = i1)
           | ((Bin
               (Bor
                ,{ expression_desc = Number (Int { i = 0l;_}) },({
                                                                   expression_desc
                                                                    =
                                                                    Caml_block_tag
                                                                    _;_}
                                                                   as a))|Bin
               (Bor
                ,({ expression_desc = Caml_block_tag _;_} as a),{
                                                                  expression_desc
                                                                    = Number
                                                                    (Int
                                                                    {
                                                                    i = 0l;_})
                                                                  })),Number
              (Int { i = 0l;_})) -> not a
           | ((Bin
               (Bor
                ,{ expression_desc = Number (Int { i = 0l;_}) },({
                                                                   expression_desc
                                                                    =
                                                                    Caml_block_tag
                                                                    _;_}
                                                                   as a))|Bin
               (Bor
                ,({ expression_desc = Caml_block_tag _;_} as a),{
                                                                  expression_desc
                                                                    = Number
                                                                    (Int
                                                                    {
                                                                    i = 0l;_})
                                                                  })),Number
              _) -> float_equal ?comment a e1
           | (Number (Float { f = f0;_}),Number (Float { f = f1 })) when
               f0 = f1 -> caml_true
           | (Char_to_int a,Char_to_int b) -> float_equal ?comment a b
           | (Char_to_int a,Number (Int { i; c = Some v }))
             |(Number (Int { i; c = Some v }),Char_to_int a) ->
               float_equal ?comment a (str (String.make 1 v))
           | (Char_of_int a,Char_of_int b) -> float_equal ?comment a b
           | _ ->
               to_ocaml_boolean
                 { expression_desc = (Bin (EqEqEq, e0, e1)); comment } : 
          t)
        let int_equal = float_equal
        let rec string_equal ?comment  (e0 : t) (e1 : t) =
          (match ((e0.expression_desc), (e1.expression_desc)) with
           | (Str (_,a0),Str (_,b0)) -> bool (Ext_string.equal a0 b0)
           | (_,_) ->
               to_ocaml_boolean
                 { expression_desc = (Bin (EqEqEq, e0, e1)); comment } : 
          t)
        let is_type_number ?comment  (e : t) =
          (string_equal ?comment (typeof e) (str "number") : t)
        let string_of_small_int_array ?comment  xs =
          ({ expression_desc = (String_of_small_int_array xs); comment } : 
          t)
        let null ?comment  () = js_global ?comment "null"
        let tag ?comment  e =
          ({
             expression_desc =
               (Bin
                  (Bor, { expression_desc = (Caml_block_tag e); comment },
                    zero_int_literal));
             comment = None
           } : t)
        let bind ?comment  fn obj =
          ({ expression_desc = (Bind (fn, obj)); comment } : t)
        let public_method_call meth_name obj label cache args =
          let len = List.length args in
          econd (int_equal (tag obj) obj_int_tag_literal)
            (if len <= 7
             then
               runtime_call Js_config.curry
                 ("js" ^ (string_of_int (len + 1))) (label :: (int cache) ::
                 obj :: args)
             else
               runtime_call Js_config.curry "js"
                 [label; int cache; obj; arr NA (obj :: args)])
            (let fn = bind (dot obj meth_name) obj in
             if len = 0
             then dot obj meth_name
             else
               if len <= 8
               then
                 (let len_str = string_of_int len in
                  runtime_call Js_config.curry (Literals.app ^ len_str) (fn
                    :: args))
               else
                 runtime_call Js_config.curry Literals.app_array
                   [fn; arr NA args])
        let set_tag ?comment  e tag =
          (seq { expression_desc = (Caml_block_set_tag (e, tag)); comment }
             unit : t)
        let set_length ?comment  e tag =
          (seq
             { expression_desc = (Caml_block_set_length (e, tag)); comment }
             unit : t)
        let obj_length ?comment  e =
          ({ expression_desc = (Length (e, Caml_block)); comment } : 
          t)
        let rec int32_bor ?comment  (e1 : J.expression) (e2 : J.expression) =
          (match ((e1.expression_desc), (e2.expression_desc)) with
           | (Number (Int { i = i1 }|Uint i1),Number (Int { i = i2 })) ->
               int ?comment (Int32.logor i1 i2)
           | (_,Bin
              (Lsr
               ,e2,{
                     expression_desc = Number
                       (Int { i = 0l }|Uint 0l|Nint 0n);_}))
               -> int32_bor e1 e2
           | (Bin
              (Lsr
               ,e1,{
                     expression_desc = Number
                       (Int { i = 0l }|Uint 0l|Nint 0n);_}),_)
               -> int32_bor e1 e2
           | (Bin
              (Lsr ,_,{ expression_desc = Number (Int { i }|Uint i);_}),Number
              (Int { i = 0l }|Uint 0l|Nint 0n)) when i > 0l -> e1
           | (Bin
              (Bor
               ,e1,{
                     expression_desc = Number
                       (Int { i = 0l }|Uint 0l|Nint 0n);_}),Number
              (Int { i = 0l }|Uint 0l|Nint 0n)) -> int32_bor e1 e2
           | _ -> { comment; expression_desc = (Bin (Bor, e1, e2)) } : 
          J.expression)
        let to_int32 ?comment  (e : J.expression) =
          (int32_bor ?comment e zero_int_literal : J.expression)
        let nint ?comment  n =
          ({ expression_desc = (Number (Nint n)); comment } : J.expression)
        let uint32 ?comment  n =
          ({ expression_desc = (Number (Uint n)); comment } : J.expression)
        let string_comp cmp ?comment  e0 e1 =
          to_ocaml_boolean @@ (bin ?comment cmp e0 e1)
        let rec int_comp (cmp : Lambda.comparison) ?comment  (e0 : t)
          (e1 : t) =
          match (cmp, (e0.expression_desc), (e1.expression_desc)) with
          | (_,Call
             ({
                expression_desc = Var (Qualified
                  (_,Runtime ,Some ("caml_int_compare"|"caml_int32_compare")));_},l::r::[],_),Number
             (Int { i = 0l })) -> int_comp cmp l r
          | (Ceq ,_,_) -> int_equal e0 e1
          | _ ->
              to_ocaml_boolean @@
                (bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1)
        let float_comp cmp ?comment  e0 e1 =
          to_ocaml_boolean @@
            (bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1)
        let rec int32_lsr ?comment  (e1 : J.expression) (e2 : J.expression) =
          (let aux i1 i = uint32 (Int32.shift_right_logical i1 i) in
           match ((e1.expression_desc), (e2.expression_desc)) with
           | (Number (Int { i = i1 }|Uint i1),Number
              (Int { i = i2 }|Uint i2)) -> aux i1 (Int32.to_int i2)
           | (Number (Nint i1),Number (Int { i = i2 }|Uint i2)) ->
               aux (Nativeint.to_int32 i1) (Int32.to_int i2)
           | (Number (Nint i1),Number (Nint i2)) ->
               aux (Nativeint.to_int32 i1) (Nativeint.to_int i2)
           | (Bin (Lsr ,_,_),Number (Int { i = 0l }|Uint 0l|Nint 0n)) -> e1
           | (Bin
              (Bor
               ,e1,{
                     expression_desc = Number
                       (Int { i = 0l;_}|Uint 0l|Nint 0n);_}),Number
              (Int { i = 0l }|Uint 0l|Nint 0n)) -> int32_lsr ?comment e1 e2
           | (_,_) -> { comment; expression_desc = (Bin (Lsr, e1, e2)) } : 
          J.expression)
        let to_uint32 ?comment  (e : J.expression) =
          (int32_lsr ?comment e zero_int_literal : J.expression)
        let rec is_out ?comment  (e : t) (range : t) =
          (match ((range.expression_desc), (e.expression_desc)) with
           | (Number (Int { i = 1l }),Var _) ->
               not
                 (or_ (triple_equal e zero_int_literal)
                    (triple_equal e one_int_literal))
           | (Number (Int
              { i = 1l }),(Bin
                           (Plus
                            ,{ expression_desc = Number (Int { i;_}) },
                            { expression_desc = Var _;_})|Bin
                           (Plus
                            ,{ expression_desc = Var _;_},{
                                                            expression_desc =
                                                              Number (Int
                                                              { i;_})
                                                            })))
               ->
               not
                 (or_ (triple_equal e (int (Int32.neg i)))
                    (triple_equal e (int (Int32.sub Int32.one i))))
           | (Number (Int { i = 1l }),Bin
              (Minus
               ,({ expression_desc = Var _;_} as x),{
                                                      expression_desc =
                                                        Number (Int { i;_})
                                                      }))
               ->
               not
                 (or_ (triple_equal x (int (Int32.add i 1l)))
                    (triple_equal x (int i)))
           | (Number (Int { i = k }),Bin
              (Minus
               ,({ expression_desc = Var _;_} as x),{
                                                      expression_desc =
                                                        Number (Int { i;_})
                                                      }))
               ->
               or_ (int_comp Cgt x (int (Int32.add i k)))
                 (int_comp Clt x (int i))
           | (Number (Int { i = k }),Var _) ->
               or_ (int_comp Cgt e (int k)) (int_comp Clt e zero_int_literal)
           | (_,Bin
              (Bor
               ,({
                   expression_desc =
                     (Bin
                      ((Plus |Minus ),{ expression_desc = Number (Int { i;_})
                                        },{ expression_desc = Var _;_})|Bin
                      ((Plus |Minus ),{ expression_desc = Var _;_},{
                                                                    expression_desc
                                                                    = Number
                                                                    (Int
                                                                    { i;_}) }))
                   } as e),{
                             expression_desc = Number
                               (Int { i = 0l }|Uint 0l|Nint 0n);_}))
               -> is_out ?comment e range
           | (_,_) -> int_comp ?comment Cgt (to_uint32 e) range : t)
        let rec float_add ?comment  (e1 : t) (e2 : t) =
          match ((e1.expression_desc), (e2.expression_desc)) with
          | (Number (Int { i;_}),Number (Int { i = j;_})) ->
              int ?comment (Int32.add i j)
          | (_,Number (Int { i = j; c })) when j < 0l ->
              float_minus ?comment e1
                {
                  e2 with
                  expression_desc = (Number (Int { i = (Int32.neg j); c }))
                }
          | (Bin
             (Plus ,a1,{ expression_desc = Number (Int { i = k;_}) }),Number
             (Int { i = j;_})) ->
              {
                comment;
                expression_desc = (Bin (Plus, a1, (int (Int32.add k j))))
              }
          | _ -> { comment; expression_desc = (Bin (Plus, e1, e2)) }
        and float_minus ?comment  (e1 : t) (e2 : t) =
          (match ((e1.expression_desc), (e2.expression_desc)) with
           | (Number (Int { i;_}),Number (Int { i = j;_})) ->
               int ?comment (Int32.sub i j)
           | _ -> { comment; expression_desc = (Bin (Minus, e1, e2)) } : 
          t)
        let unchecked_int32_add ?comment  e1 e2 = float_add ?comment e1 e2
        let int32_add ?comment  e1 e2 =
          to_int32 @@ (float_add ?comment e1 e2)
        let int32_minus ?comment  e1 e2 =
          (to_int32 @@ (float_minus ?comment e1 e2) : J.expression)
        let unchecked_int32_minus ?comment  e1 e2 =
          (float_minus ?comment e1 e2 : J.expression)
        let float_div ?comment  e1 e2 = bin ?comment Div e1 e2
        let float_notequal ?comment  e1 e2 = bin ?comment NotEqEq e1 e2
        let unchecked_int32_div ?comment  e1 e2 =
          (to_int32 (float_div ?comment e1 e2) : J.expression)[@@ocaml.doc
                                                                " Division by zero is undefined behavior"]
        let int32_asr ?comment  e1 e2 =
          ({ comment; expression_desc = (Bin (Asr, e1, e2)) } : J.expression)
        let int32_div ?comment  (e1 : J.expression) (e2 : J.expression) =
          (match ((e1.expression_desc), (e2.expression_desc)) with
           | (Length _,Number (Int { i = 2l }|Uint 2l|Nint 2n)) ->
               int32_asr e1 one_int_literal
           | (Number (Int { i = i0 }),Number (Int { i = i1 })) when i1 <> 0l
               -> int (Int32.div i0 i1)
           | (_,_) -> to_int32 (float_div ?comment e1 e2) : J.expression)
        let float_mul ?comment  e1 e2 = bin ?comment Mul e1 e2
        let int32_mod ?comment  e1 e2 =
          ({ comment; expression_desc = (Bin (Mod, e1, e2)) } : J.expression)
        let int32_lsl ?comment  (e1 : J.expression) (e2 : J.expression) =
          (match (e1, e2) with
           | ({ expression_desc = Number (Int { i = i0 }|Uint i0) },{
                                                                    expression_desc
                                                                    = Number
                                                                    (Int
                                                                    { i = i1
                                                                    }|Uint i1)
                                                                    })
               -> int ?comment (Int32.shift_left i0 (Int32.to_int i1))
           | _ -> { comment; expression_desc = (Bin (Lsl, e1, e2)) } : 
          J.expression)
        let int32_mul ?comment  (e1 : J.expression) (e2 : J.expression) =
          (match (e1, e2) with
           | ({ expression_desc = Number (Int { i = 0l }|Uint 0l|Nint 0n);_},x)
             |(x,{
                   expression_desc = Number (Int { i = 0l }|Uint 0l|Nint 0n);_})
               when Js_analyzer.no_side_effect_expression x ->
               zero_int_literal
           | ({ expression_desc = Number (Int { i = i0 });_},{
                                                               expression_desc
                                                                 = Number
                                                                 (Int
                                                                 { i = i1 });_})
               -> int (Int32.mul i0 i1)
           | (e,{ expression_desc = Number (Int { i = i0 }|Uint i0);_})
             |({ expression_desc = Number (Int { i = i0 }|Uint i0);_},e) ->
               let i = Ext_pervasives.is_pos_pow i0 in
               if i >= 0
               then int32_lsl e (small_int i)
               else
                 runtime_call ?comment Js_config.int32 Literals.imul [e1; e2]
           | _ ->
               runtime_call ?comment Js_config.int32 Literals.imul [e1; e2] : 
          J.expression)
        let unchecked_int32_mul ?comment  e1 e2 =
          ({ comment; expression_desc = (Bin (Mul, e1, e2)) } : J.expression)
        let rec int32_bxor ?comment  (e1 : t) (e2 : t) =
          (match ((e1.expression_desc), (e2.expression_desc)) with
           | (Number (Int { i = i1 }),Number (Int { i = i2 })) ->
               int ?comment (Int32.logxor i1 i2)
           | (_,Bin
              (Lsr
               ,e2,{
                     expression_desc = Number
                       (Int { i = 0l }|Uint 0l|Nint 0n);_}))
               -> int32_bxor e1 e2
           | (Bin
              (Lsr
               ,e1,{
                     expression_desc = Number
                       (Int { i = 0l }|Uint 0l|Nint 0n);_}),_)
               -> int32_bxor e1 e2
           | _ -> { comment; expression_desc = (Bin (Bxor, e1, e2)) } : 
          J.expression)
        let rec int32_band ?comment  (e1 : J.expression) (e2 : J.expression)
          =
          (match e1.expression_desc with
           | Bin (Bor ,a,{ expression_desc = Number (Int { i = 0l }) }) ->
               int32_band a e2
           | _ -> { comment; expression_desc = (Bin (Band, e1, e2)) } : 
          J.expression)
        let of_block ?comment  block e =
          (call ~info:Js_call_info.ml_full_call
             {
               comment;
               expression_desc =
                 (Fun
                    ([],
                      (block @
                         [{
                            J.statement_desc = (Return { return_value = e });
                            comment
                          }]), (Js_fun_env.empty 0)))
             } [] : t)
        let is_nil ?comment  x = triple_equal ?comment x nil
        let js_bool ?comment  x =
          ({ comment; expression_desc = (Bool x) } : t)
        let is_undef ?comment  x = triple_equal ?comment x undefined
      end 
    module Js_stmt_make :
      sig
        [@@@ocaml.text " Creator utilities for the [J] module "]
        type t = J.statement
        val mk : ?comment:string -> J.statement_desc -> t
        val empty : ?comment:string -> unit -> t
        val throw : ?comment:string -> J.expression -> t
        val if_ :
          ?comment:string ->
            ?declaration:(Lambda.let_kind* Ident.t) ->
              ?else_:J.block -> J.expression -> J.block -> t
        val block : ?comment:string -> J.block -> t
        val int_switch :
          ?comment:string ->
            ?declaration:(Lambda.let_kind* Ident.t) ->
              ?default:J.block -> J.expression -> int J.case_clause list -> t
        val string_switch :
          ?comment:string ->
            ?declaration:(Lambda.let_kind* Ident.t) ->
              ?default:J.block ->
                J.expression -> string J.case_clause list -> t
        val declare_variable :
          ?comment:string ->
            ?ident_info:J.ident_info -> kind:Lambda.let_kind -> Ident.t -> t
        val define :
          ?comment:string ->
            ?ident_info:J.ident_info ->
              kind:Lambda.let_kind -> Ident.t -> J.expression -> t
        val alias_variable :
          ?comment:string -> ?exp:J.expression -> Ident.t -> t
        val assign : ?comment:string -> J.ident -> J.expression -> t
        val assign_unit : ?comment:string -> J.ident -> t
        val declare_unit : ?comment:string -> J.ident -> t
        val while_ :
          ?comment:string ->
            ?label:J.label ->
              ?env:Js_closure.t -> J.expression -> J.block -> t
        val for_ :
          ?comment:string ->
            ?env:Js_closure.t ->
              J.for_ident_expression option ->
                J.finish_ident_expression ->
                  J.for_ident -> J.for_direction -> J.block -> t
        val try_ :
          ?comment:string ->
            ?with_:(J.ident* J.block) -> ?finally:J.block -> J.block -> t
        val exp : ?comment:string -> J.expression -> t
        val return : ?comment:string -> J.expression -> t
        val unknown_lambda : ?comment:string -> Lambda.lambda -> t
        val return_unit : ?comment:string -> unit -> t[@@ocaml.doc
                                                        " for ocaml function which returns unit \n    it will be compiled into [return 0] in js "]
        val break : ?comment:string -> unit -> t
        val continue : ?comment:string -> ?label:J.label -> unit -> t
        [@@ocaml.doc " if [label] is not set, it will default to empty "]
        val debugger : t
      end =
      struct
        module E = Js_exp_make
        type t = J.statement
        let return ?comment  e =
          ({ statement_desc = (Return { return_value = e }); comment } : 
          t)
        let return_unit ?comment  () = (return ?comment E.unit : t)
        let break ?comment  () = ({ comment; statement_desc = Break } : t)
        let mk ?comment  statement_desc = ({ statement_desc; comment } : t)
        let empty ?comment  () =
          ({ statement_desc = (Block []); comment } : t)
        let throw ?comment  v =
          ({ statement_desc = (J.Throw v); comment } : t)
        let rec block ?comment  (b : J.block) =
          (match b with
           | { statement_desc = Block bs }::[] -> block bs
           | b::[] -> b
           | [] -> empty ?comment ()
           | _ -> { statement_desc = (Block b); comment } : t)
        let rec exp ?comment  (e : E.t) =
          (match e.expression_desc with
           | Seq ({ expression_desc = Number _ },b)|Seq
             (b,{ expression_desc = Number _ }) -> exp ?comment b
           | Number _ -> block []
           | _ -> { statement_desc = (Exp e); comment } : t)
        let declare_variable ?comment  ?ident_info  ~kind  (v : Ident.t) =
          (let property: J.property = kind in
           let ident_info: J.ident_info =
             match ident_info with
             | None  -> { used_stats = NA }
             | Some x -> x in
           {
             statement_desc =
               (Variable { ident = v; value = None; property; ident_info });
             comment
           } : t)
        let define ?comment  ?ident_info  ~kind  (v : Ident.t) exp =
          (let property: J.property = kind in
           let ident_info: J.ident_info =
             match ident_info with
             | None  -> { used_stats = NA }
             | Some x -> x in
           {
             statement_desc =
               (Variable
                  { ident = v; value = (Some exp); property; ident_info });
             comment
           } : t)
        let int_switch ?comment  ?declaration  ?default  (e : J.expression)
          clauses =
          (match e.expression_desc with
           | Number (Int { i;_}) ->
               let continuation =
                 match List.find
                         (fun (x : _ J.case_clause)  ->
                            x.case = (Int32.to_int i)) clauses
                 with
                 | case -> fst case.body
                 | exception Not_found  ->
                     (match default with
                      | Some x -> x
                      | None  -> assert false) in
               (match (declaration, continuation) with
                | (Some
                   (kind,did),{
                                statement_desc = Exp
                                  {
                                    expression_desc = Bin
                                      (Eq
                                       ,{ expression_desc = Var (Id id);_},e0);_};_}::[])
                    when Ident.same did id -> define ?comment ~kind id e0
                | (Some (kind,did),_) ->
                    block ((declare_variable ?comment ~kind did) ::
                      continuation)
                | (None ,_) -> block continuation)
           | _ ->
               (match declaration with
                | Some (kind,did) ->
                    block
                      [declare_variable ?comment ~kind did;
                      {
                        statement_desc = (J.Int_switch (e, clauses, default));
                        comment
                      }]
                | None  ->
                    {
                      statement_desc = (J.Int_switch (e, clauses, default));
                      comment
                    }) : t)
        let string_switch ?comment  ?declaration  ?default 
          (e : J.expression) clauses =
          (match e.expression_desc with
           | Str (_,s) ->
               let continuation =
                 match List.find
                         (fun (x : string J.case_clause)  -> x.case = s)
                         clauses
                 with
                 | case -> fst case.body
                 | exception Not_found  ->
                     (match default with
                      | Some x -> x
                      | None  -> assert false) in
               (match (declaration, continuation) with
                | (Some
                   (kind,did),{
                                statement_desc = Exp
                                  {
                                    expression_desc = Bin
                                      (Eq
                                       ,{ expression_desc = Var (Id id);_},e0);_};_}::[])
                    when Ident.same did id -> define ?comment ~kind id e0
                | (Some (kind,did),_) ->
                    block @@ ((declare_variable ?comment ~kind did) ::
                      continuation)
                | (None ,_) -> block continuation)
           | _ ->
               (match declaration with
                | Some (kind,did) ->
                    block
                      [declare_variable ?comment ~kind did;
                      {
                        statement_desc =
                          (String_switch (e, clauses, default));
                        comment
                      }]
                | None  ->
                    {
                      statement_desc = (String_switch (e, clauses, default));
                      comment
                    }) : t)
        let rec if_ ?comment  ?declaration  ?else_  (e : J.expression)
          (then_ : J.block) =
          (let declared = ref false in
           let rec aux ?comment  (e : J.expression) (then_ : J.block)
             (else_ : J.block) acc =
             match ((e.expression_desc), then_, (else_ : J.block)) with
             | (_,{ statement_desc = Return { return_value = b;_};_}::[],
                { statement_desc = Return { return_value = a;_};_}::[]) ->
                 (return (E.econd e b a)) :: acc
             | (_,{
                    statement_desc = Exp
                      {
                        expression_desc = Bin
                          (Eq
                           ,({ expression_desc = Var (Id id0);_} as l0),a0);_};_}::[],
                {
                  statement_desc = Exp
                    {
                      expression_desc = Bin
                        (Eq ,{ expression_desc = Var (Id id1);_},b0);_};_}::[])
                 when Ident.same id0 id1 ->
                 (match declaration with
                  | Some (kind,did) when Ident.same did id0 ->
                      (declared := true;
                       (define ~kind id0 (E.econd e a0 b0))
                       ::
                       acc)
                  | _ -> (exp (E.assign l0 (E.econd e a0 b0))) :: acc)
             | (_,_,{ statement_desc = Exp { expression_desc = Number _ };_}::[])
                 -> aux ?comment e then_ [] acc
             | (_,{ statement_desc = Exp { expression_desc = Number _ };_}::[],_)
                 -> aux ?comment e [] else_ acc
             | (_,{ statement_desc = Exp b;_}::[],{ statement_desc = Exp a;_}::[])
                 -> (exp (E.econd e b a)) :: acc
             | (_,[],[]) -> (exp e) :: acc
             | (Not e,_,_::_) -> aux ?comment e else_ then_ acc
             | (_,[],_) -> aux ?comment (E.not e) else_ [] acc
             | (_,y::ys,x::xs) when
                 let open Js_analyzer in
                   (eq_statement x y) && (no_side_effect_expression e)
                 -> aux ?comment e ys xs (y :: acc)
             | (Number (Int { i = 0l;_}),_,_) ->
                 (match else_ with | [] -> acc | _ -> (block else_) :: acc)
             | (Number _,_,_)
               |(Bin
                 (Ge
                  ,{ expression_desc = Length _;_},{
                                                     expression_desc = Number
                                                       (Int { i = 0l;_})
                                                     }),_,_)
                 -> (block then_) :: acc
             | (Bin
                (Bor ,a,{ expression_desc = Number (Int { i = 0l;_}) }),_,_)
               |(Bin
                 (Bor ,{ expression_desc = Number (Int { i = 0l;_}) },a),_,_)
                 -> aux ?comment a then_ else_ acc
             | ((Bin
                 (EqEqEq ,{ expression_desc = Number (Int { i = 0l;_});_},e)
                 |Bin
                 (EqEqEq ,e,{ expression_desc = Number (Int { i = 0l;_});_})),_,else_)
                 -> aux ?comment e else_ then_ acc
             | ((Bin
                 (Gt
                  ,({ expression_desc = Length _;_} as e),{
                                                            expression_desc =
                                                              Number (Int
                                                              { i = 0l;_})
                                                            })|Int_of_boolean
                 e),_,_) -> aux ?comment e then_ else_ acc
             | _ ->
                 {
                   statement_desc =
                     (If
                        (e, then_,
                          ((match else_ with | [] -> None | v -> Some v))));
                   comment
                 } :: acc in
           let if_block =
             aux ?comment e then_
               (match else_ with | None  -> [] | Some v -> v) [] in
           match ((!declared), declaration) with
           | (true ,_)|(_,None ) -> block (List.rev if_block)
           | (false ,Some (kind,did)) ->
               block ((declare_variable ~kind did) :: (List.rev if_block)) : 
          t)
        let alias_variable ?comment  ?exp  (v : Ident.t) =
          ({
             statement_desc =
               (Variable
                  {
                    ident = v;
                    value = exp;
                    property = Alias;
                    ident_info = { used_stats = NA }
                  });
             comment
           } : t)
        let assign ?comment  id e =
          ({ statement_desc = (J.Exp (E.assign (E.var id) e)); comment } : 
          t)
        let assign_unit ?comment  id =
          ({ statement_desc = (J.Exp (E.assign (E.var id) E.unit)); comment } : 
          t)
        let declare_unit ?comment  id =
          ({
             statement_desc =
               (J.Variable
                  {
                    ident = id;
                    value = (Some E.unit);
                    property = Variable;
                    ident_info = { used_stats = NA }
                  });
             comment
           } : t)
        let rec while_ ?comment  ?label  ?env  (e : E.t) (st : J.block) =
          (match e with
           | { expression_desc = Int_of_boolean e;_} ->
               while_ ?comment ?label e st
           | _ ->
               let env =
                 match env with | None  -> Js_closure.empty () | Some x -> x in
               { statement_desc = (While (label, e, st, env)); comment } : 
          t)
        let for_ ?comment  ?env  for_ident_expression finish_ident_expression
          id direction (b : J.block) =
          (let env =
             match env with | None  -> Js_closure.empty () | Some x -> x in
           {
             statement_desc =
               (ForRange
                  (for_ident_expression, finish_ident_expression, id,
                    direction, b, env));
             comment
           } : t)
        let try_ ?comment  ?with_  ?finally  body =
          ({ statement_desc = (Try (body, with_, finally)); comment } : 
          t)
        let unknown_lambda ?(comment= "unknown")  (lam : Lambda.lambda) =
          (exp @@
             (E.str ~comment ~pure:false (Lam_util.string_of_lambda lam)) : 
          t)
        let continue ?comment  ?(label= "")  unit =
          ({ statement_desc = (J.Continue label); comment } : t)
        let debugger: t = { statement_desc = J.Debugger; comment = None }
      end 
    module Lam_compile_defs :
      sig
        [@@@ocaml.text
          " Type defintion to keep track of compilation state \n  "]
        [@@@ocaml.text
          " Some types are defined in this module to help avoiding generating unnecessary symbols \n    (generating too many symbols will make the output code unreadable)\n"]
        type jbl_label = int
        type value = {
          exit_id: Ident.t;
          args: Ident.t list;
          order_id: int;}
        type let_kind = Lambda.let_kind
        type st =
          | EffectCall
          | Declare of let_kind* J.ident
          | NeedValue
          | Assign of
          J.ident[@ocaml.doc
                   " when use [Assign], var is not needed, since it's already declared \n      make sure all [Assign] are declared first, otherwise you are creating global variables\n   "]
        type return_label =
          {
          id: Ident.t;
          label: J.label;
          params: Ident.t list;
          immutable_mask: bool array;
          mutable new_params: Ident.t Ident_map.t;
          mutable triggered: bool;}
        type return_type =
          | False
          | True of return_label option
        module HandlerMap : (Map.S with type  key =  jbl_label)
        type cxt =
          {
          st: st;
          should_return: return_type;
          jmp_table: value HandlerMap.t;
          meta: Lam_stats.meta;}
        val empty_handler_map : value HandlerMap.t
        val add_jmps :
          (Ident.t* (HandlerMap.key* 'a* Ident.t list) list) ->
            value HandlerMap.t -> (value HandlerMap.t* (int* 'a) list)
      end =
      struct
        type jbl_label = int
        module HandlerMap =
          Map.Make(struct
                     type t = jbl_label
                     let compare x y = compare (x : t) y
                   end)
        type value = {
          exit_id: Ident.t;
          args: Ident.t list;
          order_id: int;}
        type return_label =
          {
          id: Ident.t;
          label: J.label;
          params: Ident.t list;
          immutable_mask: bool array;
          mutable new_params: Ident.t Ident_map.t;
          mutable triggered: bool;}
        type return_type =
          | False
          | True of return_label option
        type let_kind = Lambda.let_kind
        type st =
          | EffectCall
          | Declare of let_kind* J.ident
          | NeedValue
          | Assign of J.ident
        type cxt =
          {
          st: st;
          should_return: return_type;
          jmp_table: value HandlerMap.t;
          meta: Lam_stats.meta;}
        let empty_handler_map = HandlerMap.empty
        let add_jmps (exit_id,code_table) (m : value HandlerMap.t) =
          let (map,_,handlers) =
            List.fold_left
              (fun (acc,prev_order_id,handlers)  ->
                 fun (l,lam,args)  ->
                   let order_id = prev_order_id + 1 in
                   ((HandlerMap.add l { exit_id; args; order_id } acc),
                     order_id, ((order_id, lam) :: handlers)))
              (m, (HandlerMap.cardinal m), []) code_table in
          (map, (List.rev handlers))
      end 
    module Int_map : sig include (Map.S with type  key =  int) end =
      struct
        include
          Map.Make(struct
                     type t = int
                     let compare (x : int) y = Pervasives.compare x y
                   end)
      end 
    module Ext_pp_scope :
      sig
        [@@@ocaml.text " Scope type to improve identifier name printing\n "]
        [@@@ocaml.text
          " Defines scope type [t], so that the pretty printer would print more beautiful code: \n    \n    print [identifer] instead of [identifier$1234] when it can\n "]
        type t
        val empty : t
        val add_ident : Ident.t -> t -> (int* t)
        val sub_scope : t -> Ident_set.t -> t
        val merge : Ident_set.t -> t -> t
        val print : Format.formatter -> t -> unit
      end =
      struct
        type t = int Int_map.t String_map.t
        let empty = String_map.empty
        let rec print fmt v =
          Format.fprintf fmt "@[<v>{";
          String_map.iter
            (fun k  ->
               fun m  -> Format.fprintf fmt "%s: @[%a@],@ " k print_int_map m)
            v;
          Format.fprintf fmt "}@]"
        and print_int_map fmt m =
          Int_map.iter (fun k  -> fun v  -> Format.fprintf fmt "%d - %d" k v)
            m
        let add_ident (id : Ident.t) (cxt : t) =
          (match String_map.find id.name cxt with
           | exception Not_found  ->
               (0,
                 (String_map.add id.name
                    (let open Int_map in add id.stamp 0 empty) cxt))
           | imap ->
               (match Int_map.find id.stamp imap with
                | exception Not_found  ->
                    let v = Int_map.cardinal imap in
                    (v,
                      (String_map.add id.name (Int_map.add id.stamp v imap)
                         cxt))
                | i -> (i, cxt)) : (int* t))
        let of_list lst cxt =
          List.fold_left (fun scope  -> fun i  -> snd (add_ident i scope))
            cxt lst
        let merge set cxt =
          Ident_set.fold
            (fun ident  -> fun acc  -> snd (add_ident ident acc)) set cxt
        let sub_scope (scope : t) ident_collection =
          (let cxt = empty in
           Ident_set.fold
             (fun (i : Ident.t)  ->
                fun acc  ->
                  match String_map.find i.name scope with
                  | exception Not_found  -> assert false
                  | imap ->
                      (match String_map.find i.name acc with
                       | exception Not_found  ->
                           String_map.add i.name imap acc
                       | _ -> acc)) ident_collection cxt : t)
      end 
    module Ext_pp :
      sig
        type t[@@ocaml.doc
                " A simple pretty printer\n    \n    Advantage compared with [Format], \n    [P.newline] does not screw the layout, have better control when do a newline (sicne JS has ASI)\n    Easy to tweak\n\n    {ul \n    {- be a little smarter}\n    {- buffer the last line, so that  we can do a smart newline, when it's really safe to do so}\n    }\n"]
        val indent_length : int
        val string : t -> string -> unit
        val space : t -> unit
        val nspace : t -> int -> unit
        val group : t -> int -> (unit -> 'a) -> 'a[@@ocaml.doc
                                                    " [group] will record current indentation \n    and indent futher\n "]
        val vgroup : t -> int -> (unit -> 'a) -> 'a
        val paren : t -> (unit -> 'a) -> 'a
        val brace : t -> (unit -> 'a) -> 'a
        val paren_group : t -> int -> (unit -> 'a) -> 'a
        val paren_vgroup : t -> int -> (unit -> 'a) -> 'a
        val brace_group : t -> int -> (unit -> 'a) -> 'a
        val brace_vgroup : t -> int -> (unit -> 'a) -> 'a
        val bracket_group : t -> int -> (unit -> 'a) -> 'a
        val bracket_vgroup : t -> int -> (unit -> 'a) -> 'a
        val newline : t -> unit
        val force_newline : t -> unit[@@ocaml.doc
                                       " [force_newline] Always print a newline "]
        val from_channel : out_channel -> t
        val from_buffer : Buffer.t -> t
        val flush : t -> unit -> unit
      end =
      struct
        module L = struct let space = " "
                          let indent_str = "  " end
        let indent_length = String.length L.indent_str
        type t =
          {
          output_string: string -> unit;
          output_char: char -> unit;
          flush: unit -> unit;
          mutable indent_level: int;
          mutable last_new_line: bool;}
        let from_channel chan =
          {
            output_string = (fun s  -> output_string chan s);
            output_char = (fun c  -> output_char chan c);
            flush = (fun _  -> flush chan);
            indent_level = 0;
            last_new_line = false
          }
        let from_buffer buf =
          {
            output_string = (fun s  -> Buffer.add_string buf s);
            output_char = (fun c  -> Buffer.add_char buf c);
            flush = (fun _  -> ());
            indent_level = 0;
            last_new_line = false
          }
        let string t s = t.output_string s; t.last_new_line <- false
        let newline t =
          if not t.last_new_line
          then
            (t.output_char '\n';
             for i = 0 to t.indent_level - 1 do t.output_string L.indent_str
             done;
             t.last_new_line <- true)
        let force_newline t =
          t.output_char '\n';
          for i = 0 to t.indent_level - 1 do t.output_string L.indent_str
          done
        let space t = string t L.space
        let nspace t n = string t (String.make n ' ')
        let group t i action =
          if i = 0
          then action ()
          else
            (let old = t.indent_level in
             t.indent_level <- t.indent_level + i;
             Ext_pervasives.finally () action
               (fun _  -> t.indent_level <- old))
        let vgroup = group
        let paren t action =
          string t "("; (let v = action () in string t ")"; v)
        let brace fmt u = string fmt "{"; (let v = u () in string fmt "}"; v)
        let bracket fmt u =
          string fmt "["; (let v = u () in string fmt "]"; v)
        let brace_vgroup st n action =
          string st "{";
          (let v =
             vgroup st n (fun _  -> newline st; (let v = action () in v)) in
           force_newline st; string st "}"; v)
        let bracket_vgroup st n action =
          string st "[";
          (let v =
             vgroup st n (fun _  -> newline st; (let v = action () in v)) in
           force_newline st; string st "]"; v)
        let bracket_group st n action =
          group st n (fun _  -> bracket st action)
        let paren_vgroup st n action =
          string st "(";
          (let v =
             group st n (fun _  -> newline st; (let v = action () in v)) in
           newline st; string st ")"; v)
        let paren_group st n action = group st n (fun _  -> paren st action)
        let brace_group st n action = group st n (fun _  -> brace st action)
        let indent t n = t.indent_level <- t.indent_level + n
        let flush t () = t.flush ()
      end 
    module Js_dump :
      sig
        [@@@ocaml.text " Print JS IR to vanilla Javascript code "]
        val pp_deps_program : J.deps_program -> Ext_pp.t -> unit
        val dump_deps_program : J.deps_program -> out_channel -> unit
        val string_of_block : J.block -> string[@@ocaml.doc
                                                 " 2 functions Only used for debugging "]
        val dump_program : J.program -> out_channel -> unit
        val string_of_expression : J.expression -> string
      end =
      struct
        module P = Ext_pp
        module E = Js_exp_make
        module S = Js_stmt_make
        module L =
          struct
            let function_ = "function"
            let var = "var"
            let return = "return"
            let eq = "="
            let require = "require"
            let goog_require = "goog.require"
            let goog_module = "goog.module"
            let lparen = "("
            let rparen = ")"
            let exports = "exports"
            let dot = "."
            let comma = ","
            let colon = ":"
            let throw = "throw"
            let default = "default"
            let length = "length"
            let char_code_at = "charCodeAt"
            let new_ = "new"
            let array = "Array"
            let question = "?"
            let plusplus = "++"
            let minusminus = "--"
            let semi = ";"
            let else_ = "else"
            let if_ = "if"
            let while_ = "while"
            let empty_block = "empty_block"
            let start_block = "start_block"
            let end_block = "end_block"
            let json = "JSON"
            let stringify = "stringify"
            let console = "console"
            let define = "define"
            let break = "break"
            let continue = "continue"
            let switch = "switch"
            let strict_directive = "'use strict';"
            let true_ = "true"
            let false_ = "false"
            let app = Literals.app
            let app_array = Literals.app_array
            let debugger = "debugger"
            let tag = "tag"
            let bind = "bind"
            let math = "Math"
            let apply = "apply"
            let null = "null"
            let string_cap = "String"
            let fromCharcode = "fromCharCode"
            let eq = "="
            let le = "<="
            let ge = ">="
            let plus_plus = "++"
            let minus_minus = "--"
            let caml_block = "Block"
            let caml_block_create = "__"
          end
        let return_indent = (String.length L.return) / Ext_pp.indent_length
        let throw_indent = (String.length L.throw) / Ext_pp.indent_length
        let semi f = P.string f L.semi
        let (op_prec,op_str) = let open Js_op_util in (op_prec, op_str)
        let best_string_quote s =
          let simple = ref 0 in
          let double = ref 0 in
          for i = 0 to (String.length s) - 1 do
            (match s.[i] with
             | '\'' -> incr simple
             | '"' -> incr double
             | _ -> ())
          done;
          if (!simple) < (!double) then '\'' else '"'
        let str_of_ident (cxt : Ext_pp_scope.t) (id : Ident.t) =
          if Ext_ident.is_js id
          then ((id.name), cxt)
          else
            (let name = Ext_ident.convert id.name in
             let (i,new_cxt) = Ext_pp_scope.add_ident id cxt in
             ((if i == 0 then name else Printf.sprintf "%s$%d" name i),
               new_cxt))[@@ocaml.doc
                          "\n   same as {!Js_dump.ident} except it generates a string instead of doing the printing\n"]
        let ident (cxt : Ext_pp_scope.t) f (id : Ident.t) =
          (let (str,cxt) = str_of_ident cxt id in P.string f str; cxt : 
          Ext_pp_scope.t)
        let array_str1 =
          Array.init 256 (fun i  -> String.make 1 (Char.chr i))[@@ocaml.doc
                                                                 " Avoid to allocate single char string too many times"]
        let array_conv =
          [|"0";"1";"2";"3";"4";"5";"6";"7";"8";"9";"a";"b";"c";"d";"e";"f"|]
          [@@ocaml.doc " For conveting \n \n"]
        let pp_string f ?(quote= '"')  ?(utf= false)  s =
          let pp_raw_string f ?(utf= false)  s =
            let l = String.length s in
            for i = 0 to l - 1 do
              let c = String.unsafe_get s i in
              match c with
              | '\b' -> P.string f "\\b"
              | '\012' -> P.string f "\\f"
              | '\n' -> P.string f "\\n"
              | '\r' -> P.string f "\\r"
              | '\t' -> P.string f "\\t"
              | '\000' when
                  (i = (l - 1)) ||
                    (let next = String.unsafe_get s (i + 1) in
                     (next < '0') || (next > '9'))
                  -> P.string f "\\0"
              | '\\' when not utf -> P.string f "\\\\"
              | '\000'..'\031'|'\127' ->
                  let c = Char.code c in
                  (P.string f "\\x";
                   P.string f (Array.unsafe_get array_conv (c lsr 4));
                   P.string f (Array.unsafe_get array_conv (c land 15)))
              | '\128'..'\255' when not utf ->
                  let c = Char.code c in
                  (P.string f "\\x";
                   P.string f (Array.unsafe_get array_conv (c lsr 4));
                   P.string f (Array.unsafe_get array_conv (c land 15)))
              | _ ->
                  (if c = quote then P.string f "\\";
                   P.string f (Array.unsafe_get array_str1 (Char.code c)))
            done in
          let quote_s = String.make 1 quote in
          P.string f quote_s; pp_raw_string f ~utf s; P.string f quote_s
        let pp_quote_string f s =
          pp_string f ~utf:false ~quote:(best_string_quote s) s
        let rec pp_function cxt (f : P.t) ?name  return (l : Ident.t list)
          (b : J.block) (env : Js_fun_env.t) =
          match (b, (name, return)) with
          | ({
               statement_desc = Return
                 {
                   return_value =
                     {
                       expression_desc = Call
                         ({ expression_desc = Var v;_},ls,{ arity = Full ;
                                                            call_info =
                                                              (Call_builtin_runtime
                                                               |Call_ml )
                                                            })
                       }
                   }
               }::[],((_,false )|(None ,true )))
              when
              Ext_list.for_all2_no_exn
                (fun a  ->
                   fun b  ->
                     match b.J.expression_desc with
                     | Var (Id i) -> Ident.same a i
                     | _ -> false) l ls
              ->
              (match name with
               | Some i ->
                   (P.string f L.var;
                    P.space f;
                    (let cxt = ident cxt f i in
                     P.space f; P.string f L.eq; P.space f; vident cxt f v))
               | None  ->
                   (if return then (P.string f L.return; P.space f);
                    vident cxt f v))
          | (_,_) ->
              let ipp_ident cxt f id un_used =
                if un_used
                then ident cxt f (Ext_ident.make_unused ())
                else ident cxt f id in
              let rec formal_parameter_list cxt (f : P.t) l =
                let rec aux i cxt l =
                  match l with
                  | [] -> cxt
                  | id::[] ->
                      ipp_ident cxt f id (Js_fun_env.get_unused env i)
                  | id::r ->
                      let cxt =
                        ipp_ident cxt f id (Js_fun_env.get_unused env i) in
                      (P.string f L.comma; P.space f; aux (i + 1) cxt r) in
                match l with
                | [] -> cxt
                | i::[] ->
                    if Js_fun_env.get_unused env 0
                    then cxt
                    else ident cxt f i
                | _ -> aux 0 cxt l in
              let rec aux cxt f ls =
                match ls with
                | [] -> cxt
                | x::[] -> ident cxt f x
                | y::ys ->
                    let cxt = ident cxt f y in
                    (P.string f L.comma; aux cxt f ys) in
              let set_env =
                match name with
                | None  -> Js_fun_env.get_bound env
                | Some id -> Ident_set.add id (Js_fun_env.get_bound env) in
              let outer_cxt = Ext_pp_scope.merge set_env cxt in
              let inner_cxt = Ext_pp_scope.sub_scope outer_cxt set_env in
              let action return =
                if return then (P.string f L.return; P.space f);
                P.string f L.function_;
                P.space f;
                (match name with
                 | None  -> ()
                 | Some x -> ignore (ident inner_cxt f x));
                (let body_cxt =
                   P.paren_group f 1
                     (fun _  -> formal_parameter_list inner_cxt f l) in
                 P.space f;
                 ignore @@
                   (P.brace_vgroup f 1
                      (fun _  -> statement_list false body_cxt f b))) in
              let lexical = Js_fun_env.get_lexical_scope env in
              let enclose action lexical return =
                if Ident_set.is_empty lexical
                then action return
                else
                  (let lexical = Ident_set.elements lexical in
                   if return then (P.string f L.return; P.space f);
                   P.string f L.lparen;
                   P.string f L.function_;
                   P.string f L.lparen;
                   ignore @@ (aux inner_cxt f lexical);
                   P.string f L.rparen;
                   P.brace_vgroup f 0 (fun _  -> action true);
                   P.string f L.lparen;
                   ignore @@ (aux inner_cxt f lexical);
                   P.string f L.rparen;
                   P.string f L.rparen) in
              (enclose action lexical return; outer_cxt)
        and output_one :
          'a . _ -> P.t -> (P.t -> 'a -> unit) -> 'a J.case_clause -> _=
          fun cxt  ->
            fun f  ->
              fun pp_cond  ->
                fun ({ case = e; body = (sl,break) } : _ J.case_clause)  ->
                  let cxt =
                    (P.group f 1) @@
                      (fun _  ->
                         (P.group f 1) @@
                           ((fun _  ->
                               P.string f "case ";
                               pp_cond f e;
                               P.space f;
                               P.string f L.colon));
                         P.space f;
                         (P.group f 1) @@
                           ((fun _  ->
                               let cxt =
                                 match sl with
                                 | [] -> cxt
                                 | _ ->
                                     (P.newline f;
                                      statement_list false cxt f sl) in
                               if break
                               then (P.newline f; P.string f L.break; semi f);
                               cxt))) in
                  P.newline f; cxt
        and loop :
          'a .
            Ext_pp_scope.t ->
              P.t ->
                (P.t -> 'a -> unit) ->
                  'a J.case_clause list -> Ext_pp_scope.t=
          fun cxt  ->
            fun f  ->
              fun pp_cond  ->
                fun cases  ->
                  match cases with
                  | [] -> cxt
                  | x::[] -> output_one cxt f pp_cond x
                  | x::xs ->
                      let cxt = output_one cxt f pp_cond x in
                      loop cxt f pp_cond xs
        and vident cxt f (v : J.vident) =
          match v with
          | Id v|Qualified (v,_,None ) -> ident cxt f v
          | Qualified (id,_,Some name) ->
              let cxt = ident cxt f id in
              (P.string f L.dot; P.string f (Ext_ident.convert name); cxt)
        and expression l cxt f (exp : J.expression) =
          (pp_comment_option f exp.comment;
           expression_desc cxt l f exp.expression_desc : Ext_pp_scope.t)
        and expression_desc cxt (l : int) f x =
          (match x with
           | Var v -> vident cxt f v
           | Bool b ->
               (if b then P.string f L.true_ else P.string f L.false_; cxt)
           | Seq (e1,e2) ->
               let action () =
                 let cxt = expression 0 cxt f e1 in
                 P.string f L.comma; P.space f; expression 0 cxt f e2 in
               if l > 0 then P.paren_group f 1 action else action ()
           | Fun (l,b,env) -> pp_function cxt f false l b env
           | Call (e,el,info) ->
               let action () =
                 P.group f 1
                   (fun _  ->
                      match (info, el) with
                      | ({ arity = Full  },_)|(_,[]) ->
                          let cxt = expression 15 cxt f e in
                          P.paren_group f 1 (fun _  -> arguments cxt f el)
                      | (_,_) ->
                          (P.string f Js_config.curry;
                           P.string f L.dot;
                           (let len = List.length el in
                            if (1 <= len) && (len <= 8)
                            then
                              (P.string f L.app;
                               P.string f (Printf.sprintf "%d" len);
                               P.paren_group f 1
                                 (fun _  -> arguments cxt f (e :: el)))
                            else
                              (P.string f L.app_array;
                               P.paren_group f 1
                                 (fun _  ->
                                    arguments cxt f [e; E.arr Mutable el]))))) in
               if l > 15 then P.paren_group f 1 action else action ()
           | Bind (a,b) ->
               expression_desc cxt l f
                 (Call
                    ({
                       expression_desc = (Dot (a, L.bind, true));
                       comment = None
                     }, [b], { arity = Full; call_info = Call_na }))
           | FlatCall (e,el) ->
               P.group f 1
                 (fun _  ->
                    let cxt = expression 15 cxt f e in
                    P.string f L.dot;
                    P.string f L.apply;
                    P.paren_group f 1
                      (fun _  ->
                         P.string f L.null;
                         P.string f L.comma;
                         P.space f;
                         expression 1 cxt f el))
           | String_of_small_int_array ({ expression_desc = desc } as e) ->
               let action () =
                 P.group f 1
                   (fun _  ->
                      P.string f L.string_cap;
                      P.string f L.dot;
                      P.string f L.fromCharcode;
                      (match desc with
                       | Array (el,_mutable) ->
                           P.paren_group f 1 (fun _  -> arguments cxt f el)
                       | _ ->
                           (P.string f L.dot;
                            P.string f L.apply;
                            P.paren_group f 1
                              (fun _  ->
                                 P.string f L.null;
                                 P.string f L.comma;
                                 expression 1 cxt f e)))) in
               if l > 15 then P.paren_group f 1 action else action ()
           | Array_append (e,el) ->
               P.group f 1
                 (fun _  ->
                    let cxt = expression 15 cxt f e in
                    P.string f ".concat";
                    P.paren_group f 1 (fun _  -> arguments cxt f [el]))
           | Array_copy e ->
               P.group f 1
                 (fun _  ->
                    let cxt = expression 15 cxt f e in
                    P.string f ".slice"; P.string f "()"; cxt)
           | Dump (level,el) ->
               let obj =
                 match level with
                 | Log  -> "log"
                 | Info  -> "info"
                 | Warn  -> "warn"
                 | Error  -> "error" in
               P.group f 1
                 (fun _  ->
                    P.string f L.console;
                    P.string f L.dot;
                    P.string f obj;
                    P.paren_group f 1 (fun _  -> arguments cxt f el))
           | Json_stringify e ->
               P.group f 1
                 (fun _  ->
                    P.string f L.json;
                    P.string f L.dot;
                    P.string f L.stringify;
                    P.paren_group f 1 (fun _  -> expression 0 cxt f e))
           | Char_to_int e ->
               (match e.expression_desc with
                | String_access (a,b) ->
                    P.group f 1
                      (fun _  ->
                         let cxt = expression 15 cxt f a in
                         P.string f L.dot;
                         P.string f L.char_code_at;
                         P.paren_group f 1 (fun _  -> expression 0 cxt f b))
                | _ ->
                    P.group f 1
                      (fun _  ->
                         let cxt = expression 15 cxt f e in
                         P.string f L.dot;
                         P.string f L.char_code_at;
                         P.string f "(0)";
                         cxt))
           | Char_of_int e ->
               P.group f 1
                 (fun _  ->
                    P.string f L.string_cap;
                    P.string f L.dot;
                    P.string f L.fromCharcode;
                    P.paren_group f 1 (fun _  -> arguments cxt f [e]))
           | Math (name,el) ->
               P.group f 1
                 (fun _  ->
                    P.string f L.math;
                    P.string f L.dot;
                    P.string f name;
                    P.paren_group f 1 (fun _  -> arguments cxt f el))
           | Str (_,s) ->
               let quote = best_string_quote s in (pp_string f ~quote s; cxt)
           | Raw_js_code (s,info) ->
               (match info with
                | Exp _ ->
                    (P.string f "("; P.string f s; P.string f ")"; cxt)
                | Stmt  -> (P.newline f; P.string f s; P.newline f; cxt))
           | Number v ->
               let s =
                 match v with
                 | Float { f = v } ->
                     Js_number.caml_float_literal_to_js_string v
                 | Int { i = v;_} -> Int32.to_string v
                 | Uint i -> Format.asprintf "%lu" i
                 | Nint i -> Nativeint.to_string i in
               let need_paren =
                 if (s.[0]) = '-'
                 then l > 13
                 else (l = 15) && (((s.[0]) <> 'I') && ((s.[0]) <> 'N')) in
               let action _ = P.string f s in
               (if need_paren then P.paren f action else action (); cxt)
           | J.Anything_to_number e|Int_of_boolean e ->
               let action () =
                 (P.group f 0) @@
                   (fun _  -> P.string f "+"; expression 13 cxt f e) in
               if l > 12 then P.paren_group f 1 action else action ()
           | Not e ->
               let action () = P.string f "!"; expression 13 cxt f e in
               if l > 13 then P.paren_group f 1 action else action ()
           | Typeof e ->
               (P.string f "typeof"; P.space f; expression 13 cxt f e)
           | Caml_block_set_tag (a,b) ->
               expression_desc cxt l f
                 (Bin
                    (Eq,
                      { expression_desc = (Caml_block_tag a); comment = None
                      }, b))
           | Caml_block_set_length (a,b) ->
               expression_desc cxt l f
                 (Bin
                    (Eq,
                      {
                        expression_desc = (Length (a, Caml_block));
                        comment = None
                      }, b))
           | Bin
               (Eq
                ,{ expression_desc = Var i },{
                                               expression_desc =
                                                 (Bin
                                                  ((Plus  as op),{
                                                                   expression_desc
                                                                    = Var j
                                                                   },delta)
                                                  |Bin
                                                  ((Plus  as op),delta,
                                                   { expression_desc = Var j
                                                     })|Bin
                                                  ((Minus  as op),{
                                                                    expression_desc
                                                                    = Var j },delta))
                                               })
               when Js_op_util.same_vident i j ->
               (match (delta, op) with
                | ({ expression_desc = Number (Int { i = 1l;_}) },Plus )
                  |({ expression_desc = Number (Int { i = (-1l);_}) },Minus )
                    -> (P.string f L.plusplus; P.space f; vident cxt f i)
                | ({ expression_desc = Number (Int { i = (-1l);_}) },Plus )
                  |({ expression_desc = Number (Int { i = 1l;_}) },Minus ) ->
                    (P.string f L.minusminus; P.space f; vident cxt f i)
                | (_,_) ->
                    let cxt = vident cxt f i in
                    (P.space f;
                     if op = Plus then P.string f "+=" else P.string f "-=";
                     P.space f;
                     expression 13 cxt f delta))
           | Bin
               (Eq
                ,{
                   expression_desc = Access
                     ({ expression_desc = Var i;_},{
                                                     expression_desc = Number
                                                       (Int { i = k0 })
                                                     })
                   },{
                       expression_desc =
                         (Bin
                          ((Plus  as op),{
                                           expression_desc = Access
                                             ({ expression_desc = Var j;_},
                                              {
                                                expression_desc = Number (Int
                                                  { i = k1 })
                                                });_},delta)|Bin
                          ((Plus  as op),delta,{
                                                 expression_desc = Access
                                                   ({
                                                      expression_desc = Var j;_},
                                                    {
                                                      expression_desc =
                                                        Number (Int
                                                        { i = k1 })
                                                      });_})|Bin
                          ((Minus  as op),{
                                            expression_desc = Access
                                              ({ expression_desc = Var j;_},
                                               {
                                                 expression_desc = Number
                                                   (Int { i = k1 })
                                                 });_},delta))
                       })
               when (k0 = k1) && (Js_op_util.same_vident i j) ->
               let aux cxt f vid i =
                 let cxt = vident cxt f vid in
                 P.string f "[";
                 P.string f (Int32.to_string i);
                 P.string f "]";
                 cxt in
               (match (delta, op) with
                | ({ expression_desc = Number (Int { i = 1l;_}) },Plus )
                  |({ expression_desc = Number (Int { i = (-1l);_}) },Minus )
                    -> (P.string f L.plusplus; P.space f; aux cxt f i k0)
                | ({ expression_desc = Number (Int { i = (-1l);_}) },Plus )
                  |({ expression_desc = Number (Int { i = 1l;_}) },Minus ) ->
                    (P.string f L.minusminus; P.space f; aux cxt f i k0)
                | (_,_) ->
                    let cxt = aux cxt f i k0 in
                    (P.space f;
                     if op = Plus then P.string f "+=" else P.string f "-=";
                     P.space f;
                     expression 13 cxt f delta))
           | Anything_to_string e ->
               expression_desc cxt l f
                 (Bin
                    (Plus,
                      { expression_desc = (Str (true, "")); comment = None },
                      e))
           | Bin
               (Minus
                ,{
                   expression_desc = Number
                     (Int { i = 0l;_}|Float { f = "0." })
                   },e)
               ->
               let action () = P.string f "-"; expression 13 cxt f e in
               if l > 13 then P.paren_group f 1 action else action ()
           | Bin (op,e1,e2) ->
               let (out,lft,rght) = op_prec op in
               let need_paren =
                 (l > out) ||
                   (match op with | Lsl |Lsr |Asr  -> true | _ -> false) in
               let action () =
                 let cxt = expression lft cxt f e1 in
                 P.space f;
                 P.string f (op_str op);
                 P.space f;
                 expression rght cxt f e2 in
               if need_paren then P.paren_group f 1 action else action ()
           | String_append (e1,e2) ->
               let op: Js_op.binop = Plus in
               let (out,lft,rght) = op_prec op in
               let need_paren =
                 (l > out) ||
                   (match op with | Lsl |Lsr |Asr  -> true | _ -> false) in
               let action () =
                 let cxt = expression lft cxt f e1 in
                 P.space f;
                 P.string f "+";
                 P.space f;
                 expression rght cxt f e2 in
               if need_paren then P.paren_group f 1 action else action ()
           | Array (el,_) ->
               (match el with
                | []|_::[] ->
                    (P.bracket_group f 1) @@
                      ((fun _  -> array_element_list cxt f el))
                | _ ->
                    (P.bracket_vgroup f 1) @@
                      ((fun _  -> array_element_list cxt f el)))
           | Caml_uninitialized_obj (tag,size) ->
               expression_desc cxt l f (Object [(Length, size); (Tag, tag)])
           | Caml_block (el,mutable_flag,tag,tag_info) ->
               (match ((tag.expression_desc), tag_info) with
                | (Number (Int
                   { i = 0l;_}),(Blk_tuple |Blk_array |Blk_variant _
                                 |Blk_record _|Blk_na |Blk_module _
                                 |Blk_constructor (_,1)))
                    -> expression_desc cxt l f (Array (el, mutable_flag))
                | (_,_) ->
                    (P.string f L.caml_block;
                     P.string f L.dot;
                     P.string f L.caml_block_create;
                     P.paren_group f 1
                       (fun _  ->
                          arguments cxt f [tag; E.arr mutable_flag el])))
           | Caml_block_tag e ->
               P.group f 1
                 (fun _  ->
                    let cxt = expression 15 cxt f e in
                    P.string f L.dot; P.string f L.tag; cxt)
           | Access (e,e')|String_access (e,e') ->
               let action () =
                 (P.group f 1) @@
                   (fun _  ->
                      let cxt = expression 15 cxt f e in
                      (P.bracket_group f 1) @@
                        (fun _  -> expression 0 cxt f e')) in
               if l > 15 then P.paren_group f 1 action else action ()
           | Length (e,_) ->
               let action () =
                 let cxt = expression 15 cxt f e in
                 P.string f L.dot; P.string f L.length; cxt in
               if l > 15 then P.paren_group f 1 action else action ()
           | Dot (e,nm,normal) ->
               if normal
               then
                 let action () =
                   let cxt = expression 15 cxt f e in
                   P.string f L.dot; P.string f (Ext_ident.convert nm); cxt in
                 (if l > 15 then P.paren_group f 1 action else action ())
               else
                 (let action () =
                    (P.group f 1) @@
                      (fun _  ->
                         let cxt = expression 15 cxt f e in
                         (P.bracket_group f 1) @@
                           ((fun _  ->
                               pp_string f ~quote:(best_string_quote nm) nm));
                         cxt) in
                  if l > 15 then P.paren_group f 1 action else action ())
           | New (e,el) ->
               let action () =
                 (P.group f 1) @@
                   (fun _  ->
                      P.string f L.new_;
                      P.space f;
                      (let cxt = expression 16 cxt f e in
                       (P.paren_group f 1) @@
                         (fun _  ->
                            match el with
                            | Some el -> arguments cxt f el
                            | None  -> cxt))) in
               if l > 15 then P.paren_group f 1 action else action ()
           | Array_of_size e ->
               let action () =
                 (P.group f 1) @@
                   (fun _  ->
                      P.string f L.new_;
                      P.space f;
                      P.string f L.array;
                      (P.paren_group f 1) @@
                        ((fun _  -> expression 0 cxt f e))) in
               if l > 15 then P.paren_group f 1 action else action ()
           | Cond (e,e1,e2) ->
               let action () =
                 let cxt = expression 3 cxt f e in
                 P.space f;
                 P.string f L.question;
                 P.space f;
                 (let cxt =
                    (P.group f 1) @@ (fun _  -> expression 3 cxt f e1) in
                  P.space f;
                  P.string f L.colon;
                  P.space f;
                  (P.group f 1) @@ ((fun _  -> expression 3 cxt f e2))) in
               if l > 2 then P.paren_vgroup f 1 action else action ()
           | Object lst ->
               (P.brace_vgroup f 1) @@
                 ((fun _  -> property_name_and_value_list cxt f lst)) : 
          Ext_pp_scope.t)
        and property_name cxt f (s : J.property_name) =
          (match s with
           | Tag  -> P.string f L.tag
           | Length  -> P.string f L.length
           | Key s -> pp_string f ~utf:true ~quote:(best_string_quote s) s
           | Int_key i -> P.string f (string_of_int i) : unit)
        and property_name_and_value_list cxt f l =
          (match l with
           | [] -> cxt
           | (pn,e)::[] ->
               (property_name cxt f pn;
                P.string f L.colon;
                P.space f;
                expression 1 cxt f e)
           | (pn,e)::r ->
               (property_name cxt f pn;
                P.string f L.colon;
                P.space f;
                (let cxt = expression 1 cxt f e in
                 P.string f L.comma;
                 P.newline f;
                 property_name_and_value_list cxt f r)) : Ext_pp_scope.t)
        and array_element_list cxt f el =
          (match el with
           | [] -> cxt
           | e::[] -> expression 1 cxt f e
           | e::r ->
               let cxt = expression 1 cxt f e in
               (P.string f L.comma; P.newline f; array_element_list cxt f r) : 
          Ext_pp_scope.t)
        and arguments cxt f l =
          (match l with
           | [] -> cxt
           | e::[] -> expression 1 cxt f e
           | e::r ->
               let cxt = expression 1 cxt f e in
               (P.string f L.comma; P.space f; arguments cxt f r) : Ext_pp_scope.t)
        and variable_declaration top cxt f
          (variable : J.variable_declaration) =
          (match variable with
           | { ident = i; value = None ; ident_info;_} ->
               if ident_info.used_stats = Dead_pure
               then cxt
               else
                 (P.string f L.var;
                  P.space f;
                  (let cxt = ident cxt f i in semi f; cxt))
           | { ident = i; value = Some e; ident_info = { used_stats;_} } ->
               (match used_stats with
                | Dead_pure  -> cxt
                | Dead_non_pure  -> statement_desc top cxt f (J.Exp e)
                | _ ->
                    (match (e, top) with
                     | ({ expression_desc = Fun (params,b,env); comment = _ },true
                        ) -> pp_function cxt f ~name:i false params b env
                     | (_,_) ->
                         (P.string f L.var;
                          P.space f;
                          (let cxt = ident cxt f i in
                           P.space f;
                           P.string f L.eq;
                           P.space f;
                           (let cxt = expression 1 cxt f e in semi f; cxt))))) : 
          Ext_pp_scope.t)
        and ipp_comment : 'a . P.t -> 'a -> unit=
          fun f  -> fun comment  -> ()
        and pp_comment f comment =
          if (String.length comment) > 0 then P.string f "/* ";
          P.string f comment;
          P.string f " */"[@@ocaml.text
                            " don't print a new line -- ASI \n    FIXME: this still does not work in some cases...\n    {[\n    return /* ... */\n    [... ]\n    ]}\n"]
        and pp_comment_option f comment =
          match comment with | None  -> () | Some x -> pp_comment f x
        and statement top cxt f
          ({ statement_desc = s; comment;_} : J.statement) =
          (pp_comment_option f comment; statement_desc top cxt f s : 
          Ext_pp_scope.t)
        and statement_desc top cxt f (s : J.statement_desc) =
          (match s with
           | Block [] -> (ipp_comment f L.empty_block; cxt)
           | Block b ->
               (ipp_comment f L.start_block;
                (let cxt = statement_list top cxt f b in
                 ipp_comment f L.end_block; cxt))
           | Variable l -> variable_declaration top cxt f l
           | Exp { expression_desc = Var _ } -> (semi f; cxt)
           | Exp e ->
               let rec need_paren (e : J.expression) =
                 match e.expression_desc with
                 | Call ({ expression_desc = Fun _ },_,_) -> true
                 | Caml_uninitialized_obj _|Raw_js_code (_,Exp _)|Fun _
                   |Object _ -> true
                 | Raw_js_code (_,Stmt )|Caml_block_set_tag _|Length _
                   |Caml_block_set_length _|Anything_to_string _
                   |String_of_small_int_array _|Call _|Array_append _
                   |Array_copy _|Caml_block_tag _|Seq _|Dot _|Cond _|Bin _
                   |String_access _|Access _|Array_of_size _|String_append _
                   |Char_of_int _|Char_to_int _|Dump _|Json_stringify _|Math
                   _|Var _|Str _|Array _|Caml_block _|FlatCall _|Typeof _
                   |Bind _|Number _|Not _|Bool _|New _|J.Anything_to_number _
                   |Int_of_boolean _ -> false in
               let cxt =
                 (if need_paren e then P.paren_group f 1 else P.group f 0)
                   (fun _  -> expression 0 cxt f e) in
               (semi f; cxt)
           | If (e,s1,s2) ->
               (P.string f L.if_;
                P.space f;
                (let cxt =
                   (P.paren_group f 1) @@ (fun _  -> expression 0 cxt f e) in
                 P.space f;
                 (let cxt = block cxt f s1 in
                  match s2 with
                  | None |Some []|Some ({ statement_desc = Block [] }::[]) ->
                      (P.newline f; cxt)
                  | Some (({ statement_desc = If _ } as nest)::[])|Some
                    ({
                       statement_desc = Block
                         (({ statement_desc = If _;_} as nest)::[]);_}::[])
                      ->
                      (P.newline f;
                       P.string f L.else_;
                       P.space f;
                       statement false cxt f nest)
                  | Some s2 ->
                      (P.newline f;
                       P.string f L.else_;
                       P.space f;
                       block cxt f s2))))
           | While (label,e,s,_env) ->
               ((match label with
                 | Some i -> (P.string f i; P.string f L.colon; P.newline f)
                 | None  -> ());
                (let cxt =
                   match e.expression_desc with
                   | Number (Int { i = 1l }) ->
                       (P.string f L.while_;
                        P.string f "(";
                        P.string f L.true_;
                        P.string f ")";
                        P.space f;
                        cxt)
                   | _ ->
                       (P.string f L.while_;
                        (let cxt =
                           (P.paren_group f 1) @@
                             (fun _  -> expression 0 cxt f e) in
                         P.space f; cxt)) in
                 let cxt = block cxt f s in semi f; cxt))
           | ForRange (for_ident_expression,finish,id,direction,s,env) ->
               let action cxt =
                 (P.vgroup f 0) @@
                   (fun _  ->
                      let cxt =
                        (P.group f 0) @@
                          (fun _  ->
                             P.string f "for";
                             (P.paren_group f 1) @@
                               ((fun _  ->
                                   let (cxt,new_id) =
                                     match (for_ident_expression,
                                             (finish.expression_desc))
                                     with
                                     | (Some
                                        ident_expression,(Number _|Var _)) ->
                                         (P.string f L.var;
                                          P.space f;
                                          (let cxt = ident cxt f id in
                                           P.space f;
                                           P.string f L.eq;
                                           P.space f;
                                           ((expression 0 cxt f
                                               ident_expression), None)))
                                     | (Some ident_expression,_) ->
                                         (P.string f L.var;
                                          P.space f;
                                          (let cxt = ident cxt f id in
                                           P.space f;
                                           P.string f L.eq;
                                           P.space f;
                                           (let cxt =
                                              expression 1 cxt f
                                                ident_expression in
                                            P.space f;
                                            P.string f L.comma;
                                            (let id =
                                               Ext_ident.create
                                                 ((Ident.name id) ^ "_finish") in
                                             let cxt = ident cxt f id in
                                             P.space f;
                                             P.string f L.eq;
                                             P.space f;
                                             ((expression 1 cxt f finish),
                                               (Some id))))))
                                     | (None ,(Number _|Var _)) ->
                                         (cxt, None)
                                     | (None ,_) ->
                                         (P.string f L.var;
                                          P.space f;
                                          (let id =
                                             Ext_ident.create
                                               ((Ident.name id) ^ "_finish") in
                                           let cxt = ident cxt f id in
                                           P.space f;
                                           P.string f L.eq;
                                           P.space f;
                                           ((expression 15 cxt f finish),
                                             (Some id)))) in
                                   semi f;
                                   P.space f;
                                   (let cxt = ident cxt f id in
                                    P.space f;
                                    (let right_prec =
                                       match direction with
                                       | Upto  ->
                                           let (_,_,right) = op_prec Le in
                                           (P.string f L.le; right)
                                       | Downto  ->
                                           let (_,_,right) = op_prec Ge in
                                           (P.string f L.ge; right) in
                                     P.space f;
                                     (let cxt =
                                        match new_id with
                                        | Some i ->
                                            expression right_prec cxt f
                                              (E.var i)
                                        | None  ->
                                            expression right_prec cxt f
                                              finish in
                                      semi f;
                                      P.space f;
                                      (let () =
                                         match direction with
                                         | Upto  -> P.string f L.plus_plus
                                         | Downto  ->
                                             P.string f L.minus_minus in
                                       ident cxt f id))))))) in
                      block cxt f s) in
               let lexical = Js_closure.get_lexical_scope env in
               if Ident_set.is_empty lexical
               then action cxt
               else
                 (let inner_cxt = Ext_pp_scope.merge lexical cxt in
                  let lexical = Ident_set.elements lexical in
                  let _enclose action inner_cxt lexical =
                    let rec aux cxt f ls =
                      match ls with
                      | [] -> cxt
                      | x::[] -> ident cxt f x
                      | y::ys ->
                          let cxt = ident cxt f y in
                          (P.string f L.comma; aux cxt f ys) in
                    P.vgroup f 0
                      (fun _  ->
                         P.string f "(function(";
                         ignore @@ (aux inner_cxt f lexical);
                         P.string f ")";
                         (let cxt =
                            P.brace_vgroup f 0 (fun _  -> action inner_cxt) in
                          P.string f "(";
                          ignore @@ (aux inner_cxt f lexical);
                          P.string f ")";
                          P.string f ")";
                          semi f;
                          cxt)) in
                  _enclose action inner_cxt lexical)
           | Continue s ->
               (P.string f L.continue;
                P.space f;
                P.string f s;
                semi f;
                P.newline f;
                cxt)
           | Debugger  ->
               (P.newline f; P.string f L.debugger; semi f; P.newline f; cxt)
           | Break  ->
               (P.string f L.break; P.space f; semi f; P.newline f; cxt)
           | Return { return_value = e } ->
               (match e with
                | { expression_desc = Fun (l,b,env);_} ->
                    let cxt = pp_function cxt f true l b env in (semi f; cxt)
                | e ->
                    (P.string f L.return;
                     P.space f;
                     (P.group f return_indent) @@
                       ((fun _  ->
                           let cxt = expression 0 cxt f e in semi f; cxt))))
           | Int_switch (e,cc,def) ->
               (P.string f L.switch;
                P.space f;
                (let cxt =
                   (P.paren_group f 1) @@ (fun _  -> expression 0 cxt f e) in
                 P.space f;
                 (P.brace_vgroup f 1) @@
                   ((fun _  ->
                       let cxt =
                         loop cxt f
                           (fun f  -> fun i  -> P.string f (string_of_int i))
                           cc in
                       match def with
                       | None  -> cxt
                       | Some def ->
                           (P.group f 1) @@
                             ((fun _  ->
                                 P.string f L.default;
                                 P.string f L.colon;
                                 P.newline f;
                                 statement_list false cxt f def))))))
           | String_switch (e,cc,def) ->
               (P.string f L.switch;
                P.space f;
                (let cxt =
                   (P.paren_group f 1) @@ (fun _  -> expression 0 cxt f e) in
                 P.space f;
                 (P.brace_vgroup f 1) @@
                   ((fun _  ->
                       let cxt =
                         loop cxt f (fun f  -> fun i  -> pp_quote_string f i)
                           cc in
                       match def with
                       | None  -> cxt
                       | Some def ->
                           (P.group f 1) @@
                             ((fun _  ->
                                 P.string f L.default;
                                 P.string f L.colon;
                                 P.newline f;
                                 statement_list false cxt f def))))))
           | Throw e ->
               (P.string f L.throw;
                P.space f;
                (P.group f throw_indent) @@
                  ((fun _  -> let cxt = expression 0 cxt f e in semi f; cxt)))
           | Try (b,ctch,fin) ->
               (P.vgroup f 0) @@
                 ((fun _  ->
                     P.string f "try";
                     P.space f;
                     (let cxt = block cxt f b in
                      let cxt =
                        match ctch with
                        | None  -> cxt
                        | Some (i,b) ->
                            (P.newline f;
                             P.string f "catch (";
                             (let cxt = ident cxt f i in
                              P.string f ")"; block cxt f b)) in
                      match fin with
                      | None  -> cxt
                      | Some b ->
                          (P.group f 1) @@
                            ((fun _  ->
                                P.string f "finally";
                                P.space f;
                                block cxt f b))))) : Ext_pp_scope.t)
        and statement_list top cxt f b =
          match b with
          | [] -> cxt
          | s::[] -> statement top cxt f s
          | s::r ->
              let cxt = statement top cxt f s in
              (P.newline f;
               if top then P.force_newline f;
               statement_list top cxt f r)
        and block cxt f b =
          P.brace_vgroup f 1 (fun _  -> statement_list false cxt f b)
        let exports cxt f (idents : Ident.t list) =
          let (outer_cxt,reversed_list,margin) =
            List.fold_left
              (fun (cxt,acc,len)  ->
                 fun (id : Ident.t)  ->
                   let s = Ext_ident.convert id.name in
                   let (str,cxt) = str_of_ident cxt id in
                   (cxt, ((s, str) :: acc), (max len (String.length s))))
              (cxt, [], 0) idents in
          P.newline f;
          Ext_list.rev_iter
            (fun (s,export)  ->
               (P.group f 0) @@
                 ((fun _  ->
                     P.string f L.exports;
                     P.string f L.dot;
                     P.string f s;
                     P.nspace f ((margin - (String.length s)) + 1);
                     P.string f L.eq;
                     P.space f;
                     P.string f export;
                     semi f));
               P.newline f) reversed_list;
          outer_cxt
        let requires require_lit cxt f (modules : (Ident.t* string) list) =
          P.newline f;
          (let (outer_cxt,reversed_list,margin) =
             List.fold_left
               (fun (cxt,acc,len)  ->
                  fun (id,s)  ->
                    let (str,cxt) = str_of_ident cxt id in
                    (cxt, ((str, s) :: acc), (max len (String.length str))))
               (cxt, [], 0) modules in
           P.force_newline f;
           Ext_list.rev_iter
             (fun (s,file)  ->
                P.string f L.var;
                P.space f;
                P.string f s;
                P.nspace f ((margin - (String.length s)) + 1);
                P.string f L.eq;
                P.space f;
                P.string f require_lit;
                (P.paren_group f 0) @@
                  ((fun _  ->
                      pp_string f ~utf:true ~quote:(best_string_quote s) file));
                semi f;
                P.newline f) reversed_list;
           outer_cxt)
        let program f cxt (x : J.program) =
          let () = P.force_newline f in
          let cxt = statement_list true cxt f x.block in
          let () = P.force_newline f in exports cxt f x.exports
        let goog_program f goog_package x =
          P.newline f;
          P.string f L.goog_module;
          P.string f "(";
          P.string f (Printf.sprintf "%S" goog_package);
          P.string f ")";
          semi f;
          (let cxt = requires L.goog_require Ext_pp_scope.empty f x.J.modules in
           program f cxt x.program)
        let node_program f (x : J.deps_program) =
          let cxt = requires L.require Ext_pp_scope.empty f x.modules in
          program f cxt x.program
        let amd_program f (x : J.deps_program) =
          P.newline f;
          (let cxt = Ext_pp_scope.empty in
           (P.vgroup f 1) @@
             (fun _  ->
                P.string f L.define;
                P.string f "([";
                P.string f (Printf.sprintf "%S" L.exports);
                List.iter
                  (fun (_,s)  ->
                     P.string f L.comma;
                     P.space f;
                     pp_string f ~utf:true ~quote:(best_string_quote s) s)
                  x.modules;
                P.string f "]";
                P.string f L.comma;
                P.newline f;
                P.string f L.function_;
                P.string f "(";
                P.string f L.exports;
                (let cxt =
                   List.fold_left
                     (fun cxt  ->
                        fun (id,_)  ->
                          P.string f L.comma; P.space f; ident cxt f id) cxt
                     x.modules in
                 P.string f ")";
                 (let v =
                    (P.brace_vgroup f 1) @@
                      (fun _  ->
                         let () = P.string f L.strict_directive in
                         program f cxt x.program) in
                  P.string f ")"; v))))
        let pp_deps_program (program : J.deps_program) (f : Ext_pp.t) =
          P.string f "// Generated CODE, PLEASE EDIT WITH CARE";
          P.newline f;
          P.string f L.strict_directive;
          P.newline f;
          ignore
            (match Js_config.get_env () with
             | AmdJS  -> amd_program f program
             | Browser  -> node_program f program
             | NodeJS  ->
                 (match Sys.getenv "OCAML_AMD_MODULE" with
                  | exception Not_found  -> node_program f program
                  | _ -> amd_program f program)
             | Goog opt ->
                 let goog_package =
                   let v = Lam_current_unit.get_module_name () in
                   match opt with
                   | None |Some "" -> v
                   | Some x -> x ^ ("." ^ v) in
                 goog_program f goog_package program);
          P.newline f;
          P.string f
            (match program.side_effect with
             | None  -> "/* No side effect */"
             | Some v -> Printf.sprintf "/* %s Not a pure module */" v);
          P.newline f;
          P.flush f ()
        let dump_program (x : J.program) oc =
          ignore (program (P.from_channel oc) Ext_pp_scope.empty x)
        let dump_deps_program x (oc : out_channel) =
          pp_deps_program x (P.from_channel oc)
        let string_of_block block =
          let buffer = Buffer.create 50 in
          let f = P.from_buffer buffer in
          let _scope = statement_list true Ext_pp_scope.empty f block in
          P.flush f (); Buffer.contents buffer
        let string_of_expression e =
          let buffer = Buffer.create 50 in
          let f = P.from_buffer buffer in
          let _scope = expression 0 Ext_pp_scope.empty f e in
          P.flush f (); Buffer.contents buffer
      end 
    module Js_output :
      sig
        [@@@ocaml.text
          " The intemediate output when compiling lambda into JS IR "]
        type st = Lam_compile_defs.st
        type finished =
          | True
          | False
          | Dummy
        type t =
          {
          block: J.block;
          value: J.expression option;
          finished: finished;}
        val make : ?value:J.expression -> ?finished:finished -> J.block -> t
        val of_stmt :
          ?value:J.expression -> ?finished:finished -> J.statement -> t
        val of_block :
          ?value:J.expression -> ?finished:finished -> J.block -> t
        val to_block : t -> J.block
        val to_break_block : t -> (J.block* bool)
        module Ops : sig val (++) : t -> t -> t end
        val dummy : t
        val handle_name_tail :
          Lam_compile_defs.st ->
            Lam_compile_defs.return_type ->
              Lambda.lambda -> J.expression -> t
        val handle_block_return :
          Lam_compile_defs.st ->
            Lam_compile_defs.return_type ->
              Lambda.lambda -> J.block -> J.expression -> t
        val concat : t list -> t
        val to_string : t -> string
      end =
      struct
        module E = Js_exp_make
        module S = Js_stmt_make
        type finished =
          | True
          | False
          | Dummy
        type t =
          {
          block: J.block;
          value: J.expression option;
          finished:
            finished[@ocaml.doc
                      " When [finished] is true the block is already terminated, value does not make sense\n        default is false, false is  an conservative approach \n     "];}
        type st = Lam_compile_defs.st
        let make ?value  ?(finished= False)  block =
          { block; value; finished }
        let of_stmt ?value  ?(finished= False)  stmt =
          { block = [stmt]; value; finished }
        let of_block ?value  ?(finished= False)  block =
          { block; value; finished }
        let dummy = { value = None; block = []; finished = Dummy }
        let handle_name_tail (name : st)
          (should_return : Lam_compile_defs.return_type) lam
          (exp : J.expression) =
          (match (name, should_return) with
           | (EffectCall ,False ) ->
               if Lam_analysis.no_side_effects lam
               then dummy
               else { block = []; value = (Some exp); finished = False }
           | (EffectCall ,True _) -> make [S.return exp] ~finished:True
           | (Declare (kind,n),False ) -> make [S.define ~kind n exp]
           | (Assign n,False ) -> make [S.assign n exp]
           | ((Declare _|Assign _),True _) ->
               make [S.unknown_lambda lam] ~finished:True
           | (NeedValue ,_) ->
               { block = []; value = (Some exp); finished = False } : 
          t)
        let handle_block_return (st : st)
          (should_return : Lam_compile_defs.return_type)
          (lam : Lambda.lambda) (block : J.block) exp =
          (match (st, should_return) with
           | (Declare (kind,n),False ) ->
               make (block @ [S.define ~kind n exp])
           | (Assign n,False ) -> make (block @ [S.assign n exp])
           | ((Declare _|Assign _),True _) ->
               make [S.unknown_lambda lam] ~finished:True
           | (EffectCall ,False ) -> make block ~value:exp
           | (EffectCall ,True _) ->
               make (block @ [S.return exp]) ~finished:True
           | (NeedValue ,_) -> make block ~value:exp : t)
        let statement_of_opt_expr (x : J.expression option) =
          (match x with
           | None  -> S.empty ()
           | Some x when Js_analyzer.no_side_effect_expression x ->
               S.empty ()
           | Some x -> S.exp x : J.statement)
        let rec unroll_block (block : J.block) =
          match block with
          | { statement_desc = Block block }::[] -> unroll_block block
          | _ -> block
        let to_block (x : t) =
          (match x with
           | { block; value = opt; finished } ->
               let block = unroll_block block in
               if finished = True
               then block
               else
                 (match opt with
                  | None  -> block
                  | Some x when Js_analyzer.no_side_effect_expression x ->
                      block
                  | Some x -> block @ [S.exp x]) : J.block)
        let to_break_block (x : t) =
          (match x with
           | { finished = True ; block;_} -> ((unroll_block block), false)
           | { block; value = None ; finished } ->
               let block = unroll_block block in
               (block,
                 ((match finished with
                   | True  -> false
                   | False |Dummy  -> true)))
           | { block; value = opt;_} ->
               let block = unroll_block block in
               ((block @ [statement_of_opt_expr opt]), true) : (J.block*
                                                                 bool))
        let rec append (x : t) (y : t) =
          (match (x, y) with
           | ({ finished = True ;_},_) -> x
           | (_,{ block = []; value = None ; finished = Dummy  }) -> x
           | ({ block = []; value = None ;_},y) -> y
           | ({ block = []; value = Some _;_},{ block = []; value = None ;_})
               -> x
           | ({ block = []; value = Some e1;_},({ block = [];
                                                  value = Some e2; finished }
                                                  as z))
               ->
               if Js_analyzer.no_side_effect_expression e1
               then z
               else { block = []; value = (Some (E.seq e1 e2)); finished }
           | ({ block = block1; value = opt_e1;_},{ block = block2;
                                                    value = opt_e2; finished
                                                    })
               ->
               let block1 = unroll_block block1 in
               make
                 (block1 @ ((statement_of_opt_expr opt_e1) ::
                    (unroll_block block2))) ?value:opt_e2 ~finished : 
          t)
        module Ops = struct let (++) (x : t) (y : t) = (append x y : t) end
        let concat (xs : t list) =
          (List.fold_right (fun x  -> fun acc  -> append x acc) xs dummy : 
          t)
        let to_string x = Js_dump.string_of_block (to_block x)
      end 
    module Js_cmj_format :
      sig
        [@@@ocaml.text
          " Define intemediate format to be serialized for cross module optimization\n "]
        [@@@ocaml.text
          " In this module, \n    currently only arity information is  exported, \n\n    Short term: constant literals are also exported \n\n    Long term:\n    Benefit? since Google Closure Compiler already did such huge amount of work\n    TODO: simple expression, literal small function  can be stored, \n    but what would happen if small function captures other environment\n    for example \n\n    {[\n      let f  = fun x -> g x \n    ]}\n\n    {[\n      let f = g \n    ]}\n"]
        type cmj_value =
          {
          arity: Lam_stats.function_arities;
          closed_lambda: Lambda.lambda option;}
        type effect = string option
        type cmj_table =
          {
          values: cmj_value String_map.t;
          effect: effect;
          goog_package: string option;}
        val pure_dummy : cmj_table
        val no_pure_dummy : cmj_table
        val from_file : string -> cmj_table
        val from_string : string -> cmj_table
        val to_file : string -> cmj_table -> unit
      end =
      struct
        type cmj_value =
          {
          arity: Lam_stats.function_arities;
          closed_lambda:
            Lambda.lambda option[@ocaml.doc
                                  " Either constant or closed functor "];}
        type effect = string option
        type cmj_table =
          {
          values: cmj_value String_map.t;
          effect: effect;
          goog_package: string option;}
        let cmj_magic_number = "BUCKLE20160310"
        let cmj_magic_number_length = String.length cmj_magic_number
        let pure_dummy =
          { values = String_map.empty; effect = None; goog_package = None }
        let no_pure_dummy =
          {
            values = String_map.empty;
            effect = (Some "");
            goog_package = None
          }
        let from_file name =
          (let ic = open_in_bin name in
           let buffer = really_input_string ic cmj_magic_number_length in
           if buffer <> cmj_magic_number
           then
             failwith
               ("cmj files have incompatible versions, please rebuilt using the new compiler : "
                  ^ __LOC__)
           else (input_value ic : cmj_table) : cmj_table)
        let from_string s =
          (let magic_number = String.sub s 0 cmj_magic_number_length in
           if magic_number = cmj_magic_number
           then Marshal.from_string s cmj_magic_number_length
           else
             failwith
               ("cmj files have incompatible versions, please rebuilt using the new compiler : "
                  ^ __LOC__) : cmj_table)
        let to_file name (v : cmj_table) =
          let oc = open_out_bin name in
          output_string oc cmj_magic_number; output_value oc v; close_out oc
      end 
    module Js_cmj_datasets :
      sig val cmj_data_sets : Js_cmj_format.cmj_table Lazy.t String_map.t end
      =
      struct
        let cmj_data_sets =
          String_map.of_list
            [("arg.cmj",
               (lazy
                  (Js_cmj_format.from_string
                     "BUCKLE20160310\132\149\166\190\000\000\001\220\000\000\000}\000\000\001\180\000\000\001\154\176\208\208\208\208@#Bad\160\176@@@@@A$Help\160\004\003@@B%align\160\176A\160\160B\144\160\176\001\004\145%*opt*@\160\176\001\004\148(speclist@@@@@\208\208@'current\160\176A@@@@A%parse\160\176@\160\160C\144\160\176\001\004i!l@\160\176\001\004j!f@\160\176\001\004k#msg@@@@@@BC*parse_argv\160\176A\160\160E\144\160\176\001\004a\004 @\160\176\001\004d$argv@\160\176\001\004e(speclist@\160\176\001\004f'anonfun@\160\176\001\004g&errmsg@@@@@\208\208@2parse_argv_dynamic\160\176A\160\160E\144\160\176\001\0043\0046@\160\176\001\0046$argv@\160\176\001\0047(speclist@\160\176\001\0048'anonfun@\160\176\001\0049&errmsg@@@@@@A-parse_dynamic\160\176@\160\160C\144\160\176\001\004o!l@\160\176\001\004p!f@\160\176\001\004q#msg@@@@@\208@%usage\160\176@\160\160B\144\160\176\001\004/(speclist@\160\176\001\0040&errmsg@@@@@\208@,usage_string\160\176A\160\160B\144\160\176\001\004+(speclist@\160\176\001\004,&errmsg@@@@@@ABCD@@")));
            ("array.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\003\135\000\000\001!\000\000\003\176\000\000\003\149\176\208\208\208@&append\160\176@\160\160B\144\160\176\001\004\012\"a1@\160\176\001\004\r\"a2@@@@@\208@$blit\160\176@\160\160E\144\160\176\001\004\026\"a1@\160\176\001\004\027$ofs1@\160\176\001\004\028\"a2@\160\176\001\004\029$ofs2@\160\176\001\004\030#len@@@@@@AB&concat\160@\144\179@\160\176\001\004\159$prim@@\166\155\2401caml_array_concatAA @@\144\176\193 \176\179\144\176I$list@\160\176\179\144\176H%array@\160\176\150\176\144\144!a\002\005\245\225\000\001\003\136\001\003\249\001\003v@\144@\002\005\245\225\000\001\003x@\144@\002\005\245\225\000\001\003}\176\179\004\014\160\004\011@\144@\002\005\245\225\000\001\003\130@\002\005\245\225\000\001\003\134\160\144\004%@\208\208@$copy\160\176@\160\160A\144\160\176\001\004\t!a@@@@@@A-create_matrix\160\176@\160\160C\144\160\176\001\004\002\"sx@\160\176\001\004\003\"sy@\160\176\001\004\004$init@@@@@\208\208@)fast_sort\160\176@\160\160B\144\160\176\001\004w#cmp@\160\176\001\004x!a@@@@@@A$fill\160\176A\160\160D\144\160\176\001\004\020!a@\160\176\001\004\021#ofs@\160\176\001\004\022#len@\160\176\001\004\023!v@@@@@\208@)fold_left\160\176@\160\160C\144\160\176\001\004F!f@\160\176\001\004G!x@\160\176\001\004H!a@@@@@\208@*fold_right\160\176@\160\160C\144\160\176\001\004L!f@\160\176\001\004M!a@\160\176\001\004N!x@@@@@@ABCDE$init\160\176@\160\160B\144\160\176\001\003\253!l@\160\176\001\003\254!f@@@@@\208\208@$iter\160\176A\160\160B\144\160\176\001\004 !f@\160\176\001\004!!a@@@@@\208@%iteri\160\176A\160\160B\144\160\176\001\004*!f@\160\176\001\004+!a@@@@@@AB+make_matrix\160\004v@\208\208\208@#map\160\176@\160\160B\144\160\176\001\004$!f@\160\176\001\004%!a@@@@@\208@$mapi\160\176@\160\160B\144\160\176\001\004.!f@\160\176\001\004/!a@@@@@@AB'of_list\160\176@\160\160A\144\160\176\001\004?!l@@@@@\208@$sort\160\176A\160\160B\144\160\176\001\004S#cmp@\160\176\001\004T!a@@@@@\208@+stable_sort\160\004\154@@ABC#sub\160\176@\160\160C\144\160\176\001\004\016!a@\160\176\001\004\017#ofs@\160\176\001\004\018#len@@@@@\208@'to_list\160\176@\160\160A\144\160\176\001\0044!a@@@@@@ADEF@@")));
            ("arrayLabels.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002\247\000\000\000\249\000\000\0030\000\000\003\026\176\208\208\208@&append\160\176@\160\160B\144\160\176\001\004\012\"a1@\160\176\001\004\r\"a2@@@@@\208@$blit\160\176@\160\160E\144\160\176\001\004\026\"a1@\160\176\001\004\027$ofs1@\160\176\001\004\028\"a2@\160\176\001\004\029$ofs2@\160\176\001\004\030#len@@@@@@AB&concat\160@@\208\208@$copy\160\176@\160\160A\144\160\176\001\004\t!a@@@@@@A-create_matrix\160\176@\160\160C\144\160\176\001\004\002\"sx@\160\176\001\004\003\"sy@\160\176\001\004\004$init@@@@@\208\208@)fast_sort\160\176@\160\160B\144\160\176\001\004w#cmp@\160\176\001\004x!a@@@@@@A$fill\160\176A\160\160D\144\160\176\001\004\020!a@\160\176\001\004\021#ofs@\160\176\001\004\022#len@\160\176\001\004\023!v@@@@@\208@)fold_left\160\176@\160\160C\144\160\176\001\004F!f@\160\176\001\004G!x@\160\176\001\004H!a@@@@@\208@*fold_right\160\176@\160\160C\144\160\176\001\004L!f@\160\176\001\004M!a@\160\176\001\004N!x@@@@@@ABCDE$init\160\176@\160\160B\144\160\176\001\003\253!l@\160\176\001\003\254!f@@@@@\208\208@$iter\160\176A\160\160B\144\160\176\001\004 !f@\160\176\001\004!!a@@@@@\208@%iteri\160\176A\160\160B\144\160\176\001\004*!f@\160\176\001\004+!a@@@@@@AB+make_matrix\160\004v@\208\208\208@#map\160\176@\160\160B\144\160\176\001\004$!f@\160\176\001\004%!a@@@@@\208@$mapi\160\176@\160\160B\144\160\176\001\004.!f@\160\176\001\004/!a@@@@@@AB'of_list\160\176@\160\160A\144\160\176\001\004?!l@@@@@\208@$sort\160\176A\160\160B\144\160\176\001\004S#cmp@\160\176\001\004T!a@@@@@\208@+stable_sort\160\004\154@@ABC#sub\160\176@\160\160C\144\160\176\001\004\016!a@\160\176\001\004\017#ofs@\160\176\001\004\018#len@@@@@\208@'to_list\160\176@\160\160A\144\160\176\001\0044!a@@@@@@ADEF@@")));
            ("bigarray.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\004/\000\000\001*\000\000\003\203\000\000\003\143\176\208\208\208\208\208\208@&Array1\160@@@A&Array2\160@@\208@&Array3\160@@@AB(Genarray\160@@\208@2array1_of_genarray\160\176@\160\160A\144\160\176\001\004\214!a@@@@@\208@2array2_of_genarray\160\176@\160\160A\144\160\176\001\004\216!a@@@@@\208@2array3_of_genarray\160\176@\160\160A\144\160\176\001\004\218!a@@@@@@ABCD(c_layout\160@\144\145\161@\144(C_layout\208\208@$char\160@\144\145\161L\144$Char@A)complex32\160@\144\145\161J\144)Complex32\208@)complex64\160@\144\145\161K\144)Complex64@ABE'float32\160@\144\145\161@\144'Float32\208@'float64\160@\144\145\161A\144'Float64\208\208@.fortran_layout\160@\144\145\161A\144.Fortran_layout@A#int\160@\144\145\161H\144#Int@BCF,int16_signed\160@\144\145\161D\144,Int16_signed\208\208@.int16_unsigned\160@\144\145\161E\144.Int16_unsigned\208@%int32\160@\144\145\161F\144%Int32\208@%int64\160@\144\145\161G\144%Int64@ABC+int8_signed\160@\144\145\161B\144+Int8_signed\208\208@-int8_unsigned\160@\144\145\161C\144-Int8_unsigned@A)nativeint\160@\144\145\161I\144)Nativeint\208\208@'reshape\160@\144\179@\160\176\001\004\239$prim@\160\176\001\004\238\004\003@@\166\155\240/caml_ba_reshapeBA @@\144\176\193 \176\179\177\144\176\001\004J\004\174@!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\024\243\001\004\219\001\024\211\160\176\150\176\144\144!b\002\005\245\225\000\001\024\245\001\004\219\001\024\212\160\176\150\176\144\144!c\002\005\245\225\000\001\024\247\001\004\219\001\024\213@\144@\002\005\245\225\000\001\024\217\176\193\004\031\176\179\144\176H%array@\160\176\179\144\176A#int@@\144@\002\005\245\225\000\001\024\223@\144@\002\005\245\225\000\001\024\227\176\179\177\004+!t\000\255\160\004)\160\004#\160\004\029@\144@\002\005\245\225\000\001\024\234@\002\005\245\225\000\001\024\240@\002\005\245\225\000\001\024\241\160\144\004B\160\144\004A@@A)reshape_1\160\176@\160\160B\144\160\176\001\004\221!a@\160\176\001\004\222$dim1@@@@\144\179@\004\b\166\155\004N\160\144\004\011\160\166\b\000\000\004\017B\160\144\004\r@@\208@)reshape_2\160\176@\160\160C\144\160\176\001\004\224!a@\160\176\001\004\225$dim1@\160\176\001\004\226$dim2@@@@@\208@)reshape_3\160\176@\160\160D\144\160\176\001\004\228!a@\160\176\001\004\229$dim1@\160\176\001\004\230$dim2@\160\176\001\004\231$dim3@@@@@@ABCDEG\144 @")));
            ("buffer.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\003:\000\000\001\n\000\000\003a\000\000\003H\176\208\208\208\208\208@*add_buffer\160\176A\160\160B\144\160\176\001\004/!b@\160\176\001\0040\"bs@@@@@@A)add_bytes\160\176A\160\160B\144\160\176\001\004,!b@\160\176\001\004-!s@@@@@\208@+add_channel\160\176A\160\160C\144\160\176\001\0042!b@\160\176\001\0043\"ic@\160\176\001\0044#len@@@@@@AB(add_char\160\176A\160\160B\144\160\176\001\004\024!b@\160\176\001\004\025!c@@@@@\208\208@*add_string\160\176A\160\160B\144\160\176\001\004'!b@\160\176\001\004(!s@@@@@@A,add_subbytes\160\176A\160\160D\144\160\176\001\004\"!b@\160\176\001\004#!s@\160\176\001\004$&offset@\160\176\001\004%#len@@@@@\208\208@.add_substitute\160\176@\160\160C\144\160\176\001\004R!b@\160\176\001\004S!f@\160\176\001\004T!s@@@@@@A-add_substring\160\176A\160\160D\144\160\176\001\004\028!b@\160\176\001\004\029!s@\160\176\001\004\030&offset@\160\176\001\004\031#len@@@@@@BCD$blit\160\176@\160\160E\144\160\176\001\004\003#src@\160\176\001\004\004&srcoff@\160\176\001\004\005#dst@\160\176\001\004\006&dstoff@\160\176\001\004\007#len@@@@@\208\208@%clear\160\176A\160\160A\144\160\176\001\004\014!b@@@@\144\179@\004\005\166\183A@\144(position\160\144\004\n\160\145\144\144@@@A(contents\160\176A\160\160A\144\160\176\001\003\251!b@@@@@@BE&create\160\176A\160\160A\144\160\176\001\003\246!n@@@@@\208\208\208@&length\160\176@\160\160A\144\160\176\001\004\012!b@@@@\144\179@\004\005\166\166A\144\004*\160\144\004\t@@A#nth\160\176A\160\160B\144\160\176\001\004\t!b@\160\176\001\004\n#ofs@@@@@\208\208@-output_buffer\160\176@\160\160B\144\160\176\001\0046\"oc@\160\176\001\0047!b@@@@@@A%reset\160\176A\160\160A\144\160\176\001\004\016!b@@@@@@BC#sub\160\176A\160\160C\144\160\176\001\003\255!b@\160\176\001\004\000#ofs@\160\176\001\004\001#len@@@@@\208@(to_bytes\160\176@\160\160A\144\160\176\001\003\253!b@@@@@@ADF@@")));
            ("bytes.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\005\129\000\000\001\202\000\000\005\226\000\000\005\181\176\208\208\208\208\208@$blit\160\176@\160\160E\144\160\176\001\004&\"s1@\160\176\001\004'$ofs1@\160\176\001\004(\"s2@\160\176\001\004)$ofs2@\160\176\001\004*#len@@@@@@A+blit_string\160\176@\160\160E\144\160\176\001\004,\"s1@\160\176\001\004-$ofs1@\160\176\001\004.\"s2@\160\176\001\004/$ofs2@\160\176\001\0040#len@@@@@\208\208@*capitalize\160\176@\160\160A\144\160\176\001\004r!s@@@@@@A#cat\160\176@\160\160B\144\160\176\001\004E\"s1@\160\176\001\004F\"s2@@@@@\208@'compare\160\176@\160\160B\144\160\176\001\004\155!x@\160\176\001\004\156!y@@@@\144\179@\004\b\166\155\240,caml_compareBA @@@\160\144\004\014\160\144\004\r@@ABC&concat\160\176@\160\160B\144\160\176\001\004:#sep@\160\176\001\004;!l@@@@@\208@(contains\160\176A\160\160B\144\160\176\001\004\147!s@\160\176\001\004\148!c@@@@@\208\208@-contains_from\160\176A\160\160C\144\160\176\001\004\142!s@\160\176\001\004\143!i@\160\176\001\004\144!c@@@@@@A$copy\160\176@\160\160A\144\160\176\001\004\007!s@@@@@@BCD%empty\160\176@@@@\208\208@'escaped\160\176@\160\160A\144\160\176\001\004T!s@@@@@@A&extend\160\176@\160\160C\144\160\176\001\004\024!s@\160\176\001\004\025$left@\160\176\001\004\026%right@@@@@\208@$fill\160\176@\160\160D\144\160\176\001\004!!s@\160\176\001\004\"#ofs@\160\176\001\004##len@\160\176\001\004$!c@@@@@\208@%index\160\176@\160\160B\144\160\176\001\004{!s@\160\176\001\004|!c@@@@@\208@*index_from\160\176@\160\160C\144\160\176\001\004~!s@\160\176\001\004\127!i@\160\176\001\004\128!c@@@@@@ABCDE$init\160\176@\160\160B\144\160\176\001\004\001!n@\160\176\001\004\002!f@@@@@\208\208\208@$iter\160\176A\160\160B\144\160\176\001\0042!f@\160\176\001\0043!a@@@@@\208@%iteri\160\176A\160\160B\144\160\176\001\0046!f@\160\176\001\0047!a@@@@@\208@)lowercase\160\176@\160\160A\144\160\176\001\004l!s@@@@@@ABC$make\160\176@\160\160B\144\160\176\001\003\253!n@\160\176\001\003\254!c@@@@@\208@#map\160\176@\160\160B\144\160\176\001\004^!f@\160\176\001\004_!s@@@@@\208@$mapi\160\176@\160\160B\144\160\176\001\004d!f@\160\176\001\004e!s@@@@@@ABD)of_string\160\176@\160\160A\144\160\176\001\004\r!s@@@@@\208\208\208\208@.rcontains_from\160\176A\160\160C\144\160\176\001\004\150!s@\160\176\001\004\151!i@\160\176\001\004\152!c@@@@@@A&rindex\160\176@\160\160B\144\160\176\001\004\135!s@\160\176\001\004\136!c@@@@@\208@+rindex_from\160\176@\160\160C\144\160\176\001\004\138!s@\160\176\001\004\139!i@\160\176\001\004\140!c@@@@@@AB#sub\160\176@\160\160C\144\160\176\001\004\015!s@\160\176\001\004\016#ofs@\160\176\001\004\017#len@@@@@\208@*sub_string\160\176A\160\160C\144\160\176\001\004\020!b@\160\176\001\004\021#ofs@\160\176\001\004\022#len@@@@@@AC)to_string\160\176A\160\160A\144\160\176\001\004\011!b@@@@@\208\208@$trim\160\176@\160\160A\144\160\176\001\004O!s@@@@@@A,uncapitalize\160\176@\160\160A\144\160\176\001\004t!s@@@@@\208\208\208@0unsafe_of_string\160@\144\179@\160\176\001\004\157$prim@@\166B\160\144\004\005@@A0unsafe_to_string\160@\144\179@\160\176\001\004\158\004\n@@\166A\160\144\004\004@@B)uppercase\160\176@\160\160A\144\160\176\001\004j!s@@@@@@CDEFG@@")));
            ("bytesLabels.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\004\147\000\000\001}\000\000\004\235\000\000\004\199\176\208\208\208\208@$blit\160\176@\160\160E\144\160\176\001\004&\"s1@\160\176\001\004'$ofs1@\160\176\001\004(\"s2@\160\176\001\004)$ofs2@\160\176\001\004*#len@@@@@\208@*capitalize\160\176@\160\160A\144\160\176\001\004r!s@@@@@\208@'compare\160\176@\160\160B\144\160\176\001\004\155!x@\160\176\001\004\156!y@@@@@@ABC&concat\160\176@\160\160B\144\160\176\001\004:#sep@\160\176\001\004;!l@@@@@\208\208@(contains\160\176A\160\160B\144\160\176\001\004\147!s@\160\176\001\004\148!c@@@@@\208@-contains_from\160\176A\160\160C\144\160\176\001\004\142!s@\160\176\001\004\143!i@\160\176\001\004\144!c@@@@@@AB$copy\160\176@\160\160A\144\160\176\001\004\007!s@@@@@@CD%empty\160\176@@@@\208\208@'escaped\160\176@\160\160A\144\160\176\001\004T!s@@@@@@A$fill\160\176@\160\160D\144\160\176\001\004!!s@\160\176\001\004\"#ofs@\160\176\001\004##len@\160\176\001\004$!c@@@@@\208@%index\160\176@\160\160B\144\160\176\001\004{!s@\160\176\001\004|!c@@@@@\208@*index_from\160\176@\160\160C\144\160\176\001\004~!s@\160\176\001\004\127!i@\160\176\001\004\128!c@@@@@@ABCE$init\160\176@\160\160B\144\160\176\001\004\001!n@\160\176\001\004\002!f@@@@@\208\208\208@$iter\160\176A\160\160B\144\160\176\001\0042!f@\160\176\001\0043!a@@@@@\208@%iteri\160\176A\160\160B\144\160\176\001\0046!f@\160\176\001\0047!a@@@@@\208@)lowercase\160\176@\160\160A\144\160\176\001\004l!s@@@@@@ABC$make\160\176@\160\160B\144\160\176\001\003\253!n@\160\176\001\003\254!c@@@@@\208@#map\160\176@\160\160B\144\160\176\001\004^!f@\160\176\001\004_!s@@@@@\208@$mapi\160\176@\160\160B\144\160\176\001\004d!f@\160\176\001\004e!s@@@@@@ABD)of_string\160\176@\160\160A\144\160\176\001\004\r!s@@@@@\208\208\208\208@.rcontains_from\160\176A\160\160C\144\160\176\001\004\150!s@\160\176\001\004\151!i@\160\176\001\004\152!c@@@@@@A&rindex\160\176@\160\160B\144\160\176\001\004\135!s@\160\176\001\004\136!c@@@@@\208@+rindex_from\160\176@\160\160C\144\160\176\001\004\138!s@\160\176\001\004\139!i@\160\176\001\004\140!c@@@@@@AB#sub\160\176@\160\160C\144\160\176\001\004\015!s@\160\176\001\004\016#ofs@\160\176\001\004\017#len@@@@@\208@*sub_string\160\176A\160\160C\144\160\176\001\004\020!b@\160\176\001\004\021#ofs@\160\176\001\004\022#len@@@@@@AC)to_string\160\176A\160\160A\144\160\176\001\004\011!b@@@@@\208\208@$trim\160\176@\160\160A\144\160\176\001\004O!s@@@@@@A,uncapitalize\160\176@\160\160A\144\160\176\001\004t!s@@@@@\208\208\208@0unsafe_of_string\160@@@A0unsafe_to_string\160@@@B)uppercase\160\176@\160\160A\144\160\176\001\004j!s@@@@@@CDEFG@@")));
            ("callback.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\245\000\000\000@\000\000\000\217\000\000\000\207\176\208@(register\160\176@\160\160B\144\160\176\001\003\242$name@\160\176\001\003\243!v@@@@\144\179@\004\b\166\155\2409caml_register_named_valueBA @@\144\176\193 \176\179\144\176C&string@@\144@\002\005\245\225\000\001\002\234\176\193\004\t\176\179\177\144\176@#ObjA!t\000\255@\144@\002\005\245\225\000\001\003U\176\179\144\176F$unit@@\144@\002\005\245\225\000\001\003X@\002\005\245\225\000\001\003[@\002\005\245\225\000\001\003\\\160\144\004(\160\144\004'@\208@2register_exception\160\176@\160\160B\144\160\176\001\003\245$name@\160\176\001\003\246#exn@@@@@@AB@@")));
            ("camlinternalFormat.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\005\031\000\000\001=\000\000\004Q\000\000\004\014\176\208\208\208@/add_in_char_set\160\176A\160\160B\144\160\176\001\003\243(char_set@\160\176\001\003\244!c@@@@@\208@*bufput_acc\160\176A\160\160B\144\160\176\002\000\000\245\012!b@\160\176\002\000\000\245\r#acc@@@@@\208@-char_of_iconv\160\176A\160\160A\144\160\176\001\004v%iconv@@@@@@ABC/create_char_set\160\176@\160\160A\144\160\176\002\000\001)b%param@@@@\144\179@\004\005\178\166\166@\145$make\160\166\147\176@%BytesA@@\160\145\144\144`\160\145\144\145@@\160\176\1925camlinternalFormat.mlI\001\001\007\001\001 \192\004\002I\001\001\007\001\0014@A\208\208@1fmt_ebb_of_string\160\176@\160\160B\144\160\176\002\000\000\249[/legacy_behavior@\160\176\002\000\000\249\\#str@@@@@@A6format_of_string_fmtty\160\176@\160\160B\144\160\176\002\000\001&Z#str@\160\176\002\000\001&[%fmtty@@@@@\208\208@7format_of_string_format\160\176@\160\160B\144\160\176\002\000\001&`#str@\160\176\002\000\001&f\004A@@@@@@A/freeze_char_set\160\176A\160\160A\144\160\176\001\003\249(char_set@@@@\144\179@\004\005\178\166\166E\145)to_string\160\166\147\176@%BytesA@@\160\144\004\016@\160\176\192\004CS\001\002^\001\002`\192\004DS\001\002^\001\002x@A@BCD.is_in_char_set\160\176A\160\160B\144\160\176\001\003\255(char_set@\160\176\001\004\000!c@@@@@\208\208@+make_printf\160\176@\160\160D\144\160\176\002\000\000\243i!k@\160\176\002\000\000\243j!o@\160\176\002\000\000\243k#acc@\160\176\002\000\000\243l#fmt@@@@@\208\208@2open_box_of_string\160\176A\160\160A\144\160\176\002\000\000\245?#str@@@@@@A*output_acc\160\176@\160\160B\144\160\176\002\000\000\244\245!o@\160\176\002\000\000\244\246#acc@@@@@@BC>param_format_of_ignored_format\160\176A\160\160B\144\160\176\001\004\022#ign@\160\176\001\004\023#fmt@@@@@\208\208\208\208@&recast\160\176@\160\160B\144\160\176\002\000\000\243 #fmt@\160\176\002\000\000\243!%fmtty@@@@@@A,rev_char_set\160\176A\160\160A\144\160\176\001\003\251(char_set@@@@@\208@-string_of_fmt\160\176A\160\160A\144\160\176\001\t@#fmt@@@@@@AB/string_of_fmtty\160\176A\160\160A\144\160\176\002\000\000\243Y%fmtty@@@@@\208@8string_of_formatting_gen\160\176@\160\160A\144\160\176\001\004\215.formatting_gen@@@@@@AC8string_of_formatting_lit\160\176@\160\160A\144\160\176\001\004\203.formatting_lit@@@@@\208\208@*strput_acc\160\176A\160\160B\144\160\176\002\000\000\245#!b@\160\176\002\000\000\245$#acc@@@@@@A$symm\160\176A\160\160A\144\160\176\002\000\001)9\004\244@@@@@\208\208@%trans\160\176A\160\160B\144\160\176\002\000\000\170R#ty1@\160\176\002\000\000\170S#ty2@@@A@@A+type_format\160\176@\160\160B\144\160\176\002\000\000\179\135#fmt@\160\176\002\000\000\179\136%fmtty@@@@@@BCDEF@@")));
            ("camlinternalFormatBasics.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\140\000\000\000%\000\000\000\130\000\000\000y\176\208\208@*concat_fmt\160\176@\160\160B\144\160\176\001\005=$fmt1@\160\176\001\005>$fmt2@@@@@@A,concat_fmtty\160\176@\160\160B\144\160\176\001\004\227&fmtty1@\160\176\001\004\228&fmtty2@@@@@\208@)erase_rel\160\176A\160\160A\144\160\176\001\005\171%param@@@@@@AB@@")));
            ("camlinternalLazy.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\173\000\000\000-\000\000\000\161\000\000\000\153\176\208\208@)Undefined\160\176@@@@\208@%force\160\176@\160\160A\144\160\176\001\003\252#lzv@@@@@@AB0force_lazy_block\160\176@\160\160A\144\160\176\001\003\243#blk@@@@@\208\208@)force_val\160\176@\160\160A\144\160\176\001\004\000#lzv@@@@@@A4force_val_lazy_block\160\176@\160\160A\144\160\176\001\003\248#blk@@@@@@BC@@")));
            ("camlinternalMod.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000f\000\000\000\030\000\000\000e\000\000\000a\176\208@(init_mod\160\176A\160\160B\144\160\176\001\003\247#loc@\160\176\001\003\248%shape@@@@@\208@*update_mod\160\176A\160\160C\144\160\176\001\004\006%shape@\160\176\001\004\007!o@\160\176\001\004\b!n@@@@@@AB@@")));
            ("camlinternalOO.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\006\142\000\000\001\157\000\000\005\177\000\000\005I\176\208\208\208\208\208\208@/add_initializer\160\176A\160\160B\144\160\176\001\004\201%table@\160\176\001\004\202!f@@@@@@A$copy\160\176@\160\160A\144\160\176\001\003\243!o@@@@\144\179@\004\005\166\155\240.caml_set_oo_idA@ @@\144\176\193 \176\150\176\144\144!a\002\005\245\225\000\001\003U\001\003\240\001\003R\004\006@\002\005\245\225\000\001\003S\160\166\155\240,caml_obj_dupAA @@\144\176\193 \176\179\144\176\001\003\240!t@@\144@\002\005\245\225\000\001\003e\176\179\004\006@\144@\002\005\245\225\000\001\003h@\002\005\245\225\000\001\003k\160\144\004(@@@B-create_object\160\176@\160\160A\144\160\176\001\004\240%table@@@@@\208\208\208@\t\"create_object_and_run_initializers\160\176@\160\160B\144\160\176\001\005\004%obj_0@\160\176\001\005\005%table@@@@@@A1create_object_opt\160\176@\160\160B\144\160\176\001\004\243%obj_0@\160\176\001\004\244%table@@@@@@B,create_table\160\176@\160\160A\144\160\176\001\004\204.public_methods@@@@@\208@+dummy_class\160\176A\160\160A\144\160\176\001\004\237#loc@@@@@@ACD+dummy_table\160\176A@@@\208\208@*get_method\160\176@\160\160B\144\160\176\001\004\136%table@\160\176\001\004\137%label@@@@@@A0get_method_label\160\176@\160\160B\144\160\176\001\004}%table@\160\176\001\004~$name@@@@@\208@1get_method_labels\160\176@\160\160B\144\160\176\001\004\129%table@\160\176\001\004\130%names@@@@@@ABE,get_variable\160\176@\160\160B\144\160\176\001\004\195%table@\160\176\001\004\196$name@@@@@\208\208@-get_variables\160\176@\160\160B\144\160\176\001\004\198%table@\160\176\001\004\199%names@@@@@\208@(inherits\160\176@\160\160F\144\160\176\001\004\213#cla@\160\176\001\004\214$vals@\160\176\001\004\215*virt_meths@\160\176\001\004\216+concr_meths@\160\176\001\006\005%param@\160\176\001\004\219#top@@@@@@AB*init_class\160\176A\160\160A\144\160\176\001\004\211%table@@@@@\208\208\208@-lookup_tables\160\176@\160\160B\144\160\176\001\005#$root@\160\176\001\005$$keys@@@@@@A*make_class\160\176A\160\160B\144\160\176\001\004\223)pub_meths@\160\176\001\004\224*class_init@@@@@\208@0make_class_store\160\176A\160\160C\144\160\176\001\004\231)pub_meths@\160\176\001\004\232*class_init@\160\176\001\004\233*init_table@@@@@@AB&narrow\160\176A\160\160D\144\160\176\001\004\141%table@\160\176\001\004\142$vars@\160\176\001\004\143*virt_meths@\160\176\001\004\144+concr_meths@@@@@@CDF*new_method\160\176@\160\160A\144\160\176\001\004z%table@@@@@\208\208\208@5new_methods_variables\160\176@\160\160C\144\160\176\001\004\185%table@\160\176\001\004\186%meths@\160\176\001\004\187$vals@@@@@@A,new_variable\160\176@\160\160B\144\160\176\001\004\179%table@\160\176\001\004\180$name@@@@@\208@&params\160\004\203@@AB3public_method_label\160\176@\160\160A\144\160\176\001\004\r!s@@@@@\208\208@0run_initializers\160\176@\160\160B\144\160\176\001\004\251#obj@\160\176\001\004\252%table@@@@@\208@4run_initializers_opt\160\176@\160\160C\144\160\176\001\004\255%obj_0@\160\176\001\005\000#obj@\160\176\001\005\001%table@@@@@@AB*set_method\160\176A\160\160C\144\160\176\001\004\132%table@\160\176\001\004\133%label@\160\176\001\004\134'element@@@@@\208@+set_methods\160\176A\160\160B\144\160\176\001\005\240%table@\160\176\001\005\241'methods@@@@@\208\208@%stats\160\176A\160\160A\144\160\176\001\005\251%param@@@@@@A%widen\160\176A\160\160A\144\160\176\001\004\163%table@@@@@@BCDEG@@")));
            ("char.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\164\000\000\0006\000\000\000\179\000\000\000\175\176\208\208@#chr\160\176A\160\160A\144\160\176\001\003\243!n@@@@@\208@'compare\160\176A\160\160B\144\160\176\001\003\255\"c1@\160\176\001\004\000\"c2@@@@@@AB'escaped\160\176@\160\160A\144\160\176\001\003\247!c@@@@@\208@)lowercase\160\176@\160\160A\144\160\176\001\003\250!c@@@@@\208@)uppercase\160\176@\160\160A\144\160\176\001\003\252!c@@@@@@ABC@@")));
            ("complex.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\001\222\000\000\000\178\000\000\002A\000\000\002;\176\208\208\208\208@#add\160\176A\160\160B\144\160\176\001\003\247!x@\160\176\001\003\248!y@@@@@\208@#arg\160\176@\160\160A\144\160\176\001\004\021!x@@@@@@AB$conj\160\176A\160\160A\144\160\176\001\003\255!x@@@@@\208@#div\160\176A\160\160B\144\160\176\001\004\004!x@\160\176\001\004\005!y@@@@@\208@#exp\160\176A\160\160A\144\160\176\001\004!!x@@@@@@ABC!i\160@@\208\208\208@#inv\160\176A\160\160A\144\160\176\001\004\011!x@@@@@\208@#log\160\176A\160\160A\144\160\176\001\004$!x@@@@@@AB#mul\160\176A\160\160B\144\160\176\001\004\001!x@\160\176\001\004\002!y@@@@@@C#neg\160\176A\160\160A\144\160\176\001\003\253!x@@@@@\208\208@$norm\160\176@\160\160A\144\160\176\001\004\015!x@@@@@@A%norm2\160\176A\160\160A\144\160\176\001\004\r!x@@@@@@BDE#one\160@@\208\208\208@%polar\160\176A\160\160B\144\160\176\001\004\023!n@\160\176\001\004\024!a@@@@@\208@#pow\160\176A\160\160B\144\160\176\001\004&!x@\160\176\001\004'!y@@@@@@AB$sqrt\160\176A\160\160A\144\160\176\001\004\026!x@@@@@@C#sub\160\176A\160\160B\144\160\176\001\003\250!x@\160\176\001\003\251!y@@@@@\208@$zero\160@@@ADF@@")));
            ("digest.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002\018\000\000\000\160\000\000\002\017\000\000\001\253\176\208\208\208@%bytes\160\176@\160\160A\144\160\176\001\003\247!b@@@@@@A'compare\160\176@\160\160B\144\160\176\001\004L!x@\160\176\001\004M!y@@@@@@B$file\160\176@\160\160A\144\160\176\001\004\001(filename@@@@@\208\208\208\208@(from_hex\160\176A\160\160A\144\160\176\001\004\018!s@@@@@@A%input\160\176A\160\160A\144\160\176\001\004\t$chan@@@@\144\179@\004\005\178\166\166\000D\1453really_input_string\160\166\147\176@*PervasivesA@@\160\144\004\016\160\145\144\144P@\160\176\192)digest.mll\001\006f\001\006w\192\004\002l\001\006f\001\006\146@A@B&output\160\176@\160\160B\144\160\176\001\004\006$chan@\160\176\001\004\007&digest@@@@\144\179@\004\b\178\166\166p\145-output_string\160\166\147\004#@@\160\144\004\017\160\144\004\016@\160\176\192\004\031j\001\006I\001\006K\192\004 j\001\006I\001\006d@A@C&string\160\176@\160\160A\144\160\176\001\003\245#str@@@@@\208\208@(subbytes\160\176@\160\160C\144\160\176\001\003\253!b@\160\176\001\003\254#ofs@\160\176\001\003\255#len@@@@@@A)substring\160\176@\160\160C\144\160\176\001\003\249#str@\160\176\001\003\250#ofs@\160\176\001\003\251#len@@@@@\208@&to_hex\160\176A\160\160A\144\160\176\001\004\r!d@@@@@@ABDE@@")));
            ("filename.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002\028\000\000\000z\000\000\001\199\000\000\001\162\176\208\208\208\208@(basename\160@@@A,check_suffix\160@@\208@.chop_extension\160\176@\160\160A\144\160\176\001\004h$name@@@@@@AB+chop_suffix\160\176@\160\160B\144\160\176\001\004d$name@\160\176\001\004e$suff@@@@@\208@&concat\160\176A\160\160B\144\160\176\001\004`'dirname@\160\176\001\004a(filename@@@@@@AC0current_dir_name\160@@\208\208\208@'dir_sep\160@@@A'dirname\160@@\208\208@1get_temp_dir_name\160\176@\160\160A\144\160\176\001\004\160%param@@@@@@A+is_implicit\160@@@BC+is_relative\160@@\208\208@.open_temp_file\160\176A\160\160D\144\160\176\001\004\141%*opt*@\160\176\001\004\144\004\003@\160\176\001\004\147&prefix@\160\176\001\004\148&suffix@@@@@@A/parent_dir_name\160@@\208\208\208@%quote\160@@@A1set_temp_dir_name\160\176A\160\160A\144\160\176\001\004\128!s@@@@@\208@-temp_dir_name\160@@@AB)temp_file\160\176@\160\160C\144\160\176\001\004\131\004$@\160\176\001\004\134&prefix@\160\176\001\004\135&suffix@@@@@@CDEF\144%match@")));
            ("format.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\022&\000\000\005c\000\000\019D\000\000\018\014\176\208\208\208\208\208\208\208@(asprintf\160\176@\160\160A\144\160\176\001\006V%param@@@@@\208@'bprintf\160\176@\160\160B\144\160\176\001\006N!b@\160\176\001\006T\004\r@@@@@@AB)close_box\160\176A\160\160A\144\160\176\001\007k%param@@@@@\208@)close_tag\160\176A\160\160A\144\160\176\001\007i\004\n@@@@@@AC*close_tbox\160\176A\160\160A\144\160\176\001\007X\004\018@@@@@\208\208@'eprintf\160\176@\160\160A\144\160\176\001\006;#fmt@@@@@@A-err_formatter\160\176@@@@\208@3flush_str_formatter\160\176@\160\160A\144\160\176\001\006\171\004>@@@@@@ABD-force_newline\160\176@\160\160A\144\160\176\001\007]\0041@@@@@\208\208\208\208@3formatter_of_buffer\160\176@\160\160A\144\160\176\001\005\149!b@@@@@@A8formatter_of_out_channel\160\176@\160\160A\144\160\176\001\005\147\"oc@@@@@\208@'fprintf\160\176@\160\160B\144\160\176\001\0063#ppf@\160\176\001\0064#fmt@@@@@\208@\t\"get_all_formatter_output_functions\160\176A\160\160A\144\160\176\001\007@\004]@@@@@@ABC1get_ellipsis_text\160\176@\160\160A\144\160\176\001\007K\004e@@@@@\208\208@;get_formatter_out_functions\160\176A\160\160A\144\160\176\001\007H\004o@@@@@@A>get_formatter_output_functions\160\176A\160\160A\144\160\176\001\007E\004w@@@@@\208@;get_formatter_tag_functions\160\176A\160\160A\144\160\176\001\007>\004\128@@@@@@ABD*get_margin\160\176@\160\160A\144\160\176\001\007R\004\136@@@@@\208\208@-get_mark_tags\160\176@\160\160A\144\160\176\001\007:\004\146@@@@@@A-get_max_boxes\160\176@\160\160A\144\160\176\001\007N\004\154@@@@@@BEF.get_max_indent\160\176@\160\160A\144\160\176\001\007P\004\162@@@@@\208\208\208\208@.get_print_tags\160\176@\160\160A\144\160\176\001\007<\004\174@@@@@@A(ifprintf\160\176@\160\160B\144\160\176\001\0066#ppf@\160\176\001\0067#fmt@@@@@\208\208@)ikfprintf\160\176@\160\160C\144\160\176\001\006/!k@\160\176\001\0060!x@\160\176\001\006]\004\223@@@@@@A(kfprintf\160\176@\160\160C\144\160\176\001\006)!k@\160\176\001\006*!o@\160\176\001\006a\004\237@@@@@\208\208@'kprintf\160\176@\160\160B\144\160\176\001\006=!k@\160\176\001\006X\004\250@@@@@@A(ksprintf\160\004\011@@BCD.make_formatter\160\176@\160\160B\144\160\176\001\005\143&output@\160\176\001\005\144%flush@@@@@\208\208@(open_box\160\176@\160\160A\144\160\176\001\007l\004\253@@@@@@A)open_hbox\160\176@\160\160A\144\160\176\001\007p\005\001\005@@@@@\208@+open_hovbox\160\176@\160\160A\144\160\176\001\007m\005\001\014@@@@@@ABE*open_hvbox\160\176@\160\160A\144\160\176\001\007n\005\001\022@@@@@\208\208\208\208@(open_tag\160\176A\160\160A\144\160\176\001\007j\005\001\"@@@@@@A)open_tbox\160\176@\160\160A\144\160\176\001\007Y\005\001*@@@@@@B)open_vbox\160\176@\160\160A\144\160\176\001\007o\005\0012@@@@@\208\208@.over_max_boxes\160\176A\160\160A\144\160\176\001\007M\005\001<@@@@@@A,pp_close_box\160\176A\160\160B\144\160\176\001\004\198%state@\160\176\001\006\218\005\001\\@@@@@\208@,pp_close_tag\160\176A\160\160B\144\160\176\001\004\203%state@\160\176\001\006\213\005\001h@@@@@\208@-pp_close_tbox\160\176A\160\160B\144\160\176\001\005\"%state@\160\176\001\006\199\005\001t@@@@@@ABCD0pp_force_newline\160\176@\160\160B\144\160\176\001\005\018%state@\160\176\001\006\204\005\001\127@@@@@\208\208\208\208@\t%pp_get_all_formatter_output_functions\160\176A\160\160B\144\160\176\001\005v%state@\160\176\001\006\181\005\001\142@@@@@@A4pp_get_ellipsis_text\160\176@\160\160B\144\160\176\001\005I%state@\160\176\001\006\188\005\001\153@@@@\144\179@\004\007\166\166O\144+pp_ellipsis\160\144\004\012@\208@>pp_get_formatter_out_functions\160\176A\160\160B\144\160\176\001\005h%state@\160\176\001\006\183\005\001\173@@@@@@AB\t!pp_get_formatter_output_functions\160\176A\160\160B\144\160\176\001\005n%state@\160\176\001\006\182\005\001\184@@@@@\208\208@>pp_get_formatter_tag_functions\160\176A\160\160B\144\160\176\001\004\220%state@\160\176\001\006\209\005\001\197@@@@@@A-pp_get_margin\160\176@\160\160B\144\160\176\001\005[%state@\160\176\001\006\186\005\001\208@@@@\144\179@\004\007\166\166E\144)pp_margin\160\144\004\012@@BC0pp_get_mark_tags\160\176@\160\160B\144\160\176\001\004\215%state@\160\176\001\006\211\005\001\227@@@@\144\179@\004\007\166\166U\144,pp_mark_tags\160\144\004\012@\208\208\208@0pp_get_max_boxes\160\176@\160\160B\144\160\176\001\005B%state@\160\176\001\006\190\005\001\249@@@@\144\179@\004\007\166\166N\144,pp_max_boxes\160\144\004\012@@A1pp_get_max_indent\160\176@\160\160B\144\160\176\001\005T%state@\160\176\001\006\187\005\002\012@@@@\144\179@\004\007\166\166G\144-pp_max_indent\160\144\004\012@@B1pp_get_print_tags\160\176@\160\160B\144\160\176\001\004\213%state@\160\176\001\006\212\005\002\031@@@@\144\179@\004\007\166\166T\144-pp_print_tags\160\144\004\012@\208@+pp_open_box\160\176@\160\160B\144\160\176\001\005\011%state@\160\176\001\005\012&indent@@@@@@ACDEFG,pp_open_hbox\160\176@\160\160B\144\160\176\001\005\004%state@\160\176\001\006\207\005\002?@@@@@\208\208\208\208@.pp_open_hovbox\160\176@\160\160B\144\160\176\001\005\t%state@\160\176\001\005\n&indent@@@@@@A-pp_open_hvbox\160\176@\160\160B\144\160\176\001\005\007%state@\160\176\001\005\b&indent@@@@@\208@+pp_open_tag\160\176A\160\160B\144\160\176\001\004\200%state@\160\176\001\004\201(tag_name@@@@@\208@,pp_open_tbox\160\176@\160\160B\144\160\176\001\005\031%state@\160\176\001\006\200\005\002t@@@@@@ABC,pp_open_vbox\160\176@\160\160B\144\160\176\001\005\005%state@\160\176\001\005\006&indent@@@@@\208\208@1pp_over_max_boxes\160\176A\160\160B\144\160\176\001\005D%state@\160\176\001\006\189\005\002\141@@@@@@A+pp_print_as\160\176@\160\160C\144\160\176\001\004\237%state@\160\176\001\004\238%isize@\160\176\001\004\239!s@@@@@\208@-pp_print_bool\160\176@\160\160B\144\160\176\001\004\250%state@\160\176\001\004\251!b@@@@@\208@.pp_print_break\160\176A\160\160C\144\160\176\001\005\022%state@\160\176\001\005\023%width@\160\176\001\005\024&offset@@@@@@ABCD-pp_print_char\160\176@\160\160B\144\160\176\001\004\253%state@\160\176\001\004\254!c@@@@@\208\208\208\208@,pp_print_cut\160\176A\160\160B\144\160\176\001\005\029%state@\160\176\001\006\201\005\002\212@@@@@@A.pp_print_float\160\176@\160\160B\144\160\176\001\004\247%state@\160\176\001\004\248!f@@@@@\208@.pp_print_flush\160\176@\160\160B\144\160\176\001\005\016%state@\160\176\001\006\205\005\002\236@@@@@\208@3pp_print_if_newline\160\176@\160\160B\144\160\176\001\005\020%state@\160\176\001\006\203\005\002\248@@@@@@ABC,pp_print_int\160\176@\160\160B\144\160\176\001\004\244%state@\160\176\001\004\245!i@@@@@\208\208\208\208@-pp_print_list\160\176@\160\160D\144\160\176\001\005/%*opt*@\160\176\001\0052$pp_v@\160\176\001\0053#ppf@\160\176\001\006\194%param@@@@@@A0pp_print_newline\160\176@\160\160B\144\160\176\001\005\015%state@\160\176\001\006\206\005\003%@@@@@@B.pp_print_space\160\176A\160\160B\144\160\176\001\005\028%state@\160\176\001\006\202\005\0030@@@@@@C/pp_print_string\160\176@\160\160B\144\160\176\001\004\241%state@\160\176\001\004\242!s@@@@@\208@,pp_print_tab\160\176A\160\160B\144\160\176\001\005*%state@\160\176\001\006\198\005\003H@@@@@@ADE/pp_print_tbreak\160\176A\160\160C\144\160\176\001\005%%state@\160\176\001\005&%width@\160\176\001\005'&offset@@@@@\208\208\208\208@-pp_print_text\160\176A\160\160B\144\160\176\001\0058#ppf@\160\176\001\0059!s@@@@@\208@\t%pp_set_all_formatter_output_functions\160\176A\160\160E\144\160\176\001\005p%state@\160\176\001\005q!f@\160\176\001\005r!g@\160\176\001\005s!h@\160\176\001\005t!i@@@@@@AB4pp_set_ellipsis_text\160\176A\160\160B\144\160\176\001\005G%state@\160\176\001\005H!s@@@@\144\179@\004\b\166\183OA\144\005\001\240\160\144\004\012\160\144\004\011@@C<pp_set_formatter_out_channel\160\176A\160\160B\144\160\176\001\005~%state@\160\176\001\005\127\"os@@@@@\208\208\208@>pp_set_formatter_out_functions\160\176A\160\160B\144\160\176\001\005b%state@\160\176\001\006\185\005\003\172@@@@@@A\t!pp_set_formatter_output_functions\160\176A\160\160C\144\160\176\001\005j%state@\160\176\001\005k!f@\160\176\001\005l!g@@@@@\208@>pp_set_formatter_tag_functions\160\176A\160\160B\144\160\176\001\004\222%state@\160\176\001\006\208\005\003\199@@@@@@AB-pp_set_margin\160\176@\160\160B\144\160\176\001\005V%state@\160\176\001\005W!n@@@@@@CD0pp_set_mark_tags\160\176A\160\160B\144\160\176\001\004\210%state@\160\176\001\004\211!b@@@@\144\179@\004\b\166\183U@\144\005\001\252\160\144\004\012\160\144\004\011@\208\208\208@0pp_set_max_boxes\160\176A\160\160B\144\160\176\001\005?%state@\160\176\001\005@!n@@@@@@A1pp_set_max_indent\160\176@\160\160B\144\160\176\001\005Q%state@\160\176\001\005R!n@@@@@@B1pp_set_print_tags\160\176A\160\160B\144\160\176\001\004\207%state@\160\176\001\004\208!b@@@@\144\179@\004\b\166\183T@\144\005\001\240\160\144\004\012\160\144\004\011@\208@*pp_set_tab\160\176@\160\160B\144\160\176\001\005,%state@\160\176\001\006\197\005\004$@@@@@\208@+pp_set_tags\160\176A\160\160B\144\160\176\001\004\217%state@\160\176\001\004\218!b@@@@@@ABCEFGH(print_as\160\176@\160\160B\144\160\176\001\007g\005\004$@\160\176\001\007h\005\004&@@@@@\208\208\208@*print_bool\160\176@\160\160A\144\160\176\001\007b\005\0041@@@@@\208@+print_break\160\176A\160\160B\144\160\176\001\007`\005\004:@\160\176\001\007a\005\004<@@@@@@AB*print_char\160\176@\160\160A\144\160\176\001\007c\005\004D@@@@@\208\208@)print_cut\160\176A\160\160A\144\160\176\001\007_\005\004N@@@@@@A+print_float\160\176@\160\160A\144\160\176\001\007d\005\004V@@@@@\208@+print_flush\160\176@\160\160A\144\160\176\001\007\\\005\004_@@@@@\208@0print_if_newline\160\176@\160\160A\144\160\176\001\007Z\005\004h@@@@@@ABCD)print_int\160\176@\160\160A\144\160\176\001\007e\005\004p@@@@@\208\208\208\208\208@-print_newline\160\176@\160\160A\144\160\176\001\007[\005\004}@@@@@@A+print_space\160\176A\160\160A\144\160\176\001\007^\005\004\133@@@@@@B,print_string\160\176@\160\160A\144\160\176\001\007f\005\004\141@@@@@\208@)print_tab\160\176A\160\160A\144\160\176\001\007T\005\004\150@@@@@@AC,print_tbreak\160\176A\160\160B\144\160\176\001\007V\005\004\158@\160\176\001\007W\005\004\160@@@@@\208\208\208@&printf\160\176@\160\160A\144\160\176\001\0069#fmt@@@@@\208@\t\"set_all_formatter_output_functions\160\176A\160\160D\144\160\176\001\007A\005\004\181@\160\176\001\007B\005\004\183@\160\176\001\007C\005\004\185@\160\176\001\007D\005\004\187@@@@@@AB1set_ellipsis_text\160\176A\160\160A\144\160\176\001\007L\005\004\195@@@@@@C9set_formatter_out_channel\160\176A\160\160A\144\160\176\001\007J\005\004\203@@@@@\208\208@;set_formatter_out_functions\160\176A\160\160A\144\160\176\001\007I\005\004\213@@@@@@A>set_formatter_output_functions\160\176A\160\160B\144\160\176\001\007F\005\004\221@\160\176\001\007G\005\004\223@@@@@\208@;set_formatter_tag_functions\160\176A\160\160A\144\160\176\001\007?\005\004\232@@@@@@ABDE*set_margin\160\176@\160\160A\144\160\176\001\007S\005\004\240@@@@@\208\208\208@-set_mark_tags\160\176A\160\160A\144\160\176\001\007;\005\004\251@@@@@@A-set_max_boxes\160\176A\160\160A\144\160\176\001\007O\005\005\003@@@@@@B.set_max_indent\160\176@\160\160A\144\160\176\001\007Q\005\005\011@@@@@\208\208@.set_print_tags\160\176A\160\160A\144\160\176\001\007=\005\005\021@@@@@@A'set_tab\160\176@\160\160A\144\160\176\001\007U\005\005\029@@@@@\208\208@(set_tags\160\176A\160\160A\144\160\176\001\0079\005\005'@@@@@\208@'sprintf\160\176@\160\160A\144\160\176\001\006D#fmt@@@@@@AB-std_formatter\160\176@@@@\208@&stdbuf\160\176A@@@\208@-str_formatter\160\176@@@@@ABCDEFGI\144*blank_line@")));
            ("gc.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\001\202\000\000\000t\000\000\001\145\000\000\001\127\176\208\208@/allocated_bytes\160\176A\160\160A\144\160\176\001\004+%param@@@@@\208@,create_alarm\160\176@\160\160A\144\160\176\001\004#!f@@@@@\208@,delete_alarm\160\176A\160\160A\144\160\176\001\004&!a@@@@\144\179@\004\005\166\183@@@\160\144\004\b\160\145\161@\144%false@@ABC(finalise\160@\144\179@\160\176\001\004)$prim@\160\176\001\004(\004\003@@\166\155\2403caml_final_registerBA @@\144\176\193 \176\193\004\003\176\150\176\144\144!a\002\005\245\225\000\001\017\022\001\004\026\001\017\011\176\179\144\176F$unit@@\144@\002\005\245\225\000\001\017\012@\002\005\245\225\000\001\017\015\176\193\004\017\004\014\176\179\004\b@\144@\002\005\245\225\000\001\017\016@\002\005\245\225\000\001\017\019@\002\005\245\225\000\001\017\020\160\144\004\"\160\144\004!@\208\208@0finalise_release\160@\144\179@\160\176\001\004'\004+@@\166\155\2402caml_final_releaseAA\004(@@\144\176\193\004'\176\179\004\030@\144@\002\005\245\225\000\001\017\023\176\179\004!@\144@\002\005\245\225\000\001\017\026@\002\005\245\225\000\001\017\029\160\144\004\016@@A*print_stat\160\176@\160\160A\144\160\176\001\004\020!c@@@@@@BD@@")));
            ("genlex.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000E\000\000\000\019\000\000\000?\000\000\000;\176\208@*make_lexer\160\176A\160\160A\144\160\176\001\004\001(keywords@@\160\160A\144\160\176\001\004v%input@@@@@@A\144'Hashtbl@")));
            ("hashtbl.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\003(\000\000\001\003\000\000\003X\000\000\0039\176\208\208\208\208@$Make\160\176A\160\160A\144\160\176\001\005\022!H@@@@@\208@*MakeSeeded\160\176A\160\160A\144\160\176\001\004\191!H@@@@@@AB#add\160\176A\160\160C\144\160\176\001\0049!h@\160\176\001\004:#key@\160\176\001\004;$info@@@@@@C%clear\160\176A\160\160A\144\160\176\001\004\030!h@@@@@\208@$copy\160\176A\160\160A\144\160\176\001\004%!h@@@@@@AD&create\160\176A\160\160B\144\160\176\001\004\023%*opt*@\160\176\001\004\026,initial_size@@@@@\208\208\208\208@$find\160\176@\160\160B\144\160\176\001\004L!h@\160\176\001\004M#key@@@@@@A(find_all\160\176@\160\160B\144\160\176\001\004X!h@\160\176\001\004Y#key@@@@@@B$fold\160\176@\160\160C\144\160\176\001\004y!f@\160\176\001\004z!h@\160\176\001\004{$init@@@@@\208\208@$hash\160\176@\160\160A\144\160\176\001\003\243!x@@@@@\208@*hash_param\160\176@\160\160C\144\160\176\001\003\245\"n1@\160\176\001\003\246\"n2@\160\176\001\003\247!x@@@@@@AB$iter\160\176A\160\160B\144\160\176\001\004p!f@\160\176\001\004q!h@@@@@\208@&length\160\176@\160\160A\144\160\176\001\004'!h@@@@\144\179@\004\005\166\166@\144$size\160\144\004\n@@ACD#mem\160\176A\160\160B\144\160\176\001\004i!h@\160\176\001\004j#key@@@@@\208\208\208@)randomize\160\176A\160\160A\144\160\176\001\005\171%param@@@@@@A&remove\160\176A\160\160B\144\160\176\001\004?!h@\160\176\001\004@#key@@@@@\208@'replace\160\176A\160\160C\144\160\176\001\004_!h@\160\176\001\004`#key@\160\176\001\004a$info@@@@@@AB%reset\160\176A\160\160A\144\160\176\001\004\"!h@@@@@\208\208@+seeded_hash\160\176@\160\160B\144\160\176\001\003\249$seed@\160\176\001\003\250!x@@@@@\208@1seeded_hash_param\160@@@AB%stats\160\176A\160\160A\144\160\176\001\004\142!h@@@@@@CDEF\1442randomized_default@")));
            ("int32.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\001\248\000\000\000\158\000\000\001\250\000\000\001\235\176\208\208\208@#abs\160\176@\160\160A\144\160\176\001\004\n!n@@@@@\208\208@'compare\160\176@\160\160B\144\160\176\001\004\021!x@\160\176\001\004\022!y@@@@\144\179@\004\b\166\155\2402caml_int32_compareB@ @@@\160\144\004\014\160\144\004\r@@A&lognot\160\176A\160\160A\144\160\176\001\004\014!n@@@@\144\179@\004\005\166\b\000\000\004\"A\160\144\004\b\160\145\144\148\018_i\000\255\255\255\255@@BC'max_int\160@@\208\208@'min_int\160@@@A)minus_one\160@@@BD#one\160@@\208\208@$pred\160\176A\160\160A\144\160\176\001\004\b!n@@@@\144\179@\004\005\166\b\000\000\004\028A\160\144\004\b\160\145\144\148\018_i\000\000\000\000\001@@A$succ\160\176A\160\160A\144\160\176\001\004\006!n@@@@\144\179@\004\005\166\b\000\000\004\027A\160\144\004\b\160\145\144\148\018_i\000\000\000\000\001@\208\208@)to_string\160\176@\160\160A\144\160\176\001\004\017!n@@@@\144\179@\004\005\166\155\2401caml_int32_formatBA @@\144\176\193 \176\179\144\176C&string@@\144@\002\005\245\225\000\001\003\236\176\193\004\t\176\179\144\176L%int32@@\144@\002\005\245\225\000\001\003\239\176\179\004\014@\144@\002\005\245\225\000\001\003\242@\002\005\245\225\000\001\003\245@\002\005\245\225\000\001\003\246\160\145\144\162\"%d@\160\144\004%@@A$zero\160@@@BCE@@")));
            ("int64.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002\004\000\000\000\158\000\000\001\253\000\000\001\235\176\208\208\208@#abs\160\176@\160\160A\144\160\176\001\004\012!n@@@@@\208\208@'compare\160\176@\160\160B\144\160\176\001\004\025!x@\160\176\001\004\026!y@@@@\144\179@\004\b\166\155\2402caml_int64_compareB@ @@@\160\144\004\014\160\144\004\r@@A&lognot\160\176A\160\160A\144\160\176\001\004\016!n@@@@\144\179@\004\005\166\b\000\000\004\"B\160\144\004\b\160\145\144\149\018_j\000\255\255\255\255\255\255\255\255@@BC'max_int\160@@\208\208@'min_int\160@@@A)minus_one\160@@@BD#one\160@@\208\208@$pred\160\176A\160\160A\144\160\176\001\004\n!n@@@@\144\179@\004\005\166\b\000\000\004\028B\160\144\004\b\160\145\144\149\018_j\000\000\000\000\000\000\000\000\001@@A$succ\160\176A\160\160A\144\160\176\001\004\b!n@@@@\144\179@\004\005\166\b\000\000\004\027B\160\144\004\b\160\145\144\149\018_j\000\000\000\000\000\000\000\000\001@\208\208@)to_string\160\176@\160\160A\144\160\176\001\004\019!n@@@@\144\179@\004\005\166\155\2401caml_int64_formatBA @@\144\176\193 \176\179\144\176C&string@@\144@\002\005\245\225\000\001\003\250\176\193\004\t\176\179\144\176M%int64@@\144@\002\005\245\225\000\001\003\253\176\179\004\014@\144@\002\005\245\225\000\001\004\000@\002\005\245\225\000\001\004\003@\002\005\245\225\000\001\004\004\160\145\144\162\"%d@\160\144\004%@@A$zero\160@@@BCE@@")));
            ("lazy.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\001E\000\000\000\\\000\000\001:\000\000\001-\176\208\208\208@)Undefined\160\176@@@@@A)force_val\160\176@\160\160A\144\160\176\001\004\000#lzv@@@@@\208@(from_fun\160\176@\160\160A\144\160\176\001\003\246!f@@@@@@AB(from_val\160\176@\160\160A\144\160\176\001\003\249!v@@@@@\208\208@&is_val\160\176A\160\160A\144\160\176\001\003\252!l@@@@\144\179@\004\005\166\157A\160\166\155\240,caml_obj_tagAA @@\144\176\193 \176\179\144\176\001\003\240!t@@\144@\002\005\245\225\000\001\003\r\176\179\144\176A#int@@\144@\002\005\245\225\000\001\003\016@\002\005\245\225\000\001\003\019\160\144\004\030@\160\166\166D\145(lazy_tag\160\166\147\176@#ObjA@@@@A-lazy_from_fun\160\004C@\208@-lazy_from_val\160\004=@\208@+lazy_is_val\160\0045@@ABCD@@")));
            ("lexing.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\003\136\000\000\001\002\000\000\003Z\000\000\003/\176\208\208\208@)dummy_pos\160@@\208\208@&engine\160\176@\160\160C\144\160\176\001\004\018#tbl@\160\176\001\004\019%state@\160\176\001\004\020#buf@@@@@@A+flush_input\160\176A\160\160A\144\160\176\001\004S\"lb@@@@@@BC,from_channel\160\176A\160\160A\144\160\176\001\004+\"ic@@@@@\208@-from_function\160\176A\160\160A\144\160\176\001\004)!f@@@@@@AD+from_string\160\176A\160\160A\144\160\176\001\004/!s@@@@@\208\208\208@&lexeme\160\176A\160\160A\144\160\176\001\0042&lexbuf@@@@@@A+lexeme_char\160\176A\160\160B\144\160\176\001\004E&lexbuf@\160\176\001\004F!i@@@@@\208@*lexeme_end\160\176@\160\160A\144\160\176\001\004J&lexbuf@@@@\144\179@\004\005\166\166C\144(pos_cnum\160\166\166K\144*lex_curr_p\160\144\004\015@@\208@,lexeme_end_p\160\176@\160\160A\144\160\176\001\004N&lexbuf@@@@\144\179@\004\005\166\166K\144\004\018\160\144\004\t@@ABC,lexeme_start\160\176@\160\160A\144\160\176\001\004H&lexbuf@@@@\144\179@\004\005\166\166C\144\004'\160\166\166J\144+lex_start_p\160\144\004\014@@\208\208@.lexeme_start_p\160\176@\160\160A\144\160\176\001\004L&lexbuf@@@@\144\179@\004\005\166\166J\144\004\019\160\144\004\t@\208@*new_engine\160\176@\160\160C\144\160\176\001\004\023#tbl@\160\176\001\004\024%state@\160\176\001\004\025#buf@@@@@@AB(new_line\160\176A\160\160A\144\160\176\001\004P&lexbuf@@@@@\208\208@*sub_lexeme\160\176A\160\160C\144\160\176\001\0045&lexbuf@\160\176\001\0046\"i1@\160\176\001\0047\"i2@@@@@@A/sub_lexeme_char\160\176A\160\160B\144\160\176\001\004?&lexbuf@\160\176\001\004@!i@@@@\144\179@\004\b\166g\160\166\166A\144*lex_buffer\160\144\004\015@\160\144\004\014@\208\208@3sub_lexeme_char_opt\160\176A\160\160B\144\160\176\001\004B&lexbuf@\160\176\001\004C!i@@@@@@A.sub_lexeme_opt\160\176A\160\160C\144\160\176\001\004:&lexbuf@\160\176\001\004;\"i1@\160\176\001\004<\"i2@@@@@@BCDEF@@")));
            ("list.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\006\138\000\000\002\026\000\000\006\243\000\000\006\198\176\208\208\208\208\208\208@&append\160\176@\160\160B\144\160\176\001\004\132\"l1@\160\176\001\004\133\"l2@@@@@@A%assoc\160\176@\160\160B\144\160\176\001\004\141!x@\160\176\001\005\154%param@@@@@\208@$assq\160\176@\160\160B\144\160\176\001\004\146!x@\160\176\001\005\152\004\r@@@@@\208@'combine\160\176A\160\160B\144\160\176\001\004\198\"l1@\160\176\001\004\199\"l2@@@@@@ABC&concat\160\176@\160\160A\144\160\176\001\005\188\004\"@@@@@\208@&exists\160\176A\160\160B\144\160\176\001\004q!p@\160\176\001\005\164\004.@@@@@@AD'exists2\160\176A\160\160C\144\160\176\001\004}!p@\160\176\001\004~\"l1@\160\176\001\004\127\"l2@@@@@\208\208\208\208@)fast_sort\160\176@\160\160B\144\160\176\001\004\220#cmp@\160\176\001\004\221!l@@@@@@A&filter\160\176@\160\160A\144\160\176\001\004\177!p@@\160\160A\144\160\176\001\005\194%param@@@@@@B$find\160\176@\160\160B\144\160\176\001\004\173!p@\160\176\001\005\145\004g@@@@@\208@(find_all\160\004\027@@AC'flatten\160\004P@\208@)fold_left\160\176@\160\160C\144\160\176\001\0042!f@\160\176\001\0043$accu@\160\176\001\0044!l@@@@@\208@*fold_left2\160\176@\160\160D\144\160\176\001\004[!f@\160\176\001\004\\$accu@\160\176\001\004]\"l1@\160\176\001\004^\"l2@@@@@@ABDE*fold_right\160\176@\160\160C\144\160\176\001\0048!f@\160\176\001\0049!l@\160\176\001\004:$accu@@@@@\208\208@+fold_right2\160\176@\160\160D\144\160\176\001\004d!f@\160\176\001\004e\"l1@\160\176\001\004f\"l2@\160\176\001\004g$accu@@@@@\208@'for_all\160\176A\160\160B\144\160\176\001\004m!p@\160\176\001\005\165\004\190@@@@@\208@(for_all2\160\176A\160\160C\144\160\176\001\004u!p@\160\176\001\004v\"l1@\160\176\001\004w\"l2@@@@@@ABC\"hd\160\176@\160\160A\144\160\176\001\005\192\004\214@@@@@\208@$iter\160\176@\160\160B\144\160\176\001\004&!f@\160\176\001\005\184\004\226@@@@@\208\208@%iter2\160\176A\160\160C\144\160\176\001\004S!f@\160\176\001\004T\"l1@\160\176\001\004U\"l2@@@@@@A%iteri\160\176@\160\160B\144\160\176\001\004/!f@\160\176\001\0040!l@@@@@@BCDF&length\160\176@\160\160A\144\160\176\001\003\245!l@@@@@\208\208\208@#map\160\176A\160\160B\144\160\176\001\004\017!f@\160\176\001\005\187\005\001\022@@@@@\208@$map2\160\176A\160\160C\144\160\176\001\004>!f@\160\176\001\004?\"l1@\160\176\001\004@\"l2@@@@@@AB$mapi\160\176A\160\160B\144\160\176\001\004\028!f@\160\176\001\004\029!l@@@@@\208\208\208@#mem\160\176A\160\160B\144\160\176\001\004\133!x@\160\176\001\005\157\005\001@@@@@@@A)mem_assoc\160\176A\160\160B\144\160\176\001\004\151!x@\160\176\001\005\150\005\001K@@@@@\208\208@(mem_assq\160\176A\160\160B\144\160\176\001\004\156!x@\160\176\001\005\148\005\001X@@@@@@A$memq\160\176A\160\160B\144\160\176\001\004\137!x@\160\176\001\005\156\005\001c@@@@@\208@%merge\160\176@\160\160C\144\160\176\001\004\205#cmp@\160\176\001\004\206\"l1@\160\176\001\004\207\"l2@@@@@@ABC#nth\160\176@\160\160B\144\160\176\001\003\253!l@\160\176\001\003\254!n@@@@@\208@)partition\160\176@\160\160B\144\160\176\001\004\184!p@\160\176\001\004\185!l@@@@@\208@,remove_assoc\160\176@\160\160B\144\160\176\001\004\161!x@\160\176\001\005\147\005\001\152@@@@@\208@+remove_assq\160\176@\160\160B\144\160\176\001\004\167!x@\160\176\001\005\146\005\001\164@@@@@@ABCDE#rev\160\176@\160\160A\144\160\176\001\004\011!l@@@@@\208\208\208@*rev_append\160\176@\160\160B\144\160\176\001\004\006\"l1@\160\176\001\004\007\"l2@@@@@@A'rev_map\160\176@\160\160B\144\160\176\001\004\031!f@\160\176\001\004 !l@@@@@@B(rev_map2\160\176@\160\160C\144\160\176\001\004G!f@\160\176\001\004H\"l1@\160\176\001\004I\"l2@@@@@\208\208@$sort\160\005\001\152@\208@)sort_uniq\160\176@\160\160B\144\160\176\001\005\020#cmp@\160\176\001\005\021!l@@@@@@AB%split\160\176A\160\160A\144\160\176\001\005\140\005\001\240@@@@@\208\208@+stable_sort\160\005\001\177@@A\"tl\160\176@\160\160A\144\160\176\001\005\191\005\001\252@@@@@@BCDFG@@")));
            ("listLabels.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\006`\000\000\002\r\000\000\006\200\000\000\006\156\176\208\208\208\208\208\208@&append\160\176@\160\160B\144\160\176\001\004\132\"l1@\160\176\001\004\133\"l2@@@@@@A%assoc\160\176@\160\160B\144\160\176\001\004\141!x@\160\176\001\005\154%param@@@@@\208@$assq\160\176@\160\160B\144\160\176\001\004\146!x@\160\176\001\005\152\004\r@@@@@\208@'combine\160\176A\160\160B\144\160\176\001\004\198\"l1@\160\176\001\004\199\"l2@@@@@@ABC&concat\160\176@\160\160A\144\160\176\001\005\188\004\"@@@@@\208@&exists\160\176A\160\160B\144\160\176\001\004q!p@\160\176\001\005\164\004.@@@@@@AD'exists2\160\176A\160\160C\144\160\176\001\004}!p@\160\176\001\004~\"l1@\160\176\001\004\127\"l2@@@@@\208\208\208\208@)fast_sort\160\176@\160\160B\144\160\176\001\004\220#cmp@\160\176\001\004\221!l@@@@@@A&filter\160\176@\160\160A\144\160\176\001\004\177!p@@\160\160A\144\160\176\001\005\194%param@@@@@@B$find\160\176@\160\160B\144\160\176\001\004\173!p@\160\176\001\005\145\004g@@@@@\208@(find_all\160\004\027@@AC'flatten\160\004P@\208@)fold_left\160\176@\160\160C\144\160\176\001\0042!f@\160\176\001\0043$accu@\160\176\001\0044!l@@@@@\208@*fold_left2\160\176@\160\160D\144\160\176\001\004[!f@\160\176\001\004\\$accu@\160\176\001\004]\"l1@\160\176\001\004^\"l2@@@@@@ABDE*fold_right\160\176@\160\160C\144\160\176\001\0048!f@\160\176\001\0049!l@\160\176\001\004:$accu@@@@@\208\208@+fold_right2\160\176@\160\160D\144\160\176\001\004d!f@\160\176\001\004e\"l1@\160\176\001\004f\"l2@\160\176\001\004g$accu@@@@@\208@'for_all\160\176A\160\160B\144\160\176\001\004m!p@\160\176\001\005\165\004\190@@@@@\208@(for_all2\160\176A\160\160C\144\160\176\001\004u!p@\160\176\001\004v\"l1@\160\176\001\004w\"l2@@@@@@ABC\"hd\160\176@\160\160A\144\160\176\001\005\192\004\214@@@@@\208@$iter\160\176@\160\160B\144\160\176\001\004&!f@\160\176\001\005\184\004\226@@@@@\208\208@%iter2\160\176A\160\160C\144\160\176\001\004S!f@\160\176\001\004T\"l1@\160\176\001\004U\"l2@@@@@@A%iteri\160\176@\160\160B\144\160\176\001\004/!f@\160\176\001\0040!l@@@@@@BCDF&length\160\176@\160\160A\144\160\176\001\003\245!l@@@@@\208\208\208@#map\160\176A\160\160B\144\160\176\001\004\017!f@\160\176\001\005\187\005\001\022@@@@@\208@$map2\160\176A\160\160C\144\160\176\001\004>!f@\160\176\001\004?\"l1@\160\176\001\004@\"l2@@@@@@AB$mapi\160\176A\160\160B\144\160\176\001\004\028!f@\160\176\001\004\029!l@@@@@\208\208\208@#mem\160\176A\160\160B\144\160\176\001\004\133!x@\160\176\001\005\157\005\001@@@@@@@A)mem_assoc\160\176A\160\160B\144\160\176\001\004\151!x@\160\176\001\005\150\005\001K@@@@@\208\208@(mem_assq\160\176A\160\160B\144\160\176\001\004\156!x@\160\176\001\005\148\005\001X@@@@@@A$memq\160\176A\160\160B\144\160\176\001\004\137!x@\160\176\001\005\156\005\001c@@@@@\208@%merge\160\176@\160\160C\144\160\176\001\004\205#cmp@\160\176\001\004\206\"l1@\160\176\001\004\207\"l2@@@@@@ABC#nth\160\176@\160\160B\144\160\176\001\003\253!l@\160\176\001\003\254!n@@@@@\208@)partition\160\176@\160\160B\144\160\176\001\004\184!p@\160\176\001\004\185!l@@@@@\208@,remove_assoc\160\176@\160\160B\144\160\176\001\004\161!x@\160\176\001\005\147\005\001\152@@@@@\208@+remove_assq\160\176@\160\160B\144\160\176\001\004\167!x@\160\176\001\005\146\005\001\164@@@@@@ABCDE#rev\160\176@\160\160A\144\160\176\001\004\011!l@@@@@\208\208@*rev_append\160\176@\160\160B\144\160\176\001\004\006\"l1@\160\176\001\004\007\"l2@@@@@@A'rev_map\160\176@\160\160B\144\160\176\001\004\031!f@\160\176\001\004 !l@@@@@\208\208@(rev_map2\160\176@\160\160C\144\160\176\001\004G!f@\160\176\001\004H\"l1@\160\176\001\004I\"l2@@@@@\208@$sort\160\005\001\152@@AB%split\160\176A\160\160A\144\160\176\001\005\140\005\001\227@@@@@\208\208@+stable_sort\160\005\001\164@@A\"tl\160\176@\160\160A\144\160\176\001\005\191\005\001\239@@@@@@BCDFG@@")));
            ("map.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000&A\000\000\011)\000\000$>\000\000#\248\176\208@$Make\160\176A\160\160A\144\160\176\001\004\014#Ord@@@@\144\179@\160\176\001\005\128&funarg@@\196B\176\001\005\222&height@\179@\160\176\001\005\223%param@@\188\144\004\004\166\166D@\160\004\004@\145\144\144@\196B\176\001\005\229&create@\179@\160\176\001\005\230!l@\160\176\001\005\231!x@\160\176\001\005\232!d@\160\176\001\005\233!r@@\196@\176\001\005\234\"hl@\178\144\004#\160\144\004\018@\160\176\192&map.ml\000@\001\t8\001\tG\192\004\002\000@\001\t8\001\tO@A\196@\176\001\005\235\"hr@\178\004\012\160\144\004\020@\160\176\192\004\011\000@\001\t8\001\tY\192\004\012\000@\001\t8\001\ta@A\166\181@\160$NodeA@\160\004\021\160\144\004%\160\144\004$\160\004\015\160\188\166\157E\160\144\004%\160\144\004\027@\166L\160\004\005\160\145\144\144A@\166L\160\004\t\160\145\144\144A@@\196B\176\001\005\236)singleton@\179@\160\176\001\005\237!x@\160\176\001\005\238!d@@\166\181@\160\004(A@\160\145\161@\144%Empty\160\144\004\015\160\144\004\014\160\145\161@\144\004\t\160\145\144\144A@\196B\176\001\005\239#bal@\179@\160\176\001\005\240!l@\160\176\001\005\241!x@\160\176\001\005\242!d@\160\176\001\005\243!r@@\196B\176\001\005\244\"hl@\188\144\004\016\166\166D@\160\004\004@\145\144\144@\196B\176\001\005\250\"hr@\188\144\004\018\166\166D@\160\004\004@\145\144\144@\188\166\157C\160\144\004\026\160\166L\160\144\004\019\160\145\144\144B@@\188\004 \196A\176\001\006\001\"lr@\166\166C@\160\004&@\196A\176\001\006\002\"ld@\166\166B@\160\004,@\196A\176\001\006\003\"lv@\166\166A@\160\0042@\196A\176\001\006\004\"ll@\166\166@@\160\0048@\188\166\004}\160\178\004\158\160\144\004\011@\160\176\192\004\157\000L\001\n\244\001\011\003\192\004\158\000L\001\n\244\001\011\012@A\160\178\004\166\160\144\004%@\160\176\192\004\165\000L\001\n\244\001\011\016\192\004\166\000L\001\n\244\001\011\025@A@\178\144\004\193\160\004\016\160\144\004\"\160\144\004*\160\178\004\b\160\004\015\160\144\004b\160\144\004a\160\004N@\160\176\192\004\184\000M\001\011\031\001\011=\192\004\185\000M\001\011\031\001\011N@A@\160\176\192\004\188\000M\001\011\031\001\011-\004\004@A\188\004\028\178\004\023\160\178\004\025\160\004(\160\004\024\160\004\023\160\166\166@@\160\004&@@\160\176\192\004\202\000R\001\011\223\001\011\248\192\004\203\000R\001\011\223\001\012\r@A\160\166\166A@\160\004.@\160\166\166B@\160\0042@\160\178\004.\160\166\166C@\160\0048@\160\004)\160\004(\160\004u@\160\176\192\004\223\000R\001\011\223\001\012\022\192\004\224\000R\001\011\223\001\012(@A@\160\176\192\004\227\000R\001\011\223\001\011\241\004\004@A\166\156@\160\166\181@B@\160\166\147\176R0Invalid_argumentC@\160\145\144\162'Map.bal@@@\166\004\015\160\166\004\014\160\166\004\r@\160\145\144\162'Map.bal@@@\188\166\004\142\160\004\137\160\166L\160\004\144\160\145\144\144B@@\188\004\160\196A\176\001\006\011\"rr@\166\166C@\160\004\166@\196A\176\001\006\012\"rd@\166\166B@\160\004\172@\196A\176\001\006\r\"rv@\166\166A@\160\004\178@\196A\176\001\006\014\"rl@\166\166@@\160\004\184@\188\166\005\001\b\160\178\005\001)\160\144\004\029@\160\176\192\005\001(\000X\001\012\197\001\012\212\192\005\001)\000X\001\012\197\001\012\221@A\160\178\005\0011\160\144\004\019@\160\176\192\005\0010\000X\001\012\197\001\012\225\192\005\0011\000X\001\012\197\001\012\234@A@\178\004\139\160\178\004\141\160\004\217\160\004\133\160\004\132\160\004\012@\160\176\192\005\001;\000Y\001\012\240\001\r\005\192\005\001<\000Y\001\012\240\001\r\022@A\160\144\004*\160\144\0042\160\004\029@\160\176\192\005\001D\000Y\001\012\240\001\012\254\192\005\001E\000Y\001\012\240\001\r\031@A\188\004\026\178\004\160\160\178\004\162\160\004\238\160\004\154\160\004\153\160\166\166@@\160\004$@@\160\176\192\005\001S\000^\001\r\177\001\r\202\192\005\001T\000^\001\r\177\001\r\220@A\160\166\166A@\160\004,@\160\166\166B@\160\0040@\160\178\004\183\160\166\166C@\160\0046@\160\004&\160\004%\160\004A@\160\176\192\005\001h\000^\001\r\177\001\r\229\192\005\001i\000^\001\r\177\001\r\250@A@\160\176\192\005\001l\000^\001\r\177\001\r\195\004\004@A\166\004\137\160\166\004\136\160\166\004\135@\160\145\144\162'Map.bal@@@\166\004\147\160\166\004\146\160\166\004\145@\160\145\144\162'Map.bal@@@\166\181@\160\005\001tA@\160\005\001(\160\004\212\160\004\211\160\005\001 \160\188\166\005\001q\160\005\001\025\160\005\001\022@\166L\160\005\001\028\160\145\144\144A@\166L\160\005\001\030\160\145\144\144A@@\196B\176\001\006\021(is_empty@\179@\160\176\001\006\022\005\001\193@@\188\144\004\003\145\161@\144%false\145\161A\144$true\165\160\160\176\001\006\023#add@\179@\160\176\001\006\024!x@\160\176\001\006\025$data@\160\176\001\006\026\005\001\217@@\188\144\004\003\196A\176\001\006\028!r@\166\166C@\160\004\007@\196A\176\001\006\029!d@\166\166B@\160\004\r@\196A\176\001\006\030!v@\166\166A@\160\004\019@\196A\176\001\006\031!l@\166\166@@\160\004\025@\196@\176\001\006 !c@\178\166\166@\145'compare\160\144\005\002\005@\160\144\004-\160\144\004\025@\160\176\192\005\001\225\000k\001\015%\001\0157\192\005\001\226\000k\001\015%\001\015F@@\188\166\157@\160\144\004\022\160\145\144\144@@\166\181@\160\005\001\223A@\160\144\004%\160\004\022\160\144\004A\160\144\004<\160\166\166D@\160\004B@@\188\166\157B\160\004\023\160\145\144\144@@\178\144\005\001\186\160\178\144\004[\160\004,\160\004\022\160\004\026@\160\176\192\005\002\012\000o\001\015\162\001\015\178\192\005\002\r\000o\001\015\162\001\015\192@A\160\0041\160\144\004Q\160\004\029@\160\176\192\005\002\020\000o\001\015\162\001\015\174\192\005\002\021\000o\001\015\162\001\015\198@A\178\004\020\160\004(\160\004;\160\004\n\160\178\004\022\160\004A\160\004+\160\004*@\160\176\192\005\002!\000q\001\015\214\001\015\236\192\005\002\"\000q\001\015\214\001\015\250@A@\160\176\192\005\002%\000q\001\015\214\001\015\226\004\004@A\166\181@\160\005\002\025A@\160\145\161@\144\005\001\241\160\004R\160\004<\160\145\161@\144\005\001\247\160\145\144\144A@@\165\160\160\176\001\006!$find@\179@\160\176\001\006\"!x@\160\176\001\006#\005\002d@@\188\144\004\003\196@\176\001\006)!c@\178\166\166@\145'compare\160\004s@\160\144\004\017\160\166\166A@\160\004\016@@\160\176\192\005\002U\000w\001\016d\001\016v\192\005\002V\000w\001\016d\001\016\133@@\188\166\004t\160\144\004\022\160\145\144\144@@\166\166B@\160\004\031@\178\144\004*\160\004\023\160\188\166\004l\160\004\015\160\145\144\144@@\166\166@@\160\004-@\166\166C@\160\0040@@\160\176\192\005\002u\000y\001\016\163\001\016\178\192\005\002v\000y\001\016\163\001\016\209@A\166\156@\160\166\147\176T)Not_foundC@@@\165\160\160\176\001\006*#mem@\179@\160\176\001\006+!x@\160\176\001\006,\005\002\171@@\188\144\004\003\196@\176\001\0062!c@\178\166\166@\145'compare\160\004\186@\160\144\004\017\160\166\166A@\160\004\016@@\160\176\192\005\002\156\000\127\001\0170\001\017B\192\005\002\157\000\127\001\0170\001\017Q@@\166I\160\166\004\188\160\144\004\023\160\145\144\144@@\160\178\144\004)\160\004\022\160\188\166\004\178\160\004\r\160\145\144\144@@\166\166@@\160\004,@\166\166C@\160\004/@@\160\176\192\005\002\187\001\000\128\001\017U\001\017h\192\005\002\188\001\000\128\001\017U\001\017\134@A@\145\161@\144\005\001\028@\165\160\160\176\001\0063+min_binding@\179@\160\176\001\0064\005\002\234@@\188\144\004\003\196A\176\001\0065!l@\166\166@@\160\004\007@\188\144\004\007\178\144\004\017\160\004\004@\160\176\192\005\002\215\001\000\133\001\017\246\001\018\021\192\005\002\216\001\000\133\001\017\246\001\018\"@A\166\181@@@\160\166\166A@\160\004\022@\160\166\166B@\160\004\026@@\166\156@\160\166\147\004l@@@\165\160\160\176\001\006>+max_binding@\179@\160\176\001\006?\005\003\018@@\188\144\004\003\196A\176\001\006@!r@\166\166C@\160\004\007@\188\144\004\007\178\144\004\017\160\004\004@\160\176\192\005\002\255\001\000\138\001\018\146\001\018\177\192\005\003\000\001\000\138\001\018\146\001\018\190@A\166\004(\160\166\166A@\160\004\021@\160\166\166B@\160\004\025@@\166\156@\160\166\147\004\147@@@\165\160\160\176\001\006I2remove_min_binding@\179@\160\176\001\006J\005\0039@@\188\144\004\003\196A\176\001\006K!l@\166\166@@\160\004\007@\188\144\004\007\178\005\001\031\160\178\144\004\019\160\004\006@\160\176\192\005\003(\001\000\143\001\019A\001\019d\192\005\003)\001\000\143\001\019A\001\019z@A\160\166\166A@\160\004\022@\160\166\166B@\160\004\026@\160\166\166C@\160\004\030@@\160\176\192\005\0038\001\000\143\001\019A\001\019`\192\005\0039\001\000\143\001\019A\001\019\128@A\166\004\007\160\004$@\166\005\002X\160\166\005\002W\160\166\005\002V@\160\145\144\1622Map.remove_min_elt@@@@\196B\176\001\006T%merge@\179@\160\176\001\006U\"t1@\160\176\001\006V\"t2@@\188\144\004\007\188\144\004\006\196@\176\001\006Y%match@\178\004\133\160\144\004\012@\160\176\192\005\003\\\001\000\150\001\019\244\001\020\011\192\005\003]\001\000\150\001\019\244\001\020\025@A\178\005\001\\\160\144\004\022\160\166\166@@\160\144\004\017@\160\166\166A@\160\004\005@\160\178\004H\160\004\020@\160\176\192\005\003o\001\000\151\001\020\029\001\0202\192\005\003p\001\000\151\001\020\029\001\020I@A@\160\176\192\005\003s\001\000\151\001\020\029\001\020'\004\004@A\144\004*\144\004(\165\160\160\176\001\006\\&remove@\179@\160\176\001\006]!x@\160\176\001\006^\005\003\163@@\188\144\004\003\196A\176\001\006`!r@\166\166C@\160\004\007@\196A\176\001\006a!d@\166\166B@\160\004\r@\196A\176\001\006b!v@\166\166A@\160\004\019@\196A\176\001\006c!l@\166\166@@\160\004\025@\196@\176\001\006d!c@\178\166\166@\145'compare\160\005\001\202@\160\144\004)\160\144\004\024@\160\176\192\005\003\170\001\000\157\001\020\171\001\020\189\192\005\003\171\001\000\157\001\020\171\001\020\204@@\188\166\005\001\201\160\144\004\020\160\145\144\144@@\178\144\004o\160\144\004\"\160\144\0046@\160\176\192\005\003\188\001\000\159\001\020\232\001\020\244\192\005\003\189\001\000\159\001\020\232\001\020\253@A\188\166\005\001\196\160\004\018\160\145\144\144@@\178\005\001\195\160\178\144\004P\160\004%\160\004\020@\160\176\192\005\003\205\001\000\161\001\021\027\001\021+\192\005\003\206\001\000\161\001\021\027\001\0217@A\160\004)\160\144\004H\160\004\026@\160\176\192\005\003\213\001\000\161\001\021\027\001\021'\192\005\003\214\001\000\161\001\021\027\001\021=@A\178\005\001\213\160\004\"\160\0043\160\004\n\160\178\004\021\160\0049\160\004&@\160\176\192\005\003\225\001\000\163\001\021M\001\021c\192\005\003\226\001\000\163\001\021M\001\021o@A@\160\176\192\005\003\229\001\000\163\001\021M\001\021Y\004\004@A\145\161@\144\005\003\173@\165\160\160\176\001\006e$iter@\179@\160\176\001\006f!f@\160\176\001\006g\005\004\022@@\188\144\004\003\173\178\144\004\r\160\144\004\011\160\166\166@@\160\004\n@@\160\176\192\005\004\001\001\000\168\001\021\194\001\021\204\192\005\004\002\001\000\168\001\021\194\001\021\212@A\173\178\004\011\160\166\166A@\160\004\020@\160\166\166B@\160\004\024@@\160\176\192\005\004\015\001\000\168\001\021\194\001\021\214\192\005\004\016\001\000\168\001\021\194\001\021\219@@\178\004\026\160\004\025\160\166\166C@\160\004\"@@\160\176\192\005\004\025\001\000\168\001\021\194\001\021\221\192\005\004\026\001\000\168\001\021\194\001\021\229@A\145\161@\144\"()@\165\160\160\176\001\006m#map@\179@\160\176\001\006n!f@\160\176\001\006o\005\004L@@\188\144\004\003\196@\176\001\006u\"l'@\178\144\004\015\160\144\004\r\160\166\166@@\160\004\012@@\160\176\192\005\0049\001\000\174\001\022D\001\022W\192\005\004:\001\000\174\001\022D\001\022^@A\196@\176\001\006v\"d'@\178\004\r\160\166\166B@\160\004\024@@\160\176\192\005\004E\001\000\175\001\022b\001\022u\192\005\004F\001\000\175\001\022b\001\022x@@\196@\176\001\006w\"r'@\178\004\027\160\004\026\160\166\166C@\160\004%@@\160\176\192\005\004R\001\000\176\001\022|\001\022\143\192\005\004S\001\000\176\001\022|\001\022\150@A\166\181@\160\005\004GA@\160\144\004,\160\166\166A@\160\0042@\160\144\004#\160\144\004\025\160\166\166D@\160\004:@@\145\161@\144\005\004,@\165\160\160\176\001\006x$mapi@\179@\160\176\001\006y!f@\160\176\001\006z\005\004\149@@\188\144\004\003\196A\176\001\006~!v@\166\166A@\160\004\007@\196@\176\001\006\128\"l'@\178\144\004\021\160\144\004\019\160\166\166@@\160\004\018@@\160\176\192\005\004\136\001\000\183\001\023\026\001\023-\192\005\004\137\001\000\183\001\023\026\001\0235@A\196@\176\001\006\129\"d'@\178\004\r\160\144\004\026\160\166\166B@\160\004 @@\160\176\192\005\004\150\001\000\184\001\0239\001\023L\192\005\004\151\001\000\184\001\0239\001\023Q@@\196@\176\001\006\130\"r'@\178\004\029\160\004\028\160\166\166C@\160\004-@@\160\176\192\005\004\163\001\000\185\001\023U\001\023h\192\005\004\164\001\000\185\001\023U\001\023p@A\166\181@\160\005\004\152A@\160\144\004.\160\004\028\160\144\004\"\160\144\004\022\160\166\166D@\160\004?@@\145\161@\144\005\004z@\165\160\160\176\001\006\131$fold@\179@\160\176\001\006\132!f@\160\176\001\006\133!m@\160\176\001\006\134$accu@@\188\144\004\007\178\144\004\016\160\144\004\014\160\166\166C@\160\004\t@\160\178\004\007\160\166\166A@\160\004\015@\160\166\166B@\160\004\019@\160\178\004\019\160\004\018\160\166\166@@\160\004\026@\160\144\004\031@\160\176\192\005\004\228\001\000\192\001\023\250\001\024\020\192\005\004\229\001\000\192\001\023\250\001\024#@A@\160\176\192\005\004\232\001\000\192\001\023\250\001\024\r\192\005\004\233\001\000\192\001\023\250\001\024$@@@\160\176\192\005\004\236\001\000\192\001\023\250\001\024\004\004\004@A\004\012@\165\160\160\176\001\006\140'for_all@\179@\160\176\001\006\141!p@\160\176\001\006\142\005\005\026@@\188\144\004\003\166H\160\178\144\004\n\160\166\166A@\160\004\t@\160\166\166B@\160\004\r@@\160\176\192\005\005\b\001\000\196\001\024]\001\024|\192\005\005\t\001\000\196\001\024]\001\024\129@@\160\166H\160\178\144\004\031\160\004\019\160\166\166@@\160\004\027@@\160\176\192\005\005\022\001\000\196\001\024]\001\024\133\192\005\005\023\001\000\196\001\024]\001\024\144@A\160\178\004\012\160\004\030\160\166\166C@\160\004&@@\160\176\192\005\005!\001\000\196\001\024]\001\024\148\192\005\005\"\001\000\196\001\024]\001\024\159@A@@\145\161A\144\005\003~@\165\160\160\176\001\006\148&exists@\179@\160\176\001\006\149!p@\160\176\001\006\150\005\005S@@\188\144\004\003\166I\160\178\144\004\n\160\166\166A@\160\004\t@\160\166\166B@\160\004\r@@\160\176\192\005\005A\001\000\200\001\024\216\001\024\247\192\005\005B\001\000\200\001\024\216\001\024\252@@\160\166I\160\178\144\004\031\160\004\019\160\166\166@@\160\004\027@@\160\176\192\005\005O\001\000\200\001\024\216\001\025\000\192\005\005P\001\000\200\001\024\216\001\025\n@A\160\178\004\012\160\004\030\160\166\166C@\160\004&@@\160\176\192\005\005Z\001\000\200\001\024\216\001\025\014\192\005\005[\001\000\200\001\024\216\001\025\024@A@@\145\161@\144\005\003\187@\165\160\160\176\001\006\156/add_min_binding@\179@\160\176\001\006\157!k@\160\176\001\006\158!v@\160\176\001\006\159\005\005\143@@\188\144\004\003\178\005\003m\160\178\144\004\017\160\144\004\015\160\144\004\014\160\166\166@@\160\004\r@@\160\176\192\005\005}\001\000\213\001\026\199\001\026\211\192\005\005~\001\000\213\001\026\199\001\026\234@A\160\166\166A@\160\004\021@\160\166\166B@\160\004\025@\160\166\166C@\160\004\029@@\160\176\192\005\005\141\001\000\213\001\026\199\001\026\207\192\005\005\142\001\000\213\001\026\199\001\026\240@A\178\144\005\005e\160\004\030\160\004\029@\160\176\192\005\005\149\001\000\211\001\026\136\001\026\153\192\005\005\150\001\000\211\001\026\136\001\026\166@A@\165\160\160\176\001\006\165/add_max_binding@\179@\160\176\001\006\166!k@\160\176\001\006\167!v@\160\176\001\006\168\005\005\199@@\188\144\004\003\178\005\003\165\160\166\166@@\160\004\006@\160\166\166A@\160\004\n@\160\166\166B@\160\004\014@\160\178\144\004\029\160\144\004\027\160\144\004\026\160\166\166C@\160\004\025@@\160\176\192\005\005\193\001\000\218\001\027\\\001\027n\192\005\005\194\001\000\218\001\027\\\001\027\133@A@\160\176\192\005\005\197\001\000\218\001\027\\\001\027d\004\004@A\178\0047\160\004\016\160\004\015@\160\176\192\005\005\203\001\000\216\001\027\029\001\027.\192\005\005\204\001\000\216\001\027\029\001\027;@A@\165\160\160\176\001\006\174$join@\179@\160\176\001\006\175!l@\160\176\001\006\176!v@\160\176\001\006\177!d@\160\176\001\006\178!r@@\188\144\004\r\188\144\004\006\196A\176\001\006\181\"rh@\166\166D@\160\004\007@\196A\176\001\006\186\"lh@\166\166D@\160\004\015@\188\166\005\005\128\160\144\004\t\160\166L\160\144\004\019\160\145\144\144B@@\178\005\003\249\160\166\166@@\160\004 @\160\166\166A@\160\004$@\160\166\166B@\160\004(@\160\178\144\004;\160\166\166C@\160\004/@\160\144\004:\160\144\0049\160\144\0048@\160\176\192\005\006\023\001\000\228\001\028\188\001\028\231\192\005\006\024\001\000\228\001\028\188\001\028\246@A@\160\176\192\005\006\027\001\000\228\001\028\188\001\028\218\004\004@A\188\166\005\005\173\160\004)\160\166L\160\0040\160\145\144\144B@@\178\005\004$\160\178\004\031\160\144\004W\160\004\028\160\004\027\160\166\166@@\160\004O@@\160\176\192\005\0063\001\000\229\001\028\252\001\029\030\192\005\0064\001\000\229\001\028\252\001\029-@A\160\166\166A@\160\004W@\160\166\166B@\160\004[@\160\166\166C@\160\004_@@\160\176\192\005\006C\001\000\229\001\028\252\001\029\026\192\005\006D\001\000\229\001\028\252\001\0296@A\178\005\005\158\160\004\029\160\0048\160\0047\160\0046@\160\176\192\005\006L\001\000\230\001\029<\001\029F\192\005\006M\001\000\230\001\029<\001\029T@A\178\004\153\160\004@\160\004?\160\004(@\160\176\192\005\006T\001\000\226\001\028P\001\028f\192\005\006U\001\000\226\001\028P\001\028{@A\178\004\229\160\004H\160\004G\160\004F@\160\176\192\005\006\\\001\000\225\001\028$\001\028:\192\005\006]\001\000\225\001\028$\001\028O@A@\196B\176\001\006\191&concat@\179@\160\176\001\006\192\"t1@\160\176\001\006\193\"t2@@\188\144\004\007\188\144\004\006\196@\176\001\006\196\005\003\024@\178\005\003\156\160\144\004\011@\160\176\192\005\006s\001\000\241\001\030_\001\030v\192\005\006t\001\000\241\001\030_\001\030\132@A\178\004l\160\144\004\021\160\166\166@@\160\144\004\016@\160\166\166A@\160\004\005@\160\178\005\003_\160\004\020@\160\176\192\005\006\134\001\000\242\001\030\136\001\030\158\192\005\006\135\001\000\242\001\030\136\001\030\181@A@\160\176\192\005\006\138\001\000\242\001\030\136\001\030\146\004\004@A\144\004)\144\004'\196B\176\001\006\199.concat_or_join@\179@\160\176\001\006\200\"t1@\160\176\001\006\201!v@\160\176\001\006\202!d@\160\176\001\006\203\"t2@@\188\144\004\007\178\004\150\160\144\004\016\160\144\004\015\160\166\166@@\160\004\n@\160\144\004\015@\160\176\192\005\006\172\001\000\246\001\030\237\001\030\255\192\005\006\173\001\000\246\001\030\237\001\031\r@A\178\144\004Q\160\004\016\160\004\t@\160\176\192\005\006\180\001\000\247\001\031\014\001\031\030\192\005\006\181\001\000\247\001\031\014\001\031*@A\165\160\160\176\001\006\205%split@\179@\160\176\001\006\206!x@\160\176\001\006\207\005\006\227@@\188\144\004\003\196A\176\001\006\209!r@\166\166C@\160\004\007@\196A\176\001\006\210!d@\166\166B@\160\004\r@\196A\176\001\006\211!v@\166\166A@\160\004\019@\196A\176\001\006\212!l@\166\166@@\160\004\025@\196@\176\001\006\213!c@\178\166\166@\145'compare\160\005\005\n@\160\144\004)\160\144\004\024@\160\176\192\005\006\234\001\000\253\001\031\154\001\031\172\192\005\006\235\001\000\253\001\031\154\001\031\187@@\188\166\005\005\t\160\144\004\020\160\145\144\144@@\166\005\004\027\160\144\004!\160\166\181@\160$SomeA@\160\144\0044@\160\144\004<@\188\166\005\005\006\160\004\020\160\145\144\144@@\196@\176\001\006\214\005\003\179@\178\144\004R\160\004'\160\004\023@\160\176\192\005\007\015\001\001\000\001 \003\001 $\192\005\007\016\001\001\000\001 \003\001 -@A\166\005\0048\160\166\166@@\160\144\004\015@\160\166\166A@\160\004\005@\160\178\005\001\019\160\166\166B@\160\004\011@\160\004;\160\004&\160\004%@\160\176\192\005\007&\001\001\000\001 \003\001 <\192\005\007'\001\001\000\001 \003\001 I@A@\196@\176\001\006\218\005\003\212@\178\004!\160\004G\160\004.@\160\176\192\005\007/\001\001\002\001 Z\001 {\192\005\0070\001\001\002\001 Z\001 \132@A\166\005\004X\160\178\005\001*\160\004?\160\004O\160\004:\160\166\166@@\160\144\004\019@@\160\176\192\005\007>\001\001\002\001 Z\001 \137\192\005\007?\001\001\002\001 Z\001 \150@A\160\166\166A@\160\004\t@\160\166\166B@\160\004\r@@\145\178@@\160\161@\144\005\007\017\160\161@\144$None\160\161@\144\005\007\024@@\165\160\160\176\001\006\222%merge@\179@\160\176\001\006\223!f@\160\176\001\006\224\"s1@\160\176\001\006\225\"s2@@\186\188\144\004\b\196A\176\001\006\231\"v1@\166\166A@\160\004\007@\188\166\005\007T\160\166\166D@\160\004\r@\160\178\005\007y\160\144\004\021@\160\176\192\005\007x\001\001\007\001 \249\001!+\192\005\007y\001\001\007\001 \249\001!4@A@\196@\176\001\006\233\005\004&@\178\004s\160\144\004\024\160\004\011@\160\176\192\005\007\130\001\001\b\001!8\001!U\192\005\007\131\001\001\b\001!8\001!`@A\178\144\004\248\160\178\144\0042\160\144\0040\160\166\166@@\160\004*@\160\166\166@@\160\144\004\025@@\160\176\192\005\007\150\001\001\t\001!d\001!}\192\005\007\151\001\001\t\001!d\001!\140@A\160\004\027\160\178\004\017\160\004\030\160\166\181@\160\004\165A@\160\166\166B@\160\004?@@\160\166\166A@\160\004\021@@\160\176\192\005\007\170\001\001\t\001!d\001!\144\192\005\007\171\001\001\t\001!d\001!\163@@\160\178\004&\160\004%\160\166\166C@\160\004N@\160\166\166B@\160\004$@@\160\176\192\005\007\185\001\001\t\001!d\001!\164\192\005\007\186\001\001\t\001!d\001!\179@A@\160\176\192\005\007\189\001\001\t\001!d\001!n\004\004@A\169T@\188\144\004`\169T@\145\161@\144\005\007\137\160T@\188\004\007\196A\176\001\006\240\"v2@\166\166A@\160\004\r@\196@\176\001\006\242\005\004y@\178\004\198\160\144\004\n\160\144\004v@\160\176\192\005\007\214\001\001\011\001!\222\001!\251\192\005\007\215\001\001\011\001!\222\001\"\006@A\178\004T\160\178\004S\160\004R\160\166\166@@\160\144\004\019@\160\166\166@@\160\004%@@\160\176\192\005\007\231\001\001\012\001\"\n\001\"#\192\005\007\232\001\001\012\001\"\n\001\"2@A\160\004\025\160\178\004b\160\004\028\160\166\166A@\160\004\017@\160\166\181@\160\004\250A@\160\166\166B@\160\0049@@@\160\176\192\005\007\251\001\001\012\001\"\n\001\"6\192\005\007\252\001\001\012\001\"\n\001\"I@@\160\178\004w\160\004v\160\166\166B@\160\004$@\160\166\166C@\160\004H@@\160\176\192\005\b\n\001\001\012\001\"\n\001\"J\192\005\b\011\001\001\012\001\"\n\001\"Y@A@\160\176\192\005\b\014\001\001\012\001\"\n\001\"\020\004\004@A\166\156@\160\166\181@B@\160\166\147\176Z.Assert_failureC@\160\145\178@B\160\144\162\005\b\030@\160\144\144\001\001\014\160\144\144J@@@@\165\160\160\176\001\006\246&filter@\179@\160\176\001\006\247!p@\160\176\001\006\248\005\bR@@\188\144\004\003\196A\176\001\006\251!d@\166\166B@\160\004\007@\196A\176\001\006\252!v@\166\166A@\160\004\r@\196@\176\001\006\254\"l'@\178\144\004\027\160\144\004\025\160\166\166@@\160\004\024@@\160\176\192\005\bK\001\001\020\001#\018\001#%\192\005\bL\001\001\020\001#\018\001#/@A\196@\176\001\006\255#pvd@\178\004\r\160\144\004\026\160\144\004\"@\160\176\192\005\bW\001\001\021\001#3\001#G\192\005\bX\001\001\021\001#3\001#L@@\196@\176\001\007\000\"r'@\178\004\027\160\004\026\160\166\166C@\160\0041@@\160\176\192\005\bd\001\001\022\001#P\001#c\192\005\be\001\001\022\001#P\001#m@A\188\144\004\026\178\005\002_\160\144\004,\160\004\026\160\004\025\160\144\004\021@\160\176\192\005\bq\001\001\023\001#q\001#\135\192\005\br\001\001\023\001#q\001#\149@A\178\005\001\197\160\004\011\160\004\b@\160\176\192\005\bx\001\001\023\001#q\001#\155\192\005\by\001\001\023\001#q\001#\167@A\145\161@\144\005\bA@\165\160\160\176\001\007\001)partition@\179@\160\176\001\007\002!p@\160\176\001\007\003\005\b\170@@\188\144\004\003\196A\176\001\007\006!d@\166\166B@\160\004\007@\196A\176\001\007\007!v@\166\166A@\160\004\r@\196@\176\001\007\t\005\005B@\178\144\004\026\160\144\004\024\160\166\166@@\160\004\023@@\160\176\192\005\b\162\001\001\029\001$H\001$a\192\005\b\163\001\001\029\001$H\001$n@A\196A\176\001\007\n\"lf@\166\166A@\160\144\004\020@\196A\176\001\007\011\"lt@\166\166@@\160\004\007@\196@\176\001\007\012#pvd@\178\004\026\160\144\004&\160\144\004.@\160\176\192\005\b\187\001\001\030\001$r\001$\134\192\005\b\188\001\001\030\001$r\001$\139@@\196@\176\001\007\r\005\005i@\178\004'\160\004&\160\166\166C@\160\004<@@\160\176\192\005\b\199\001\001\031\001$\143\001$\168\192\005\b\200\001\001\031\001$\143\001$\181@A\196A\176\001\007\014\"rf@\166\166A@\160\144\004\018@\196A\176\001\007\015\"rt@\166\166@@\160\004\007@\188\144\004&\166\005\005\255\160\178\005\002\209\160\144\0041\160\004(\160\004'\160\144\004\016@\160\176\192\005\b\227\001\001!\001$\202\001$\218\192\005\b\228\001\001!\001$\202\001$\232@A\160\178\005\0028\160\144\004D\160\144\004!@\160\176\192\005\b\237\001\001!\001$\202\001$\234\192\005\b\238\001\001!\001$\202\001$\246@A@\166\005\006\022\160\178\005\002C\160\004\023\160\004\020@\160\176\192\005\b\246\001\001\"\001$\248\001%\b\192\005\b\247\001\001\"\001$\248\001%\020@A\160\178\005\002\240\160\004\019\160\004F\160\004E\160\004\020@\160\176\192\005\t\000\001\001\"\001$\248\001%\022\192\005\t\001\001\001\"\001$\248\001%$@A@\145\178@@\160\161@\144\005\b\203\160\161@\144\005\b\206@@\165\160\160\176\001\007\016)cons_enum@\179@\160\176\001\007\017!m@\160\176\001\007\018!e@@\188\144\004\007\178\144\004\r\160\166\166@@\160\004\007@\160\166\181@\160$MoreA@\160\166\166A@\160\004\016@\160\166\166B@\160\004\020@\160\166\166C@\160\004\024@\160\144\004\029@@\160\176\192\005\t3\001\001)\001%\179\001%\210\192\005\t4\001\001)\001%\179\001%\240@A\004\005@\196B\176\001\007\024'compare@\179@\160\176\001\007\025#cmp@\160\176\001\007\026\"m1@\160\176\001\007\027\"m2@@\165\160\160\176\001\007\028+compare_aux@\179@\160\176\001\007\029\"e1@\160\176\001\007\030\"e2@@\188\144\004\007\188\144\004\006\196@\176\001\007)!c@\178\166\166@\145'compare\160\005\007\129@\160\166\166@@\160\004\016@\160\166\166@@\160\004\018@@\160\176\192\005\te\001\0012\001&\212\001&\232\192\005\tf\001\0012\001&\212\001&\249@@\188\166\157A\160\144\004\025\160\145\144\144@@\004\005\196@\176\001\007*!c@\178\144\004;\160\166\166A@\160\004*@\160\166\166A@\160\004,@@\160\176\192\005\t\127\001\0014\001'\031\001'3\192\005\t\128\001\0014\001'\031\001'<@@\188\166\004\026\160\144\004\020\160\145\144\144@@\004\005\178\144\004F\160\178\004t\160\166\166B@\160\004B@\160\166\166C@\160\004F@@\160\176\192\005\t\151\001\0016\001'b\001'z\192\005\t\152\001\0016\001'b\001'\139@A\160\178\004\130\160\166\166B@\160\004N@\160\166\166C@\160\004R@@\160\176\192\005\t\165\001\0016\001'b\001'\140\192\005\t\166\001\0016\001'b\001'\157@A@\160\176\192\005\t\169\001\0016\001'b\001'n\004\004@A\145\144\144A\188\004]\145\144\144\000\255\145\144\144@@\178\004+\160\178\004\158\160\144\004|\160\145\161@\144#End@\160\176\192\005\t\192\001\0017\001'\158\001'\179\192\005\t\193\001\0017\001'\158\001'\197@A\160\178\004\171\160\144\004\134\160\145\161@\144\004\r@\160\176\192\005\t\204\001\0017\001'\158\001'\198\192\005\t\205\001\0017\001'\158\001'\216@A@\160\176\192\005\t\208\001\0017\001'\158\001'\167\004\004@A\196B\176\001\007+%equal@\179@\160\176\001\007,#cmp@\160\176\001\007-\"m1@\160\176\001\007.\"m2@@\165\160\160\176\001\007/)equal_aux@\179@\160\176\001\0070\"e1@\160\176\001\0071\"e2@@\188\144\004\007\188\144\004\006\166H\160\166\005\b\012\160\178\166\166@\145'compare\160\005\b\030@\160\166\166@@\160\004\017@\160\166\166@@\160\004\019@@\160\176\192\005\n\002\001\001@\001(\194\001(\206\192\005\n\003\001\001@\001(\194\001(\223@@\160\145\144\144@@\160\166H\160\178\144\0047\160\166\166A@\160\004&@\160\166\166A@\160\004(@@\160\176\192\005\n\023\001\001@\001(\194\001(\231\192\005\n\024\001\001@\001(\194\001(\240@@\160\178\144\004;\160\178\005\001\005\160\166\166B@\160\0047@\160\166\166C@\160\004;@@\160\176\192\005\n(\001\001A\001(\244\001)\n\192\005\n)\001\001A\001(\244\001)\027@A\160\178\005\001\019\160\166\166B@\160\004C@\160\166\166C@\160\004G@@\160\176\192\005\n6\001\001A\001(\244\001)\028\192\005\n7\001\001A\001(\244\001)-@A@\160\176\192\005\n:\001\001A\001(\244\001)\000\004\004@A@@\145\161@\144\005\b\154\188\004R\145\161@\144\005\b\158\145\161A\144\005\b\157@\178\004+\160\178\005\001/\160\144\004q\160\145\161@\144\004\145@\160\176\192\005\nP\001\001B\001).\001)A\192\005\nQ\001\001B\001).\001)S@A\160\178\005\001;\160\144\004z\160\145\161@\144\004\157@\160\176\192\005\n\\\001\001B\001).\001)T\192\005\n]\001\001B\001).\001)f@A@\160\176\192\005\n`\001\001B\001).\001)7\004\004@A\165\160\160\176\001\007<(cardinal@\179@\160\176\001\007=\005\n\139@@\188\144\004\003\166L\160\166L\160\178\144\004\r\160\166\166@@\160\004\011@@\160\176\192\005\nw\001\001F\001)\155\001)\186\192\005\nx\001\001F\001)\155\001)\196@A\160\145\144\144A@\160\178\004\015\160\166\166C@\160\004\025@@\160\176\192\005\n\133\001\001F\001)\155\001)\203\192\005\n\134\001\001F\001)\155\001)\213@A@\145\144\144@@\165\160\160\176\001\007C,bindings_aux@\179@\160\176\001\007D$accu@\160\176\001\007E\005\n\183@@\188\144\004\003\178\144\004\012\160\166\181@\160\"::A@\160\166\005\007\198\160\166\166A@\160\004\014@\160\166\166B@\160\004\018@@\160\178\004\018\160\144\004\027\160\166\166C@\160\004\026@@\160\176\192\005\n\178\001\001J\001*\022\001*M\192\005\n\179\001\001J\001*\022\001*`@A@\160\166\166@@\160\004\"@@\160\176\192\005\n\186\001\001J\001*\022\001*5\192\005\n\187\001\001J\001*\022\001*c@A\004\017@\196B\176\001\007K(bindings@\179@\160\176\001\007L!s@@\178\004,\160\145\161@\144\"[]\160\144\004\n@\160\176\192\005\n\205\001\001M\001*z\001*\128\192\005\n\206\001\001M\001*z\001*\145@A\166\181@B@\160\145\161@\144\005\n\153\160\144\005\t=\160\005\b/\160\005\b\211\160\005\005J\160\005\007\019\160\005\003T\160\144\005\001\168\160\144\005\001\014\160\005\006\233\160\005\006\026\160\005\005\213\160\005\005\157\160\005\002\163\160\005\002M\160\004w\160\144\004,\160\005\b\023\160\005\007\240\160\144\005\b*\160\005\003\228\160\005\b\140\160\005\006\192\160\005\006r@@A@@")));
            ("marshal.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002\160\000\000\000\169\000\000\002A\000\000\002&\176\208\208\208\208@)data_size\160\176@\160\160B\144\160\176\001\004\003$buff@\160\176\001\004\004#ofs@@@@@@A*from_bytes\160\176@\160\160B\144\160\176\001\004\t$buff@\160\176\001\004\n#ofs@@@@@@B,from_channel\160@\144\179@\160\176\001\004\015$prim@@\166\155\2400caml_input_valueAA @@\144\176\193 \176\179\177\144\176@*PervasivesA*in_channel\000\255@\144@\002\005\245\225\000\001\007U\176\150\176\144\144!a\002\005\245\225\000\001\007[\001\003\254\001\007X@\002\005\245\225\000\001\007Y\160\144\004\027@\208@+from_string\160\176@\160\160B\144\160\176\001\004\r$buff@\160\176\001\004\014#ofs@@@@@\208@+header_size\160@@@ABC)to_buffer\160\176@\160\160E\144\160\176\001\003\249$buff@\160\176\001\003\250#ofs@\160\176\001\003\251#len@\160\176\001\003\252!v@\160\176\001\003\253%flags@@@@@\208@*to_channel\160@\144\179@\160\176\001\004\018\004F@\160\176\001\004\017\004H@\160\176\001\004\016\004J@@\166\155\2401caml_output_valueCA\004I@@\144\176\193\004H\176\179\177\004G+out_channel\000\255@\144@\002\005\245\225\000\001\002\237\176\193\004O\176\150\176\144\144!a\002\005\245\225\000\001\005\174\001\003\244\001\005\158\176\193\004W\176\179\144\176I$list@\160\176\179\144\176\001\003\240,extern_flags@@\144@\002\005\245\225\000\001\005\159@\144@\002\005\245\225\000\001\005\163\176\179\144\176F$unit@@\144@\002\005\245\225\000\001\005\167@\002\005\245\225\000\001\005\170@\002\005\245\225\000\001\005\171@\002\005\245\225\000\001\005\172\160\144\0040\160\144\0040\160\144\0040@\208@*total_size\160\176A\160\160B\144\160\176\001\004\006$buff@\160\176\001\004\007#ofs@@@@@@ABD@@")));
            ("moreLabels.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000.\000\000\000\012\000\000\000+\000\000\000)\176\208@'Hashtbl\160@@\208@#Map\160@@\208@#Set\160@@@ABC\144'Hashtbl@")));
            ("nativeint.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002\025\000\000\000\163\000\000\002\017\000\000\001\255\176\208\208\208@#abs\160\176@\160\160A\144\160\176\001\004\n!n@@@@@\208\208@'compare\160\176@\160\160B\144\160\176\001\004\022!x@\160\176\001\004\023!y@@@@\144\179@\004\b\166\155\2406caml_nativeint_compareB@ @@@\160\144\004\014\160\144\004\r@@A&lognot\160\176A\160\160A\144\160\176\001\004\015!n@@@@\144\179@\004\005\166\b\000\000\004\"@\160\144\004\b\160\145\144\150\018_n\000\001\255\255\255\255@@BC'max_int\160\176A@@@\208\208@'min_int\160\004\005@@A)minus_one\160@@@BD#one\160@@\208\208@$pred\160\176A\160\160A\144\160\176\001\004\b!n@@@@\144\179@\004\005\166\b\000\000\004\028@\160\144\004\b\160\145\144\150\018_n\000\001\000\000\000\001@\208@$size\160\176A@@@@AB$succ\160\176A\160\160A\144\160\176\001\004\006!n@@@@\144\179@\004\005\166\b\000\000\004\027@\160\144\004\b\160\145\144\150\018_n\000\001\000\000\000\001@\208\208@)to_string\160\176@\160\160A\144\160\176\001\004\018!n@@@@\144\179@\004\005\166\155\2405caml_nativeint_formatBA @@\144\176\193 \176\179\144\176C&string@@\144@\002\005\245\225\000\001\004c\176\193\004\t\176\179\144\176K)nativeint@@\144@\002\005\245\225\000\001\004f\176\179\004\014@\144@\002\005\245\225\000\001\004i@\002\005\245\225\000\001\004l@\002\005\245\225\000\001\004m\160\145\144\162\"%d@\160\144\004%@@A$zero\160@@@BCE@@")));
            ("obj.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\004e\000\000\000\216\000\000\0039\000\000\002\240\176\208\208\208\208@,abstract_tag\160@@@A+closure_tag\160@@\208\208@*custom_tag\160@@@A0double_array_tag\160@@@BC,double_field\160\176A\160\160B\144\160\176\001\003\252!x@\160\176\001\003\253!i@@@@\144\179@\004\b\166\b\000\000\004\021C\160\144\004\011\160\144\004\n@\208\208@*double_tag\160@@\208@,extension_id\160\176A\160\160A\144\160\176\001\004%!x@@@@@@AB.extension_name\160\176A\160\160A\144\160\176\001\004\"!x@@@@@\208\208@.extension_slot\160\176@\160\160A\144\160\176\001\004(!x@@@@@@A)final_tag\160@@@BCD\t\"first_non_constant_constructor_tag\160@@\208\208\208\208@+forward_tag\160@@@A)infix_tag\160@@\208@'int_tag\160@@@AB\t!last_non_constant_constructor_tag\160@@@C(lazy_tag\160@@\208\208\208\208@'marshal\160\176@\160\160A\144\160\176\001\004\007#obj@@@@\144\179@\004\005\166\155\240;caml_output_value_to_stringBA @\160\160\160)ocaml.doc\176\192&_none_A@\000\255\004\002A\144\160\160\160\176\145\162\t\188 [Marshal.to_bytes v flags] returns a byte sequence containing\n   the representation of [v].\n   The [flags] argument has the same meaning as for\n   {!Marshal.to_channel}.\n   @since 4.02.0 @\176\192+marshal.mli\000r\001\024\164\001\024\164\192\004\002\000v\001\025R\001\025e@@@\004\004@@\144\176\193 \176\150\176\144\144!a\002\005\245\225\000\001\005\190\001\003\245\001\005\175\176\193\004\t\176\179\144\176I$list@\160\176\179\144\176\001\003\240,extern_flags@@\144@\002\005\245\225\000\001\005\176@\144@\002\005\245\225\000\001\005\180\176\179\144\176O%bytes@@\144@\002\005\245\225\000\001\005\184@\002\005\245\225\000\001\005\187@\002\005\245\225\000\001\005\188\160\144\004=\160\145\161@\144\"[]@@A+no_scan_tag\160@@@B*object_tag\160@@\208@/out_of_heap_tag\160@@@AC0set_double_field\160\176A\160\160C\144\160\176\001\003\255!x@\160\176\001\004\000!i@\160\176\001\004\001!v@@@@\144\179@\004\011\166\b\000\000\004\022C\160\144\004\014\160\144\004\r\160\144\004\012@\208@*string_tag\160@@\208@-unaligned_tag\160@@\208@)unmarshal\160\176A\160\160B\144\160\176\001\004\t#str@\160\176\001\004\n#pos@@@@@@ABCDEF@@")));
            ("oo.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000[\000\000\000\024\000\000\000V\000\000\000R\176\208@$copy\160\176@\160\160A\144\160\176\001\003\243!o@@@@@\208@*new_method\160\176@\160\160A\144\160\176\001\004\r!s@@@@@\208@3public_method_label\160\004\n@@ABC@@")));
            ("parsing.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002\162\000\000\000\180\000\000\002t\000\000\002S\176\208\208\208\208@+Parse_error\160\176@@@@\208@&YYexit\160\004\004@@AB,clear_parser\160\176A\160\160A\144\160\176\001\004g%param@@@@@\208@4is_current_lookahead\160\176@\160\160A\144\160\176\001\004Y#tok@@@@@\208@+parse_error\160\176A\160\160A\144\160\176\001\004[#msg@@@@\144\179@\004\005\145\161@\144\"()@ABC(peek_val\160\176A\160\160B\144\160\176\001\004F#env@\160\176\001\004G!n@@@@@\208@'rhs_end\160\176@\160\160A\144\160\176\001\004W!n@@@@@\208@+rhs_end_pos\160\176A\160\160A\144\160\176\001\004Q!n@@@@@@ABD)rhs_start\160\176@\160\160A\144\160\176\001\004U!n@@@@@\208\208@-rhs_start_pos\160\176A\160\160A\144\160\176\001\004O!n@@@@@\208@)set_trace\160@\144\179@\160\176\001\004\\$prim@@\166\155\2405caml_set_parser_traceAA @@\144\176\193 \176\179\144\176E$bool@@\144@\002\005\245\225\000\001\005\242\176\179\004\006@\144@\002\005\245\225\000\001\005\245@\002\005\245\225\000\001\005\248\160\144\004\022@@AB*symbol_end\160\176@\160\160A\144\160\176\001\004]\004s@@@@@\208\208@.symbol_end_pos\160\176A\160\160A\144\160\176\001\004_\004}@@@@@@A,symbol_start\160\176@\160\160A\144\160\176\001\004^\004\133@@@@@\208@0symbol_start_pos\160\176@\160\160A\144\160\176\001\004`\004\142@@@@@\208@'yyparse\160\176@\160\160D\144\160\176\001\0040&tables@\160\176\001\0041%start@\160\176\001\0042%lexer@\160\176\001\0043&lexbuf@@@@@@ABCDE@@")));
            ("pervasives.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\020!\000\000\005\b\000\000\017\129\000\000\016\201\176\208\208\208\208\208\208@!@\160\176@\160\160B\144\160\176\001\004\132\"l1@\160\176\001\004\133\"l2@@@@@@A$Exit\160\176@@@@\208\208@)LargeFile\160@@@A!^\160\176A\160\160B\144\160\176\001\004_\"s1@\160\176\001\004`\"s2@@@@@\208@\"^^\160\176A\160\160B\144\160\176\001\005]%param@\160\176\001\005^%param@@@@@@ABC#abs\160\176@\160\160A\144\160\176\001\004\026!x@@@@@\208\208\208@'at_exit\160\176A\160\160A\144\160\176\001\0056!f@@@@@@A.bool_of_string\160\176A\160\160A\144\160\176\001\005q\004\030@@@@@@B+char_of_int\160\176@\160\160A\144\160\176\001\004g!n@@@@@\208\208@(close_in\160@\144\179@\160\176\001\005E$prim@@\166\155\2405caml_ml_close_channelAA @@\144\176\193 \176\179\144\176\001\004\136*in_channel@@\144@\002\005\245\225\000\001\012\161\176\179\144\176F$unit@@\144@\002\005\245\225\000\001\012\164@\002\005\245\225\000\001\012\167\160\144\004\025@\208@.close_in_noerr\160\176@\160\160A\144\160\176\001\005\000\"ic@@@@@@AB)close_out\160\176@\160\160A\144\160\176\001\004\198\"oc@@@@\144\179@\004\005\173\166\155\240-caml_ml_flushAA\004-@@\144\176\193\004,\176\179\144\176\001\004\137+out_channel@@\144@\002\005\245\225\000\001\006\185\176\179\004+@\144@\002\005\245\225\000\001\006\188@\002\005\245\225\000\001\006\191\160\144\004\023@\166\155\2405caml_ml_close_channelAA\004?@@\144\176\193\004>\176\179\004\018@\144@\002\005\245\225\000\001\b\192\176\179\004:@\144@\002\005\245\225\000\001\b\195@\002\005\245\225\000\001\b\198\160\144\004&@\208@/close_out_noerr\160\176@\160\160A\144\160\176\001\004\200\"oc@@@@@\208@*do_at_exit\160\176@\160\160A\144\160\176\001\005[\004\145@@@@@@ABCDE-epsilon_float\160@@\208\208\208\208@$exit\160\176@\160\160A\144\160\176\001\005:'retcode@@@@@@A(failwith\160\176A\160\160A\144\160\176\001\003\238!s@@@A\144\179@\004\005\166\156@\160\166\181@B@\160\166\147\176S'FailureC@\160\144\004\016@@@B%flush\160@\144\179@\160\176\001\005Z\004\142@@\166\155\004`\160\144\004\005@\208@)flush_all\160\176@\160\160A\144\160\176\001\005k\004\202@@@@@\208@1in_channel_length\160@\144\179@\160\176\001\005F\004\162@@\166\155\2404caml_ml_channel_sizeAA\004\161@@\144\176\193\004\160\176\179\004\159@\144@\002\005\245\225\000\001\012\154\176\179\144\176A#int@@\144@\002\005\245\225\000\001\012\157@\002\005\245\225\000\001\012\160\160\144\004\019@@ABC(infinity\160@@\208\208@%input\160\176@\160\160D\144\160\176\001\004\213\"ic@\160\176\001\004\214!s@\160\176\001\004\215#ofs@\160\176\001\004\216#len@@@@@\208\208@0input_binary_int\160@\144\179@\160\176\001\005K\004\210@@\166\155\2401caml_ml_input_intAA\004\209@@\144\176\193\004\208\176\179\004\207@\144@\002\005\245\225\000\001\012z\176\179\0040@\144@\002\005\245\225\000\001\012}@\002\005\245\225\000\001\012\128\160\144\004\016@@A*input_byte\160@\144\179@\160\176\001\005L\004\231@@\166\155\2402caml_ml_input_charAA\004\230@@\144\176\193\004\229\176\179\004\228@\144@\002\005\245\225\000\001\012s\176\179\004E@\144@\002\005\245\225\000\001\012v@\002\005\245\225\000\001\012y\160\144\004\016@@BC*input_char\160@\144\179@\160\176\001\005M\004\252@@\166\155\2402caml_ml_input_charAA\004\251@@\144\176\193\004\250\176\179\004\249@\144@\002\005\245\225\000\001\tq\176\179\144\176B$char@@\144@\002\005\245\225\000\001\tt@\002\005\245\225\000\001\tw\160\144\004\019@\208@*input_line\160\176A\160\160A\144\160\176\001\004\234$chan@@@@@\208@+input_value\160@\144\179@\160\176\001\005J\005\001\031@@\166\155\2400caml_input_valueAA\005\001\030@@\144\176\193\005\001\029\176\179\005\001\028@\144@\002\005\245\225\000\001\012\129\176\150\176\144\144!a\002\005\245\225\000\001\012\135\001\004\250\001\012\132@\002\005\245\225\000\001\012\133\160\144\004\019@@ABDEF+invalid_arg\160\176A\160\160A\144\160\176\001\003\240!s@@@A\144\179@\004\005\166\156@\160\166\004\192\160\166\147\176R0Invalid_argumentC@\160\144\004\015@@\208\208\208@$lnot\160\176A\160\160A\144\160\176\001\004\031!x@@@@\144\179@\004\005\166S\160\144\004\007\160\145\144\144\000\255@@A#max\160\176@\160\160B\144\160\176\001\004\007!x@\160\176\001\004\b!y@@@@@\208\208@)max_float\160@@@A'max_int\160\176A@@@@BC#min\160\176@\160\160B\144\160\176\001\004\004!x@\160\176\001\004\005!y@@@@@\208\208@)min_float\160@@@A'min_int\160\004\019@\208@#nan\160@@@ABDG,neg_infinity\160@@\208\208\208\208\208\208@'open_in\160\176@\160\160A\144\160\176\001\004\207$name@@@@@\208@+open_in_bin\160\176@\160\160A\144\160\176\001\004\209$name@@@@@\208@+open_in_gen\160\176@\160\160C\144\160\176\001\004\203$mode@\160\176\001\004\204$perm@\160\176\001\004\205$name@@@@@@ABC(open_out\160\176@\160\160A\144\160\176\001\004\159$name@@@@@\208@,open_out_bin\160\176@\160\160A\144\160\176\001\004\161$name@@@@@@AD,open_out_gen\160\176@\160\160C\144\160\176\001\004\155$mode@\160\176\001\004\156$perm@\160\176\001\004\157$name@@@@@\208\208\208@2out_channel_length\160@\144\179@\160\176\001\005P\005\001\218@@\166\155\2404caml_ml_channel_sizeAA\005\001\217@@\144\176\193\005\001\216\176\179\005\001\172@\144@\002\005\245\225\000\001\b\185\176\179\005\0018@\144@\002\005\245\225\000\001\b\188@\002\005\245\225\000\001\b\191\160\144\004\016@@A&output\160\176@\160\160D\144\160\176\001\004\178\"oc@\160\176\001\004\179!s@\160\176\001\004\180#ofs@\160\176\001\004\181#len@@@@@\208@1output_binary_int\160@\144\179@\160\176\001\005U\005\002\002@\160\176\001\005T\005\002\004@@\166\155\2402caml_ml_output_intBA\005\002\003@@\144\176\193\005\002\002\176\179\005\001\214@\144@\002\005\245\225\000\001\bj\176\193\005\002\007\176\179\005\001d@\144@\002\005\245\225\000\001\bm\176\179\005\002\003@\144@\002\005\245\225\000\001\bp@\002\005\245\225\000\001\bs@\002\005\245\225\000\001\bt\160\144\004\023\160\144\004\023@@AB+output_byte\160@\144\179@\160\176\001\005W\005\002 @\160\176\001\005V\005\002\"@@\166\155\2403caml_ml_output_charBA\005\002!@@\144\176\193\005\002 \176\179\005\001\244@\144@\002\005\245\225\000\001\b_\176\193\005\002%\176\179\005\001\130@\144@\002\005\245\225\000\001\bb\176\179\005\002!@\144@\002\005\245\225\000\001\be@\002\005\245\225\000\001\bh@\002\005\245\225\000\001\bi\160\144\004\023\160\144\004\023@\208@,output_bytes\160\176@\160\160B\144\160\176\001\004\172\"oc@\160\176\001\004\173!s@@@@@@ACE+output_char\160@\144\179@\160\176\001\005Y\005\002K@\160\176\001\005X\005\002M@@\166\155\2403caml_ml_output_charBA\005\002L@@\144\176\193\005\002K\176\179\005\002\031@\144@\002\005\245\225\000\001\007,\176\193\005\002P\176\179\005\001S@\144@\002\005\245\225\000\001\007/\176\179\005\002L@\144@\002\005\245\225\000\001\0072@\002\005\245\225\000\001\0075@\002\005\245\225\000\001\0076\160\144\004\023\160\144\004\023@\208\208@-output_string\160\176@\160\160B\144\160\176\001\004\175\"oc@\160\176\001\004\176!s@@@@@@A0output_substring\160\176@\160\160D\144\160\176\001\004\183\"oc@\160\176\001\004\184!s@\160\176\001\004\185#ofs@\160\176\001\004\186#len@@@@@\208\208@,output_value\160\176@\160\160B\144\160\176\001\004\191$chan@\160\176\001\004\192!v@@@@\144\179@\004\b\166\155\2401caml_output_valueCA\005\002\146@@\144\176\193\005\002\145\176\179\005\002e@\144@\002\005\245\225\000\001\bu\176\193\005\002\150\176\150\176\144\144!a\002\005\245\225\000\001\b\136\001\004\189\001\bx\176\193\005\002\158\176\179\144\176I$list@\160\176\179\005\002\157@\144@\002\005\245\225\000\001\by@\144@\002\005\245\225\000\001\b}\176\179\005\002\161@\144@\002\005\245\225\000\001\b\129@\002\005\245\225\000\001\b\132@\002\005\245\225\000\001\b\133@\002\005\245\225\000\001\b\134\160\144\004*\160\144\004)\160\145\161@\144\"[]@\208@&pos_in\160@\144\179@\160\176\001\005G\005\002\196@@\166\155\240.caml_ml_pos_inAA\005\002\195@@\144\176\193\005\002\194\176\179\005\002\193@\144@\002\005\245\225\000\001\012\147\176\179\005\002\"@\144@\002\005\245\225\000\001\012\150@\002\005\245\225\000\001\012\153\160\144\004\016@@AB'pos_out\160@\144\179@\160\176\001\005Q\005\002\217@@\166\155\240/caml_ml_pos_outAA\005\002\216@@\144\176\193\005\002\215\176\179\005\002\171@\144@\002\005\245\225\000\001\b\178\176\179\005\0027@\144@\002\005\245\225\000\001\b\181@\002\005\245\225\000\001\b\184\160\144\004\016@\208@+prerr_bytes\160\176@\160\160A\144\160\176\001\005\020!s@@@@@@ACDF*prerr_char\160\176@\160\160A\144\160\176\001\005\016!c@@@@@\208\208\208@-prerr_endline\160\176@\160\160A\144\160\176\001\005\026!s@@@@@@A+prerr_float\160\176@\160\160A\144\160\176\001\005\024!f@@@@@@B)prerr_int\160\176@\160\160A\144\160\176\001\005\022!i@@@@@\208\208\208@-prerr_newline\160\176@\160\160A\144\160\176\001\005c\005\003S@@@@@@A,prerr_string\160\176@\160\160A\144\160\176\001\005\018!s@@@@@\208@+print_bytes\160\176@\160\160A\144\160\176\001\005\007!s@@@@@@AB*print_char\160\176@\160\160A\144\160\176\001\005\003!c@@@@@\208\208@-print_endline\160\176@\160\160A\144\160\176\001\005\r!s@@@@@@A+print_float\160\176@\160\160A\144\160\176\001\005\011!f@@@@@@BCDG)print_int\160\176@\160\160A\144\160\176\001\005\t!i@@@@@\208\208\208\208@-print_newline\160\176@\160\160A\144\160\176\001\005d\005\003\152@@@@@@A,print_string\160\176@\160\160A\144\160\176\001\005\005!s@@@@@\208\208@*read_float\160\176@\160\160A\144\160\176\001\005`\005\003\171@@@@@@A(read_int\160\176@\160\160A\144\160\176\001\005a\005\003\179@@@@@@BC)read_line\160\176A\160\160A\144\160\176\001\005b\005\003\187@@@@@\208\208@,really_input\160\176@\160\160D\144\160\176\001\004\224\"ic@\160\176\001\004\225!s@\160\176\001\004\226#ofs@\160\176\001\004\227#len@@@@@\208@3really_input_string\160\176A\160\160B\144\160\176\001\004\229\"ic@\160\176\001\004\230#len@@@@@\208@'seek_in\160@\144\179@\160\176\001\005I\005\003\180@\160\176\001\005H\005\003\182@@\166\155\240/caml_ml_seek_inBA\005\003\181@@\144\176\193\005\003\180\176\179\005\003\179@\144@\002\005\245\225\000\001\012\136\176\193\005\003\185\176\179\005\003\022@\144@\002\005\245\225\000\001\012\139\176\179\005\003\181@\144@\002\005\245\225\000\001\012\142@\002\005\245\225\000\001\012\145@\002\005\245\225\000\001\012\146\160\144\004\023\160\144\004\023@@ABC(seek_out\160@\144\179@\160\176\001\005S\005\003\210@\160\176\001\005R\005\003\212@@\166\155\2400caml_ml_seek_outBA\005\003\211@@\144\176\193\005\003\210\176\179\005\003\166@\144@\002\005\245\225\000\001\b\167\176\193\005\003\215\176\179\005\0034@\144@\002\005\245\225\000\001\b\170\176\179\005\003\211@\144@\002\005\245\225\000\001\b\173@\002\005\245\225\000\001\b\176@\002\005\245\225\000\001\b\177\160\144\004\023\160\144\004\023@\208\208\208@2set_binary_mode_in\160@\144\179@\160\176\001\005D\005\003\243@\160\176\001\005C\005\003\245@@\166\155\2407caml_ml_set_binary_modeBA\005\003\244@@\144\176\193\005\003\243\176\179\005\003\242@\144@\002\005\245\225\000\001\012\187\176\193\005\003\248\176\179\144\176E$bool@@\144@\002\005\245\225\000\001\012\190\176\179\005\003\247@\144@\002\005\245\225\000\001\012\193@\002\005\245\225\000\001\012\196@\002\005\245\225\000\001\012\197\160\144\004\026\160\144\004\026@@A3set_binary_mode_out\160@\144\179@\160\176\001\005O\005\004\020@\160\176\001\005N\005\004\022@@\166\155\2407caml_ml_set_binary_modeBA\005\004\021@@\144\176\193\005\004\020\176\179\005\003\232@\144@\002\005\245\225\000\001\b\253\176\193\005\004\025\176\179\004!@\144@\002\005\245\225\000\001\t\000\176\179\005\004\021@\144@\002\005\245\225\000\001\t\003@\002\005\245\225\000\001\t\006@\002\005\245\225\000\001\t\007\160\144\004\023\160\144\004\023@@B&stderr\160\005\004z@@CDE%stdin\160\005\004|@\208\208@&stdout\160\005\004\128@@A.string_of_bool\160\176A\160\160A\144\160\176\001\004u!b@@@@\144\179@\004\005\188\144\004\006\145\144\162$true@\145\144\162%false@\208\208@/string_of_float\160\176@\160\160A\144\160\176\001\004\129!f@@@@@\208@0string_of_format\160\176@\160\160A\144\160\176\001\005_\005\004\140@@@@\144\179@\004\004\166\166A@\160\144\004\007@@AB-string_of_int\160\176@\160\160A\144\160\176\001\004x!n@@@@\144\179@\004\005\166\155\240/caml_format_intBA\005\004m@@\144\176\193\005\004l\176\179\144\176C&string@@\144@\002\005\245\225\000\001\004\250\176\193\005\004t\176\179\005\003\209@\144@\002\005\245\225\000\001\004\253\176\179\004\011@\144@\002\005\245\225\000\001\005\000@\002\005\245\225\000\001\005\003@\002\005\245\225\000\001\005\004\160\145\144\162\"%d@\160\144\004 @\208\208@3unsafe_really_input\160\176@\160\160D\144\160\176\001\004\218\"ic@\160\176\001\004\219!s@\160\176\001\004\220#ofs@\160\176\001\004\221#len@@@@@@A1valid_float_lexem\160\176@\160\160A\144\160\176\001\004|!s@@@@@@BCDFHI@@")));
            ("printexc.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\005_\000\000\001-\000\000\004E\000\000\003\247\176\208\208\208\208\208@$Slot\160@@@A/backtrace_slots\160\176A\160\160A\144\160\176\001\004J-raw_backtrace@@@@@@B0backtrace_status\160@\144\179@\160\176\001\004{$prim@@\166\155\2405caml_backtrace_statusAA @@\144\176\193 \176\179\144\176F$unit@@\144@\002\005\245\225\000\001\022)\176\179\144\176E$bool@@\144@\002\005\245\225\000\001\022,@\002\005\245\225\000\001\022/\160\144\004\025@@C%catch\160\176@\160\160B\144\160\176\001\004\018#fct@\160\176\001\004\019#arg@@@@@\208\208@:convert_raw_backtrace_slot\160@\144\179@\160\176\001\004x\004,@@\166\155\240?caml_convert_raw_backtrace_slotAA\004+@@\144\176\193\004*\176\179\144\176\001\004\0212raw_backtrace_slot@@\144@\002\005\245\225\000\001\016A\176\179\144\176\001\004\024.backtrace_slot@@\144@\002\005\245\225\000\001\016D@\002\005\245\225\000\001\016G\160\144\004\022@\208@+exn_slot_id\160\176A\160\160A\144\160\176\001\004c!x@@@@@\208@-exn_slot_name\160\176A\160\160A\144\160\176\001\004f!x@@@@@@ABC-get_backtrace\160\176A\160\160A\144\160\176\001\004\133%param@@@@@\208\208@-get_callstack\160@\144\179@\160\176\001\004y\004f@@\166\155\240:caml_get_current_callstackAA\004e@@\144\176\193\004d\176\179\144\176A#int@@\144@\002\005\245\225\000\001\022M\176\179\144\176\001\004\022-raw_backtrace@@\144@\002\005\245\225\000\001\022P@\002\005\245\225\000\001\022S\160\144\004\022@@A1get_raw_backtrace\160@\144\179@\160\176\001\004z\004\129@@\166\155\240\t caml_get_exception_raw_backtraceAA\004\128@@\144\176\193\004\127\176\179\004~@\144@\002\005\245\225\000\001\015\246\176\179\004\024@\144@\002\005\245\225\000\001\015\249@\002\005\245\225\000\001\015\252\160\144\004\016@\208@6get_raw_backtrace_slot\160\176A\160\160B\144\160\176\001\004W$bckt@\160\176\001\004X!i@@@@\144\179@\004\b\166\b\000\000\004\021@\160\144\004\011\160\144\004\n@@ABDE%print\160\176@\160\160B\144\160\176\001\004\014#fct@\160\176\001\004\015#arg@@@@@\208\208\208@/print_backtrace\160\176@\160\160A\144\160\176\001\0042'outchan@@@@@@A3print_raw_backtrace\160\176@\160\160B\144\160\176\001\004/'outchan@\160\176\001\0040-raw_backtrace@@@@@\208\208@4raw_backtrace_length\160\176A\160\160A\144\160\176\001\004U$bckt@@@@\144\179@\004\005\166\b\000\000\004\018@\160\144\004\b@@A7raw_backtrace_to_string\160\176A\160\160A\144\160\176\001\004:-raw_backtrace@@@@@@BC0record_backtrace\160@\144\179@\160\176\001\004|\004\233@@\166\155\2405caml_record_backtraceAA\004\232@@\144\176\193\004\231\176\179\004\224@\144@\002\005\245\225\000\001\022\"\176\179\004\233@\144@\002\005\245\225\000\001\022%@\002\005\245\225\000\001\022(\160\144\004\016@\208\208@0register_printer\160\176A\160\160A\144\160\176\001\004]\"fn@@@@@\208@>set_uncaught_exception_handler\160\176A\160\160A\144\160\176\001\004j\"fn@@@@@@AB)to_string\160\176@\160\160A\144\160\176\001\003\253!x@@@@@@CDF@@")));
            ("printf.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\001\167\000\000\000\131\000\000\001\180\000\000\001\168\176\208\208\208@'bprintf\160\176@\160\160B\144\160\176\001\004\005!b@\160\176\001\004\006#fmt@@@@@@A'eprintf\160\176@\160\160A\144\160\176\001\004\r#fmt@@@@@@B'fprintf\160\176@\160\160B\144\160\176\001\004\002\"oc@\160\176\001\004\003#fmt@@@@@\208\208\208@(ifprintf\160\176@\160\160B\144\160\176\001\004\b\"oc@\160\176\001\004\t#fmt@@@@@@A)ikfprintf\160\176@\160\160C\144\160\176\001\003\253!k@\160\176\001\003\254\"oc@\160\176\001\004\030%param@@@@@\208@(kbprintf\160\176@\160\160C\144\160\176\001\003\247!k@\160\176\001\003\248!b@\160\176\001\004!\004\016@@@@@@AB(kfprintf\160\176@\160\160C\144\160\176\001\003\241!k@\160\176\001\003\242!o@\160\176\001\004#\004\030@@@@@\208\208\208@'kprintf\160\176@\160\160B\144\160\176\001\004\015!k@\160\176\001\004\024\004,@@@@@@A(ksprintf\160\004\011@@B&printf\160\176@\160\160A\144\160\176\001\004\011#fmt@@@@@\208@'sprintf\160\176@\160\160A\144\160\176\001\004\021#fmt@@@@@@ACDE@@")));
            ("queue.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\001\233\000\000\000\177\000\000\0026\000\000\002%\176\208\208\208@%Empty\160\176@@@@@A#add\160\176A\160\160B\144\160\176\001\003\251!x@\160\176\001\003\252!q@@@@@\208@%clear\160\176A\160\160A\144\160\176\001\003\249!q@@@@@\208@$copy\160\176A\160\160A\144\160\176\001\004\011!q@@@@@@ABC&create\160\176A\160\160A\144\160\176\001\0042%param@@@@\144\179@\004\005\166\181@\146\160&length$tailA\160\145\144\144@\160\145\161@\144$None@\208\208\208\208@$fold\160\176@\160\160C\144\160\176\001\004\029!f@\160\176\001\004\030$accu@\160\176\001\004\031!q@@@@@@A(is_empty\160\176A\160\160A\144\160\176\001\004\019!q@@@@\144\179@\004\005\166\157@\160\166\166@\144\004/\160\144\004\012@\160\145\144\144@@\208\208@$iter\160\176@\160\160B\144\160\176\001\004\023!f@\160\176\001\004\024!q@@@@@@A&length\160\176@\160\160A\144\160\176\001\004\021!q@@@@\144\179@\004\005\166\166@\144\004Q\160\144\004\t@@BC$peek\160\176@\160\160A\144\160\176\001\004\003!q@@@@@\208@#pop\160\176@\160\160A\144\160\176\001\004\006!q@@@@@@AD$push\160\004\149@\208@$take\160\004\012@\208@#top\160\004\025@\208@(transfer\160\176A\160\160B\144\160\176\001\004&\"q1@\160\176\001\004'\"q2@@@@@@ABCEF@@")));
            ("random.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\001\141\000\000\000y\000\000\001\162\000\000\001\142\176\208\208\208\208@%State\160@@@A$bits\160\176@\160\160A\144\160\176\001\004X%param@@@@@\208@$bool\160\176A\160\160A\144\160\176\001\004W\004\n@@@@@@AB%float\160\176A\160\160A\144\160\176\001\004K%scale@@@@@\208@)full_init\160\176A\160\160A\144\160\176\001\004N$seed@@@@@\208@)get_state\160\176@\160\160A\144\160\176\001\004U\004&@@@@@@ABC$init\160\176A\160\160A\144\160\176\001\004P$seed@@@@@\208\208@#int\160\176@\160\160A\144\160\176\001\004C%bound@@@@@@A%int32\160\176@\160\160A\144\160\176\001\004E%bound@@@@@\208\208\208@%int64\160\176@\160\160A\144\160\176\001\004I%bound@@@@@@A)nativeint\160\176@\160\160A\144\160\176\001\004G%bound@@@@@@B)self_init\160\176A\160\160A\144\160\176\001\004V\004`@@@@@\208@)set_state\160\176A\160\160A\144\160\176\001\004T!s@@@@@@ACDE@@")));
            ("scanf.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002 \000\000\000\162\000\000\002\028\000\000\002\007\176\208\208\208@,Scan_failure\160\176@@@@@A(Scanning\160@@\208@&bscanf\160\176@\160\160B\144\160\176\001\018U\"ib@\160\176\001\018V#fmt@@@@@\208@-bscanf_format\160\176@\160\160C\144\160\176\001\018`\"ib@\160\176\001\018a&format@\160\176\001\018b!f@@@@@\208@2format_from_string\160\176@\160\160B\144\160\176\001\018q!s@\160\176\001\018r#fmt@@@@@@ABCD&fscanf\160\176@\160\160B\144\160\176\001\018X\"ic@\160\176\001\018Y#fmt@@@@@\208\208\208@'kfscanf\160\176@\160\160C\144\160\176\001\018Q\"ic@\160\176\001\018R\"ef@\160\176\001\018S#fmt@@@@@@A&kscanf\160\176@\160\160C\144\160\176\001\0187\"ib@\160\176\001\0188\"ef@\160\176\001\018z%param@@@@@\208@'ksscanf\160\176@\160\160C\144\160\176\001\018M!s@\160\176\001\018N\"ef@\160\176\001\018O#fmt@@@@@@AB%scanf\160\176@\160\160A\144\160\176\001\018^#fmt@@@@@\208@&sscanf\160\176@\160\160B\144\160\176\001\018[!s@\160\176\001\018\\#fmt@@@@@\208@-sscanf_format\160\176@\160\160C\144\160\176\001\018g!s@\160\176\001\018h&format@\160\176\001\018i!f@@@@@\208@)unescaped\160\176@\160\160A\144\160\176\001\018u!s@@@@@@ABCDE\144%stdin@")));
            ("set.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000+H\000\000\012\140\000\000(\189\000\000(s\176\208@$Make\160\176A\160\160A\144\160\176\001\004\016#Ord@@@@\144\179@\160\176\001\005[&funarg@@\196B\176\001\005\217&height@\179@\160\176\001\005\218%param@@\188\144\004\004\166\166C@\160\004\004@\145\144\144@\196B\176\001\005\223&create@\179@\160\176\001\005\224!l@\160\176\001\005\225!v@\160\176\001\005\226!r@@\196B\176\001\005\227\"hl@\188\144\004\r\166\166C@\160\004\004@\145\144\144@\196B\176\001\005\232\"hr@\188\144\004\018\166\166C@\160\004\004@\145\144\144@\166\181@\160$NodeA@\160\004\023\160\144\004\"\160\004\015\160\188\166\157E\160\144\004#\160\144\004\026@\166L\160\004\005\160\145\144\144A@\166L\160\004\t\160\145\144\144A@@\196B\176\001\005\237#bal@\179@\160\176\001\005\238!l@\160\176\001\005\239!v@\160\176\001\005\240!r@@\196B\176\001\005\241\"hl@\188\144\004\r\166\166C@\160\004\004@\145\144\144@\196B\176\001\005\246\"hr@\188\144\004\018\166\166C@\160\004\004@\145\144\144@\188\166\157C\160\144\004\026\160\166L\160\144\004\019\160\145\144\144B@@\188\004 \196A\176\001\005\252\"lr@\166\166B@\160\004&@\196A\176\001\005\253\"lv@\166\166A@\160\004,@\196A\176\001\005\254\"ll@\166\166@@\160\0042@\188\166\004V\160\178\144\004\149\160\144\004\012@\160\176\192&set.ml\000X\001\012o\001\012~\192\004\002\000X\001\012o\001\012\135@A\160\178\004\n\160\144\004!@\160\176\192\004\t\000X\001\012o\001\012\139\192\004\n\000X\001\012o\001\012\148@A@\178\144\004\151\160\004\017\160\144\004$\160\178\004\006\160\004\r\160\144\004Y\160\004F@\160\176\192\004\024\000Y\001\012\154\001\012\181\192\004\025\000Y\001\012\154\001\012\196@A@\160\176\192\004\028\000Y\001\012\154\001\012\168\004\004@A\188\004\024\178\004\019\160\178\004\021\160\004%\160\004\020\160\166\166@@\160\004!@@\160\176\192\004)\000^\001\rP\001\ri\192\004*\000^\001\rP\001\r{@A\160\166\166A@\160\004)@\160\178\004%\160\166\166B@\160\004/@\160\004\"\160\004g@\160\176\192\0049\000^\001\rP\001\r\128\192\004:\000^\001\rP\001\r\144@A@\160\176\192\004=\000^\001\rP\001\rb\004\004@A\166\156@\160\166\181@B@\160\166\147\176R0Invalid_argumentC@\160\145\144\162'Set.bal@@@\166\004\015\160\166\004\014\160\166\004\r@\160\145\144\162'Set.bal@@@\188\166\004\128\160\004{\160\166L\160\004\130\160\145\144\144B@@\188\004\146\196A\176\001\006\004\"rr@\166\166B@\160\004\152@\196A\176\001\006\005\"rv@\166\166A@\160\004\158@\196A\176\001\006\006\"rl@\166\166@@\160\004\164@\188\166\004\211\160\178\004}\160\144\004\023@\160\176\192\004|\000d\001\014)\001\0148\192\004}\000d\001\014)\001\014A@A\160\178\004\133\160\144\004\019@\160\176\192\004\132\000d\001\014)\001\014E\192\004\133\000d\001\014)\001\014N@A@\178\004{\160\178\004}\160\004\197\160\004w\160\004\011@\160\176\192\004\142\000e\001\014T\001\014i\192\004\143\000e\001\014T\001\014x@A\160\144\004)\160\004\026@\160\176\192\004\149\000e\001\014T\001\014b\192\004\150\000e\001\014T\001\014~@A\188\004\023\178\004\141\160\178\004\143\160\004\215\160\004\137\160\166\166@@\160\004 @@\160\176\192\004\163\000j\001\015\011\001\015$\192\004\164\000j\001\015\011\001\0154@A\160\166\166A@\160\004(@\160\178\004\159\160\166\166B@\160\004.@\160\004\031\160\0048@\160\176\192\004\179\000j\001\015\011\001\0159\192\004\180\000j\001\015\011\001\015K@A@\160\176\192\004\183\000j\001\015\011\001\015\029\004\004@A\166\004z\160\166\004y\160\166\004x@\160\145\144\162'Set.bal@@@\166\004\132\160\166\004\131\160\166\004\130@\160\145\144\162'Set.bal@@@\166\181@\160\005\0014A@\160\005\001\011\160\004\189\160\005\001\002\160\188\166\005\0012\160\004\251\160\004\248@\166L\160\004\254\160\145\144\144A@\166L\160\005\001\000\160\145\144\144A@@\165\160\160\176\001\006\011#add@\179@\160\176\001\006\012!x@\160\176\001\006\r!t@@\188\144\004\004\196A\176\001\006\015!r@\166\166B@\160\004\007@\196A\176\001\006\016!v@\166\166A@\160\004\r@\196A\176\001\006\017!l@\166\166@@\160\004\019@\196@\176\001\006\018!c@\178\166\166@\145'compare\160\144\005\001\169@\160\144\004%\160\144\004\025@\160\176\192\005\001\019\000t\001\0165\001\016G\192\005\001\020\000t\001\0165\001\016V@@\188\166\157@\160\144\004\022\160\145\144\144@@\004.\188\166\157B\160\004\t\160\145\144\144@@\178\144\005\001s\160\178\144\004E\160\004\030\160\144\0040@\160\176\192\005\0010\000v\001\016y\001\016\149\192\005\0011\000v\001\016y\001\016\158@A\160\004#\160\144\004C@\160\176\192\005\0017\000v\001\016y\001\016\145\192\005\0018\000v\001\016y\001\016\162@A\178\004\019\160\004\014\160\004,\160\178\004\020\160\0041\160\004\012@\160\176\192\005\001B\000v\001\016y\001\016\176\192\005\001C\000v\001\016y\001\016\185@A@\160\176\192\005\001F\000v\001\016y\001\016\168\004\004@A\166\181@\160\005\001\175A@\160\145\161@\144%Empty\160\004B\160\145\161@\144\004\006\160\145\144\144A@@\196B\176\001\006\019)singleton@\179@\160\176\001\006\020!x@@\166\181@\160\005\001\199A@\160\145\161@\144\004\024\160\144\004\011\160\145\161@\144\004\030\160\145\144\144A@\165\160\160\176\001\006\021/add_min_element@\179@\160\176\001\006\022!v@\160\176\001\006\023\005\002\015@@\188\144\004\003\178\004W\160\178\144\004\014\160\144\004\012\160\166\166@@\160\004\011@@\160\176\192\005\001\137\001\000\132\001\018\152\001\018\164\192\005\001\138\001\000\132\001\018\152\001\018\185@A\160\166\166A@\160\004\019@\160\166\166B@\160\004\023@@\160\176\192\005\001\149\001\000\132\001\018\152\001\018\160\192\005\001\150\001\000\132\001\018\152\001\018\189@A\178\144\004@\160\004\024@\160\176\192\005\001\156\001\000\130\001\018^\001\018o\192\005\001\157\001\000\130\001\018^\001\018z@A@\165\160\160\176\001\006\028/add_max_element@\179@\160\176\001\006\029!v@\160\176\001\006\030\005\002=@@\188\144\004\003\178\004\133\160\166\166@@\160\004\006@\160\166\166A@\160\004\n@\160\178\144\004\022\160\144\004\020\160\166\166B@\160\004\019@@\160\176\192\005\001\191\001\000\137\001\019\"\001\0192\192\005\001\192\001\000\137\001\019\"\001\019G@A@\160\176\192\005\001\195\001\000\137\001\019\"\001\019*\004\004@A\178\004-\160\004\014@\160\176\192\005\001\200\001\000\135\001\018\232\001\018\249\192\005\001\201\001\000\135\001\018\232\001\019\004@A@\165\160\160\176\001\006#$join@\179@\160\176\001\006$!l@\160\176\001\006%!v@\160\176\001\006&!r@@\188\144\004\n\188\144\004\006\196A\176\001\006)\"rh@\166\166C@\160\004\007@\196A\176\001\006-\"lh@\166\166C@\160\004\015@\188\166\005\002\018\160\144\004\t\160\166L\160\144\004\019\160\145\144\144B@@\178\004\207\160\166\166@@\160\004 @\160\166\166A@\160\004$@\160\178\144\0044\160\166\166B@\160\004+@\160\144\0043\160\144\0042@\160\176\192\005\002\011\001\000\147\001\020p\001\020\152\192\005\002\012\001\000\147\001\020p\001\020\165@A@\160\176\192\005\002\015\001\000\147\001\020p\001\020\142\004\004@A\188\166\005\0029\160\004#\160\166L\160\004*\160\145\144\144B@@\178\004\244\160\178\004\029\160\144\004N\160\004\026\160\166\166@@\160\004H@@\160\176\192\005\002&\001\000\148\001\020\171\001\020\205\192\005\002'\001\000\148\001\020\171\001\020\218@A\160\166\166A@\160\004P@\160\166\166B@\160\004T@@\160\176\192\005\0022\001\000\148\001\020\171\001\020\201\192\005\0023\001\000\148\001\020\171\001\020\224@A\178\005\002)\160\004\024\160\0041\160\0040@\160\176\192\005\002:\001\000\149\001\020\230\001\020\240\192\005\002;\001\000\149\001\020\230\001\020\252@A\178\004\135\160\0048\160\004!@\160\176\192\005\002A\001\000\145\001\020\014\001\020$\192\005\002B\001\000\145\001\020\014\001\0207@A\178\004\196\160\004?\160\004>@\160\176\192\005\002H\001\000\144\001\019\228\001\019\250\192\005\002I\001\000\144\001\019\228\001\020\r@A@\165\160\160\176\001\0061'min_elt@\179@\160\176\001\0062\005\002\230@@\188\144\004\003\196A\176\001\0063!l@\166\166@@\160\004\007@\188\144\004\007\178\144\004\017\160\004\004@\160\176\192\005\002a\001\000\156\001\021\146\001\021\174\192\005\002b\001\000\156\001\021\146\001\021\183@A\166\166A@\160\004\019@\166\156@\160\166\147\176T)Not_foundC@@@\165\160\160\176\001\0069'max_elt@\179@\160\176\001\006:\005\003\t@@\188\144\004\003\196A\176\001\006;!r@\166\166B@\160\004\007@\188\144\004\007\178\144\004\017\160\004\004@\160\176\192\005\002\132\001\000\161\001\022\027\001\0227\192\005\002\133\001\000\161\001\022\027\001\022@@A\166\166A@\160\004\019@\166\156@\160\166\147\004#@@@\165\160\160\176\001\006B.remove_min_elt@\179@\160\176\001\006C\005\003*@@\188\144\004\003\196A\176\001\006D!l@\166\166@@\160\004\007@\188\144\004\007\178\005\001z\160\178\144\004\019\160\004\006@\160\176\192\005\002\167\001\000\168\001\022\244\001\023\020\192\005\002\168\001\000\168\001\022\244\001\023&@A\160\166\166A@\160\004\022@\160\166\166B@\160\004\026@@\160\176\192\005\002\179\001\000\168\001\022\244\001\023\016\192\005\002\180\001\000\168\001\022\244\001\023*@A\166\004\007\160\004 @\166\005\002y\160\166\005\002x\160\166\005\002w@\160\145\144\1622Set.remove_min_elt@@@@\196B\176\001\006K%merge@\179@\160\176\001\006L\"t1@\160\176\001\006M\"t2@@\188\144\004\007\188\144\004\006\178\005\001\169\160\144\004\012\160\178\004w\160\144\004\r@\160\176\192\005\002\216\001\000\178\001\024\030\001\0247\192\005\002\217\001\000\178\001\024\030\001\024C@A\160\178\0049\160\004\b@\160\176\192\005\002\223\001\000\178\001\024\030\001\024D\192\005\002\224\001\000\178\001\024\030\001\024W@A@\160\176\192\005\002\227\001\000\178\001\024\030\001\0240\004\004@A\144\004\031\144\004\029\196B\176\001\006P&concat@\179@\160\176\001\006Q\"t1@\160\176\001\006R\"t2@@\188\144\004\007\188\144\004\006\178\004\245\160\144\004\012\160\178\004\156\160\144\004\r@\160\176\192\005\002\253\001\000\188\001\025P\001\025j\192\005\002\254\001\000\188\001\025P\001\025v@A\160\178\004^\160\004\b@\160\176\192\005\003\004\001\000\188\001\025P\001\025w\192\005\003\005\001\000\188\001\025P\001\025\138@A@\160\176\192\005\003\b\001\000\188\001\025P\001\025b\004\004@A\144\004\031\144\004\029\165\160\160\176\001\006U%split@\179@\160\176\001\006V!x@\160\176\001\006W\005\003\170@@\188\144\004\003\196A\176\001\006Y!r@\166\166B@\160\004\007@\196A\176\001\006Z!v@\166\166A@\160\004\r@\196A\176\001\006[!l@\166\166@@\160\004\019@\196@\176\001\006\\!c@\178\166\166@\145'compare\160\005\002'@\160\144\004#\160\144\004\024@\160\176\192\005\0039\001\000\200\001\027!\001\0273\192\005\003:\001\000\200\001\027!\001\027B@@\188\166\005\002&\160\144\004\020\160\145\144\144@@\166\181@@@\160\144\004\"\160\145\161A\144$true\160\144\0045@\188\166\005\0020\160\004\019\160\145\144\144@@\196@\176\001\006]%match@\178\144\004L\160\004'\160\004\022@\160\176\192\005\003^\001\000\203\001\027\136\001\027\169\192\005\003_\001\000\203\001\027\136\001\027\178@A\166\004\029\160\166\166@@\160\144\004\016@\160\166\166A@\160\004\005@\160\178\005\001l\160\166\166B@\160\004\011@\160\004;\160\004%@\160\176\192\005\003t\001\000\203\001\027\136\001\027\193\192\005\003u\001\000\203\001\027\136\001\027\204@A@\196@\176\001\006a\004!@\178\004 \160\004F\160\004.@\160\176\192\005\003}\001\000\205\001\027\221\001\027\254\192\005\003~\001\000\205\001\027\221\001\028\007@A\166\004<\160\178\005\001\130\160\004=\160\004N\160\166\166@@\160\144\004\018@@\160\176\192\005\003\139\001\000\205\001\027\221\001\028\012\192\005\003\140\001\000\205\001\027\221\001\028\023@A\160\166\166A@\160\004\t@\160\166\166B@\160\004\r@@\145\178@@\160\161@\144\005\002L\160\161@\144%false\160\161@\144\005\002S@@\196B\176\001\006f(is_empty@\179@\160\176\001\006g\005\004;@@\188\144\004\003\145\161@\144\004\015\145\161A\144\004d\165\160\160\176\001\006h#mem@\179@\160\176\001\006i!x@\160\176\001\006j\005\004N@@\188\144\004\003\196@\176\001\006o!c@\178\166\166@\145'compare\160\005\002\185@\160\144\004\017\160\166\166A@\160\004\016@@\160\176\192\005\003\205\001\000\216\001\028\243\001\029\005\192\005\003\206\001\000\216\001\028\243\001\029\020@@\166I\160\166\005\002\187\160\144\004\023\160\145\144\144@@\160\178\144\004)\160\004\022\160\188\166\005\002\191\160\004\r\160\145\144\144@@\166\166@@\160\004,@\166\166B@\160\004/@@\160\176\192\005\003\236\001\000\217\001\029\024\001\029+\192\005\003\237\001\000\217\001\029\024\001\029I@A@\145\161@\144\004T@\165\160\160\176\001\006p&remove@\179@\160\176\001\006q!x@\160\176\001\006r\005\004\144@@\188\144\004\003\196A\176\001\006t!r@\166\166B@\160\004\007@\196A\176\001\006u!v@\166\166A@\160\004\r@\196A\176\001\006v!l@\166\166@@\160\004\019@\196@\176\001\006w!c@\178\166\166@\145'compare\160\005\003\r@\160\144\004#\160\144\004\024@\160\176\192\005\004\031\001\000\222\001\029\158\001\029\176\192\005\004 \001\000\222\001\029\158\001\029\191@@\188\166\005\003\012\160\144\004\020\160\145\144\144@@\178\144\005\001i\160\144\004\"\160\144\0040@\160\176\192\005\0041\001\000\223\001\029\195\001\029\219\192\005\0042\001\000\223\001\029\195\001\029\228@A\188\166\005\003\021\160\004\018\160\145\144\144@@\178\005\003\020\160\178\144\004J\160\004%\160\004\020@\160\176\192\005\004B\001\000\224\001\029\234\001\030\006\192\005\004C\001\000\224\001\029\234\001\030\018@A\160\004)\160\004\024@\160\176\192\005\004H\001\000\224\001\029\234\001\030\002\192\005\004I\001\000\224\001\029\234\001\030\022@A\178\005\003$\160\004 \160\0041\160\178\004\018\160\0046\160\004#@\160\176\192\005\004S\001\000\224\001\029\234\001\030$\192\005\004T\001\000\224\001\029\234\001\0300@A@\160\176\192\005\004W\001\000\224\001\029\234\001\030\028\004\004@A\145\161@\144\005\003\r@\165\160\160\176\001\006x%union@\179@\160\176\001\006y\"s1@\160\176\001\006z\"s2@@\188\144\004\007\188\144\004\006\196A\176\001\006}\"h2@\166\166C@\160\004\007@\196A\176\001\006\127\"v2@\166\166A@\160\004\r@\196A\176\001\006\129\"h1@\166\166C@\160\004\021@\196A\176\001\006\131\"v1@\166\166A@\160\004\027@\188\166\005\004\226\160\144\004\015\160\144\004\029@\188\166\005\003t\160\004\004\160\145\144\144A@\178\005\003g\160\144\004!\160\144\0043@\160\176\192\005\004\151\001\000\232\001\030\237\001\031\b\192\005\004\152\001\000\232\001\030\237\001\031\017@A\196@\176\001\006\133\005\001D@\178\005\001C\160\144\004 \160\144\004;@\160\176\192\005\004\162\001\000\233\001\031\029\001\031=\192\005\004\163\001\000\233\001\031\029\001\031H@A\178\005\002\165\160\178\144\004J\160\166\166@@\160\004D@\160\166\166@@\160\144\004\023@@\160\176\192\005\004\179\001\000\234\001\031L\001\031_\192\005\004\180\001\000\234\001\031L\001\031l@A\160\004\025\160\178\004\017\160\166\166B@\160\004T@\160\166\166B@\160\004\016@@\160\176\192\005\004\194\001\000\234\001\031L\001\031p\192\005\004\195\001\000\234\001\031L\001\031}@A@\160\176\192\005\004\198\001\000\234\001\031L\001\031Z\004\004@A\188\166\005\003\178\160\004D\160\145\144\144A@\178\005\003\165\160\0043\160\0042@\160\176\192\005\004\211\001\000\237\001\031\157\001\031\184\192\005\004\212\001\000\237\001\031\157\001\031\193@A\196@\176\001\006\137\005\001\128@\178\005\001\127\160\004G\160\004F@\160\176\192\005\004\220\001\000\238\001\031\205\001\031\237\192\005\004\221\001\000\238\001\031\205\001\031\248@A\178\005\002\223\160\178\004:\160\166\166@@\160\144\004\016@\160\166\166@@\160\004\128@@\160\176\192\005\004\236\001\000\239\001\031\252\001 \015\192\005\004\237\001\000\239\001\031\252\001 \028@A\160\004]\160\178\004J\160\166\166B@\160\004\016@\160\166\166B@\160\004\143@@\160\176\192\005\004\251\001\000\239\001\031\252\001  \192\005\004\252\001\000\239\001\031\252\001 -@A@\160\176\192\005\004\255\001\000\239\001\031\252\001 \n\004\004@A\144\004\159\144\004\157@\165\160\160\176\001\006\141%inter@\179@\160\176\001\006\142\"s1@\160\176\001\006\143\"s2@@\188\144\004\007\188\144\004\006\196A\176\001\006\150\"r1@\166\166B@\160\004\t@\196A\176\001\006\151\"v1@\166\166A@\160\004\015@\196A\176\001\006\152\"l1@\166\166@@\160\004\021@\196@\176\001\006\153\005\001\207@\178\005\001\206\160\144\004\016\160\144\004\031@\160\176\192\005\005-\001\000\247\001 \210\001 \226\192\005\005.\001\000\247\001 \210\001 \237@A\196A\176\001\006\155\"l2@\166\166@@\160\144\004\017@\188\166\157A\160\166\166A@\160\004\b@\160\145\144\144@@\178\005\003B\160\178\144\004@\160\144\004(\160\144\004\025@\160\176\192\005\005K\001\000\251\001!a\001!t\192\005\005L\001\000\251\001!a\001!\129@A\160\004&\160\178\004\012\160\144\004?\160\166\166B@\160\004!@@\160\176\192\005\005X\001\000\251\001!a\001!\133\192\005\005Y\001\000\251\001!a\001!\146@A@\160\176\192\005\005\\\001\000\251\001!a\001!o\004\004@A\178\144\005\002x\160\178\004\029\160\004\028\160\144\0044@\160\176\192\005\005f\001\000\249\001!\018\001!'\192\005\005g\001\000\249\001!\018\001!4@A\160\178\004&\160\004\026\160\166\004\025\160\0049@@\160\176\192\005\005p\001\000\249\001!\018\001!5\192\005\005q\001\000\249\001!\018\001!B@A@\160\176\192\005\005t\001\000\249\001!\018\001! \004\004@A\145\161@\144\005\004*\145\161@\144\005\004-@\165\160\160\176\001\006\159$diff@\179@\160\176\001\006\160\"s1@\160\176\001\006\161\"s2@@\188\144\004\007\188\144\004\006\196A\176\001\006\167\"r1@\166\166B@\160\004\t@\196A\176\001\006\168\"v1@\166\166A@\160\004\015@\196A\176\001\006\169\"l1@\166\166@@\160\004\021@\196@\176\001\006\170\005\002H@\178\005\002G\160\144\004\016\160\144\004\031@\160\176\192\005\005\166\001\001\002\001\"#\001\"3\192\005\005\167\001\001\002\001\"#\001\">@A\196A\176\001\006\172\"l2@\166\166@@\160\144\004\017@\188\166\004y\160\166\166A@\160\004\007@\160\145\144\144@@\178\004\\\160\178\144\004?\160\144\004'\160\144\004\024@\160\176\192\005\005\195\001\001\006\001\"\177\001\"\198\192\005\005\196\001\001\006\001\"\177\001\"\210@A\160\178\004\011\160\144\004=\160\166\166B@\160\004\031@@\160\176\192\005\005\207\001\001\006\001\"\177\001\"\211\192\005\005\208\001\001\006\001\"\177\001\"\223@A@\160\176\192\005\005\211\001\001\006\001\"\177\001\"\191\004\004@A\178\005\003\213\160\178\004\027\160\004\026\160\144\0041@\160\176\192\005\005\220\001\001\004\001\"c\001\"v\192\005\005\221\001\001\004\001\"c\001\"\130@A\160\004>\160\178\004%\160\004\026\160\166\004\025\160\0047@@\160\176\192\005\005\231\001\001\004\001\"c\001\"\134\192\005\005\232\001\001\004\001\"c\001\"\146@A@\160\176\192\005\005\235\001\001\004\001\"c\001\"q\004\004@A\144\004k\145\161@\144\005\004\162@\165\160\160\176\001\006\176)cons_enum@\179@\160\176\001\006\177!s@\160\176\001\006\178!e@@\188\144\004\007\178\144\004\r\160\166\166@@\160\004\007@\160\166\181@\160$MoreA@\160\166\166A@\160\004\016@\160\166\166B@\160\004\020@\160\144\004\025@@\160\176\192\005\006\021\001\001\r\001#_\001#{\192\005\006\022\001\001\r\001#_\001#\150@A\004\005@\165\160\160\176\001\006\183+compare_aux@\179@\160\176\001\006\184\"e1@\160\176\001\006\185\"e2@@\188\144\004\007\188\144\004\006\196@\176\001\006\194!c@\178\166\166@\145'compare\160\005\005$@\160\166\166@@\160\004\016@\160\166\166@@\160\004\018@@\160\176\192\005\006:\001\001\021\001$J\001$\\\192\005\006;\001\001\021\001$J\001$m@@\188\166\157A\160\144\004\025\160\145\144\144@@\004\005\178\144\004-\160\178\004J\160\166\166A@\160\004)@\160\166\166B@\160\004-@@\160\176\192\005\006S\001\001\024\001$\150\001$\177\192\005\006T\001\001\024\001$\150\001$\194@A\160\178\004X\160\166\166A@\160\0045@\160\166\166B@\160\0049@@\160\176\192\005\006a\001\001\024\001$\150\001$\195\192\005\006b\001\001\024\001$\150\001$\212@A@\160\176\192\005\006e\001\001\024\001$\150\001$\165\004\004@A\145\144\144A\188\004D\145\144\144\000\255\145\144\144@@\196B\176\001\006\195'compare@\179@\160\176\001\006\196\"s1@\160\176\001\006\197\"s2@@\178\0045\160\178\004~\160\144\004\n\160\145\161@\144#End@\160\176\192\005\006\134\001\001\027\001$\238\001%\000\192\005\006\135\001\001\027\001$\238\001%\018@A\160\178\004\139\160\144\004\020\160\145\161@\144\004\r@\160\176\192\005\006\146\001\001\027\001$\238\001%\019\192\005\006\147\001\001\027\001$\238\001%%@A@\160\176\192\005\006\150\001\001\027\001$\238\001$\244\004\004@A\196B\176\001\006\198%equal@\179@\160\176\001\006\199\"s1@\160\176\001\006\200\"s2@@\166\005\005\139\160\178\144\0044\160\144\004\011\160\144\004\n@\160\176\192\005\006\171\001\001\030\001%=\001%C\192\005\006\172\001\001\030\001%=\001%P@A\160\145\144\144@@\165\160\160\176\001\006\201&subset@\179@\160\176\001\006\202\"s1@\160\176\001\006\203\"s2@@\188\144\004\007\188\144\004\006\196A\176\001\006\208\"r2@\166\166B@\160\004\007@\196A\176\001\006\210\"l2@\166\166@@\160\004\r@\196A\176\001\006\212\"r1@\166\166B@\160\004\021@\196A\176\001\006\213\"v1@\166\166A@\160\004\027@\196A\176\001\006\214\"l1@\166\166@@\160\004!@\196@\176\001\006\215!c@\178\166\166@\145'compare\160\005\005\220@\160\144\004\022\160\166\166A@\160\004.@@\160\176\192\005\006\240\001\001'\001&\016\001&\"\192\005\006\241\001\001'\001&\016\001&3@@\188\166\005\005\221\160\144\004\022\160\145\144\144@@\166H\160\178\144\004J\160\144\004&\160\144\004:@\160\176\192\005\007\004\001\001)\001&O\001&[\192\005\007\005\001\001)\001&O\001&g@A\160\178\004\011\160\144\004<\160\144\004J@\160\176\192\005\007\014\001\001)\001&O\001&k\192\005\007\015\001\001)\001&O\001&w@A@\188\166\005\005\242\160\004\030\160\145\144\144@@\166H\160\178\004\029\160\166\181@\160\005\007\131A@\160\004 \160\0047\160\145\161@\144\005\005\214\160\145\144\144@@\160\004(@\160\176\192\005\007+\001\001+\001&\149\001&\161\192\005\007,\001\001+\001&\149\001&\196@A\160\178\0042\160\004'\160\144\004w@\160\176\192\005\0074\001\001+\001&\149\001&\200\192\005\0075\001\001+\001&\149\001&\212@A@\166H\160\178\004<\160\166\181@\160\005\007\162A@\160\145\161@\144\005\005\243\160\004Y\160\004:\160\145\144\144@@\160\004=@\160\176\192\005\007J\001\001-\001&\228\001&\240\192\005\007K\001\001-\001&\228\001'\019@A\160\178\004Q\160\004P\160\144\004\150@\160\176\192\005\007S\001\001-\001&\228\001'\023\192\005\007T\001\001-\001&\228\001'#@A@\145\161@\144\005\003\187\145\161A\144\005\004\016@\165\160\160\176\001\006\216$iter@\179@\160\176\001\006\217!f@\160\176\001\006\218\005\007\250@@\188\144\004\003\173\178\144\004\r\160\144\004\011\160\166\166@@\160\004\n@@\160\176\192\005\007s\001\0011\001'W\001's\192\005\007t\001\0011\001'W\001'{@A\173\178\004\011\160\166\166A@\160\004\020@@\160\176\192\005\007}\001\0011\001'W\001'}\192\005\007~\001\0011\001'W\001'\128@@\178\004\022\160\004\021\160\166\166B@\160\004\030@@\160\176\192\005\007\135\001\0011\001'W\001'\130\192\005\007\136\001\0011\001'W\001'\138@A\145\161@\144\"()@\165\160\160\176\001\006\223$fold@\179@\160\176\001\006\224!f@\160\176\001\006\225!s@\160\176\001\006\226$accu@@\188\144\004\007\178\144\004\016\160\144\004\014\160\166\166B@\160\004\t@\160\178\004\007\160\166\166A@\160\004\015@\160\178\004\015\160\004\014\160\166\166@@\160\004\022@\160\144\004\027@\160\176\192\005\007\183\001\0016\001'\209\001'\251\192\005\007\184\001\0016\001'\209\001(\n@A@\160\176\192\005\007\187\001\0016\001'\209\001'\246\192\005\007\188\001\0016\001'\209\001(\011@@@\160\176\192\005\007\191\001\0016\001'\209\001'\237\004\004@A\004\012@\165\160\160\176\001\006\231'for_all@\179@\160\176\001\006\232!p@\160\176\001\006\233\005\b_@@\188\144\004\003\166H\160\178\144\004\n\160\166\166A@\160\004\t@@\160\176\192\005\007\215\001\001:\001(D\001(`\192\005\007\216\001\001:\001(D\001(c@@\160\166H\160\178\144\004\027\160\004\015\160\166\166@@\160\004\023@@\160\176\192\005\007\229\001\001:\001(D\001(g\192\005\007\230\001\001:\001(D\001(r@A\160\178\004\012\160\004\026\160\166\166B@\160\004\"@@\160\176\192\005\007\240\001\001:\001(D\001(v\192\005\007\241\001\001:\001(D\001(\129@A@@\145\161A\144\005\004\170@\165\160\160\176\001\006\238&exists@\179@\160\176\001\006\239!p@\160\176\001\006\240\005\b\148@@\188\144\004\003\166I\160\178\144\004\n\160\166\166A@\160\004\t@@\160\176\192\005\b\012\001\001>\001(\186\001(\214\192\005\b\r\001\001>\001(\186\001(\217@@\160\166I\160\178\144\004\027\160\004\015\160\166\166@@\160\004\023@@\160\176\192\005\b\026\001\001>\001(\186\001(\221\192\005\b\027\001\001>\001(\186\001(\231@A\160\178\004\012\160\004\026\160\166\166B@\160\004\"@@\160\176\192\005\b%\001\001>\001(\186\001(\235\192\005\b&\001\001>\001(\186\001(\245@A@@\145\161@\144\005\004\141@\165\160\160\176\001\006\245&filter@\179@\160\176\001\006\246!p@\160\176\001\006\247\005\b\201@@\188\144\004\003\196A\176\001\006\250!v@\166\166A@\160\004\007@\196@\176\001\006\252\"l'@\178\144\004\021\160\144\004\019\160\166\166@@\160\004\018@@\160\176\192\005\bJ\001\001D\001)\135\001)\154\192\005\bK\001\001D\001)\135\001)\164@A\196@\176\001\006\253\"pv@\178\004\r\160\144\004\026@\160\176\192\005\bT\001\001E\001)\168\001)\187\192\005\bU\001\001E\001)\168\001)\190@@\196@\176\001\006\254\"r'@\178\004\025\160\004\024\160\166\166B@\160\004)@@\160\176\192\005\ba\001\001F\001)\194\001)\213\192\005\bb\001\001F\001)\194\001)\223@A\188\144\004\024\178\005\006f\160\144\004*\160\004\024\160\144\004\020@\160\176\192\005\bm\001\001G\001)\227\001)\248\192\005\bn\001\001G\001)\227\001*\004@A\178\005\003\018\160\004\n\160\004\b@\160\176\192\005\bt\001\001G\001)\227\001*\n\192\005\bu\001\001G\001)\227\001*\022@A\145\161@\144\005\007+@\165\160\160\176\001\006\255)partition@\179@\160\176\001\007\000!p@\160\176\001\007\001\005\t\024@@\188\144\004\003\196A\176\001\007\004!v@\166\166A@\160\004\007@\196@\176\001\007\006\005\0057@\178\144\004\020\160\144\004\018\160\166\166@@\160\004\017@@\160\176\192\005\b\152\001\001M\001*\180\001*\205\192\005\b\153\001\001M\001*\180\001*\218@A\196A\176\001\007\007\"lf@\166\166A@\160\144\004\020@\196A\176\001\007\b\"lt@\166\166@@\160\004\007@\196@\176\001\007\t\"pv@\178\004\026\160\144\004&@\160\176\192\005\b\175\001\001N\001*\222\001*\241\192\005\b\176\001\001N\001*\222\001*\244@@\196@\176\001\007\n\005\005\\@\178\004%\160\004$\160\166\166B@\160\0044@@\160\176\192\005\b\187\001\001O\001*\248\001+\017\192\005\b\188\001\001O\001*\248\001+\030@A\196A\176\001\007\011\"rf@\166\166A@\160\144\004\018@\196A\176\001\007\012\"rt@\166\166@@\160\004\007@\188\144\004$\166\005\005\137\160\178\005\006\207\160\144\004/\160\004&\160\144\004\015@\160\176\192\005\b\214\001\001Q\001+2\001+B\192\005\b\215\001\001Q\001+2\001+N@A\160\178\005\003|\160\144\004A\160\144\004 @\160\176\192\005\b\224\001\001Q\001+2\001+P\192\005\b\225\001\001Q\001+2\001+\\@A@\166\005\005\159\160\178\005\003\135\160\004\022\160\004\020@\160\176\192\005\b\233\001\001R\001+^\001+n\192\005\b\234\001\001R\001+^\001+z@A\160\178\005\006\237\160\004\019\160\004C\160\004\019@\160\176\192\005\b\242\001\001R\001+^\001+|\192\005\b\243\001\001R\001+^\001+\136@A@\145\178@@\160\161@\144\005\007\171\160\161@\144\005\007\174@@\165\160\160\176\001\007\r(cardinal@\179@\160\176\001\007\014\005\t\152@@\188\144\004\003\166L\160\166L\160\178\144\004\r\160\166\166@@\160\004\011@@\160\176\192\005\t\018\001\001V\001+\190\001+\218\192\005\t\019\001\001V\001+\190\001+\228@A\160\145\144\144A@\160\178\004\015\160\166\166B@\160\004\025@@\160\176\192\005\t \001\001V\001+\190\001+\235\192\005\t!\001\001V\001+\190\001+\245@A@\145\144\144@@\165\160\160\176\001\007\019,elements_aux@\179@\160\176\001\007\020$accu@\160\176\001\007\021\005\t\196@@\188\144\004\003\178\144\004\012\160\166\181@\160\"::A@\160\166\166A@\160\004\012@\160\178\004\012\160\144\004\021\160\166\166B@\160\004\020@@\160\176\192\005\tG\001\001Z\001,6\001,e\192\005\tH\001\001Z\001,6\001,x@A@\160\166\166@@\160\004\028@@\160\176\192\005\tO\001\001Z\001,6\001,R\192\005\tP\001\001Z\001,6\001,{@A\004\017@\196B\176\001\007\026(elements@\179@\160\176\001\007\027!s@@\178\004&\160\145\161@\144\"[]\160\144\004\n@\160\176\192\005\tb\001\001]\001,\146\001,\152\192\005\tc\001\001]\001,\146\001,\169@A\165\160\160\176\001\007\029$find@\179@\160\176\001\007\030!x@\160\176\001\007\031\005\n\003@@\188\144\004\003\196A\176\001\007\"!v@\166\166A@\160\004\007@\196@\176\001\007$!c@\178\166\166@\145'compare\160\005\bt@\160\144\004\023\160\144\004\018@\160\176\192\005\t\134\001\001d\001- \001-2\192\005\t\135\001\001d\001- \001-A@@\188\166\005\bs\160\144\004\020\160\145\144\144@@\004\r\178\144\004+\160\004\018\160\188\166\005\bv\160\004\012\160\145\144\144@@\166\166@@\160\004.@\166\166B@\160\0041@@\160\176\192\005\t\163\001\001f\001-_\001-n\192\005\t\164\001\001f\001-_\001-\141@A\166\156@\160\166\147\005\007?@@@\196B\176\001\007%.of_sorted_list@\179@\160\176\001\007&!l@@\165\160\160\176\001\007'#sub@\179@\160\176\001\007(!n@\160\176\001\007)!l@@\186\188\166j\160\145\144\144C\160\144\004\014@\169F@\167\144\004\017\208D\160\160@\166\005\006\137\160\145\161@\144\005\b\131\160\144\004\024@\160\160A\188\144\004\028\166\005\006\148\160\166\181@\160\005\nAA@\160\145\161@\144\005\b\146\160\166\166@@\160\004\014@\160\145\161@\144\005\b\154\160\145\144\144A@\160\166\166A@\160\004\026@@\169F@\160\160B\188\004\030\196A\176\001\007/\005\006\159@\166\166A@\160\004#@\188\144\004\006\166\005\006\184\160\166\181@\160\005\neA@\160\166\181@\160\005\niA@\160\145\161@\144\005\b\186\160\166\166@@\160\0046@\160\145\161@\144\005\b\194\160\145\144\144A@\160\166\166@@\160\004\030@\160\145\161@\144\005\b\206\160\145\144\144B@\160\166\166A@\160\004*@@\169F@\169F@\160\160C\188\004S\196A\176\001\0073\005\006\212@\166\166A@\160\004X@\188\144\004\006\196A\176\001\0074\005\006\219@\166\166A@\160\004\006@\188\144\004\006\166\005\006\244\160\166\181@\160\005\n\161A@\160\166\181@\160\005\n\165A@\160\145\161@\144\005\b\246\160\166\166@@\160\004r@\160\145\161@\144\005\b\254\160\145\144\144A@\160\166\166@@\160\004%@\160\166\181@\160\005\n\189A@\160\145\161@\144\005\t\014\160\166\166@@\160\004*@\160\145\161@\144\005\t\022\160\145\144\144A@\160\145\144\144B@\160\166\166A@\160\004:@@\169F@\169F@\169F@@@@@\160F@\196B\176\001\007;\"nl@\166O\160\144\004\194\160\145\144\144B@\196@\176\001\007<\005\007)@\178\144\004\206\160\144\004\015\160\144\004\203@\160\176\192\005\n\136\001\001r\001/\030\001/6\192\005\n\137\001\001r\001/\030\001/>@A\196A\176\001\007=!l@\166\166A@\160\144\004\018@\188\144\004\b\196@\176\001\007A\005\007>@\178\004\021\160\166M\160\166M\160\144\004\228\160\004\026@\160\145\144\144A@\160\166\166A@\160\004\019@@\160\176\192\005\n\167\001\001v\001/\144\001/\171\192\005\n\168\001\001v\001/\144\001/\189@A\166\005\007f\160\178\005\n\160\160\166\166@@\160\004 @\160\166\166@@\160\004\"@\160\166\166@@\160\144\004%@@\160\176\192\005\n\187\001\001w\001/\193\001/\205\192\005\n\188\001\001w\001/\193\001/\226@A\160\166\166A@\160\004\t@@\166\156@\160\166\181@B@\160\166\147\176Z.Assert_failureC@\160\145\178@B\160\144\162\005\n\208@\160\144\144\001\001t\160\144\144R@@@@\166\166@@\160\178\004Z\160\178\166\166@\145&length\160\166\147\176@$ListA@@\160\144\005\0019@\160\176\192\005\n\234\001\001y\001/\239\001/\254\192\005\n\235\001\001y\001/\239\0010\r@A\160\004\006@\160\176\192\005\n\239\001\001y\001/\239\001/\249\192\005\n\240\001\001y\001/\239\0010\016@A@\196B\176\001\007D'of_list@\179@\160\176\001\007E!l@@\188\144\004\004\196A\176\001\007F\005\007\165@\166\166A@\160\004\006@\196A\176\001\007G\"x0@\166\166@@\160\004\012@\188\144\004\012\196A\176\001\007H\005\007\178@\166\166A@\160\004\006@\196A\176\001\007I\"x1@\166\166@@\160\004\012@\188\144\004\012\196A\176\001\007J\005\007\191@\166\166A@\160\004\006@\196A\176\001\007K\"x2@\166\166@@\160\004\012@\188\144\004\012\196A\176\001\007L\005\007\204@\166\166A@\160\004\006@\196A\176\001\007M\"x3@\166\166@@\160\004\012@\188\144\004\012\188\166\166A@\160\004\005@\178\144\005\001\137\160\178\166\166j\145)sort_uniq\160\166\147\176@$ListA@@\160\166\166@\145'compare\160\005\n9@\160\004M@\160\176\192\005\011H\001\001\131\0011`\0011|\192\005\011I\001\001\131\0011`\0011\154@A@\160\176\192\005\011L\001\001\131\0011`\0011m\004\004@A\178\005\n$\160\166\166@@\160\004%@\160\178\005\n*\160\144\004/\160\178\005\n.\160\144\004@\160\178\005\n2\160\144\004Q\160\178\005\t\200\160\144\004b@\160\176\192\005\011d\001\001\130\0011\015\0011N\192\005\011e\001\001\130\0011\015\0011\\@A@\160\176\192\005\011h\001\001\130\0011\015\0011F\192\005\011i\001\001\130\0011\015\0011]@A@\160\176\192\005\011l\001\001\130\0011\015\0011>\192\005\011m\001\001\130\0011\015\0011^@A@\160\176\192\005\011p\001\001\130\0011\015\00116\192\005\011q\001\001\130\0011\015\0011_@A@\160\176\192\005\011t\001\001\130\0011\015\0011/\004\004@A\178\005\nL\160\144\004Q\160\178\005\nP\160\144\004b\160\178\005\nT\160\144\004s\160\178\005\t\234\160\144\004\132@\160\176\192\005\011\134\001\001\129\0010\203\0010\254\192\005\011\135\001\001\129\0010\203\0011\012@A@\160\176\192\005\011\138\001\001\129\0010\203\0010\246\192\005\011\139\001\001\129\0010\203\0011\r@A@\160\176\192\005\011\142\001\001\129\0010\203\0010\238\192\005\011\143\001\001\129\0010\203\0011\014@A@\160\176\192\005\011\146\001\001\129\0010\203\0010\231\004\004@A\178\005\nj\160\144\004|\160\178\005\nn\160\144\004\141\160\178\005\n\004\160\144\004\158@\160\176\192\005\011\160\001\001\128\0010\148\0010\187\192\005\011\161\001\001\128\0010\148\0010\201@A@\160\176\192\005\011\164\001\001\128\0010\148\0010\179\192\005\011\165\001\001\128\0010\148\0010\202@A@\160\176\192\005\011\168\001\001\128\0010\148\0010\172\004\004@A\178\005\n\128\160\144\004\159\160\178\005\n\022\160\144\004\176@\160\176\192\005\011\178\001\001\127\0010j\0010\133\192\005\011\179\001\001\127\0010j\0010\147@A@\160\176\192\005\011\182\001\001\127\0010j\0010~\004\004@A\178\005\n \160\144\004\186@\160\176\192\005\011\188\001\001~\0010M\0010]\192\005\011\189\001\001~\0010M\0010i@A\145\161@\144\005\ns\166\181@B@\160\004\006\160\144\005\b$\160\005\007\237\160\005\n\158\160\005\n1\160\005\007\141\160\005\007$\160\005\006\136\160\005\006\017\160\005\005*\160\144\005\0058\160\005\004\212\160\005\004h\160\005\0044\160\005\003\247\160\005\003\195\160\005\003\149\160\005\003H\160\005\002\205\160\144\005\002\136\160\005\t~\160\005\t\\\160\144\005\t\145\160\005\b\134\160\005\002O\160\144\004\240@@A@@")));
            ("sort.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\133\000\000\000+\000\000\000\141\000\000\000\136\176\208\208@%array\160\176A\160\160B\144\160\176\001\004\014#cmp@\160\176\001\004\015#arr@@@@@@A$list\160\176@\160\160B\144\160\176\001\003\249%order@\160\176\001\003\250!l@@@@@\208@%merge\160\176@\160\160C\144\160\176\001\003\241%order@\160\176\001\003\242\"l1@\160\176\001\003\243\"l2@@@@@@AB@@")));
            ("stack.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002\014\000\000\000\197\000\000\002e\000\000\002W\176\208\208@%Empty\160\176@@@@\208@%clear\160\176A\160\160A\144\160\176\001\003\245!s@@@@\144\179@\004\005\166\183@A\144!c\160\144\004\n\160\145\161@\144\"[]@\208@$copy\160\176A\160\160A\144\160\176\001\003\247!s@@@@\144\179@\004\005\166\181@\146\144\004\024A\160\166\166@\144\004\028\160\144\004\014@@@ABC&create\160\176A\160\160A\144\160\176\001\004\015%param@@@@\144\179@\004\005\166\181@\146\144\004-A\160\145\161@\144\004*@\208\208\208@(is_empty\160\176A\160\160A\144\160\176\001\004\003!s@@@@\144\179@\004\005\166\157@\160\166\166@\144\004E\160\144\004\012@\160\145\161@\144\004D@\208@$iter\160\176@\160\160B\144\160\176\001\004\007!f@\160\176\001\004\b!s@@@@\144\179@\004\b\178\166\166I\145$iter\160\166\147\176@$ListA@@\160\144\004\019\160\166\166@\144\004j\160\144\004\022@@\160\176\192(stack.mlh\001\004\247\001\005\006\192\004\002h\001\004\247\001\005\021@A@AB&length\160\176@\160\160A\144\160\176\001\004\005!s@@@@\144\179@\004\005\178\166\166@\145&length\160\166\147\176@$ListA@@\160\166\166@\144\004\138\160\144\004\020@@\160\176\192\004 f\001\004\215\001\004\230\192\004!f\001\004\215\001\004\245@A\208@#pop\160\176@\160\160A\144\160\176\001\003\252!s@@@@@@AC$push\160\176A\160\160B\144\160\176\001\003\249!x@\160\176\001\003\250!s@@@@@\208@#top\160\176@\160\160A\144\160\176\001\004\000!s@@@@@@ADE@@")));
            ("stdLabels.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000>\000\000\000\019\000\000\000A\000\000\000=\176\208\208@%Array\160@\144\145\161@A@A%Bytes\160@\144\004\005\208@$List\160@\144\004\t\208@&String\160@\144\004\r@ABC@@")));
            ("std_exit.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\005\000\000\000\003\000\000\000\b\000\000\000\b\176@\144 @")));
            ("stream.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002\168\000\000\000\234\000\000\003\t\000\000\002\240\176\208\208\208\208@%Error\160\176@@@@@A'Failure\160\004\003@\208\208@%count\160@\144\179@\160\176\001\004m$prim@@\166\166@@\160\144\004\006@\208@$dump\160\176@\160\160B\144\160\176\001\004e!f@\160\176\001\004f!s@@@@@@AB%empty\160\176A\160\160A\144\160\176\001\004:!s@@@@@@CD$from\160\176A\160\160A\144\160\176\001\004A!f@@@@@\208@$iapp\160\176A\160\160B\144\160\176\001\004Q!i@\160\176\001\004R!s@@@@@\208@%icons\160\176A\160\160B\144\160\176\001\004T!i@\160\176\001\004U!s@@@@@\208@%ising\160\176A\160\160A\144\160\176\001\004W!i@@@@@@ABCE$iter\160\176@\160\160B\144\160\176\001\004<!f@\160\176\001\004=$strm@@@@@\208\208\208\208@$junk\160\176@\160\160A\144\160\176\001\004%!s@@@@@@A$lapp\160\176A\160\160B\144\160\176\001\004Y!f@\160\176\001\004Z!s@@@@@\208@%lcons\160\176A\160\160B\144\160\176\001\004\\!f@\160\176\001\004]!s@@@@@\208@%lsing\160\176A\160\160A\144\160\176\001\004_!f@@@@@@ABC$next\160\176@\160\160A\144\160\176\001\0047!s@@@@@\208@%npeek\160\176@\160\160B\144\160\176\001\0041!n@\160\176\001\0042!s@@@@@@AD(of_bytes\160\176A\160\160A\144\160\176\001\004K!s@@@@@\208\208@*of_channel\160\176A\160\160A\144\160\176\001\004O\"ic@@@@@@A'of_list\160\176A\160\160A\144\160\176\001\004C!l@@@@@\208\208@)of_string\160\176A\160\160A\144\160\176\001\004G!s@@@@@@A$peek\160\176@\160\160A\144\160\176\001\004\027!s@@@@@\208@&sempty\160@@\208@%slazy\160\176A\160\160A\144\160\176\001\004b!f@@@@@@ABCDEF@@")));
            ("string.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\t$\000\000\002\190\000\000\t\007\000\000\b\214\176\208\208\208\208@$blit\160\176@\160\160E\144\160\176\001\004,\"s1@\160\176\001\004-$ofs1@\160\176\001\004.\"s2@\160\176\001\004/$ofs2@\160\176\001\0040#len@@@@@\208@*capitalize\160\176@\160\160A\144\160\176\001\004G!s@@@@\144\179@\004\005\178\166\166`\1450unsafe_to_string\160\166\147\176@%BytesA@@\160\178\166\166]\145*capitalize\160\166\147\004\011@@\160\178\166\166a\1450unsafe_of_string\160\166\147\004\020@@\160\144\004\"@\160\176\192)string.ml\000x\001\014\154\001\014\169\192\004\002\000x\001\014\154\001\014\176@@@\160\176\192\004\005\000x\001\014\154\001\014\156\004\004@A@\160\176\004\003\192\004\b\000x\001\014\154\001\014\183@@\208@'compare\160\176@\160\160B\144\160\176\001\004L!x@\160\176\001\004M!y@@@@\144\179@\004\b\166\155\2403caml_string_compareB@ @@@\160\144\004\014\160\144\004\r@@ABC&concat\160\176A\160\160B\144\160\176\001\004\n#sep@\160\176\001\004\011!l@@@@@\208@(contains\160\176A\160\160B\144\160\176\001\0048!s@\160\176\001\0049!c@@@@\144\179@\004\b\178\166\166X\145(contains\160\166\147\004\\@@\160\178\166\004Q\160\166\004N@@\160\144\004\022@\160\176\192\004M\000n\001\r\160\001\r\173\192\004N\000n\001\r\160\001\r\180@@\160\144\004\025@\160\176\192\004S\000n\001\r\160\001\r\162\192\004T\000n\001\r\160\001\r\182@A\208@-contains_from\160\176A\160\160C\144\160\176\001\004;!s@\160\176\001\004<!i@\160\176\001\004=!c@@@@@@ABD$copy\160\176@\160\160A\144\160\176\001\004\002!s@@@@\144\179@\004\005\178\166\004\144\160\166\004\141@@\160\178\166\166C\145$copy\160\166\147\004\149@@\160\178\166\004\138\160\166\004\135@@\160\144\004\024@\160\176\192\004\134e\001\006\173\001\006\182\192\004\135e\001\006\173\001\006\189@@@\160\176\192\004\138e\001\006\173\001\006\175\004\004@A@\160\176\004\003\192\004\141e\001\006\173\001\006\196@@\208\208@'escaped\160\176@\160\160A\144\160\176\001\004%!s@@@@@@A$fill\160\176@\160\160D\144\160\176\001\004!!s@\160\176\001\004\"#ofs@\160\176\001\004##len@\160\176\001\004$!c@@@@@\208@%index\160\176@\160\160B\144\160\176\001\004*!s@\160\176\001\004+!c@@@@\144\179@\004\b\178\166\166T\145%index\160\166\147\004\218@@\160\178\166\004\207\160\166\004\204@@\160\144\004\022@\160\176\192\004\203\000f\001\012\238\001\012\248\192\004\204\000f\001\012\238\001\012\255@@\160\144\004\025@\160\176\192\004\209\000f\001\012\238\001\012\240\192\004\210\000f\001\012\238\001\r\001@A\208@*index_from\160\176@\160\160C\144\160\176\001\0040!s@\160\176\001\0041!i@\160\176\001\0042!c@@@@@@ABCE$init\160\176@\160\160B\144\160\176\001\003\255!n@\160\176\001\004\000!f@@@@\144\179@\004\b\178\166\005\001\017\160\166\005\001\014@@\160\178\166\166A\145$init\160\166\147\005\001\022@@\160\144\004\022\160\144\004\021@\160\176\192\005\001\004c\001\006\140\001\006\142\192\005\001\005c\001\006\140\001\006\152@A@\160\176\004\004\192\005\001\bc\001\006\140\001\006\159@@\208\208@$iter\160\176A\160\160B\144\160\176\001\004\021!f@\160\176\001\004\022!s@@@@\144\179@\004\b\178\166\166N\145$iter\160\166\147\005\0019@@\160\144\004\017\160\178\166\005\0010\160\166\005\001-@@\160\144\004\021@\160\176\192\005\001,\000@\001\tU\001\t`\192\005\001-\000@\001\tU\001\tg@@@\160\176\192\005\0010\000@\001\tU\001\tW\004\004@A\208@%iteri\160\176A\160\160B\144\160\176\001\004\024!f@\160\176\001\004\025!s@@@@\144\179@\004\b\178\166\166O\145%iteri\160\166\147\005\001`@@\160\144\004\017\160\178\166\005\001W\160\166\005\001T@@\160\144\004\021@\160\176\192\005\001S\000B\001\tx\001\t\132\192\005\001T\000B\001\tx\001\t\139@@@\160\176\192\005\001W\000B\001\tx\001\tz\004\004@A\208@)lowercase\160\176@\160\160A\144\160\176\001\004E!s@@@@\144\179@\004\005\178\166\005\001\132\160\166\005\001\129@@\160\178\166\166\\\145)lowercase\160\166\147\005\001\137@@\160\178\166\005\001~\160\166\005\001{@@\160\144\004\024@\160\176\192\005\001z\000v\001\014j\001\014x\192\005\001{\000v\001\014j\001\014\127@@@\160\176\192\005\001~\000v\001\014j\001\014l\004\004@A@\160\176\004\003\192\005\001\129\000v\001\014j\001\014\134@@@ABC$make\160\176@\160\160B\144\160\176\001\003\252!n@\160\176\001\003\253!c@@@@\144\179@\004\b\178\166\005\001\176\160\166\005\001\173@@\160\178\166\166@\145$make\160\166\147\005\001\181@@\160\144\004\022\160\144\004\021@\160\176\192\005\001\163a\001\006i\001\006k\192\005\001\164a\001\006i\001\006u@A@\160\176\004\004\192\005\001\167a\001\006i\001\006|@@\208\208\208@#map\160\176@\160\160B\144\160\176\001\004\027!f@\160\176\001\004\028!s@@@@@@A$mapi\160\176@\160\160B\144\160\176\001\004\030!f@\160\176\001\004\031!s@@@@@\208\208@.rcontains_from\160\176A\160\160C\144\160\176\001\004?!s@\160\176\001\004@!i@\160\176\001\004A!c@@@@@@A&rindex\160\176@\160\160B\144\160\176\001\004-!s@\160\176\001\004.!c@@@@\144\179@\004\b\178\166\166U\145&rindex\160\166\147\005\002\002@@\160\178\166\005\001\247\160\166\005\001\244@@\160\144\004\022@\160\176\192\005\001\243\000h\001\r\019\001\r\030\192\005\001\244\000h\001\r\019\001\r%@@\160\144\004\025@\160\176\192\005\001\249\000h\001\r\019\001\r\021\192\005\001\250\000h\001\r\019\001\r'@A\208@+rindex_from\160\176@\160\160C\144\160\176\001\0044!s@\160\176\001\0045!i@\160\176\001\0046!c@@@@@@ABC#sub\160\176@\160\160C\144\160\176\001\004\004!s@\160\176\001\004\005#ofs@\160\176\001\004\006#len@@@@@\208@$trim\160\176@\160\160A\144\160\176\001\004#!s@@@@@\208\208@,uncapitalize\160\176@\160\160A\144\160\176\001\004I!s@@@@\144\179@\004\005\178\166\005\002Q\160\166\005\002N@@\160\178\166\166^\145,uncapitalize\160\166\147\005\002V@@\160\178\166\005\002K\160\166\005\002H@@\160\144\004\024@\160\176\192\005\002G\000z\001\014\205\001\014\222\192\005\002H\000z\001\014\205\001\014\229@@@\160\176\192\005\002K\000z\001\014\205\001\014\207\004\004@A@\160\176\004\003\192\005\002N\000z\001\014\205\001\014\236@@@A)uppercase\160\176@\160\160A\144\160\176\001\004C!s@@@@\144\179@\004\005\178\166\005\002z\160\166\005\002w@@\160\178\166\166[\145)uppercase\160\166\147\005\002\127@@\160\178\166\005\002t\160\166\005\002q@@\160\144\004\024@\160\176\192\005\002p\000t\001\014;\001\014I\192\005\002q\000t\001\014;\001\014P@@@\160\176\192\005\002t\000t\001\014;\001\014=\004\004@A@\160\176\004\003\192\005\002w\000t\001\014;\001\014W@@@BCDEF@@")));
            ("stringLabels.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\003\225\000\000\001O\000\000\004E\000\000\004)\176\208\208\208\208@$blit\160\176@\160\160E\144\160\176\001\004,\"s1@\160\176\001\004-$ofs1@\160\176\001\004.\"s2@\160\176\001\004/$ofs2@\160\176\001\0040#len@@@@@\208@*capitalize\160\176@\160\160A\144\160\176\001\004G!s@@@@@\208@'compare\160\176@\160\160B\144\160\176\001\004L!x@\160\176\001\004M!y@@@@@@ABC&concat\160\176A\160\160B\144\160\176\001\004\n#sep@\160\176\001\004\011!l@@@@@\208@(contains\160\176A\160\160B\144\160\176\001\0048!s@\160\176\001\0049!c@@@@@\208@-contains_from\160\176A\160\160C\144\160\176\001\004;!s@\160\176\001\004<!i@\160\176\001\004=!c@@@@@@ABD$copy\160\176@\160\160A\144\160\176\001\004\002!s@@@@@\208\208@'escaped\160\176@\160\160A\144\160\176\001\004%!s@@@@@@A$fill\160\176@\160\160D\144\160\176\001\004!!s@\160\176\001\004\"#ofs@\160\176\001\004##len@\160\176\001\004$!c@@@@@\208@%index\160\176@\160\160B\144\160\176\001\004*!s@\160\176\001\004+!c@@@@@\208@*index_from\160\176@\160\160C\144\160\176\001\0040!s@\160\176\001\0041!i@\160\176\001\0042!c@@@@@@ABCE$init\160\176@\160\160B\144\160\176\001\003\255!n@\160\176\001\004\000!f@@@@@\208\208@$iter\160\176A\160\160B\144\160\176\001\004\021!f@\160\176\001\004\022!s@@@@@\208@%iteri\160\176A\160\160B\144\160\176\001\004\024!f@\160\176\001\004\025!s@@@@@\208@)lowercase\160\176@\160\160A\144\160\176\001\004E!s@@@@@@ABC$make\160\176@\160\160B\144\160\176\001\003\252!n@\160\176\001\003\253!c@@@@@\208\208\208@#map\160\176@\160\160B\144\160\176\001\004\027!f@\160\176\001\004\028!s@@@@@@A$mapi\160\176@\160\160B\144\160\176\001\004\030!f@\160\176\001\004\031!s@@@@@\208\208@.rcontains_from\160\176A\160\160C\144\160\176\001\004?!s@\160\176\001\004@!i@\160\176\001\004A!c@@@@@@A&rindex\160\176@\160\160B\144\160\176\001\004-!s@\160\176\001\004.!c@@@@@\208@+rindex_from\160\176@\160\160C\144\160\176\001\0044!s@\160\176\001\0045!i@\160\176\001\0046!c@@@@@@ABC#sub\160\176@\160\160C\144\160\176\001\004\004!s@\160\176\001\004\005#ofs@\160\176\001\004\006#len@@@@@\208@$trim\160\176@\160\160A\144\160\176\001\004#!s@@@@@\208\208@,uncapitalize\160\176@\160\160A\144\160\176\001\004I!s@@@@@@A)uppercase\160\176@\160\160A\144\160\176\001\004C!s@@@@@@BCDEF@@")));
            ("sys.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\003\005\000\000\000\165\000\000\002\129\000\000\002Q\176\208\208\208\208\208@%Break\160\176@@@@@A$argv\160@@\208\208@*big_endian\160\176A@@@\208@+catch_break\160\176A\160\160A\144\160\176\001\004-\"on@@@@@@AB&cygwin\160\004\r@@CD/executable_name\160@@\208\208@+interactive\160\004\019@@A%is_js\160\004\028@\208\208@0max_array_length\160\004\025@@A1max_string_length\160\004\027@\208@-ocaml_version\160@@@ABCE'os_type\160@@\208\208\208@*set_signal\160\176A\160\160B\144\160\176\001\004\020'sig_num@\160\176\001\004\021'sig_beh@@@@\144\179@\004\b\166F\160\166\155\240;caml_install_signal_handlerBA @@\144\176\193 \176\179\144\176A#int@@\144@\002\005\245\225\000\001\006\215\176\193\004\t\176\179\144\176\001\004\014/signal_behavior@@\144@\002\005\245\225\000\001\006\218\176\179\004\006@\144@\002\005\245\225\000\001\006\221@\002\005\245\225\000\001\006\224@\002\005\245\225\000\001\006\225\160\144\004%\160\144\004$@@@A'sigabrt\160@@@B'sigalrm\160@@\208\208@'sigchld\160@@\208@'sigcont\160@@@AB&sigfpe\160@@\208@&sighup\160@@@ACDF&sigill\160@@\208\208\208\208@&sigint\160@@@A'sigkill\160@@\208@'sigpipe\160@@\208@'sigprof\160@@@ABC'sigquit\160@@\208@'sigsegv\160@@\208@'sigstop\160@@@ABD'sigterm\160@@\208\208\208@'sigtstp\160@@\208@'sigttin\160@@\208@'sigttou\160@@@ABC'sigusr1\160@@\208@'sigusr2\160@@\208@)sigvtalrm\160@@@ABD$unix\160\004\144@\208@%win32\160\004\147@\208@)word_size\160\004\150@@ABEFG@@")));
            ("unix.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000BK\000\000\014\183\000\0004\006\000\0002.\176\208\208\208\208\208\208\208@)LargeFile\160@@@A*Unix_error\160\176@@@@\208@&accept\160@\144\179@\160\176\001\007 $prim@@\166\155\240+unix_acceptAA @@\144\176\193 \176\179\144\176\001\004W*file_descr@@\144@\002\005\245\225\000\001\024\198\176\146\160\176\179\004\t@\144@\002\005\245\225\000\001\024\201\160\176\179\144\176\001\005F(sockaddr@@\144@\002\005\245\225\000\001\024\204@\002\005\245\225\000\001\024\207@\002\005\245\225\000\001\024\208\160\144\004 @@AB&access\160@\144\179@\160\176\001\007k\004%@\160\176\001\007j\004'@@\166\155\240+unix_accessBA\004&@@\144\176\193\004%\176\179\144\176C&string@@\144@\002\005\245\225\000\001\020\186\176\193\004-\176\179\144\176I$list@\160\176\179\144\176\001\004\1961access_permission@@\144@\002\005\245\225\000\001\020\189@\144@\002\005\245\225\000\001\020\193\176\179\144\176F$unit@@\144@\002\005\245\225\000\001\020\197@\002\005\245\225\000\001\020\200@\002\005\245\225\000\001\020\201\160\144\004'\160\144\004'@\208\208@%alarm\160@\144\179@\160\176\001\007@\004U@@\166\155\240*unix_alarmAA\004T@@\144\176\193\004S\176\179\144\176A#int@@\144@\002\005\245\225\000\001\022\198\176\179\004\006@\144@\002\005\245\225\000\001\022\201@\002\005\245\225\000\001\022\204\160\144\004\019@\208@$bind\160@\144\179@\160\176\001\007\031\004n@\160\176\001\007\030\004p@@\166\155\240)unix_bindBA\004o@@\144\176\193\004n\176\179\004m@\144@\002\005\245\225\000\001\024\209\176\193\004s\176\179\004e@\144@\002\005\245\225\000\001\024\212\176\179\004<@\144@\002\005\245\225\000\001\024\215@\002\005\245\225\000\001\024\218@\002\005\245\225\000\001\024\219\160\144\004\023\160\144\004\023@@AB%chdir\160@\144\179@\160\176\001\007_\004\140@@\166\155\240*unix_chdirAA\004\139@@\144\176\193\004\138\176\179\004e@\144@\002\005\245\225\000\001\021%\176\179\004S@\144@\002\005\245\225\000\001\021(@\002\005\245\225\000\001\021+\160\144\004\016@@CD%chmod\160@\144\179@\160\176\001\007v\004\161@\160\176\001\007u\004\163@@\166\155\240*unix_chmodBA\004\162@@\144\176\193\004\161\176\179\004|@\144@\002\005\245\225\000\001\020\127\176\193\004\166\176\179\144\176\001\004j)file_perm@@\144@\002\005\245\225\000\001\020\130\176\179\004r@\144@\002\005\245\225\000\001\020\133@\002\005\245\225\000\001\020\136@\002\005\245\225\000\001\020\137\160\144\004\026\160\144\004\026@\208\208@%chown\160@\144\179@\160\176\001\007r\004\196@\160\176\001\007q\004\198@\160\176\001\007p\004\200@@\166\155\240*unix_chownCA\004\199@@\144\176\193\004\198\176\179\004\161@\144@\002\005\245\225\000\001\020\149\176\193\004\203\176\179\004x@\144@\002\005\245\225\000\001\020\152\176\193\004\208\176\179\004}@\144@\002\005\245\225\000\001\020\155\176\179\004\153@\144@\002\005\245\225\000\001\020\158@\002\005\245\225\000\001\020\161@\002\005\245\225\000\001\020\162@\002\005\245\225\000\001\020\163\160\144\004\030\160\144\004\030\160\144\004\030@\208@&chroot\160@\144\179@\160\176\001\007]\004\236@@\166\155\240+unix_chrootAA\004\235@@\144\176\193\004\234\176\179\004\197@\144@\002\005\245\225\000\001\0213\176\179\004\179@\144@\002\005\245\225\000\001\0216@\002\005\245\225\000\001\0219\160\144\004\016@@AB3clear_close_on_exec\160@\144\179@\160\176\001\007c\005\001\001@@\166\155\2408unix_clear_close_on_execAA\005\001\000@@\144\176\193\004\255\176\179\004\254@\144@\002\005\245\225\000\001\020\241\176\179\004\200@\144@\002\005\245\225\000\001\020\244@\002\005\245\225\000\001\020\247\160\144\004\016@\208@.clear_nonblock\160@\144\179@\160\176\001\007e\005\001\023@@\166\155\2403unix_clear_nonblockAA\005\001\022@@\144\176\193\005\001\021\176\179\005\001\020@\144@\002\005\245\225\000\001\020\227\176\179\004\222@\144@\002\005\245\225\000\001\020\230@\002\005\245\225\000\001\020\233\160\144\004\016@@ACE%close\160@\144\179@\160\176\001\007\149\005\001,@@\166\155\240*unix_closeAA\005\001+@@\144\176\193\005\001*\176\179\005\001)@\144@\002\005\245\225\000\001\015\249\176\179\004\243@\144@\002\005\245\225\000\001\015\252@\002\005\245\225\000\001\015\255\160\144\004\016@\208\208\208\208@-close_process\160\176@\160\160A\144\160\176\001\007\186%param@@@@@\208@2close_process_full\160\176@\160\160A\144\160\176\001\007\182\004\n@@@@@@AB0close_process_in\160\176@\160\160A\144\160\176\001\006\226&inchan@@@@@\208\208@1close_process_out\160\176@\160\160A\144\160\176\001\006\229'outchan@@@@@@A(closedir\160@\144\179@\160\176\001\007Y\005\001k@@\166\155\240-unix_closedirAA\005\001j@@\144\176\193\005\001i\176\179\144\176\001\004\220*dir_handle@@\144@\002\005\245\225\000\001\021O\176\179\005\0015@\144@\002\005\245\225\000\001\021R@\002\005\245\225\000\001\021U\160\144\004\019@\208@'connect\160@\144\179@\160\176\001\007\029\005\001\132@\160\176\001\007\028\005\001\134@@\166\155\240,unix_connectBA\005\001\133@@\144\176\193\005\001\132\176\179\005\001\131@\144@\002\005\245\225\000\001\024\220\176\193\005\001\137\176\179\005\001{@\144@\002\005\245\225\000\001\024\223\176\179\005\001R@\144@\002\005\245\225\000\001\024\226@\002\005\245\225\000\001\024\229@\002\005\245\225\000\001\024\230\160\144\004\023\160\144\004\023@@ABC.create_process\160\176@\160\160E\144\160\176\001\006m#cmd@\160\176\001\006n$args@\160\176\001\006o)new_stdin@\160\176\001\006p*new_stdout@\160\176\001\006q*new_stderr@@@@@\208\208@2create_process_env\160\176@\160\160F\144\160\176\001\006t#cmd@\160\176\001\006u$args@\160\176\001\006v#env@\160\176\001\006w)new_stdin@\160\176\001\006x*new_stdout@\160\176\001\006y*new_stderr@@@@@@A3descr_of_in_channel\160@\144\179@\160\176\001\007\146\005\001\209@@\166\155\2407caml_channel_descriptorAA\005\001\208@@\144\176\193\005\001\207\176\179\177\144\176@*PervasivesA*in_channel\000\255@\144@\002\005\245\225\000\001\018\242\176\179\005\001\214@\144@\002\005\245\225\000\001\018\245@\002\005\245\225\000\001\018\248\160\144\004\021@@BD4descr_of_out_channel\160@\144\179@\160\176\001\007\145\005\001\235@@\166\155\2407caml_channel_descriptorAA\005\001\234@@\144\176\193\005\001\233\176\179\177\004\026+out_channel\000\255@\144@\002\005\245\225\000\001\018\249\176\179\005\001\237@\144@\002\005\245\225\000\001\018\252@\002\005\245\225\000\001\018\255\160\144\004\018@\208\208\208@2domain_of_sockaddr\160\176A\160\160A\144\160\176\001\007\254\004\192@@@@@@A#dup\160@\144\179@\160\176\001\007i\005\002\r@@\166\155\240(unix_dupAA\005\002\012@@\144\176\193\005\002\011\176\179\005\002\n@\144@\002\005\245\225\000\001\020\202\176\179\005\002\r@\144@\002\005\245\225\000\001\020\205@\002\005\245\225\000\001\020\208\160\144\004\016@\208@$dup2\160@\144\179@\160\176\001\007h\005\002#@\160\176\001\007g\005\002%@@\166\155\240)unix_dup2BA\005\002$@@\144\176\193\005\002#\176\179\005\002\"@\144@\002\005\245\225\000\001\020\209\176\193\005\002(\176\179\005\002'@\144@\002\005\245\225\000\001\020\212\176\179\005\001\241@\144@\002\005\245\225\000\001\020\215@\002\005\245\225\000\001\020\218@\002\005\245\225\000\001\020\219\160\144\004\023\160\144\004\023@@AB+environment\160@\144\179@\160\176\001\007\173\005\002A@@\166\155\2400unix_environmentAA\005\002@@@\144\176\193\005\002?\176\179\005\002\005@\144@\002\005\245\225\000\001\014\252\176\179\144\176H%array@\160\176\179\005\002#@\144@\002\005\245\225\000\001\014\255@\144@\002\005\245\225\000\001\015\003@\002\005\245\225\000\001\015\007\160\144\004\023@\208@-error_message\160@\144\179@\160\176\001\007\174\005\002^@@\166\155\2402unix_error_messageAA\005\002]@@\144\176\193\005\002\\\176\179\144\176\001\003\240%error@@\144@\002\005\245\225\000\001\003\026\176\179\005\002=@\144@\002\005\245\225\000\001\003\029@\002\005\245\225\000\001\003 \160\144\004\019@\208\208@0establish_server\160\176A\160\160B\144\160\176\001\006\249*server_fun@\160\176\001\006\250(sockaddr@@@@@@A%execv\160@\144\179@\160\176\001\007\169\005\002\132@\160\176\001\007\168\005\002\134@@\166\155\240*unix_execvBA\005\002\133@@\144\176\193\005\002\132\176\179\005\002_@\144@\002\005\245\225\000\001\0155\176\193\005\002\137\176\179\004G\160\176\179\005\002g@\144@\002\005\245\225\000\001\0158@\144@\002\005\245\225\000\001\015<\176\150\176\144\144!a\002\005\245\225\000\001\015D\001\004M\001\015@@\002\005\245\225\000\001\015A@\002\005\245\225\000\001\015B\160\144\004\030\160\144\004\030@@BCDEF&execve\160@\144\179@\160\176\001\007\167\005\002\169@\160\176\001\007\166\005\002\171@\160\176\001\007\165\005\002\173@@\166\155\240+unix_execveCA\005\002\172@@\144\176\193\005\002\171\176\179\005\002\134@\144@\002\005\245\225\000\001\015E\176\193\005\002\176\176\179\004n\160\176\179\005\002\142@\144@\002\005\245\225\000\001\015H@\144@\002\005\245\225\000\001\015L\176\193\005\002\185\176\179\004w\160\176\179\005\002\151@\144@\002\005\245\225\000\001\015P@\144@\002\005\245\225\000\001\015T\176\150\176\144\144!a\002\005\245\225\000\001\015]\001\004N\001\015X@\002\005\245\225\000\001\015Y@\002\005\245\225\000\001\015Z@\002\005\245\225\000\001\015[\160\144\004)\160\144\004)\160\144\004)@\208\208\208@&execvp\160@\144\179@\160\176\001\007\164\005\002\222@\160\176\001\007\163\005\002\224@@\166\155\240+unix_execvpBA\005\002\223@@\144\176\193\005\002\222\176\179\005\002\185@\144@\002\005\245\225\000\001\015^\176\193\005\002\227\176\179\004\161\160\176\179\005\002\193@\144@\002\005\245\225\000\001\015a@\144@\002\005\245\225\000\001\015e\176\150\176\144\144!a\002\005\245\225\000\001\015m\001\004O\001\015i@\002\005\245\225\000\001\015j@\002\005\245\225\000\001\015k\160\144\004\030\160\144\004\030@@A'execvpe\160@\144\179@\160\176\001\007\162\005\003\003@\160\176\001\007\161\005\003\005@\160\176\001\007\160\005\003\007@@\166\155\240,unix_execvpeCA\005\003\006@@\144\176\193\005\003\005\176\179\005\002\224@\144@\002\005\245\225\000\001\015n\176\193\005\003\n\176\179\004\200\160\176\179\005\002\232@\144@\002\005\245\225\000\001\015q@\144@\002\005\245\225\000\001\015u\176\193\005\003\019\176\179\004\209\160\176\179\005\002\241@\144@\002\005\245\225\000\001\015y@\144@\002\005\245\225\000\001\015}\176\150\176\144\144!a\002\005\245\225\000\001\015\134\001\004P\001\015\129@\002\005\245\225\000\001\015\130@\002\005\245\225\000\001\015\131@\002\005\245\225\000\001\015\132\160\144\004)\160\144\004)\160\144\004)@\208@&fchmod\160@\144\179@\160\176\001\007t\005\0036@\160\176\001\007s\005\0038@@\166\155\240+unix_fchmodBA\005\0037@@\144\176\193\005\0036\176\179\005\0035@\144@\002\005\245\225\000\001\020\138\176\193\005\003;\176\179\005\002\149@\144@\002\005\245\225\000\001\020\141\176\179\005\003\004@\144@\002\005\245\225\000\001\020\144@\002\005\245\225\000\001\020\147@\002\005\245\225\000\001\020\148\160\144\004\023\160\144\004\023@\208@&fchown\160@\144\179@\160\176\001\007o\005\003U@\160\176\001\007n\005\003W@\160\176\001\007m\005\003Y@@\166\155\240+unix_fchownCA\005\003X@@\144\176\193\005\003W\176\179\005\003V@\144@\002\005\245\225\000\001\020\164\176\193\005\003\\\176\179\005\003\t@\144@\002\005\245\225\000\001\020\167\176\193\005\003a\176\179\005\003\014@\144@\002\005\245\225\000\001\020\170\176\179\005\003*@\144@\002\005\245\225\000\001\020\173@\002\005\245\225\000\001\020\176@\002\005\245\225\000\001\020\177@\002\005\245\225\000\001\020\178\160\144\004\030\160\144\004\030\160\144\004\030@@ABC$fork\160@\144\179@\160\176\001\007\159\005\003|@@\166\155\240)unix_forkAA\005\003{@@\144\176\193\005\003z\176\179\005\003@@\144@\002\005\245\225\000\001\015\135\176\179\005\003*@\144@\002\005\245\225\000\001\015\138@\002\005\245\225\000\001\015\141\160\144\004\016@\208\208@%fstat\160@\144\179@\160\176\001\007\135\005\003\147@@\166\155\240*unix_fstatAA\005\003\146@@\144\176\193\005\003\145\176\179\005\003\144@\144@\002\005\245\225\000\001\019\168\176\179\144\176\001\004\156%stats@@\144@\002\005\245\225\000\001\019\171@\002\005\245\225\000\001\019\174\160\144\004\019@@A)ftruncate\160@\144\179@\160\176\001\007\139\005\003\171@\160\176\001\007\138\005\003\173@@\166\155\240.unix_ftruncateBA\005\003\172@@\144\176\193\005\003\171\176\179\005\003\170@\144@\002\005\245\225\000\001\019\029\176\193\005\003\176\176\179\005\003]@\144@\002\005\245\225\000\001\019 \176\179\005\003y@\144@\002\005\245\225\000\001\019#@\002\005\245\225\000\001\019&@\002\005\245\225\000\001\019'\160\144\004\023\160\144\004\023@\208\208@+getaddrinfo\160\176@\160\160C\144\160\176\001\006\006$node@\160\176\001\006\007'service@\160\176\001\006\b$opts@@@@@@A&getcwd\160@\144\179@\160\176\001\007^\005\003\218@@\166\155\240+unix_getcwdAA\005\003\217@@\144\176\193\005\003\216\176\179\005\003\158@\144@\002\005\245\225\000\001\021,\176\179\005\003\182@\144@\002\005\245\225\000\001\021/@\002\005\245\225\000\001\0212\160\144\004\016@\208@'getegid\160@\144\179@\160\176\001\0073\005\003\240@@\166\155\240,unix_getegidAA\005\003\239@@\144\176\193\005\003\238\176\179\005\003\180@\144@\002\005\245\225\000\001\0230\176\179\005\003\158@\144@\002\005\245\225\000\001\0233@\002\005\245\225\000\001\0236\160\144\004\016@@ABCDG&getenv\160@\144\179@\160\176\001\007\172\005\004\005@@\166\155\240/caml_sys_getenvAA\005\004\004@@\144\176\193\005\004\003\176\179\005\003\222@\144@\002\005\245\225\000\001\015\b\176\179\005\003\225@\144@\002\005\245\225\000\001\015\011@\002\005\245\225\000\001\015\014\160\144\004\016@\208\208\208\208\208\208\208@'geteuid\160@\144\179@\160\176\001\0076\005\004!@@\166\155\240,unix_geteuidAA\005\004 @@\144\176\193\005\004\031\176\179\005\003\229@\144@\002\005\245\225\000\001\023\027\176\179\005\003\207@\144@\002\005\245\225\000\001\023\030@\002\005\245\225\000\001\023!\160\144\004\016@@A&getgid\160@\144\179@\160\176\001\0074\005\0046@@\166\155\240+unix_getgidAA\005\0045@@\144\176\193\005\0044\176\179\005\003\250@\144@\002\005\245\225\000\001\023)\176\179\005\003\228@\144@\002\005\245\225\000\001\023,@\002\005\245\225\000\001\023/\160\144\004\016@\208\208@(getgrgid\160@\144\179@\160\176\001\007)\005\004M@@\166\155\240-unix_getgrgidAA\005\004L@@\144\176\193\005\004K\176\179\005\003\248@\144@\002\005\245\225\000\001\023\241\176\179\144\176\001\005*+group_entry@@\144@\002\005\245\225\000\001\023\244@\002\005\245\225\000\001\023\247\160\144\004\019@@A(getgrnam\160@\144\179@\160\176\001\007+\005\004e@@\166\155\240-unix_getgrnamAA\005\004d@@\144\176\193\005\004c\176\179\005\004>@\144@\002\005\245\225\000\001\023\227\176\179\004\024@\144@\002\005\245\225\000\001\023\230@\002\005\245\225\000\001\023\233\160\144\004\016@@BC)getgroups\160@\144\179@\160\176\001\0071\005\004z@@\166\155\240.unix_getgroupsAA\005\004y@@\144\176\193\005\004x\176\179\005\004>@\144@\002\005\245\225\000\001\023>\176\179\005\0029\160\176\179\005\004+@\144@\002\005\245\225\000\001\023A@\144@\002\005\245\225\000\001\023E@\002\005\245\225\000\001\023I\160\144\004\020@\208\208\208\208@-gethostbyaddr\160@\144\179@\160\176\001\007\019\005\004\151@@\166\155\2402unix_gethostbyaddrAA\005\004\150@@\144\176\193\005\004\149\176\179\144\176\001\0054)inet_addr@@\144@\002\005\245\225\000\001\031}\176\179\144\176\001\005\205*host_entry@@\144@\002\005\245\225\000\001\031\128@\002\005\245\225\000\001\031\131\160\144\004\022@@A-gethostbyname\160@\144\179@\160\176\001\007\020\005\004\178@@\166\155\2402unix_gethostbynameAA\005\004\177@@\144\176\193\005\004\176\176\179\005\004\139@\144@\002\005\245\225\000\001\031v\176\179\004\024@\144@\002\005\245\225\000\001\031y@\002\005\245\225\000\001\031|\160\144\004\016@@B+gethostname\160@\144\179@\160\176\001\007\021\005\004\199@@\166\155\2400unix_gethostnameAA\005\004\198@@\144\176\193\005\004\197\176\179\005\004\139@\144@\002\005\245\225\000\001\031o\176\179\005\004\163@\144@\002\005\245\225\000\001\031r@\002\005\245\225\000\001\031u\160\144\004\016@@C)getitimer\160@\144\179@\160\176\001\007:\005\004\220@@\166\155\240.unix_getitimerAA\005\004\219@@\144\176\193\005\004\218\176\179\144\176\001\005\016.interval_timer@@\144@\002\005\245\225\000\001\023\002\176\179\144\176\001\005\0205interval_timer_status@@\144@\002\005\245\225\000\001\023\005@\002\005\245\225\000\001\023\b\160\144\004\022@\208@(getlogin\160@\144\179@\160\176\001\007-\005\004\248@@\166\155\240-unix_getloginAA\005\004\247@@\144\176\193\005\004\246\176\179\005\004\188@\144@\002\005\245\225\000\001\023\213\176\179\005\004\212@\144@\002\005\245\225\000\001\023\216@\002\005\245\225\000\001\023\219\160\144\004\016@\208\208@+getnameinfo\160\176@\160\160B\144\160\176\001\006\029$addr@\160\176\001\006\030$opts@@@@@@A+getpeername\160@\144\179@\160\176\001\007\022\005\005\027@@\166\155\2400unix_getpeernameAA\005\005\026@@\144\176\193\005\005\025\176\179\005\005\024@\144@\002\005\245\225\000\001\025\004\176\179\005\005\014@\144@\002\005\245\225\000\001\025\007@\002\005\245\225\000\001\025\n\160\144\004\016@@BCDE&getpid\160@\144\179@\160\176\001\007\155\005\0050@@\166\155\240+unix_getpidAA\005\005/@@\144\176\193\005\005.\176\179\005\004\244@\144@\002\005\245\225\000\001\015\173\176\179\005\004\222@\144@\002\005\245\225\000\001\015\176@\002\005\245\225\000\001\015\179\160\144\004\016@\208\208\208@'getppid\160@\144\179@\160\176\001\007\154\005\005H@@\166\155\240,unix_getppidAA\005\005G@@\144\176\193\005\005F\176\179\005\005\012@\144@\002\005\245\225\000\001\015\180\176\179\005\004\246@\144@\002\005\245\225\000\001\015\183@\002\005\245\225\000\001\015\186\160\144\004\016@\208@.getprotobyname\160@\144\179@\160\176\001\007\018\005\005^@@\166\155\2403unix_getprotobynameAA\005\005]@@\144\176\193\005\005\\\176\179\005\0057@\144@\002\005\245\225\000\001\031\132\176\179\144\176\001\005\210.protocol_entry@@\144@\002\005\245\225\000\001\031\135@\002\005\245\225\000\001\031\138\160\144\004\019@\208@0getprotobynumber\160@\144\179@\160\176\001\007\017\005\005w@@\166\155\2405unix_getprotobynumberAA\005\005v@@\144\176\193\005\005u\176\179\005\005\"@\144@\002\005\245\225\000\001\031\139\176\179\004\025@\144@\002\005\245\225\000\001\031\142@\002\005\245\225\000\001\031\145\160\144\004\016@@ABC(getpwnam\160@\144\179@\160\176\001\007,\005\005\140@@\166\155\240-unix_getpwnamAA\005\005\139@@\144\176\193\005\005\138\176\179\005\005e@\144@\002\005\245\225\000\001\023\220\176\179\144\176\001\005\",passwd_entry@@\144@\002\005\245\225\000\001\023\223@\002\005\245\225\000\001\023\226\160\144\004\019@\208@(getpwuid\160@\144\179@\160\176\001\007*\005\005\165@@\166\155\240-unix_getpwuidAA\005\005\164@@\144\176\193\005\005\163\176\179\005\005P@\144@\002\005\245\225\000\001\023\234\176\179\004\025@\144@\002\005\245\225\000\001\023\237@\002\005\245\225\000\001\023\240\160\144\004\016@\208@-getservbyname\160@\144\179@\160\176\001\007\016\005\005\187@\160\176\001\007\015\005\005\189@@\166\155\2402unix_getservbynameBA\005\005\188@@\144\176\193\005\005\187\176\179\005\005\150@\144@\002\005\245\225\000\001\031\146\176\193\005\005\192\176\179\005\005\155@\144@\002\005\245\225\000\001\031\149\176\179\144\176\001\005\214-service_entry@@\144@\002\005\245\225\000\001\031\152@\002\005\245\225\000\001\031\155@\002\005\245\225\000\001\031\156\160\144\004\026\160\144\004\026@\208@-getservbyport\160@\144\179@\160\176\001\007\014\005\005\221@\160\176\001\007\r\005\005\223@@\166\155\2402unix_getservbyportBA\005\005\222@@\144\176\193\005\005\221\176\179\005\005\138@\144@\002\005\245\225\000\001\031\157\176\193\005\005\226\176\179\005\005\189@\144@\002\005\245\225\000\001\031\160\176\179\004\"@\144@\002\005\245\225\000\001\031\163@\002\005\245\225\000\001\031\166@\002\005\245\225\000\001\031\167\160\144\004\023\160\144\004\023@@ABCD+getsockname\160@\144\179@\160\176\001\007\023\005\005\251@@\166\155\2400unix_getsocknameAA\005\005\250@@\144\176\193\005\005\249\176\179\005\005\248@\144@\002\005\245\225\000\001\024\253\176\179\005\005\238@\144@\002\005\245\225\000\001\025\000@\002\005\245\225\000\001\025\003\160\144\004\016@\208\208@*getsockopt\160\176@\160\160B\144\160\176\001\005\176\"fd@\160\176\001\005\177#opt@@@@@\208@0getsockopt_error\160\176@\160\160A\144\160\176\001\005\204\"fd@@@@@@AB0getsockopt_float\160\176@\160\160B\144\160\176\001\005\197\"fd@\160\176\001\005\198#opt@@@@@\208@.getsockopt_int\160\176@\160\160B\144\160\176\001\005\183\"fd@\160\176\001\005\184#opt@@@@@\208@1getsockopt_optint\160\176@\160\160B\144\160\176\001\005\190\"fd@\160\176\001\005\191#opt@@@@@@ABCEF,gettimeofday\160@\144\179@\160\176\001\007D\005\006N@@\166\155\2401unix_gettimeofdayAA\005\006M@@\144\176\193\005\006L\176\179\005\006\018@\144@\002\005\245\225\000\001\022\166\176\179\144\176D%float@@\144@\002\005\245\225\000\001\022\169@\002\005\245\225\000\001\022\172\160\144\004\019@\208\208\208@&getuid\160@\144\179@\160\176\001\0077\005\006i@@\166\155\240+unix_getuidAA\005\006h@@\144\176\193\005\006g\176\179\005\006-@\144@\002\005\245\225\000\001\023\020\176\179\005\006\023@\144@\002\005\245\225\000\001\023\023@\002\005\245\225\000\001\023\026\160\144\004\016@@A&gmtime\160@\144\179@\160\176\001\007C\005\006~@@\166\155\240+unix_gmtimeAA\005\006}@@\144\176\193\005\006|\176\179\004-@\144@\002\005\245\225\000\001\022\173\176\179\144\176\001\004\253\"tm@@\144@\002\005\245\225\000\001\022\176@\002\005\245\225\000\001\022\179\160\144\004\019@@B1handle_unix_error\160\176@\160\160B\144\160\176\001\004>!f@\160\176\001\004?#arg@@@@@\208\208\208@3in_channel_of_descr\160@\144\179@\160\176\001\007\148\005\006\165@@\166\155\240:caml_ml_open_descriptor_inAA\005\006\164@@\144\176\193\005\006\163\176\179\005\006\162@\144@\002\005\245\225\000\001\018\228\176\179\005\004\215@\144@\002\005\245\225\000\001\018\231@\002\005\245\225\000\001\018\234\160\144\004\016@@A.inet6_addr_any\160\176@@@@\208\208@3inet6_addr_loopback\160\176@@@@@A-inet_addr_any\160\005\006\198@\208@2inet_addr_loopback\160\005\006\201@@ABC3inet_addr_of_string\160@\144\179@\160\176\001\007(\005\006\199@@\166\155\2408unix_inet_addr_of_stringAA\005\006\198@@\144\176\193\005\006\197\176\179\005\006\160@\144@\002\005\245\225\000\001\024\028\176\179\005\0023@\144@\002\005\245\225\000\001\024\031@\002\005\245\225\000\001\024\"\160\144\004\016@\208@*initgroups\160@\144\179@\160\176\001\007/\005\006\221@\160\176\001\007.\005\006\223@@\166\155\240/unix_initgroupsBA\005\006\222@@\144\176\193\005\006\221\176\179\005\006\184@\144@\002\005\245\225\000\001\023V\176\193\005\006\226\176\179\005\006\143@\144@\002\005\245\225\000\001\023Y\176\179\005\006\171@\144@\002\005\245\225\000\001\023\\@\002\005\245\225\000\001\023_@\002\005\245\225\000\001\023`\160\144\004\023\160\144\004\023@@ADEG&isatty\160@\144\179@\160\176\001\007\134\005\006\251@@\166\155\240+unix_isattyAA\005\006\250@@\144\176\193\005\006\249\176\179\005\006\248@\144@\002\005\245\225\000\001\019\175\176\179\144\176E$bool@@\144@\002\005\245\225\000\001\019\178@\002\005\245\225\000\001\019\181\160\144\004\019@\208\208\208\208@$kill\160@\144\179@\160\176\001\007K\005\007\023@\160\176\001\007J\005\007\025@@\166\155\240)unix_killBA\005\007\024@@\144\176\193\005\007\023\176\179\005\006\196@\144@\002\005\245\225\000\001\021\200\176\193\005\007\028\176\179\005\006\201@\144@\002\005\245\225\000\001\021\203\176\179\005\006\229@\144@\002\005\245\225\000\001\021\206@\002\005\245\225\000\001\021\209@\002\005\245\225\000\001\021\210\160\144\004\023\160\144\004\023@@A$link\160@\144\179@\160\176\001\007x\005\0075@\160\176\001\007w\005\0077@@\166\155\240)unix_linkBA\005\0076@@\144\176\193\005\0075\176\179\005\007\016@\144@\002\005\245\225\000\001\019\200\176\193\005\007:\176\179\005\007\021@\144@\002\005\245\225\000\001\019\203\176\179\005\007\003@\144@\002\005\245\225\000\001\019\206@\002\005\245\225\000\001\019\209@\002\005\245\225\000\001\019\210\160\144\004\023\160\144\004\023@\208\208\208@&listen\160@\144\179@\160\176\001\007\027\005\007V@\160\176\001\007\026\005\007X@@\166\155\240+unix_listenBA\005\007W@@\144\176\193\005\007V\176\179\005\007U@\144@\002\005\245\225\000\001\024\231\176\193\005\007[\176\179\005\007\b@\144@\002\005\245\225\000\001\024\234\176\179\005\007$@\144@\002\005\245\225\000\001\024\237@\002\005\245\225\000\001\024\240@\002\005\245\225\000\001\024\241\160\144\004\023\160\144\004\023@@A)localtime\160@\144\179@\160\176\001\007B\005\007t@@\166\155\240.unix_localtimeAA\005\007s@@\144\176\193\005\007r\176\179\005\001#@\144@\002\005\245\225\000\001\022\180\176\179\004\246@\144@\002\005\245\225\000\001\022\183@\002\005\245\225\000\001\022\186\160\144\004\016@@B%lockf\160@\144\179@\160\176\001\007N\005\007\137@\160\176\001\007M\005\007\139@\160\176\001\007L\005\007\141@@\166\155\240*unix_lockfCA\005\007\140@@\144\176\193\005\007\139\176\179\005\007\138@\144@\002\005\245\225\000\001\021\185\176\193\005\007\144\176\179\144\176\001\004\230,lock_command@@\144@\002\005\245\225\000\001\021\188\176\193\005\007\152\176\179\005\007E@\144@\002\005\245\225\000\001\021\191\176\179\005\007a@\144@\002\005\245\225\000\001\021\194@\002\005\245\225\000\001\021\197@\002\005\245\225\000\001\021\198@\002\005\245\225\000\001\021\199\160\144\004!\160\144\004!\160\144\004!@@CD%lseek\160@\144\179@\160\176\001\007\144\005\007\179@\160\176\001\007\143\005\007\181@\160\176\001\007\142\005\007\183@@\166\155\240*unix_lseekCA\005\007\182@@\144\176\193\005\007\181\176\179\005\007\180@\144@\002\005\245\225\000\001\019\003\176\193\005\007\186\176\179\005\007g@\144@\002\005\245\225\000\001\019\006\176\193\005\007\191\176\179\144\176\001\004\141,seek_command@@\144@\002\005\245\225\000\001\019\t\176\179\005\007r@\144@\002\005\245\225\000\001\019\012@\002\005\245\225\000\001\019\015@\002\005\245\225\000\001\019\016@\002\005\245\225\000\001\019\017\160\144\004!\160\144\004!\160\144\004!@\208\208@%lstat\160@\144\179@\160\176\001\007\136\005\007\223@@\166\155\240*unix_lstatAA\005\007\222@@\144\176\193\005\007\221\176\179\005\007\184@\144@\002\005\245\225\000\001\019\161\176\179\005\004L@\144@\002\005\245\225\000\001\019\164@\002\005\245\225\000\001\019\167\160\144\004\016@@A%mkdir\160@\144\179@\160\176\001\007b\005\007\244@\160\176\001\007a\005\007\246@@\166\155\240*unix_mkdirBA\005\007\245@@\144\176\193\005\007\244\176\179\005\007\207@\144@\002\005\245\225\000\001\021\019\176\193\005\007\249\176\179\005\007S@\144@\002\005\245\225\000\001\021\022\176\179\005\007\194@\144@\002\005\245\225\000\001\021\025@\002\005\245\225\000\001\021\028@\002\005\245\225\000\001\021\029\160\144\004\023\160\144\004\023@\208@&mkfifo\160@\144\179@\160\176\001\007W\005\b\019@\160\176\001\007V\005\b\021@@\166\155\240+unix_mkfifoBA\005\b\020@@\144\176\193\005\b\019\176\179\005\007\238@\144@\002\005\245\225\000\001\021s\176\193\005\b\024\176\179\005\007r@\144@\002\005\245\225\000\001\021v\176\179\005\007\225@\144@\002\005\245\225\000\001\021y@\002\005\245\225\000\001\021|@\002\005\245\225\000\001\021}\160\144\004\023\160\144\004\023@\208@&mktime\160@\144\179@\160\176\001\007A\005\b2@@\166\155\240+unix_mktimeAA\005\b1@@\144\176\193\005\b0\176\179\005\001\177@\144@\002\005\245\225\000\001\022\187\176\146\160\176\179\005\001\231@\144@\002\005\245\225\000\001\022\190\160\176\179\005\001\187@\144@\002\005\245\225\000\001\022\193@\002\005\245\225\000\001\022\196@\002\005\245\225\000\001\022\197\160\144\004\023@@ABCE$nice\160@\144\179@\160\176\001\007\153\005\bN@@\166\155\240)unix_niceAA\005\bM@@\144\176\193\005\bL\176\179\005\007\249@\144@\002\005\245\225\000\001\015\187\176\179\005\007\252@\144@\002\005\245\225\000\001\015\190@\002\005\245\225\000\001\015\193\160\144\004\016@\208\208\208\208@/open_connection\160\176A\160\160A\144\160\176\001\006\241(sockaddr@@@@@@A,open_process\160\176A\160\160A\144\160\176\001\006\188#cmd@@@@@\208@1open_process_full\160\176A\160\160B\144\160\176\001\006\208#cmd@\160\176\001\006\209#env@@@@@@AB/open_process_in\160\176@\160\160A\144\160\176\001\006\176#cmd@@@@@\208\208@0open_process_out\160\176@\160\160A\144\160\176\001\006\182#cmd@@@@@@A'opendir\160@\144\179@\160\176\001\007\\\005\b\154@@\166\155\240,unix_opendirAA\005\b\153@@\144\176\193\005\b\152\176\179\005\bs@\144@\002\005\245\225\000\001\021:\176\179\005\0072@\144@\002\005\245\225\000\001\021=@\002\005\245\225\000\001\021@\160\144\004\016@@BC(openfile\160@\144\179@\160\176\001\007\152\005\b\175@\160\176\001\007\151\005\b\177@\160\176\001\007\150\005\b\179@@\166\155\240)unix_openCA\005\b\178@@\144\176\193\005\b\177\176\179\005\b\140@\144@\002\005\245\225\000\001\015\229\176\193\005\b\182\176\179\005\b\137\160\176\179\144\176\001\004[)open_flag@@\144@\002\005\245\225\000\001\015\232@\144@\002\005\245\225\000\001\015\236\176\193\005\b\194\176\179\005\b\028@\144@\002\005\245\225\000\001\015\240\176\179\005\b\196@\144@\002\005\245\225\000\001\015\243@\002\005\245\225\000\001\015\246@\002\005\245\225\000\001\015\247@\002\005\245\225\000\001\015\248\160\144\004%\160\144\004%\160\144\004%@\208@4out_channel_of_descr\160@\144\179@\160\176\001\007\147\005\b\222@@\166\155\240;caml_ml_open_descriptor_outAA\005\b\221@@\144\176\193\005\b\220\176\179\005\b\219@\144@\002\005\245\225\000\001\018\235\176\179\005\006\246@\144@\002\005\245\225\000\001\018\238@\002\005\245\225\000\001\018\241\160\144\004\016@\208\208@%pause\160\176@\160\160A\144\160\176\001\b\007\005\007\176@@@@\144\179@\004\004\166\155\240/unix_sigsuspendAA\005\b\248@@\144\176\193\005\b\247\176\179\005\b\202\160\176\179\005\b\167@\144@\002\005\245\225\000\001\021\247@\144@\002\005\245\225\000\001\021\251\176\179\005\b\196@\144@\002\005\245\225\000\001\021\255@\002\005\245\225\000\001\022\002\160\166\155\2400unix_sigprocmaskBA\005\t\n@@\144\176\193\005\t\t\176\179\144\176\001\004\2393sigprocmask_command@@\144@\002\005\245\225\000\001\021\214\176\193\005\t\017\176\179\005\b\228\160\176\179\005\b\193@\144@\002\005\245\225\000\001\021\217@\144@\002\005\245\225\000\001\021\221\176\179\005\b\235\160\176\179\005\b\200@\144@\002\005\245\225\000\001\021\225@\144@\002\005\245\225\000\001\021\229@\002\005\245\225\000\001\021\233@\002\005\245\225\000\001\021\234\160\145\161A\144)SIG_BLOCK\160\145\161@\144\"[]@@@A$pipe\160@\144\179@\160\176\001\007X\005\t8@@\166\155\240)unix_pipeAA\005\t7@@\144\176\193\005\t6\176\179\005\b\252@\144@\002\005\245\225\000\001\021V\176\146\160\176\179\005\t;@\144@\002\005\245\225\000\001\021Y\160\176\179\005\t?@\144@\002\005\245\225\000\001\021\\@\002\005\245\225\000\001\021_@\002\005\245\225\000\001\021`\160\144\004\023@@BCDFH&putenv\160@\144\179@\160\176\001\007\171\005\tT@\160\176\001\007\170\005\tV@@\166\155\240+unix_putenvBA\005\tU@@\144\176\193\005\tT\176\179\005\t/@\144@\002\005\245\225\000\001\015\015\176\193\005\tY\176\179\005\t4@\144@\002\005\245\225\000\001\015\018\176\179\005\t\"@\144@\002\005\245\225\000\001\015\021@\002\005\245\225\000\001\015\024@\002\005\245\225\000\001\015\025\160\144\004\023\160\144\004\023@\208\208\208\208\208@$read\160\176@\160\160D\144\160\176\001\004q\"fd@\160\176\001\004r#buf@\160\176\001\004s#ofs@\160\176\001\004t#len@@@@@@A'readdir\160@\144\179@\160\176\001\007[\005\t\137@@\166\155\240,unix_readdirAA\005\t\136@@\144\176\193\005\t\135\176\179\005\b\030@\144@\002\005\245\225\000\001\021A\176\179\005\te@\144@\002\005\245\225\000\001\021D@\002\005\245\225\000\001\021G\160\144\004\016@\208@(readlink\160@\144\179@\160\176\001\007S\005\t\159@@\166\155\240-unix_readlinkAA\005\t\158@@\144\176\193\005\t\157\176\179\005\tx@\144@\002\005\245\225\000\001\021l\176\179\005\t{@\144@\002\005\245\225\000\001\021o@\002\005\245\225\000\001\021r\160\144\004\016@\208@$recv\160\176@\160\160E\144\160\176\001\005a\"fd@\160\176\001\005b#buf@\160\176\001\005c#ofs@\160\176\001\005d#len@\160\176\001\005e%flags@@@@@\208@(recvfrom\160\176@\160\160E\144\160\176\001\005g\"fd@\160\176\001\005h#buf@\160\176\001\005i#ofs@\160\176\001\005j#len@\160\176\001\005k%flags@@@@@@ABCD&rename\160@\144\179@\160\176\001\007z\005\t\224@\160\176\001\007y\005\t\226@@\166\155\240+unix_renameBA\005\t\225@@\144\176\193\005\t\224\176\179\005\t\187@\144@\002\005\245\225\000\001\019\189\176\193\005\t\229\176\179\005\t\192@\144@\002\005\245\225\000\001\019\192\176\179\005\t\174@\144@\002\005\245\225\000\001\019\195@\002\005\245\225\000\001\019\198@\002\005\245\225\000\001\019\199\160\144\004\023\160\144\004\023@\208\208\208@)rewinddir\160@\144\179@\160\176\001\007Z\005\n\001@@\166\155\240.unix_rewinddirAA\005\n\000@@\144\176\193\005\t\255\176\179\005\b\150@\144@\002\005\245\225\000\001\021H\176\179\005\t\200@\144@\002\005\245\225\000\001\021K@\002\005\245\225\000\001\021N\160\144\004\016@@A%rmdir\160@\144\179@\160\176\001\007`\005\n\022@@\166\155\240*unix_rmdirAA\005\n\021@@\144\176\193\005\n\020\176\179\005\t\239@\144@\002\005\245\225\000\001\021\030\176\179\005\t\221@\144@\002\005\245\225\000\001\021!@\002\005\245\225\000\001\021$\160\144\004\016@\208@&select\160@@@AB$send\160\176@\160\160E\144\160\176\001\005m\"fd@\160\176\001\005n#buf@\160\176\001\005o#ofs@\160\176\001\005p#len@\160\176\001\005q%flags@@@@@\208@.send_substring\160\176@\160\160E\144\160\176\001\005z\"fd@\160\176\001\005{#buf@\160\176\001\005|#ofs@\160\176\001\005}#len@\160\176\001\005~%flags@@@@@\208@&sendto\160\176@\160\160F\144\160\176\001\005s\"fd@\160\176\001\005t#buf@\160\176\001\005u#ofs@\160\176\001\005v#len@\160\176\001\005w%flags@\160\176\001\005x$addr@@@@@\208@0sendto_substring\160\176@\160\160F\144\160\176\001\005\128\"fd@\160\176\001\005\129#buf@\160\176\001\005\130#ofs@\160\176\001\005\131#len@\160\176\001\005\132%flags@\160\176\001\005\133$addr@@@@@@ABCDE1set_close_on_exec\160@\144\179@\160\176\001\007d\005\n\139@@\166\155\2406unix_set_close_on_execAA\005\n\138@@\144\176\193\005\n\137\176\179\005\n\136@\144@\002\005\245\225\000\001\020\234\176\179\005\nR@\144@\002\005\245\225\000\001\020\237@\002\005\245\225\000\001\020\240\160\144\004\016@\208\208\208\208\208@,set_nonblock\160@\144\179@\160\176\001\007f\005\n\165@@\166\155\2401unix_set_nonblockAA\005\n\164@@\144\176\193\005\n\163\176\179\005\n\162@\144@\002\005\245\225\000\001\020\220\176\179\005\nl@\144@\002\005\245\225\000\001\020\223@\002\005\245\225\000\001\020\226\160\144\004\016@@A&setgid\160@\144\179@\160\176\001\0072\005\n\186@@\166\155\240+unix_setgidAA\005\n\185@@\144\176\193\005\n\184\176\179\005\ne@\144@\002\005\245\225\000\001\0237\176\179\005\n\129@\144@\002\005\245\225\000\001\023:@\002\005\245\225\000\001\023=\160\144\004\016@\208@)setgroups\160@\144\179@\160\176\001\0070\005\n\208@@\166\155\240.unix_setgroupsAA\005\n\207@@\144\176\193\005\n\206\176\179\005\b\140\160\176\179\005\n~@\144@\002\005\245\225\000\001\023J@\144@\002\005\245\225\000\001\023N\176\179\005\n\155@\144@\002\005\245\225\000\001\023R@\002\005\245\225\000\001\023U\160\144\004\020@@AB)setitimer\160@\144\179@\160\176\001\0079\005\n\233@\160\176\001\0078\005\n\235@@\166\155\240.unix_setitimerBA\005\n\234@@\144\176\193\005\n\233\176\179\005\006\015@\144@\002\005\245\225\000\001\023\t\176\193\005\n\238\176\179\005\006\014@\144@\002\005\245\225\000\001\023\012\176\179\005\006\017@\144@\002\005\245\225\000\001\023\015@\002\005\245\225\000\001\023\018@\002\005\245\225\000\001\023\019\160\144\004\023\160\144\004\023@\208\208@&setsid\160@\144\179@\160\176\001\007\001\005\011\t@@\166\155\240+unix_setsidAA\005\011\b@@\144\176\193\005\011\007\176\179\005\n\205@\144@\002\005\245\225\000\001&&\176\179\005\n\183@\144@\002\005\245\225\000\001&)@\002\005\245\225\000\001&,\160\144\004\016@@A*setsockopt\160\176@\160\160C\144\160\176\001\005\179\"fd@\160\176\001\005\180#opt@\160\176\001\005\181!v@@@@@\208\208@0setsockopt_float\160\176@\160\160C\144\160\176\001\005\200\"fd@\160\176\001\005\201#opt@\160\176\001\005\202!v@@@@@@A.setsockopt_int\160\176@\160\160C\144\160\176\001\005\186\"fd@\160\176\001\005\187#opt@\160\176\001\005\188!v@@@@@\208@1setsockopt_optint\160\176@\160\160C\144\160\176\001\005\193\"fd@\160\176\001\005\194#opt@\160\176\001\005\195!v@@@@@@ABCD&setuid\160@\144\179@\160\176\001\0075\005\011]@@\166\155\240+unix_setuidAA\005\011\\@@\144\176\193\005\011[\176\179\005\011\b@\144@\002\005\245\225\000\001\023\"\176\179\005\011$@\144@\002\005\245\225\000\001\023%@\002\005\245\225\000\001\023(\160\144\004\016@\208\208@(shutdown\160@\144\179@\160\176\001\007\025\005\011t@\160\176\001\007\024\005\011v@@\166\155\240-unix_shutdownBA\005\011u@@\144\176\193\005\011t\176\179\005\011s@\144@\002\005\245\225\000\001\024\242\176\193\005\011y\176\179\144\176\001\005K0shutdown_command@@\144@\002\005\245\225\000\001\024\245\176\179\005\011E@\144@\002\005\245\225\000\001\024\248@\002\005\245\225\000\001\024\251@\002\005\245\225\000\001\024\252\160\144\004\026\160\144\004\026@\208@3shutdown_connection\160\176@\160\160A\144\160\176\001\006\245&inchan@@@@\144\179@\004\005\166\155\004%\160\166\155\005\t\205\160\144\004\011@\160\145\161A\144-SHUTDOWN_SEND@@AB*sigpending\160@\144\179@\160\176\001\007G\005\011\173@@\166\155\240/unix_sigpendingAA\005\011\172@@\144\176\193\005\011\171\176\179\005\011q@\144@\002\005\245\225\000\001\021\235\176\179\005\011\129\160\176\179\005\011^@\144@\002\005\245\225\000\001\021\238@\144@\002\005\245\225\000\001\021\242@\002\005\245\225\000\001\021\246\160\144\004\020@\208@+sigprocmask\160@\144\179@\160\176\001\007I\005\011\199@\160\176\001\007H\005\011\201@@\166\155\005\002\190\160\144\004\007\160\144\004\007@\208@*sigsuspend\160@\144\179@\160\176\001\007F\005\011\214@@\166\155\005\002\221\160\144\004\005@@ABCE,single_write\160\176@\160\160D\144\160\176\001\004{\"fd@\160\176\001\004|#buf@\160\176\001\004}#ofs@\160\176\001\004~#len@@@@@\208\208\208@6single_write_substring\160\176@\160\160D\144\160\176\001\004\133\"fd@\160\176\001\004\134#buf@\160\176\001\004\135#ofs@\160\176\001\004\136#len@@@@@@A%sleep\160@\144\179@\160\176\001\007?\005\012\007@@\166\155\240*unix_sleepAA\005\012\006@@\144\176\193\005\012\005\176\179\005\011\178@\144@\002\005\245\225\000\001\022\205\176\179\005\011\206@\144@\002\005\245\225\000\001\022\208@\002\005\245\225\000\001\022\211\160\144\004\016@\208@&socket\160@\144\179@\160\176\001\007&\005\012\029@\160\176\001\007%\005\012\031@\160\176\001\007$\005\012!@@\166\155\240+unix_socketCA\005\012 @@\144\176\193\005\012\031\176\179\144\176\001\005=-socket_domain@@\144@\002\005\245\225\000\001\024\164\176\193\005\012'\176\179\144\176\001\005A+socket_type@@\144@\002\005\245\225\000\001\024\167\176\193\005\012/\176\179\005\011\220@\144@\002\005\245\225\000\001\024\170\176\179\005\0121@\144@\002\005\245\225\000\001\024\173@\002\005\245\225\000\001\024\176@\002\005\245\225\000\001\024\177@\002\005\245\225\000\001\024\178\160\144\004$\160\144\004$\160\144\004$@\208@*socketpair\160@\144\179@\160\176\001\007#\005\012K@\160\176\001\007\"\005\012M@\160\176\001\007!\005\012O@@\166\155\240/unix_socketpairCA\005\012N@@\144\176\193\005\012M\176\179\004.@\144@\002\005\245\225\000\001\024\179\176\193\005\012R\176\179\004+@\144@\002\005\245\225\000\001\024\182\176\193\005\012W\176\179\005\012\004@\144@\002\005\245\225\000\001\024\185\176\146\160\176\179\005\012\\@\144@\002\005\245\225\000\001\024\188\160\176\179\005\012`@\144@\002\005\245\225\000\001\024\191@\002\005\245\225\000\001\024\194@\002\005\245\225\000\001\024\195@\002\005\245\225\000\001\024\196@\002\005\245\225\000\001\024\197\160\144\004%\160\144\004%\160\144\004%@@ABC$stat\160@\144\179@\160\176\001\007\137\005\012y@@\166\155\240)unix_statAA\005\012x@@\144\176\193\005\012w\176\179\005\012R@\144@\002\005\245\225\000\001\019\154\176\179\005\b\230@\144@\002\005\245\225\000\001\019\157@\002\005\245\225\000\001\019\160\160\144\004\016@\208@&stderr\160@@@ADFG%stdin\160@@\208\208\208@&stdout\160@@\208\208@3string_of_inet_addr\160@\144\179@\160\176\001\007'\005\012\154@@\166\155\2408unix_string_of_inet_addrAA\005\012\153@@\144\176\193\005\012\152\176\179\005\b\003@\144@\002\005\245\225\000\001\024#\176\179\005\012v@\144@\002\005\245\225\000\001\024&@\002\005\245\225\000\001\024)\160\144\004\016@@A'symlink\160@\144\179@\160\176\001\007U\005\012\175@\160\176\001\007T\005\012\177@@\166\155\240,unix_symlinkBA\005\012\176@@\144\176\193\005\012\175\176\179\005\012\138@\144@\002\005\245\225\000\001\021a\176\193\005\012\180\176\179\005\012\143@\144@\002\005\245\225\000\001\021d\176\179\005\012}@\144@\002\005\245\225\000\001\021g@\002\005\245\225\000\001\021j@\002\005\245\225\000\001\021k\160\144\004\023\160\144\004\023@@BC&system\160\176@\160\160A\144\160\176\001\006]#cmd@@@@@\208\208@'tcdrain\160@\144\179@\160\176\001\007\006\005\012\216@@\166\155\240,unix_tcdrainAA\005\012\215@@\144\176\193\005\012\214\176\179\005\012\213@\144@\002\005\245\225\000\001&\003\176\179\005\012\159@\144@\002\005\245\225\000\001&\006@\002\005\245\225\000\001&\t\160\144\004\016@\208\208@&tcflow\160@\144\179@\160\176\001\007\003\005\012\239@\160\176\001\007\002\005\012\241@@\166\155\240+unix_tcflowBA\005\012\240@@\144\176\193\005\012\239\176\179\005\012\238@\144@\002\005\245\225\000\001&\027\176\193\005\012\244\176\179\144\176\001\006S+flow_action@@\144@\002\005\245\225\000\001&\030\176\179\005\012\192@\144@\002\005\245\225\000\001&!@\002\005\245\225\000\001&$@\002\005\245\225\000\001&%\160\144\004\026\160\144\004\026@@A'tcflush\160@\144\179@\160\176\001\007\005\005\r\016@\160\176\001\007\004\005\r\018@@\166\155\240,unix_tcflushBA\005\r\017@@\144\176\193\005\r\016\176\179\005\r\015@\144@\002\005\245\225\000\001&\r\176\193\005\r\021\176\179\144\176\001\006N+flush_queue@@\144@\002\005\245\225\000\001&\016\176\179\005\012\225@\144@\002\005\245\225\000\001&\019@\002\005\245\225\000\001&\022@\002\005\245\225\000\001&\023\160\144\004\026\160\144\004\026@@BC)tcgetattr\160@\144\179@\160\176\001\007\012\005\r1@@\166\155\240.unix_tcgetattrAA\005\r0@@\144\176\193\005\r/\176\179\005\r.@\144@\002\005\245\225\000\001%\223\176\179\144\176\001\006\031+terminal_io@@\144@\002\005\245\225\000\001%\226@\002\005\245\225\000\001%\229\160\144\004\019@\208\208\208@+tcsendbreak\160@\144\179@\160\176\001\007\b\005\rL@\160\176\001\007\007\005\rN@@\166\155\2400unix_tcsendbreakBA\005\rM@@\144\176\193\005\rL\176\179\005\rK@\144@\002\005\245\225\000\001%\248\176\193\005\rQ\176\179\005\012\254@\144@\002\005\245\225\000\001%\251\176\179\005\r\026@\144@\002\005\245\225\000\001%\254@\002\005\245\225\000\001&\001@\002\005\245\225\000\001&\002\160\144\004\023\160\144\004\023@@A)tcsetattr\160@\144\179@\160\176\001\007\011\005\rj@\160\176\001\007\n\005\rl@\160\176\001\007\t\005\rn@@\166\155\240.unix_tcsetattrCA\005\rm@@\144\176\193\005\rl\176\179\005\rk@\144@\002\005\245\225\000\001%\233\176\193\005\rq\176\179\144\176\001\006G,setattr_when@@\144@\002\005\245\225\000\001%\236\176\193\005\ry\176\179\004G@\144@\002\005\245\225\000\001%\239\176\179\005\rB@\144@\002\005\245\225\000\001%\242@\002\005\245\225\000\001%\245@\002\005\245\225\000\001%\246@\002\005\245\225\000\001%\247\160\144\004!\160\144\004!\160\144\004!@@B$time\160@\144\179@\160\176\001\007E\005\r\148@@\166\155\240)unix_timeAA\005\r\147@@\144\176\193\005\r\146\176\179\005\rX@\144@\002\005\245\225\000\001\022\159\176\179\005\007F@\144@\002\005\245\225\000\001\022\162@\002\005\245\225\000\001\022\165\160\144\004\016@\208@%times\160@\144\179@\160\176\001\007>\005\r\170@@\166\155\240*unix_timesAA\005\r\169@@\144\176\193\005\r\168\176\179\005\rn@\144@\002\005\245\225\000\001\022\212\176\179\144\176\001\004\248-process_times@@\144@\002\005\245\225\000\001\022\215@\002\005\245\225\000\001\022\218\160\144\004\019@@ACDE(truncate\160@\144\179@\160\176\001\007\141\005\r\194@\160\176\001\007\140\005\r\196@@\166\155\240-unix_truncateBA\005\r\195@@\144\176\193\005\r\194\176\179\005\r\157@\144@\002\005\245\225\000\001\019\018\176\193\005\r\199\176\179\005\rt@\144@\002\005\245\225\000\001\019\021\176\179\005\r\144@\144@\002\005\245\225\000\001\019\024@\002\005\245\225\000\001\019\027@\002\005\245\225\000\001\019\028\160\144\004\023\160\144\004\023@\208\208\208@%umask\160@\144\179@\160\176\001\007l\005\r\227@@\166\155\240*unix_umaskAA\005\r\226@@\144\176\193\005\r\225\176\179\005\r\142@\144@\002\005\245\225\000\001\020\179\176\179\005\r\145@\144@\002\005\245\225\000\001\020\182@\002\005\245\225\000\001\020\185\160\144\004\016@@A&unlink\160@\144\179@\160\176\001\007{\005\r\248@@\166\155\240+unix_unlinkAA\005\r\247@@\144\176\193\005\r\246\176\179\005\r\209@\144@\002\005\245\225\000\001\019\182\176\179\005\r\191@\144@\002\005\245\225\000\001\019\185@\002\005\245\225\000\001\019\188\160\144\004\016@\208@&utimes\160@\144\179@\160\176\001\007=\005\014\014@\160\176\001\007<\005\014\016@\160\176\001\007;\005\014\018@@\166\155\240+unix_utimesCA\005\014\017@@\144\176\193\005\014\016\176\179\005\r\235@\144@\002\005\245\225\000\001\022\219\176\193\005\014\021\176\179\005\007\198@\144@\002\005\245\225\000\001\022\222\176\193\005\014\026\176\179\005\007\203@\144@\002\005\245\225\000\001\022\225\176\179\005\r\227@\144@\002\005\245\225\000\001\022\228@\002\005\245\225\000\001\022\231@\002\005\245\225\000\001\022\232@\002\005\245\225\000\001\022\233\160\144\004\030\160\144\004\030\160\144\004\030@@AB$wait\160@\144\179@\160\176\001\007\158\005\0145@@\166\155\240)unix_waitAA\005\0144@@\144\176\193\005\0143\176\179\005\r\249@\144@\002\005\245\225\000\001\015\142\176\146\160\176\179\005\r\230@\144@\002\005\245\225\000\001\015\145\160\176\179\144\176\001\004F.process_status@@\144@\002\005\245\225\000\001\015\148@\002\005\245\225\000\001\015\151@\002\005\245\225\000\001\015\152\160\144\004\026@\208@'waitpid\160@\144\179@\160\176\001\007\157\005\014U@\160\176\001\007\156\005\014W@@\166\155\240,unix_waitpidBA\005\014V@@\144\176\193\005\014U\176\179\005\014(\160\176\179\144\176\001\004J)wait_flag@@\144@\002\005\245\225\000\001\015\153@\144@\002\005\245\225\000\001\015\157\176\193\005\014a\176\179\005\014\014@\144@\002\005\245\225\000\001\015\161\176\146\160\176\179\005\014\020@\144@\002\005\245\225\000\001\015\164\160\176\179\004.@\144@\002\005\245\225\000\001\015\167@\002\005\245\225\000\001\015\170@\002\005\245\225\000\001\015\171@\002\005\245\225\000\001\015\172\160\144\004%\160\144\004%@\208@%write\160\176@\160\160D\144\160\176\001\004v\"fd@\160\176\001\004w#buf@\160\176\001\004x#ofs@\160\176\001\004y#len@@@@@\208@/write_substring\160\176@\160\160D\144\160\176\001\004\128\"fd@\160\176\001\004\129#buf@\160\176\001\004\130#ofs@\160\176\001\004\131#len@@@@@@ABCDFHIJ\144 @")));
            ("unixLabels.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\016\002\000\000\003\189\000\000\r\227\000\000\012\244\176\208\208\208\208\208\208\208@)LargeFile\160@@@A*Unix_error\160\176@@@@\208@&accept\160@@@AB&access\160@@\208\208@%alarm\160@@\208@$bind\160@@@AB%chdir\160@@@CD%chmod\160@@\208\208@%chown\160@@\208@&chroot\160@@@AB3clear_close_on_exec\160@@\208@.clear_nonblock\160@@@ACE%close\160@@\208\208\208\208@-close_process\160\176@\160\160A\144\160\176\001\007\186%param@@@@@\208@2close_process_full\160\176@\160\160A\144\160\176\001\007\182\004\n@@@@@@AB0close_process_in\160\176@\160\160A\144\160\176\001\006\226&inchan@@@@@\208\208@1close_process_out\160\176@\160\160A\144\160\176\001\006\229'outchan@@@@@@A(closedir\160@@\208@'connect\160@@@ABC.create_process\160\176@\160\160E\144\160\176\001\006m#cmd@\160\176\001\006n$args@\160\176\001\006o)new_stdin@\160\176\001\006p*new_stdout@\160\176\001\006q*new_stderr@@@@@\208\208@2create_process_env\160\176@\160\160F\144\160\176\001\006t#cmd@\160\176\001\006u$args@\160\176\001\006v#env@\160\176\001\006w)new_stdin@\160\176\001\006x*new_stdout@\160\176\001\006y*new_stderr@@@@@@A3descr_of_in_channel\160@@@BD4descr_of_out_channel\160@@\208\208\208@2domain_of_sockaddr\160\176A\160\160A\144\160\176\001\007\254\004a@@@@@@A#dup\160@@\208@$dup2\160@@@AB+environment\160@@\208@-error_message\160@@\208\208@0establish_server\160\176A\160\160B\144\160\176\001\006\249*server_fun@\160\176\001\006\250(sockaddr@@@@@@A%execv\160@@@BCDEF&execve\160@@\208\208\208@&execvp\160@@@A'execvpe\160@@\208@&fchmod\160@@\208@&fchown\160@@@ABC$fork\160@@\208\208@%fstat\160@@@A)ftruncate\160@@\208\208@+getaddrinfo\160\176@\160\160C\144\160\176\001\006\006$node@\160\176\001\006\007'service@\160\176\001\006\b$opts@@@@@@A&getcwd\160@@\208@'getegid\160@@@ABCDG&getenv\160@@\208\208\208\208\208\208\208@'geteuid\160@@@A&getgid\160@@\208\208@(getgrgid\160@@@A(getgrnam\160@@@BC)getgroups\160@@\208\208\208\208@-gethostbyaddr\160@@@A-gethostbyname\160@@@B+gethostname\160@@@C)getitimer\160@@\208@(getlogin\160@@\208\208@+getnameinfo\160\176@\160\160B\144\160\176\001\006\029$addr@\160\176\001\006\030$opts@@@@@@A+getpeername\160@@@BCDE&getpid\160@@\208\208\208@'getppid\160@@\208@.getprotobyname\160@@\208@0getprotobynumber\160@@@ABC(getpwnam\160@@\208@(getpwuid\160@@\208@-getservbyname\160@@\208@-getservbyport\160@@@ABCD+getsockname\160@@\208\208@*getsockopt\160\176@\160\160B\144\160\176\001\005\176\"fd@\160\176\001\005\177#opt@@@@@\208@0getsockopt_error\160\176@\160\160A\144\160\176\001\005\204\"fd@@@@@@AB0getsockopt_float\160\176@\160\160B\144\160\176\001\005\197\"fd@\160\176\001\005\198#opt@@@@@\208@.getsockopt_int\160\176@\160\160B\144\160\176\001\005\183\"fd@\160\176\001\005\184#opt@@@@@\208@1getsockopt_optint\160\176@\160\160B\144\160\176\001\005\190\"fd@\160\176\001\005\191#opt@@@@@@ABCEF,gettimeofday\160@@\208\208\208@&getuid\160@@@A&gmtime\160@@@B1handle_unix_error\160\176@\160\160B\144\160\176\001\004>!f@\160\176\001\004?#arg@@@@@\208\208\208@3in_channel_of_descr\160@@@A.inet6_addr_any\160\176@@@@\208\208@3inet6_addr_loopback\160\176@@@@@A-inet_addr_any\160\005\001\131@\208@2inet_addr_loopback\160\005\001\134@@ABC3inet_addr_of_string\160@@\208@*initgroups\160@@@ADEG&isatty\160@@\208\208\208\208@$kill\160@@@A$link\160@@\208\208\208@&listen\160@@@A)localtime\160@@@B%lockf\160@@@CD%lseek\160@@\208\208@%lstat\160@@@A%mkdir\160@@\208@&mkfifo\160@@\208@&mktime\160@@@ABCE$nice\160@@\208\208\208\208@/open_connection\160\176A\160\160A\144\160\176\001\006\241(sockaddr@@@@@@A,open_process\160\176A\160\160A\144\160\176\001\006\188#cmd@@@@@\208@1open_process_full\160\176A\160\160B\144\160\176\001\006\208#cmd@\160\176\001\006\209#env@@@@@@AB/open_process_in\160\176@\160\160A\144\160\176\001\006\176#cmd@@@@@\208\208@0open_process_out\160\176@\160\160A\144\160\176\001\006\182#cmd@@@@@@A'opendir\160@@@BC(openfile\160@@\208@4out_channel_of_descr\160@@\208\208@%pause\160\176@\160\160A\144\160\176\001\b\007\005\001\203@@@@@@A$pipe\160@@@BCDFH&putenv\160@@\208\208\208\208\208@$read\160\176@\160\160D\144\160\176\001\004q\"fd@\160\176\001\004r#buf@\160\176\001\004s#ofs@\160\176\001\004t#len@@@@@@A'readdir\160@@\208@(readlink\160@@\208@$recv\160\176@\160\160E\144\160\176\001\005a\"fd@\160\176\001\005b#buf@\160\176\001\005c#ofs@\160\176\001\005d#len@\160\176\001\005e%flags@@@@@\208@(recvfrom\160\176@\160\160E\144\160\176\001\005g\"fd@\160\176\001\005h#buf@\160\176\001\005i#ofs@\160\176\001\005j#len@\160\176\001\005k%flags@@@@@@ABCD&rename\160@@\208\208\208@)rewinddir\160@@@A%rmdir\160@@\208@&select\160@@@AB$send\160\176@\160\160E\144\160\176\001\005m\"fd@\160\176\001\005n#buf@\160\176\001\005o#ofs@\160\176\001\005p#len@\160\176\001\005q%flags@@@@@\208@.send_substring\160\176@\160\160E\144\160\176\001\005z\"fd@\160\176\001\005{#buf@\160\176\001\005|#ofs@\160\176\001\005}#len@\160\176\001\005~%flags@@@@@\208@&sendto\160\176@\160\160F\144\160\176\001\005s\"fd@\160\176\001\005t#buf@\160\176\001\005u#ofs@\160\176\001\005v#len@\160\176\001\005w%flags@\160\176\001\005x$addr@@@@@\208@0sendto_substring\160\176@\160\160F\144\160\176\001\005\128\"fd@\160\176\001\005\129#buf@\160\176\001\005\130#ofs@\160\176\001\005\131#len@\160\176\001\005\132%flags@\160\176\001\005\133$addr@@@@@@ABCDE1set_close_on_exec\160@@\208\208\208\208\208@,set_nonblock\160@@@A&setgid\160@@\208@)setgroups\160@@@AB)setitimer\160@@\208\208@&setsid\160@@@A*setsockopt\160\176@\160\160C\144\160\176\001\005\179\"fd@\160\176\001\005\180#opt@\160\176\001\005\181!v@@@@@\208\208@0setsockopt_float\160\176@\160\160C\144\160\176\001\005\200\"fd@\160\176\001\005\201#opt@\160\176\001\005\202!v@@@@@@A.setsockopt_int\160\176@\160\160C\144\160\176\001\005\186\"fd@\160\176\001\005\187#opt@\160\176\001\005\188!v@@@@@\208@1setsockopt_optint\160\176@\160\160C\144\160\176\001\005\193\"fd@\160\176\001\005\194#opt@\160\176\001\005\195!v@@@@@@ABCD&setuid\160@@\208\208@(shutdown\160@@\208@3shutdown_connection\160\176@\160\160A\144\160\176\001\006\245&inchan@@@@@@AB*sigpending\160@@\208@+sigprocmask\160@@\208@*sigsuspend\160@@@ABCE,single_write\160\176@\160\160D\144\160\176\001\004{\"fd@\160\176\001\004|#buf@\160\176\001\004}#ofs@\160\176\001\004~#len@@@@@\208\208\208@6single_write_substring\160\176@\160\160D\144\160\176\001\004\133\"fd@\160\176\001\004\134#buf@\160\176\001\004\135#ofs@\160\176\001\004\136#len@@@@@@A%sleep\160@@\208@&socket\160@@\208@*socketpair\160@@@ABC$stat\160@@\208@&stderr\160@@@ADFG%stdin\160@@\208\208\208@&stdout\160@@\208\208@3string_of_inet_addr\160@@@A'symlink\160@@@BC&system\160\176@\160\160A\144\160\176\001\006]#cmd@@@@@\208\208@'tcdrain\160@@\208\208@&tcflow\160@@@A'tcflush\160@@@BC)tcgetattr\160@@\208\208\208@+tcsendbreak\160@@@A)tcsetattr\160@@@B$time\160@@\208@%times\160@@@ACDE(truncate\160@@\208\208\208@%umask\160@@@A&unlink\160@@\208@&utimes\160@@@AB$wait\160@@\208@'waitpid\160@@\208@%write\160\176@\160\160D\144\160\176\001\004v\"fd@\160\176\001\004w#buf@\160\176\001\004x#ofs@\160\176\001\004y#len@@@@@\208@/write_substring\160\176@\160\160D\144\160\176\001\004\128\"fd@\160\176\001\004\129#buf@\160\176\001\004\130#ofs@\160\176\001\004\131#len@@@@@@ABCDFHIJ\144$Unix@")));
            ("weak.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\003\224\000\000\001\n\000\000\003n\000\000\003Y\176\208\208\208\208\208@$Make\160\176A\160\160A\144\160\176\001\0044!H@@@@@@A$blit\160@@@B%check\160@\144\179@\160\176\001\004\249$prim@\160\176\001\004\248\004\003@@\166\155\240/caml_weak_checkBA @@\144\176\193 \176\179\144\176\001\003\240!t@\160\176\150\176\144\144!a\002\005\245\225\000\001\003\200\001\003\247\001\003\185@\144@\002\005\245\225\000\001\003\187\176\193\004\016\176\179\144\176A#int@@\144@\002\005\245\225\000\001\003\191\176\179\144\176E$bool@@\144@\002\005\245\225\000\001\003\194@\002\005\245\225\000\001\003\197@\002\005\245\225\000\001\003\198\160\144\004*\160\144\004)@@C&create\160@\144\179@\160\176\001\005\001\0041@@\166\155\2400caml_weak_createAA\004.@@\144\176\193\004-\176\179\004\029@\144@\002\005\245\225\000\001\002\237\176\179\004/\160\176\150\176\144\144!a\002\005\245\225\000\001\002\248\001\003\241\001\002\240@\144@\002\005\245\225\000\001\002\242@\002\005\245\225\000\001\002\246\160\144\004\023@\208@$fill\160\176A\160\160D\144\160\176\001\003\250\"ar@\160\176\001\003\251#ofs@\160\176\001\003\252#len@\160\176\001\003\253!x@@@@@@AD#get\160@\144\179@\160\176\001\004\253\004`@\160\176\001\004\252\004b@@\166\155\240-caml_weak_getBA\004_@@\144\176\193\004^\176\179\004]\160\176\150\176\144\144!a\002\005\245\225\000\001\003\166\001\003\245\001\003\149@\144@\002\005\245\225\000\001\003\151\176\193\004j\176\179\004Z@\144@\002\005\245\225\000\001\003\155\176\179\144\176J&option@\160\004\018@\144@\002\005\245\225\000\001\003\159@\002\005\245\225\000\001\003\163@\002\005\245\225\000\001\003\164\160\144\004\"\160\144\004\"@\208\208@(get_copy\160@\144\179@\160\176\001\004\251\004\139@\160\176\001\004\250\004\141@@\166\155\2402caml_weak_get_copyBA\004\138@@\144\176\193\004\137\176\179\004\136\160\176\150\176\144\144!a\002\005\245\225\000\001\003\184\001\003\246\001\003\167@\144@\002\005\245\225\000\001\003\169\176\193\004\149\176\179\004\133@\144@\002\005\245\225\000\001\003\173\176\179\004+\160\004\015@\144@\002\005\245\225\000\001\003\177@\002\005\245\225\000\001\003\181@\002\005\245\225\000\001\003\182\160\144\004\031\160\144\004\031@@A&length\160\176A\160\160A\144\160\176\001\003\243!x@@@@\144\179@\004\005\166M\160\166\b\000\000\004\018@\160\144\004\n@\160\145\144\144A@\208@#set\160@\144\179@\160\176\001\005\000\004\199@\160\176\001\004\255\004\201@\160\176\001\004\254\004\203@@\166\155\240-caml_weak_setCA\004\200@@\144\176\193\004\199\176\179\004\198\160\176\150\176\144\144!a\002\005\245\225\000\001\003\148\001\003\244\001\003\127@\144@\002\005\245\225\000\001\003\129\176\193\004\211\176\179\004\195@\144@\002\005\245\225\000\001\003\133\176\193\004\216\176\179\004k\160\004\017@\144@\002\005\245\225\000\001\003\137\176\179\144\176F$unit@@\144@\002\005\245\225\000\001\003\141@\002\005\245\225\000\001\003\144@\002\005\245\225\000\001\003\145@\002\005\245\225\000\001\003\146\160\144\004)\160\144\004)\160\144\004)@@ABE@@")));
            ("block.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\175\000\000\0003\000\000\000\165\000\000\000\161\176\208@\"__\160\176@\160\160B\144\160\176\001\003\241#tag@\160\176\001\003\242%block@@@@\144\179@\004\b\173\166\155\2400caml_obj_set_tagBA @@\144\176\193 \176\179\144\176\001\003\240!t@@\144@\002\005\245\225\000\001\003\020\176\193\004\t\176\179\144\176A#int@@\144@\002\005\245\225\000\001\003\023\176\179\144\176F$unit@@\144@\002\005\245\225\000\001\003\026@\002\005\245\225\000\001\003\029@\002\005\245\225\000\001\003\030\160\144\004$\160\144\004)@\144\004'@A@@")));
            ("caml_array.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\230\000\000\000>\000\000\000\210\000\000\000\200\176\208\208\208@/caml_array_blit\160\176A\160\160E\144\160\176\001\004\022\"a1@\160\176\001\004\023\"i1@\160\176\001\004\024\"a2@\160\176\001\004\025\"i2@\160\176\001\004\026#len@@@@@@A1caml_array_concat\160\176@\160\160A\144\160\176\001\004\006!l@@@@@@B.caml_array_sub\160\176@\160\160C\144\160\176\001\003\241!x@\160\176\001\003\242&offset@\160\176\001\003\243#len@@@@@\208@.caml_make_vect\160\176@\160\160B\144\160\176\001\004\017#len@\160\176\001\004\018$init@@@@@@AC@@")));
            ("caml_backtrace.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\144\000\000\000\028\000\000\000g\000\000\000[\176\208@?caml_convert_raw_backtrace_slot\160\176A\160\160A\144\160\176\001\003\241%param@@@A\144\179@\004\005\166\156@\160\166\181@B@\160\166\147\176S'FailureC@\160\145\144\162\t-caml_convert_raw_backtrace_slot unimplemented@@@@A@@")));
            ("caml_bigarray.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\026\253\000\000\006J\000\000\021\011\000\000\020m\176\208\208\208\208@6caml_array_bound_error\160\176A\160\160A\144\160\176\001\004\169%param@@@A\144\179@\004\005\166\156@\160\166\181@B@\160\166\147\176R0Invalid_argumentC@\160\145\144\1623index out of bounds@@@\208@,caml_ba_blit\160@\144\179@\160\176\001\004\148$prim@\160\176\001\004\147\004\003@@\166\155\240 BA @\160\160\160'bs.call\176\1920caml_bigarray.ml\001\002\139\001L\133\001L\136\192\004\002\001\002\139\001L\133\001L\143@\144\160\160\160\176\145\162.$$caml_ba_blit@\176\192\004\012\001\002\139\001L\133\001L\144\192\004\r\001\002\139\001L\133\001L\160@@@\004\003@\160\160\160(bs.local\176\192\004\019\001\002\139\001L\133\001L\165\192\004\020\001\002\139\001L\133\001L\173@\144@@\144\176\193 \176\179\177\177\144\176@(BigarrayA(GenarrayO!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\015l\001\004Z\001\015Q\160\176\150\176\144\144!b\002\005\245\225\000\001\015n\001\004Z\001\015R\160\176\150\176\144\144!c\002\005\245\225\000\001\015p\001\004Z\001\015S@\144@\002\005\245\225\000\001\015W\176\193\004\"\176\179\177\004!!t\000\255\160\004\028\160\004\022\160\004\016@\144@\002\005\245\225\000\001\015`\176\179\144\176F$unit@@\144@\002\005\245\225\000\001\015f@\002\005\245\225\000\001\015i@\002\005\245\225\000\001\015j\160\144\004Y\160\144\004X@@AB.caml_ba_create\160@\144\179@\160\176\001\004\166\004`@\160\176\001\004\165\004b@\160\176\001\004\164\004d@@\166\155\240 CA\004a@\160\160\160'bs.call\176\192\004`\001\002g\001G\227\001G\230\192\004a\001\002g\001G\227\001G\237@\144\160\160\160\176\145\1620$$caml_ba_create@\176\192\004k\001\002g\001G\227\001G\238\192\004l\001\002g\001G\227\001H\000@@@\004\003@\160\160\160(bs.local\176\192\004r\001\002g\001G\227\001H\005\192\004s\001\002g\001G\227\001H\r@\144@@\144\176\193\004_\176\179\177\004]$kind\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\014_\001\004Q\001\012\140\160\176\150\176\144\144!b\002\005\245\225\000\001\014a\001\004Q\001\012\141@\144@\002\005\245\225\000\001\012\144\176\193\004t\176\179\177\004r&layout\000\255\160\176\150\176\144\144!c\002\005\245\225\000\001\014c\001\004Q\001\rX@\144@\002\005\245\225\000\001\rZ\176\193\004\130\176\179\144\176H%array@\160\176\179\144\176A#int@@\144@\002\005\245\225\000\001\r^@\144@\002\005\245\225\000\001\rb\176\179\177\004\142!t\000\255\160\0040\160\004*\160\004\029@\144@\002\005\245\225\000\001\r\201@\002\005\245\225\000\001\014[@\002\005\245\225\000\001\014\\@\002\005\245\225\000\001\014]\160\144\004`\160\144\004`\160\144\004`@\208\208@+caml_ba_dim\160@\144\179@\160\176\001\004\157\004\203@\160\176\001\004\156\004\205@@\166\155\240 BA\004\202@\160\160\160'bs.call\176\192\004\201\001\002u\001I\210\001I\213\192\004\202\001\002u\001I\210\001I\220@\144\160\160\160\176\145\162-$$caml_ba_dim@\176\192\004\212\001\002u\001I\210\001I\221\192\004\213\001\002u\001I\210\001I\236@@@\004\003@\160\160\160(bs.local\176\192\004\219\001\002u\001I\210\001I\241\192\004\220\001\002u\001I\210\001I\249@\144@@\144\176\193\004\200\176\179\177\004\199!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\014\203\001\004U\001\014\182\160\176\150\176\144\144!b\002\005\245\225\000\001\014\205\001\004U\001\014\183\160\176\150\176\144\144!c\002\005\245\225\000\001\014\207\001\004U\001\014\184@\144@\002\005\245\225\000\001\014\188\176\193\004\228\176\179\004\\@\144@\002\005\245\225\000\001\014\194\176\179\004_@\144@\002\005\245\225\000\001\014\197@\002\005\245\225\000\001\014\200@\002\005\245\225\000\001\014\201\160\144\004H\160\144\004H@@A-caml_ba_dim_1\160@\144\179@\160\176\001\004\130\005\001\026@@\166\155\240 AA\005\001\023@\160\160\160'bs.call\176\192\005\001\022\001\002\165\001O\207\001O\210\192\005\001\023\001\002\165\001O\207\001O\217@\144\160\160\160\176\145\162/$$caml_ba_dim_1@\176\192\005\001!\001\002\165\001O\207\001O\218\192\005\001\"\001\002\165\001O\207\001O\235@@@\004\003@\160\160\160(bs.local\176\192\005\001(\001\002\165\001O\207\001O\240\192\005\001)\001\002\165\001O\207\001O\248@\144@@\144\176\193\005\001\021\176\179\177\005\001\020!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\016*\001\004a\001\016\025\160\176\150\176\144\144!b\002\005\245\225\000\001\016,\001\004a\001\016\026\160\176\150\176\144\144!c\002\005\245\225\000\001\016.\001\004a\001\016\027@\144@\002\005\245\225\000\001\016\031\176\179\004\167@\144@\002\005\245\225\000\001\016%@\002\005\245\225\000\001\016(\160\144\004A@\208@-caml_ba_dim_2\160@\144\179@\160\176\001\004\129\005\001a@@\166\155\240 AA\005\001^@\160\160\160'bs.call\176\192\005\001]\001\002\168\001P7\001P:\192\005\001^\001\002\168\001P7\001PA@\144\160\160\160\176\145\162/$$caml_ba_dim_2@\176\192\005\001h\001\002\168\001P7\001PB\192\005\001i\001\002\168\001P7\001PS@@@\004\003@\160\160\160(bs.local\176\192\005\001o\001\002\168\001P7\001PX\192\005\001p\001\002\168\001P7\001P`@\144@@\144\176\193\005\001\\\176\179\177\005\001[!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\016@\001\004b\001\016/\160\176\150\176\144\144!b\002\005\245\225\000\001\016B\001\004b\001\0160\160\176\150\176\144\144!c\002\005\245\225\000\001\016D\001\004b\001\0161@\144@\002\005\245\225\000\001\0165\176\179\004\238@\144@\002\005\245\225\000\001\016;@\002\005\245\225\000\001\016>\160\144\004A@\208@-caml_ba_dim_3\160@\144\179@\160\176\001\004\128\005\001\168@@\166\155\240 AA\005\001\165@\160\160\160'bs.call\176\192\005\001\164\001\002\171\001P\159\001P\162\192\005\001\165\001\002\171\001P\159\001P\169@\144\160\160\160\176\145\162/$$caml_ba_dim_3@\176\192\005\001\175\001\002\171\001P\159\001P\170\192\005\001\176\001\002\171\001P\159\001P\187@@@\004\003@\160\160\160(bs.local\176\192\005\001\182\001\002\171\001P\159\001P\192\192\005\001\183\001\002\171\001P\159\001P\200@\144@@\144\176\193\005\001\163\176\179\177\005\001\162!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\016V\001\004c\001\016E\160\176\150\176\144\144!b\002\005\245\225\000\001\016X\001\004c\001\016F\160\176\150\176\144\144!c\002\005\245\225\000\001\016Z\001\004c\001\016G@\144@\002\005\245\225\000\001\016K\176\179\005\0015@\144@\002\005\245\225\000\001\016Q@\002\005\245\225\000\001\016T\160\144\004A@@ABCD,caml_ba_fill\160@\144\179@\160\176\001\004\146\005\001\238@\160\176\001\004\145\005\001\240@@\166\155\240 BA\005\001\237@\160\160\160'bs.call\176\192\005\001\236\001\002\142\001L\242\001L\245\192\005\001\237\001\002\142\001L\242\001L\252@\144\160\160\160\176\145\162.$$caml_ba_fill@\176\192\005\001\247\001\002\142\001L\242\001L\253\192\005\001\248\001\002\142\001L\242\001M\r@@@\004\003@\160\160\160(bs.local\176\192\005\001\254\001\002\142\001L\242\001M\018\192\005\001\255\001\002\142\001L\242\001M\026@\144@@\144\176\193\005\001\235\176\179\177\005\001\234!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\015\131\001\004[\001\015q\160\176\150\176\144\144!b\002\005\245\225\000\001\015\133\001\004[\001\015r\160\176\150\176\144\144!c\002\005\245\225\000\001\015\135\001\004[\001\015s@\144@\002\005\245\225\000\001\015w\176\193\005\002\007\004\023\176\179\005\001\221@\144@\002\005\245\225\000\001\015}@\002\005\245\225\000\001\015\128@\002\005\245\225\000\001\015\129\160\144\004E\160\144\004E@\208\208\208@-caml_ba_get_1\160@\144\179@\160\176\001\004\142\005\002=@\160\176\001\004\141\005\002?@@\166\155\240 BA\005\002<@\160\160\160'bs.call\176\192\005\002;\001\002\151\001M\244\001M\247\192\005\002<\001\002\151\001M\244\001M\254@\144\160\160\160\176\145\162/$$caml_ba_get_1@\176\192\005\002F\001\002\151\001M\244\001M\255\192\005\002G\001\002\151\001M\244\001N\016@@@\004\003@\160\160\160(bs.local\176\192\005\002M\001\002\151\001M\244\001N\021\192\005\002N\001\002\151\001M\244\001N\029@\144@@\144\176\193\005\002:\176\179\177\005\0029!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\015\191\001\004]\001\015\173\160\176\150\176\144\144!b\002\005\245\225\000\001\015\193\001\004]\001\015\174\160\176\150\176\144\144!c\002\005\245\225\000\001\015\195\001\004]\001\015\175@\144@\002\005\245\225\000\001\015\179\176\193\005\002V\176\179\005\001\206@\144@\002\005\245\225\000\001\015\185\004\026@\002\005\245\225\000\001\015\188@\002\005\245\225\000\001\015\189\160\144\004E\160\144\004E@@A-caml_ba_get_2\160@\144\179@\160\176\001\004\133\005\002\137@\160\176\001\004\132\005\002\139@\160\176\001\004\131\005\002\141@@\166\155\240 CA\005\002\138@\160\160\160'bs.call\176\192\005\002\137\001\002\162\001Og\001Oj\192\005\002\138\001\002\162\001Og\001Oq@\144\160\160\160\176\145\162/$$caml_ba_get_2@\176\192\005\002\148\001\002\162\001Og\001Or\192\005\002\149\001\002\162\001Og\001O\131@@@\004\003@\160\160\160(bs.local\176\192\005\002\155\001\002\162\001Og\001O\136\192\005\002\156\001\002\162\001Og\001O\144@\144@@\144\176\193\005\002\136\176\179\177\005\002\135!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\016\020\001\004`\001\015\254\160\176\150\176\144\144!b\002\005\245\225\000\001\016\022\001\004`\001\015\255\160\176\150\176\144\144!c\002\005\245\225\000\001\016\024\001\004`\001\016\000@\144@\002\005\245\225\000\001\016\004\176\193\005\002\164\176\179\005\002\028@\144@\002\005\245\225\000\001\016\n\176\193\005\002\169\176\179\005\002!@\144@\002\005\245\225\000\001\016\r\004\031@\002\005\245\225\000\001\016\016@\002\005\245\225\000\001\016\017@\002\005\245\225\000\001\016\018\160\144\004L\160\144\004L\160\144\004L@\208\208@-caml_ba_get_3\160@@@A3caml_ba_get_generic\160@\144\179@\160\176\001\004\163\005\002\226@\160\176\001\004\162\005\002\228@@\166\155\240 BA\005\002\225@\160\160\160'bs.call\176\192\005\002\224\001\002k\001Hd\001Hg\192\005\002\225\001\002k\001Hd\001Hn@\144\160\160\160\176\145\1625$$caml_ba_get_generic@\176\192\005\002\235\001\002k\001Hd\001Ho\192\005\002\236\001\002k\001Hd\001H\134@@@\004\003@\160\160\160(bs.local\176\192\005\002\242\001\002k\001Hd\001H\139\192\005\002\243\001\002k\001Hd\001H\147@\144@@\144\176\193\005\002\223\176\179\177\005\002\222!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\014{\001\004R\001\014d\160\176\150\176\144\144!b\002\005\245\225\000\001\014}\001\004R\001\014e\160\176\150\176\144\144!c\002\005\245\225\000\001\014\127\001\004R\001\014f@\144@\002\005\245\225\000\001\014j\176\193\005\002\251\176\179\005\002y\160\176\179\005\002v@\144@\002\005\245\225\000\001\014p@\144@\002\005\245\225\000\001\014t\004\030@\002\005\245\225\000\001\014x@\002\005\245\225\000\001\014y\160\144\004I\160\144\004I@@BC0caml_ba_get_size\160\176@\160\160A\144\160\176\001\003\245$dims@@@@@\208@,caml_ba_kind\160@\144\179@\160\176\001\004\155\005\003<@@\166\155\240 AA\005\0039@\160\160\160'bs.call\176\192\005\0038\001\002x\001JA\001JD\192\005\0039\001\002x\001JA\001JK@\144\160\160\160\176\145\162.$$caml_ba_kind@\176\192\005\003C\001\002x\001JA\001JL\192\005\003D\001\002x\001JA\001J\\@@@\004\003@\160\160\160(bs.local\176\192\005\003J\001\002x\001JA\001Ja\192\005\003K\001\002x\001JA\001Ji@\144@@\144\176\193\005\0037\176\179\177\005\0036!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\014\229\001\004V\001\014\208\160\176\150\176\144\144!b\002\005\245\225\000\001\014\231\001\004V\001\014\209\160\176\150\176\144\144!c\002\005\245\225\000\001\014\233\001\004V\001\014\210@\144@\002\005\245\225\000\001\014\214\176\179\005\002\242\160\004\024\160\004\018@\144@\002\005\245\225\000\001\014\222@\002\005\245\225\000\001\014\227\160\144\004C@\208@.caml_ba_layout\160@\144\179@\160\176\001\004\154\005\003\133@@\166\155\240 AA\005\003\130@\160\160\160'bs.call\176\192\005\003\129\001\002{\001J\175\001J\178\192\005\003\130\001\002{\001J\175\001J\185@\144\160\160\160\176\145\1620$$caml_ba_layout@\176\192\005\003\140\001\002{\001J\175\001J\186\192\005\003\141\001\002{\001J\175\001J\204@@@\004\003@\160\160\160(bs.local\176\192\005\003\147\001\002{\001J\175\001J\209\192\005\003\148\001\002{\001J\175\001J\217@\144@@\144\176\193\005\003\128\176\179\177\005\003\127!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\014\253\001\004W\001\014\234\160\176\150\176\144\144!b\002\005\245\225\000\001\014\255\001\004W\001\014\235\160\176\150\176\144\144!c\002\005\245\225\000\001\015\001\001\004W\001\014\236@\144@\002\005\245\225\000\001\014\240\176\179\005\003&\160\004\n@\144@\002\005\245\225\000\001\014\247@\002\005\245\225\000\001\014\251\160\144\004B@\208@9caml_ba_map_file_bytecode\160\176A\160\160A\144\160\176\001\004\167\005\003\232@@@A\144\179@\004\004\166\156@\160\166\005\003\231\160\166\147\176S'FailureC@\160\145\144\162\t)caml_ba_map_file_bytecode not implemented@@@@ABCDE0caml_ba_num_dims\160@\144\179@\160\176\001\004\158\005\003\229@@\166\155\240 AA\005\003\226@\160\160\160'bs.call\176\192\005\003\225\001\002r\001Ib\001Ie\192\005\003\226\001\002r\001Ib\001Il@\144\160\160\160\176\145\1622$$caml_ba_num_dims@\176\192\005\003\236\001\002r\001Ib\001Im\192\005\003\237\001\002r\001Ib\001I\129@@@\004\003@\160\160\160(bs.local\176\192\005\003\243\001\002r\001Ib\001I\134\192\005\003\244\001\002r\001Ib\001I\142@\144@@\144\176\193\005\003\224\176\179\177\005\003\223!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\014\177\001\004T\001\014\160\160\176\150\176\144\144!b\002\005\245\225\000\001\014\179\001\004T\001\014\161\160\176\150\176\144\144!c\002\005\245\225\000\001\014\181\001\004T\001\014\162@\144@\002\005\245\225\000\001\014\166\176\179\005\003r@\144@\002\005\245\225\000\001\014\172@\002\005\245\225\000\001\014\175\160\144\004A@\208\208\208\208@/caml_ba_reshape\160@\144\179@\160\176\001\004\144\005\004/@\160\176\001\004\143\005\0041@@\166\155\240 BA\005\004.@\160\160\160'bs.call\176\192\005\004-\001\002\147\001M\130\001M\133\192\005\004.\001\002\147\001M\130\001M\140@\144\160\160\160\176\145\1621$$caml_ba_reshape@\176\192\005\0048\001\002\147\001M\130\001M\141\192\005\0049\001\002\147\001M\130\001M\160@@@\004\003@\160\160\160(bs.local\176\192\005\004?\001\002\147\001M\130\001M\165\192\005\004@\001\002\147\001M\130\001M\173@\144@@\144\176\193\005\004,\176\179\177\005\004+!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\015\168\001\004\\\001\015\136\160\176\150\176\144\144!b\002\005\245\225\000\001\015\170\001\004\\\001\015\137\160\176\150\176\144\144!c\002\005\245\225\000\001\015\172\001\004\\\001\015\138@\144@\002\005\245\225\000\001\015\142\176\193\005\004H\176\179\005\003\198\160\176\179\005\003\195@\144@\002\005\245\225\000\001\015\148@\144@\002\005\245\225\000\001\015\152\176\179\177\005\004N!t\000\255\160\004#\160\004\029\160\004\023@\144@\002\005\245\225\000\001\015\159@\002\005\245\225\000\001\015\165@\002\005\245\225\000\001\015\166\160\144\004Q\160\144\004Q@@A-caml_ba_set_1\160@\144\179@\160\176\001\004\140\005\004\135@\160\176\001\004\139\005\004\137@\160\176\001\004\138\005\004\139@@\166\155\240 CA\005\004\136@\160\160\160'bs.call\176\192\005\004\135\001\002\155\001No\001Nr\192\005\004\136\001\002\155\001No\001Ny@\144\160\160\160\176\145\162/$$caml_ba_set_1@\176\192\005\004\146\001\002\155\001No\001Nz\192\005\004\147\001\002\155\001No\001N\139@@@\004\003@\160\160\160(bs.local\176\192\005\004\153\001\002\155\001No\001N\144\192\005\004\154\001\002\155\001No\001N\152@\144@@\144\176\193\005\004\134\176\179\177\005\004\133!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\015\218\001\004^\001\015\196\160\176\150\176\144\144!b\002\005\245\225\000\001\015\220\001\004^\001\015\197\160\176\150\176\144\144!c\002\005\245\225\000\001\015\222\001\004^\001\015\198@\144@\002\005\245\225\000\001\015\202\176\193\005\004\162\176\179\005\004\026@\144@\002\005\245\225\000\001\015\208\176\193\005\004\167\004\028\176\179\005\004}@\144@\002\005\245\225\000\001\015\211@\002\005\245\225\000\001\015\214@\002\005\245\225\000\001\015\215@\002\005\245\225\000\001\015\216\160\144\004L\160\144\004L\160\144\004L@\208@-caml_ba_set_2\160@@\208@-caml_ba_set_3\160@@@ABC3caml_ba_set_generic\160@\144\179@\160\176\001\004\161\005\004\226@\160\176\001\004\160\005\004\228@\160\176\001\004\159\005\004\230@@\166\155\240 CA\005\004\227@\160\160\160'bs.call\176\192\005\004\226\001\002o\001H\241\001H\244\192\005\004\227\001\002o\001H\241\001H\251@\144\160\160\160\176\145\1625$$caml_ba_set_generic@\176\192\005\004\237\001\002o\001H\241\001H\252\192\005\004\238\001\002o\001H\241\001I\019@@@\004\003@\160\160\160(bs.local\176\192\005\004\244\001\002o\001H\241\001I\024\192\005\004\245\001\002o\001H\241\001I @\144@@\144\176\193\005\004\225\176\179\177\005\004\224!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\014\155\001\004S\001\014\128\160\176\150\176\144\144!b\002\005\245\225\000\001\014\157\001\004S\001\014\129\160\176\150\176\144\144!c\002\005\245\225\000\001\014\159\001\004S\001\014\130@\144@\002\005\245\225\000\001\014\134\176\193\005\004\253\176\179\005\004{\160\176\179\005\004x@\144@\002\005\245\225\000\001\014\140@\144@\002\005\245\225\000\001\014\144\176\193\005\005\006\004 \176\179\005\004\220@\144@\002\005\245\225\000\001\014\148@\002\005\245\225\000\001\014\151@\002\005\245\225\000\001\014\152@\002\005\245\225\000\001\014\153\160\144\004P\160\144\004P\160\144\004P@\208\208@-caml_ba_slice\160@\144\179@\160\176\001\004\150\005\005=@\160\176\001\004\149\005\005?@@\166\155\240 BA\005\005<@\160\160\160'bs.call\176\192\005\005;\001\002\134\001K\250\001K\253\192\005\005<\001\002\134\001K\250\001L\004@\144\160\160\160\176\145\162/$$caml_ba_slice@\176\192\005\005F\001\002\134\001K\250\001L\005\192\005\005G\001\002\134\001K\250\001L\022@@@\004\003@\160\160\160(bs.local\176\192\005\005M\001\002\134\001K\250\001L\027\192\005\005N\001\002\134\001K\250\001L#@\144@@\144\176\193\005\005:\176\179\177\005\0059!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\015N\001\004Y\001\015)\160\176\150\176\144\144!b\002\005\245\225\000\001\015P\001\004Y\001\015*\160\176\179\177\005\005K(c_layout\000\255@\144@\002\005\245\225\000\001\015+@\144@\002\005\245\225\000\001\0151\176\193\005\005U\176\179\005\004\211\160\176\179\005\004\208@\144@\002\005\245\225\000\001\0157@\144@\002\005\245\225\000\001\015;\176\179\177\005\005[!t\000\255\160\004\"\160\004\028\160\176\179\004\022@\144@\002\005\245\225\000\001\015?@\144@\002\005\245\225\000\001\015E@\002\005\245\225\000\001\015K@\002\005\245\225\000\001\015L\160\144\004S\160\144\004S@@A+caml_ba_sub\160@\144\179@\160\176\001\004\153\005\005\151@\160\176\001\004\152\005\005\153@\160\176\001\004\151\005\005\155@@\166\155\240 CA\005\005\152@\160\160\160'bs.call\176\192\005\005\151\001\002\128\001KL\001KO\192\005\005\152\001\002\128\001KL\001KV@\144\160\160\160\176\145\162-$$caml_ba_sub@\176\192\005\005\162\001\002\128\001KL\001KW\192\005\005\163\001\002\128\001KL\001Kf@@@\004\003@\160\160\160(bs.local\176\192\005\005\169\001\002\128\001KL\001Kk\192\005\005\170\001\002\128\001KL\001Ks@\144@@\144\176\193\005\005\150\176\179\177\005\005\149!t\000\255\160\176\150\176\144\144!a\002\005\245\225\000\001\015&\001\004X\001\015\002\160\176\150\176\144\144!b\002\005\245\225\000\001\015(\001\004X\001\015\003\160\176\179\004\\@\144@\002\005\245\225\000\001\015\004@\144@\002\005\245\225\000\001\015\n\176\193\005\005\175\176\179\005\005'@\144@\002\005\245\225\000\001\015\016\176\193\005\005\180\176\179\005\005,@\144@\002\005\245\225\000\001\015\019\176\179\177\005\005\182!t\000\255\160\004!\160\004\027\160\176\179\004q@\144@\002\005\245\225\000\001\015\022@\144@\002\005\245\225\000\001\015\028@\002\005\245\225\000\001\015\"@\002\005\245\225\000\001\015#@\002\005\245\225\000\001\015$\160\144\004T\160\144\004T\160\144\004T@@BD5caml_invalid_argument\160\176A\160\160A\144\160\176\001\003\242!s@@@A\144\179@\004\005\166\156@\160\166\005\006\015\160\166\147\005\006\014@\160\144\004\r@@\208@.index_offset_c\160\176@\160\160C\144\160\176\001\003\250&n_dims@\160\176\001\003\251$dims@\160\176\001\003\252%index@@@@@\208@4index_offset_fortran\160\176@\160\160C\144\160\176\001\004\002&n_dims@\160\176\001\004\003$dims@\160\176\001\004\004%index@@@@@@ABEF\144 @")));
            ("caml_builtin_exceptions.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\250\000\000\000%\000\000\000\172\000\000\000\151\176\208\208\208\208@.assert_failure\160@@@A0division_by_zero\160@@@B+end_of_file\160@@\208@'failure\160@@@AC0invalid_argument\160@@\208\208\208@-match_failure\160@@@A)not_found\160@@@B-out_of_memory\160@@\208\208@.stack_overflow\160@@\208@.sys_blocked_io\160@@@AB)sys_error\160@@\208@:undefined_recursive_module\160@@@ACDE@@")));
            ("caml_exceptions.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000i\000\000\000\031\000\000\000j\000\000\000e\176\208@.caml_set_oo_id\160\176@\160\160A\144\160\176\001\003\242!b@@@@@\208\208@&create\160\176@\160\160A\144\160\176\001\003\245#str@@@@@@A&get_id\160\176@\160\160A\144\160\176\001\003\247%param@@@@@@BC@@")));
            ("caml_float.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\001\197\000\000\000g\000\000\001\129\000\000\001g\176\208\208\208\208@3caml_classify_float\160\176A\160\160A\144\160\176\001\004\022!x@@@@@@A3caml_copysign_float\160\176@\160\160B\144\160\176\001\004'!x@\160\176\001\004(!y@@@@@\208\208@0caml_expm1_float\160\176@\160\160A\144\160\176\001\004,!x@@@@@@A2caml_float_compare\160\176A\160\160B\144\160\176\001\004$!x@\160\176\001\004%!y@@@@@@BC0caml_frexp_float\160\176@\160\160A@@@@\208\208@0caml_hypot_float\160\176@\160\160B@@@@@A8caml_int32_bits_of_float\160\176@\160\160A\144\160\176\001\004\019!x@@@@@@BD8caml_int32_float_of_bits\160\176@\160\160A\144\160\176\001\004\003!x@@@@@\208\208@0caml_ldexp_float\160\176@\160\160B@@@@\208@0caml_log10_float\160\176@\160\160A@@@@@AB/caml_modf_float\160\176A\160\160A\144\160\176\001\004\024!x@@@@@@CE\1440caml_ldexp_float@")));
            ("caml_format.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\001\153\000\000\000T\000\000\001?\000\000\001$\176\208\208\208@4caml_float_of_string\160\176@\160\160A\144\160\176\001\004\167!s@@@@@@A1caml_format_float\160\176@\160\160B\144\160\176\001\004\149#fmt@\160\176\001\004\150!x@@@@@@B/caml_format_int\160\176@\160\160B\144\160\176\001\004g#fmt@\160\176\001\004h!i@@@@@\208\208@1caml_int32_format\160\004\014@\208@4caml_int32_of_string\160\176@\160\160A\144\160\176\001\004\n!s@@@@@@AB1caml_int64_format\160\176@\160\160B\144\160\176\001\004k#fmt@\160\176\001\004l!x@@@@@\208\208\208@4caml_int64_of_string\160\176@\160\160A\144\160\176\001\004\028!s@@@@@@A2caml_int_of_string\160\004!@@B5caml_nativeint_format\160\0044@\208@8caml_nativeint_of_string\160\004&@@ACDE\144/float_of_string@")));
            ("caml_gc.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002L\000\000\000\170\000\000\0022\000\000\002\026\176\208\208\208\208@3caml_final_register\160\176A\160\160B\144\160\176\001\003\254%param@\160\176\001\003\255%param@@@@\144\179@\004\b\145\161@\144\"()\208@2caml_final_release\160\176A\160\160A\144\160\176\001\003\253\004\016@@@@\144\179@\004\004\145\161@\144\004\015@AB2caml_gc_compaction\160\176A\160\160A\144\160\176\001\004\000\004\029@@@@\144\179@\004\004\145\161@\144\004\028@C0caml_gc_counters\160\176A\160\160A\144\160\176\001\004\b\004*@@@@\144\179@\004\004\145\178@@\160\144\147\"0.\160\144\147\"0.\160\144\147\"0.@\208@2caml_gc_full_major\160\176A\160\160A\144\160\176\001\004\001\004C@@@@\144\179@\004\004\145\161@\144\004B@AD+caml_gc_get\160\176A\160\160A\144\160\176\001\004\006\004P@@@@@\208\208\208\208@-caml_gc_major\160\176A\160\160A\144\160\176\001\004\002\004\\@@@@\144\179@\004\004\145\161@\144\004[@A3caml_gc_major_slice\160\176A\160\160A\144\160\176\001\004\003\004i@@@@\144\179@\004\004\145\144\144@@B-caml_gc_minor\160\176A\160\160A\144\160\176\001\004\004\004v@@@@\144\179@\004\004\145\161@\144\004u@C2caml_gc_quick_stat\160\176@\160\160A\144\160\176\001\004\t\004\131@@@@@\208\208@+caml_gc_set\160\176A\160\160A\144\160\176\001\004\005\004\141@@@@\144\179@\004\004\145\161@\144\004\140@A,caml_gc_stat\160\176@\160\160A\144\160\176\001\004\n\004\154@@@@@@BDE@@")));
            ("caml_hash.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000J\000\000\000\020\000\000\000D\000\000\000@\176\208@)caml_hash\160\176A\160\160D\144\160\176\001\004\r%count@\160\176\001\004\014&_limit@\160\176\001\004\015$seed@\160\176\001\004\016#obj@@@@@@A@@")));
            ("caml_int32.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\199\000\000\000:\000\000\000\202\000\000\000\191\176\208\208@,caml_bswap16\160\176A\160\160A\144\160\176\001\003\247!x@@@@@\208@0caml_int32_bswap\160\176A\160\160A\144\160\176\001\003\249!x@@@@@\208@4caml_nativeint_bswap\160\004\n@@ABC$idiv\160\176A\160\160B\144\160\176\001\003\241!x@\160\176\001\003\242!y@@@@@\208@$imod\160\176A\160\160B\144\160\176\001\003\244!x@\160\176\001\003\245!y@@@@@\208@$imul\160\176@\160\160B@@@@@ABD\144$imul@")));
            ("caml_int64.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\004f\000\000\001n\000\000\004\186\000\000\004\146\176\208\208\208\208\208@#add\160\176A\160\160B\144\160\176\001\004\225%param@\160\176\001\004\226%param@@@@@@A$asr_\160\176@\160\160B\144\160\176\001\004)!x@\160\176\001\004*'numBits@@@@@\208\208\208@-bits_of_float\160\176A\160\160A\144\160\176\001\004\170!x@@@@@@A'compare\160\176@\160\160B\144\160\176\001\004w$self@\160\176\001\004x%other@@@@@\208@,discard_sign\160\176A\160\160A\144\160\176\001\004\133!x@@@@@@AB#div\160\176@\160\160B\144\160\176\001\004`$self@\160\176\001\004a%other@@@@@\208\208@'div_mod\160\176A\160\160B\144\160\176\001\004s$self@\160\176\001\004t%other@@@@@@A\"eq\160\176A\160\160B\144\160\176\001\004\018!x@\160\176\001\004\019!y@@@@@\208@-float_of_bits\160\176@\160\160A\144\160\176\001\004\153!x@@@@@@ABCD\"ge\160\176A\160\160B\144\160\176\001\004\204\004j@\160\176\001\004\205\004i@@@@@\208\208\208@%get64\160\176A\160\160B\144\160\176\001\004\176!s@\160\176\001\004\177!i@@@@@@A\"gt\160\176A\160\160B\144\160\176\001\004R!x@\160\176\001\004S!y@@@@@@B'is_zero\160\176A\160\160A\144\160\176\001\004\219\004\140@@@@@\208@\"le\160\176A\160\160B\144\160\176\001\004U!x@\160\176\001\004V!y@@@@@@ACE$lsl_\160\176@\160\160B\144\160\176\001\004\030!x@\160\176\001\004\031'numBits@@@@@\208\208@$lsr_\160\176@\160\160B\144\160\176\001\004#!x@\160\176\001\004$'numBits@@@@@\208@\"lt\160\176A\160\160B\144\160\176\001\004O!x@\160\176\001\004P!y@@@@@@AB'max_int\160@@@CF'min_int\160@@\208\208\208\208\208@$mod_\160\176A\160\160B\144\160\176\001\004p$self@\160\176\001\004q%other@@@@@@A#mul\160\176@\160\160B\144\160\176\001\004-$this@\160\176\001\004.%other@@@@@@B#neg\160\176@\160\160A\144\160\176\001\004\023!x@@@@@\208@#neq\160\176A\160\160B\144\160\176\001\004L!x@\160\176\001\004M!y@@@@@@AC#not\160\176A\160\160A\144\160\176\001\004\224\004\255@@@@@\208\208@(of_float\160\176@\160\160A\144\160\176\001\004^!x@@@@@@A(of_int32\160\176A\160\160A\144\160\176\001\004{\"lo@@@@@@BD#one\160@@\208\208\208@#sub\160\176A\160\160B\144\160\176\001\004\025!x@\160\176\001\004\026!y@@@@@@A$swap\160\176A\160\160A\144\160\176\001\004\206\005\001,@@@@@\208@(to_float\160\176@\160\160A\144\160\176\001\004\203\005\0015@@@@@\208@&to_hex\160\176@\160\160A\144\160\176\001\004\127!x@@@@@@ABC(to_int32\160\176A\160\160A\144\160\176\001\004}!x@@@@\144\179@\004\005\166\b\000\000\004!@\160\166\166A\144\"lo\160\144\004\r@\160\145\144\150\018_n\000\001\000\000\000\000@\208@$zero\160@@@ADEG\144.two_ptr_32_dbl@")));
            ("caml_io.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\003\b\000\000\000\179\000\000\002u\000\000\002D\176\208\208\208\208@-caml_ml_flush\160\176A\160\160A\144\160\176\001\004\004\"oc@@@@@@A-caml_ml_input\160\176A\160\160D\144\160\176\001\004\023\"ic@\160\176\001\004\024%bytes@\160\176\001\004\025&offset@\160\176\001\004\026#len@@@A\144\179@\004\014\166\156@\160\166\181@B@\160\166\147\176S'FailureC@\160\145\144\162\t caml_ml_input ic not implemented@@@\208\208@2caml_ml_input_char\160\176A\160\160A\144\160\176\001\004\028\"ic@@@A\144\179@\004\005\166\004\028\160\166\004\027\160\166\147\004\026@\160\145\144\162\t!caml_ml_input_char not implemnted@@@@A:caml_ml_open_descriptor_in\160\176A\160\160A\144\160\176\001\004\000!i@@@A\144\179@\004\005\166\156@\160\166\0042\160\166\147\0041@\160\145\144\162\t*caml_ml_open_descriptor_in not implemented@@@@BC;caml_ml_open_descriptor_out\160\176A\160\160A\144\160\176\001\004\002!i@@@A\144\179@\004\005\166\156@\160\166\004I\160\166\147\004H@\160\145\144\162\t+caml_ml_open_descriptor_out not implemented@@@\208\208\208@9caml_ml_out_channels_list\160\176A\160\160A\144\160\176\001\0040%param@@@@@@A.caml_ml_output\160\176@\160\160D\144\160\176\001\004\t\"oc@\160\176\001\004\n#str@\160\176\001\004\011&offset@\160\176\001\004\012#len@@@@@\208@3caml_ml_output_char\160\176@\160\160B\144\160\176\001\004\020\"oc@\160\176\001\004\021$char@@@@@@AB/node_std_output\160\176@\160\160A@@@@@CD&stderr\160\176A@@@\208@%stdin\160\176@@@@\208@&stdout\160\004\b@@ABE\144%stdin@")));
            ("caml_lexer.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\003.\000\000\000\189\000\000\002\140\000\000\002o\176\208\208@/caml_lex_engine\160@\144\179@\160\176\001\003\251$prim@\160\176\001\003\250\004\003@\160\176\001\003\249\004\005@@\166\155\240 CA @\160\160\160'bs.call\176\192-caml_lexer.ml\001\0017\001'\160\001'\163\192\004\002\001\0017\001'\160\001'\170@\144\160\160\160\176\145\1621$$caml_lex_engine@\176\192\004\012\001\0017\001'\160\001'\171\192\004\r\001\0017\001'\160\001'\190@@@\004\003@\160\160\160(bs.local\176\192\004\019\001\0017\001'\160\001'\195\192\004\020\001\0017\001'\160\001'\203@\144@@\144\176\193 \176\179\177\144\176@&LexingA*lex_tables\000\255@\144@\002\005\245\225\000\001\003|\176\193\004\011\176\179\144\176A#int@@\144@\002\005\245\225\000\001\003\127\176\193\004\019\176\179\177\144\176@&LexingA&lexbuf\000\255@\144@\002\005\245\225\000\001\003\130\176\179\004\016@\144@\002\005\245\225\000\001\003\133@\002\005\245\225\000\001\003\136@\002\005\245\225\000\001\003\137@\002\005\245\225\000\001\003\138\160\144\004I\160\144\004H\160\144\004H@\208@3caml_new_lex_engine\160@\144\179@\160\176\001\003\248\004S@\160\176\001\003\247\004U@\160\176\001\003\246\004W@@\166\155\240 CA\004R@\160\160\160'bs.call\176\192\004Q\001\001;\001(%\001((\192\004R\001\001;\001(%\001(/@\144\160\160\160\176\145\1625$$caml_new_lex_engine@\176\192\004\\\001\001;\001(%\001(0\192\004]\001\001;\001(%\001(G@@@\004\003@\160\160\160(bs.local\176\192\004c\001\001;\001(%\001(L\192\004d\001\001;\001(%\001(T@\144@@\144\176\193\004P\176\179\177\144\176@&LexingA*lex_tables\000\255@\144@\002\005\245\225\000\001\003\139\176\193\004Z\176\179\004O@\144@\002\005\245\225\000\001\003\142\176\193\004_\176\179\177\144\176@&LexingA&lexbuf\000\255@\144@\002\005\245\225\000\001\003\145\176\179\004\\@\144@\002\005\245\225\000\001\003\148@\002\005\245\225\000\001\003\151@\002\005\245\225\000\001\003\152@\002\005\245\225\000\001\003\153\160\144\004B\160\144\004B\160\144\004B@@AB$fail\160\176A\160\160A\144\160\176\001\003\252%param@@@A\144\179@\004\005\166\156@\160\166\181@B@\160\166\147\176S'FailureC@\160\145\144\1623lexing: empty token@@@@C\144 @")));
            ("caml_md5.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000@\000\000\000\017\000\000\000:\000\000\0007\176\208@/caml_md5_string\160\176@\160\160C\144\160\176\001\004-!s@\160\176\001\004.%start@\160\176\001\004/#len@@@@@@A@@")));
            ("caml_obj.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002j\000\000\000\163\000\000\002;\000\000\002\029\176\208\208\208\208@,caml_compare\160\176@\160\160B\144\160\176\001\004\011!a@\160\176\001\004\012!b@@@@@@A*caml_equal\160\176@\160\160B\144\160\176\001\004#!a@\160\176\001\004$!b@@@@@\208@1caml_greaterequal\160\176A\160\160B\144\160\176\001\0043!a@\160\176\001\0044!b@@@@@\208@0caml_greaterthan\160\176A\160\160B\144\160\176\001\0046!a@\160\176\001\0047!b@@@@@@ABC2caml_int32_compare\160\176A\160\160B\144\160\176\001\004\002!x@\160\176\001\004\003!y@@@@@\208@0caml_int_compare\160\004\r@@AD6caml_lazy_make_forward\160\176A\160\160A\144\160\176\001\003\251!x@@@@\144\179@\004\005\166\181\001\000\250B@\160\144\004\b@\208\208\208\208@.caml_lessequal\160\176A\160\160B\144\160\176\001\0049!a@\160\176\001\004:!b@@@@@@A-caml_lessthan\160\176A\160\160B\144\160\176\001\004<!a@\160\176\001\004=!b@@@@@@B6caml_nativeint_compare\160\004:@\208@-caml_notequal\160\176A\160\160B\144\160\176\001\004.!a@\160\176\001\004/!b@@@@@@AC,caml_obj_dup\160\176@\160\160A\144\160\176\001\003\241!x@@@@@\208@1caml_obj_truncate\160\176@\160\160B\144\160\176\001\003\246!x@\160\176\001\003\247(new_size@@@@@\208@1caml_update_dummy\160\176@\160\160B\144\160\176\001\003\253!x@\160\176\001\003\254!y@@@@@@ABDE@@")));
            ("caml_oo.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000K\000\000\000\017\000\000\000<\000\000\0008\176\208@6caml_get_public_method\160\176A\160\160C\144\160\176\001\003\243#obj@\160\176\001\003\244#tag@\160\176\001\003\245'cacheid@@@@@@A@@")));
            ("caml_parser.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\001 \000\000\000=\000\000\000\220\000\000\000\206\176\208@1caml_parse_engine\160@@\208@5caml_set_parser_trace\160@\144\179@\160\176\001\003\245$prim@@\166\155\240 AA @\160\160\160'bs.call\176\192.caml_parser.ml\001\001n\0011\210\0011\213\192\004\002\001\001n\0011\210\0011\220@\144\160\160\160\176\145\1627$$caml_set_parser_trace@\176\192\004\012\001\001n\0011\210\0011\221\192\004\r\001\001n\0011\210\0011\246@@@\004\003@\160\160\160(bs.local\176\192\004\019\001\001n\0011\210\0011\251\192\004\020\001\001n\0011\210\0012\003@\144@@\144\176\193 \176\179\144\176E$bool@@\144@\002\005\245\225\000\001\003\195\176\179\004\006@\144@\002\005\245\225\000\001\003\198@\002\005\245\225\000\001\003\201\160\144\0041@@AB\144 @")));
            ("caml_primitive.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\004\000\000\000\001\000\000\000\004\000\000\000\004\176@@@")));
            ("caml_queue.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\199\000\000\000K\000\000\000\230\000\000\000\222\176\208@&create\160\176A\160\160A\144\160\176\001\004\006%param@@@@\144\179@\004\005\166\181@\146\160&length$tailA\160\145\144\144@\160\145\161@\144$None@\208\208@(is_empty\160\176A\160\160A\144\160\176\001\004\003!q@@@@\144\179@\004\005\166\157@\160\166\166@\144\004\030\160\144\004\012@\160\145\144\144@@@A$push\160\176A\160\160B\144\160\176\001\003\248!x@\160\176\001\003\249!q@@@@@\208@*unsafe_pop\160\176@\160\160A\144\160\176\001\003\255!q@@@@@@ABC@@")));
            ("caml_string.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\003\182\000\000\000\245\000\000\003K\000\000\003&\176\208\208\208@#add\160\176@\160\160B\144\160\176\001\0045$prim@\160\176\001\0044\004\003@@@@\144\179@\004\007\166\155\2400js_string_appendBA @@\144\176\193 \176\179\144\176C&string@@\144@\002\005\245\225\000\001\004w\176\193\004\t\176\179\004\b@\144@\002\005\245\225\000\001\004z\176\179\004\011@\144@\002\005\245\225\000\001\004}@\002\005\245\225\000\001\004\128@\002\005\245\225\000\001\004\129\160\144\004\031\160\144\004\030@@A/bytes_of_string\160\176@\160\160A\144\160\176\001\004\021!s@@@@@\208\208@/bytes_to_string\160\176@\160\160A\144\160\176\001\004$!a@@@@@\208@/caml_blit_bytes\160\176A\160\160E\144\160\176\001\004\011\"s1@\160\176\001\004\012\"i1@\160\176\001\004\r\"s2@\160\176\001\004\014\"i2@\160\176\001\004\015#len@@@@@@AB0caml_blit_string\160\176A\160\160E\144\160\176\001\004\001\"s1@\160\176\001\004\002\"i1@\160\176\001\004\003\"s2@\160\176\001\004\004\"i2@\160\176\001\004\005#len@@@@@\208@2caml_create_string\160\176@\160\160A\144\160\176\001\003\246#len@@@@@\208@0caml_fill_string\160\176A\160\160D\144\160\176\001\003\251!s@\160\176\001\003\252!i@\160\176\001\003\253!l@\160\176\001\003\254!c@@@@@@ABCD1caml_is_printable\160\176A\160\160A\144\160\176\001\004+!c@@@@@\208\208\208@3caml_string_compare\160\176A\160\160B\144\160\176\001\003\248\"s1@\160\176\001\003\249\"s2@@@@@@A/caml_string_get\160\176A\160\160B\144\160\176\001\003\243!s@\160\176\001\003\244!i@@@@@@B1caml_string_get16\160\176A\160\160B\144\160\176\001\004.!s@\160\176\001\004/!i@@@@@\208\208@1caml_string_get32\160\176A\160\160B\144\160\176\001\0041!s@\160\176\001\0042!i@@@@@@A9caml_string_of_char_array\160\176@\160\160A\144\160\176\001\004&%chars@@@@@\208@1js_string_of_char\160\176@\160\160A\144\160\176\001\0046\004\204@@@@\144\179@\004\004\166\155\2403String.fromCharCodeAA\004\201@\160\160\160'bs.call\176\192%js.ml\000W\001\n\186\001\n\195\192\004\002\000W\001\n\186\001\n\202@\144@@\144\176\193\004\209\176\179\144\176B$char@@\144@\002\005\245\225\000\001\004.\176\179\004\214@\144@\002\005\245\225\000\001\0041@\002\005\245\225\000\001\0044\160\144\004\030@@ABCE@@")));
            ("caml_sys.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\001\203\000\000\000r\000\000\001\139\000\000\001s\176\208\208@4caml_raise_not_found\160\176A\160\160A\144\160\176\001\004\003%param@@@A\144\179@\004\005\166\156@\160\166\147\176T)Not_foundC@@\208@/caml_sys_getcwd\160\176A\160\160A\144\160\176\001\003\255\004\019@@@@\144\179@\004\004\145\144\162!/@@AB/caml_sys_getenv\160@\144\179@\160\176\001\003\254$prim@@\166\155\2401$$caml_sys_getenvAA @\160\160\160'bs.call\176\192+caml_sys.mle\001\004\216\001\004\221\192\004\002e\001\004\216\001\004\228@\144@\160\160\160(bs.local\176\192\004\te\001\004\216\001\004\234\192\004\ne\001\004\216\001\004\242@\144@@\144\176\193 \176\179\144\176C&string@@\144@\002\005\245\225\000\001\003\t\176\179\004\006@\144@\002\005\245\225\000\001\003\012@\002\005\245\225\000\001\003\015\160\144\004'@\208\208@4caml_sys_random_seed\160\176A\160\160A\144\160\176\001\004\001\004O@@@@@\208@7caml_sys_system_command\160\176A\160\160A\144\160\176\001\004\000\004X@@@@\144\179@\004\004\145\144\144\000\127@AB-caml_sys_time\160\176A\160\160A\144\160\176\001\004\002\004e@@@@@@CD\144 @")));
            ("caml_utils.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\031\000\000\000\t\000\000\000\031\000\000\000\029\176\208@&repeat\160\176@\160\160B@@@@@A\144&repeat@")));
            ("caml_weak.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\002(\000\000\000\156\000\000\001\252\000\000\001\235\176\208\208\208\208@.caml_weak_blit\160\176A\160\160E\144\160\176\001\004\022\"a1@\160\176\001\004\023\"i1@\160\176\001\004\024\"a2@\160\176\001\004\025\"i2@\160\176\001\004\026#len@@@@@@A/caml_weak_check\160\176A\160\160B\144\160\176\001\004\005\"xs@\160\176\001\004\006!i@@@@@@B0caml_weak_create\160\176@\160\160A\144\160\176\001\003\247!n@@@@\144\179@\004\005\166\155\240/js_create_arrayAA @@\144\176\193 \176\179\144\176A#int@@\144@\002\005\245\225\000\001\004\172\176\179\144\176H%array@\160\176\150\176\144\144!a\002\005\245\225\000\001\004\183\001\004\028\001\004\175@\144@\002\005\245\225\000\001\004\177@\002\005\245\225\000\001\004\181\160\144\004\"@@C-caml_weak_get\160\176@\160\160B\144\160\176\001\003\254\"xs@\160\176\001\003\255!i@@@@\144\179@\004\b\166\155\240+js_from_defAA\004,@@\144\176\193\004+\176\179\144\176\001\003\252!t@\160\176\150\176\144\144!a\002\005\245\225\000\001\003\192\001\003\253\001\003\179@\144@\002\005\245\225\000\001\003\181\176\179\144\176J&option@\160\004\r@\144@\002\005\245\225\000\001\003\186@\002\005\245\225\000\001\003\190\160\166\b\000\000\004\021@\160\144\004'\160\144\004&@@\208\208@2caml_weak_get_copy\160\176A\160\160B\144\160\176\001\004\001\"xs@\160\176\001\004\002!i@@@@@@A-caml_weak_set\160\176A\160\160C\144\160\176\001\003\249\"xs@\160\176\001\003\250!i@\160\176\001\003\251!v@@@@@@BD@@")));
            ("curry.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\005O\000\000\001\193\000\000\005\128\000\000\005j\176\208\208\208\208@\"_1\160\176@\160\160B\144\160\176\001\004\t!o@\160\176\001\004\n!x@@@@@@A\"_2\160\176@\160\160C\144\160\176\001\004\r!o@\160\176\001\004\014!x@\160\176\001\004\015!y@@@@@@B\"_3\160\176@\160\160D\144\160\176\001\004\018!o@\160\176\001\004\019\"a0@\160\176\001\004\020\"a1@\160\176\001\004\021\"a2@@@@@\208\208@\"_4\160\176@\160\160E\144\160\176\001\004\024!o@\160\176\001\004\025\"a0@\160\176\001\004\026\"a1@\160\176\001\004\027\"a2@\160\176\001\004\028\"a3@@@@@@A\"_5\160\176@\160\160F\144\160\176\001\004\031!o@\160\176\001\004 \"a0@\160\176\001\004!\"a1@\160\176\001\004\"\"a2@\160\176\001\004#\"a3@\160\176\001\004$\"a4@@@@@@BC\"_6\160\176@\160\160G\144\160\176\001\004'!o@\160\176\001\004(\"a0@\160\176\001\004)\"a1@\160\176\001\004*\"a2@\160\176\001\004+\"a3@\160\176\001\004,\"a4@\160\176\001\004-\"a5@@@@@\208\208\208@\"_7\160\176@\160\160H\144\160\176\001\0040!o@\160\176\001\0041\"a0@\160\176\001\0042\"a1@\160\176\001\0043\"a2@\160\176\001\0044\"a3@\160\176\001\0045\"a4@\160\176\001\0046\"a5@\160\176\001\0047\"a6@@@@@\208@\"_8\160\176@\160\160I\144\160\176\001\004:!o@\160\176\001\004;\"a0@\160\176\001\004<\"a1@\160\176\001\004=\"a2@\160\176\001\004>\"a3@\160\176\001\004?\"a4@\160\176\001\004@\"a5@\160\176\001\004A\"a6@\160\176\001\004B\"a7@@@@@@AB#app\160\176@\160\160B\144\160\176\001\003\252!f@\160\176\001\003\253$args@@@@@\208\208@&curry1\160\176@\160\160C\144\160\176\001\004\004!o@\160\176\001\004\005!x@\160\176\001\004\006%arity@@@@@@A\"js\160\176@\160\160D\144\160\176\001\004E%label@\160\176\001\004F'cacheid@\160\176\001\004G#obj@\160\176\001\004H$args@@@@@\208@#js1\160\176@\160\160C\144\160\176\001\004K%label@\160\176\001\004L'cacheid@\160\176\001\004M#obj@@@@@@ABC#js2\160\176@\160\160D\144\160\176\001\004P%label@\160\176\001\004Q'cacheid@\160\176\001\004R#obj@\160\176\001\004S\"a1@@@@@\208\208@#js3\160\176@\160\160E\144\160\176\001\004V%label@\160\176\001\004W'cacheid@\160\176\001\004X#obj@\160\176\001\004Y\"a1@\160\176\001\004Z\"a2@@@@@@A#js4\160\176@\160\160F\144\160\176\001\004]%label@\160\176\001\004^'cacheid@\160\176\001\004_#obj@\160\176\001\004`\"a1@\160\176\001\004a\"a2@\160\176\001\004b\"a3@@@@@\208\208@#js5\160\176@\160\160G\144\160\176\001\004e%label@\160\176\001\004f'cacheid@\160\176\001\004g#obj@\160\176\001\004h\"a1@\160\176\001\004i\"a2@\160\176\001\004j\"a3@\160\176\001\004k\"a4@@@@@@A#js6\160\176@\160\160H\144\160\176\001\004n%label@\160\176\001\004o'cacheid@\160\176\001\004p#obj@\160\176\001\004q\"a1@\160\176\001\004r\"a2@\160\176\001\004s\"a3@\160\176\001\004t\"a4@\160\176\001\004u\"a5@@@@@\208@#js7\160\176@\160\160I\144\160\176\001\004x%label@\160\176\001\004y'cacheid@\160\176\001\004z#obj@\160\176\001\004{\"a1@\160\176\001\004|\"a2@\160\176\001\004}\"a3@\160\176\001\004~\"a4@\160\176\001\004\127\"a5@\160\176\001\004\128\"a6@@@@@\208@#js8\160\176@\160\160J\144\160\176\001\004\131%label@\160\176\001\004\132'cacheid@\160\176\001\004\133#obj@\160\176\001\004\134\"a1@\160\176\001\004\135\"a2@\160\176\001\004\136\"a3@\160\176\001\004\137\"a4@\160\176\001\004\138\"a5@\160\176\001\004\139\"a6@\160\176\001\004\140\"a7@@@@@@ABCDEF@@")));
            ("fn.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\004\000\000\000\001\000\000\000\004\000\000\000\004\176@@@")));
            ("js.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000y\000\000\000\028\000\000\000r\000\000\000j\176\208\208\208\208@%Array\160@@@A%Bytes\160@@\208\208@*Caml_int64\160@@@A(Caml_obj\160@@@BC#Def\160@@\208@%Float\160@@@AD$Null\160@@\208@(Null_def\160@@\208@&String\160@@@ABE@@")));
            ("js_error.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000\004\000\000\000\001\000\000\000\004\000\000\000\004\176@@@")));
            ("js_primitive.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000U\000\000\000\021\000\000\000L\000\000\000G\176\208\208@4js_from_nullable_def\160\176A\160\160A\144\160\176\001\003\243!x@@@@@@A/js_is_nil_undef\160\176A\160\160A\144\160\176\001\003\241!x@@@@@@B@@")));
            ("typed_array.cmj",
              (lazy
                 (Js_cmj_format.from_string
                    "BUCKLE20160310\132\149\166\190\000\000\000>\000\000\000\n\000\000\000-\000\000\000(\176\208\208\208@-Float32_array\160@@@A-Float64_array\160@@@B+Int32_array\160@@@C@@")))]
      end 
    module Config_util :
      sig
        [@@@ocaml.text
          " A simple wrapper around [Config] module in compiler-libs, so that the search path\n    is the same\n"]
        val find : string -> string[@@ocaml.doc
                                     " [find filename] Input is a file name, output is absolute path "]
        val find_cmj : string -> Js_cmj_format.cmj_table
      end =
      struct
        let find file = Misc.find_in_path_uncap (!Config.load_path) file
        let find_cmj file =
          match find file with
          | f -> Js_cmj_format.from_file f
          | exception Not_found  ->
              let target = String.uncapitalize (Filename.basename file) in
              (match String_map.find target Js_cmj_datasets.cmj_data_sets
               with
               | v ->
                   (match Lazy.force v with
                    | exception _ ->
                        (Ext_log.warn __LOC__
                           "@[%s corrupted in database, when looking %s while compiling %s please update @]"
                           file target (Lam_current_unit.get_file ());
                         Js_cmj_format.no_pure_dummy)
                    | v -> v)
               | exception Not_found  ->
                   (Ext_log.warn __LOC__ "@[%s not found @]" file;
                    Js_cmj_format.no_pure_dummy))
      end 
    module Lam_compile_env :
      sig
        [@@@ocaml.text
          " Helper for global Ocaml module index into meaningful names  "]
        type primitive_description =
          Types.type_expr option Primitive.description
        type key = (Ident.t* Env.t* bool)[@@ocaml.doc
                                           " the boolean is expand or not\n      when it's passed as module, it should be expanded, \n      otherwise for alias, [include Array], it's okay to return an identifier\n      TODO: be more clear about its concept\n  "]
        type ident_info =
          {
          id: Ident.t;
          name: string;
          signatures: Types.signature;
          arity: Lam_stats.function_arities;
          closed_lambda: Lambda.lambda option;}
        type module_info = {
          signature: Types.signature;
          pure: bool;}
        type _ t =
          | No_env: Js_cmj_format.cmj_table t
          | Has_env: Env.t -> module_info t
        val find_and_add_if_not_exist :
          (Ident.t* int) ->
            Env.t ->
              not_found:(Ident.t -> 'a) -> found:(ident_info -> 'a) -> 'a
        val query_and_add_if_not_exist :
          Lam_module_ident.t ->
            'a t -> not_found:(unit -> 'b) -> found:('a -> 'b) -> 'b
        val add_js_module : ?id:Ident.t -> string -> Ident.t[@@ocaml.doc
                                                              " add third party dependency "]
        val reset : unit -> unit
        val is_pure : Lam_module_ident.t -> bool
        val get_goog_package_name : Lam_module_ident.t -> string option
        val get_requried_modules :
          Env.t ->
            Lam_module_ident.t list ->
              Lam_module_ident.t Hash_set.hashset -> Lam_module_ident.t list
      end =
      struct
        module E = Js_exp_make
        module S = Js_stmt_make
        type module_id = Lam_module_ident.t
        type ml_module_info =
          {
          signatures: Types.signature;
          cmj_table: Js_cmj_format.cmj_table;}
        type env_value =
          | Visit of ml_module_info
          | Runtime of
          bool[@ocaml.doc
                " A built in module probably from our runtime primitives, \n                         so it does not have any [signature]\n                      "]
          |
          External[@ocaml.doc
                    " Also a js file, but this belong to third party \n                      "]
        type module_info = {
          signature: Types.signature;
          pure: bool;}
        type primitive_description =
          Types.type_expr option Primitive.description
        type key = (Ident.t* Env.t* bool)[@@ocaml.doc
                                           " we need register which global variable is an dependency "]
        type ident_info =
          {
          id: Ident.t;
          name: string;
          signatures: Types.signature;
          arity: Lam_stats.function_arities;
          closed_lambda: Lambda.lambda option;}
        open Js_output.Ops
        let cached_tbl: (module_id,env_value) Hashtbl.t = Hashtbl.create 31
        let reset () = Hashtbl.clear cached_tbl
        let add_js_module ?id  module_name =
          let id =
            match id with
            | None  -> Ext_ident.create_js_module module_name
            | Some id -> id in
          Hashtbl.replace cached_tbl
            (Lam_module_ident.of_external id module_name) External;
          id
        let add_cached_tbl = Hashtbl.add cached_tbl
        let find_and_add_if_not_exist (id,pos) env ~not_found  ~found  =
          let oid = Lam_module_ident.of_ml id in
          match Hashtbl.find cached_tbl oid with
          | exception Not_found  ->
              let cmj_table = Config_util.find_cmj (id.name ^ ".cmj") in
              (match Type_util.find_serializable_signatures_by_path
                       (Pident id) env
               with
               | None  -> not_found id
               | Some signature ->
                   (add_cached_tbl oid
                      (Visit { signatures = signature; cmj_table });
                    (let name = Type_util.get_name signature pos in
                     let (arity,closed_lambda) =
                       match String_map.find name cmj_table.values with
                       | exception Not_found  -> (NA, None)
                       | { arity; closed_lambda } -> (arity, closed_lambda) in
                     found
                       {
                         id;
                         name;
                         signatures = signature;
                         arity;
                         closed_lambda
                       })))
          | Visit { signatures = serializable_sigs; cmj_table = { values;_} }
              ->
              let name = Type_util.get_name serializable_sigs pos in
              let (arity,closed_lambda) =
                match String_map.find name values with
                | exception Not_found  -> (NA, None)
                | { arity; closed_lambda;_} -> (arity, closed_lambda) in
              found
                {
                  id;
                  name;
                  signatures = serializable_sigs;
                  arity;
                  closed_lambda
                }
          | Runtime _ -> assert false
          | External  -> assert false
        type _ t =
          | No_env: Js_cmj_format.cmj_table t
          | Has_env: Env.t -> module_info t
        let query_and_add_if_not_exist (type u) (oid : Lam_module_ident.t)
          (env : u t) ~not_found  ~found:(found : u -> _)  =
          match Hashtbl.find cached_tbl oid with
          | exception Not_found  ->
              (match oid.kind with
               | Runtime  ->
                   (add_cached_tbl oid (Runtime true);
                    (match env with
                     | Has_env _ -> found { signature = []; pure = true }
                     | No_env  -> found Js_cmj_format.pure_dummy))
               | External _ ->
                   (add_cached_tbl oid External;
                    (match env with
                     | Has_env _ -> found { signature = []; pure = false }
                     | No_env  -> found Js_cmj_format.no_pure_dummy))
               | Ml  ->
                   let cmj_table =
                     Config_util.find_cmj
                       ((Lam_module_ident.name oid) ^ ".cmj") in
                   (match env with
                    | Has_env env ->
                        (match Type_util.find_serializable_signatures_by_path
                                 (Pident (oid.id)) env
                         with
                         | None  -> not_found ()
                         | Some signature ->
                             (add_cached_tbl oid
                                (Visit { signatures = signature; cmj_table });
                              found
                                { signature; pure = (cmj_table.effect = None)
                                }))
                    | No_env  -> found cmj_table))
          | Visit { signatures; cmj_table;_} ->
              (match env with
               | Has_env _ ->
                   found
                     {
                       signature = signatures;
                       pure = (cmj_table.effect = None)
                     }
               | No_env  -> found cmj_table)
          | Runtime pure ->
              (match env with
               | Has_env _ -> found { signature = []; pure }
               | No_env  ->
                   found
                     (if pure
                      then Js_cmj_format.pure_dummy
                      else Js_cmj_format.no_pure_dummy))
          | External  ->
              (match env with
               | Has_env _ -> found { signature = []; pure = false }
               | No_env  -> found Js_cmj_format.no_pure_dummy)
        let is_pure id =
          query_and_add_if_not_exist id No_env ~not_found:(fun _  -> false)
            ~found:(fun x  -> x.effect = None)
        let get_goog_package_name (({ kind;_} as id) : Lam_module_ident.t) =
          match kind with
          | Runtime  -> Some "buckle.runtime"
          | Ml |External _ ->
              query_and_add_if_not_exist id No_env
                ~not_found:(fun _  -> None) ~found:(fun x  -> x.goog_package)
        let get_requried_modules env (extras : module_id list)
          (hard_dependencies : _ Hash_set.hashset) =
          (let mem (x : Lam_module_ident.t) =
             (not (is_pure x)) || (Hash_set.mem hard_dependencies x) in
           Hashtbl.iter
             (fun (id : module_id)  ->
                fun _  -> if mem id then Hash_set.add hard_dependencies id)
             cached_tbl;
           List.iter
             (fun id  -> if mem id then Hash_set.add hard_dependencies id)
             extras;
           Hash_set.elements hard_dependencies : module_id list)
      end 
    module Js_of_lam_option :
      sig
        val get : J.expression -> J.expression
        val none : J.expression
        val some : J.expression -> J.expression
      end =
      struct
        module E = Js_exp_make
        let get arg = (E.index arg 0l : J.expression)
        let none: J.expression =
          {
            expression_desc = (Number (Int { i = 0l; c = None }));
            comment = (Some "None")
          }
        let some x =
          ({
             expression_desc =
               (Caml_block
                  ([x], Immutable, E.zero_int_literal,
                    (Blk_constructor ("Some", 1))));
             comment = None
           } : J.expression)
      end 
    module Ext_marshal :
      sig
        [@@@ocaml.text
          " Extension to the standard library [Marshall] module \n "]
        val to_file : string -> 'a -> unit
        val from_file : string -> 'a
      end =
      struct
        let to_file filename v =
          let chan = open_out_bin filename in
          Marshal.to_channel chan v []; close_out chan
        let from_file filename =
          let chan = open_in_bin filename in
          let v = Marshal.from_channel chan in close_in chan; v[@@ocaml.doc
                                                                 " [bin] mode for WIN support "]
      end 
    module Lam_methname :
      sig
        type t =
          | Js_read_index
          | Js_write_index
          | Js_write
          | Js_read
          | Js of int option
          | Ml of int option
          | Unknown of int option
        val process : string -> (t* string)
      end =
      struct
        type t =
          | Js_read_index
          | Js_write_index
          | Js_write
          | Js_read
          | Js of int option
          | Ml of int option
          | Unknown of int option[@@ocaml.doc
                                   "\n\nNote that js object support gettter, setter\n\n{[ \n  var obj = {\n    get latest () {\n    if (log.length == 0) return undefined;\n    return log[log.length - 1]\n    }\n  } \n]}\n\nIf it's getter then the [.length] property does not make sense, \nwe should avoid using it which means when branch to js, if there is no \nargs, we should not do \n{{ Curry.app0(obj.name) }} \n"]
        let process (x : string) =
          (match x with
           | "index__unsafe_r"|"index__r_unsafe"|"index__r"|"index__" ->
               (Js_read_index, "index")
           | "index__w_unsafe"|"index__w"|"index__w_js_unsafe"|"index__w_js"
               -> (Js_write_index, "index")
           | _ ->
               let sub = "__" in
               let v = Ext_string.rfind ~sub x in
               if v < 0
               then ((Unknown None), x)
               else
                 (let len_sub = String.length sub in
                  let indicator = Ext_string.tail_from x (v + len_sub) in
                  let normal_name = String.sub x 0 v in
                  match indicator with
                  | "r" -> (Js_read, normal_name)
                  | "w" -> (Js_write, normal_name)
                  | _ ->
                      let props = Ext_string.split indicator '_' in
                      let kind = ref None in
                      let arity = ref None in
                      let fail l =
                        let error =
                          "invalid indicator" ^
                            (indicator ^
                               ("in method name " ^
                                  (x ^ (":" ^ (Lam_current_unit.get_file ()))))) in
                        Ext_log.err l "%s" error; failwith error in
                      let update_ref r k =
                        match !r with
                        | None  -> r := (Some k)
                        | Some x -> if x <> k then fail __LOC__ in
                      (List.iter
                         (fun x  ->
                            match x with
                            | "js" -> update_ref kind `Js
                            | "ml" -> update_ref kind `Ml
                            | "gen" -> update_ref kind `Unknown
                            | "unsafe" -> ()
                            | _ ->
                                (match int_of_string x with
                                 | exception _ -> fail __LOC__
                                 | v -> update_ref arity v)) props;
                       (let arity = !arity in
                        ((match !kind with
                          | Some `Js|None  -> Js arity
                          | Some `Ml -> Ml arity
                          | Some `Unknown -> Unknown arity), normal_name)))) : 
          (t* string))
      end 
    module Lam_exit_code :
      sig val has_exit_code : (int -> bool) -> Lambda.lambda -> bool end =
      struct
        let rec has_exit_code exits (lam : Lambda.lambda) =
          (match lam with
           | Lambda.Lvar _|Lambda.Lconst _|Lambda.Lfunction _ -> false
           | Lambda.Lapply (l,args,_apply_info) ->
               (has_exit_code exits l) ||
                 (List.exists (fun x  -> has_exit_code exits x) args)
           | Lambda.Llet (_kind,_id,v,body) ->
               (has_exit_code exits v) || (has_exit_code exits body)
           | Lambda.Lletrec (binding,body) ->
               (List.exists (fun (_,l)  -> has_exit_code exits l) binding) ||
                 (has_exit_code exits body)
           | Lambda.Lprim (_,ls) ->
               List.exists (fun x  -> has_exit_code exits x) ls
           | Lambda.Lswitch (l,lam_switch) ->
               (has_exit_code exits l) ||
                 (has_exit_code_lam_switch exits lam_switch)
           | Lambda.Lstringswitch (l,ls,opt) ->
               (has_exit_code exits l) ||
                 ((List.exists (fun (_,l)  -> has_exit_code exits l) ls) ||
                    ((match opt with
                      | None  -> false
                      | Some x -> has_exit_code exits l)))
           | Lambda.Lstaticraise (v,ls) ->
               (exits v) || (List.exists (has_exit_code exits) ls)
           | Lambda.Lstaticcatch (l,_,handler) ->
               (has_exit_code exits l) || (has_exit_code exits handler)
           | Lambda.Ltrywith (l,_,handler) ->
               (has_exit_code exits l) || (has_exit_code exits handler)
           | Lambda.Lifthenelse (a,b,c) ->
               (has_exit_code exits a) ||
                 ((has_exit_code exits b) || (has_exit_code exits c))
           | Lambda.Lsequence (a,b) ->
               (has_exit_code exits a) || (has_exit_code exits b)
           | Lambda.Lwhile (a,b) ->
               (has_exit_code exits a) || (has_exit_code exits b)
           | Lambda.Lfor (_,a,b,_dir,body) ->
               (has_exit_code exits a) ||
                 ((has_exit_code exits b) || (has_exit_code exits body))
           | Lambda.Lassign (_,a) -> has_exit_code exits a
           | Lambda.Lsend (_,obj,l,ls,_loc) ->
               (has_exit_code exits obj) ||
                 ((has_exit_code exits l) ||
                    (List.exists (has_exit_code exits) ls))
           | Lambda.Levent (b,_) -> has_exit_code exits b
           | Lambda.Lifused (_,b) -> has_exit_code exits b : bool)
        and has_exit_code_lam_switch exits
          (lam_switch : Lambda.lambda_switch) =
          match lam_switch with
          | { sw_numconsts = _; sw_consts; sw_numblocks = _; sw_blocks;
              sw_failaction } ->
              (List.exists (fun (_,l)  -> has_exit_code exits l) sw_consts)
                ||
                ((List.exists (fun (_,l)  -> has_exit_code exits l) sw_blocks)
                   ||
                   ((match sw_failaction with
                     | None  -> false
                     | Some x -> has_exit_code exits x)))
      end 
    module Js_of_lam_module :
      sig
        val make : ?comment:string -> J.expression list -> J.expression
        val is_empty_shape : J.expression -> bool
      end =
      struct
        module E = Js_exp_make
        let make ?comment  (args : J.expression list) =
          E.make_block ?comment E.zero_int_literal Blk_na args Immutable
        let is_empty_shape (shape : J.expression) =
          match shape with
          | {
              expression_desc = Caml_block
                ({ expression_desc = Caml_block ([],_,_,_);_}::[],_,_,_);_}
              -> true
          | _ -> false
      end 
    module Js_of_lam_exception :
      sig
        val get_builtin_by_name : string -> J.expression
        val caml_set_oo_id : J.expression list -> J.expression
      end =
      struct
        module E = Js_exp_make[@@ocaml.doc
                                " An pattern match on {!caml_set_oo_id args}\n    Note that in the trunk, it is immutable by default now \n "]
        let match_exception_def (args : J.expression list) =
          match args with
          | {
              expression_desc = Caml_block
                (exception_str::{
                                  expression_desc = J.Number (Int
                                    { i = 0l;_});_}::[],mutable_flag,
                 { expression_desc = J.Number (Int { i = object_tag;_});_},_);_}::[]
              ->
              if object_tag = 248l
              then Some (exception_str, mutable_flag)
              else None
          | _ -> None
        let make_exception exception_str mutable_flag =
          (E.runtime_call Js_config.exceptions Literals.create
             [exception_str] : J.expression)
        let get_builtin_by_name name =
          E.runtime_ref Js_config.builtin_exceptions (String.lowercase name)
        let caml_set_oo_id args =
          match match_exception_def args with
          | Some (exception_str,mutable_flag) ->
              make_exception exception_str mutable_flag
          | _ -> E.runtime_call Js_config.exceptions "caml_set_oo_id" args
      end 
    module Lam_compile_global :
      sig
        [@@@ocaml.text
          " Compile ocaml external module call , e.g [List.length] to  JS IR "]
        val get_exp : Lam_compile_env.key -> J.expression
        val query_lambda : Ident.t -> Env.t -> Lambda.lambda
      end =
      struct
        module E = Js_exp_make
        module S = Js_stmt_make
        open Js_output.Ops
        let query_lambda id env =
          Lam_compile_env.query_and_add_if_not_exist
            (Lam_module_ident.of_ml id) (Has_env env)
            ~not_found:(fun id  -> assert false)
            ~found:(fun { signature = sigs;_}  ->
                      Lambda.Lprim
                        ((Pmakeblock (0, Blk_na, Immutable)),
                          ((List.mapi
                              (fun i  ->
                                 fun _  ->
                                   Lambda.Lprim
                                     ((Pfield (i, Lambda.Fld_na)),
                                       [Lprim ((Pgetglobal id), [])]))) sigs)))
        let get_exp (key : Lam_compile_env.key) =
          (match key with
           | (id,env,expand) ->
               if Ident.is_predef_exn id
               then Js_of_lam_exception.get_builtin_by_name id.name
               else
                 Lam_compile_env.query_and_add_if_not_exist
                   (Lam_module_ident.of_ml id) (Has_env env)
                   ~not_found:(fun id  -> assert false)
                   ~found:(fun { signature = sigs;_}  ->
                             if expand
                             then
                               let len = List.length sigs in
                               Js_of_lam_module.make ~comment:(id.name)
                                 (Ext_list.init len
                                    (fun i  ->
                                       E.ml_var_dot id
                                         (Type_util.get_name sigs i)))
                             else E.ml_var id) : J.expression)
      end 
    module Parsetree_util :
      sig
        val is_single_string : Parsetree.payload -> string option
        val is_string_or_strings :
          Parsetree.payload ->
            [ `None  | `Single of string  | `Some of string list ]
        val has_arity : Parsetree.attributes -> int option
        val attr_attribute_from_type :
          Parsetree.core_type -> Parsetree.attribute
      end =
      struct
        let is_single_string (x : Parsetree.payload) =
          match x with
          | Parsetree.PStr
              ({
                 pstr_desc = Pstr_eval
                   ({ pexp_desc = Pexp_constant (Const_string (name,_));_},_);_}::[])
              -> Some name
          | _ -> None
        let is_string_or_strings (x : Parsetree.payload) =
          (let module M = struct exception Not_str end in
             match x with
             | PStr
                 ({
                    pstr_desc = Pstr_eval
                      ({
                         pexp_desc = Pexp_apply
                           ({
                              pexp_desc = Pexp_constant (Const_string
                                (name,_));_},args);_},_);_}::[])
                 ->
                 (try
                    `Some (name ::
                      (args |>
                         (List.map
                            (fun (_label,e)  ->
                               match (e : Parsetree.expression) with
                               | {
                                   pexp_desc = Pexp_constant (Const_string
                                     (name,_));_}
                                   -> name
                               | _ -> raise M.Not_str))))
                  with | M.Not_str  -> `None)
             | Parsetree.PStr
                 ({
                    pstr_desc = Pstr_eval
                      ({ pexp_desc = Pexp_constant (Const_string (name,_));_},_);_}::[])
                 -> `Single name
             | _ -> `None : [ `None  | `Single of string 
                            | `Some of string list ])
        let has_arity (attrs : Parsetree.attributes) =
          Ext_list.find_opt
            (fun (attr : Parsetree.attribute)  ->
               match attr with
               | ({ txt = "arity";_},PStr
                  ({
                     pstr_desc = Pstr_eval
                       ({ pexp_desc = Pexp_constant (Const_int i) },_attr);_}::[]))
                   -> Some i
               | _ -> None) attrs
        let attr_attribute_from_type (x : Parsetree.core_type) =
          (let rec aux acc (x : Parsetree.core_type) =
             match x.ptyp_desc with
             | Ptyp_arrow (_,_,r) -> aux (acc + 1) r
             | _ -> acc in
           let n = aux 0 x in
           let loc = x.ptyp_loc in
           ({ txt = "arity"; loc },
             (PStr
                [{
                   pstr_desc =
                     (Pstr_eval
                        ({
                           pexp_desc = (Pexp_constant (Const_int n));
                           pexp_loc = loc;
                           pexp_attributes = []
                         }, []));
                   pstr_loc = loc
                 }])) : Parsetree.attribute)
      end 
    module Js_of_lam_tuple :
      sig
        [@@@ocaml.text " Utilities for compiling lambda tuple into JS IR "]
        val make : J.expression list -> J.expression
      end =
      struct
        module E = Js_exp_make
        let make (args : J.expression list) =
          E.make_block ~comment:"tuple" E.zero_int_literal Blk_tuple args
            Immutable
      end 
    module Js_of_lam_array :
      sig
        [@@@ocaml.text " Utilities for creating Array of JS IR "]
        val make_array :
          J.mutable_flag ->
            Lambda.array_kind -> J.expression list -> J.expression[@@ocaml.doc
                                                                    " create an array "]
        val set_array :
          J.expression -> J.expression -> J.expression -> J.expression
        [@@ocaml.doc
          " Here we don't care about [array_kind],  \n    In the future, we might used TypedArray for FloatArray\n "]
        val ref_array : J.expression -> J.expression -> J.expression
      end =
      struct
        module E = Js_exp_make
        let make_array mt (kind : Lambda.array_kind) args =
          match kind with
          | Pgenarray |Paddrarray  -> E.arr ~comment:"array" mt args
          | Pintarray  -> E.arr ~comment:"int array" mt args
          | Pfloatarray  -> E.arr ~comment:"float array" mt args
        let set_array e e0 e1 = E.assign (E.access e e0) e1
        let ref_array e e0 = E.access e e0
      end 
    module Js_long :
      sig
        type int64_call = J.expression list -> J.expression
        val make_const : lo:Int32.t -> hi:Int32.t -> J.expression
        val of_const : int64 -> J.expression
        val to_int32 : int64_call
        val of_int32 : int64_call
        val comp : Lambda.comparison -> int64_call
        val neg : int64_call
        val add : int64_call
        val sub : int64_call
        val mul : int64_call
        val div : int64_call
        val xor : int64_call
        val mod_ : int64_call
        val lsl_ : int64_call
        val lsr_ : int64_call
        val asr_ : int64_call
        val and_ : int64_call
        val or_ : int64_call
        val swap : int64_call
        val discard_sign : int64_call
        val div_mod : int64_call
        val to_hex : int64_call
        val to_float : int64_call
        val of_float : int64_call
        val compare : int64_call
        val of_string : int64_call
        val float_of_bits : int64_call
        val bits_of_float : int64_call
        val get64 : int64_call
      end =
      struct
        module E = Js_exp_make
        type int64_call = J.expression list -> J.expression
        let int64_call (fn : string) args =
          E.runtime_call Js_config.int64 fn args
        let record_info = Lambda.Blk_record [|"hi";"lo"|]
        let make_const ~lo  ~hi  =
          E.make_block ~comment:"int64" E.zero_int_literal record_info
            [E.int hi; E.to_uint32 @@ (E.int lo)] Immutable
        let make ~lo  ~hi  =
          E.make_block ~comment:"int64" E.zero_int_literal record_info
            [hi; E.to_uint32 lo] Immutable
        let get_lo x = E.index x 1l
        let get_hi x = E.index x 0l
        let of_const (v : Int64.t) =
          make_const ~lo:(Int64.to_int32 v)
            ~hi:(Int64.to_int32 (Int64.shift_right v 32))
        let to_int32 args =
          match args with
          | v::[] -> E.to_int32 @@ (get_lo v)
          | _ -> assert false
        let of_int32 (args : J.expression list) =
          match args with
          | { expression_desc = Number (Int { i });_}::[] ->
              if i < 0l
              then make_const ~lo:i ~hi:(-1l)
              else make_const ~lo:i ~hi:0l
          | _ -> int64_call "of_int32" args
        let comp (cmp : Lambda.comparison) args =
          E.runtime_call Js_config.int64
            (match cmp with
             | Ceq  -> "eq"
             | Cneq  -> "neq"
             | Clt  -> "lt"
             | Cgt  -> "gt"
             | Cle  -> "le"
             | Cge  -> "ge") args
        let neg args = int64_call "neg" args
        let add args = int64_call "add" args
        let sub args = int64_call "sub" args
        let mul args = int64_call "mul" args
        let div args = int64_call "div" args
        let bit_op op args =
          match args with
          | l::r::[] ->
              make ~lo:(op (get_lo l) (get_lo r))
                ~hi:(op (get_hi l) (get_hi r))
          | _ -> assert false
        let xor = bit_op E.int32_bxor
        let or_ = bit_op E.int32_bor
        let and_ = bit_op E.int32_band
        let lsl_ args = int64_call "lsl_" args
        let lsr_ args = int64_call "lsr_" args
        let asr_ args = int64_call "asr_" args
        let mod_ args = int64_call "mod_" args
        let swap args = int64_call "swap" args
        let of_float (args : J.expression list) = int64_call "of_float" args
        let compare (args : J.expression list) = int64_call "compare" args
        let of_string (args : J.expression list) =
          int64_call "of_string" args
        let discard_sign (args : J.expression list) =
          int64_call "discard_sign" args
        let div_mod (args : J.expression list) = int64_call "div_mod" args
        let to_hex (args : J.expression list) = int64_call "to_hex" args
        let get64 = int64_call "get64"
        let float_of_bits = int64_call "float_of_bits"
        let bits_of_float = int64_call "bits_of_float"
        let to_float (args : J.expression list) =
          match args with
          | _::[] -> int64_call "to_float" args
          | _ -> assert false
      end 
    module Lam_dispatch_primitive :
      sig
        [@@@ocaml.text
          " Compile lambda primitives (note this is different external c calls) "]
        val query :
          Lam_compile_env.primitive_description ->
            J.expression list -> J.expression[@@ocaml.doc
                                               " \n    @return None when the primitives are not handled in  pre-processing\n "]
      end =
      struct
        module E = Js_exp_make
        module S = Js_stmt_make
        let query (prim : Lam_compile_env.primitive_description)
          (args : J.expression list) =
          (let prim_name = prim.prim_name in
           let call m = E.runtime_call m prim_name args in
           match prim_name with
           | "caml_gc_stat"|"caml_gc_quick_stat"|"caml_gc_counters"
             |"caml_gc_get"|"caml_gc_set"|"caml_gc_minor"
             |"caml_gc_major_slice"|"caml_gc_major"|"caml_gc_full_major"
             |"caml_gc_compaction"|"caml_final_register"|"caml_final_release"
               -> call Js_config.gc
           | "caml_abs_float" -> E.math "abs" args
           | "caml_acos_float" -> E.math "acos" args
           | "caml_add_float" ->
               (match args with
                | e0::e1::[] -> E.float_add e0 e1
                | _ -> assert false)
           | "caml_div_float" ->
               (match args with
                | e0::e1::[] -> E.float_div e0 e1
                | _ -> assert false)
           | "caml_sub_float" ->
               (match args with
                | e0::e1::[] -> E.float_minus e0 e1
                | _ -> assert false)
           | "caml_eq_float" ->
               (match args with
                | e0::e1::[] -> E.float_equal e0 e1
                | _ -> assert false)
           | "caml_ge_float" ->
               (match args with
                | e0::e1::[] -> E.float_comp Cge e0 e1
                | _ -> assert false)
           | "caml_gt_float" ->
               (match args with
                | e0::e1::[] -> E.float_comp Cgt e0 e1
                | _ -> assert false)
           | "caml_tan_float" -> E.math "tan" args
           | "caml_tanh_float" -> E.math "tanh" args
           | "caml_asin_float" -> E.math "asin" args
           | "caml_atan2_float" -> E.math "atan2" args
           | "caml_atan_float" -> E.math "atan" args
           | "caml_ceil_float" -> E.math "ceil" args
           | "caml_cos_float" -> E.math "cos" args
           | "caml_cosh_float" -> E.math "cosh" args
           | "caml_exp_float" -> E.math "exp" args
           | "caml_sin_float" -> E.math "sin" args
           | "caml_sinh_float" -> E.math "sinh" args
           | "caml_sqrt_float" -> E.math "sqrt" args
           | "caml_float_of_int" ->
               (match args with | e::[] -> e | _ -> assert false)
           | "caml_floor_float" -> E.math "floor" args
           | "caml_log_float" -> E.math "log" args
           | "caml_log10_float" -> E.math "log10" args
           | "caml_log1p_float" -> E.math "log1p" args
           | "caml_power_float" -> E.math "pow" args
           | "caml_make_float_vect" -> E.new_ (E.js_global "Array") args
           | "caml_array_append" ->
               (match args with
                | e0::e1::[] -> E.array_append e0 e1
                | _ -> assert false)
           | "caml_array_get"|"caml_array_get_addr"|"caml_array_get_float"
             |"caml_array_unsafe_get"|"caml_array_unsafe_get_float" ->
               (match args with
                | e0::e1::[] -> Js_of_lam_array.ref_array e0 e1
                | _ -> assert false)
           | "caml_array_set"|"caml_array_set_addr"|"caml_array_set_float"
             |"caml_array_unsafe_set"|"caml_array_unsafe_set_addr"
             |"caml_array_unsafe_set_float" ->
               (match args with
                | e0::e1::e2::[] -> Js_of_lam_array.set_array e0 e1 e2
                | _ -> assert false)
           | "caml_int32_add" ->
               (match args with
                | e0::e1::[] -> E.int32_add e0 e1
                | _ -> assert false)
           | "caml_nativeint_add" ->
               (match args with
                | e0::e1::[] -> E.unchecked_int32_add e0 e1
                | _ -> assert false)
           | "caml_int32_div"|"caml_nativeint_div" ->
               (match args with
                | e0::e1::[] -> E.int32_div e0 e1
                | _ -> assert false)
           | "caml_int32_mul" ->
               (match args with
                | e0::e1::[] -> E.int32_mul e0 e1
                | _ -> assert false)
           | "caml_nativeint_mul" ->
               (match args with
                | e0::e1::[] -> E.unchecked_int32_mul e0 e1
                | _ -> assert false)
           | "caml_int32_of_int"|"caml_nativeint_of_int"
             |"caml_nativeint_of_int32" ->
               (match args with | e::[] -> e | _ -> assert false)
           | "caml_int32_of_float"|"caml_int_of_float"
             |"caml_nativeint_of_float" ->
               (match args with | e::[] -> E.to_int32 e | _ -> assert false)
           | "caml_int32_to_float"|"caml_int32_to_int"
             |"caml_nativeint_to_int"|"caml_nativeint_to_float"
             |"caml_nativeint_to_int32" ->
               (match args with | e::[] -> e | _ -> assert false)
           | "caml_int32_sub" ->
               (match args with
                | e0::e1::[] -> E.int32_minus e0 e1
                | _ -> assert false)
           | "caml_nativeint_sub" ->
               (match args with
                | e0::e1::[] -> E.unchecked_int32_minus e0 e1
                | _ -> assert false)
           | "caml_int32_xor"|"caml_nativeint_xor" ->
               (match args with
                | e0::e1::[] -> E.int32_bxor e0 e1
                | _ -> assert false)
           | "caml_int32_and"|"caml_nativeint_and" ->
               (match args with
                | e0::e1::[] -> E.int32_band e0 e1
                | _ -> assert false)
           | "caml_int32_or"|"caml_nativeint_or" ->
               (match args with
                | e0::e1::[] -> E.int32_bor e0 e1
                | _ -> assert false)
           | "caml_le_float" ->
               (match args with
                | e0::e1::[] -> E.float_comp Cle e0 e1
                | _ -> assert false)
           | "caml_lt_float" ->
               (match args with
                | e0::e1::[] -> E.float_comp Clt e0 e1
                | _ -> assert false)
           | "caml_neg_float" ->
               (match args with
                | e::[] -> E.int32_minus E.zero_int_literal e
                | _ -> assert false)
           | "caml_neq_float" ->
               (match args with
                | e0::e1::[] -> E.float_notequal e0 e1
                | _ -> assert false)
           | "caml_mul_float" ->
               (match args with
                | e0::e1::[] -> E.float_mul e0 e1
                | _ -> assert false)
           | "caml_int64_to_float" -> Js_long.to_float args
           | "caml_int64_of_float" -> Js_long.of_float args
           | "caml_int64_compare" -> Js_long.compare args
           | "js_int64_discard_sign" -> Js_long.discard_sign args
           | "js_int64_div_mod" -> Js_long.div_mod args
           | "js_int64_to_hex" -> Js_long.to_hex args
           | "caml_int64_bits_of_float" -> Js_long.bits_of_float args
           | "caml_int64_float_of_bits" -> Js_long.float_of_bits args
           | "caml_int64_bswap" -> Js_long.swap args
           | "caml_int32_float_of_bits"|"caml_int32_bits_of_float"
             |"caml_classify_float"|"caml_modf_float"|"caml_ldexp_float"
             |"caml_frexp_float"|"caml_float_compare"|"caml_copysign_float"
             |"caml_expm1_float"|"caml_hypot_float" -> call Js_config.float
           | "caml_fmod_float" ->
               (match args with
                | e0::e1::[] -> E.float_mod e0 e1
                | _ -> assert false)
           | "caml_string_equal" ->
               (match args with
                | e0::e1::[] -> E.string_equal e0 e1
                | _ -> assert false)
           | "caml_string_notequal" ->
               (match args with
                | e0::e1::[] -> E.string_comp NotEqEq e0 e1
                | _ -> assert false)
           | "caml_string_lessequal" ->
               (match args with
                | e0::e1::[] -> E.string_comp Le e0 e1
                | _ -> assert false)
           | "caml_string_lessthan" ->
               (match args with
                | e0::e1::[] -> E.string_comp Lt e0 e1
                | _ -> assert false)
           | "caml_string_greaterequal" ->
               (match args with
                | e0::e1::[] -> E.string_comp Ge e0 e1
                | _ -> assert false)
           | "caml_string_greaterthan" ->
               (match args with
                | e0::e1::[] -> E.string_comp Gt e0 e1
                | _ -> assert false)
           | "caml_create_string" ->
               (match args with
                | ({ expression_desc = Number (Int { i;_});_} as v)::[] when
                    i >= 0l -> E.uninitialized_array v
                | _ -> call Js_config.string)
           | "caml_string_get"|"caml_string_compare"|"string_of_bytes"
             |"bytes_of_string"|"caml_is_printable"
             |"caml_string_of_char_array"|"caml_fill_string"
             |"caml_blit_string"|"caml_blit_bytes" -> call Js_config.string
           | "caml_register_named_value" -> E.unit
           | "caml_backtrace_status"|"caml_get_exception_backtrace"
             |"caml_get_exception_raw_backtrace"|"caml_record_backtrace"
             |"caml_convert_raw_backtrace"|"caml_get_current_callstack" ->
               E.unit
           | "caml_set_oo_id" -> Js_of_lam_exception.caml_set_oo_id args
           | "caml_sys_const_big_endian" -> E.bool Sys.big_endian
           | "caml_sys_const_word_size" -> E.small_int Sys.word_size
           | "caml_sys_const_ostype_cygwin" -> E.caml_false
           | "caml_sys_const_ostype_win32" -> E.caml_false
           | "caml_sys_const_ostype_unix" -> E.caml_true
           | "caml_is_js" -> E.caml_true
           | "caml_sys_get_config" ->
               Js_of_lam_tuple.make
                 [E.str Sys.os_type;
                 E.small_int Sys.word_size;
                 E.bool Sys.big_endian]
           | "caml_sys_get_argv" ->
               Js_of_lam_tuple.make
                 [E.str "cmd"; Js_of_lam_array.make_array NA Pgenarray []]
           | "caml_sys_time"|"caml_sys_random_seed"|"caml_sys_getenv"
             |"caml_sys_system_command"|"caml_sys_getcwd" ->
               call Js_config.sys
           | "caml_lex_engine"|"caml_new_lex_engine" -> call Js_config.lexer
           | "caml_parse_engine"|"caml_set_parser_trace" ->
               call Js_config.parser
           | "caml_array_sub"|"caml_array_concat"|"caml_array_blit"
             |"caml_make_vect" -> call Js_config.array
           | "caml_ml_flush"|"caml_ml_out_channels_list"
             |"caml_ml_open_descriptor_in"|"caml_ml_open_descriptor_out"
             |"caml_ml_output_char"|"caml_ml_output"|"caml_ml_input_char" ->
               call Js_config.io
           | "caml_update_dummy"|"caml_obj_dup" ->
               (match args with
                | a::[] when Js_analyzer.is_constant a -> a
                | _ -> call Js_config.obj_runtime)
           | "caml_obj_block" ->
               (match args with
                | tag::{ expression_desc = Number (Int { i;_});_}::[] ->
                    E.make_block tag Blk_na
                      (Ext_list.init (Int32.to_int i)
                         (fun _  -> E.zero_int_literal)) NA
                | tag::size::[] -> E.uninitialized_object tag size
                | _ -> assert false)
           | "caml_format_float"|"caml_nativeint_format"|"caml_int32_format"
             |"caml_float_of_string"|"caml_int_of_string"
             |"caml_int32_of_string"|"caml_nativeint_of_string"
             |"caml_int64_format"|"caml_int64_of_string" ->
               call Js_config.format
           | "caml_format_int" ->
               (match args with
                | { expression_desc = Str (_,"%d");_}::v::[] ->
                    E.int_to_string v
                | _ -> call Js_config.format)
           | "caml_obj_is_block" ->
               (match args with
                | e::[] -> E.is_caml_block e
                | _ -> assert false)
           | "caml_obj_truncate"|"caml_lazy_make_forward"|"caml_compare"
             |"caml_int_compare"|"caml_int32_compare"
             |"caml_nativeint_compare"|"caml_equal"|"caml_notequal"
             |"caml_greaterequal"|"caml_greaterthan"|"caml_lessequal"
             |"caml_lessthan" -> call Js_config.obj_runtime
           | "caml_obj_set_tag" ->
               (match args with
                | a::b::[] -> E.set_tag a b
                | _ -> assert false)
           | "caml_obj_tag" ->
               (match args with | e::[] -> E.tag e | _ -> assert false)
           | "unix_tcdrain"|"unix_tcflush"|"unix_setsid"|"unix_tcflow"
             |"unix_tcgetattr"|"unix_tcsetattr"|"unix_tcsendbreak"
             |"unix_getprotobynumber"|"unix_getprotobyname"
             |"unix_getservbyport"|"unix_getservbyname"|"unix_getservbyaddr"
             |"unix_gethostbyname"|"unix_gethostname"|"unix_getpeername"
             |"unix_accept"|"unix_bind"|"unix_connect"|"unix_listen"
             |"unix_shutdown"|"unix_getsockname"|"unix_gethostbyaddr"
             |"unix_getgrnam"|"unix_getpwuid"|"unix_getgrgid"
             |"unix_inet_addr_of_string"|"unix_string_of_inet_addr"
             |"unix_socket"|"unix_socketpair"|"unix_error_message"
             |"unix_read"|"unix_write"|"unix_single_write"
             |"unix_set_close_on_exec"|"unix_sigprocmask"|"unix_sigsuspend"
             |"unix_recv"|"unix_recvfrom"|"unix_send"|"unix_sendto"
             |"unix_getsockopt"|"unix_setsockopt"|"unix_getaddrinfo"
             |"unix_getnameinfo"|"unix_waitpid"|"unix_wait"|"unix_fork"
             |"unix_execv"|"unix_dup"|"unix_close"|"unix_dup2"|"unix_execvp"
             |"unix_execvpe"|"unix_pipe"|"unix_execve"
             |"caml_channel_descriptor"|"unix_putenv"|"unix_environment"
             |"unix_lseek"|"unix_getppid"|"unix_getpid"|"unix_nice"
             |"unix_open"|"unix_truncate"|"unix_ftruncate"|"unix_stat"
             |"unix_lstat"|"unix_fstat"|"unix_isatty"|"unix_lseek_64"
             |"unix_truncate_64"|"unix_ftruncate_64"|"unix_stat_64"
             |"unix_lstat_64"|"unix_fstat_64"|"unix_unlink"|"unix_rename"
             |"unix_link"|"unix_chmod"|"unix_fchmod"|"unix_chown"
             |"unix_fchown"|"unix_umask"|"unix_access"|"unix_set_nonblock"
             |"unix_clear_nonblock"|"unix_clear_close_on_exec"|"unix_mkdir"
             |"unix_rmdir"|"unix_chdir"|"unix_getcwd"|"unix_chroot"
             |"unix_opendir"|"unix_readdir"|"unix_rewinddir"|"unix_closedir"
             |"unix_mkfifo"|"unix_symlink"|"unix_readlink"|"unix_select"
             |"unix_lockf"|"unix_kill"|"unix_sigpending"|"unix_time"
             |"unix_gettimeofday"|"unix_gmtime"|"unix_localtime"
             |"unix_mktime"|"unix_alarm"|"unix_sleep"|"unix_times"
             |"unix_utimes"|"unix_getitimer"|"unix_setitimer"|"unix_getuid"
             |"unix_geteuid"|"unix_setuid"|"unix_getgid"|"unix_getegid"
             |"unix_setgid"|"unix_getgroups"|"unix_setgroups"
             |"unix_initgroups"|"unix_getlogin"|"unix_getpwnam" ->
               call Js_config.unix
           | "caml_ba_init" ->
               (match args with | e::[] -> E.seq e E.unit | _ -> assert false)
           | "caml_ba_create"|"caml_ba_get_generic"|"caml_ba_set_generic"
             |"caml_ba_num_dims"|"caml_ba_dim"|"caml_ba_kind"
             |"caml_ba_layout"|"caml_ba_sub"|"caml_ba_slice"|"caml_ba_blit"
             |"caml_ba_fill"|"caml_ba_reshape"|"caml_ba_map_file_bytecode" ->
               call Js_config.bigarray
           | "caml_convert_raw_backtrace_slot" -> call Js_config.backtrace
           | "caml_bswap16"|"caml_int32_bswap"|"caml_nativeint_bswap" ->
               call Js_config.int32
           | "caml_get_public_method" -> call Js_config.oo
           | "caml_install_signal_handler" ->
               (match args with
                | num::behavior::[] -> E.seq num behavior
                | _ -> assert false)
           | "caml_md5_string" -> call Js_config.md5
           | "caml_hash" -> call Js_config.hash
           | "caml_weak_set"|"caml_weak_create"|"caml_weak_get"
             |"caml_weak_check"|"caml_weak_blit"|"caml_weak_get_copy" ->
               call Js_config.weak
           | "caml_output_value_to_buffer"|"caml_marshal_data_size"
             |"caml_input_value_from_string"|"caml_output_value"
             |"caml_input_value"|"caml_output_value_to_string"
             |"caml_md5_chan"|"caml_hash_univ_param"|"caml_sys_close"
             |"caml_sys_open"|"caml_ml_input"|"caml_ml_input_scan_line"
             |"caml_ml_input_int"|"caml_ml_close_channel"
             |"caml_ml_output_int"|"caml_sys_exit"|"caml_ml_channel_size_64"
             |"caml_ml_channel_size"|"caml_ml_pos_in_64"|"caml_ml_pos_in"
             |"caml_ml_seek_in"|"caml_ml_seek_in_64"|"caml_ml_pos_out"
             |"caml_ml_pos_out_64"|"caml_ml_seek_out"|"caml_ml_seek_out_64"
             |"caml_ml_set_binary_mode" -> call Js_config.prim
           | "js_function_length" ->
               (match args with
                | f::[] -> E.function_length f
                | _ -> assert false)
           | "js_create_array" ->
               (match args with
                | e::[] -> E.uninitialized_array e
                | _ -> assert false)
           | "js_array_append" ->
               (match args with
                | a::b::[] -> E.array_append a b
                | _ -> assert false)
           | "js_string_append" ->
               (match args with
                | a::b::[] -> E.string_append a b
                | _ -> assert false)
           | "js_apply" ->
               (match args with
                | f::args::[] -> E.flat_call f args
                | _ -> assert false)
           | "js_string_of_small_int_array" ->
               (match args with
                | e::[] -> E.string_of_small_int_array e
                | _ -> assert false)
           | "js_string_of_char" ->
               (match args with
                | { expression_desc = Number (Int { i;_}) }::[] ->
                    E.str (String.make 1 (Char.chr (Int32.to_int i)))
                | _ -> call Js_config.string)
           | "js_boolean_to_bool" ->
               (match args with
                | e::[] -> E.to_ocaml_boolean e
                | _ -> assert false)
           | "js_is_instance_array" ->
               (match args with
                | e::[] -> E.is_instance_array e
                | _ -> assert false)
           | "js_typeof" ->
               (match args with | e::[] -> E.typeof e | _ -> assert false)
           | "js_dump" -> E.seq (E.dump Log args) E.unit
           | "caml_anything_to_string"|"js_anything_to_string" ->
               (match args with
                | e::[] -> E.anything_to_string e
                | _ -> assert false)
           | "js_anything_to_number" ->
               (match args with | e::[] -> E.to_number e | _ -> assert false)
           | "js_json_stringify" ->
               (match args with
                | e::[] -> E.to_json_string e
                | _ -> assert false)
           | "js_apply1"|"js_apply2"|"js_apply3"|"js_apply4"|"js_apply5"
             |"js_apply6"|"js_apply7"|"js_apply8" ->
               (match args with
                | fn::rest ->
                    E.call ~info:{ arity = Full; call_info = Call_na } fn
                      rest
                | _ -> assert false)
           | "js_uninitialized_object" ->
               (match args with
                | tag::size::[] -> E.uninitialized_object tag size
                | _ -> assert false)
           | "js_obj_length" ->
               (match args with | e::[] -> E.obj_length e | _ -> assert false)
           | "js_pure_expr" ->
               (match args with
                | { expression_desc = Str (_,s) }::[] ->
                    E.raw_js_code
                      (Exp (Parsetree_util.has_arity prim.prim_attributes)) s
                | _ ->
                    (Ext_log.err __LOC__
                       "JS.unsafe_js_expr is applied to an non literal string in %s"
                       (Lam_current_unit.get_file ());
                     assert false))
           | "js_pure_stmt" ->
               (match args with
                | { expression_desc = Str (_,s) }::[] -> E.raw_js_code Stmt s
                | _ ->
                    (Ext_log.err __LOC__
                       "JS.unsafe_js_expr is applied to an non literal string in %s"
                       (Lam_current_unit.get_file ());
                     assert false))
           | "js_is_nil" ->
               (match args with | e::[] -> E.is_nil e | _ -> assert false)
           | "js_is_undef" ->
               (match args with | e::[] -> E.is_undef e | _ -> assert false)
           | "js_is_nil_undef"|"js_from_nullable_def" ->
               call Js_config.js_primitive
           | "js_from_def" ->
               (match args with
                | e::[] ->
                    (match e.expression_desc with
                     | Var _ ->
                         E.econd (E.is_undef e) Js_of_lam_option.none
                           (Js_of_lam_option.some e)
                     | _ ->
                         let id = Ext_ident.create "v" in
                         let tmp = E.var id in
                         let open E in
                           seq (assign tmp e)
                             (econd (is_undef tmp) Js_of_lam_option.none
                                (Js_of_lam_option.some tmp)))
                | _ -> assert false)
           | "js_from_nullable" ->
               (match args with
                | e::[] ->
                    (match e.expression_desc with
                     | Var _ ->
                         E.econd (E.is_nil e) Js_of_lam_option.none
                           (Js_of_lam_option.some e)
                     | _ ->
                         let id = Ext_ident.create "v" in
                         let tmp = E.var id in
                         let open E in
                           seq (assign tmp e)
                             (econd (is_nil tmp) Js_of_lam_option.none
                                (Js_of_lam_option.some tmp)))
                | _ -> assert false)
           | "js_obj_set_length" ->
               (match args with
                | a::b::[] -> E.set_length a b
                | _ -> assert false)
           | _ ->
               let comment = "Missing primitve" in
               (Ext_log.warn __LOC__ "%s: %s when compiling %s\n" comment
                  prim_name (Lam_current_unit.get_file ());
                E.dump ~comment Error [E.str ~comment ~pure:false prim_name]) : 
          J.expression)[@@ocaml.doc
                         " \nThere are two things we need consider:\n1.  For some primitives we can replace caml-primitive with js primitives directly\n2.  For some standard library functions, we prefer to replace with javascript primitives\n    For example [Pervasives[\"^\"] -> ^]\n    We can collect all mli files in OCaml and replace it with an efficient javascript runtime\n"]
      end 
    module Js_array :
      sig
        val set_array :
          J.expression -> J.expression -> J.expression -> J.expression
        val ref_array : J.expression -> J.expression -> J.expression
      end =
      struct
        module E = Js_exp_make
        let set_array e e0 e1 = E.assign (E.access e e0) e1
        let ref_array e e0 = E.access e e0
      end 
    module Lam_compile_external_call :
      sig
        [@@@ocaml.text " Compile ocaml external function call to JS IR. "]
        [@@@ocaml.text
          " \n    This module define how the FFI (via `external`) works with attributes. \n    Note it will route to {!Lam_compile_global} \n    for compiling normal functions without attributes.\n "]
        [@@@ocaml.text
          " TODO: document supported attributes\n    Attributes starting with `js` are reserved\n    examples: \"bs.splice\"\n "]
        val translate :
          Lam_compile_defs.cxt ->
            Types.type_expr option Primitive.description ->
              J.expression list -> J.expression
      end =
      struct
        module E = Js_exp_make
        open Parsetree_util
        type external_module_name =
          | Single of string
          | Bind of string* string
        type 'a external_module =
          {
          txt: 'a;
          external_module_name: external_module_name option;}
        let handle_external module_name =
          match module_name with
          | Some module_name ->
              let id =
                match module_name with
                | Single module_name ->
                    ((Lam_compile_env.add_js_module module_name),
                      module_name)
                | Bind (module_name,name) ->
                    ((Lam_compile_env.add_js_module
                        ~id:(Ext_ident.create_js_module name) module_name),
                      module_name) in
              Some id
          | None  -> None
        type js_call = {
          splice: bool;
          qualifiers: string list;
          name: string;}
        type js_send = {
          splice: bool;
          name: string;}
        type js_global =
          {
          name: string;
          external_module_name: external_module_name option;}
        type js_new = {
          name: string;}
        type js_set = {
          name: string;}
        type js_get = {
          name: string;}
        type ffi =
          | Obj_create
          | Js_global of js_global
          | Js_call of js_call external_module
          | Js_send of js_send
          | Js_new of js_new external_module
          | Js_set of js_set
          | Js_get of js_get
          | Js_get_index
          | Js_set_index
          | Normal
        type prim = Types.type_expr option Primitive.description
        let handle_attributes (({ prim_attributes } as _prim) : prim) =
          (let qualifiers = ref [] in
           let call_name = ref None in
           let external_module_name = ref None in
           let is_obj = ref false in
           let js_global = ref `None in
           let js_send = ref `None in
           let js_set = ref `None in
           let js_get = ref `None in
           let js_set_index = ref false in
           let js_get_index = ref false in
           let js_splice = ref false in
           let start_loc: Location.t option ref = ref None in
           let finish_loc = ref None in
           let js_new = ref None in
           let () =
             prim_attributes |>
               (List.iter
                  (fun
                     (((x : string Asttypes.loc),pay_load) :
                       Parsetree.attribute)
                      ->
                     if (!start_loc) = None then start_loc := (Some (x.loc));
                     finish_loc := (Some (x.loc));
                     (match x.txt with
                      | "bs.val" ->
                          (match is_single_string pay_load with
                           | Some name -> js_global := (`Value name)
                           | None  -> js_global := (`Value (_prim.prim_name)))
                      | "bs.splice" -> js_splice := true
                      | "bs.send" ->
                          (match is_single_string pay_load with
                           | Some name -> js_send := (`Value name)
                           | None  -> js_send := (`Value (_prim.prim_name)))
                      | "bs.set" ->
                          (match is_single_string pay_load with
                           | Some name -> js_set := (`Value name)
                           | None  -> js_set := (`Value (_prim.prim_name)))
                      | "bs.get" ->
                          (match is_single_string pay_load with
                           | Some name -> js_get := (`Value name)
                           | None  -> js_get := (`Value (_prim.prim_name)))
                      | "bs.call" ->
                          (match is_single_string pay_load with
                           | Some name -> call_name := (Some ((x.loc), name))
                           | None  ->
                               call_name :=
                                 (Some ((x.loc), (_prim.prim_name))))
                      | "bs.module" ->
                          (match is_string_or_strings pay_load with
                           | `Single name ->
                               external_module_name := (Some (Single name))
                           | `Some (a::b::[]) ->
                               external_module_name := (Some (Bind (a, b)))
                           | `Some _ -> ()
                           | `None -> ())
                      | "bs.new" ->
                          (match is_single_string pay_load with
                           | Some x -> js_new := (Some x)
                           | None  -> js_new := (Some (_prim.prim_name)))
                      | "bs.set_index" -> js_set_index := true
                      | "bs.get_index" -> js_get_index := true
                      | "bs.obj" -> is_obj := true
                      | _ -> ()))) in
           let loc: Location.t option =
             match ((!start_loc), (!finish_loc)) with
             | (None ,None ) -> None
             | (Some { loc_start;_},Some { loc_end;_}) ->
                 Some { loc_start; loc_end; loc_ghost = false }
             | _ -> assert false in
           (loc,
             (if !is_obj
              then Obj_create
              else
                if !js_get_index
                then Js_get_index
                else
                  if !js_set_index
                  then Js_set_index
                  else
                    (match ((!call_name), (!js_global), (!js_send),
                             (!js_new), (!js_set), (!js_get))
                     with
                     | (Some (_,fn),`None,`None,_,`None,`None) ->
                         Js_call
                           {
                             txt =
                               {
                                 splice = (!js_splice);
                                 qualifiers = (!qualifiers);
                                 name = fn
                               };
                             external_module_name = (!external_module_name)
                           }
                     | (None ,`Value name,`None,_,`None,`None) ->
                         Js_global
                           {
                             name;
                             external_module_name = (!external_module_name)
                           }
                     | (None ,`None,`Value name,_,`None,`None) ->
                         Js_send { splice = (!js_splice); name }
                     | (None ,`None,`None,Some name,`None,`None) ->
                         Js_new
                           {
                             txt = { name };
                             external_module_name = (!external_module_name)
                           }
                     | (None ,`None,`None,None ,`Value name,`None) ->
                         Js_set { name }
                     | (None ,`None,`None,None ,`None,`Value name) ->
                         Js_get { name }
                     | (None ,`None,`None,None ,`None,`None) -> Normal
                     | _ ->
                         Location.raise_errorf ?loc "Ill defined attribute"))) : 
          (Location.t option* ffi))
        let ocaml_to_js last (js_splice : bool)
          ((label : string),(ty : Types.type_expr)) (arg : J.expression) =
          (if last && js_splice
           then
             match ty with
             | { desc = Tconstr (p,_,_) } when Path.same p Predef.path_array
                 ->
                 (match arg with
                  | { expression_desc = Array (ls,_mutable_flag) } -> ls
                  | _ -> assert false)
             | _ -> assert false
           else
             (match (ty, (Type_util.label_name label)) with
              | ({ desc = Tconstr (p,_,_) },_) when
                  Path.same p Predef.path_unit -> []
              | ({ desc = Tconstr (p,_,_) },_) when
                  Path.same p Predef.path_bool ->
                  (match arg.expression_desc with
                   | Number (Int { i = 0l;_}) -> [E.caml_false]
                   | Number _ -> [E.caml_true]
                   | _ -> [E.econd arg E.caml_true E.caml_false])
              | (_,`Optional label) ->
                  (match arg.expression_desc with
                   | Array (x::y::[],_mutable_flag) -> [y]
                   | Number _ -> [E.nil]
                   | _ ->
                       [E.econd arg (Js_of_lam_option.get arg) E.undefined])
              | _ -> [arg]) : E.t list)
        let translate (cxt : Lam_compile_defs.cxt)
          (({ prim_attributes; prim_ty } as prim) :
            Types.type_expr option Primitive.description)
          (args : J.expression list) =
          let (loc,ffi) = handle_attributes prim in
          match ffi with
          | Obj_create  ->
              (match prim_ty with
               | Some ty ->
                   let (_return_type,arg_types) = Type_util.list_of_arrow ty in
                   let kvs: J.property_map =
                     Ext_list.filter_map2
                       (fun (label,(ty : Types.type_expr))  ->
                          fun (arg : J.expression)  ->
                            match ((ty.desc), (Type_util.label_name label))
                            with
                            | (Tconstr (p,_,_),_) when
                                Path.same p Predef.path_unit -> None
                            | (Tconstr (p,_,_),`Label label) when
                                Path.same p Predef.path_bool ->
                                (match arg.expression_desc with
                                 | Number (Int { i = 0l;_}) ->
                                     Some ((Js_op.Key label), E.caml_false)
                                 | Number _ ->
                                     Some ((Js_op.Key label), E.caml_true)
                                 | _ ->
                                     Some
                                       ((Js_op.Key label),
                                         (E.econd arg E.caml_true
                                            E.caml_false)))
                            | (_,`Label label) ->
                                Some ((Js_op.Key label), arg)
                            | (_,`Optional label) ->
                                (match arg.expression_desc with
                                 | Array (x::y::[],_mutable_flag) ->
                                     Some ((Js_op.Key label), y)
                                 | Number _ -> None
                                 | _ ->
                                     Some
                                       ((Key label),
                                         (E.econd arg
                                            (Js_of_lam_option.get arg)
                                            E.undefined)))) arg_types args in
                   E.obj kvs
               | None  -> assert false)
          | Js_call
              { external_module_name = module_name;
                txt = { name = fn; splice = js_splice; qualifiers } }
              ->
              (match prim_ty with
               | Some ty ->
                   let (_return_type,arg_types) = Type_util.list_of_arrow ty in
                   let args =
                     Ext_list.flat_map2_last (ocaml_to_js js_splice)
                       arg_types args in
                   let qualifiers = List.rev qualifiers in
                   let fn =
                     match handle_external module_name with
                     | Some (id,_) ->
                         List.fold_left E.dot (E.var id) (qualifiers @ [fn])
                     | None  ->
                         (match qualifiers @ [fn] with
                          | y::ys -> List.fold_left E.dot (E.js_var y) ys
                          | _ -> assert false) in
                   if Type_util.is_unit _return_type
                   then
                     E.seq
                       (E.call ~info:{ arity = Full; call_info = Call_na } fn
                          args) E.unit
                   else
                     E.call ~info:{ arity = Full; call_info = Call_na } fn
                       args
               | None  -> assert false)
          | Js_new
              { external_module_name = module_name; txt = { name = fn } } ->
              (match prim_ty with
               | Some ty ->
                   let (_return_type,arg_types) = Type_util.list_of_arrow ty in
                   let args =
                     Ext_list.flat_map2_last (ocaml_to_js false) arg_types
                       args in
                   let fn =
                     match handle_external module_name with
                     | Some (id,name) -> E.external_var_dot id name fn
                     | None  -> E.js_var fn in
                   ((match cxt.st with
                     | Declare (_,id)|Assign id ->
                         Ext_ident.make_js_object id
                     | EffectCall |NeedValue  -> ());
                    E.new_ fn args)
               | None  -> assert false)
          | Js_global { name; external_module_name } ->
              (match (name, (handle_external external_module_name)) with
               | ("true",None ) -> E.js_bool true
               | ("false",None ) -> E.js_bool false
               | ("null",None ) -> E.nil
               | ("undefined",None ) -> E.undefined
               | (_,Some (id,mod_name)) ->
                   E.external_var_dot id mod_name name
               | (_,None ) -> E.var (Ext_ident.create_js name))
          | Js_send { splice = js_splice; name } ->
              (match (args, prim_ty) with
               | (self::args,Some ty) ->
                   ((let (_return_type,self_type::arg_types) =
                       Type_util.list_of_arrow ty in
                     let args =
                       Ext_list.flat_map2_last (ocaml_to_js js_splice)
                         arg_types args in
                     E.call ~info:{ arity = Full; call_info = Call_na }
                       (E.dot self name) args)[@warning "-8"])
               | _ -> Location.raise_errorf ?loc "Ill defined attribute")
          | Js_get { name } ->
              (match args with
               | obj::[] -> E.dot obj name
               | _ -> Location.raise_errorf ?loc "Ill defined attribute")
          | Js_set { name } ->
              (match args with
               | obj::v::[] -> E.assign (E.dot obj name) v
               | _ -> Location.raise_errorf ?loc "Ill defined attribute")
          | Js_get_index  ->
              (match args with
               | obj::v::[] -> Js_array.ref_array obj v
               | _ -> Location.raise_errorf ?loc "Ill defined attribute")
          | Js_set_index  ->
              (match args with
               | obj::v::value::[] -> Js_array.set_array obj v value
               | _ -> Location.raise_errorf ?loc "Ill defined attribute")
          | Normal  -> Lam_dispatch_primitive.query prim args
      end 
    module Js_of_lam_string :
      sig
        [@@@ocaml.text
          " Utilities to wrap [string] and [bytes] compilation, \n\n   this is isolated, so that we can swap different representation in the future.\n   [string] is Immutable, so there is not [set_string] method\n"]
        val ref_string : J.expression -> J.expression -> J.expression
        val ref_byte : J.expression -> J.expression -> J.expression
        val set_byte :
          J.expression -> J.expression -> J.expression -> J.expression
        val caml_char_of_int :
          ?comment:string -> J.expression -> J.expression
        val caml_char_to_int :
          ?comment:string -> J.expression -> J.expression
        val const_char : char -> J.expression
        val bytes_to_string : J.expression -> J.expression
        val bytes_of_string : J.expression -> J.expression
      end =
      struct
        module E = Js_exp_make
        module A =
          struct
            let const_char (i : char) = E.str (String.make 1 i)
            let caml_char_of_int ?comment  (v : J.expression) =
              E.char_of_int ?comment v
            let caml_char_to_int ?comment  v = E.char_to_int ?comment v
            let ref_string e e1 = E.string_access e e1
            let ref_byte e e0 = E.char_of_int (E.access e e0)
            let set_byte e e0 e1 =
              E.assign (E.access e e0) (E.char_to_int e1)
            let bytes_to_string e =
              E.runtime_call Js_config.string "bytes_to_string" [e]
            let bytes_of_string s =
              E.runtime_call Js_config.string "bytes_of_string" [s]
          end
        module B =
          struct
            let const_char (i : char) =
              E.int
                ~comment:("\"" ^
                            ((Ext_string.escaped (String.make 1 i)) ^ "\""))
                ~c:i (Int32.of_int @@ (Char.code i))
            let caml_char_of_int ?comment  (v : J.expression) = v
            let caml_char_to_int ?comment  v = v
            let ref_string e e1 = E.char_to_int (E.string_access e e1)
            let ref_byte e e0 = E.access e e0
            let set_byte e e0 e1 = E.assign (E.access e e0) e1
            [@@@ocaml.text
              "\n   Note that [String.fromCharCode] also works, but it only \n   work for small arrays, however, for {bytes_to_string} it is likely the bytes \n   will become big\n   {[\n   String.fromCharCode.apply(null,[87,97])\n   \"Wa\"\n   String.fromCharCode(87,97)\n   \"Wa\" \n   ]}\n   This does not work for large arrays\n   {[\n   String.fromCharCode.apply(null, prim = Array[1048576]) \n   Maxiume call stack size exceeded\n   ]}\n "]
            let bytes_to_string e =
              E.runtime_call Js_config.string "bytes_to_string" [e][@@ocaml.text
                                                                    "\n   Note that [String.fromCharCode] also works, but it only \n   work for small arrays, however, for {bytes_to_string} it is likely the bytes \n   will become big\n   {[\n   String.fromCharCode.apply(null,[87,97])\n   \"Wa\"\n   String.fromCharCode(87,97)\n   \"Wa\" \n   ]}\n   This does not work for large arrays\n   {[\n   String.fromCharCode.apply(null, prim = Array[1048576]) \n   Maxiume call stack size exceeded\n   ]}\n "]
            let bytes_of_string s =
              E.runtime_call Js_config.string "bytes_of_string" [s]
          end
        include B
      end 
    module Js_of_lam_record :
      sig
        [@@@ocaml.text " Utilities for compiling lambda record into JS IR "]
        val make :
          J.mutable_flag -> (string* J.expression) list -> J.expression
        val field :
          Lambda.field_dbg_info -> J.expression -> J.jsint -> J.expression
        val copy : Js_exp_make.unary_op
      end =
      struct
        module E = Js_exp_make
        let empty_record_info = Lambda.Blk_record [||]
        let make mutable_flag (args : (string* J.expression) list) =
          E.make_block ~comment:"record" E.zero_int_literal empty_record_info
            (List.map snd args) mutable_flag
        let field field_info e i =
          match field_info with
          | Lambda.Fld_na  -> E.index e i
          | Lambda.Fld_record s|Lambda.Fld_module s -> E.index ~comment:s e i
        let copy = E.array_copy[@@ocaml.doc
                                 "\n   used in [Pduprecord]\n   this is due to we encode record as an array, it is going to change\n   if we have another encoding       \n"]
      end 
    module Js_of_lam_float_record :
      sig
        [@@@ocaml.text
          " Compile a special representation in OCaml when all fields are of type [float] \n    check the invariant in {!Js_of_lam_array.make_array}\n"]
        val set_double_field :
          Lambda.set_field_dbg_info ->
            J.expression -> J.jsint -> J.expression -> J.expression
        val get_double_feild :
          Lambda.field_dbg_info -> J.expression -> J.jsint -> J.expression
      end =
      struct
        module E = Js_exp_make
        let get_double_feild field_info e i =
          match field_info with
          | Lambda.Fld_na  -> E.index e i
          | Lambda.Fld_record s|Lambda.Fld_module s -> E.index ~comment:s e i
        let set_double_field field_info e i e0 =
          let v =
            match field_info with
            | Lambda.Fld_set_na  -> E.index e i
            | Fld_record_set s -> E.index ~comment:s e i in
          E.assign v e0
      end 
    module Js_of_lam_block :
      sig
        [@@@ocaml.text
          " Utilities for creating block of lambda expression in JS IR "]
        val make_block :
          Js_op.mutable_flag ->
            Lambda.tag_info ->
              J.expression -> J.expression list -> J.expression
        val field :
          Lambda.field_dbg_info -> J.expression -> J.jsint -> J.expression
        val set_field :
          Lambda.set_field_dbg_info ->
            J.expression -> J.jsint -> J.expression -> J.expression
      end =
      struct
        module E = Js_exp_make
        let make_block mutable_flag (tag_info : Lambda.tag_info) tag args =
          match (mutable_flag, tag_info) with
          | (_,Blk_array ) ->
              Js_of_lam_array.make_array mutable_flag Pgenarray args
          | (_,_) -> E.make_block tag tag_info args mutable_flag
        let field field_info e i =
          match field_info with
          | Lambda.Fld_na  -> E.index e i
          | Lambda.Fld_record s|Lambda.Fld_module s -> E.index ~comment:s e i
        let set_field field_info e i e0 =
          let v =
            match field_info with
            | Lambda.Fld_set_na  -> E.index e i
            | Fld_record_set s -> E.index ~comment:s e i in
          E.assign v e0
      end 
    module Lam_compile_primitive :
      sig
        [@@@ocaml.text " Primitive compilation  "]
        val translate :
          Lam_compile_defs.cxt ->
            Lambda.primitive -> J.expression list -> J.expression
      end =
      struct
        module E = Js_exp_make
        let decorate_side_effect
          ({ st; should_return;_} : Lam_compile_defs.cxt) e =
          (match (st, should_return) with
           | (_,True _)|((Assign _|Declare _|NeedValue ),_) -> E.seq e E.unit
           | (EffectCall ,False ) -> e : E.t)
        let translate (({ meta = { env;_};_} as cxt) : Lam_compile_defs.cxt)
          (prim : Lambda.primitive) (args : J.expression list) =
          (match prim with
           | Pmakeblock (tag,tag_info,mutable_flag) ->
               Js_of_lam_block.make_block
                 (Js_op_util.of_lam_mutable_flag mutable_flag) tag_info
                 (E.small_int tag) args
           | Pfield (i,fld_info) ->
               (match args with
                | e::[] -> Js_of_lam_block.field fld_info e (Int32.of_int i)
                | _ -> assert false)
           | Pnegbint (Pint32 ) ->
               (match args with
                | e::[] -> E.int32_minus E.zero_int_literal e
                | _ -> assert false)
           | Pnegbint (Pnativeint ) ->
               (match args with
                | e::[] -> E.unchecked_int32_minus E.zero_int_literal e
                | _ -> assert false)
           | Pnegbint (Pint64 ) -> Js_long.neg args
           | Pnegint  ->
               (match args with
                | e::[] -> E.unchecked_int32_minus E.zero_int_literal e
                | _ -> assert false)
           | Pnegfloat  ->
               (match args with
                | e::[] -> E.float_minus E.zero_float_lit e
                | _ -> assert false)
           | Paddint |Paddbint (Pint32 ) ->
               (match args with
                | e1::e2::[] -> E.int32_add e1 e2
                | _ -> assert false)
           | Paddbint (Pnativeint ) ->
               (match args with
                | e1::e2::[] -> E.unchecked_int32_add e1 e2
                | _ -> assert false)
           | Paddbint (Pint64 ) -> Js_long.add args
           | Paddfloat  ->
               (match args with
                | e1::e2::[] -> E.float_add e1 e2
                | _ -> assert false)
           | Psubint  ->
               (match args with
                | e1::e2::[] -> E.int32_minus e1 e2
                | _ -> assert false)
           | Psubbint (Pint32 ) ->
               (match args with
                | e1::e2::[] -> E.int32_minus e1 e2
                | _ -> assert false)
           | Psubbint (Pnativeint ) ->
               (match args with
                | e1::e2::[] -> E.unchecked_int32_minus e1 e2
                | _ -> assert false)
           | Psubbint (Pint64 ) -> Js_long.sub args
           | Psubfloat  ->
               (match args with
                | e1::e2::[] -> E.float_minus e1 e2
                | _ -> assert false)
           | Pmulbint (Lambda.Pnativeint ) ->
               (match args with
                | e1::e2::[] -> E.unchecked_int32_mul e1 e2
                | _ -> assert false)
           | Pmulint |Pmulbint (Lambda.Pint32 ) ->
               (match args with
                | e1::e2::[] -> E.int32_mul e1 e2
                | _ -> assert false)
           | Pmulbint (Pint64 ) -> Js_long.mul args
           | Pmulfloat  ->
               (match args with
                | e1::e2::[] -> E.float_mul e1 e2
                | _ -> assert false)
           | Pdivfloat  ->
               (match args with
                | e1::e2::[] -> E.float_div e1 e2
                | _ -> assert false)
           | Pdivint |Pdivbint (Lambda.Pnativeint )|Pdivbint (Lambda.Pint32 )
               ->
               (match args with
                | e1::e2::[] -> E.int32_div e1 e2
                | _ -> assert false)
           | Pdivbint (Lambda.Pint64 ) -> Js_long.div args
           | Pmodint |Pmodbint (Lambda.Pnativeint )|Pmodbint (Lambda.Pint32 )
               ->
               (match args with
                | e1::e2::[] -> E.int32_mod e1 e2
                | _ -> assert false)
           | Pmodbint (Lambda.Pint64 ) -> Js_long.mod_ args
           | Plslint |Plslbint (Lambda.Pnativeint )|Plslbint (Lambda.Pint32 )
               ->
               (match args with
                | e1::e2::[] -> E.int32_lsl e1 e2
                | _ -> assert false)
           | Plslbint (Lambda.Pint64 ) -> Js_long.lsl_ args
           | Plsrbint (Lambda.Pnativeint ) ->
               (match args with
                | e1::e2::[] -> E.int32_lsr e1 e2
                | _ -> assert false)
           | Plsrint |Plsrbint (Lambda.Pint32 ) ->
               (match args with
                | e1::{
                        J.expression_desc = Number
                          (Int { i = 0l;_}|Uint 0l|Nint 0n);_}::[]
                    -> e1
                | e1::e2::[] -> E.to_int32 @@ (E.int32_lsr e1 e2)
                | _ -> assert false)
           | Plsrbint (Lambda.Pint64 ) -> Js_long.lsr_ args
           | Pasrint |Pasrbint (Lambda.Pnativeint )|Pasrbint (Lambda.Pint32 )
               ->
               (match args with
                | e1::e2::[] -> E.int32_asr e1 e2
                | _ -> assert false)
           | Pasrbint (Lambda.Pint64 ) -> Js_long.asr_ args
           | Pandint |Pandbint (Lambda.Pnativeint )|Pandbint (Lambda.Pint32 )
               ->
               (match args with
                | e1::e2::[] -> E.int32_band e1 e2
                | _ -> assert false)
           | Pandbint (Lambda.Pint64 ) -> Js_long.and_ args
           | Porint |Porbint (Lambda.Pnativeint )|Porbint (Lambda.Pint32 ) ->
               (match args with
                | e1::e2::[] -> E.int32_bor e1 e2
                | _ -> assert false)
           | Porbint (Lambda.Pint64 ) -> Js_long.or_ args
           | Pxorint |Pxorbint (Lambda.Pnativeint )|Pxorbint (Lambda.Pint32 )
               ->
               (match args with
                | e1::e2::[] -> E.int32_bxor e1 e2
                | _ -> assert false)
           | Pxorbint (Lambda.Pint64 ) -> Js_long.xor args
           | Pbintcomp (Pnativeint ,cmp)|Pfloatcomp cmp|Pintcomp cmp
             |Pbintcomp (Pint32 ,cmp) ->
               (match args with
                | e1::e2::[] -> E.int_comp cmp e1 e2
                | _ -> assert false)
           | Pbintcomp (Pint64 ,cmp) -> Js_long.comp cmp args
           | Pcvtbint ((Pint32 |Pnativeint ),Pint64 ) ->
               Js_long.of_int32 args
           | Pcvtbint (Pint64 ,Pint64 )|Pcvtbint
             ((Pnativeint |Pint32 ),(Pnativeint |Pint32 )) ->
               (match args with | e0::[] -> e0 | _ -> assert false)
           | Pcvtbint (Pint64 ,(Pnativeint |Pint32 )) ->
               Js_long.to_int32 args
           | Pintoffloat  ->
               (match args with | e::[] -> E.to_int32 e | _ -> assert false)
           | Pbintofint (Pint64 ) -> Js_long.of_int32 args
           | Pbintofint (Pnativeint |Pint32 )|Pintofbint (Pnativeint )
             |Pintofbint (Pint32 )|Pfloatofint  ->
               (match args with | e::[] -> e | _ -> assert false)
           | Pintofbint (Pint64 ) -> Js_long.to_int32 args
           | Pabsfloat  ->
               (match args with
                | e::[] -> E.math "abs" [e]
                | _ -> assert false)
           | Pnot  ->
               (match args with | e::[] -> E.not e | _ -> assert false)
           | Poffsetint n ->
               (match args with
                | e::[] -> E.int32_add e (E.small_int n)
                | _ -> assert false)
           | Poffsetref n ->
               (match args with
                | e::[] ->
                    let v = Js_of_lam_block.field Fld_na e 0l in
                    E.assign v (E.int32_add v (E.small_int n))
                | _ -> assert false)
           | Psequand  ->
               (match args with
                | e1::e2::[] -> E.and_ e1 e2
                | _ -> assert false)
           | Psequor  ->
               (match args with
                | e1::e2::[] -> E.or_ e1 e2
                | _ -> assert false)
           | Pisout  ->
               (match args with
                | range::e::[] -> E.is_out e range
                | _ -> assert false)
           | Pidentity  -> (match args with | e::[] -> e | _ -> assert false)
           | Pmark_ocaml_object  ->
               (match args with | e::[] -> e | _ -> assert false)
           | Pchar_of_int  ->
               (match args with
                | e::[] -> Js_of_lam_string.caml_char_of_int e
                | _ -> assert false)
           | Pchar_to_int  ->
               (match args with
                | e::[] -> Js_of_lam_string.caml_char_to_int e
                | _ -> assert false)
           | Pbytes_of_string  ->
               (match args with
                | e::[] -> Js_of_lam_string.bytes_of_string e
                | _ -> assert false)
           | Pbytes_to_string  ->
               (match args with
                | e::[] -> Js_of_lam_string.bytes_to_string e
                | _ -> assert false)
           | Pstringlength  ->
               (match args with
                | e::[] -> E.string_length e
                | _ -> assert false)
           | Pbyteslength  ->
               (match args with
                | e::[] -> E.bytes_length e
                | _ -> assert false)
           | Pbytessetu |Pbytessets  ->
               (match args with
                | e::e0::e1::[] ->
                    decorate_side_effect cxt
                      (Js_of_lam_string.set_byte e e0 e1)
                | _ -> assert false)
           | Pstringsetu |Pstringsets  ->
               (Ext_log.err __LOC__
                  "string is immutable, %s is not available"
                  "string.unsafe_get";
                assert false)
           | Pbytesrefu |Pbytesrefs  ->
               (match args with
                | e::e1::[] -> Js_of_lam_string.ref_byte e e1
                | _ -> assert false)
           | Pstringrefu |Pstringrefs  ->
               (match args with
                | e::e1::[] -> Js_of_lam_string.ref_string e e1
                | _ -> assert false)
           | Pignore  -> (match args with | e::[] -> e | _ -> assert false)
           | Pgetglobal i -> Lam_compile_global.get_exp (i, env, false)
           | Praise _raise_kind -> assert false
           | Prevapply _ ->
               (match args with
                | arg::f::[] -> E.call ~info:Js_call_info.dummy f [arg]
                | _ -> assert false)
           | Pdirapply _ ->
               (match args with
                | f::arg::[] -> E.call ~info:Js_call_info.dummy f [arg]
                | _ -> assert false)
           | Ploc kind -> assert false
           | Parraylength (Pgenarray )|Parraylength (Paddrarray )
             |Parraylength (Pintarray )|Parraylength (Pfloatarray ) ->
               (match args with
                | e::[] -> E.array_length e
                | _ -> assert false)
           | Psetfield (i,_,field_info) ->
               (match args with
                | e0::e1::[] ->
                    decorate_side_effect cxt
                      (Js_of_lam_block.set_field field_info e0
                         (Int32.of_int i) e1)
                | _ -> assert false)
           | Psetfloatfield (i,field_info) ->
               (match args with
                | e::e0::[] ->
                    decorate_side_effect cxt
                      (Js_of_lam_float_record.set_double_field field_info e
                         (Int32.of_int i) e0)
                | _ -> assert false)
           | Pfloatfield (i,field_info) ->
               (match args with
                | e::[] ->
                    Js_of_lam_float_record.get_double_feild field_info e
                      (Int32.of_int i)
                | _ -> assert false)
           | Parrayrefu _kind|Parrayrefs _kind ->
               (match args with
                | e::e1::[] -> Js_of_lam_array.ref_array e e1
                | _ -> assert false)
           | Pmakearray kind -> Js_of_lam_array.make_array Mutable kind args
           | Parraysetu _kind|Parraysets _kind ->
               (match args with
                | e::e0::e1::[] ->
                    (decorate_side_effect cxt) @@
                      (Js_of_lam_array.set_array e e0 e1)
                | _ -> assert false)
           | Pccall ({ prim_attributes; prim_ty } as prim) ->
               Lam_compile_external_call.translate cxt prim args
           | Pisint  ->
               (match args with
                | e::[] -> E.is_type_number e
                | _ -> assert false)
           | Pctconst ct ->
               (match ct with
                | Big_endian  ->
                    if Sys.big_endian then E.caml_true else E.caml_false
                | Word_size  -> E.small_int Sys.word_size
                | Ostype_unix  ->
                    if Sys.unix then E.caml_true else E.caml_false
                | Ostype_win32  ->
                    if Sys.win32 then E.caml_true else E.caml_false
                | Ostype_cygwin  ->
                    if Sys.cygwin then E.caml_true else E.caml_false)
           | Psetglobal _ -> assert false
           | Pduprecord ((Record_regular |Record_float ),0)|Pduprecord
             ((Record_regular |Record_float ),_) ->
               (match args with
                | e::[] -> Js_of_lam_record.copy e
                | _ -> assert false)
           | Pbigarrayref (unsafe,dimension,kind,layout) ->
               (match (dimension, kind, layout, unsafe) with
                | (1,(Pbigarray_float32 |Pbigarray_float64 |Pbigarray_sint8 
                      |Pbigarray_uint8 |Pbigarray_sint16 |Pbigarray_uint16 
                      |Pbigarray_int32 |Pbigarray_int64 |Pbigarray_caml_int 
                      |Pbigarray_native_int |Pbigarray_complex32 
                      |Pbigarray_complex64 ),Pbigarray_c_layout
                   ,_) ->
                    (match args with
                     | x::indx::[] -> Js_of_lam_array.ref_array x indx
                     | _ -> assert false)
                | (_,_,_,_) ->
                    E.runtime_call Js_config.bigarray
                      ("caml_ba_get_" ^ (string_of_int dimension)) args)
           | Pbigarrayset (unsafe,dimension,kind,layout) ->
               (match (dimension, kind, layout, unsafe) with
                | (1,(Pbigarray_float32 |Pbigarray_float64 |Pbigarray_sint8 
                      |Pbigarray_uint8 |Pbigarray_sint16 |Pbigarray_uint16 
                      |Pbigarray_int32 |Pbigarray_int64 |Pbigarray_caml_int 
                      |Pbigarray_native_int |Pbigarray_complex32 
                      |Pbigarray_complex64 ),Pbigarray_c_layout
                   ,_) ->
                    (match args with
                     | x::index::value::[] ->
                         Js_of_lam_array.set_array x index value
                     | _ -> assert false)
                | (_,_,_,_) ->
                    E.runtime_call Js_config.bigarray
                      ("caml_ba_set_" ^ (string_of_int dimension)) args)
           | Pbigarraydim i ->
               E.runtime_call Js_config.bigarray
                 ("caml_ba_dim_" ^ (string_of_int i)) args
           | Pbswap16  -> E.runtime_call Js_config.int32 "caml_bswap16" args
           | Pbbswap (Lambda.Pnativeint )|Pbbswap (Lambda.Pint32 ) ->
               E.runtime_call Js_config.int32 "caml_int32_bswap" args
           | Pbbswap (Lambda.Pint64 ) -> Js_long.swap args
           | Pstring_load_16 unsafe ->
               E.runtime_call Js_config.string "caml_string_get16" args
           | Pstring_load_32 unsafe ->
               E.runtime_call Js_config.string "caml_string_get32" args
           | Pstring_load_64 unsafe -> Js_long.get64 args
           | Plazyforce |Pbittest |Pint_as_pointer |Pstring_set_16 _
             |Pstring_set_32 _|Pstring_set_64 _|Pbigstring_load_16 _
             |Pbigstring_load_32 _|Pbigstring_load_64 _|Pbigstring_set_16 _
             |Pbigstring_set_32 _|Pbigstring_set_64 _ ->
               let comment = "Missing primitve" in
               let s = Lam_util.string_of_primitive prim in
               let warn = Printf.sprintf "%s: %s\n" comment s in
               (Ext_log.warn __LOC__ "%s" warn; E.dump Error [E.str warn]) : 
          J.expression)
      end 
    module Lam_compile_const :
      sig
        [@@@ocaml.text " Compile lambda constant to JS "]
        val translate : Lambda.structured_constant -> J.expression
      end =
      struct
        module E = Js_exp_make
        let rec translate (x : Lambda.structured_constant) =
          (match x with
           | Const_base c ->
               (match c with
                | Const_int i -> E.int (Int32.of_int i)
                | Const_char i -> Js_of_lam_string.const_char i
                | Const_int32 i -> E.int i
                | Const_int64 i -> Js_long.of_const i
                | Const_nativeint i -> E.nint i
                | Const_float f -> E.float f
                | Const_string (i,_) -> E.str i)
           | Const_pointer (c,pointer_info) ->
               E.int
                 ?comment:(Lam_compile_util.comment_of_pointer_info
                             pointer_info) (Int32.of_int c)
           | Const_block (tag,tag_info,xs) ->
               Js_of_lam_block.make_block NA tag_info (E.small_int tag)
                 (List.map translate xs)
           | Const_float_array ars ->
               Js_of_lam_array.make_array Mutable Pfloatarray
                 (List.map (fun x  -> E.float x) ars)
           | Const_immstring s -> E.str s : J.expression)
      end 
    module Lam_beta_reduce_util :
      sig
        val simple_beta_reduce :
          Ident.t list ->
            Lambda.lambda -> Lambda.lambda list -> Lambda.lambda option
      end =
      struct
        type value = {
          mutable used: bool;
          lambda: Lambda.lambda;}
        let param_hash: (Ident.t,value) Hashtbl.t = Hashtbl.create 20
        let simple_beta_reduce params body args =
          let module E = struct exception Not_simple_apply end in
            let rec find_param v opt =
              match Hashtbl.find param_hash v with
              | exp ->
                  (if exp.used
                   then raise E.Not_simple_apply
                   else exp.used <- true;
                   exp.lambda)
              | exception Not_found  -> opt in
            let rec aux acc (us : Lambda.lambda list) =
              match us with
              | [] -> List.rev acc
              | (Lvar x as a)::rest -> aux ((find_param x a) :: acc) rest
              | (Lconst _ as u)::rest -> aux (u :: acc) rest
              | _::_ -> raise E.Not_simple_apply in
            match (body : Lambda.lambda) with
            | Lprim (primitive,args') ->
                let () =
                  List.iter2
                    (fun p  ->
                       fun a  ->
                         Hashtbl.add param_hash p
                           { lambda = a; used = false }) params args in
                (match aux [] args' with
                 | us ->
                     let result =
                       Hashtbl.fold
                         (fun _param  ->
                            fun { lambda; used }  ->
                              fun code  ->
                                if not used
                                then Lambda.Lsequence (lambda, code)
                                else code) param_hash
                         (Lambda.Lprim (primitive, us)) in
                     (Hashtbl.clear param_hash; Some result)
                 | exception _ -> (Hashtbl.clear param_hash; None))
            | Lapply ((Lvar fn_name as f),args',info) ->
                let () =
                  List.iter2
                    (fun p  ->
                       fun a  ->
                         Hashtbl.add param_hash p
                           { lambda = a; used = false }) params args in
                (match aux [] args' with
                 | us ->
                     let f = find_param fn_name f in
                     let result =
                       Hashtbl.fold
                         (fun _param  ->
                            fun { lambda; used }  ->
                              fun code  ->
                                if not used
                                then Lambda.Lsequence (lambda, code)
                                else code) param_hash
                         (Lambda.Lapply (f, us, info)) in
                     (Hashtbl.clear param_hash; Some result)
                 | exception _ -> (Hashtbl.clear param_hash; None))
            | _ -> None
      end 
    module Ext_hashtbl :
      sig
        val of_list : ('a* 'b) list -> ('a,'b) Hashtbl.t
        val of_list2 : 'a list -> 'b list -> ('a,'b) Hashtbl.t
        val add_list : ('a,'b) Hashtbl.t -> ('a* 'b) list -> unit
        val add_list2 : ('a,'b) Hashtbl.t -> 'a list -> 'b list -> unit
      end =
      struct
        let of_list kvs =
          let map = Hashtbl.create 51 in
          List.iter (fun (k,v)  -> Hashtbl.add map k v) kvs; map
        let of_list2 ks vs =
          let map = Hashtbl.create 51 in
          List.iter2 (fun k  -> fun v  -> Hashtbl.add map k v) ks vs; map
        let add_list map kvs =
          List.iter (fun (k,v)  -> Hashtbl.add map k v) kvs
        let add_list2 map ks vs =
          List.iter2 (fun k  -> fun v  -> Hashtbl.add map k v) ks vs
      end 
    module Lam_beta_reduce :
      sig
        [@@@ocaml.text " Beta reduction of lambda IR "]
        val beta_reduce :
          Ident.t list ->
            Lambda.lambda -> Lambda.lambda list -> Lambda.lambda
        val propogate_beta_reduce :
          Lam_stats.meta ->
            Ident.t list ->
              Lambda.lambda -> Lambda.lambda list -> Lambda.lambda
        val refresh : Lambda.lambda -> Lambda.lambda
        val propogate_beta_reduce_with_map :
          Lam_stats.meta ->
            Lam_analysis.stats Ident_map.t ->
              Ident.t list ->
                Lambda.lambda -> Lambda.lambda list -> Lambda.lambda[@@ocaml.doc
                                                                    " \n   {[ Lam_beta_reduce.propogate_beta_reduce_with_map \n       meta param_map\n       params body args]}\n\n   [param_map] collect the usage of parameters, it's readonly\n   it can be  produced by \n\n   {[!Lam_analysis.free_variables meta.export_idents \n       (Lam_analysis.param_map_of_list params) body]}\n\n   TODO:\n   replace [propogate_beta_reduce] with such implementation \n   {[\n     let propogate_beta_reduce meta params body args = \n       let (_, param_map) = \n         Lam_analysis.is_closed_with_map Ident_set.empty params body in \n       propogate_beta_reduce_with_map meta param_map params body args  \n   ]}\n"]
      end =
      struct
        let rewrite (map : (Ident.t,_) Hashtbl.t) (lam : Lambda.lambda) =
          (let rebind i =
             let i' = Ident.rename i in
             Hashtbl.add map i (Lambda.Lvar i'); i' in
           let rec option_map op =
             match op with | None  -> None | Some x -> Some (aux x)
           and aux (lam : Lambda.lambda) =
             (match lam with
              | Lvar v -> (try Hashtbl.find map v with | Not_found  -> lam)
              | Llet (str,v,l1,l2) ->
                  let v = rebind v in
                  let l1 = aux l1 in let l2 = aux l2 in Llet (str, v, l1, l2)
              | Lletrec (bindings,body) ->
                  let bindings =
                    bindings |>
                      (List.map
                         (fun (k,l)  -> let k = rebind k in (k, (aux l)))) in
                  let body = aux body in Lletrec (bindings, body)
              | Lfunction (kind,params,body) ->
                  let params = List.map rebind params in
                  let body = aux body in Lfunction (kind, params, body)
              | Lstaticcatch (l1,(i,xs),l2) ->
                  let l1 = aux l1 in
                  let xs = List.map rebind xs in
                  let l2 = aux l2 in Lstaticcatch (l1, (i, xs), l2)
              | Lfor (ident,l1,l2,dir,l3) ->
                  let ident = rebind ident in
                  let l1 = aux l1 in
                  let l2 = aux l2 in
                  let l3 = aux l3 in Lfor (ident, (aux l1), l2, dir, l3)
              | Lconst _ -> lam
              | Lprim (prim,ll) -> Lprim (prim, (List.map aux ll))
              | Lapply (fn,args,info) ->
                  let fn = aux fn in
                  let args = List.map aux args in Lapply (fn, args, info)
              | Lswitch
                  (l,{ sw_failaction; sw_consts; sw_blocks; sw_numblocks;
                       sw_numconsts })
                  ->
                  let l = aux l in
                  Lswitch
                    (l,
                      {
                        sw_consts =
                          (List.map (fun (v,l)  -> (v, (aux l))) sw_consts);
                        sw_blocks =
                          (List.map (fun (v,l)  -> (v, (aux l))) sw_blocks);
                        sw_numconsts;
                        sw_numblocks;
                        sw_failaction = (option_map sw_failaction)
                      })
              | Lstringswitch (l,sw,d) ->
                  let l = aux l in
                  Lam_comb.stringswitch l
                    (List.map (fun (i,l)  -> (i, (aux l))) sw) (option_map d)
              | Lstaticraise (i,ls) -> Lstaticraise (i, (List.map aux ls))
              | Ltrywith (l1,v,l2) ->
                  let l1 = aux l1 in
                  let v = rebind v in let l2 = aux l2 in Ltrywith (l1, v, l2)
              | Lifthenelse (l1,l2,l3) ->
                  let l1 = aux l1 in
                  let l2 = aux l2 in let l3 = aux l3 in Lam_comb.if_ l1 l2 l3
              | Lsequence (l1,l2) ->
                  let l1 = aux l1 in let l2 = aux l2 in Lsequence (l1, l2)
              | Lwhile (l1,l2) ->
                  let l1 = aux l1 in let l2 = aux l2 in Lwhile (l1, l2)
              | Lassign (v,l) -> Lassign (v, (aux l))
              | Lsend (u,m,o,ll,v) ->
                  let m = aux m in
                  let o = aux o in
                  let ll = List.map aux ll in Lsend (u, m, o, ll, v)
              | Levent (l,event) -> let l = aux l in Levent (l, event)
              | Lifused (v,l) -> let l = aux l in Lifused (v, l) : Lambda.lambda) in
           aux lam : Lambda.lambda)
        let refresh lam = rewrite (Hashtbl.create 17) lam
        let propogate_beta_reduce (meta : Lam_stats.meta) params body args =
          match Lam_beta_reduce_util.simple_beta_reduce params body args with
          | Some x -> x
          | None  ->
              let (rest_bindings,rev_new_params) =
                List.fold_left2
                  (fun (rest_bindings,acc)  ->
                     fun old_param  ->
                       fun (arg : Lambda.lambda)  ->
                         match arg with
                         | Lconst _|Lvar _ -> (rest_bindings, (arg :: acc))
                         | _ ->
                             let p = Ident.rename old_param in
                             (((p, arg) :: rest_bindings), ((Lambda.Lvar p)
                               :: acc))) ([], []) params args in
              let new_body =
                rewrite
                  (Ext_hashtbl.of_list2 (List.rev params) rev_new_params)
                  body in
              List.fold_right
                (fun (param,(arg : Lambda.lambda))  ->
                   fun l  ->
                     let arg =
                       match arg with
                       | Lvar v ->
                           ((match Hashtbl.find meta.ident_tbl v with
                             | exception Not_found  -> ()
                             | ident_info ->
                                 Hashtbl.add meta.ident_tbl param ident_info);
                            arg)
                       | Lprim (Pgetglobal ident,[]) ->
                           Lam_compile_global.query_lambda ident meta.env
                       | Lprim (Pmakeblock (_,_,Immutable ),ls) ->
                           (Hashtbl.replace meta.ident_tbl param
                              (Lam_util.kind_of_lambda_block Normal ls);
                            arg)
                       | _ -> arg in
                     Lam_util.refine_let param arg l) rest_bindings new_body
        let propogate_beta_reduce_with_map (meta : Lam_stats.meta)
          (map : Lam_analysis.stats Ident_map.t) params body args =
          match Lam_beta_reduce_util.simple_beta_reduce params body args with
          | Some x -> x
          | None  ->
              let (rest_bindings,rev_new_params) =
                List.fold_left2
                  (fun (rest_bindings,acc)  ->
                     fun old_param  ->
                       fun (arg : Lambda.lambda)  ->
                         match arg with
                         | Lconst _|Lvar _ -> (rest_bindings, (arg :: acc))
                         | Lprim (Pgetglobal ident,[]) ->
                             let p = Ident.rename old_param in
                             (((p, arg) :: rest_bindings), ((Lambda.Lvar p)
                               :: acc))
                         | _ ->
                             if Lam_analysis.no_side_effects arg
                             then
                               (match Ident_map.find old_param map with
                                | exception Not_found  -> assert false
                                | { top = true ; times = 0 }
                                  |{ top = true ; times = 1 } ->
                                    (rest_bindings, (arg :: acc))
                                | _ ->
                                    let p = Ident.rename old_param in
                                    (((p, arg) :: rest_bindings),
                                      ((Lambda.Lvar p) :: acc)))
                             else
                               (let p = Ident.rename old_param in
                                (((p, arg) :: rest_bindings),
                                  ((Lambda.Lvar p) :: acc)))) ([], []) params
                  args in
              let new_body =
                rewrite
                  (Ext_hashtbl.of_list2 (List.rev params) rev_new_params)
                  body in
              List.fold_right
                (fun (param,(arg : Lambda.lambda))  ->
                   fun l  ->
                     let arg =
                       match arg with
                       | Lvar v ->
                           ((match Hashtbl.find meta.ident_tbl v with
                             | exception Not_found  -> ()
                             | ident_info ->
                                 Hashtbl.add meta.ident_tbl param ident_info);
                            arg)
                       | Lprim (Pgetglobal ident,[]) ->
                           Lam_compile_global.query_lambda ident meta.env
                       | Lprim (Pmakeblock (_,_,Immutable ),ls) ->
                           (Hashtbl.replace meta.ident_tbl param
                              (Lam_util.kind_of_lambda_block Normal ls);
                            arg)
                       | _ -> arg in
                     Lam_util.refine_let param arg l) rest_bindings new_body
        let beta_reduce params body args =
          match Lam_beta_reduce_util.simple_beta_reduce params body args with
          | Some x -> x
          | None  ->
              List.fold_left2
                (fun l  ->
                   fun param  -> fun arg  -> Lam_util.refine_let param arg l)
                body params args
      end 
    module Js_ast_util :
      sig
        val named_expression : J.expression -> (J.statement* Ident.t) option
      end =
      struct
        module E = Js_exp_make
        module S = Js_stmt_make
        let named_expression (e : J.expression) =
          (match e.expression_desc with
           | Var _|Bool _|Str _|Number _ -> None
           | _ ->
               let obj = Ext_ident.create Literals.tmp in
               let obj_code = S.define ~kind:Strict obj e in
               Some (obj_code, obj) : (J.statement* Ident.t) option)
      end 
    module Lam_compile :
      sig
        [@@@ocaml.text " Compile single lambda IR to JS IR  "]
        val compile_let :
          Lambda.let_kind ->
            Lam_compile_defs.cxt -> J.ident -> Lambda.lambda -> Js_output.t
        val compile_recursive_lets :
          Lam_compile_defs.cxt ->
            (Ident.t* Lambda.lambda) list -> Js_output.t
        val compile_lambda :
          Lam_compile_defs.cxt -> Lambda.lambda -> Js_output.t
      end =
      struct
        open Js_output.Ops
        module E = Js_exp_make
        module S = Js_stmt_make
        let method_cache_id = ref 1
        let rec flat_catches acc (x : Lambda.lambda) =
          (match x with
           | Lstaticcatch
               (Lstaticcatch
                (l,(code,bindings),handler),(code1,bindings1),handler1)
               when
               not @@
                 (Lam_exit_code.has_exit_code
                    (fun exit  ->
                       (exit = code1) ||
                         (List.exists (fun (c,_,_)  -> c = exit) acc))
                    handler)
               ->
               flat_catches ((code, handler, bindings) ::
                 (code1, handler1, bindings1) :: acc) l
           | Lstaticcatch (l,(code,bindings),handler) ->
               (((code, handler, bindings) :: acc), l)
           | _ -> (acc, x) : ((int* Lambda.lambda* Ident.t list) list*
                               Lambda.lambda))
        let flatten_caches x = flat_catches [] x
        let translate_dispatch = ref (fun _  -> assert false)
        type default_case =
          | Default of Lambda.lambda
          | Complete
          | NonComplete
        let rec get_exp_with_index (cxt : Lam_compile_defs.cxt) lam
          ((id : Ident.t),(pos : int),env) =
          (let f = Js_output.handle_name_tail cxt.st cxt.should_return lam in
           Lam_compile_env.find_and_add_if_not_exist (id, pos) env
             ~not_found:(fun id  ->
                           f
                             (E.str ~pure:false
                                (Printf.sprintf "Err %s %d %d" id.name
                                   id.flags pos)))
             ~found:(fun { id; name; closed_lambda }  ->
                       match (id, name, closed_lambda) with
                       | ({ name = "Sys";_},"os_type",_) ->
                           f (E.str Sys.os_type)
                       | (_,_,Some lam) when Lam_util.not_function lam ->
                           compile_lambda cxt lam
                       | _ -> f (E.ml_var_dot id name)) : Js_output.t)
        and get_exp_with_args (cxt : Lam_compile_defs.cxt) lam args_lambda
          (id : Ident.t) (pos : int) env =
          (Lam_compile_env.find_and_add_if_not_exist (id, pos) env
             ~not_found:(fun id  -> assert false)
             ~found:(fun { id; name; arity; closed_lambda;_}  ->
                       let (args_code,args) =
                         List.fold_right
                           (fun (x : Lambda.lambda)  ->
                              fun (args_code,args)  ->
                                match x with
                                | Lprim (Pgetglobal i,[]) ->
                                    (args_code,
                                      ((Lam_compile_global.get_exp
                                          (i, env, true)) :: args))
                                | _ ->
                                    (match compile_lambda
                                             {
                                               cxt with
                                               st = NeedValue;
                                               should_return = False
                                             } x
                                     with
                                     | { block = a; value = Some b } ->
                                         ((a @ args_code), (b :: args))
                                     | _ -> assert false)) args_lambda
                           ([], []) in
                       match closed_lambda with
                       | Some (Lfunction (_,params,body)) when
                           Ext_list.same_length params args_lambda ->
                           let (_,param_map) =
                             Lam_analysis.is_closed_with_map Ident_set.empty
                               params body in
                           compile_lambda cxt
                             (Lam_beta_reduce.propogate_beta_reduce_with_map
                                cxt.meta param_map params body args_lambda)
                       | _ ->
                           (Js_output.handle_block_return cxt.st
                              cxt.should_return lam args_code)
                             @@
                             ((match (id, name, args) with
                               | ({ name = "Pervasives";_},"^",e0::e1::[]) ->
                                   E.string_append e0 e1
                               | ({ name = "Pervasives";_},"print_endline",(
                                  _::[] as args)) ->
                                   E.seq (E.dump Log args) E.unit
                               | ({ name = "Pervasives";_},"prerr_endline",(
                                  _::[] as args)) ->
                                   E.seq (E.dump Error args) E.unit
                               | ({ name = "CamlinternalMod";_},"update_mod",shape::_module::_::[])
                                   when Js_of_lam_module.is_empty_shape shape
                                   -> E.unit
                               | ({ name = "CamlinternalMod";_},"init_mod",_::shape::[])
                                   when Js_of_lam_module.is_empty_shape shape
                                   -> E.dummy_obj ()
                               | _ ->
                                   let rec aux (acc : J.expression)
                                     (arity : Lam_stats.function_arities)
                                     args (len : int) =
                                     match (arity, len) with
                                     | (_,0) -> acc
                                     | (Determin (a,(x,_)::rest,b),len) ->
                                         let x = if x = 0 then 1 else x in
                                         if len >= x
                                         then
                                           let (first_part,continue) =
                                             Ext_list.take x args in
                                           aux
                                             (E.call
                                                ~info:{
                                                        arity = Full;
                                                        call_info = Call_ml
                                                      } acc first_part)
                                             (Determin (a, rest, b)) continue
                                             (len - x)
                                         else acc
                                     | (Determin (a,[],b),_) ->
                                         E.call ~info:Js_call_info.dummy acc
                                           args
                                     | (NA ,_) ->
                                         E.call ~info:Js_call_info.dummy acc
                                           args in
                                   aux (E.ml_var_dot id name) arity args
                                     (List.length args)))) : Js_output.t)
        and compile_let flag (cxt : Lam_compile_defs.cxt) id
          (arg : Lambda.lambda) =
          (match (flag, arg) with
           | (let_kind,_) ->
               compile_lambda
                 {
                   cxt with
                   st = (Declare (let_kind, id));
                   should_return = False
                 } arg : Js_output.t)[@@ocaml.doc
                                       " \n    The second return values are values which need to be wrapped using \n   [caml_update_dummy] \n"]
        and compile_recursive_let (cxt : Lam_compile_defs.cxt) (id : Ident.t)
          (arg : Lambda.lambda) =
          (match arg with
           | Lfunction (kind,params,body) ->
               let continue_label =
                 Lam_util.generate_label ~name:(id.name) () in
               ((Js_output.handle_name_tail (Declare (Alias, id)) False arg
                   (let ret: Lam_compile_defs.return_label =
                      {
                        id;
                        label = continue_label;
                        params;
                        immutable_mask =
                          (Array.make (List.length params) true);
                        new_params = Ident_map.empty;
                        triggered = false
                      } in
                    let output =
                      compile_lambda
                        {
                          cxt with
                          st = EffectCall;
                          should_return = (True (Some ret));
                          jmp_table = Lam_compile_defs.empty_handler_map
                        } body in
                    if ret.triggered
                    then
                      let body_block = Js_output.to_block output in
                      E.fun_ ~immutable_mask:(ret.immutable_mask)
                        (List.map
                           (fun x  ->
                              try Ident_map.find x ret.new_params
                              with | Not_found  -> x) params)
                        [S.while_ E.caml_true
                           (Ident_map.fold
                              (fun old  ->
                                 fun new_param  ->
                                   fun acc  ->
                                     (S.define ~kind:Alias old
                                        (E.var new_param))
                                     :: acc) ret.new_params body_block)]
                    else E.fun_ params (Js_output.to_block output))), [])
           | Lprim (Pmakeblock (0,_,_),ls) when
               List.for_all (function | Lambda.Lvar _ -> true | _ -> false)
                 ls
               ->
               ((Js_output.of_block
                   ((S.define ~kind:Variable id (E.arr Mutable [])) ::
                   (List.mapi
                      (fun i  ->
                         fun x  ->
                           match x with
                           | Lambda.Lvar lid ->
                               S.exp
                                 (Js_array.set_array (E.var id)
                                    (E.int (Int32.of_int i)) (E.var lid))
                           | _ -> assert false) ls))), [])
           | Lprim (Pmakeblock _,_) ->
               (match compile_lambda
                        { cxt with st = NeedValue; should_return = False }
                        arg
                with
                | { block = b; value = Some v } ->
                    ((Js_output.of_block
                        (b @
                           [S.exp
                              (E.runtime_call Js_config.obj_runtime
                                 "caml_update_dummy" [E.var id; v])])), 
                      [id])
                | _ -> assert false)
           | Lvar _ ->
               ((compile_lambda
                   {
                     cxt with
                     st = (Declare (Alias, id));
                     should_return = False
                   } arg), [])
           | _ ->
               ((compile_lambda
                   {
                     cxt with
                     st = (Declare (Alias, id));
                     should_return = False
                   } arg), []) : (Js_output.t* Ident.t list))[@@ocaml.doc
                                                               " \n    The second return values are values which need to be wrapped using \n   [caml_update_dummy] \n"]
        and compile_recursive_lets cxt id_args =
          (let (output_code,ids) =
             List.fold_right
               (fun (ident,arg)  ->
                  fun (acc,ids)  ->
                    let (code,declare_ids) =
                      compile_recursive_let cxt ident arg in
                    ((code ++ acc), (declare_ids @ ids))) id_args
               (Js_output.dummy, []) in
           match ids with
           | [] -> output_code
           | _ ->
               (Js_output.of_block @@
                  (List.map
                     (fun id  -> S.define ~kind:Variable id (E.dummy_obj ()))
                     ids))
                 ++ output_code : Js_output.t)
        and compile_general_cases :
          'a .
            ('a -> J.expression) ->
              (J.expression -> J.expression -> J.expression) ->
                Lam_compile_defs.cxt ->
                  (?default:J.block ->
                     ?declaration:(Lambda.let_kind* Ident.t) ->
                       _ -> 'a J.case_clause list -> J.statement)
                    ->
                    _ -> ('a* Lambda.lambda) list -> default_case -> J.block=
          fun f  ->
            fun eq  ->
              fun cxt  ->
                fun switch  ->
                  fun v  ->
                    fun table  ->
                      fun default  ->
                        let wrap (cxt : Lam_compile_defs.cxt) k =
                          let (cxt,define) =
                            match cxt.st with
                            | Declare (kind,did) ->
                                ({ cxt with st = (Assign did) },
                                  (Some (kind, did)))
                            | _ -> (cxt, None) in
                          k cxt define in
                        match (table, default) with
                        | ([],Default lam) ->
                            Js_output.to_block (compile_lambda cxt lam)
                        | ([],(Complete |NonComplete )) -> []
                        | ((id,lam)::[],Complete ) ->
                            Js_output.to_block @@ (compile_lambda cxt lam)
                        | ((id,lam)::[],NonComplete ) ->
                            (wrap cxt) @@
                              ((fun cxt  ->
                                  fun define  ->
                                    [S.if_ ?declaration:define (eq v (f id))
                                       (Js_output.to_block @@
                                          (compile_lambda cxt lam))]))
                        | ((id,lam)::[],Default x)
                          |((id,lam)::(_,x)::[],Complete ) ->
                            (wrap cxt) @@
                              ((fun cxt  ->
                                  fun define  ->
                                    let else_block =
                                      Js_output.to_block
                                        (compile_lambda cxt x) in
                                    let then_block =
                                      Js_output.to_block
                                        (compile_lambda cxt lam) in
                                    [S.if_ ?declaration:define (eq v (f id))
                                       then_block ~else_:else_block]))
                        | (_,_) ->
                            (wrap cxt) @@
                              ((fun cxt  ->
                                  fun declaration  ->
                                    let default =
                                      match default with
                                      | Complete  -> None
                                      | NonComplete  -> None
                                      | Default lam ->
                                          Some
                                            (Js_output.to_block
                                               (compile_lambda cxt lam)) in
                                    let body =
                                      (table |>
                                         (Ext_list.stable_group
                                            (fun (_,lam)  ->
                                               fun (_,lam1)  ->
                                                 Lam_analysis.eq_lambda lam
                                                   lam1)))
                                        |>
                                        (Ext_list.flat_map
                                           (fun group  ->
                                              group |>
                                                (Ext_list.map_last
                                                   (fun last  ->
                                                      fun (x,lam)  ->
                                                        if last
                                                        then
                                                          {
                                                            J.case = x;
                                                            body =
                                                              (Js_output.to_break_block
                                                                 (compile_lambda
                                                                    cxt lam))
                                                          }
                                                        else
                                                          {
                                                            case = x;
                                                            body =
                                                              ([], false)
                                                          })))) in
                                    [switch ?default ?declaration v body]))
        and compile_cases cxt =
          compile_general_cases (fun x  -> E.small_int x) E.int_equal cxt
            (fun ?default  ->
               fun ?declaration  ->
                 fun e  ->
                   fun clauses  ->
                     S.int_switch ?default ?declaration e clauses)
        and compile_string_cases cxt =
          compile_general_cases E.str E.string_equal cxt
            (fun ?default  ->
               fun ?declaration  ->
                 fun e  ->
                   fun clauses  ->
                     S.string_switch ?default ?declaration e clauses)
        and compile_lambda
          (({ st; should_return; jmp_table; meta = { env;_} } as cxt) :
            Lam_compile_defs.cxt)
          (lam : Lambda.lambda) =
          (match lam with
           | Lfunction (kind,params,body) ->
               Js_output.handle_name_tail st should_return lam
                 (E.fun_ params
                    (Js_output.to_block
                       (compile_lambda
                          {
                            cxt with
                            st = EffectCall;
                            should_return = (True None);
                            jmp_table = Lam_compile_defs.empty_handler_map
                          } body)))
           | Lapply
               (Lapply
                (an,args',({ apply_status = App_na  } as _info1)),args,(
                { apply_status = App_na  } as _info2))
               ->
               compile_lambda cxt
                 (Lapply
                    (an, (args' @ args), (Lam_util.mk_apply_info App_na)))
           | Lapply
               (Lprim
                (Pfield (n,_),(Lprim (Pgetglobal id,[]))::[]),args_lambda,_info)
               -> get_exp_with_args cxt lam args_lambda id n env
           | Lapply (fn,args_lambda,info) ->
               ((let (args_code,fn_code::args) =
                   List.fold_right
                     (fun (x : Lambda.lambda)  ->
                        fun (args_code,fn_code)  ->
                          match x with
                          | Lprim (Pgetglobal ident,[]) ->
                              (args_code,
                                ((Lam_compile_global.get_exp
                                    (ident, env, true)) :: fn_code))
                          | _ ->
                              (match compile_lambda
                                       {
                                         cxt with
                                         st = NeedValue;
                                         should_return = False
                                       } x
                               with
                               | { block = a; value = Some b } ->
                                   ((a @ args_code), (b :: fn_code))
                               | _ -> assert false)) (fn :: args_lambda)
                     ([], []) in
                 (match (fn, should_return) with
                  | (Lvar id',True (Some ({ id; label; params;_} as ret)))
                      when Ident.same id id' ->
                      (ret.triggered <- true;
                       (let block =
                          args_code @
                            ((let (_,assigned_params,new_params) =
                                List.fold_left2
                                  (fun (i,assigns,new_params)  ->
                                     fun param  ->
                                       fun (arg : J.expression)  ->
                                         match arg with
                                         | { expression_desc = Var (Id x);_}
                                             when Ident.same x param ->
                                             ((i + 1), assigns, new_params)
                                         | _ ->
                                             let (new_param,m) =
                                               match Ident_map.find param
                                                       ret.new_params
                                               with
                                               | exception Not_found  ->
                                                   ((ret.immutable_mask).(i)
                                                    <- false;
                                                    (let v =
                                                       Ext_ident.create
                                                         ("_" ^
                                                            param.Ident.name) in
                                                     (v,
                                                       (Ident_map.add param v
                                                          new_params))))
                                               | v -> (v, new_params) in
                                             ((i + 1), ((new_param, arg) ::
                                               assigns), m))
                                  (0, [], Ident_map.empty) params args in
                              let () =
                                ret.new_params <-
                                  let open Ident_map in
                                    merge_disjoint new_params ret.new_params in
                              assigned_params |>
                                (List.map
                                   (fun (param,arg)  -> S.assign param arg)))
                               @ [S.continue ()]) in
                        Js_output.of_block ~finished:True block))
                  | _ ->
                      Js_output.handle_block_return st should_return lam
                        args_code
                        (E.call
                           ~info:(match (fn, info) with
                                  | (_,{ apply_status = App_ml_full  }) ->
                                      { arity = Full; call_info = Call_ml }
                                  | (_,{ apply_status = App_js_full  }) ->
                                      { arity = Full; call_info = Call_na }
                                  | (_,{ apply_status = App_na  }) ->
                                      { arity = NA; call_info = Call_ml })
                           fn_code args)))[@warning "-8"])
           | Llet (let_kind,id,arg,body) ->
               let args_code = compile_let let_kind cxt id arg in
               args_code ++ (compile_lambda cxt body)
           | Lletrec (id_args,body) ->
               let v = compile_recursive_lets cxt id_args in
               v ++ (compile_lambda cxt body)
           | Lvar id ->
               Js_output.handle_name_tail st should_return lam (E.var id)
           | Lconst c ->
               Js_output.handle_name_tail st should_return lam
                 (Lam_compile_const.translate c)
           | Lprim (Pfield (n,_),(Lprim (Pgetglobal id,[]))::[]) ->
               get_exp_with_index cxt lam (id, n, env)
           | Lprim (Praise _raise_kind,e::[]) ->
               (match compile_lambda
                        { cxt with should_return = False; st = NeedValue } e
                with
                | { block = b; value = Some v } ->
                    Js_output.make (b @ [S.throw v]) ~value:E.undefined
                      ~finished:True
                | { value = None ;_} -> assert false)
           | Lprim (Psequand ,l::r::[]) ->
               (match cxt with
                | { should_return = True _ } ->
                    compile_lambda cxt (Lam_comb.if_ l r Lam_util.lam_false)
                | _ ->
                    let (l_block,l_expr) =
                      match compile_lambda
                              {
                                cxt with
                                st = NeedValue;
                                should_return = False
                              } l
                      with
                      | { block = a; value = Some b } -> (a, b)
                      | _ -> assert false in
                    let (r_block,r_expr) =
                      match compile_lambda
                              {
                                cxt with
                                st = NeedValue;
                                should_return = False
                              } r
                      with
                      | { block = a; value = Some b } -> (a, b)
                      | _ -> assert false in
                    let args_code = l_block @ r_block in
                    let exp = E.and_ l_expr r_expr in
                    Js_output.handle_block_return st should_return lam
                      args_code exp)
           | Lprim (Psequor ,l::r::[]) ->
               (match cxt with
                | { should_return = True _ } ->
                    (compile_lambda cxt) @@
                      (Lam_comb.if_ l Lam_util.lam_true r)
                | _ ->
                    let (l_block,l_expr) =
                      match compile_lambda
                              {
                                cxt with
                                st = NeedValue;
                                should_return = False
                              } l
                      with
                      | { block = a; value = Some b } -> (a, b)
                      | _ -> assert false in
                    let (r_block,r_expr) =
                      match compile_lambda
                              {
                                cxt with
                                st = NeedValue;
                                should_return = False
                              } r
                      with
                      | { block = a; value = Some b } -> (a, b)
                      | _ -> assert false in
                    let args_code = l_block @ r_block in
                    let exp = E.or_ l_expr r_expr in
                    Js_output.handle_block_return st should_return lam
                      args_code exp)
           | Lprim (prim,args_lambda) ->
               let cont args_code exp =
                 Js_output.handle_block_return st should_return lam args_code
                   exp in
               (match prim with
                | Pccall { prim_name = "js_debugger";_} ->
                    cont [S.debugger] E.unit
                | Pccall { prim_name = name } when
                    Ext_string.starts_with name "js_fn_" ->
                    let (arity,kind) =
                      let mk =
                        Ext_string.starts_with_and_number name ~offset:6
                          "mk_" in
                      if mk < 0
                      then
                        let run =
                          Ext_string.starts_with_and_number name ~offset:6
                            "run_" in
                        (run, `Run)
                      else (mk, `Mk) in
                    if kind = `Run
                    then
                      (match args_lambda with
                       | fn::rest ->
                           (compile_lambda cxt) @@
                             (Lambda.Lapply
                                (fn, rest,
                                  {
                                    apply_loc = Location.none;
                                    apply_status = App_js_full
                                  }))
                       | _ -> assert false)
                    else
                      (match args_lambda with
                       | fn::[] ->
                           if arity = 0
                           then
                             compile_lambda cxt
                               (Lfunction
                                  (Lambda.Curried, [],
                                    (Lambda.Lapply
                                       (fn, [Lam_util.lam_unit],
                                         Lam_util.default_apply_info))))
                           else
                             (match fn with
                              | Lambda.Lfunction (kind,args,body) ->
                                  let len = List.length args in
                                  if len = arity
                                  then compile_lambda cxt fn
                                  else
                                    if len > arity
                                    then
                                      (let (first,rest) =
                                         Ext_list.take arity args in
                                       compile_lambda cxt
                                         (Lambda.Lfunction
                                            (kind, first,
                                              (Lambda.Lfunction
                                                 (kind, rest, body)))))
                                    else
                                      compile_lambda cxt
                                        (Lam_util.eta_conversion arity
                                           Lam_util.default_apply_info fn [])
                              | _ ->
                                  compile_lambda cxt
                                    (Lam_util.eta_conversion arity
                                       Lam_util.default_apply_info fn []))
                       | _ -> assert false)
                | _ ->
                    let (args_block,args_expr) =
                      Ext_list.split_map
                        (fun (x : Lambda.lambda)  ->
                           match compile_lambda
                                   {
                                     cxt with
                                     st = NeedValue;
                                     should_return = False
                                   } x
                           with
                           | { block = a; value = Some b } -> (a, b)
                           | _ -> assert false) args_lambda in
                    let args_code = List.concat args_block in
                    let exp =
                      Lam_compile_primitive.translate cxt prim args_expr in
                    cont args_code exp)
           | Lsequence (l1,l2) ->
               let output_l1 =
                 compile_lambda
                   { cxt with st = EffectCall; should_return = False } l1 in
               let output_l2 = compile_lambda cxt l2 in
               output_l1 ++ output_l2
           | Lifthenelse (p,t_br,f_br) ->
               (match compile_lambda
                        { cxt with st = NeedValue; should_return = False } p
                with
                | { block = b; value = Some e } ->
                    (match (st, should_return,
                             (compile_lambda { cxt with st = NeedValue } t_br),
                             (compile_lambda { cxt with st = NeedValue } f_br))
                     with
                     | (NeedValue
                        ,_,{ block = []; value = Some out1 },{ block = [];
                                                               value = Some
                                                                 out2
                                                               })
                         -> Js_output.make b ~value:(E.econd e out1 out2)
                     | (NeedValue ,_,_,_) ->
                         let id = Ext_ident.gen_js () in
                         (match ((compile_lambda
                                    { cxt with st = (Assign id) } t_br),
                                  (compile_lambda
                                     { cxt with st = (Assign id) } f_br))
                          with
                          | (out1,out2) ->
                              Js_output.make
                                (((S.declare_variable ~kind:Variable id) ::
                                   b) @
                                   [S.if_ e (Js_output.to_block out1)
                                      ~else_:(Js_output.to_block out2)])
                                ~value:(E.var id))
                     | (Declare
                        (kind,id),_,{ block = []; value = Some out1 },
                        { block = []; value = Some out2 }) ->
                         Js_output.make
                           [S.define ~kind id (E.econd e out1 out2)]
                     | (Declare (kind,id),_,_,_) ->
                         Js_output.make
                           (b @
                              [S.if_ ~declaration:(kind, id) e
                                 (Js_output.to_block @@
                                    (compile_lambda
                                       { cxt with st = (Assign id) } t_br))
                                 ~else_:(Js_output.to_block @@
                                           (compile_lambda
                                              { cxt with st = (Assign id) }
                                              f_br))])
                     | (Assign
                        id,_,{ block = []; value = Some out1 },{ block = [];
                                                                 value = Some
                                                                   out2
                                                                 })
                         ->
                         Js_output.make [S.assign id (E.econd e out1 out2)]
                     | (EffectCall ,True
                        _,{ block = []; value = Some out1 },{ block = [];
                                                              value = Some
                                                                out2
                                                              })
                         ->
                         Js_output.make [S.return (E.econd e out1 out2)]
                           ~finished:True
                     | (EffectCall ,False
                        ,{ block = []; value = Some out1 },{ block = [];
                                                             value = Some
                                                               out2
                                                             })
                         ->
                         (match ((Js_exp_make.extract_non_pure out1),
                                  (Js_exp_make.extract_non_pure out2))
                          with
                          | (None ,None ) -> Js_output.make b
                          | (Some out1,Some out2) ->
                              Js_output.make b ~value:(E.econd e out1 out2)
                          | (Some out1,None ) ->
                              Js_output.make (b @ [S.if_ e [S.exp out1]])
                          | (None ,Some out2) ->
                              Js_output.make @@
                                (b @ [S.if_ (E.not e) [S.exp out2]]))
                     | (EffectCall ,False
                        ,{ block = []; value = Some out1 },_) ->
                         if Js_analyzer.no_side_effect_expression out1
                         then
                           Js_output.make
                             (b @
                                [S.if_ (E.not e)
                                   (Js_output.to_block @@
                                      (compile_lambda cxt f_br))])
                         else
                           Js_output.make
                             (b @
                                [S.if_ e
                                   (Js_output.to_block @@
                                      (compile_lambda cxt t_br))
                                   ~else_:(Js_output.to_block @@
                                             (compile_lambda cxt f_br))])
                     | (EffectCall ,False
                        ,_,{ block = []; value = Some out2 }) ->
                         let else_ =
                           if Js_analyzer.no_side_effect_expression out2
                           then None
                           else
                             Some
                               (Js_output.to_block @@
                                  (compile_lambda cxt f_br)) in
                         Js_output.make
                           (b @
                              [S.if_ e
                                 (Js_output.to_block @@
                                    (compile_lambda cxt t_br)) ?else_])
                     | ((Assign _|EffectCall ),_,_,_) ->
                         let then_output =
                           Js_output.to_block @@ (compile_lambda cxt t_br) in
                         let else_output =
                           Js_output.to_block @@ (compile_lambda cxt f_br) in
                         Js_output.make
                           (b @ [S.if_ e then_output ~else_:else_output]))
                | _ -> assert false)
           | Lstringswitch (l,cases,default) ->
               (match compile_lambda
                        { cxt with should_return = False; st = NeedValue } l
                with
                | { block; value = Some e } ->
                    let default =
                      match default with
                      | Some x -> Default x
                      | None  -> Complete in
                    (match st with
                     | NeedValue  ->
                         let v = Ext_ident.gen_js () in
                         Js_output.make
                           (block @
                              (compile_string_cases
                                 { cxt with st = (Declare (Variable, v)) } e
                                 cases default)) ~value:(E.var v)
                     | _ ->
                         Js_output.make
                           (block @
                              (compile_string_cases cxt e cases default)))
                | _ -> assert false)
           | Lswitch
               (lam,{ sw_numconsts; sw_consts; sw_numblocks; sw_blocks;
                      sw_failaction = default })
               ->
               let default: default_case =
                 match default with | None  -> Complete | Some x -> Default x in
               let compile_whole (({ st;_} as cxt) : Lam_compile_defs.cxt) =
                 match (sw_numconsts, sw_numblocks,
                         (compile_lambda
                            { cxt with should_return = False; st = NeedValue
                            } lam))
                 with
                 | (0,_,{ block; value = Some e }) ->
                     compile_cases cxt (E.tag e) sw_blocks default
                 | (_,0,{ block; value = Some e }) ->
                     compile_cases cxt e sw_consts default
                 | (_,_,{ block; value = Some e }) ->
                     let dispatch e =
                       [S.if_ (E.is_type_number e)
                          (compile_cases cxt e sw_consts default)
                          ~else_:(compile_cases cxt (E.tag e) sw_blocks
                                    default)] in
                     (match e.expression_desc with
                      | J.Var _ -> dispatch e
                      | _ ->
                          let v = Ext_ident.gen_js () in
                          (S.define ~kind:Variable v e) ::
                            (dispatch (E.var v)))
                 | (_,_,{ value = None ;_}) -> assert false in
               (match st with
                | NeedValue  ->
                    let v = Ext_ident.gen_js () in
                    Js_output.make ((S.declare_variable ~kind:Variable v) ::
                      (compile_whole { cxt with st = (Assign v) }))
                      ~value:(E.var v)
                | Declare (kind,id) ->
                    Js_output.make ((S.declare_variable ~kind id) ::
                      (compile_whole { cxt with st = (Assign id) }))
                | EffectCall |Assign _ -> Js_output.make (compile_whole cxt))
           | Lstaticraise (i,largs) ->
               (match Lam_compile_defs.HandlerMap.find i cxt.jmp_table with
                | { exit_id; args; order_id } ->
                    let args_code =
                      Js_output.concat @@
                        (List.map2
                           (fun (x : Lambda.lambda)  ->
                              fun (arg : Ident.t)  ->
                                match x with
                                | Lvar id ->
                                    Js_output.make [S.assign arg (E.var id)]
                                | _ ->
                                    compile_lambda
                                      {
                                        cxt with
                                        st = (Assign arg);
                                        should_return = False
                                      } x) largs (args : Ident.t list)) in
                    args_code ++
                      (Js_output.make
                         [S.assign exit_id (E.small_int order_id)]
                         ~value:E.undefined)
                | exception Not_found  ->
                    Js_output.make [S.unknown_lambda ~comment:"error" lam])
           | Lstaticcatch _ ->
               let (code_table,body) = flatten_caches lam in
               let exit_id = Ext_ident.gen_js ~name:"exit" () in
               let exit_expr = E.var exit_id in
               let bindings =
                 Ext_list.flat_map (fun (_,_,bindings)  -> bindings)
                   code_table in
               let (jmp_table,handlers) =
                 Lam_compile_defs.add_jmps (exit_id, code_table) jmp_table in
               let declares =
                 (S.define ~kind:Variable exit_id E.zero_int_literal) ::
                 (List.map (fun x  -> S.declare_variable ~kind:Variable x)
                    bindings) in
               (match st with
                | NeedValue  ->
                    let v = Ext_ident.gen_js () in
                    let lbody =
                      compile_lambda { cxt with jmp_table; st = (Assign v) }
                        body in
                    ((Js_output.make ((S.declare_variable ~kind:Variable v)
                        :: declares))
                       ++ lbody)
                      ++
                      (Js_output.make
                         (compile_cases
                            { cxt with st = (Assign v); jmp_table } exit_expr
                            handlers NonComplete) ~value:(E.var v))
                | Declare (kind,id) ->
                    let declares = (S.declare_variable ~kind id) :: declares in
                    let lbody =
                      compile_lambda { cxt with jmp_table; st = (Assign id) }
                        body in
                    ((Js_output.make declares) ++ lbody) ++
                      (Js_output.make
                         (compile_cases
                            { cxt with jmp_table; st = (Assign id) }
                            exit_expr handlers NonComplete))
                | EffectCall |Assign _ ->
                    let lbody = compile_lambda { cxt with jmp_table } body in
                    ((Js_output.make declares) ++ lbody) ++
                      (Js_output.make
                         (compile_cases { cxt with jmp_table } exit_expr
                            handlers NonComplete)))
           | Lwhile (p,body) ->
               (match compile_lambda
                        { cxt with st = NeedValue; should_return = False } p
                with
                | { block; value = Some e } ->
                    let e =
                      match block with | [] -> e | _ -> E.of_block block e in
                    let block =
                      [S.while_ e
                         (Js_output.to_block @@
                            (compile_lambda
                               {
                                 cxt with
                                 st = EffectCall;
                                 should_return = False
                               } body))] in
                    (match (st, should_return) with
                     | (Declare (_kind,x),_) ->
                         Js_output.make (block @ [S.declare_unit x])
                     | (Assign x,_) ->
                         Js_output.make (block @ [S.assign_unit x])
                     | (EffectCall ,True _) ->
                         Js_output.make (block @ [S.return_unit ()])
                           ~finished:True
                     | (EffectCall ,_) -> Js_output.make block
                     | (NeedValue ,_) -> Js_output.make block ~value:E.unit)
                | _ -> assert false)
           | Lfor (id,start,finish,direction,body) ->
               let block =
                 match ((compile_lambda
                           { cxt with st = NeedValue; should_return = False }
                           start),
                         (compile_lambda
                            { cxt with st = NeedValue; should_return = False
                            } finish))
                 with
                 | ({ block = b1; value = Some e1 },{ block = b2;
                                                      value = Some e2 })
                     ->
                     (match (b1, b2) with
                      | (_,[]) ->
                          b1 @
                            [S.for_ (Some e1) e2 id direction
                               (Js_output.to_block @@
                                  (compile_lambda
                                     {
                                       cxt with
                                       should_return = False;
                                       st = EffectCall
                                     } body))]
                      | (_,_) when Js_analyzer.no_side_effect_expression e1
                          ->
                          b1 @
                            (b2 @
                               [S.for_ (Some e1) e2 id direction
                                  (Js_output.to_block @@
                                     (compile_lambda
                                        {
                                          cxt with
                                          should_return = False;
                                          st = EffectCall
                                        } body))])
                      | (_,_) ->
                          b1 @
                            (((S.define ~kind:Variable id e1) :: b2) @
                               [S.for_ None e2 id direction
                                  (Js_output.to_block @@
                                     (compile_lambda
                                        {
                                          cxt with
                                          should_return = False;
                                          st = EffectCall
                                        } body))]))
                 | _ -> assert false in
               (match (st, should_return) with
                | (EffectCall ,False ) -> Js_output.make block
                | (EffectCall ,True _) ->
                    Js_output.make (block @ [S.return_unit ()])
                      ~finished:True
                | ((Declare _|Assign _),True _) ->
                    Js_output.make [S.unknown_lambda lam]
                | (Declare (_kind,x),False ) ->
                    Js_output.make (block @ [S.declare_unit x])
                | (Assign x,False ) ->
                    Js_output.make (block @ [S.assign_unit x])
                | (NeedValue ,_) -> Js_output.make block ~value:E.unit)
           | Lassign (id,lambda) ->
               let block =
                 match lambda with
                 | Lprim (Poffsetint v,(Lvar id')::[]) when Ident.same id id'
                     ->
                     [S.exp
                        (E.assign (E.var id)
                           (E.int32_add (E.var id) (E.small_int v)))]
                 | _ ->
                     (match compile_lambda
                              {
                                cxt with
                                st = NeedValue;
                                should_return = False
                              } lambda
                      with
                      | { block = b; value = Some v } -> b @ [S.assign id v]
                      | _ -> assert false) in
               (match (st, should_return) with
                | (EffectCall ,False ) -> Js_output.make block
                | (EffectCall ,True _) ->
                    Js_output.make (block @ [S.return_unit ()])
                      ~finished:True
                | ((Declare _|Assign _),True _) ->
                    Js_output.make [S.unknown_lambda lam]
                | (Declare (_kind,x),False ) ->
                    Js_output.make (block @ [S.declare_unit x])
                | (Assign x,False ) ->
                    Js_output.make (block @ [S.assign_unit x])
                | (NeedValue ,_) -> Js_output.make block ~value:E.unit)
           | Ltrywith
             ((Lprim
                 (Pccall { prim_name = "caml_sys_getenv";_},(Lconst _)::[])
                 as body),id,Lifthenelse
              (Lprim
               (Pintcomp (Ceq ),(Lvar id2)::(Lprim
                (Pgetglobal { name = "Not_found" },_))::[]),cont,_reraise))
             |Ltrywith
             ((Lprim
                 (Pccall { prim_name = "caml_sys_getenv";_},(Lconst _)::[])
                 as body),id,Lifthenelse
              (Lprim
               (Pintcomp (Ceq ),(Lprim
                (Pgetglobal { name = "Not_found";_},_))::(Lvar id2)::[]),cont,_reraise))
               when Ident.same id id2 ->
               compile_lambda cxt (Ltrywith (body, id, cont))
           | Ltrywith (lam,id,catch) ->
               let aux st =
                 [S.try_
                    (Js_output.to_block (compile_lambda { cxt with st } lam))
                    ~with_:(id,
                             (Js_output.to_block @@
                                (compile_lambda { cxt with st } catch)))] in
               (match st with
                | NeedValue  ->
                    let v = Ext_ident.gen_js () in
                    Js_output.make ((S.declare_variable ~kind:Variable v) ::
                      (aux (Assign v))) ~value:(E.var v)
                | Declare (kind,id) ->
                    Js_output.make ((S.declare_variable ~kind id) ::
                      (aux (Assign id)))
                | Assign _|EffectCall  -> Js_output.make (aux st))
           | Lsend (meth_kind,met,obj,args,loc) ->
               (match (met :: obj :: args) |>
                        (Ext_list.split_map
                           (fun (x : Lambda.lambda)  ->
                              match x with
                              | Lprim (Pgetglobal i,[]) ->
                                  ([],
                                    (Lam_compile_global.get_exp
                                       (i, env, true)))
                              | Lprim (Pccall { prim_name;_},[]) ->
                                  ([],
                                    (E.var (Ext_ident.create_js prim_name)))
                              | _ ->
                                  (match compile_lambda
                                           {
                                             cxt with
                                             st = NeedValue;
                                             should_return = False
                                           } x
                                   with
                                   | { block = a; value = Some b } -> (a, b)
                                   | _ -> assert false)))
                with
                | (_,([]|_::[])) -> assert false
                | (args_code,label::obj'::args) ->
                    let cont =
                      Js_output.handle_block_return st should_return lam
                        (List.concat args_code) in
                    let cont2 obj_code v =
                      Js_output.handle_block_return st should_return lam
                        (obj_code :: (List.concat args_code)) v in
                    let cont3 obj' k =
                      match Js_ast_util.named_expression obj' with
                      | None  -> cont (k obj')
                      | Some (obj_code,v) ->
                          let obj' = E.var v in cont2 obj_code (k obj') in
                    (match meth_kind with
                     | Self  ->
                         cont3 obj'
                           (fun obj'  ->
                              E.call ~info:Js_call_info.dummy
                                (Js_of_lam_array.ref_array
                                   (Js_of_lam_record.field Fld_na obj' 0l)
                                   label) (obj' :: args))
                     | Cached |Public (None ) ->
                         let get =
                           E.runtime_ref Js_config.oo
                             "caml_get_public_method" in
                         let cache = !method_cache_id in
                         let () = incr method_cache_id in
                         cont3 obj'
                           (fun obj'  ->
                              E.call ~info:Js_call_info.dummy
                                (E.call ~info:Js_call_info.dummy get
                                   [obj'; label; E.small_int cache]) (obj' ::
                                args))
                     | Public (Some name) ->
                         let js_no_arity obj' name =
                           match args with
                           | [] -> cont @@ (E.dot obj' name)
                           | _ ->
                               cont3 obj'
                                 (fun obj'  -> E.bind_call obj' name args) in
                         (match ((Lam_methname.process name), obj) with
                          | ((Js_read_index ,_name),_) ->
                              (match args with
                               | [] ->
                                   let i = Ext_ident.create "i" in
                                   (cont3 obj') @@
                                     ((fun obj'  ->
                                         E.fun_ [i]
                                           (let open S in
                                              [return
                                                 (Js_array.ref_array obj'
                                                    (E.var i))])))
                               | x::[] -> cont @@ (Js_array.ref_array obj' x)
                               | x::rest ->
                                   cont @@
                                     (E.call ~info:Js_call_info.dummy
                                        (Js_array.ref_array obj' x) rest))
                          | ((Js_write_index ,_name),_) ->
                              (match args with
                               | [] ->
                                   let i = Ext_ident.create "i" in
                                   let v = Ext_ident.create "v" in
                                   (cont3 obj') @@
                                     ((fun obj'  ->
                                         E.fun_ [i; v]
                                           (let open S in
                                              [return
                                                 (E.seq
                                                    (Js_array.set_array obj'
                                                       (E.var i) (E.var v))
                                                    E.unit)])))
                               | i::[] ->
                                   let v = Ext_ident.create "v" in
                                   (cont3 obj') @@
                                     ((fun obj'  ->
                                         E.fun_ [v]
                                           (let open S in
                                              [return
                                                 (E.seq
                                                    (Js_array.set_array obj'
                                                       i (E.var v)) E.unit)])))
                               | x::y::[] ->
                                   cont @@ (Js_array.set_array obj' x y)
                               | x::y::rest ->
                                   cont @@
                                     (E.call ~info:Js_call_info.dummy
                                        (Js_array.set_array obj' x y) rest))
                          | ((Js_write ,name),_) ->
                              (match args with
                               | [] ->
                                   let v = Ext_ident.create "v" in
                                   (cont3 obj') @@
                                     ((fun obj'  ->
                                         E.fun_ [v]
                                           (let open S in
                                              [return
                                                 (E.assign (E.dot obj' name)
                                                    (E.var v))])))
                               | v::[] ->
                                   cont @@ (E.assign (E.dot obj' name) v)
                               | _::_ -> assert false)
                          | ((Js_read ,name),_) -> cont @@ (E.dot obj' name)
                          | ((Js (Some arity),name),_) ->
                              let (args,n,rest) =
                                Ext_list.try_take arity args in
                              if n = arity
                              then
                                (match rest with
                                 | [] ->
                                     cont @@
                                       (E.call
                                          ~info:{
                                                  arity = Full;
                                                  call_info = Call_na
                                                } (E.dot obj' name) args)
                                 | _ ->
                                     cont @@
                                       (E.call ~info:Js_call_info.dummy
                                          (E.call
                                             ~info:{
                                                     arity = Full;
                                                     call_info = Call_na
                                                   } (E.dot obj' name) args)
                                          rest))
                              else
                                (let rest =
                                   Ext_list.init (arity - n)
                                     (fun i  ->
                                        Ext_ident.create Literals.prim) in
                                 (cont3 obj') @@
                                   (fun obj'  ->
                                      E.fun_ rest
                                        (let open S in
                                           [return
                                              (E.call
                                                 ~info:{
                                                         arity = Full;
                                                         call_info = Call_na
                                                       } (E.dot obj' name)
                                                 (args @
                                                    (List.map E.var rest)))])))
                          | ((Js (None ),p_name),_) ->
                              js_no_arity obj' p_name
                          | (_,Lprim (Pccall { prim_name = _;_},[])) ->
                              js_no_arity obj' name
                          | (_,Lvar id) when Ext_ident.is_js_object id ->
                              js_no_arity obj' name
                          | (((Ml _|Unknown _),_),_) ->
                              let cache = !method_cache_id in
                              (incr method_cache_id;
                               cont3 obj'
                                 (fun obj'  ->
                                    E.public_method_call name obj' label
                                      (Int32.of_int cache) args)))))
           | Levent (lam,_lam_event) -> compile_lambda cxt lam
           | Lifused (_,lam) -> compile_lambda cxt lam : Js_output.t)
      end 
    module Lam_stats_util :
      sig
        [@@@ocaml.text " Utilities for lambda analysis "]
        val pp_alias_tbl : Format.formatter -> Lam_stats.alias_tbl -> unit
        val pp_arities :
          Format.formatter -> Lam_stats.function_arities -> unit
        val get_arity :
          Lam_stats.meta -> Lambda.lambda -> Lam_stats.function_arities
      end =
      struct
        let pp = Format.fprintf
        let pp_arities (fmt : Format.formatter)
          (x : Lam_stats.function_arities) =
          match x with
          | NA  -> pp fmt "?"
          | Determin (b,ls,tail) ->
              (pp fmt "@[";
               if not b then pp fmt "~";
               pp fmt "[";
               Format.pp_print_list
                 ~pp_sep:(fun fmt  -> fun ()  -> pp fmt ",")
                 (fun fmt  -> fun (x,_)  -> Format.pp_print_int fmt x) fmt ls;
               if tail then pp fmt "@ *";
               pp fmt "]@]")
        let pp_arities_tbl (fmt : Format.formatter)
          (arities_tbl : (Ident.t,Lam_stats.function_arities ref) Hashtbl.t)
          =
          Hashtbl.fold
            (fun (i : Ident.t)  ->
               fun (v : Lam_stats.function_arities ref)  ->
                 fun _  ->
                   pp Format.err_formatter "@[%s -> %a@]@." i.name pp_arities
                     (!v)) arities_tbl ()
        let pp_alias_tbl fmt (tbl : Lam_stats.alias_tbl) =
          Hashtbl.iter
            (fun k  ->
               fun v  -> pp fmt "@[%a -> %a@]@." Ident.print k Ident.print v)
            tbl
        let merge (((n : int),params) as y) (x : Lam_stats.function_arities)
          =
          (match x with
           | NA  -> Determin (false, [y], false)
           | Determin (b,xs,tail) -> Determin (b, (y :: xs), tail) : 
          Lam_stats.function_arities)
        let rec get_arity (meta : Lam_stats.meta) (lam : Lambda.lambda) =
          (match lam with
           | Lconst _ -> Determin (true, [], false)
           | Lvar v ->
               (match Hashtbl.find meta.ident_tbl v with
                | exception Not_found  -> (NA : Lam_stats.function_arities)
                | Function { arity;_} -> arity
                | _ -> (NA : Lam_stats.function_arities))
           | Llet (_,_,_,l) -> get_arity meta l
           | Lprim
               (Pccall
                { prim_name = "js_pure_expr"; prim_attributes },(Lconst
                (Const_base (Const_string (_str,_))))::[])
               ->
               (match Parsetree_util.has_arity prim_attributes with
                | Some arity -> Determin (false, [(arity, None)], false)
                | None  -> NA)
           | Lprim (Pfield (n,_),(Lprim (Pgetglobal id,[]))::[]) ->
               Lam_compile_env.find_and_add_if_not_exist (id, n) meta.env
                 ~not_found:(fun _  -> assert false)
                 ~found:(fun x  -> x.arity)
           | Lprim (Pfield _,_) -> NA
           | Lprim (Praise _,_) -> Determin (true, [], true)
           | Lprim (Pccall _,_) -> Determin (false, [], false)
           | Lprim _ -> Determin (true, [], false)
           | Lletrec (_,body) -> get_arity meta body
           | Lapply (app,args,_info) ->
               let fn = get_arity meta app in
               (match fn with
                | NA  -> NA
                | Determin (b,xs,tail) ->
                    let rec take (xs : _ list) arg_length =
                      match xs with
                      | (x,y)::xs ->
                          if arg_length = x
                          then Lam_stats.Determin (b, xs, tail)
                          else
                            if arg_length > x
                            then take xs (arg_length - x)
                            else
                              Determin
                                (b,
                                  (((x - arg_length),
                                     ((match y with
                                       | Some y ->
                                           Some (Ext_list.drop arg_length y)
                                       | None  -> None))) :: xs), tail)
                      | [] ->
                          if tail
                          then Determin (b, [], tail)
                          else
                            if not b
                            then NA
                            else failwith (Lam_util.string_of_lambda lam) in
                    take xs (List.length args))
           | Lfunction (kind,params,l) ->
               let n = List.length params in
               merge (n, (Some params)) (get_arity meta l)
           | Lswitch
               (l,{ sw_failaction; sw_consts; sw_blocks; sw_numblocks = _;
                    sw_numconsts = _ })
               ->
               all_lambdas meta
                 (let rest =
                    (sw_consts |> (List.map snd)) @
                      (sw_blocks |> (List.map snd)) in
                  match sw_failaction with
                  | None  -> rest
                  | Some x -> x :: rest)
           | Lstringswitch (l,sw,d) ->
               (match d with
                | None  -> all_lambdas meta (List.map snd sw)
                | Some v -> all_lambdas meta (v :: (List.map snd sw)))
           | Lstaticraise _ -> NA
           | Lstaticcatch (_,_,handler) -> get_arity meta handler
           | Ltrywith (l1,_,l2) -> all_lambdas meta [l1; l2]
           | Lifthenelse (l1,l2,l3) -> all_lambdas meta [l2; l3]
           | Lsequence (_,l2) -> get_arity meta l2
           | Lsend (u,m,o,ll,v) -> NA
           | Levent (l,event) -> NA
           | Lifused (v,l) -> NA
           | Lwhile _|Lfor _|Lassign _ -> Determin (true, [], false) : 
          Lam_stats.function_arities)
        and all_lambdas meta (xs : Lambda.lambda list) =
          match xs with
          | y::ys ->
              let arity = get_arity meta y in
              List.fold_left
                (fun exist  ->
                   fun (v : Lambda.lambda)  ->
                     match (exist : Lam_stats.function_arities) with
                     | NA  -> NA
                     | Determin (b,xs,tail) ->
                         (match get_arity meta v with
                          | NA  -> NA
                          | Determin (u,ys,tail2) ->
                              let rec aux (b,acc) xs ys =
                                match (xs, ys) with
                                | ([],[]) ->
                                    (b, (List.rev acc), (tail && tail2))
                                | ([],y::ys) when tail ->
                                    aux (b, (y :: acc)) [] ys
                                | (x::xs,[]) when tail2 ->
                                    aux (b, (x :: acc)) [] xs
                                | (x::xs,y::ys) when x = y ->
                                    aux (b, (y :: acc)) xs ys
                                | (_,_) -> (false, (List.rev acc), false) in
                              let (b,acc,tail3) = aux ((u && b), []) xs ys in
                              Determin (b, acc, tail3))) arity ys
          | _ -> assert false
      end 
    module Lam_inline_util :
      sig
        [@@@ocaml.text " Utilities for lambda inlining "]
        val maybe_functor : string -> bool
        val should_be_functor : string -> Lambda.lambda -> bool
      end =
      struct
        let maybe_functor (name : string) =
          ((name.[0]) >= 'A') && ((name.[0]) <= 'Z')
        let should_be_functor (name : string) lam =
          (maybe_functor name) &&
            ((function | Lambda.Lfunction _ -> true | _ -> false) lam)
        let app_definitely_inlined (body : Lambda.lambda) =
          match body with
          | Lvar _|Lconst _|Lprim _|Lapply _ -> true
          | Llet _|Lletrec _|Lstringswitch _|Lswitch _|Lstaticraise _
            |Lfunction _|Lstaticcatch _|Ltrywith _|Lifthenelse _|Lsequence _
            |Lwhile _|Lfor _|Lassign _|Lsend _|Levent _|Lifused _ -> false
      end 
    module Ext_option :
      sig
        [@@@ocaml.text " Utilities for [option] type "]
        val bind : 'a option -> ('a -> 'b) -> 'b option
      end =
      struct
        let bind v f = match v with | None  -> None | Some x -> Some (f x)
      end 
    module Lam_stats_export :
      sig
        val export_to_cmj :
          Lam_stats.meta ->
            Js_cmj_format.effect ->
              Lam_module_ident.t list ->
                Lambda.lambda Ident_map.t -> Js_cmj_format.cmj_table
      end =
      struct
        let pp = Format.fprintf
        let meaningless_names = ["*opt*"; "param"]
        let rec dump_ident fmt (id : Ident.t)
          (arity : Lam_stats.function_arities) =
          pp fmt "@[<2>export var %s:@ %a@ ;@]" (Ext_ident.convert id.name)
            dump_arity arity
        and dump_arity fmt (arity : Lam_stats.function_arities) =
          match arity with
          | NA  -> pp fmt "any"
          | Determin (_,[],_) -> pp fmt "any"
          | Determin (_,(n,args)::xs,_) ->
              let args =
                match args with
                | Some args -> args
                | None  -> Ext_list.init n (fun _  -> Ident.create "param") in
              pp fmt "@[(%a)@ =>@ any@]"
                (Format.pp_print_list
                   ~pp_sep:(fun fmt  ->
                              fun _  ->
                                Format.pp_print_string fmt ",";
                                Format.pp_print_space fmt ())
                   (fun fmt  ->
                      fun ident  ->
                        pp fmt "@[%s@ :@ any@]"
                          (Ext_ident.convert @@ (Ident.name ident)))) args
        let export_to_cmj (meta : Lam_stats.meta) maybe_pure external_ids
          export_map =
          (let values =
             List.fold_left
               (fun acc  ->
                  fun (x : Ident.t)  ->
                    let arity = Lam_stats_util.get_arity meta (Lvar x) in
                    match Ident_map.find x export_map with
                    | lambda ->
                        if Lam_analysis.safe_to_inline lambda
                        then
                          let closed_lambda =
                            if
                              Lam_inline_util.should_be_functor x.name lambda
                            then
                              (if Lam_analysis.is_closed lambda
                               then Some lambda
                               else None)
                            else
                              (let lam_size = Lam_analysis.size lambda in
                               let free_variables =
                                 Lam_analysis.free_variables Ident_set.empty
                                   Ident_map.empty lambda in
                               if
                                 (lam_size < Lam_analysis.small_inline_size)
                                   && (Ident_map.is_empty free_variables)
                               then
                                 (Ext_log.dwarn __LOC__
                                    "%s recorded for inlining @." x.name;
                                  Some lambda)
                               else None) in
                          String_map.add x.name
                            (let open Js_cmj_format in
                               { arity; closed_lambda }) acc
                        else
                          String_map.add x.name
                            (let open Js_cmj_format in
                               { arity; closed_lambda = None }) acc
                    | exception Not_found  ->
                        String_map.add x.name
                          (let open Js_cmj_format in
                             { arity; closed_lambda = None }) acc)
               String_map.empty meta.exports in
           let rec dump fmt ids =
             match ids with
             | [] -> ()
             | x::xs ->
                 (dump_ident fmt x (Lam_stats_util.get_arity meta (Lvar x));
                  Format.pp_print_space fmt ();
                  dump fmt xs) in
           let () =
             if
               (!Js_config.default_gen_tds) &&
                 (not (Ext_string.is_empty meta.filename))
             then
               (Ext_pervasives.with_file_as_pp
                  ((Ext_filename.chop_extension ~loc:__LOC__ meta.filename) ^
                     ".d.ts"))
                 @@ (fun fmt  -> pp fmt "@[<v>%a@]@." dump meta.exports) in
           let effect =
             match maybe_pure with
             | None  ->
                 Ext_option.bind
                   (Ext_list.for_all_ret
                      (fun (id : Lam_module_ident.t)  ->
                         Lam_compile_env.query_and_add_if_not_exist id
                           (Has_env (meta.env)) ~not_found:(fun _  -> false)
                           ~found:(fun i  -> i.pure)) external_ids)
                   (fun x  -> Lam_module_ident.name x)
             | Some _ -> maybe_pure in
           {
             values;
             effect;
             goog_package = (Js_config.get_goog_package_name ())
           } : Js_cmj_format.cmj_table)
      end 
    module Lam_pass_remove_alias :
      sig
        [@@@ocaml.text " Keep track of the global module Aliases "]
        [@@@ocaml.text
          "\n    One way:  guarantee that all global aliases *would be removed* ,\n    it will not be aliased \n    \n    So the only remaining place for globals is either \n    just  Pgetglobal in functor application or \n    `Lprim (Pfield( i ), [Pgetglobal])`\n\n    This pass does not change meta  data\n"]
        val simplify_alias : Lam_stats.meta -> Lambda.lambda -> Lambda.lambda
      end =
      struct
        let simplify_alias (meta : Lam_stats.meta) (lam : Lambda.lambda) =
          (let rec simpl (lam : Lambda.lambda) =
             (match lam with
              | Lvar v ->
                  (try Lvar (Hashtbl.find meta.alias_tbl v)
                   with | Not_found  -> lam)
              | Llet (kind,k,(Lprim (Pgetglobal i,[]) as g),l) ->
                  let v = simpl l in
                  if Ident_set.mem k meta.export_idents
                  then Llet (kind, k, g, v)
                  else v
              | Lprim (Pfield (i,_),(Lvar v)::[]) ->
                  Lam_util.get lam v i meta.ident_tbl
              | Lifthenelse ((Lvar id as l1),l2,l3) ->
                  (match Hashtbl.find meta.ident_tbl id with
                   | ImmutableBlock (_,Normal )|MutableBlock _ -> simpl l2
                   | ImmutableBlock ([|SimpleForm l|],x) ->
                       (match x with
                        | Null  ->
                            Lam_comb.if_
                              (Lprim (Lam_util.js_is_nil_primitive, [l]))
                              (simpl l3) (simpl l2)
                        | Undefined  ->
                            Lam_comb.if_
                              (Lprim (Lam_util.js_is_undef_primitive, [l]))
                              (simpl l3) (simpl l2)
                        | Null_undefined  ->
                            Lam_comb.if_
                              (Lprim
                                 ((Pintcomp Ceq), [l; Lvar Ext_ident.nil]))
                              (simpl l3) (simpl l2)
                        | Normal  -> Lam_comb.if_ l1 (simpl l2) (simpl l3))
                   | _ -> Lam_comb.if_ l1 (simpl l2) (simpl l3)
                   | exception Not_found  ->
                       Lam_comb.if_ l1 (simpl l2) (simpl l3))
              | Lifthenelse (l1,l2,l3) ->
                  Lam_comb.if_ (simpl l1) (simpl l2) (simpl l3)
              | Lconst _ -> lam
              | Llet (str,v,l1,l2) -> Llet (str, v, (simpl l1), (simpl l2))
              | Lletrec (bindings,body) ->
                  let bindings =
                    List.map (fun (k,l)  -> (k, (simpl l))) bindings in
                  Lletrec (bindings, (simpl body))
              | Lprim (prim,ll) -> Lprim (prim, (List.map simpl ll))
              | Lapply
                  ((Lprim
                      (Pfield (index,_),(Lprim (Pgetglobal ident,[]))::[]) as
                      l1),args,info)
                  ->
                  Lam_compile_env.find_and_add_if_not_exist (ident, index)
                    meta.env ~not_found:(fun _  -> assert false)
                    ~found:(fun i  ->
                              match i with
                              | {
                                  closed_lambda = Some (Lfunction
                                    (Curried ,params,body))
                                  } when
                                  (Ext_list.same_length params args) &&
                                    (List.for_all
                                       (fun (arg : Lambda.lambda)  ->
                                          match arg with
                                          | Lvar p ->
                                              (try
                                                 (Hashtbl.find meta.ident_tbl
                                                    p)
                                                   <> Parameter
                                               with | Not_found  -> true)
                                          | _ -> true) args)
                                  ->
                                  simpl @@
                                    (Lam_beta_reduce.propogate_beta_reduce
                                       meta params body args)
                              | _ ->
                                  Lapply
                                    ((simpl l1), (List.map simpl args), info))
              | Lapply ((Lvar v as l1),args,info) ->
                  (match Hashtbl.find meta.ident_tbl v with
                   | Function
                       { lambda = (Lfunction (_,params,body) as _m);
                         rec_flag;_}
                       ->
                       let lam_size = Lam_analysis.size body in
                       if Ext_list.same_length args params
                       then
                         (if Lam_inline_util.maybe_functor v.name
                          then
                            simpl
                              (Lam_beta_reduce.propogate_beta_reduce meta
                                 params body args)
                          else
                            if lam_size < Lam_analysis.small_inline_size
                            then
                              (let param_map =
                                 Lam_analysis.is_closed_with_map
                                   meta.export_idents params body in
                               let is_export_id =
                                 Ident_set.mem v meta.export_idents in
                               match (is_export_id, param_map) with
                               | (false ,(_,param_map))
                                 |(true ,(true ,param_map)) ->
                                   if rec_flag = Rec
                                   then
                                     Lam_beta_reduce.propogate_beta_reduce_with_map
                                       meta param_map params body args
                                   else
                                     simpl
                                       (Lam_beta_reduce.propogate_beta_reduce_with_map
                                          meta param_map params body args)
                               | _ ->
                                   Lapply
                                     ((simpl l1), (List.map simpl args),
                                       info))
                            else
                              Lapply
                                ((simpl l1), (List.map simpl args), info))
                       else Lapply ((simpl l1), (List.map simpl args), info)
                   | _ -> Lapply ((simpl l1), (List.map simpl args), info)
                   | exception Not_found  ->
                       Lapply ((simpl l1), (List.map simpl args), info))
              | Lapply (Lfunction (Curried ,params,body),args,_) when
                  Ext_list.same_length params args ->
                  simpl
                    (Lam_beta_reduce.propogate_beta_reduce meta params body
                       args)
              | Lapply
                  (Lfunction (Tupled ,params,body),(Lprim
                   (Pmakeblock _,args))::[],_)
                  when Ext_list.same_length params args ->
                  simpl
                    (Lam_beta_reduce.propogate_beta_reduce meta params body
                       args)
              | Lapply (l1,ll,info) ->
                  Lapply ((simpl l1), (List.map simpl ll), info)
              | Lfunction (kind,params,l) ->
                  Lfunction (kind, params, (simpl l))
              | Lswitch
                  (l,{ sw_failaction; sw_consts; sw_blocks; sw_numblocks;
                       sw_numconsts })
                  ->
                  Lswitch
                    ((simpl l),
                      {
                        sw_consts =
                          (List.map (fun (v,l)  -> (v, (simpl l))) sw_consts);
                        sw_blocks =
                          (List.map (fun (v,l)  -> (v, (simpl l))) sw_blocks);
                        sw_numconsts;
                        sw_numblocks;
                        sw_failaction =
                          ((match sw_failaction with
                            | None  -> None
                            | Some x -> Some (simpl x)))
                      })
              | Lstringswitch (l,sw,d) ->
                  Lstringswitch
                    ((simpl l), (List.map (fun (i,l)  -> (i, (simpl l))) sw),
                      ((match d with
                        | Some d -> Some (simpl d)
                        | None  -> None)))
              | Lstaticraise (i,ls) -> Lstaticraise (i, (List.map simpl ls))
              | Lstaticcatch (l1,(i,x),l2) ->
                  Lstaticcatch ((simpl l1), (i, x), (simpl l2))
              | Ltrywith (l1,v,l2) -> Ltrywith ((simpl l1), v, (simpl l2))
              | Lsequence (Lprim (Pgetglobal id,[]),l2) when
                  Lam_compile_env.is_pure (Lam_module_ident.of_ml id) ->
                  simpl l2
              | Lsequence (l1,l2) -> Lsequence ((simpl l1), (simpl l2))
              | Lwhile (l1,l2) -> Lwhile ((simpl l1), (simpl l2))
              | Lfor (flag,l1,l2,dir,l3) ->
                  Lfor (flag, (simpl l1), (simpl l2), dir, (simpl l3))
              | Lassign (v,l) -> Lassign (v, (simpl l))
              | Lsend (u,m,o,ll,v) ->
                  Lsend (u, (simpl m), (simpl o), (List.map simpl ll), v)
              | Levent (l,event) -> Levent ((simpl l), event)
              | Lifused (v,l) -> Lifused (v, (simpl l)) : Lambda.lambda) in
           simpl lam : Lambda.lambda)
      end 
    module Lam_pass_lets_dce :
      sig
        val simplify_lets : Lambda.lambda -> Lambda.lambda[@@ocaml.doc
                                                            "\n   This pass would do beta reduction, and dead code elimination (adapted from compiler's built-in [Simplif] module )\n\n   1. beta reduction -> Llet (Strict )\n  \n   2. The global table [occ] associates to each let-bound identifier\n   the number of its uses (as a reference):\n     - 0 if never used\n     - 1 if used exactly once in and *not under a lambda or within a loop\n     - > 1 if used several times or under a lambda or within a loop.\n\n   The local table [bv] associates to each locally-let-bound variable\n   its reference count, as above.  [bv] is enriched at let bindings\n   but emptied when crossing lambdas and loops. \n\n   For this pass, when it' used under a lambda or within a loop, we don't do anything,\n   in theory, we can still do something if it's pure but we are conservative here.\n\n   [bv] is used to help caculate [occ] it is not useful outside\n\n "]
      end =
      struct
        open Asttypes
        exception Real_reference
        let rec eliminate_ref id (lam : Lambda.lambda) =
          match lam with
          | Lvar v -> if Ident.same v id then raise Real_reference else lam
          | Lprim (Pfield (0,_),(Lvar v)::[]) when Ident.same v id -> Lvar id
          | Lfunction (kind,params,body) as lam ->
              if Lambda.IdentSet.mem id (Lambda.free_variables lam)
              then raise Real_reference
              else lam
          | Lprim (Psetfield (0,_,_),(Lvar v)::e::[]) when Ident.same v id ->
              Lassign (id, (eliminate_ref id e))
          | Lprim (Poffsetref delta,(Lvar v)::[]) when Ident.same v id ->
              Lassign (id, (Lprim ((Poffsetint delta), [Lvar id])))
          | Lconst _ -> lam
          | Lapply (e1,el,loc) ->
              Lapply
                ((eliminate_ref id e1), (List.map (eliminate_ref id) el),
                  loc)
          | Llet (str,v,e1,e2) ->
              Llet (str, v, (eliminate_ref id e1), (eliminate_ref id e2))
          | Lletrec (idel,e2) ->
              Lletrec
                ((List.map (fun (v,e)  -> (v, (eliminate_ref id e))) idel),
                  (eliminate_ref id e2))
          | Lprim (p,el) -> Lprim (p, (List.map (eliminate_ref id) el))
          | Lswitch (e,sw) ->
              Lswitch
                ((eliminate_ref id e),
                  {
                    sw_numconsts = (sw.sw_numconsts);
                    sw_consts =
                      (List.map (fun (n,e)  -> (n, (eliminate_ref id e)))
                         sw.sw_consts);
                    sw_numblocks = (sw.sw_numblocks);
                    sw_blocks =
                      (List.map (fun (n,e)  -> (n, (eliminate_ref id e)))
                         sw.sw_blocks);
                    sw_failaction =
                      (Misc.may_map (eliminate_ref id) sw.sw_failaction)
                  })
          | Lstringswitch (e,sw,default) ->
              Lstringswitch
                ((eliminate_ref id e),
                  (List.map (fun (s,e)  -> (s, (eliminate_ref id e))) sw),
                  (Misc.may_map (eliminate_ref id) default))
          | Lstaticraise (i,args) ->
              Lstaticraise (i, (List.map (eliminate_ref id) args))
          | Lstaticcatch (e1,i,e2) ->
              Lstaticcatch ((eliminate_ref id e1), i, (eliminate_ref id e2))
          | Ltrywith (e1,v,e2) ->
              Ltrywith ((eliminate_ref id e1), v, (eliminate_ref id e2))
          | Lifthenelse (e1,e2,e3) ->
              Lam_comb.if_ (eliminate_ref id e1) (eliminate_ref id e2)
                (eliminate_ref id e3)
          | Lsequence (e1,e2) ->
              Lsequence ((eliminate_ref id e1), (eliminate_ref id e2))
          | Lwhile (e1,e2) ->
              Lwhile ((eliminate_ref id e1), (eliminate_ref id e2))
          | Lfor (v,e1,e2,dir,e3) ->
              Lfor
                (v, (eliminate_ref id e1), (eliminate_ref id e2), dir,
                  (eliminate_ref id e3))
          | Lassign (v,e) -> Lassign (v, (eliminate_ref id e))
          | Lsend (k,m,o,el,loc) ->
              Lsend
                (k, (eliminate_ref id m), (eliminate_ref id o),
                  (List.map (eliminate_ref id) el), loc)
          | Levent (l,ev) -> Levent ((eliminate_ref id l), ev)
          | Lifused (v,e) -> Lifused (v, (eliminate_ref id e))
        type used_info = {
          mutable times: int;
          mutable captured: bool;}
        type occ_tbl = (Ident.t,used_info) Hashtbl.t
        type local_tbl = used_info Ident_map.t
        let dummy_info () = { times = 0; captured = false }
        let absorb_info (x : used_info) (y : used_info) =
          match (x, y) with
          | ({ times = x0 },{ times = y0; captured }) ->
              (x.times <- x0 + y0; if captured then x.captured <- true)
        let lets_helper (count_var : Ident.t -> used_info) lam =
          let subst = Hashtbl.create 31 in
          let used v = (count_var v).times > 0 in
          let rec simplif (lam : Lambda.lambda) =
            match lam with
            | Lvar v -> (try Hashtbl.find subst v with | Not_found  -> lam)
            | Llet ((Strict |Alias |StrictOpt ),v,Lvar w,l2) ->
                (Hashtbl.add subst v (simplif (Lvar w)); simplif l2)
            | Llet
                ((Strict |StrictOpt  as kind),v,Lprim
                 ((Pmakeblock (0,tag_info,Mutable ) as prim),linit::[]),lbody)
                ->
                let slinit = simplif linit in
                let slbody = simplif lbody in
                (try
                   Lam_util.refine_let ~kind:Variable v slinit
                     (eliminate_ref v slbody)
                 with
                 | Real_reference  ->
                     Lam_util.refine_let ~kind v (Lprim (prim, [slinit]))
                       slbody)
            | Llet (Alias ,v,l1,l2) ->
                (match ((count_var v), l1) with
                 | ({ times = 0;_},_) -> simplif l2
                 | ({ times = 1; captured = false  },_)
                   |({ times = 1; captured = true  },(Lconst _|Lvar _))
                   |(_,(Lconst
                        (Const_base
                         (Const_int _|Const_char _|Const_float _|Const_int32
                          _|Const_nativeint _)|Lambda.Const_pointer _)|Lprim
                        (Lambda.Pfield _,(Lprim (Lambda.Pgetglobal _,_))::[])))
                     -> (Hashtbl.add subst v (simplif l1); simplif l2)
                 | _ -> Llet (Alias, v, (simplif l1), (simplif l2)))
            | Llet ((StrictOpt  as kind),v,l1,l2) ->
                if not @@ (used v)
                then simplif l2
                else Lam_util.refine_let ~kind v (simplif l1) (simplif l2)
            | Llet ((Strict |Variable  as kind),v,l1,l2) ->
                if not @@ (used v)
                then
                  let l1 = simplif l1 in
                  let l2 = simplif l2 in
                  (if Lam_analysis.no_side_effects l1
                   then l2
                   else Lsequence (l1, l2))
                else Lam_util.refine_let ~kind v (simplif l1) (simplif l2)
            | Lifused (v,l) ->
                if used v then simplif l else Lambda.lambda_unit
            | Lsequence (Lifused (v,l1),l2) ->
                if used v
                then Lsequence ((simplif l1), (simplif l2))
                else simplif l2
            | Lsequence (l1,l2) -> Lsequence ((simplif l1), (simplif l2))
            | Lapply (Lfunction (Curried ,params,body),args,_) when
                Ext_list.same_length params args ->
                simplif (Lam_beta_reduce.beta_reduce params body args)
            | Lapply
                (Lfunction (Tupled ,params,body),(Lprim
                 (Pmakeblock _,args))::[],_)
                when Ext_list.same_length params args ->
                simplif (Lam_beta_reduce.beta_reduce params body args)
            | Lapply (l1,ll,loc) ->
                Lapply ((simplif l1), (List.map simplif ll), loc)
            | Lfunction (kind,params,l) ->
                Lfunction (kind, params, (simplif l))
            | Lconst _ -> lam
            | Lletrec (bindings,body) ->
                Lletrec
                  ((List.map (fun (v,l)  -> (v, (simplif l))) bindings),
                    (simplif body))
            | Lprim (p,ll) -> Lprim (p, (List.map simplif ll))
            | Lswitch (l,sw) ->
                let new_l = simplif l
                and new_consts =
                  List.map (fun (n,e)  -> (n, (simplif e))) sw.sw_consts
                and new_blocks =
                  List.map (fun (n,e)  -> (n, (simplif e))) sw.sw_blocks
                and new_fail = Misc.may_map simplif sw.sw_failaction in
                Lswitch
                  (new_l,
                    {
                      sw with
                      sw_consts = new_consts;
                      sw_blocks = new_blocks;
                      sw_failaction = new_fail
                    })
            | Lstringswitch (l,sw,d) ->
                Lam_comb.stringswitch (simplif l)
                  (List.map (fun (s,l)  -> (s, (simplif l))) sw)
                  (Misc.may_map simplif d)
            | Lstaticraise (i,ls) -> Lstaticraise (i, (List.map simplif ls))
            | Lstaticcatch (l1,(i,args),l2) ->
                Lstaticcatch ((simplif l1), (i, args), (simplif l2))
            | Ltrywith (l1,v,l2) -> Ltrywith ((simplif l1), v, (simplif l2))
            | Lifthenelse (l1,l2,l3) ->
                Lam_comb.if_ (simplif l1) (simplif l2) (simplif l3)
            | Lwhile (l1,l2) -> Lwhile ((simplif l1), (simplif l2))
            | Lfor (v,l1,l2,dir,l3) ->
                Lfor (v, (simplif l1), (simplif l2), dir, (simplif l3))
            | Lassign (v,l) -> Lassign (v, (simplif l))
            | Lsend (k,m,o,ll,loc) ->
                Lsend
                  (k, (simplif m), (simplif o), (List.map simplif ll), loc)
            | Levent (l,ev) -> Levent ((simplif l), ev) in
          simplif lam
        let apply_lets occ lambda =
          let count_var v =
            try Hashtbl.find occ v with | Not_found  -> dummy_info () in
          lets_helper count_var lambda
        let collect_occurs lam =
          (let occ: occ_tbl = Hashtbl.create 83 in
           let used v =
             match Hashtbl.find occ v with
             | exception Not_found  -> false
             | { times;_} -> times > 0 in
           let bind_var bv ident =
             let r = dummy_info () in
             Hashtbl.add occ ident r; Ident_map.add ident r bv in
           let add_one_use bv ident =
             match Ident_map.find ident bv with
             | r -> r.times <- r.times + 1
             | exception Not_found  ->
                 (match Hashtbl.find occ ident with
                  | r -> absorb_info r { times = 1; captured = true }
                  | exception Not_found  -> ()) in
           let inherit_use bv ident bid =
             let n =
               try Hashtbl.find occ bid with | Not_found  -> dummy_info () in
             match Ident_map.find ident bv with
             | r -> absorb_info r n
             | exception Not_found  ->
                 (match Hashtbl.find occ ident with
                  | r -> absorb_info r { n with captured = true }
                  | exception Not_found  -> ()) in
           let rec count (bv : local_tbl) (lam : Lambda.lambda) =
             match lam with
             | Lfunction (kind,params,l) -> count Ident_map.empty l
             | Lvar v -> add_one_use bv v
             | Llet (_,v,Lvar w,l2) ->
                 (count (bind_var bv v) l2; inherit_use bv w v)
             | Llet (kind,v,l1,l2) ->
                 (count (bind_var bv v) l2;
                  if (kind = Strict) || (used v) then count bv l1)
             | Lprim (_,ll) -> List.iter (count bv) ll
             | Lletrec (bindings,body) ->
                 (List.iter (fun (v,l)  -> count bv l) bindings;
                  count bv body)
             | Lapply (Lfunction (Curried ,params,body),args,_) when
                 Ext_list.same_length params args ->
                 count bv (Lam_beta_reduce.beta_reduce params body args)
             | Lapply
                 (Lfunction (Tupled ,params,body),(Lprim
                  (Pmakeblock _,args))::[],_)
                 when Ext_list.same_length params args ->
                 count bv (Lam_beta_reduce.beta_reduce params body args)
             | Lapply (l1,ll,_) -> (count bv l1; List.iter (count bv) ll)
             | Lassign (_,l) -> count bv l
             | Lconst cst -> ()
             | Lswitch (l,sw) ->
                 (count_default bv sw;
                  count bv l;
                  List.iter (fun (_,l)  -> count bv l) sw.sw_consts;
                  List.iter (fun (_,l)  -> count bv l) sw.sw_blocks)
             | Lstringswitch (l,sw,d) ->
                 (count bv l;
                  List.iter (fun (_,l)  -> count bv l) sw;
                  (match d with | Some d -> count bv d | None  -> ()))
             | Lstaticraise (i,ls) -> List.iter (count bv) ls
             | Lstaticcatch (l1,(i,_),l2) -> (count bv l1; count bv l2)
             | Ltrywith (l1,v,l2) -> (count bv l1; count bv l2)
             | Lifthenelse (l1,l2,l3) ->
                 (count bv l1; count bv l2; count bv l3)
             | Lsequence (l1,l2) -> (count bv l1; count bv l2)
             | Lwhile (l1,l2) ->
                 (count Ident_map.empty l1; count Ident_map.empty l2)
             | Lfor (_,l1,l2,dir,l3) ->
                 (count bv l1; count bv l2; count Ident_map.empty l3)
             | Lsend (_,m,o,ll,_) -> List.iter (count bv) (m :: o :: ll)
             | Levent (l,_) -> count bv l
             | Lifused (v,l) -> if used v then count bv l
           and count_default bv sw =
             match sw.sw_failaction with
             | None  -> ()
             | Some al ->
                 let nconsts = List.length sw.sw_consts
                 and nblocks = List.length sw.sw_blocks in
                 if
                   (nconsts < sw.sw_numconsts) && (nblocks < sw.sw_numblocks)
                 then (count bv al; count bv al)
                 else
                   (assert
                      ((nconsts < sw.sw_numconsts) ||
                         (nblocks < sw.sw_numblocks));
                    count bv al) in
           count Ident_map.empty lam; occ : occ_tbl)
        let simplify_lets (lam : Lambda.lambda) =
          let occ = collect_occurs lam in apply_lets occ lam
      end 
    module Lam_pass_exits :
      sig
        [@@@ocaml.text
          " A pass used to optimize the exit code compilation, adaped from the compiler's\n    [simplif] module\n "]
        val count_helper : Lambda.lambda -> (int,int ref) Hashtbl.t
        type subst_tbl = (int,(Ident.t list* Lambda.lambda)) Hashtbl.t
        val subst_helper :
          subst_tbl -> (int -> int) -> Lambda.lambda -> Lambda.lambda
        val simplify_exits : Lambda.lambda -> Lambda.lambda
      end =
      struct
        let count_exit exits i =
          try !(Hashtbl.find exits i) with | Not_found  -> 0
        and incr_exit exits i =
          try incr (Hashtbl.find exits i)
          with | Not_found  -> Hashtbl.add exits i (ref 1)
        let count_helper (lam : Lambda.lambda) =
          (let exits = Hashtbl.create 17 in
           let rec count (lam : Lambda.lambda) =
             match lam with
             | Lstaticraise (i,ls) -> (incr_exit exits i; List.iter count ls)
             | Lstaticcatch (l1,(i,[]),Lstaticraise (j,[])) ->
                 (count l1;
                  (let ic = count_exit exits i in
                   try let r = Hashtbl.find exits j in r := ((!r) + ic)
                   with | Not_found  -> Hashtbl.add exits j (ref ic)))
             | Lstaticcatch (l1,(i,_),l2) ->
                 (count l1; if (count_exit exits i) > 0 then count l2)
             | Lstringswitch (l,sw,d) ->
                 (count l;
                  List.iter (fun (_,l)  -> count l) sw;
                  (match d with | None  -> () | Some d -> count d))
             | Lvar _|Lconst _ -> ()
             | Lapply (l1,ll,_) -> (count l1; List.iter count ll)
             | Lfunction (_,_,l) -> count l
             | Llet (_,_,l1,l2) -> (count l2; count l1)
             | Lletrec (bindings,body) ->
                 (List.iter (fun (_,l)  -> count l) bindings; count body)
             | Lprim (_,ll) -> List.iter count ll
             | Lswitch (l,sw) ->
                 (count_default sw;
                  count l;
                  List.iter (fun (_,l)  -> count l) sw.sw_consts;
                  List.iter (fun (_,l)  -> count l) sw.sw_blocks)
             | Ltrywith (l1,v,l2) -> (count l1; count l2)
             | Lifthenelse (l1,l2,l3) -> (count l1; count l2; count l3)
             | Lsequence (l1,l2) -> (count l1; count l2)
             | Lwhile (l1,l2) -> (count l1; count l2)
             | Lfor (_,l1,l2,dir,l3) -> (count l1; count l2; count l3)
             | Lassign (_,l) -> count l
             | Lsend (_,m,o,ll,_) -> (count m; count o; List.iter count ll)
             | Levent (l,_) -> count l
             | Lifused (_,l) -> count l
           and count_default sw =
             match sw.sw_failaction with
             | None  -> ()
             | Some al ->
                 let nconsts = List.length sw.sw_consts
                 and nblocks = List.length sw.sw_blocks in
                 if
                   (nconsts < sw.sw_numconsts) && (nblocks < sw.sw_numblocks)
                 then (count al; count al)
                 else
                   (assert
                      ((nconsts < sw.sw_numconsts) ||
                         (nblocks < sw.sw_numblocks));
                    count al) in
           count lam; exits : (int,int ref) Hashtbl.t)
        type subst_tbl = (int,(Ident.t list* Lambda.lambda)) Hashtbl.t
        let subst_helper (subst : subst_tbl) query lam =
          let rec simplif (lam : Lambda.lambda) =
            match lam with
            | Lstaticraise (i,[]) ->
                (match Hashtbl.find subst i with
                 | (_,handler) -> handler
                 | exception Not_found  -> lam)
            | Lstaticraise (i,ls) ->
                let ls = List.map simplif ls in
                (match Hashtbl.find subst i with
                 | (xs,handler) ->
                     let ys = List.map Ident.rename xs in
                     let env =
                       List.fold_right2
                         (fun x  ->
                            fun y  -> fun t  -> Ident.add x (Lambda.Lvar y) t)
                         xs ys Ident.empty in
                     List.fold_right2
                       (fun y  ->
                          fun l  -> fun r  -> Lambda.Llet (Alias, y, l, r))
                       ys ls (Lambda.subst_lambda env handler)
                 | exception Not_found  -> Lstaticraise (i, ls))
            | Lstaticcatch (l1,(i,[]),(Lstaticraise (j,[]) as l2)) ->
                (Hashtbl.add subst i ([], (simplif l2)); simplif l1)
            | Lstaticcatch (l1,(i,xs),l2) ->
                (match ((query i), l2) with
                 | (0,_) -> simplif l1
                 | (_,Lvar _)|(_,Lconst _) ->
                     (Hashtbl.add subst i (xs, (simplif l2)); simplif l1)
                 | (1,_) when i >= 0 ->
                     (Hashtbl.add subst i (xs, (simplif l2)); simplif l1)
                 | (j,_) ->
                     let lam_size = Lam_analysis.size l2 in
                     let ok_to_inline =
                       (i >= 0) &&
                         (((j <= 2) &&
                             (lam_size < Lam_analysis.exit_inline_size))
                            || (lam_size < 5)) in
                     if ok_to_inline
                     then
                       (Hashtbl.add subst i
                          (xs, (Lam_beta_reduce.refresh @@ (simplif l2)));
                        simplif l1)
                     else Lstaticcatch ((simplif l1), (i, xs), (simplif l2)))
            | Lvar _|Lconst _ -> lam
            | Lapply (l1,ll,loc) ->
                Lapply ((simplif l1), (List.map simplif ll), loc)
            | Lfunction (kind,params,l) ->
                Lfunction (kind, params, (simplif l))
            | Llet (kind,v,l1,l2) ->
                Llet (kind, v, (simplif l1), (simplif l2))
            | Lletrec (bindings,body) ->
                Lletrec
                  ((List.map (fun (v,l)  -> (v, (simplif l))) bindings),
                    (simplif body))
            | Lprim (p,ll) ->
                let ll = List.map simplif ll in
                (match (p, ll) with
                 | (Prevapply loc,x::(Lapply (f,args,_))::[])
                   |(Prevapply loc,x::(Levent (Lapply (f,args,_),_))::[]) ->
                     Lapply
                       (f, (args @ [x]), (Lambda.default_apply_info ~loc ()))
                 | (Prevapply loc,x::f::[]) ->
                     Lapply (f, [x], (Lambda.default_apply_info ~loc ()))
                 | (Pdirapply loc,(Lapply (f,args,_))::x::[])
                   |(Pdirapply loc,(Levent (Lapply (f,args,_),_))::x::[]) ->
                     Lapply
                       (f, (args @ [x]), (Lambda.default_apply_info ~loc ()))
                 | (Pdirapply loc,f::x::[]) ->
                     Lapply (f, [x], (Lambda.default_apply_info ~loc ()))
                 | _ -> Lprim (p, ll))
            | Lswitch (l,sw) ->
                let new_l = simplif l
                and new_consts =
                  List.map (fun (n,e)  -> (n, (simplif e))) sw.sw_consts
                and new_blocks =
                  List.map (fun (n,e)  -> (n, (simplif e))) sw.sw_blocks
                and new_fail = Misc.may_map simplif sw.sw_failaction in
                Lswitch
                  (new_l,
                    {
                      sw with
                      sw_consts = new_consts;
                      sw_blocks = new_blocks;
                      sw_failaction = new_fail
                    })
            | Lstringswitch (l,sw,d) ->
                Lam_comb.stringswitch (simplif l)
                  (List.map (fun (s,l)  -> (s, (simplif l))) sw)
                  (Misc.may_map simplif d)
            | Ltrywith (l1,v,l2) -> Ltrywith ((simplif l1), v, (simplif l2))
            | Lifthenelse (l1,l2,l3) ->
                Lam_comb.if_ (simplif l1) (simplif l2) (simplif l3)
            | Lsequence (l1,l2) -> Lsequence ((simplif l1), (simplif l2))
            | Lwhile (l1,l2) -> Lwhile ((simplif l1), (simplif l2))
            | Lfor (v,l1,l2,dir,l3) ->
                Lfor (v, (simplif l1), (simplif l2), dir, (simplif l3))
            | Lassign (v,l) -> Lassign (v, (simplif l))
            | Lsend (k,m,o,ll,loc) ->
                Lsend
                  (k, (simplif m), (simplif o), (List.map simplif ll), loc)
            | Levent (l,ev) -> Levent ((simplif l), ev)
            | Lifused (v,l) -> Lifused (v, (simplif l)) in
          simplif lam
        let simplify_exits (lam : Lambda.lambda) =
          let exits = count_helper lam in
          subst_helper (Hashtbl.create 17) (count_exit exits) lam
      end 
    module Lam_pass_collect :
      sig
        [@@@ocaml.text
          " This pass is used to collect meta data information.\n\n    It includes:\n    alias table, arity for identifiers and might more information,\n    \n    ATTENTION:\n    For later pass to keep its information complete and up to date,\n    we  need update its table accordingly\n\n    - Alias inference is not for substitution, it is for analyze which module is \n      actually a global module or an exception, so it can be relaxed a bit\n      (without relying on strict analysis)\n\n    - Js object (local) analysis \n\n    Design choice:\n\n    Side effectful operations:\n       - Lassign \n       - Psetfield\n\n    1. What information should be collected:\n\n    2. What's the key\n       If it's identifier, \n       \n    Information that is always sound, not subject to change \n\n    - shall we collect that if an identifier is passed as a parameter, (useful for escape analysis), \n    however, since it's going to change after inlning (for local function)\n\n    - function arity, subject to change when you make it a mutable ref and change it later\n    \n    - Immutable blocks of identifiers\n     \n      if identifier itself is function/non block then the access can be inlined \n      if identifier itself is immutable block can be inlined\n      if identifier is mutable block can be inlined (without Lassign) since\n\n    - When collect some information, shall we propogate this information to \n      all alias table immeidately\n\n      - annotation identifiers (at first time)\n      -\n "]
        val collect_helper : Lam_stats.meta -> Lambda.lambda -> unit[@@ocaml.doc
                                                                    " Modify existing [meta] "]
        val count_alias_globals :
          Env.t -> string -> Ident.t list -> Lambda.lambda -> Lam_stats.meta
        [@@ocaml.doc " return a new [meta] "]
      end =
      struct
        let annotate (meta : Lam_stats.meta) rec_flag (k : Ident.t)
          (v : Lam_stats.function_arities) lambda =
          match Hashtbl.find meta.ident_tbl k with
          | exception Not_found  ->
              Hashtbl.add meta.ident_tbl k
                (Function { kind = NA; arity = v; lambda; rec_flag })
          | Function old -> old.arity <- v
          | _ -> assert false
        let collect_helper (meta : Lam_stats.meta) (lam : Lambda.lambda) =
          let rec collect_bind rec_flag (kind : Lambda.let_kind)
            (ident : Ident.t) (lam : Lambda.lambda) =
            match lam with
            | Lconst v -> Hashtbl.replace meta.ident_tbl ident (Constant v)
            | Lprim (Pmakeblock (_,_,Immutable ),ls) ->
                (Hashtbl.replace meta.ident_tbl ident
                   (Lam_util.kind_of_lambda_block Normal ls);
                 List.iter collect ls)
            | Lprim
                (Pccall
                 { prim_name = "js_from_nullable";_},((Lvar _)::[] as ls))
                ->
                Hashtbl.replace meta.ident_tbl ident
                  (Lam_util.kind_of_lambda_block Null ls)
            | Lprim
                (Pccall { prim_name = "js_from_def";_},((Lvar _)::[] as ls))
                ->
                Hashtbl.replace meta.ident_tbl ident
                  (Lam_util.kind_of_lambda_block Undefined ls)
            | Lprim
                (Pccall
                 { prim_name = "js_from_nullable_def";_},((Lvar _)::[] as ls))
                ->
                Hashtbl.replace meta.ident_tbl ident
                  (Lam_util.kind_of_lambda_block Null_undefined ls)
            | Lprim (Pgetglobal v,[]) ->
                (Lam_util.alias meta ident v (Module v) kind;
                 (match kind with
                  | Alias  -> ()
                  | Strict |StrictOpt |Variable  ->
                      Lam_util.add_required_module v meta))
            | Lvar v -> Lam_util.alias meta ident v NA kind
            | Lfunction (_,params,l) ->
                (List.iter (fun p  -> Hashtbl.add meta.ident_tbl p Parameter)
                   params;
                 (let arity = Lam_stats_util.get_arity meta lam in
                  annotate meta rec_flag ident arity lam; collect l))
            | x ->
                (collect x;
                 if Ident_set.mem ident meta.export_idents
                 then
                   annotate meta rec_flag ident
                     (Lam_stats_util.get_arity meta x) lam)
          and collect (lam : Lambda.lambda) =
            match lam with
            | Lconst _ -> ()
            | Lvar _ -> ()
            | Lapply (l1,ll,_) -> (collect l1; List.iter collect ll)
            | Lfunction (_kind,params,l) ->
                (List.iter (fun p  -> Hashtbl.add meta.ident_tbl p Parameter)
                   params;
                 collect l)
            | Llet (kind,ident,arg,body) ->
                (collect_bind Non_rec kind ident arg; collect body)
            | Lletrec (bindings,body) ->
                (List.iter
                   (fun (ident,arg)  -> collect_bind Rec Strict ident arg)
                   bindings;
                 collect body)
            | Lprim (_p,ll) -> List.iter collect ll
            | Lswitch (l,{ sw_failaction; sw_consts; sw_blocks }) ->
                (collect l;
                 List.iter (fun (_,l)  -> collect l) sw_consts;
                 List.iter (fun (_,l)  -> collect l) sw_blocks;
                 (match sw_failaction with
                  | None  -> ()
                  | Some x -> collect x))
            | Lstringswitch (l,sw,d) ->
                (collect l;
                 List.iter (fun (_,l)  -> collect l) sw;
                 (match d with | Some d -> collect d | None  -> ()))
            | Lstaticraise (code,ls) ->
                (Hash_set.add meta.exit_codes code; List.iter collect ls)
            | Lstaticcatch (l1,(_,_),l2) -> (collect l1; collect l2)
            | Ltrywith (l1,_,l2) -> (collect l1; collect l2)
            | Lifthenelse (l1,l2,l3) -> (collect l1; collect l2; collect l3)
            | Lsequence (l1,l2) -> (collect l1; collect l2)
            | Lwhile (l1,l2) -> (collect l1; collect l2)
            | Lfor (_,l1,l2,dir,l3) -> (collect l1; collect l2; collect l3)
            | Lassign (v,l) -> collect l
            | Lsend (_,m,o,ll,_) -> List.iter collect (m :: o :: ll)
            | Levent (l,_) -> collect l
            | Lifused (_,l) -> collect l in
          collect lam[@@ocaml.doc
                       " it only make senses recording arities for \n    function definition,\n    alias propgation - and toplevel identifiers, this needs to be exported\n "]
        let count_alias_globals env filename export_idents
          (lam : Lambda.lambda) =
          (let meta: Lam_stats.meta =
             {
               alias_tbl = (Hashtbl.create 31);
               ident_tbl = (Hashtbl.create 31);
               exit_codes = (Hash_set.create 31);
               exports = export_idents;
               required_modules = [];
               filename;
               env;
               export_idents = (Lam_util.ident_set_of_list export_idents)
             } in
           collect_helper meta lam; meta : Lam_stats.meta)
      end 
    module Lam_pass_alpha_conversion :
      sig
        [@@@ocaml.text " alpha conversion based on arity "]
        val alpha_conversion :
          Lam_stats.meta -> Lambda.lambda -> Lambda.lambda
      end =
      struct
        let alpha_conversion (meta : Lam_stats.meta) (lam : Lambda.lambda) =
          (let rec simpl (lam : Lambda.lambda) =
             match lam with
             | Lconst _ -> lam
             | Lvar _ -> lam
             | Lapply (l1,ll,info) ->
                 (match Lam_stats_util.get_arity meta l1 with
                  | NA  -> Lapply ((simpl l1), (List.map simpl ll), info)
                  | Determin (b,args,tail) ->
                      let len = List.length ll in
                      let rec take args =
                        match args with
                        | (x,_)::xs ->
                            if x = len
                            then
                              Lambda.Lapply
                                ((simpl l1), (List.map simpl ll),
                                  { info with apply_status = App_ml_full })
                            else
                              if x > len
                              then
                                (let fn = simpl l1 in
                                 let args = List.map simpl ll in
                                 Lam_util.eta_conversion (x - len)
                                   { info with apply_status = App_ml_full }
                                   fn args)
                              else
                                (let (first,rest) = Ext_list.take x ll in
                                 Lapply
                                   ((Lapply
                                       ((simpl l1), (List.map simpl first),
                                         {
                                           info with
                                           apply_status = App_ml_full
                                         })), (List.map simpl rest), info))
                        | _ -> Lapply ((simpl l1), (List.map simpl ll), info) in
                      take args)
             | Llet (str,v,l1,l2) -> Llet (str, v, (simpl l1), (simpl l2))
             | Lletrec (bindings,body) ->
                 let bindings =
                   List.map (fun (k,l)  -> (k, (simpl l))) bindings in
                 Lletrec (bindings, (simpl body))
             | Lprim (prim,ll) -> Lprim (prim, (List.map simpl ll))
             | Lfunction (kind,params,l) ->
                 Lfunction (kind, params, (simpl l))
             | Lswitch
                 (l,{ sw_failaction; sw_consts; sw_blocks; sw_numblocks;
                      sw_numconsts })
                 ->
                 Lswitch
                   ((simpl l),
                     {
                       sw_consts =
                         (List.map (fun (v,l)  -> (v, (simpl l))) sw_consts);
                       sw_blocks =
                         (List.map (fun (v,l)  -> (v, (simpl l))) sw_blocks);
                       sw_numconsts;
                       sw_numblocks;
                       sw_failaction =
                         ((match sw_failaction with
                           | None  -> None
                           | Some x -> Some (simpl x)))
                     })
             | Lstringswitch (l,sw,d) ->
                 Lam_comb.stringswitch (simpl l)
                   (List.map (fun (i,l)  -> (i, (simpl l))) sw)
                   (match d with | Some d -> Some (simpl d) | None  -> None)
             | Lstaticraise (i,ls) -> Lstaticraise (i, (List.map simpl ls))
             | Lstaticcatch (l1,(i,x),l2) ->
                 Lstaticcatch ((simpl l1), (i, x), (simpl l2))
             | Ltrywith (l1,v,l2) -> Ltrywith ((simpl l1), v, (simpl l2))
             | Lifthenelse (l1,l2,l3) ->
                 Lam_comb.if_ (simpl l1) (simpl l2) (simpl l3)
             | Lsequence (l1,l2) -> Lsequence ((simpl l1), (simpl l2))
             | Lwhile (l1,l2) -> Lwhile ((simpl l1), (simpl l2))
             | Lfor (flag,l1,l2,dir,l3) ->
                 Lfor (flag, (simpl l1), (simpl l2), dir, (simpl l3))
             | Lassign (v,l) -> Lassign (v, (simpl l))
             | Lsend (u,m,o,ll,v) ->
                 Lsend (u, (simpl m), (simpl o), (List.map simpl ll), v)
             | Levent (l,event) -> Levent ((simpl l), event)
             | Lifused (v,l) -> Lifused (v, (simpl l)) in
           simpl lam : Lambda.lambda)
      end 
    module Js_shake :
      sig
        [@@@ocaml.text
          " A module to shake JS IR\n   \n    Tree shaking is not going to change the closure \n "]
        val shake_program : J.program -> J.program
      end =
      struct
        let get_initial_exports count_non_variable_declaration_statement
          (export_set : Ident_set.t) (block : J.block) =
          let result =
            List.fold_left
              (fun acc  ->
                 fun (st : J.statement)  ->
                   match st.statement_desc with
                   | Variable { ident; value;_} ->
                       if Ident_set.mem ident acc
                       then
                         (match value with
                          | None  -> acc
                          | Some x ->
                              let open Ident_set in
                                union
                                  (Js_analyzer.free_variables_of_expression
                                     empty empty x) acc)
                       else
                         (match value with
                          | None  -> acc
                          | Some x ->
                              if Js_analyzer.no_side_effect_expression x
                              then acc
                              else
                                let open Ident_set in
                                  union
                                    (Js_analyzer.free_variables_of_expression
                                       empty empty x) (add ident acc))
                   | _ ->
                       if
                         (Js_analyzer.no_side_effect_statement st) ||
                           (not count_non_variable_declaration_statement)
                       then acc
                       else
                         let open Ident_set in
                           union
                             (Js_analyzer.free_variables_of_statement empty
                                empty st) acc) export_set block in
          (result, (let open Ident_set in diff result export_set))[@@ocaml.doc
                                                                    " we also need make it complete \n "]
        let shake_program (program : J.program) =
          let debug_file = "pervasives.ml" in
          let _d () =
            if Ext_string.ends_with program.name debug_file
            then Ext_log.err __LOC__ "@[%s@]@." program.name in
          let shake_block block export_set =
            let block = List.rev @@ (Js_analyzer.rev_toplevel_flatten block) in
            let loop block export_set =
              (let rec aux acc block =
                 let (result,diff) = get_initial_exports false acc block in
                 if Ident_set.is_empty diff then result else aux result block in
               let (first_iteration,delta) =
                 get_initial_exports true export_set block in
               if not @@ (Ident_set.is_empty delta)
               then aux first_iteration block
               else first_iteration : Ident_set.t) in
            let really_set = loop block export_set in
            List.fold_right
              (fun (st : J.statement)  ->
                 fun acc  ->
                   match st.statement_desc with
                   | Variable { ident; value;_} ->
                       if Ident_set.mem ident really_set
                       then st :: acc
                       else
                         (match value with
                          | None  -> acc
                          | Some x ->
                              if Js_analyzer.no_side_effect_expression x
                              then acc
                              else st :: acc)
                   | _ ->
                       if Js_analyzer.no_side_effect_statement st
                       then acc
                       else st :: acc) block [] in
          {
            program with
            block = (shake_block program.block program.export_set)
          }
      end 
    module Js_program_loader :
      sig
        [@@@ocaml.text
          " A module to create the whole JS program IR with [requires] and [exports] "]
        val make_program : string -> Ident.t list -> J.block -> J.program
        val decorate_deps :
          J.required_modules -> string option -> J.program -> J.deps_program
        val string_of_module_id : Lam_module_ident.t -> string
      end =
      struct
        module E = Js_exp_make
        module S = Js_stmt_make
        open Js_output.Ops
        let string_of_module_id (x : Lam_module_ident.t) =
          (match x.kind with
           | Runtime |Ml  ->
               let id = x.id in
               let file = Printf.sprintf "%s.js" id.name in
               (match Js_config.get_env () with
                | Goog _ ->
                    let base = String.uncapitalize id.name in
                    (match Lam_compile_env.get_goog_package_name x with
                     | None |Some "" -> base
                     | Some v -> v ^ ("." ^ base))
                | Browser  ->
                    let target =
                      Filename.chop_extension @@ (String.uncapitalize file) in
                    if String_set.mem target Js_config.runtime_set
                    then "./runtime/" ^ target
                    else "./stdlib/" ^ target
                | AmdJS |NodeJS  ->
                    let filename = String.uncapitalize id.name in
                    if String_set.mem filename Js_config.runtime_set
                    then
                      let path =
                        match Sys.getenv "OCAML_JS_RUNTIME_PATH" with
                        | exception Not_found  ->
                            Filename.concat
                              (Filename.dirname
                                 (Filename.dirname Sys.executable_name))
                              "runtime"
                        | f -> f in
                      Ext_filename.node_relative_path (!Location.input_name)
                        (Filename.concat path filename)
                    else
                      (match Config_util.find file with
                       | exception Not_found  ->
                           (Ext_log.warn __LOC__
                              "@[%s not found in search path - while compiling %s @] "
                              file (!Location.input_name);
                            Printf.sprintf "%s" (String.uncapitalize id.name))
                       | path ->
                           Ext_filename.node_relative_path
                             (!Location.input_name) path))
           | External name -> name : string)
        let make_program name export_idents block =
          ({
             name;
             exports = export_idents;
             export_set = (Ident_set.of_list export_idents);
             block
           } : J.program)
        let decorate_deps modules side_effect program =
          ({ program; modules; side_effect } : J.deps_program)
      end 
    module Js_map =
      struct
        open J[@@ocaml.doc " GENERATED CODE, map visitor for JS IR  "]
        class virtual map =
          object (o : 'self_type)
            method string : string -> string= o#unknown
            method option :
              'a 'a_out .
                ('self_type -> 'a -> 'a_out) -> 'a option -> 'a_out option=
              fun _f_a  ->
                function
                | None  -> None
                | Some _x -> let _x = _f_a o _x in Some _x
            method list :
              'a 'a_out .
                ('self_type -> 'a -> 'a_out) -> 'a list -> 'a_out list=
              fun _f_a  ->
                function
                | [] -> []
                | _x::_x_i1 ->
                    let _x = _f_a o _x in
                    let _x_i1 = o#list _f_a _x_i1 in _x :: _x_i1
            method int : int -> int= o#unknown
            method bool : bool -> bool=
              function | false  -> false | true  -> true
            method vident : vident -> vident=
              function
              | Id _x -> let _x = o#ident _x in Id _x
              | Qualified (_x,_x_i1,_x_i2) ->
                  let _x = o#ident _x in
                  let _x_i1 = o#kind _x_i1 in
                  let _x_i2 = o#option (fun o  -> o#string) _x_i2 in
                  Qualified (_x, _x_i1, _x_i2)
            method variable_declaration :
              variable_declaration -> variable_declaration=
              fun
                { ident = _x; value = _x_i1; property = _x_i2;
                  ident_info = _x_i3 }
                 ->
                let _x = o#ident _x in
                let _x_i1 = o#option (fun o  -> o#expression) _x_i1 in
                let _x_i2 = o#property _x_i2 in
                let _x_i3 = o#ident_info _x_i3 in
                {
                  ident = _x;
                  value = _x_i1;
                  property = _x_i2;
                  ident_info = _x_i3
                }
            method tag_info : tag_info -> tag_info= o#unknown
            method statement_desc : statement_desc -> statement_desc=
              function
              | Block _x -> let _x = o#block _x in Block _x
              | Variable _x ->
                  let _x = o#variable_declaration _x in Variable _x
              | Exp _x -> let _x = o#expression _x in Exp _x
              | If (_x,_x_i1,_x_i2) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#block _x_i1 in
                  let _x_i2 = o#option (fun o  -> o#block) _x_i2 in
                  If (_x, _x_i1, _x_i2)
              | While (_x,_x_i1,_x_i2,_x_i3) ->
                  let _x = o#option (fun o  -> o#label) _x in
                  let _x_i1 = o#expression _x_i1 in
                  let _x_i2 = o#block _x_i2 in
                  let _x_i3 = o#unknown _x_i3 in
                  While (_x, _x_i1, _x_i2, _x_i3)
              | ForRange (_x,_x_i1,_x_i2,_x_i3,_x_i4,_x_i5) ->
                  let _x = o#option (fun o  -> o#for_ident_expression) _x in
                  let _x_i1 = o#finish_ident_expression _x_i1 in
                  let _x_i2 = o#for_ident _x_i2 in
                  let _x_i3 = o#for_direction _x_i3 in
                  let _x_i4 = o#block _x_i4 in
                  let _x_i5 = o#unknown _x_i5 in
                  ForRange (_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5)
              | Continue _x -> let _x = o#label _x in Continue _x
              | Break  -> Break
              | Return _x -> let _x = o#return_expression _x in Return _x
              | Int_switch (_x,_x_i1,_x_i2) ->
                  let _x = o#expression _x in
                  let _x_i1 =
                    o#list (fun o  -> o#case_clause (fun o  -> o#int)) _x_i1 in
                  let _x_i2 = o#option (fun o  -> o#block) _x_i2 in
                  Int_switch (_x, _x_i1, _x_i2)
              | String_switch (_x,_x_i1,_x_i2) ->
                  let _x = o#expression _x in
                  let _x_i1 =
                    o#list (fun o  -> o#case_clause (fun o  -> o#string))
                      _x_i1 in
                  let _x_i2 = o#option (fun o  -> o#block) _x_i2 in
                  String_switch (_x, _x_i1, _x_i2)
              | Throw _x -> let _x = o#expression _x in Throw _x
              | Try (_x,_x_i1,_x_i2) ->
                  let _x = o#block _x in
                  let _x_i1 =
                    o#option
                      (fun o  ->
                         fun (_x,_x_i1)  ->
                           let _x = o#exception_ident _x in
                           let _x_i1 = o#block _x_i1 in (_x, _x_i1)) _x_i1 in
                  let _x_i2 = o#option (fun o  -> o#block) _x_i2 in
                  Try (_x, _x_i1, _x_i2)
              | Debugger  -> Debugger
            method statement : statement -> statement=
              fun { statement_desc = _x; comment = _x_i1 }  ->
                let _x = o#statement_desc _x in
                let _x_i1 = o#option (fun o  -> o#string) _x_i1 in
                { statement_desc = _x; comment = _x_i1 }
            method return_expression :
              return_expression -> return_expression=
              fun { return_value = _x }  ->
                let _x = o#expression _x in { return_value = _x }
            method required_modules : required_modules -> required_modules=
              o#unknown
            method property_name : property_name -> property_name= o#unknown
            method property_map : property_map -> property_map=
              o#list
                (fun o  ->
                   fun (_x,_x_i1)  ->
                     let _x = o#property_name _x in
                     let _x_i1 = o#expression _x_i1 in (_x, _x_i1))
            method property : property -> property= o#unknown
            method program : program -> program=
              fun
                { name = _x; block = _x_i1; exports = _x_i2;
                  export_set = _x_i3 }
                 ->
                let _x = o#string _x in
                let _x_i1 = o#block _x_i1 in
                let _x_i2 = o#exports _x_i2 in
                let _x_i3 = o#unknown _x_i3 in
                {
                  name = _x;
                  block = _x_i1;
                  exports = _x_i2;
                  export_set = _x_i3
                }
            method number : number -> number= o#unknown
            method mutable_flag : mutable_flag -> mutable_flag= o#unknown
            method length_object : length_object -> length_object= o#unknown
            method label : label -> label= o#string
            method kind : kind -> kind= o#unknown
            method jsint : jsint -> jsint= o#unknown
            method int_op : int_op -> int_op= o#unknown
            method ident_info : ident_info -> ident_info= o#unknown
            method ident : ident -> ident= o#unknown
            method for_ident_expression :
              for_ident_expression -> for_ident_expression= o#expression
            method for_ident : for_ident -> for_ident= o#ident
            method for_direction : for_direction -> for_direction= o#unknown
            method finish_ident_expression :
              finish_ident_expression -> finish_ident_expression=
              o#expression
            method expression_desc : expression_desc -> expression_desc=
              function
              | Math (_x,_x_i1) ->
                  let _x = o#string _x in
                  let _x_i1 = o#list (fun o  -> o#expression) _x_i1 in
                  Math (_x, _x_i1)
              | Length (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#length_object _x_i1 in Length (_x, _x_i1)
              | Char_of_int _x -> let _x = o#expression _x in Char_of_int _x
              | Char_to_int _x -> let _x = o#expression _x in Char_to_int _x
              | Array_of_size _x ->
                  let _x = o#expression _x in Array_of_size _x
              | Array_copy _x -> let _x = o#expression _x in Array_copy _x
              | Array_append (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#expression _x_i1 in Array_append (_x, _x_i1)
              | String_append (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#expression _x_i1 in String_append (_x, _x_i1)
              | Int_of_boolean _x ->
                  let _x = o#expression _x in Int_of_boolean _x
              | Anything_to_number _x ->
                  let _x = o#expression _x in Anything_to_number _x
              | Bool _x -> let _x = o#bool _x in Bool _x
              | Typeof _x -> let _x = o#expression _x in Typeof _x
              | Not _x -> let _x = o#expression _x in Not _x
              | String_of_small_int_array _x ->
                  let _x = o#expression _x in String_of_small_int_array _x
              | Json_stringify _x ->
                  let _x = o#expression _x in Json_stringify _x
              | Anything_to_string _x ->
                  let _x = o#expression _x in Anything_to_string _x
              | Dump (_x,_x_i1) ->
                  let _x = o#unknown _x in
                  let _x_i1 = o#list (fun o  -> o#expression) _x_i1 in
                  Dump (_x, _x_i1)
              | Seq (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#expression _x_i1 in Seq (_x, _x_i1)
              | Cond (_x,_x_i1,_x_i2) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#expression _x_i1 in
                  let _x_i2 = o#expression _x_i2 in Cond (_x, _x_i1, _x_i2)
              | Bin (_x,_x_i1,_x_i2) ->
                  let _x = o#binop _x in
                  let _x_i1 = o#expression _x_i1 in
                  let _x_i2 = o#expression _x_i2 in Bin (_x, _x_i1, _x_i2)
              | FlatCall (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#expression _x_i1 in FlatCall (_x, _x_i1)
              | Bind (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#expression _x_i1 in Bind (_x, _x_i1)
              | Call (_x,_x_i1,_x_i2) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#list (fun o  -> o#expression) _x_i1 in
                  let _x_i2 = o#unknown _x_i2 in Call (_x, _x_i1, _x_i2)
              | String_access (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#expression _x_i1 in String_access (_x, _x_i1)
              | Access (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#expression _x_i1 in Access (_x, _x_i1)
              | Dot (_x,_x_i1,_x_i2) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#string _x_i1 in
                  let _x_i2 = o#bool _x_i2 in Dot (_x, _x_i1, _x_i2)
              | New (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 =
                    o#option (fun o  -> o#list (fun o  -> o#expression))
                      _x_i1 in
                  New (_x, _x_i1)
              | Var _x -> let _x = o#vident _x in Var _x
              | Fun (_x,_x_i1,_x_i2) ->
                  let _x = o#list (fun o  -> o#ident) _x in
                  let _x_i1 = o#block _x_i1 in
                  let _x_i2 = o#unknown _x_i2 in Fun (_x, _x_i1, _x_i2)
              | Str (_x,_x_i1) ->
                  let _x = o#bool _x in
                  let _x_i1 = o#string _x_i1 in Str (_x, _x_i1)
              | Raw_js_code (_x,_x_i1) ->
                  let _x = o#string _x in
                  let _x_i1 = o#code_info _x_i1 in Raw_js_code (_x, _x_i1)
              | Array (_x,_x_i1) ->
                  let _x = o#list (fun o  -> o#expression) _x in
                  let _x_i1 = o#mutable_flag _x_i1 in Array (_x, _x_i1)
              | Caml_block (_x,_x_i1,_x_i2,_x_i3) ->
                  let _x = o#list (fun o  -> o#expression) _x in
                  let _x_i1 = o#mutable_flag _x_i1 in
                  let _x_i2 = o#expression _x_i2 in
                  let _x_i3 = o#tag_info _x_i3 in
                  Caml_block (_x, _x_i1, _x_i2, _x_i3)
              | Caml_uninitialized_obj (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#expression _x_i1 in
                  Caml_uninitialized_obj (_x, _x_i1)
              | Caml_block_tag _x ->
                  let _x = o#expression _x in Caml_block_tag _x
              | Caml_block_set_tag (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#expression _x_i1 in
                  Caml_block_set_tag (_x, _x_i1)
              | Caml_block_set_length (_x,_x_i1) ->
                  let _x = o#expression _x in
                  let _x_i1 = o#expression _x_i1 in
                  Caml_block_set_length (_x, _x_i1)
              | Number _x -> let _x = o#number _x in Number _x
              | Object _x -> let _x = o#property_map _x in Object _x
            method expression : expression -> expression=
              fun { expression_desc = _x; comment = _x_i1 }  ->
                let _x = o#expression_desc _x in
                let _x_i1 = o#option (fun o  -> o#string) _x_i1 in
                { expression_desc = _x; comment = _x_i1 }
            method exports : exports -> exports= o#unknown
            method exception_ident : exception_ident -> exception_ident=
              o#ident
            method deps_program : deps_program -> deps_program=
              fun { program = _x; modules = _x_i1; side_effect = _x_i2 }  ->
                let _x = o#program _x in
                let _x_i1 = o#required_modules _x_i1 in
                let _x_i2 = o#option (fun o  -> o#string) _x_i2 in
                { program = _x; modules = _x_i1; side_effect = _x_i2 }
            method code_info : code_info -> code_info= o#unknown
            method case_clause :
              'a 'a_out .
                ('self_type -> 'a -> 'a_out) ->
                  'a case_clause -> 'a_out case_clause=
              fun _f_a  ->
                fun { case = _x; body = _x_i1 }  ->
                  let _x = _f_a o _x in
                  let _x_i1 =
                    (fun (_x,_x_i1)  ->
                       let _x = o#block _x in
                       let _x_i1 = o#bool _x_i1 in (_x, _x_i1)) _x_i1 in
                  { case = _x; body = _x_i1 }
            method block : block -> block= o#list (fun o  -> o#statement)
            method binop : binop -> binop= o#unknown
            method unknown : 'a . 'a -> 'a= fun x  -> x
          end
      end
    module Js_pass_tailcall_inline :
      sig
        [@@@ocaml.text
          " This pass detect functions used once and if it is used in used\n    in the tail position, it will get inlined, this will help \n    remove some common use cases like This\n    {[\n      let length x = \n        let rec aux n x = \n          match x with \n          | [] -> n \n          | _ :: rest -> aux (n + 1) rest in\n        aux 0 x         \n    ]} \n"]
        val tailcall_inline : J.program -> J.program
      end =
      struct
        module S = Js_stmt_make
        module E = Js_exp_make
        let count_collects () =
          object (self)
            inherit  Js_fold.fold as super
            val stats = (Hashtbl.create 83 : (Ident.t,int ref) Hashtbl.t)
            val defined_idents =
              (Hashtbl.create 83 : (Ident.t,J.variable_declaration) Hashtbl.t)
            val mutable export_set = (Ident_set.empty : Ident_set.t)
            val mutable name = ("" : string)
            method add_use id =
              match Hashtbl.find stats id with
              | exception Not_found  -> Hashtbl.add stats id (ref 1)
              | v -> incr v
            method! program x =
              export_set <- x.export_set; name <- x.name; super#program x
            method! variable_declaration
              ({ ident; value; property; ident_info } as v) =
              Hashtbl.add defined_idents ident v;
              (match value with | None  -> self | Some x -> self#expression x)
            method! ident id = self#add_use id; self
            method get_stats =
              Hashtbl.iter
                (fun ident  ->
                   fun (v : J.variable_declaration)  ->
                     if Ident_set.mem ident export_set
                     then Js_op_util.update_used_stats v.ident_info Exported
                     else
                       (let pure =
                          match v.value with
                          | None  -> false
                          | Some x -> Js_analyzer.no_side_effect_expression x in
                        match Hashtbl.find stats ident with
                        | exception Not_found  ->
                            Js_op_util.update_used_stats v.ident_info
                              (if pure then Dead_pure else Dead_non_pure)
                        | num ->
                            if (!num) = 1
                            then
                              Js_op_util.update_used_stats v.ident_info
                                (if pure then Once_pure else Used)))
                defined_idents;
              defined_idents
          end[@@ocaml.doc
               " Update ident info use cases, it is a non pure function, \n    it will annotate [program] with some meta data\n    TODO: Ident Hashtbl could be improved, \n    since in this case it can not be global?  \n\n "]
        let get_stats program =
          ((count_collects ())#program program)#get_stats
        let subst name export_set stats =
          object (self)
            inherit  Js_map.map as super
            method! statement st =
              match st with
              | {
                  statement_desc = Variable
                    { value = _; ident_info = { used_stats = Dead_pure  } };
                  comment = _ } -> S.block []
              | {
                  statement_desc = Variable
                    { ident_info = { used_stats = Dead_non_pure  };
                      value = Some v;_};_}
                  -> S.exp v
              | _ -> super#statement st
            method! variable_declaration
              ({ ident; value; property; ident_info } as v) =
              let v = super#variable_declaration v in
              Hashtbl.add stats ident v; v
            method! block bs =
              match bs with
              | ({
                   statement_desc = Variable
                     ({ value = Some ({ expression_desc = Fun _;_} as v) } as
                        vd);
                   comment = _ } as st)::rest
                  ->
                  let is_export = Ident_set.mem vd.ident export_set in
                  if is_export
                  then (self#statement st) :: (self#block rest)
                  else
                    (match (Hashtbl.find stats vd.ident : J.variable_declaration)
                     with
                     | exception Not_found  ->
                         if Js_analyzer.no_side_effect_expression v
                         then (S.exp v) :: (self#block rest)
                         else self#block rest
                     | _ -> (self#statement st) :: (self#block rest))
              | ({
                   statement_desc = Return
                     {
                       return_value =
                         {
                           expression_desc = Call
                             ({ expression_desc = Var (Id id) },args,_info)
                           }
                       }
                   } as st)::rest
                  ->
                  (match Hashtbl.find stats id with
                   | exception Not_found  -> (self#statement st) ::
                       (self#block rest)
                   | {
                       value = Some
                         { expression_desc = Fun (params,block,_env);
                           comment = _ };
                       property = (Alias |StrictOpt |Strict );
                       ident_info = { used_stats = Once_pure  }; ident = _ }
                       as v when Ext_list.same_length params args ->
                       (Js_op_util.update_used_stats v.ident_info Dead_pure;
                        (let block =
                           List.fold_right2
                             (fun param  ->
                                fun arg  ->
                                  fun acc  ->
                                    (S.define ~kind:Variable param arg) ::
                                    acc) params args (self#block block) in
                         block @ (self#block rest)))
                   | _ -> (self#statement st) :: (self#block rest))
              | x::xs -> (self#statement x) :: (self#block xs)
              | [] -> []
          end[@@ocaml.doc
               " There is a side effect when traversing dead code, since \n   we assume that substitue a node would mark a node as dead node,\n  \n    so if we traverse a dead node, this would get a wrong result.\n   it does happen in such scenario\n   {[\n     let generic_basename is_dir_sep current_dir_name name =\n       let rec find_end n =\n         if n < 0 then String.sub name 0 1\n         else if is_dir_sep name n then find_end (n - 1)\n         else find_beg n (n + 1)\n       and find_beg n p =\n         if n < 0 then String.sub name 0 p\n         else if is_dir_sep name n then String.sub name (n + 1) (p - n - 1)\n         else find_beg (n - 1) p\n       in\n       if name = \"\"\n       then current_dir_name\n       else find_end (String.length name - 1)\n   ]}\n   [find_beg] can potentially be expanded in [find_end] and in [find_end]'s expansion, \n   if the order is not correct, or even worse, only the wrong one [find_beg] in [find_end] get expanded \n   (when we forget to recursive apply), then some code non-dead [find_beg] will be marked as dead, \n   while it is still called \n"]
        let tailcall_inline (program : J.program) =
          let _stats = get_stats program in
          let _export_set = program.export_set in
          program |> (subst program.name _export_set _stats)#program
      end 
    module Js_pass_scope :
      sig
        [@@@ocaml.text " A module to do scope analysis over JS IR "]
        val program : J.program -> Ident_set.t
      end =
      struct
        let _l idents =
          Ext_log.err __LOC__ "hey .. %s@."
            ((String.concat ",") @@
               (List.map (fun i  -> i.Ident.name) idents))
        let scope_pass =
          object (self)
            inherit  Js_fold.fold as super
            val defined_idents = Ident_set.empty
            val used_idents = Ident_set.empty[@@ocaml.doc
                                               " [used_idents] \n        does not contain locally defined idents "]
            [@@ocaml.doc
              " we need collect mutable values and loop defined varaibles "]
            val loop_mutable_values = Ident_set.empty[@@ocaml.doc
                                                       " we need collect mutable values and loop defined varaibles "]
            val mutable_values = Ident_set.empty
            val closured_idents = Ident_set.empty
            val in_loop = false[@@ocaml.doc " check if in loop or not "]
            method get_in_loop = in_loop
            method get_defined_idents = defined_idents
            method get_used_idents = used_idents
            method get_loop_mutable_values = loop_mutable_values
            method get_mutable_values = mutable_values
            method get_closured_idents = closured_idents
            method with_in_loop b =
              if b = self#get_in_loop then self else {<in_loop = b>}
            method with_loop_mutable_values b = {<loop_mutable_values = b>}
            method add_loop_mutable_variable id =
              {<loop_mutable_values = Ident_set.add id loop_mutable_values;
                mutable_values = Ident_set.add id mutable_values>}
            method add_mutable_variable id =
              {<mutable_values = Ident_set.add id mutable_values>}
            method add_defined_ident ident =
              {<defined_idents = Ident_set.add ident defined_idents>}
            method! expression x =
              match x.expression_desc with
              | Fun (params,block,env) ->
                  let param_set = Ident_set.of_list params in
                  let obj =
                    ({<defined_idents = Ident_set.empty;used_idents =
                                                          Ident_set.empty;
                       in_loop = false;loop_mutable_values = Ident_set.empty;
                       mutable_values =
                         Ident_set.of_list
                           (Js_fun_env.get_mutable_params params env);
                       closured_idents = Ident_set.empty>})#block block in
                  let (defined_idents',used_idents') =
                    ((obj#get_defined_idents), (obj#get_used_idents)) in
                  (params |>
                     (List.iteri
                        (fun i  ->
                           fun v  ->
                             if not (Ident_set.mem v used_idents')
                             then Js_fun_env.mark_unused env i));
                   (let closured_idents' =
                      let open Ident_set in
                        diff used_idents' (union defined_idents' param_set) in
                    Js_fun_env.set_bound env closured_idents';
                    (let lexical_scopes =
                       let open Ident_set in
                         inter closured_idents' self#get_loop_mutable_values in
                     Js_fun_env.set_lexical_scope env lexical_scopes;
                     {<used_idents =
                         Ident_set.union used_idents closured_idents';
                       closured_idents =
                         Ident_set.union closured_idents closured_idents'>})))
              | _ -> super#expression x
            method! variable_declaration x =
              match x with
              | { ident; value; property } ->
                  let obj =
                    (match ((self#get_in_loop), property) with
                     | (true ,Variable ) ->
                         self#add_loop_mutable_variable ident
                     | (true ,(Strict |StrictOpt |Alias )) ->
                         (match value with
                          | None  -> self#add_loop_mutable_variable ident
                          | Some x ->
                              (match x.expression_desc with
                               | Fun _|Number _|Str _ -> self
                               | _ -> self#add_loop_mutable_variable ident))
                     | (false ,Variable ) -> self#add_mutable_variable ident
                     | (false ,(Strict |StrictOpt |Alias )) -> self)#add_defined_ident
                      ident in
                  (match value with
                   | None  -> obj
                   | Some x -> obj#expression x)
            method! statement x =
              match x.statement_desc with
              | ForRange (_,_,loop_id,_,_,a_env) as y ->
                  let obj =
                    ({<in_loop = true;loop_mutable_values =
                                        Ident_set.singleton loop_id;used_idents
                                                                    =
                                                                    Ident_set.empty;
                       defined_idents = Ident_set.singleton loop_id;closured_idents
                                                                    =
                                                                    Ident_set.empty>})#statement_desc
                      y in
                  let (defined_idents',used_idents',closured_idents') =
                    ((obj#get_defined_idents), (obj#get_used_idents),
                      (obj#get_closured_idents)) in
                  let lexical_scope =
                    let open Ident_set in
                      inter (diff closured_idents' defined_idents')
                        self#get_loop_mutable_values in
                  let () = Js_closure.set_lexical_scope a_env lexical_scope in
                  {<used_idents = Ident_set.union used_idents used_idents';
                    defined_idents =
                      Ident_set.union defined_idents defined_idents';
                    closured_idents =
                      Ident_set.union closured_idents lexical_scope>}
              | While (_label,pred,body,_env) ->
                  (((self#expression pred)#with_in_loop true)#block body)#with_in_loop
                    self#get_in_loop
              | _ -> super#statement x
            method! exception_ident x =
              {<used_idents = Ident_set.add x used_idents;defined_idents =
                                                            Ident_set.add x
                                                              defined_idents>}
            method! for_ident x =
              {<loop_mutable_values = Ident_set.add x loop_mutable_values>}
            method! ident x =
              if Ident_set.mem x defined_idents
              then self
              else {<used_idents = Ident_set.add x used_idents>}
          end
        let program js = (scope_pass#program js)#get_loop_mutable_values
      end 
    module Js_pass_flatten_and_mark_dead :
      sig
        [@@@ocaml.text
          " A pass to mark some declarations in JS IR as dead code "]
        val program : J.program -> J.program
      end =
      struct
        module E = Js_exp_make
        module S = Js_stmt_make
        class count var =
          object (self : 'self)
            val mutable appears = 0
            inherit  Js_fold.fold as super
            method! ident x =
              if Ident.same x var then appears <- appears + 1; self
            method get_appears = appears
          end
        class rewrite_return ?return_value  () =
          let mk_return =
            match return_value with
            | None  -> (fun e  -> S.exp e)
            | Some ident -> (fun e  -> S.define ~kind:Variable ident e)
          in
          object (self : 'self)
            inherit  Js_map.map as super
            method! statement x =
              match x.statement_desc with
              | Return { return_value = e } -> mk_return e
              | _ -> super#statement x
            method! expression x = x
          end
        let mark_dead =
          object (self)
            inherit  Js_fold.fold as super
            val mutable name = ""
            val mutable ident_use_stats =
              (Hashtbl.create 17 : (Ident.t,[ `Info of J.ident_info 
                                            | `Recursive ])
                                     Hashtbl.t)
            val mutable export_set = (Ident_set.empty : Ident_set.t)
            method mark_not_dead ident =
              match Hashtbl.find ident_use_stats ident with
              | exception Not_found  ->
                  Hashtbl.add ident_use_stats ident `Recursive
              | `Recursive -> ()
              | `Info x -> Js_op_util.update_used_stats x Used
            method scan b ident (ident_info : J.ident_info) =
              let is_export = Ident_set.mem ident export_set in
              let () =
                if is_export
                then Js_op_util.update_used_stats ident_info Exported in
              match Hashtbl.find ident_use_stats ident with
              | `Recursive ->
                  (Js_op_util.update_used_stats ident_info Used;
                   Hashtbl.replace ident_use_stats ident (`Info ident_info))
              | `Info _ ->
                  Ext_log.warn __LOC__ "@[%s$%d in %s@]" ident.name
                    ident.stamp name
              | exception Not_found  ->
                  (Hashtbl.add ident_use_stats ident (`Info ident_info);
                   Js_op_util.update_used_stats ident_info
                     (if b then Scanning_pure else Scanning_non_pure))
            method promote_dead =
              Hashtbl.iter
                (fun _id  ->
                   fun (info : [ `Info of J.ident_info  | `Recursive ])  ->
                     match info with
                     | `Info ({ used_stats = Scanning_pure  } as info) ->
                         Js_op_util.update_used_stats info Dead_pure
                     | `Info ({ used_stats = Scanning_non_pure  } as info) ->
                         Js_op_util.update_used_stats info Dead_non_pure
                     | _ -> ()) ident_use_stats;
              Hashtbl.clear ident_use_stats
            method! program x =
              export_set <- x.export_set; name <- x.name; super#program x
            method! ident x = self#mark_not_dead x; self
            method! variable_declaration vd =
              match vd with
              | { ident_info = { used_stats = Dead_pure  };_} -> self
              | { ident_info = { used_stats = Dead_non_pure  }; value } ->
                  (match value with
                   | None  -> self
                   | Some x -> self#expression x)
              | { ident; ident_info; value;_} ->
                  let pure =
                    match value with
                    | None  -> false
                    | Some x ->
                        (ignore (self#expression x);
                         Js_analyzer.no_side_effect_expression x) in
                  (self#scan pure ident ident_info; self)
          end
        let mark_dead_code js =
          let _ = mark_dead#program js in mark_dead#promote_dead; js
        let subst_map name =
          object (self)
            inherit  Js_map.map as super
            val mutable substitution = Hashtbl.create 17
            method get_substitution = substitution
            method add_substitue (ident : Ident.t) (e : J.expression) =
              Hashtbl.replace substitution ident e
            method! statement v =
              match v.statement_desc with
              | Variable { ident; ident_info = { used_stats = Dead_pure  };_}
                  -> { v with statement_desc = (Block []) }
              | Variable
                  { ident; ident_info = { used_stats = Dead_non_pure  };
                    value = None  }
                  -> { v with statement_desc = (Block []) }
              | Variable
                  { ident; ident_info = { used_stats = Dead_non_pure  };
                    value = Some x }
                  -> { v with statement_desc = (Exp x) }
              | Variable
                  ({ ident; property = (Strict |StrictOpt |Alias );
                     value = Some
                       ({
                          expression_desc = Caml_block
                            ((_::_::_ as ls),Immutable ,tag,tag_info)
                          } as block)
                     } as variable)
                  ->
                  let (_,e,bindings) =
                    List.fold_left
                      (fun (i,e,acc)  ->
                         fun (x : J.expression)  ->
                           match x.expression_desc with
                           | J.Var _|Number _|Str _ ->
                               ((i + 1), (x :: e), acc)
                           | _ ->
                               let v' = self#expression x in
                               let match_id =
                                 Ext_ident.create
                                   (Printf.sprintf "%s_%03d" ident.name i) in
                               ((i + 1), ((E.var match_id) :: e),
                                 ((match_id, v') :: acc))) (0, [], []) ls in
                  let e =
                    {
                      block with
                      expression_desc =
                        (Caml_block ((List.rev e), Immutable, tag, tag_info))
                    } in
                  let () = self#add_substitue ident e in
                  let original_statement =
                    {
                      v with
                      statement_desc =
                        (Variable { variable with value = (Some e) })
                    } in
                  (match bindings with
                   | [] -> original_statement
                   | _ ->
                       S.block @@
                         (Ext_list.rev_map_acc [original_statement]
                            (fun (id,v)  -> S.define ~kind:Strict id v)
                            bindings))
              | _ -> super#statement v
            method! expression x =
              match x.expression_desc with
              | Access
                  ({ expression_desc = Var (Id id) },{
                                                       expression_desc =
                                                         Number (Int 
                                                         { i;_})
                                                       })
                  ->
                  (match Hashtbl.find self#get_substitution id with
                   | { expression_desc = Caml_block (ls,Immutable ,_,_) } ->
                       (match List.nth ls (Int32.to_int i) with
                        | { expression_desc = (J.Var _|Number _|Str _) } as x
                            -> x
                        | exception _ ->
                            (Ext_log.err __LOC__
                               "suspcious code %s when compiling %s@."
                               (Printf.sprintf "%s/%d" id.name id.stamp) name;
                             super#expression x)
                        | _ -> super#expression x)
                   | _ -> super#expression x
                   | exception Not_found  -> super#expression x)
              | _ -> super#expression x
          end
        let program (js : J.program) =
          (js |> (subst_map js.name)#program) |> mark_dead_code
      end 
    module Js_pass_flatten :
      sig
        [@@@ocaml.text
          " A pass converting nested js statement into a flatten visual appearance \n\n    Note this module is used to convert some nested expressions to flat statements, \n    in general, it's more human readable, and since it generate flat statements, we can spot\n    some inline opportunities for the produced statemetns, \n    (inline) expressions inside a nested expression would generate ugly code.\n\n    Since we are aiming to flatten expressions, we should avoid some smart constructors in {!Js_helper}, \n    it  tries to spit out expression istead of statements if it can\n"]
        val program : J.program -> J.program
      end =
      struct
        module E = Js_exp_make
        module S = Js_stmt_make
        let flatten_map =
          object (self)
            inherit  Js_map.map as super
            method! statement x =
              match x.statement_desc with
              | Exp ({ expression_desc = Seq _;_} as v) ->
                  S.block
                    (List.rev_map self#statement
                       (Js_analyzer.rev_flatten_seq v))
              | Exp { expression_desc = Cond (a,b,c); comment } ->
                  {
                    statement_desc =
                      (If
                         (a, [self#statement (S.exp b)],
                           (Some [self#statement (S.exp c)])));
                    comment
                  }
              | Exp
                  {
                    expression_desc = Bin
                      (Eq ,a,({ expression_desc = Seq _;_} as v));_}
                  ->
                  let block = Js_analyzer.rev_flatten_seq v in
                  (match block with
                   | { statement_desc = Exp last_one;_}::rest_rev ->
                       S.block
                         (Ext_list.rev_map_append self#statement rest_rev
                            [self#statement @@ (S.exp (E.assign a last_one))])
                   | _ -> assert false)
              | Return
                  {
                    return_value =
                      { expression_desc = Cond (a,b,c); comment }
                    }
                  ->
                  {
                    statement_desc =
                      (If
                         (a, [self#statement (S.return b)],
                           (Some [self#statement (S.return c)])));
                    comment
                  }
              | Return { return_value = ({ expression_desc = Seq _;_} as v) }
                  ->
                  let block = Js_analyzer.rev_flatten_seq v in
                  (match block with
                   | { statement_desc = Exp last_one;_}::rest_rev ->
                       super#statement
                         (S.block
                            (Ext_list.rev_map_append self#statement rest_rev
                               [S.return last_one]))
                   | _ -> assert false)
              | Block (x::[]) -> self#statement x
              | _ -> super#statement x
            method! block b =
              match b with
              | { statement_desc = Block bs }::rest -> self#block (bs @ rest)
              | x::rest -> (self#statement x) :: (self#block rest)
              | [] -> []
          end
        let program (x : J.program) = flatten_map#program x
      end 
    module Js_pass_debug :
      sig val dump : string -> J.program -> J.program end =
      struct
        let log_counter = ref 0
        let dump name (prog : J.program) =
          let () =
            if
              ((Js_config.get_env ()) <> Browser) &&
                (Lam_current_unit.is_same_file ())
            then
              (incr log_counter;
               Ext_pervasives.with_file_as_chan
                 ((Ext_filename.chop_extension ~loc:__LOC__
                     (Lam_current_unit.get_file ()))
                    ^ (Printf.sprintf ".%02d.%s.jsx" (!log_counter) name))
                 (fun chan  -> Js_dump.dump_program prog chan)) in
          prog
      end 
    module Lam_compile_group :
      sig
        [@@@ocaml.text " BuckleScript entry point in the OCaml compiler "]
        [@@@ocaml.text
          " Compile and register the hook of function to compile  a lambda to JS IR \n "]
        val compile :
          filename:string ->
            bool ->
              Env.t -> Types.signature -> Lambda.lambda -> J.deps_program
        [@@ocaml.doc
          " For toplevel, [filename] is [\"\"] which is the same as\n    {!Env.get_unit_name ()}\n "]
        val lambda_as_module :
          Env.t -> Types.signature -> string -> Lambda.lambda -> unit
      end =
      struct
        module E = Js_exp_make
        module S = Js_stmt_make
        open Js_output.Ops
        exception Not_a_module
        let compile_group
          (({ filename = file_name; env } as meta) : Lam_stats.meta)
          (x : Lam_group.t) =
          (match (x, file_name) with
           | (Single
              (_,({ name = ("stdout"|"stderr"|"stdin");_} as id),_),"pervasives.ml")
               ->
               Js_output.of_stmt @@
                 (S.alias_variable id
                    ~exp:(E.runtime_ref Js_config.io id.name))
           | (Single (_,({ name = "infinity";_} as id),_),"pervasives.ml") ->
               Js_output.of_stmt @@
                 (S.alias_variable id ~exp:(E.js_global "Infinity"))
           | (Single
              (_,({ name = "neg_infinity";_} as id),_),"pervasives.ml") ->
               Js_output.of_stmt @@
                 (S.alias_variable id ~exp:(E.js_global "-Infinity"))
           | (Single (_,({ name = "nan";_} as id),_),"pervasives.ml") ->
               Js_output.of_stmt @@
                 (S.alias_variable id ~exp:(E.js_global "NaN"))
           | (Single (_,({ name = "^";_} as id),_),"pervasives.ml") ->
               Js_output.of_stmt @@
                 (S.alias_variable id
                    ~exp:(let a = Ext_ident.create "a" in
                          let b = Ext_ident.create "b" in
                          E.fun_ [a; b]
                            [S.return (E.string_append (E.var a) (E.var b))]))
           | (Single
              (_,({ name = "print_endline";_} as id),_),"pervasives.ml") ->
               Js_output.of_stmt @@
                 (S.alias_variable id
                    ~exp:(let param = Ext_ident.create "param" in
                          E.fun_ [param]
                            [S.return
                               (E.seq
                                  (E.call
                                     ~info:{
                                             arity = Full;
                                             call_info = Call_na
                                           } (E.js_global "console.log")
                                     [E.var param]) E.zero_int_literal)]))
           | (Single
              (_,({ name = "prerr_endline";_} as id),_),"pervasives.ml") ->
               Js_output.of_stmt @@
                 (S.alias_variable id
                    ~exp:(let param = Ext_ident.create "param" in
                          E.fun_ [param]
                            [S.return
                               (E.seq
                                  (E.call
                                     ~info:{
                                             arity = Full;
                                             call_info = Call_na
                                           } (E.js_global "console.error")
                                     [E.var param]) E.zero_int_literal)]))
           | (Single
              (_,({ name = "string_of_int";_} as id),_),"pervasives.ml") ->
               Js_output.of_stmt @@
                 (S.alias_variable id
                    ~exp:(let arg = Ext_ident.create "param" in
                          E.fun_ [arg]
                            [S.return (E.anything_to_string (E.var arg))]))
           | (Single (_,({ name = "max_float";_} as id),_),"pervasives.ml")
               ->
               Js_output.of_stmt @@
                 (S.alias_variable id
                    ~exp:(E.js_global_dot "Number" "MAX_VALUE"))
           | (Single (_,({ name = "min_float";_} as id),_),"pervasives.ml")
               ->
               Js_output.of_stmt @@
                 (S.alias_variable id
                    ~exp:(E.js_global_dot "Number" "MIN_VALUE"))
           | (Single
              (_,({ name = "epsilon_float";_} as id),_),"pervasives.ml") ->
               Js_output.of_stmt @@
                 (S.alias_variable id ~exp:(E.float "2.220446049250313e-16"))
           | (Single (_,({ name = "cat";_} as id),_),"bytes.ml") ->
               Js_output.of_stmt @@
                 (S.alias_variable id
                    ~exp:(let a = Ext_ident.create "a" in
                          let b = Ext_ident.create "b" in
                          E.fun_ [a; b]
                            [S.return (E.array_append (E.var a) (E.var b))]))
           | (Single
              (_,({ name = ("max_array_length"|"max_string_length");_} as id),_),"sys.ml")
               ->
               Js_output.of_stmt @@
                 (S.alias_variable id ~exp:(E.float "4_294_967_295."))
           | (Single
              (_,({ name = "max_int";_} as id),_),("sys.ml"|"nativeint.ml"))
               ->
               Js_output.of_stmt @@
                 (S.alias_variable id ~exp:(E.float "9007199254740991."))
           | (Single
              (_,({ name = "min_int";_} as id),_),("sys.ml"|"nativeint.ml"))
               ->
               Js_output.of_stmt @@
                 (S.alias_variable id ~exp:(E.float "-9007199254740991."))
           | (Single (kind,id,lam),_) ->
               Lam_compile.compile_let kind
                 {
                   st = (Declare (kind, id));
                   should_return = False;
                   jmp_table = Lam_compile_defs.empty_handler_map;
                   meta
                 } id lam
           | (Recursive id_lams,_) ->
               Lam_compile.compile_recursive_lets
                 {
                   st = EffectCall;
                   should_return = False;
                   jmp_table = Lam_compile_defs.empty_handler_map;
                   meta
                 } id_lams
           | (Nop lam,_) ->
               Lam_compile.compile_lambda
                 {
                   st = EffectCall;
                   should_return = False;
                   jmp_table = Lam_compile_defs.empty_handler_map;
                   meta
                 } lam : Js_output.t)
        let compile ~filename  non_export env _sigs lam =
          let export_idents =
            if non_export then [] else Translmod.get_export_identifiers () in
          let () =
            export_idents |>
              (List.iter
                 (fun (id : Ident.t)  ->
                    Ext_log.dwarn __LOC__ "export: %s/%d" id.name id.stamp)) in
          let () = Translmod.reset () in
          let () = Lam_compile_env.reset () in
          let _d = Lam_util.dump env in
          let _j = Js_pass_debug.dump in
          let lam = _d "initial" lam in
          let lam = Lam_group.deep_flatten lam in
          let lam = _d "flatten" lam in
          let meta =
            Lam_pass_collect.count_alias_globals env filename export_idents
              lam in
          let lam =
            let lam =
              ((((((lam |> (_d "flattern")) |> Lam_pass_exits.simplify_exits)
                    |> (_d "simplyf_exits"))
                   |> (Lam_pass_remove_alias.simplify_alias meta))
                  |> (_d "simplify_alias"))
                 |> Lam_group.deep_flatten)
                |> (_d "flatten") in
            let () = Lam_pass_collect.collect_helper meta lam in
            let lam = Lam_pass_remove_alias.simplify_alias meta lam in
            let lam = Lam_group.deep_flatten lam in
            let () = Lam_pass_collect.collect_helper meta lam in
            let lam =
              ((lam |> (_d "alpha_before")) |>
                 (Lam_pass_alpha_conversion.alpha_conversion meta))
                |> Lam_pass_exits.simplify_exits in
            let () = Lam_pass_collect.collect_helper meta lam in
            ((((((((lam |> (_d "simplify_alias_before")) |>
                     (Lam_pass_remove_alias.simplify_alias meta))
                    |> (_d "alpha_conversion"))
                   |> (Lam_pass_alpha_conversion.alpha_conversion meta))
                  |> (_d "simplify_lets"))
                 |> Lam_pass_lets_dce.simplify_lets)
                |> (_d "simplify_lets"))
               |> Lam_pass_exits.simplify_exits)
              |> (_d "simplify_lets") in
          match (lam : Lambda.lambda) with
          | Lprim (Psetglobal id,biglambda::[]) ->
              (match Lam_group.flatten [] biglambda with
               | (Lprim (Pmakeblock (_,_,_),lambda_exports),rest) ->
                   let (coercion_groups,new_exports,new_export_set,export_map)
                     =
                     if non_export
                     then ([], [], Ident_set.empty, Ident_map.empty)
                     else
                       List.fold_right2
                         (fun eid  ->
                            fun lam  ->
                              fun
                                (coercions,new_exports,new_export_set,export_map)
                                 ->
                                match (lam : Lambda.lambda) with
                                | Lvar id when
                                    (Ident.name id) = (Ident.name eid) ->
                                    (coercions, (id :: new_exports),
                                      (Ident_set.add id new_export_set),
                                      export_map)
                                | _ ->
                                    (((Lam_group.Single (Strict, eid, lam))
                                      :: coercions), (eid :: new_exports),
                                      (Ident_set.add eid new_export_set),
                                      (Ident_map.add eid lam export_map)))
                         meta.exports lambda_exports
                         ([], [], Ident_set.empty, Ident_map.empty) in
                   let () =
                     new_exports |>
                       (List.iter
                          (fun (id : Ident.t)  ->
                             Ext_log.dwarn __LOC__ "export: %s/%d" id.name
                               id.stamp)) in
                   let meta =
                     {
                       meta with
                       export_idents = new_export_set;
                       exports = new_exports
                     } in
                   let (export_map,rest) =
                     List.fold_left
                       (fun (export_map,acc)  ->
                          fun x  ->
                            ((match (x : Lam_group.t) with
                              | Single (_,id,lam) when
                                  Ident_set.mem id new_export_set ->
                                  Ident_map.add id lam export_map
                              | _ -> export_map), (x :: acc)))
                       (export_map, coercion_groups) rest in
                   let () =
                     if Lam_current_unit.is_same_file ()
                     then
                       let f =
                         (Ext_filename.chop_extension ~loc:__LOC__ filename)
                           ^ ".lambda" in
                       (Ext_pervasives.with_file_as_pp f) @@
                         (fun fmt  ->
                            Format.pp_print_list
                              ~pp_sep:Format.pp_print_newline
                              (Lam_group.pp_group env) fmt rest) in
                   let rest = Lam_dce.remove meta.exports rest in
                   let module E = struct exception Not_pure of string end in
                     let no_side_effects rest =
                       Ext_list.for_all_opt
                         (fun (x : Lam_group.t)  ->
                            match x with
                            | Single (kind,id,body) ->
                                (match kind with
                                 | Strict |Variable  ->
                                     if
                                       not @@
                                         (Lam_analysis.no_side_effects body)
                                     then Some (Printf.sprintf "%s" id.name)
                                     else None
                                 | _ -> None)
                            | Recursive bindings ->
                                Ext_list.for_all_opt
                                  (fun (id,lam)  ->
                                     if
                                       not @@
                                         (Lam_analysis.no_side_effects lam)
                                     then
                                       Some
                                         (Printf.sprintf "%s" id.Ident.name)
                                     else None) bindings
                            | Nop lam ->
                                if not @@ (Lam_analysis.no_side_effects lam)
                                then Some ""
                                else None) rest in
                     let maybe_pure = no_side_effects rest in
                     let body =
                       ((rest |>
                           (List.map (fun group  -> compile_group meta group)))
                          |> Js_output.concat)
                         |> Js_output.to_block in
                     let js =
                       Js_program_loader.make_program filename meta.exports
                         body in
                     ((((((((((js |> (_j "initial")) |>
                                Js_pass_flatten.program)
                               |> (_j "flattern"))
                              |> Js_pass_tailcall_inline.tailcall_inline)
                             |> (_j "inline_and_shake"))
                            |> Js_pass_flatten_and_mark_dead.program)
                           |> (_j "flatten_and_mark_dead"))
                          |>
                          (fun js  ->
                             ignore @@ (Js_pass_scope.program js); js))
                         |> Js_shake.shake_program)
                        |> (_j "shake"))
                       |>
                       ((fun (js : J.program)  ->
                           let external_module_ids =
                             Lam_compile_env.get_requried_modules meta.env
                               meta.required_modules
                               (Js_fold_basic.calculate_hard_dependencies
                                  js.block) in
                           let required_modules =
                             List.map
                               (fun id  ->
                                  ((Lam_module_ident.id id),
                                    (Js_program_loader.string_of_module_id id)))
                               external_module_ids in
                           let v =
                             Lam_stats_export.export_to_cmj meta maybe_pure
                               external_module_ids
                               (if non_export
                                then Ident_map.empty
                                else export_map) in
                           if not @@ (Ext_string.is_empty filename)
                           then
                             Js_cmj_format.to_file
                               ((Ext_filename.chop_extension ~loc:__LOC__
                                   filename)
                                  ^ ".cmj") v;
                           Js_program_loader.decorate_deps required_modules
                             v.effect js))
               | _ -> raise Not_a_module)
          | _ -> raise Not_a_module[@@ocaml.doc
                                     " Actually simplify_lets is kind of global optimization since it requires you to know whether \n    it's used or not \n    [non_export] is only used in playground\n"]
        let lambda_as_module env (sigs : Types.signature) (filename : string)
          (lam : Lambda.lambda) =
          Lam_current_unit.set_file filename;
          Lam_current_unit.iset_debug_file "tuple_alloc.ml";
          Ext_pervasives.with_file_as_chan
            ((Ext_filename.chop_extension ~loc:__LOC__ filename) ^
               (Js_config.get_ext ()))
            (fun chan  ->
               Js_dump.dump_deps_program
                 (compile ~filename false env sigs lam) chan)
      end 
    module Ext_map :
      sig
        module Make :
        functor (Ord : Map.OrderedType) ->
          sig
            include (Map.S with type  key =  Ord.t)
            val of_list : (key* 'a) list -> 'a t
          end
      end =
      struct
        module Make(S:Map.OrderedType) =
          struct
            include Map.Make(S)
            let of_list (xs : ('a* 'b) list) =
              List.fold_left (fun acc  -> fun (k,v)  -> add k v acc) empty xs
          end
      end 
    module Lam_register : sig  end =
      struct
        let () =
          Printlambda.serialize_raw_js := Lam_compile_group.lambda_as_module
      end 
    module Ident_util =
      struct
        [@@@ocaml.text " Utilities for [ident] type "]
        let print_identset set =
          Lambda.IdentSet.iter
            (fun x  -> Ext_log.err __LOC__ "@[%a@]@." Ident.print x) set
          [@@ocaml.text " Utilities for [ident] type "]
      end
    module Lam_runtime : sig  end =
      struct
        [@@@ocaml.text " Pre-defined runtime function name "]
        let builtin_modules =
          [("caml_array", true);
          ("caml_format", true);
          ("caml_md5", true);
          ("caml_sys", true);
          ("caml_bigarray", true);
          ("caml_hash", true);
          ("caml_obj", true);
          ("caml_c_ffi", true);
          ("caml_int64", true);
          ("caml_polyfill", true);
          ("caml_exceptions", true);
          ("caml_unix", true);
          ("caml_io", true);
          ("caml_primitive", true);
          ("caml_utils", true);
          ("caml_file", true);
          ("caml_lexer", true);
          ("caml_float", true);
          ("caml_marshal", true);
          ("caml_string", true)][@@ocaml.text
                                  " Pre-defined runtime function name "]
      end 
    module Ppx_entry =
      struct
        [@@@ocaml.text
          "\n1. extension point \n   {[ \n     [%unsafe{| blabla |}]\n   ]}\n   will be desugared into \n   {[ \n     let module Js = \n     struct unsafe_js : string -> 'a end \n     in Js.unsafe_js {| blabla |}\n   ]}\n   The major benefit is to better error reporting (with locations).\n   Otherwise\n\n   {[\n\n     let f u = Js.unsafe_js u \n     let _ = f (1 + 2)\n   ]}\n   And if it is inlined some where   \n"]
        let tmp_module_name = "J"[@@ocaml.text
                                   "\n1. extension point \n   {[ \n     [%unsafe{| blabla |}]\n   ]}\n   will be desugared into \n   {[ \n     let module Js = \n     struct unsafe_js : string -> 'a end \n     in Js.unsafe_js {| blabla |}\n   ]}\n   The major benefit is to better error reporting (with locations).\n   Otherwise\n\n   {[\n\n     let f u = Js.unsafe_js u \n     let _ = f (1 + 2)\n   ]}\n   And if it is inlined some where   \n"]
        let tmp_fn = "unsafe_expr"
        let predef_string_type = Ast_helper.Typ.var "string"
        let predef_any_type = Ast_helper.Typ.any ()
        let predef_unit_type = Ast_helper.Typ.var "unit"
        let predef_val_unit =
          Ast_helper.Exp.construct
            { txt = (Lident "()"); loc = Location.none } None
        let prim = "js_pure_expr"
        let prim_stmt = "js_pure_stmt"
        let prim_debugger = "js_debugger"
        let handle_raw ?ty  loc e attrs =
          let attrs =
            match ty with
            | Some ty -> (Parsetree_util.attr_attribute_from_type ty) ::
                attrs
            | None  -> attrs in
          (Ast_helper.Exp.letmodule { txt = tmp_module_name; loc }
             (Ast_helper.Mod.structure
                [Ast_helper.Str.primitive
                   (Ast_helper.Val.mk ~attrs { loc; txt = tmp_fn }
                      ~prim:[prim]
                      (Ast_helper.Typ.arrow "" predef_string_type
                         predef_any_type))]))
            @@
            (let u =
               Ast_helper.Exp.apply
                 (Ast_helper.Exp.ident
                    { txt = (Ldot ((Lident tmp_module_name), tmp_fn)); loc })
                 [("", e)] in
             match ty with
             | Some ty -> Ast_helper.Exp.constraint_ ~loc u ty
             | None  -> u)
        let rec unsafe_mapper: Ast_mapper.mapper =
          {
            Ast_mapper.default_mapper with
            expr =
              (fun mapper  ->
                 fun e  ->
                   match e.pexp_desc with
                   | Pexp_extension
                       ({ txt = "bs.raw"; loc },PStr
                        ({
                           pstr_desc = Pstr_eval
                             (({
                                 pexp_desc = Pexp_constant (Const_string
                                   (_,_));
                                 pexp_attributes = attrs } as e),_);
                           pstr_loc = _ }::[]))
                       -> handle_raw loc e attrs
                   | Pexp_extension
                     ({ txt = "bs.raw"; loc },PStr
                      ({
                         pstr_desc = Parsetree.Pstr_eval
                           ({
                              pexp_desc = Pexp_constraint
                                (({
                                    pexp_desc = Pexp_constant (Const_string
                                      (_,_));_}
                                    as e),ty);
                              pexp_attributes = attrs },_)
                         }::[]))|Pexp_constraint
                     ({
                        pexp_desc = Pexp_extension
                          ({ txt = "bs.raw"; loc },PStr
                           ({
                              pstr_desc = Pstr_eval
                                (({
                                    pexp_desc = Pexp_constant (Const_string
                                      (_,_));
                                    pexp_attributes = attrs } as e),_)
                              }::[]))
                        },ty)
                       -> handle_raw ~ty loc e attrs
                   | Pexp_extension
                       ({ txt = "bs.raw"; loc },(PTyp _|PPat _|PStr _)) ->
                       Location.raise_errorf ~loc
                         "bs.raw can only be applied to a string"
                   | Pexp_extension ({ txt = "bs.debug"; loc },payload) ->
                       (match payload with
                        | Parsetree.PStr [] ->
                            Ast_helper.Exp.letmodule
                              { txt = tmp_module_name; loc }
                              (Ast_helper.Mod.structure
                                 [Ast_helper.Str.primitive
                                    (Ast_helper.Val.mk { loc; txt = tmp_fn }
                                       ~prim:[prim_debugger]
                                       (Ast_helper.Typ.arrow ""
                                          predef_unit_type predef_unit_type))])
                              (Ast_helper.Exp.apply
                                 (Ast_helper.Exp.ident
                                    {
                                      txt =
                                        (Ldot
                                           ((Lident tmp_module_name), tmp_fn));
                                      loc
                                    }) [("", predef_val_unit)])
                        | Parsetree.PTyp _|Parsetree.PPat (_,_)
                          |Parsetree.PStr _ ->
                            Location.raise_errorf ~loc
                              "bs.raw can only be applied to a string")
                   | _ -> Ast_mapper.default_mapper.expr mapper e);
            structure_item =
              (fun mapper  ->
                 fun (str : Parsetree.structure_item)  ->
                   match str.pstr_desc with
                   | Pstr_extension
                       (({ txt = "bs.raw"; loc },payload),_attrs) ->
                       (match payload with
                        | Parsetree.PStr
                            ({
                               pstr_desc = Parsetree.Pstr_eval
                                 (({
                                     pexp_desc = Pexp_constant (Const_string
                                       (cont,opt_label));
                                     pexp_loc; pexp_attributes } as e),_);
                               pstr_loc }::[])
                            ->
                            Ast_helper.Str.eval @@
                              (Ast_helper.Exp.letmodule
                                 { txt = tmp_module_name; loc }
                                 (Ast_helper.Mod.structure
                                    [Ast_helper.Str.primitive
                                       (Ast_helper.Val.mk
                                          { loc; txt = tmp_fn }
                                          ~prim:[prim_stmt]
                                          (Ast_helper.Typ.arrow ""
                                             predef_string_type
                                             predef_any_type))])
                                 (Ast_helper.Exp.apply
                                    (Ast_helper.Exp.ident
                                       {
                                         txt =
                                           (Ldot
                                              ((Lident tmp_module_name),
                                                tmp_fn));
                                         loc
                                       }) [("", e)]))
                        | Parsetree.PTyp _|Parsetree.PPat (_,_)
                          |Parsetree.PStr _ ->
                            Location.raise_errorf ~loc
                              "bs.raw can only be applied to a string")
                   | _ -> Ast_mapper.default_mapper.structure_item mapper str)
          }
        let rewrite_signature:
          (Parsetree.signature -> Parsetree.signature) ref =
          ref (fun x  -> x)
        let rewrite_implementation:
          (Parsetree.structure -> Parsetree.structure) ref =
          ref (fun x  -> unsafe_mapper.structure unsafe_mapper x)
      end
    module Js_implementation :
      sig
        [@@@ocaml.text " High level compilation module "]
        val interface : Format.formatter -> string -> string -> unit[@@ocaml.doc
                                                                    " This module defines a function to compile the program directly into [js]\n    given [filename] and [outputprefix], \n    it will be useful if we don't care about bytecode output(generating js only).\n "]
        val implementation : Format.formatter -> string -> string -> unit
        [@@ocaml.doc
          " [implementation ppf sourcefile outprefix] compiles to JS directly "]
      end =
      struct
        open Format
        open Typedtree
        open Compenv
        let fprintf = Format.fprintf
        let tool_name = "bucklescript"
        let print_if ppf flag printer arg =
          if !flag then fprintf ppf "%a@." printer arg; arg
        let interface ppf sourcefile outputprefix =
          Compmisc.init_path false;
          (let modulename = module_of_filename ppf sourcefile outputprefix in
           Env.set_unit_name modulename;
           (let initial_env = Compmisc.initial_env () in
            let ast = Pparse.parse_interface ~tool_name ppf sourcefile in
            let ast = (!Ppx_entry.rewrite_signature) ast in
            if !Clflags.dump_parsetree
            then fprintf ppf "%a@." Printast.interface ast;
            if !Clflags.dump_source
            then fprintf ppf "%a@." Pprintast.signature ast;
            (let tsg = Typemod.type_interface initial_env ast in
             if !Clflags.dump_typedtree
             then fprintf ppf "%a@." Printtyped.interface tsg;
             (let sg = tsg.sig_type in
              if !Clflags.print_types
              then
                Printtyp.wrap_printing_env initial_env
                  (fun ()  ->
                     fprintf std_formatter "%a@." Printtyp.signature
                       (Typemod.simplify_signature sg));
              ignore (Includemod.signatures initial_env sg sg);
              Typecore.force_delayed_checks ();
              Warnings.check_fatal ();
              if not (!Clflags.print_types)
              then
                (let sg =
                   Env.save_signature sg modulename (outputprefix ^ ".cmi") in
                 Typemod.save_signature modulename tsg outputprefix
                   sourcefile initial_env sg)))))
        let implementation ppf sourcefile outputprefix =
          Compmisc.init_path false;
          (let modulename =
             Compenv.module_of_filename ppf sourcefile outputprefix in
           Env.set_unit_name modulename;
           (let env = Compmisc.initial_env () in
            try
              let (typedtree,coercion,finalenv,current_signature) =
                (((((Pparse.parse_implementation ~tool_name ppf sourcefile)
                      |>
                      (print_if ppf Clflags.dump_parsetree
                         Printast.implementation))
                     |> (!Ppx_entry.rewrite_implementation))
                    |> (print_if ppf Clflags.dump_source Pprintast.structure))
                   |>
                   (Typemod.type_implementation_more sourcefile outputprefix
                      modulename env))
                  |>
                  (print_if ppf Clflags.dump_typedtree
                     (fun fmt  ->
                        fun (ty,co,_,_)  ->
                          Printtyped.implementation_with_coercion fmt
                            (ty, co))) in
              if !Clflags.print_types
              then Warnings.check_fatal ()
              else
                (((typedtree, coercion) |>
                    (Translmod.transl_implementation modulename))
                   |>
                   (print_if ppf Clflags.dump_rawlambda Printlambda.lambda))
                  |>
                  ((fun lambda  ->
                      match Lam_compile_group.lambda_as_module finalenv
                              current_signature sourcefile lambda
                      with
                      | e -> e
                      | exception e ->
                          let file = "bsc.dump" in
                          (Ext_pervasives.with_file_as_chan file
                             (fun ch  ->
                                (output_string ch) @@
                                  (Printexc.raw_backtrace_to_string
                                     (Printexc.get_raw_backtrace ())));
                           Ext_log.err __LOC__
                             "Compilation fatal error, stacktrace saved into %s"
                             file;
                           raise e)));
              Stypes.dump (Some (outputprefix ^ ".annot"))
            with
            | x -> (Stypes.dump (Some (outputprefix ^ ".annot")); raise x)))
      end 
    module Js_main =
      struct
        open Config
        open Clflags
        open Compenv
        let process_interface_file ppf name =
          Js_implementation.interface ppf name (output_prefix name)
        let process_implementation_file ppf name =
          let opref = output_prefix name in
          Js_implementation.implementation ppf name opref;
          objfiles := ((opref ^ ".cmo") :: (!objfiles))
        let process_file ppf name =
          if
            (Filename.check_suffix name ".ml") ||
              (Filename.check_suffix name ".mlt")
          then
            let opref = output_prefix name in
            (Js_implementation.implementation ppf name opref;
             objfiles := ((opref ^ ".cmo") :: (!objfiles)))
          else
            if Filename.check_suffix name (!Config.interface_suffix)
            then
              (let opref = output_prefix name in
               Compile.interface ppf name opref;
               if !make_package
               then objfiles := ((opref ^ ".cmi") :: (!objfiles)))
            else raise (Arg.Bad ("don't know what to do with " ^ name))
        let usage = "Usage: ocamlc <options> <files>\nOptions are:"
        let ppf = Format.err_formatter
        let anonymous filename =
          readenv ppf Before_compile; process_file ppf filename
        let impl filename =
          readenv ppf Before_compile;
          process_implementation_file ppf filename
        let intf filename =
          readenv ppf Before_compile; process_interface_file ppf filename
        let show_config () = Config.print_config stdout; exit 0
        module Options =
          Main_args.Make_bytecomp_options(struct
                                            let set r () = r := true
                                            let unset r () = r := false
                                            let _a = set make_archive
                                            let _absname =
                                              set Location.absname
                                            let _annot = set annotations
                                            let _binannot =
                                              set binary_annotations
                                            let _c = set compile_only
                                            let _cc s =
                                              c_compiler := (Some s)
                                            let _cclib s =
                                              ccobjs :=
                                                ((Misc.rev_split_words s) @
                                                   (!ccobjs))
                                            let _ccopt s =
                                              first_ccopts := (s ::
                                                (!first_ccopts))
                                            let _compat_32 =
                                              set bytecode_compatible_32
                                            let _config = show_config
                                            let _custom = set custom_runtime
                                            let _no_check_prims =
                                              set no_check_prims
                                            let _dllib s =
                                              dllibs :=
                                                ((Misc.rev_split_words s) @
                                                   (!dllibs))
                                            let _dllpath s =
                                              dllpaths := ((!dllpaths) @ [s])
                                            let _for_pack s =
                                              for_package := (Some s)
                                            let _g = set debug
                                            let _i () =
                                              print_types := true;
                                              compile_only := true
                                            let _I s =
                                              include_dirs := (s ::
                                                (!include_dirs))
                                            let _impl = impl
                                            let _intf = intf
                                            let _intf_suffix s =
                                              Config.interface_suffix := s
                                            let _keep_docs = set keep_docs
                                            let _keep_locs = set keep_locs
                                            let _labels = unset classic
                                            let _linkall =
                                              set link_everything
                                            let _make_runtime () =
                                              custom_runtime := true;
                                              make_runtime := true;
                                              link_everything := true
                                            let _no_alias_deps =
                                              set transparent_modules
                                            let _no_app_funct =
                                              unset applicative_functors
                                            let _noassert = set noassert
                                            let _nolabels = set classic
                                            let _noautolink =
                                              set no_auto_link
                                            let _nostdlib =
                                              set no_std_include
                                            let _o s =
                                              output_name := (Some s)
                                            let _open s =
                                              open_modules := (s ::
                                                (!open_modules))
                                            let _output_obj () =
                                              output_c_object := true;
                                              custom_runtime := true
                                            let _output_complete_obj () =
                                              output_c_object := true;
                                              output_complete_object := true;
                                              custom_runtime := true
                                            let _pack = set make_package
                                            let _pp s =
                                              preprocessor := (Some s)
                                            let _ppx s =
                                              first_ppx := (s ::
                                                (!first_ppx))
                                            let _principal = set principal
                                            let _rectypes =
                                              set recursive_types
                                            let _runtime_variant s =
                                              runtime_variant := s
                                            let _safe_string =
                                              unset unsafe_string
                                            let _short_paths =
                                              unset real_paths
                                            let _strict_sequence =
                                              set strict_sequence
                                            let _strict_formats =
                                              set strict_formats
                                            let _thread = set use_threads
                                            let _vmthread = set use_vmthreads
                                            let _unsafe = set fast
                                            let _unsafe_string =
                                              set unsafe_string
                                            let _use_prims s = use_prims := s
                                            let _use_runtime s =
                                              use_runtime := s
                                            let _v () =
                                              print_version_and_library
                                                "compiler"
                                            let _version =
                                              print_version_string
                                            let _vnum = print_version_string
                                            let _w =
                                              Warnings.parse_options false
                                            let _warn_error =
                                              Warnings.parse_options true
                                            let _warn_help =
                                              Warnings.help_warnings
                                            let _where =
                                              print_standard_library
                                            let _verbose = set verbose
                                            let _nopervasives =
                                              set nopervasives
                                            let _dsource = set dump_source
                                            let _dparsetree =
                                              set dump_parsetree
                                            let _dtypedtree =
                                              set dump_typedtree
                                            let _drawlambda =
                                              set dump_rawlambda
                                            let _dlambda = set dump_lambda
                                            let _dinstr = set dump_instr
                                            let anonymous = anonymous
                                          end)
        let buckle_script_flags =
          ("-js-module", (Arg.String Js_config.cmd_set_module),
            " set module system: commonjs (default), amdjs, google:package_name")
          ::
          ("-js-gen-tds", (Arg.Set Js_config.default_gen_tds),
            " set will generate `.d.ts` file for typescript (experimental)")
          :: Options.list
        let () = Clflags.unsafe_string := false
        let main () =
          try
            readenv ppf Before_args;
            Arg.parse buckle_script_flags anonymous usage;
            readenv ppf Before_link;
            if
              (List.length
                 (List.filter (fun x  -> !x)
                    [make_archive;
                    make_package;
                    compile_only;
                    output_c_object]))
                > 1
            then
              (if !print_types
               then
                 fatal
                   "Option -i is incompatible with -pack, -a, -output-obj"
               else
                 fatal
                   "Please specify at most one of -pack, -a, -c, -output-obj");
            if !make_archive
            then
              (Compmisc.init_path false;
               Bytelibrarian.create_archive ppf (Compenv.get_objfiles ())
                 (extract_output (!output_name));
               Warnings.check_fatal ())
            else
              if !make_package
              then ()
              else if (not (!compile_only)) && ((!objfiles) <> []) then ();
            exit 0
          with | x -> (Location.report_exception ppf x; exit 2)
        let _ = main ()
      end
    module Lam_mk :
      sig
        [@@@ocaml.text
          " A module prepared for smart constructors of {!Lambda.lambda}"]
        val lfunction :
          Lambda.function_kind ->
            Ident.t list -> Lambda.lambda -> Lambda.lambda
      end =
      struct
        let lfunction kind params (body : Lambda.lambda) =
          if params = []
          then body
          else
            (match body with
             | Lfunction (kind',params',body') when kind = kind' ->
                 Lfunction (kind', (params @ params'), body')
             | _ -> Lfunction (kind, params, body))
      end 
    module Ext_array :
      sig
        [@@@ocaml.text " Some utilities for {!Array} operations "]
        val reverse_in_place : 'a array -> unit
        val filter : ('a -> bool) -> 'a array -> 'a array
        val filter_map : ('a -> 'b option) -> 'a array -> 'b array
        val range : int -> int -> int array
        val map2i :
          (int -> 'a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
      end =
      struct
        let reverse_in_place a =
          let aux a i len =
            if len = 0
            then ()
            else
              for k = 0 to (len - 1) / 2 do
                (let t = Array.unsafe_get a (i + k) in
                 Array.unsafe_set a (i + k)
                   (Array.unsafe_get a (((i + len) - 1) - k));
                 Array.unsafe_set a (((i + len) - 1) - k) t)
              done in
          aux a 0 (Array.length a)
        let reverse_of_list =
          function
          | [] -> [||]
          | hd::tl as l ->
              let len = List.length l in
              let a = Array.make len hd in
              let rec fill i =
                function
                | [] -> a
                | hd::tl ->
                    (Array.unsafe_set a ((len - i) - 2) hd; fill (i + 1) tl) in
              fill 0 tl
        let filter f a =
          let arr_len = Array.length a in
          let rec aux acc i =
            if i = arr_len
            then reverse_of_list acc
            else
              (let v = Array.unsafe_get a i in
               if f v then aux (v :: acc) (i + 1) else aux acc (i + 1)) in
          aux [] 0
        let filter_map (f : _ -> _ option) a =
          let arr_len = Array.length a in
          let rec aux acc i =
            if i = arr_len
            then reverse_of_list acc
            else
              (let v = Array.unsafe_get a i in
               match f v with
               | Some v -> aux (v :: acc) (i + 1)
               | None  -> aux acc (i + 1)) in
          aux [] 0
        let range from to_ =
          if from > to_
          then invalid_arg "Ext_array.range"
          else Array.init ((to_ - from) + 1) (fun i  -> i + from)
        let map2i f a b =
          let len = Array.length a in
          if len <> (Array.length b)
          then invalid_arg "Ext_array.map2i"
          else
            Array.mapi (fun i  -> fun a  -> f i a (Array.unsafe_get b i)) a
      end 
    module Js_pass_beta : sig [@@@ocaml.text " "] end =
      struct
        module S = Js_stmt_make
        module E = Js_exp_make
        [@@@ocaml.text
          " Update ident info use cases, it is a non pure function, \n    it will annotate [program] with some meta data\n    TODO: Ident Hashtbl could be improved, \n    since in this case it can not be global?  \n\n "]
        type inline_state =
          | False
          | Inline_ignore of bool
          | Inline_ret of J.expression* bool
          | Inline_return
        let pass_beta =
          object (self)
            inherit  Js_map.map as super
            val inline_state = False
            method with_inline_state x = {<inline_state = x>}
            method! block bs =
              match bs with
              | { statement_desc = Block bs;_}::rest ->
                  self#block (bs @ rest)
              | {
                  statement_desc = Exp
                    {
                      expression_desc = Call
                        ({ expression_desc = Fun (params,body,env) },args,_info);_};_}::rest
                  when Ext_list.same_length args params ->
                  let body = self#block body in
                  (List.fold_right2
                     (fun p  ->
                        fun a  ->
                          fun acc  -> (S.define ~kind:Variable p a) :: acc)
                     params args
                     ((self#with_inline_state
                         (Inline_ignore (Js_fun_env.is_tailcalled env)))#block
                        body))
                    @ (self#block rest)
              | {
                  statement_desc = Exp
                    {
                      expression_desc = Bin
                        (Eq
                         ,e,{
                              expression_desc = Call
                                ({ expression_desc = Fun (params,body,env) },args,_info);_});_};_}::rest
                  when Ext_list.same_length args params ->
                  let body = self#block body in
                  (List.fold_right2
                     (fun p  ->
                        fun a  ->
                          fun acc  -> (S.define ~kind:Variable p a) :: acc)
                     params args
                     ((self#with_inline_state
                         (Inline_ret (e, (Js_fun_env.is_tailcalled env))))#block
                        body))
                    @ (self#block rest)
              | {
                  statement_desc = Return
                    {
                      return_value =
                        {
                          expression_desc = Call
                            ({ expression_desc = Fun (params,body,_) },args,_info);_}
                      };_}::rest
                  when Ext_list.same_length args params ->
                  let body = self#block body in
                  (List.fold_right2
                     (fun p  ->
                        fun a  ->
                          fun acc  -> (S.define ~kind:Variable p a) :: acc)
                     params args
                     ((self#with_inline_state Inline_return)#block body))
                    @ (self#block rest)
              | ({ statement_desc = Return { return_value = e } } as st)::rest
                  ->
                  (match inline_state with
                   | False  -> (self#statement st) :: (self#block rest)
                   | Inline_ignore b -> (S.exp (self#expression e)) ::
                       (if b
                        then (S.break ()) :: (self#block rest)
                        else self#block rest)
                   | Inline_ret (v,b) ->
                       (S.exp (E.assign v (self#expression e))) ::
                       (if b
                        then (S.break ()) :: (self#block rest)
                        else self#block rest)
                   | Inline_return  -> (S.return (self#expression e)) ::
                       (self#block rest))
              | x::xs -> (self#statement x) :: (self#block xs)
              | [] -> []
            method! expression e =
              match e.expression_desc with
              | Fun (params,body,env) ->
                  {
                    e with
                    expression_desc =
                      (Fun
                         (params, (({<inline_state = False>})#block body),
                           env))
                  }
              | _ -> super#expression e
          end
      end 
    module Ext_char :
      sig
        [@@@ocaml.text
          " Extension to Standard char module, avoid locale sensitivity "]
        val escaped : char -> string
      end =
      struct
        external string_unsafe_set :
          string -> int -> char -> unit = "%string_unsafe_set"
        external string_create : int -> string = "caml_create_string"
        external unsafe_chr : int -> char = "%identity"
        let escaped =
          function
          | '\'' -> "\\'"
          | '\\' -> "\\\\"
          | '\n' -> "\\n"
          | '\t' -> "\\t"
          | '\r' -> "\\r"
          | '\b' -> "\\b"
          | ' '..'~' as c ->
              let s = string_create 1 in (string_unsafe_set s 0 c; s)
          | c ->
              let n = Char.code c in
              let s = string_create 4 in
              (string_unsafe_set s 0 '\\';
               string_unsafe_set s 1 (unsafe_chr (48 + (n / 100)));
               string_unsafe_set s 2 (unsafe_chr (48 + ((n / 10) mod 10)));
               string_unsafe_set s 3 (unsafe_chr (48 + (n mod 10)));
               s)[@@ocaml.doc
                   " {!Char.escaped} is locale sensitive in 4.02.3, fixed in the trunk,\n    backport it here\n "]
      end 
  end