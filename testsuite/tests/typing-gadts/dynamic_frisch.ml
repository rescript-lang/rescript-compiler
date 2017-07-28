(* Encoding generics using GADTs *)
(* (c) Alain Frisch / Lexifi *)
(* cf. http://www.lexifi.com/blog/dynamic-types *)

(* Basic tag *)

type 'a ty =
  | Int: int ty
  | String: string ty
  | List: 'a ty -> 'a list ty
  | Pair: ('a ty * 'b ty) -> ('a * 'b) ty
;;

(* Tagging data *)

type variant =
  | VInt of int
  | VString of string
  | VList of variant list
  | VPair of variant * variant

let rec variantize: type t. t ty -> t -> variant =
  fun ty x ->
    (* type t is abstract here *)
    match ty with
    | Int -> VInt x  (* in this branch: t = int *)
    | String -> VString x (* t = string *)
    | List ty1 ->
        VList (List.map (variantize ty1) x)
        (* t = 'a list for some 'a *)
    | Pair (ty1, ty2) ->
        VPair (variantize ty1 (fst x), variantize ty2 (snd x))
        (* t = ('a, 'b) for some 'a and 'b *)

exception VariantMismatch

let rec devariantize: type t. t ty -> variant -> t =
  fun ty v ->
    match ty, v with
    | Int, VInt x -> x
    | String, VString x -> x
    | List ty1, VList vl ->
        List.map (devariantize ty1) vl
    | Pair (ty1, ty2), VPair (x1, x2) ->
        (devariantize ty1 x1, devariantize ty2 x2)
    | _ -> raise VariantMismatch
;;

(* Handling records *)

type 'a ty =
  | Int: int ty
  | String: string ty
  | List: 'a ty -> 'a list ty
  | Pair: ('a ty * 'b ty) -> ('a * 'b) ty
  | Record: 'a record -> 'a ty

and 'a record =
    {
     path: string;
     fields: 'a field_ list;
    }

and 'a field_ =
  | Field: ('a, 'b) field -> 'a field_

and ('a, 'b) field =
    {
     label: string;
     field_type: 'b ty;
     get: ('a -> 'b);
    }
;;

(* Again *)

type variant =
  | VInt of int
  | VString of string
  | VList of variant list
  | VPair of variant * variant
  | VRecord of (string * variant) list

let rec variantize: type t. t ty -> t -> variant =
  fun ty x ->
    (* type t is abstract here *)
    match ty with
    | Int -> VInt x  (* in this branch: t = int *)
    | String -> VString x (* t = string *)
    | List ty1 ->
        VList (List.map (variantize ty1) x)
        (* t = 'a list for some 'a *)
    | Pair (ty1, ty2) ->
        VPair (variantize ty1 (fst x), variantize ty2 (snd x))
        (* t = ('a, 'b) for some 'a and 'b *)
    | Record {fields} ->
        VRecord
          (List.map (fun (Field{field_type; label; get}) ->
                       (label, variantize field_type (get x))) fields)
;;

(* Extraction *)

type 'a ty =
  | Int: int ty
  | String: string ty
  | List: 'a ty -> 'a list ty
  | Pair: ('a ty * 'b ty) -> ('a * 'b) ty
  | Record: ('a, 'builder) record -> 'a ty

and ('a, 'builder) record =
    {
     path: string;
     fields: ('a, 'builder) field list;
     create_builder: (unit -> 'builder);
     of_builder: ('builder -> 'a);
    }

and ('a, 'builder) field =
  | Field: ('a, 'builder, 'b) field_ -> ('a, 'builder) field

and ('a, 'builder, 'b) field_ =
  {
   label: string;
   field_type: 'b ty;
   get: ('a -> 'b);
   set: ('builder -> 'b -> unit);
  }

let rec devariantize: type t. t ty -> variant -> t =
  fun ty v ->
    match ty, v with
    | Int, VInt x -> x
    | String, VString x -> x
    | List ty1, VList vl ->
        List.map (devariantize ty1) vl
    | Pair (ty1, ty2), VPair (x1, x2) ->
        (devariantize ty1 x1, devariantize ty2 x2)
    | Record {fields; create_builder; of_builder}, VRecord fl ->
        if List.length fields <> List.length fl then raise VariantMismatch;
        let builder = create_builder () in
        List.iter2
          (fun (Field {label; field_type; set}) (lab, v) ->
            if label <> lab then raise VariantMismatch;
            set builder (devariantize field_type v)
          )
          fields fl;
        of_builder builder
    | _ -> raise VariantMismatch
;;

type my_record  =
    {
     a: int;
     b: string list;
    }

let my_record =
  let fields =
    [
     Field {label = "a"; field_type = Int;
            get = (fun {a} -> a);
            set = (fun (r, _) x -> r := Some x)};
     Field {label = "b"; field_type = List String;
            get = (fun {b} -> b);
            set = (fun (_, r) x -> r := Some x)};
    ]
  in
  let create_builder () = (ref None, ref None) in
  let of_builder (a, b) =
    match !a, !b with
    | Some a, Some b -> {a; b}
    | _ -> failwith "Some fields are missing in record of type my_record"
  in
  Record {path = "My_module.my_record"; fields; create_builder; of_builder}
;;

(* Extension to recursive types and polymorphic variants *)
(* by Jacques Garrigue *)

type noarg = Noarg

type (_,_) ty =
  | Int: (int,_) ty
  | String: (string,_) ty
  | List: ('a,'e) ty -> ('a list, 'e) ty
  | Option: ('a,'e) ty -> ('a option, 'e) ty
  | Pair: (('a,'e) ty * ('b,'e) ty) -> ('a * 'b,'e) ty
  (* Support for type variables and recursive types *)
  | Var: ('a, 'a -> 'e) ty
  | Rec: ('a, 'a -> 'e) ty -> ('a,'e) ty
  | Pop: ('a, 'e) ty -> ('a, 'b -> 'e) ty
  (* Change the representation of a type *)
  | Conv: string * ('a -> 'b) * ('b -> 'a) * ('b, 'e) ty -> ('a, 'e) ty
  (* Sum types (both normal sums and polymorphic variants) *)
  | Sum: ('a, 'e, 'b) ty_sum -> ('a, 'e) ty

and ('a, 'e, 'b) ty_sum =
    { sum_proj: 'a -> string * 'e ty_dyn option;
      sum_cases: (string * ('e,'b) ty_case) list;
      sum_inj: 'c. ('b,'c) ty_sel * 'c -> 'a; }

and 'e ty_dyn =              (* dynamic type *)
  | Tdyn : ('a,'e) ty * 'a -> 'e ty_dyn

and (_,_) ty_sel =           (* selector from a list of types *)
  | Thd : ('a -> 'b, 'a) ty_sel
  | Ttl : ('b -> 'c, 'd) ty_sel -> ('a -> 'b -> 'c, 'd) ty_sel

and (_,_) ty_case =          (* type a sum case *)
  | TCarg : ('b,'a) ty_sel * ('a,'e) ty -> ('e,'b) ty_case
  | TCnoarg : ('b,noarg) ty_sel -> ('e,'b) ty_case
;;

type _ ty_env =              (* type variable substitution *)
  | Enil : unit ty_env
  | Econs : ('a,'e) ty * 'e ty_env -> ('a -> 'e) ty_env
;;

(* Comparing selectors *)
type (_,_) eq = Eq: ('a,'a) eq

let rec eq_sel : type a b c. (a,b) ty_sel -> (a,c) ty_sel -> (b,c) eq option =
  fun s1 s2 ->
    match s1, s2 with
    | Thd, Thd -> Some Eq
    | Ttl s1, Ttl s2 ->
        (match eq_sel s1 s2 with None -> None | Some Eq -> Some Eq)
    | _ -> None

(* Auxiliary function to get the type of a case from its selector *)
let rec get_case : type a b e.
  (b, a) ty_sel -> (string * (e,b) ty_case) list -> string * (a, e) ty option =
  fun sel cases ->
  match cases with
  | (name, TCnoarg sel') :: rem ->
      begin match eq_sel sel sel' with
      | None -> get_case sel rem
      | Some Eq -> name, None
      end
  | (name, TCarg (sel', ty)) :: rem ->
      begin match eq_sel sel sel' with
      | None -> get_case sel rem
      | Some Eq -> name, Some ty
      end
  | [] -> raise Not_found
;;

(* Untyped representation of values *)
type variant =
  | VInt of int
  | VString of string
  | VList of variant list
  | VOption of variant option
  | VPair of variant * variant
  | VConv of string * variant
  | VSum of string * variant option

let may_map f = function Some x -> Some (f x) | None -> None

let rec variantize : type a e. e ty_env -> (a,e) ty -> a -> variant =
  fun e ty v ->
  match ty with
  | Int -> VInt v
  | String -> VString v
  | List t -> VList (List.map (variantize e t) v)
  | Option t -> VOption (may_map (variantize e t) v)
  | Pair (t1, t2) -> VPair (variantize e t1 (fst v), variantize e t2 (snd v))
  | Rec t -> variantize (Econs (ty, e)) t v
  | Pop t -> (match e with Econs (_, e') -> variantize e' t v)
  | Var -> (match e with Econs (t, e') -> variantize e' t v)
  | Conv (s, proj, inj, t) -> VConv (s, variantize e t (proj v))
  | Sum ops ->
      let tag, arg = ops.sum_proj v in
      VSum (tag, may_map (function Tdyn (ty,arg) -> variantize e ty arg) arg)
;;

let rec devariantize : type t e. e ty_env -> (t, e) ty -> variant -> t =
  fun e ty v ->
  match ty, v with
  | Int, VInt x -> x
  | String, VString x -> x
  | List ty1, VList vl ->
      List.map (devariantize e ty1) vl
  | Pair (ty1, ty2), VPair (x1, x2) ->
      (devariantize e ty1 x1, devariantize e ty2 x2)
  | Rec t, _ -> devariantize (Econs (ty, e)) t v
  | Pop t, _ -> (match e with Econs (_, e') -> devariantize e' t v)
  | Var, _ -> (match e with Econs (t, e') -> devariantize e' t v)
  | Conv (s, proj, inj, t), VConv (s', v) when s = s' ->
      inj (devariantize e t v)
  | Sum ops, VSum (tag, a) ->
      begin try match List.assoc tag ops.sum_cases, a with
      | TCarg (sel, t), Some a -> ops.sum_inj (sel, devariantize e t a)
      | TCnoarg sel, None -> ops.sum_inj (sel, Noarg)
      | _ -> raise VariantMismatch
      with Not_found -> raise VariantMismatch
      end
  | _ -> raise VariantMismatch
;;

(* First attempt: represent 1-constructor variants using Conv *)
let wrap_A t = Conv ("`A", (fun (`A x) -> x), (fun x -> `A x), t);;

let ty a = Rec (wrap_A (Option (Pair (a, Var)))) ;;
let v = variantize Enil (ty Int);;
let x = v (`A (Some (1, `A (Some (2, `A None))))) ;;

(* Can also use it to decompose a tuple *)

let triple t1 t2 t3 =
  Conv ("Triple", (fun (a,b,c) -> (a,(b,c))),
        (fun (a,(b,c)) -> (a,b,c)), Pair (t1, Pair (t2, t3)))

let v = variantize Enil (triple String Int Int) ("A", 2, 3) ;;

(* Second attempt: introduce a real sum construct *)
let ty_abc =
  (* Could also use [get_case] for proj, but direct definition is shorter *)
  let proj = function
      `A n -> "A", Some (Tdyn (Int, n))
    | `B s -> "B", Some (Tdyn (String, s))
    | `C   -> "C", None
  (* Define inj in advance to be able to write the type annotation easily *)
  and inj : type c. (int -> string -> noarg -> unit, c) ty_sel * c ->
    [`A of int | `B of string | `C] = function
        Thd, v -> `A v
      | Ttl Thd, v -> `B v
      | Ttl (Ttl Thd), Noarg -> `C
  in
  (* Coherence of sum_inj and sum_cases is checked by the typing *)
  Sum { sum_proj = proj; sum_inj = inj; sum_cases =
        [ "A", TCarg (Thd, Int); "B", TCarg (Ttl Thd, String);
          "C", TCnoarg (Ttl (Ttl Thd)) ] }
;;

let v = variantize Enil ty_abc (`A 3)
let a = devariantize Enil ty_abc v

(* And an example with recursion... *)
type 'a vlist = [`Nil | `Cons of 'a * 'a vlist]

let ty_list : type a e. (a, e) ty -> (a vlist, e) ty = fun t ->
  let tcons = Pair (Pop t, Var) in
  Rec (Sum {
       sum_proj = (function
           `Nil -> "Nil", None
         | `Cons p -> "Cons", Some (Tdyn (tcons, p)));
       sum_cases = ["Nil", TCnoarg Thd; "Cons", TCarg (Ttl Thd, tcons)];
       sum_inj = fun (type c) ->
         (function
         | Thd, Noarg -> `Nil
         | Ttl Thd, v -> `Cons v
         : (noarg -> a * a vlist -> unit, c) ty_sel * c -> a vlist)
         (* One can also write the type annotation directly *)
     })

let v = variantize Enil (ty_list Int) (`Cons (1, `Cons (2, `Nil))) ;;


(* Simpler but weaker approach *)

type (_,_) ty =
  | Int: (int,_) ty
  | String: (string,_) ty
  | List: ('a,'e) ty -> ('a list, 'e) ty
  | Option: ('a,'e) ty -> ('a option, 'e) ty
  | Pair: (('a,'e) ty * ('b,'e) ty) -> ('a * 'b,'e) ty
  | Var: ('a, 'a -> 'e) ty
  | Rec: ('a, 'a -> 'e) ty -> ('a,'e) ty
  | Pop: ('a, 'e) ty -> ('a, 'b -> 'e) ty
  | Conv: string * ('a -> 'b) * ('b -> 'a) * ('b, 'e) ty -> ('a, 'e) ty
  | Sum: ('a -> string * 'e ty_dyn option) * (string * 'e ty_dyn option -> 'a)
             -> ('a, 'e) ty
and 'e ty_dyn =
  | Tdyn : ('a,'e) ty * 'a -> 'e ty_dyn

let ty_abc : ([`A of int | `B of string | `C],'e) ty =
  (* Could also use [get_case] for proj, but direct definition is shorter *)
  Sum (
  (function
      `A n -> "A", Some (Tdyn (Int, n))
    | `B s -> "B", Some (Tdyn (String, s))
    | `C   -> "C", None),
  (function
      "A", Some (Tdyn (Int, n)) -> `A n
    | "B", Some (Tdyn (String, s)) -> `B s
    | "C", None -> `C
    | _ -> invalid_arg "ty_abc"))
;;

(* Breaks: no way to pattern-match on a full recursive type *)
let ty_list : type a e. (a,e) ty -> (a vlist,e) ty = fun t ->
  let targ = Pair (Pop t, Var) in
  Rec (Sum (
  (function `Nil -> "Nil", None
    | `Cons p -> "Cons", Some (Tdyn (targ, p))),
  (function "Nil", None -> `Nil
    | "Cons", Some (Tdyn (Pair (_, Var), (p : a * a vlist))) -> `Cons p)))
;;

(* Define Sum using object instead of record for first-class polymorphism *)

type (_,_) ty =
  | Int: (int,_) ty
  | String: (string,_) ty
  | List: ('a,'e) ty -> ('a list, 'e) ty
  | Option: ('a,'e) ty -> ('a option, 'e) ty
  | Pair: (('a,'e) ty * ('b,'e) ty) -> ('a * 'b,'e) ty
  | Var: ('a, 'a -> 'e) ty
  | Rec: ('a, 'a -> 'e) ty -> ('a,'e) ty
  | Pop: ('a, 'e) ty -> ('a, 'b -> 'e) ty
  | Conv: string * ('a -> 'b) * ('b -> 'a) * ('b, 'e) ty -> ('a, 'e) ty
  | Sum: < proj: 'a -> string * 'e ty_dyn option;
           cases: (string * ('e,'b) ty_case) list;
           inj: 'c. ('b,'c) ty_sel * 'c -> 'a >
          -> ('a, 'e) ty

and 'e ty_dyn =
  | Tdyn : ('a,'e) ty * 'a -> 'e ty_dyn

and (_,_) ty_sel =
  | Thd : ('a -> 'b, 'a) ty_sel
  | Ttl : ('b -> 'c, 'd) ty_sel -> ('a -> 'b -> 'c, 'd) ty_sel

and (_,_) ty_case =
  | TCarg : ('b,'a) ty_sel * ('a,'e) ty -> ('e,'b) ty_case
  | TCnoarg : ('b,noarg) ty_sel -> ('e,'b) ty_case
;;

let ty_abc : ([`A of int | `B of string | `C] as 'a, 'e) ty =
  Sum (object
    method proj = function
        `A n -> "A", Some (Tdyn (Int, n))
      | `B s -> "B", Some (Tdyn (String, s))
      | `C -> "C", None
    method cases =
      [ "A", TCarg (Thd, Int); "B", TCarg (Ttl Thd, String);
        "C", TCnoarg (Ttl (Ttl Thd)) ];
    method inj : type c.
        (int -> string -> noarg -> unit, c) ty_sel * c ->
          [`A of int | `B of string | `C] =
      function
        Thd, v -> `A v
      | Ttl Thd, v -> `B v
      | Ttl (Ttl Thd), Noarg -> `C
      | _ -> assert false
  end)

type 'a vlist = [`Nil | `Cons of 'a * 'a vlist]

let ty_list : type a e. (a, e) ty -> (a vlist, e) ty = fun t ->
  let tcons = Pair (Pop t, Var) in
  Rec (Sum (object
    method proj = function
        `Nil -> "Nil", None
      | `Cons p -> "Cons", Some (Tdyn (tcons, p))
    method cases = ["Nil", TCnoarg Thd; "Cons", TCarg (Ttl Thd, tcons)]
    method inj : type c.(noarg -> a * a vlist -> unit, c) ty_sel * c -> a vlist
    = function
      | Thd, Noarg -> `Nil
      | Ttl Thd, v -> `Cons v
  end))
;;

(*
type (_,_) ty_assoc =
  | Anil : (unit,'e) ty_assoc
  | Acons : string * ('a,'e) ty * ('b,'e) ty_assoc -> ('a -> 'b, 'e) ty_assoc

and (_,_) ty_pvar =
  | Pnil : ('a,'e) ty_pvar
  | Pconst : 't * ('b,'e) ty_pvar -> ('t -> 'b, 'e) ty_pvar
  | Parg : 't * ('a,'e) ty * ('b,'e) ty_pvar -> ('t * 'a -> 'b, 'e) ty_pvar
*)
