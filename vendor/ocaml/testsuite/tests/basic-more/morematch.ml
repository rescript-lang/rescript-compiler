(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(**************************************************************)
(*  This suite tests the pattern-matching compiler            *)
(*  it should just compile and run.                           *)
(*  While compiling the following messages are normal:        *)
(**************************************************************)

(*
File "morematch.ml", line 38, characters 10-93:
Warning: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
0
File "morematch.ml", line 376, characters 2-15:
Warning: this match case is unused.
File "morematch.ml", line 443, characters 2-7:
Warning: this match case is unused.
*)

let test msg f arg r =
  if f arg <> r then begin
    prerr_endline msg ;
    failwith "Malaise"
  end
;;

type t = A | B | C | D | E | F
  ;;

let f x = match x with
| A | B | C -> 1
| D | E -> 2
| F -> 3;;

test "un" f C 1 ;
test "un" f D 2 ;
test "un" f F 3 ; ()
;;

let g x = match x with
  1 -> 1
| 2 -> 2
| 3 -> 3
| 4 | 5 -> 4
| 6 -> 5
| 7 | 8 -> 6
| 9 -> 7
| _ -> assert false
;;

test "deux" g 5 4 ;
test "deux" g 6 5 ;
test "deux" g 9 7 ; ()
;;


let g x = match x with
  1 -> 1
| 2 -> 2
| 3 -> 3
| 4 | 5 -> 4
| 6 -> 5
| 7 | 8 -> 6
| 9 -> 7
| _ -> 8;;

test "trois" g 10 8
;;

let g x= match  x with
  1 -> 1
| 2 -> 2
| 3 -> 3
| 4 | 5 -> 4
| 6 -> 5
| 4|5|7 -> 100
| 7 | 8 -> 6
| 9 -> 7
| _ -> 8;;
test "quatre" g 4 4 ;
test "quatre" g 7 100 ; ()
;;

(*
File "morematch.ml", line 73, characters 2-5:
Warning U: this sub-pattern is unused.
File "morematch.ml", line 74, characters 2-3:
Warning U: this sub-pattern is unused.
*)

let h x =
 match x with
   (1,1) -> 1
| (2|3), 1 -> 2
| 2,(2|3) -> 3
| (4,4) -> 5
| _ -> 100
;;

test "cinq" h (2,2) 3 ;
test "cinq" h (2,1) 2 ;
test "cinq" h (2,4) 100 ; ()
;;

(* idem hh (2,5) *)

let hh x = match x with
| 1,1 -> 1
| 2,1 -> 2
| (2|3),(1|2|3|4) -> 3
| 2,5 -> 4
| (4,4) -> 5
| _ -> 100
;;

let hhh x = match x with
| 1,1 -> 1
| (2|3),1 -> 2
| 2,2 -> 3
| _ -> 100
;;

let h x =
 match x with
   (1,1) -> 1
| 3,1 -> 2
| 2,(2|3) -> 3
| (4,4) -> 5
| _ -> 100
;;

let h x = match x with
  1 -> 1
| 2|3 -> 2
| 4 -> 4
| 5 -> 5
| 6|7 -> 6
| 8 -> 8
| _ -> 100
;;
let f x = match x with
| ((1|2),(3|4))|((3|4),(1|2)) -> 1
| (3,(5|6)) -> 2
| _ -> 3
;;

test "six" f (1,3) 1 ;
test "six" f (3,2) 1 ;
test "six" f (3,5) 2 ;
test "six" f (3,7) 3 ; ()
;;

type tt = {a : bool list ; b : bool}

let f = function
  | {a=([]|[true])} -> 1
  | {a=false::_}|{b=(true|false)}    -> 2
;;

test "sept" f {a=[] ; b = true} 1 ;
test "sept" f {a=[true] ; b = false} 1 ;
test "sept" f {a=[false ; true] ; b = true} 2 ;
test "sept" f {a=[false] ; b = false} 2 ; ()
;;

let f = function
  | (([]|[true]),_) -> 1
  | (false::_,_)|(_,(true|false)) -> 2
;;

test "huit" f ([],true) 1 ;
test "huit" f ([true],false) 1 ;
test "huit" f ([false ; true], true) 2 ;
test "huit" f ([false], false) 2 ; ()
;;


let split_cases = function
   | `Nil | `Cons _ as x -> `A x
   | `Snoc _ as x -> `B x
;;

test "oubli" split_cases `Nil (`A `Nil);
test "oubli" split_cases (`Cons 1) (`A (`Cons 1));
test "oubli" split_cases (`Snoc 1) (`B (`Snoc 1)) ; ()
;;

type t1 = A of int | B of int
let f1 = function
  | (A x | B x) -> x
;;

test "neuf" f1 (A 1) 1 ;
test "neuf" f1 (B 1) 1 ;
;;

type coucou = A of int | B of int * int | C
;;


let g = function
  | (A x | B (_,x)) -> x
  | C -> 0
;;


test "dix" g (A 1) 1 ;
test "dix" g (B (1,2)) 2 ;
;;



let h = function
  | ([x]|[1 ; x ]|[1 ; 2 ; x]) -> x
  | _ -> 0
;;

test "encore" h [1] 1 ;
test "encore" h [1;2] 2 ;
test "encore" h [1;2;3] 3 ;
test "encore" h [0 ; 0] 0 ; ()
;;

let f = function
| (x,(0 as y)) | (y,x) -> y-x
;;

test "foo1" f (1,0) (-1);
test "foo1" f (1,2) (-1)
;;


let f = function (([]|[_]) as x)|(_::([] as x))|(_::_::x)  -> x
;;

test "zob" f [] [] ;
test "zob" f [1] [1] ;
test "zob" f [1;2;3] [3]
;;


type zob = A | B | C | D of zob * int | E of zob * zob

let rec f = function
  | (A | B | C) -> A
  | D (x,i) -> D (f x,i)
  | E (x,_) -> D (f x,0)
;;


test "fin" f B A ;
test "fin" f (D (C,1)) (D (A,1)) ;
test "fin" f (E (C,A)) (D (A,0)) ; ()
;;

type length =
    Char of int | Pixel of int | Percent of int | No of string | Default

let length = function
  | Char n -> n | Pixel n -> n
  | _       -> 0
;;

test "length" length (Char 10) 10 ;
test "length" length (Pixel 20) 20 ;
test "length" length Default 0 ;
test "length" length (Percent 100) 0 ; ()
;;

let length2 = function
  | Char n -> n | Percent n -> n
  | _       -> 0
;;

test "length2" length2 (Char 10) 10 ;
test "length2" length2 (Pixel 20) 0 ;
test "length2" length2 Default 0 ;
test "length2" length2(Percent 100) 100 ; ()
;;

let length3 = function
  | Char _ | No _ -> true
  | _ -> false
;;

test "length3" length3 (Char 10) true ;
test "length3" length3 (No "") true ;
test "length3" length3 (Pixel 20) false ;
test "length3" length3 Default false ;
test "length3" length3(Percent 100) false ; ()
;;

type hevea = A | B | C

let h x = match x with
| A -> 1
| B|C -> 2
;;

test "hevea" h A 1 ;
test "hevea" h B 2 ;
test "hevea" h B 2 ; ()
;;
type lambda =
    Lvar of int
  | Lconst of int
  | Lapply of lambda * lambda list
  | Lfunction of bool  * int list * lambda
  | Llet of  bool * int * lambda * lambda
  | Lletrec of (int * lambda) list * lambda
  | Lprim of string * lambda list
  | Lswitch of lambda * lambda_switch
  | Lstaticfail
  | Lcatch of lambda * lambda
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * int list) * lambda
  | Ltrywith of lambda * int * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of int * lambda * lambda * bool * lambda
  | Lassign of int * lambda
  | Lsend of lambda * lambda * lambda list
  | Levent of lambda * lambda_event
  | Lifused of int * lambda
and lambda_switch =
  { sw_numconsts: int;                  (* Number of integer cases *)
    sw_consts: (int * lambda) list;     (* Integer cases *)
    sw_numblocks: int;                  (* Number of tag block cases *)
    sw_blocks: (int * lambda) list;     (* Tag block cases *)
    sw_checked: bool ;                  (* True if bound checks needed *)
    sw_nofail: bool}                    (* True if should not fail *)
and lambda_event =
  { lev_loc: int;
    lev_kind: bool ;
    lev_repr: int ref option;
    lev_env: int list }

let rec approx_present v l = true

let rec lower_bind v arg lam = match lam with
| Lifthenelse (cond, ifso, ifnot) -> 1
| Lswitch (ls,({sw_consts=[i,act] ; sw_blocks = []} as _sw))
    when not (approx_present v ls) -> 2
| Lswitch (ls,({sw_consts=[] ; sw_blocks = [i,act]} as _sw))
    when not (approx_present v ls) -> 3
| Llet (true , vv, lv, l) -> 4
| _ -> 5
;;

test "lower_bind" (lower_bind 0 0) (Llet (true,0, Lvar 1, Lvar 2)) 4 ;
test "lower_bind" (lower_bind 0 0) (Lvar 0) 5 ;
test "lower_bind" (lower_bind 0 0) (Lifthenelse (Lvar 0, Lvar 1, Lvar 2)) 1
;;


type field_kind =
    Fvar of field_kind option ref
  | Fpresent
  | Fabsent

let unify_kind (k1, k2) =  match k1, k2 with
    (Fvar r, (Fvar _ | Fpresent))             -> 1
  | (Fpresent, Fvar r)                        -> 2
  | (Fpresent, Fpresent)                      -> 3
  | _                                         -> 4


let r = ref (Some Fpresent)
;;

test "unify"  unify_kind (Fvar r, Fpresent) 1 ;
test "unify"  unify_kind (Fvar r, Fvar r) 1 ;
test "unify"  unify_kind (Fvar r, Fabsent) 4 ;
test "unify"  unify_kind (Fpresent, Fvar r) 2 ;
test "unify"  unify_kind (Fpresent, Fpresent) 3 ;
test "unify"  unify_kind (Fabsent, Fpresent) 4 ; ()
;;


type youyou = A | B | C | D of youyou

let foo (k1, k2) = match k1,k2 with
| D _, (A|D _) -> 1
| (A|B),D _ -> 2
| C,_       -> 3
| _, (A|B|C) -> 4
;;

test "foo2" foo (D A,A) 1 ;
test "foo2" foo (D A,B) 4 ;
test "foo2" foo (A,A) 4 ; ()
;;

type yaya = A | B
;;

let yaya = function
| A,_,_ -> 1
| _,A,_ -> 2
| B,B,_ -> 3
| A,_,(100|103) -> 5
;;

test "yaya" yaya (A,A,0) 1 ;
test "yaya" yaya (B,A,0) 2 ;
test "yaya" yaya (B,B,100) 3 ; ()
;;

(*
let yoyo =  function
| [],_,_ -> 1
| _,[],_ -> 2
| _::_,_::_,_ -> 3
| [],_,(100|103|104) -> 5
| [],_,(100|103) -> 6
| [],_,(1000|1001|1002|20000) -> 7
;;

test "yoyo" yoyo ([],[],0) 1 ;
test "yoyo" yoyo ([1],[],0) 2 ;
test "yoyo" yoyo ([1],[1],100) 3 ; ()
;;

let youyou = function
  | (100|103|104) -> 1
  | (100|103|101) -> 2
  | (1000|1001|1002|20000) -> 3
  | _ -> -1
;;

test "youyou" youyou 100 1 ;
test "youyou" youyou 101 2 ;
test "youyou" youyou 1000 3
;;
*)
type autre =
  |  C | D | E of autre | F of autre * autre | H of autre | I | J | K of string

let rec autre = function
| C,_,_ -> 1
| _,C,_ -> 2
| D,D,_ -> 3
| (D|F (_,_)|H _|K _),_,_ -> 4
| (_, (D|I|E _|F (_, _)|H _|K _), _) -> 8
| (J,J,((C|D) as x |E x|F (_,x))) | (J,_,((C|J) as x)) -> autre (x,x,x)
| (J, J, (I|H _|K _)) -> 9
| I,_,_ -> 6
| E _,_,_ -> 7
;;
(*
File "morematch.ml", line 437, characters 43-44:
Warning U: this sub-pattern is unused.
*)
test "autre" autre (J,J,F (D,D)) 3 ;
test "autre" autre (J,J,D) 3 ;
test "autre" autre (J,J,I) 9 ;
test "autre" autre (H I,I,I) 4 ;
test "autre" autre (J,J,H I) 9 ; ()
;;


type youpi = YA | YB | YC
and hola = X | Y | Z | T of hola | U of hola | V of hola

let xyz = function
| YA,_,_ -> 1
| _,YA,_ -> 2
| YB,YB,_ -> 3
| ((YB|YC), (YB|YC), (X|Y|Z|V _|T _)) -> 6
| _,_,(X|U _) -> 8
| _,_,Y -> 5
;;
(*
File "morematch.ml", line 459, characters 7-8:
Warning U: this sub-pattern is unused.
File "morematch.ml", line 460, characters 2-7:
Warning U: this match case is unused.
*)
test "xyz" xyz (YC,YC,X) 6 ;
test "xyz" xyz (YC,YB,U X) 8 ;
test "xyz" xyz (YB,YC,X) 6 ; ()
;;


(* Ce test est pour le compilo lui-meme *)
let eq (x,y) = x=y
;;

test "eq" eq ("coucou", "coucou") true ; ()
;;

(* Test des gardes, non trivial *)

let is_none = function
  | None -> true
  | _ -> false

let garde x = match x with
| (Some _, _) when is_none (snd x) -> 1
| (Some (pc, _), Some pc') when pc = pc' -> 2
| _ -> 3
;;

test "garde" garde (Some (1,1),None) 1 ;
test "garde" garde (Some (1,1),Some 1) 2 ;
test "garde" garde (Some (2,1),Some 1) 3 ; ()
;;

let orstring = function
  | ("A"|"B"|"C") -> 2
  | "D" -> 3
  | _ -> 4
;;

test "orstring" orstring "A" 2 ;
test "orstring" orstring "B" 2 ;
test "orstring" orstring "C" 2 ;
test "orstring" orstring "D" 3 ;
test "orstring" orstring "E" 4 ; ()
;;

type var_t = [`Variant of [ `Some of string | `None | `Foo] ]

let crash (pat:var_t) =
      match pat with
      | `Variant (`Some tag) -> tag
      | `Variant (`None) -> "none"
      | _ -> "foo"

;;

test "crash" crash (`Variant `None) "none" ;
test "crash" crash (`Variant (`Some "coucou")) "coucou" ;
test "crash" crash (`Variant (`Foo)) "foo" ; ()
;;

let flatgarde c =
let x,y = c in
match x,y with
| (1,2)|(2,3) when y=2 -> 1
| (1,_)|(_,3) -> 2
| _ -> 3
;;

test "flatgarde" flatgarde (1,2) 1 ;
test "flatgarde" flatgarde (1,3) 2 ;
test "flatgarde" flatgarde (2,3) 2 ;
test "flatgarde" flatgarde (2,4) 3 ; ()
;;


(* Les bugs de jerome *)
type f =
  | ABSENT
  | FILE
  | SYMLINK
  | DIRECTORY

type r =
  | Unchanged
  | Deleted
  | Modified
  | PropsChanged
  | Created

let replicaContent2shortString rc =
    let (typ, status) = rc in
    match typ, status with
      _, Unchanged             -> "        "
    | ABSENT, Deleted         -> "deleted "
    | FILE, Created           -> "new file"
    | FILE, Modified          -> "changed "
    | FILE, PropsChanged      -> "props   "
    | SYMLINK, Created        -> "new link"
    | SYMLINK, Modified       -> "chgd lnk"
    | DIRECTORY, Created      -> "new dir "
    | DIRECTORY, Modified     -> "chgd dir"
    | DIRECTORY, PropsChanged -> "props   "
    (* Cases that can't happen... *)

    | ABSENT, (Created | Modified | PropsChanged)
    | SYMLINK, PropsChanged
    | (FILE|SYMLINK|DIRECTORY), Deleted
                                -> "assert false"
;;


test "jerome_constr"
   replicaContent2shortString (ABSENT, Unchanged) "        " ;
test "jerome_constr"
   replicaContent2shortString (ABSENT, Deleted) "deleted " ;
test "jerome_constr"
   replicaContent2shortString (FILE, Modified) "changed " ;
test "jerome_constr"
   replicaContent2shortString (DIRECTORY, PropsChanged) "props   " ;
test "jerome_constr"
   replicaContent2shortString (FILE, Deleted) "assert false" ;
test "jerome_constr"
   replicaContent2shortString (SYMLINK, Deleted) "assert false" ;
test "jerome_constr"
   replicaContent2shortString (SYMLINK, PropsChanged) "assert false" ;
test "jerome_constr"
   replicaContent2shortString (DIRECTORY, Deleted) "assert false" ;
test "jerome_constr"
   replicaContent2shortString (ABSENT, Created) "assert false" ;
test "jerome_constr"
   replicaContent2shortString (ABSENT, Modified) "assert false" ;
test "jerome_constr"
   replicaContent2shortString (ABSENT, PropsChanged) "assert false" ;
;;


let replicaContent2shortString rc =
    let (typ, status) = rc in
    match typ, status with
      _, `Unchanged             -> "        "
    | `ABSENT, `Deleted         -> "deleted "
    | `FILE, `Created           -> "new file"
    | `FILE, `Modified          -> "changed "
    | `FILE, `PropsChanged      -> "props   "
    | `SYMLINK, `Created        -> "new link"
    | `SYMLINK, `Modified       -> "chgd lnk"
    | `DIRECTORY, `Created      -> "new dir "
    | `DIRECTORY, `Modified     -> "chgd dir"
    | `DIRECTORY, `PropsChanged -> "props   "
    (* Cases that can't happen... *)

    | `ABSENT, (`Created | `Modified | `PropsChanged)
    | `SYMLINK, `PropsChanged
    | (`FILE|`SYMLINK|`DIRECTORY), `Deleted
                                -> "assert false"
;;


test "jerome_variant"
   replicaContent2shortString (`ABSENT, `Unchanged) "        " ;
test "jerome_variant"
   replicaContent2shortString (`ABSENT, `Deleted) "deleted " ;
test "jerome_variant"
   replicaContent2shortString (`FILE, `Modified) "changed " ;
test "jerome_variant"
   replicaContent2shortString (`DIRECTORY, `PropsChanged) "props   " ;
test "jerome_variant"
   replicaContent2shortString (`FILE, `Deleted) "assert false" ;
test "jerome_variant"
   replicaContent2shortString (`SYMLINK, `Deleted) "assert false" ;
test "jerome_variant"
   replicaContent2shortString (`SYMLINK, `PropsChanged) "assert false" ;
test "jerome_variant"
   replicaContent2shortString (`DIRECTORY, `Deleted) "assert false" ;
test "jerome_variant"
   replicaContent2shortString (`ABSENT, `Created) "assert false" ;
test "jerome_variant"
   replicaContent2shortString (`ABSENT, `Modified) "assert false" ;
test "jerome_variant"
   replicaContent2shortString (`ABSENT, `PropsChanged) "assert false" ;
;;

(* bug 319 *)

type ab = A of int | B of int
type cd = C | D

let ohl = function
  | (A (p) | B (p)), C -> p
  | (A (p) | B (p)), D -> p
;;

test "ohl" ohl (A 0,C) 0 ;
test "ohl" ohl (B 0,D) 0 ; ()
;;

(* bug 324 *)
type pottier =
  | A
  | B
;;

let pottier x =
  match x with
  | (( (A, 1) | (B, 2)),A) -> false
  | _ -> true
;;

test "pottier" pottier ((B,2),A) false ;
test "pottier" pottier ((B,2),B) true ;
test "pottier" pottier ((A,2),A) true ; ()
;;

(* bug 325 in bytecode compiler *)
let coquery q =  match q with
| y,0,([modu;defs]| [defs;modu;_]) -> y+defs-modu
| _ -> 0
;;

test "coquery" coquery (1,0,[1 ; 2 ; 3]) 0 ;
test "coquery" coquery (1,0,[1 ; 2]) 2 ; ()
;;

(*
  Two other variable in or-pat tests
*)
type vars = A of int | B of (int * int) | C
;;


let vars1 = function
  | (A x | B (_,x)) -> x
  | C -> 0
;;

test "vars1" vars1 (A 1) 1 ;
test "vars1" vars1 (B (1,2)) 2 ; ()
;;

let vars2 = function
  | ([x]|[1 ; x ]|[1 ; 2 ; x]) -> x
  | _ -> 0
;;

test"vars2" vars2 [1] 1 ;
test"vars2" vars2 [1;2] 2 ;
test"vars2" vars2 [1;2;3] 3 ;
test"vars2" vars2 [0 ; 0] 0 ; ()
;;

(* Bug 342 *)
type eber = {x:int; y: int; z:bool}

let eber = function
  | {x=a; z=true}
  | {y=a; z=false} -> a
;;

test "eber" eber {x=0 ; y=1 ; z=true} 0 ;
test "eber" eber {x=1 ; y=0 ; z=false} 0 ; ()
;;


(* Enchainement des test d'intervalle *)

let escaped = function
  | '"' | '\\' | '\n' | '\t' -> 2
  | c -> 1
;;

test "escaped" escaped '"' 2 ;
test "escaped" escaped '\\' 2 ;
test "escaped" escaped '\n' 2 ;
test "escaped" escaped '\t' 2 ;
test "escaped" escaped '\000' 1 ;
test "escaped" escaped ' ' 1 ;
test "escaped" escaped '\000' 1 ;
test "escaped" escaped '[' 1 ;
test "escaped" escaped ']' 1 ;
test "escaped" escaped '!' 1 ;
test "escaped" escaped '#' 1 ;
()
;;

(* For compilation speed (due to J. Garigue) *)
exception Unknown_Reply of int

type command_reply =
   RPL_TRYAGAIN
 | RPL_TRACEEND
 | RPL_TRACELOG
 | RPL_ADMINEMAIL
 | RPL_ADMINLOC2
 | RPL_ADMINLOC1
 | RPL_ADMINME
 | RPL_LUSERME
 | RPL_LUSERCHANNELS
 | RPL_LUSERUNKNOWN
 | RPL_LUSEROP
 | RPL_LUSERCLIENT
 | RPL_STATSDLINE
 | RPL_STATSDEBUG
 | RPL_STATSDEFINE
 | RPL_STATSBLINE
 | RPL_STATSPING
 | RPL_STATSSLINE
 | RPL_STATSHLINE
 | RPL_STATSOLINE
 | RPL_STATSUPTIME
 | RPL_STATSLLINE
 | RPL_STATSVLINE
 | RPL_SERVLISTEND
 | RPL_SERVLIST
 | RPL_SERVICE
 | RPL_ENDOFSERVICES
 | RPL_SERVICEINFO
 | RPL_UMODEIS
 | RPL_ENDOFSTATS
 | RPL_STATSYLINE
 | RPL_STATSQLINE
 | RPL_STATSKLINE
 | RPL_STATSILINE
 | RPL_STATSNLINE
 | RPL_STATSCLINE
 | RPL_STATSCOMMANDS
 | RPL_STATSLINKINFO
 | RPL_TRACERECONNECT
 | RPL_TRACECLASS
 | RPL_TRACENEWTYPE
 | RPL_TRACESERVICE
 | RPL_TRACESERVER
 | RPL_TRACEUSER
 | RPL_TRACEOPERATOR
 | RPL_TRACEUNKNOWN
 | RPL_TRACEHANDSHAKE
 | RPL_TRACECONNECTING
 | RPL_TRACELINK
 | RPL_NOUSERS
 | RPL_ENDOFUSERS
 | RPL_USERS
 | RPL_USERSSTART
 | RPL_TIME
 | RPL_NOTOPERANYMORE
 | RPL_MYPORTIS
 | RPL_YOURESERVICE
 | RPL_REHASHING
 | RPL_YOUREOPER
 | RPL_ENDOFMOTD
 | RPL_MOTDSTART
 | RPL_ENDOFINFO
 | RPL_INFOSTART
 | RPL_MOTD
 | RPL_INFO
 | RPL_ENDOFBANLIST
 | RPL_BANLIST
 | RPL_ENDOFLINKS
 | RPL_LINKS
 | RPL_CLOSEEND
 | RPL_CLOSING
 | RPL_KILLDONE
 | RPL_ENDOFNAMES
 | RPL_NAMREPLY
 | RPL_ENDOFWHO
 | RPL_WHOREPLY
 | RPL_VERSION
 | RPL_SUMMONING
 | RPL_INVITING
 | RPL_TOPIC
 | RPL_NOTOPIC
 | RPL_CHANNELMODEIS
 | RPL_LISTEND
 | RPL_LIST
 | RPL_LISTSTART
 | RPL_WHOISCHANNELS
 | RPL_ENDOFWHOIS
 | RPL_WHOISIDLE
 | RPL_WHOISCHANOP
 | RPL_ENDOFWHOWAS
 | RPL_WHOWASUSER
 | RPL_WHOISOPERATOR
 | RPL_WHOISSERVER
 | RPL_WHOISUSER
 | RPL_NOWAWAY
 | RPL_UNAWAY
 | RPL_TEXT
 | RPL_ISON
 | RPL_USERHOST
 | RPL_AWAY
 | RPL_NONE

let get_command_reply n =
match n with
   263    ->     RPL_TRYAGAIN
 | 319    ->     RPL_WHOISCHANNELS
 | 318    ->     RPL_ENDOFWHOIS
 | 317    ->     RPL_WHOISIDLE
 | 316    ->     RPL_WHOISCHANOP
 | 369    ->     RPL_ENDOFWHOWAS
 | 314    ->     RPL_WHOWASUSER
 | 313    ->     RPL_WHOISOPERATOR
 | 312    ->     RPL_WHOISSERVER
 | 311    ->     RPL_WHOISUSER
 | 262    ->     RPL_TRACEEND
 | 261    ->     RPL_TRACELOG
 | 259    ->     RPL_ADMINEMAIL
 | 258    ->     RPL_ADMINLOC2
 | 257    ->     RPL_ADMINLOC1
 | 256    ->     RPL_ADMINME
 | 255    ->     RPL_LUSERME
 | 254    ->     RPL_LUSERCHANNELS
 | 253    ->     RPL_LUSERUNKNOWN
 | 252    ->     RPL_LUSEROP
 | 251    ->     RPL_LUSERCLIENT
 | 250    ->     RPL_STATSDLINE
 | 249    ->     RPL_STATSDEBUG
 | 248    ->     RPL_STATSDEFINE
 | 247    ->     RPL_STATSBLINE
 | 246    ->     RPL_STATSPING
 | 245    ->     RPL_STATSSLINE
 | 244    ->     RPL_STATSHLINE
 | 243    ->     RPL_STATSOLINE
 | 242    ->     RPL_STATSUPTIME
 | 241    ->     RPL_STATSLLINE
 | 240    ->     RPL_STATSVLINE
 | 235    ->     RPL_SERVLISTEND
 | 234    ->     RPL_SERVLIST
 | 233    ->     RPL_SERVICE
 | 232    ->     RPL_ENDOFSERVICES
 | 231    ->     RPL_SERVICEINFO
 | 221    ->     RPL_UMODEIS
 | 219    ->     RPL_ENDOFSTATS
 | 218    ->     RPL_STATSYLINE
 | 217    ->     RPL_STATSQLINE
 | 216    ->     RPL_STATSKLINE
 | 215    ->     RPL_STATSILINE
 | 214    ->     RPL_STATSNLINE
 | 213    ->     RPL_STATSCLINE
 | 212    ->     RPL_STATSCOMMANDS
 | 211    ->     RPL_STATSLINKINFO
 | 210    ->     RPL_TRACERECONNECT
 | 209    ->     RPL_TRACECLASS
 | 208    ->     RPL_TRACENEWTYPE
 | 207    ->     RPL_TRACESERVICE
 | 206    ->     RPL_TRACESERVER
 | 205    ->     RPL_TRACEUSER
 | 204    ->     RPL_TRACEOPERATOR
 | 203    ->     RPL_TRACEUNKNOWN
 | 202    ->     RPL_TRACEHANDSHAKE
 | 201    ->     RPL_TRACECONNECTING
 | 200    ->     RPL_TRACELINK
 | 395    ->     RPL_NOUSERS
 | 394    ->     RPL_ENDOFUSERS
 | 393    ->     RPL_USERS
 | 392    ->     RPL_USERSSTART
 | 391    ->     RPL_TIME
 | 385    ->     RPL_NOTOPERANYMORE
 | 384    ->     RPL_MYPORTIS
 | 383    ->     RPL_YOURESERVICE
 | 382    ->     RPL_REHASHING
 | 381    ->     RPL_YOUREOPER
 | 376    ->     RPL_ENDOFMOTD
 | 375    ->     RPL_MOTDSTART
 | 374    ->     RPL_ENDOFINFO
 | 373    ->     RPL_INFOSTART
 | 372    ->     RPL_MOTD
 | 371    ->     RPL_INFO
 | 368    ->     RPL_ENDOFBANLIST
 | 367    ->     RPL_BANLIST
 | 365    ->     RPL_ENDOFLINKS
 | 364    ->     RPL_LINKS
 | 363    ->     RPL_CLOSEEND
 | 362    ->     RPL_CLOSING
 | 361    ->     RPL_KILLDONE
 | 366    ->     RPL_ENDOFNAMES
 | 353    ->     RPL_NAMREPLY
 | 315    ->     RPL_ENDOFWHO
 | 352    ->     RPL_WHOREPLY
 | 351    ->     RPL_VERSION
 | 342    ->     RPL_SUMMONING
 | 341    ->     RPL_INVITING
 | 332    ->     RPL_TOPIC
 | 331    ->     RPL_NOTOPIC
 | 324    ->     RPL_CHANNELMODEIS
 | 323    ->     RPL_LISTEND
 | 322    ->     RPL_LIST
 | 321    ->     RPL_LISTSTART
 | 306    ->     RPL_NOWAWAY
 | 305    ->     RPL_UNAWAY
 | 304    ->     RPL_TEXT
 | 303    ->     RPL_ISON
 | 302    ->     RPL_USERHOST
 | 301    ->     RPL_AWAY
 | 300    ->     RPL_NONE
 | _ -> raise (Unknown_Reply n)

(* Bug 454 *)
type  habert_a=
  | A of habert_c
  | B of habert_c

and habert_c= {lvar:int; lassoc: habert_c;lnb:int}


let habert=function
  | (A {lnb=i}|B {lnb=i}) when i=0 -> 1
  | A {lassoc=({lnb=j});lnb=i} -> 2
  | _ -> 3
;;

let rec ex0 = {lvar=0 ; lnb=0 ; lassoc=ex1}
and ex1 = {lvar=1 ; lnb=1 ; lassoc=ex0} in

test "habert" habert (A ex0) 1 ;
test "habert" habert (B ex0) 1 ;
test "habert" habert (A ex1) 2 ;
test "habert" habert (B ex1) 3 ;

(* Problems with interval test in arithmetic mod 2^31, bug #359 *)
(* From manuel Fahndrich *)

type type_expr = [
  | `TTuple of type_expr list
  | `TConstr of type_expr list
  | `TVar of string
  | `TVariant of string list
  | `TBlock of int
  | `TCopy of type_expr
  ]

and recurs_type_expr = [
  | `TTuple of type_expr list
  | `TConstr of type_expr list
  | `TVariant of string list
  ]


let rec maf te =
    match te with
    | `TCopy te -> 1
    | `TVar _ -> 2
    | `TBlock _ -> 2
    | #recurs_type_expr as desc ->

        let te =
          (match desc with
            `TTuple tl ->
              4
          | `TConstr tl ->
              5
          | `TVariant (row) ->
              6
                )
        in

        te
;;

let base = `TBlock 0
;;

test "maf" maf (`TCopy base) 1 ;
test "maf" maf (`TVar "test") 2 ;
test "maf" maf (`TBlock 0) 2 ;
test "maf" maf (`TTuple []) 4 ;
test "maf" maf (`TConstr []) 5 ;
test "maf" maf (`TVariant []) 6
;;

(* PR#1310
  Using ``get_args'' in place or an ad-hoc ``matcher'' function for tuples.
  Has made the compiler [3.05] to fail.
*)
type t_seb = Uin | Uout
;;

let rec seb = function
  | ((i, Uin) | (i, Uout)), Uout -> 1
  | ((j, Uin) | (j, Uout)), Uin ->  2
;;

test "seb" seb ((0,Uin),Uout) 1 ;
test "seb" seb ((0,Uout),Uin) 2 ;
()
;;

(* Talk with Jacques
     - type 'b is still open ??
     - better case generation, accept intervals of size 1 when ok_inter is
       false (in Switch)
*)

(*
File "morematch.ml", line 1060, characters 8-65:
Warning: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
A `D
*)
type ('a, 'b) t_j = A of 'a | B of 'b * 'a | C

let f = function
  | A (`A|`C) -> 0
  | B (`B,`D) -> 1
  | C -> 2

let g x = try f x with Match_failure _ -> 3

let _ =
  test "jacques" g (A `A) 0 ;
  test "jacques" g (A `C) 0 ;
  test "jacques" g (B (`B,`D)) 1 ;
  test "jacaues" g C 2 ;
(*  test "jacques" g (B (`A,`D)) 3 ; (* type incorrect expected behavior ? *)*)
  ()

(*
  Compilation bug, segfault, because of incorrect compilation
  of unused match case .. -> "11"
*)

type t_l = A | B

let f = function
  |  _, _, _, _, _, _, _, _, _, _, _, _, _, B, _, _ -> "0"
  |  _, _, _, B, A, _, _, _, _, _, _, _, _, _, _, _ -> "1"
  |  _, _, _, B, _, A, _, _, A, _, _, _, _, _, _, _ -> "2"
  |  _, _, _, _, _, _, _, _, _, _, B, A, _, A, _, _ -> "3"
  |  _, _, _, _, _, _, _, B, _, _, _, _, B, _, A, A -> "4"
  |  A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ -> "5"
  |  _, _, _, _, _, _, _, B, _, B, _, _, _, _, _, _ -> "6"
  |  _, B, _, _, _, _, _, _, _, _, _, _, _, _, _, _ -> "7"
  |  _, A, A, _, A, _, B, _, _, _, _, _, _, _, _, B -> "8"
  |  _, _, _, _, B, _, _, _, _, _, _, _, _, _, B, _ -> "9"
  |  _, _, _, _, _, _, _, _, _, _, _, B, _, _, _, _ -> "10"
  |  _, _, _, _, _, A, _, _, _, _, B, _, _, _, _, _ -> "11"
  |  B, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ -> "12"
  |  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ -> "13"

(*
File "morematch.ml", line 1094, characters 5-51:
Warning: this match case is unused.
File "morematch.ml", line 1096, characters 5-51:
Warning: this match case is unused.
*)
let _  =
  test "luc"  f (B, A, A, A, A, A, A, A, A, A, A, B, A, A, A, A) "10" ;
  test "luc"  f (B, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A) "12" ;
 ()

(*
  By Gilles Peskine, compilation raised some assert false i make_failactionneg
*)

type bg = [
  | `False
  | `True
  ]

type vg = [
  | `A
  | `B
  | `U of int
  | `V of int
  ]

type tg = {
    v : vg;
    x : bg;
  }

let predg x = true

let rec gilles o = match o with
  | {v = (`U data | `V data); x = `False} when predg o -> 1
  | {v = (`A|`B) ; x = `False}
  | {v = (`U _ | `V _); x = `False}
  | {v = _ ; x = `True}
    -> 2

(*
  Match in trywith should always have a default case
*)

exception Found of string * int
exception Error of string


let lucexn  e =
  try
    try  raise e  with Error msg -> msg
  with Found (s,r) -> s^string_of_int r

let () =
  test "lucexn1" lucexn  (Error "coucou") "coucou" ;
  test "lucexn2" lucexn (Found ("int: ",0)) "int: 0" ;
  ()

(*
  PR#5758: different representations of floats
*)

let pr5758 x str =
  match (x, str) with
  | (1. , "A") -> "Matched A"
  | (1.0, "B") -> "Matched B"
  | (1. , "C") -> "Matched C"
  | result ->
    match result with
    | (1., "A") -> "Failed match A then later matched"
    | _ -> "Failed twice"
;;

let () =
  test "pr5758" (pr5758 1.) "A" "Matched A"
;;
