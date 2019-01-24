(* Warn about all relevant cases when possible *)
let f = function
    None, None -> 1
  | Some _, Some _ -> 2;;

(* Exhaustiveness check is very slow *)
type _ t =
  A : int t | B : bool t | C : char t | D : float t
type (_,_,_,_) u = U : (int, int, int, int) u
type v = E | F | G
;;

let f : type a b c d e f g.
      a t * b t * c t * d t * e t * f t * g t * v
       * (a,b,c,d) u * (e,f,g,g) u -> int =
 function A, A, A, A, A, A, A, _, U, U -> 1
   | _, _, _, _, _, _, _, G, _, _ -> 1
   (*| _ -> _ *)
;;

(* Unused cases *)
let f (x : int t) = match x with A -> 1 | _ -> 2;; (* warn *)
let f (x : unit t option) = match x with None -> 1 | _ -> 2 ;; (* warn? *)
let f (x : unit t option) = match x with None -> 1 | Some _ -> 2 ;; (* warn *)
let f (x : int t option) = match x with None -> 1 | _ -> 2;;
let f (x : int t option) = match x with None -> 1;; (* warn *)

(* Example with record, type, single case *)

type 'a box = Box of 'a
type 'a pair = {left: 'a; right: 'a};;

let f : (int t box pair * bool) option -> unit = function None -> ();;
let f : (string t box pair * bool) option -> unit = function None -> ();;
let f = function {left=Box 0; _ } -> ();;
let f = function {left=Box 0;right=Box 1} -> ();;

(* Examples from ML2015 paper *)

type _ t =
  | Int : int t
  | Bool : bool t
;;

let f : type a. a t -> a = function
  | Int -> 1
  | Bool -> true
;;
let g : int t -> int = function
  | Int -> 1
;;
let h : type a. a t -> a t -> bool =
  fun x y -> match x, y with
  | Int, Int -> true
  | Bool, Bool -> true
;;
type (_, _) cmp =
 | Eq : ('a, 'a) cmp
 | Any: ('a, 'b) cmp
module A : sig type a type b val eq : (a, b) cmp end
  = struct type a type b = a let eq = Eq end
;;
let f : (A.a, A.b) cmp -> unit = function Any -> ()
;;
let deep : char t option -> char =
  function None -> 'c'
;;
type zero = Zero
type _ succ = Succ
;;
type (_,_,_) plus =
  | Plus0 : (zero, 'a, 'a) plus
  | PlusS : ('a, 'b, 'c) plus ->
       ('a succ, 'b, 'c succ) plus
;;
let trivial : (zero succ, zero, zero) plus option -> bool =
  function None -> false
;;
let easy : (zero, zero succ, zero) plus option -> bool =
  function None -> false
;;
let harder : (zero succ, zero succ, zero succ) plus option -> bool =
  function None -> false
;;
let harder : (zero succ, zero succ, zero succ) plus option  -> bool =
  function None -> false | Some (PlusS _) -> .
;;
let inv_zero : type a b c d. (a,b,c) plus -> (c,d,zero) plus -> bool =
  fun p1 p2 ->
    match p1, p2 with
    | Plus0, Plus0 -> true
;;


(* Empty match *)

type _ t = Int : int t;;
let f (x : bool t) = match x with _ -> . ;; (* ok *)


(* trefis in PR#6437 *)

let f () = match None with _ -> .;; (* error *)
let g () = match None with _ -> () | exception _ -> .;; (* error *)
let h () = match None with _ -> .  | exception _ -> .;; (* error *)
let f x = match x with _ -> () | None -> .;; (* do not warn *)

(* #7059, all clauses guarded *)

let f x y = match 1 with 1 when x = y -> 1;;

(* #7504, Example with no constraints on a record *)
let f = function {contents=_}, 0 -> 0;;
