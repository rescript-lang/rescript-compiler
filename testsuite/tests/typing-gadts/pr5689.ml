type inkind = [ `Link | `Nonlink ]

type _ inline_t =
   | Text: string -> [< inkind > `Nonlink ] inline_t
   | Bold: 'a inline_t list -> 'a inline_t
   | Link: string -> [< inkind > `Link ] inline_t
   | Mref: string * [ `Nonlink ] inline_t list -> [< inkind > `Link ] inline_t
;;

let uppercase seq =
   let rec process: type a. a inline_t -> a inline_t = function
       | Text txt       -> Text (String.uppercase txt)
       | Bold xs        -> Bold (List.map process xs)
       | Link lnk       -> Link lnk
       | Mref (lnk, xs) -> Mref (lnk, List.map process xs)
   in List.map process seq
;;

type ast_t =
   | Ast_Text of string
   | Ast_Bold of ast_t list
   | Ast_Link of string
   | Ast_Mref of string * ast_t list
;;

let inlineseq_from_astseq seq =
   let rec process_nonlink = function
       | Ast_Text txt  -> Text txt
       | Ast_Bold xs   -> Bold (List.map process_nonlink xs)
       | _             -> assert false in
   let rec process_any = function
       | Ast_Text txt       -> Text txt
       | Ast_Bold xs        -> Bold (List.map process_any xs)
       | Ast_Link lnk       -> Link lnk
       | Ast_Mref (lnk, xs) -> Mref (lnk, List.map process_nonlink xs)
   in List.map process_any seq
;;

(* OK *)
type _ linkp =
 | Nonlink : [ `Nonlink ] linkp
 | Maylink : inkind linkp
;;
let inlineseq_from_astseq seq =
 let rec process : type a. a linkp -> ast_t -> a inline_t =
   fun allow_link ast ->
     match (allow_link, ast) with
     | (Maylink, Ast_Text txt)    -> Text txt
     | (Nonlink, Ast_Text txt)    -> Text txt
     | (x, Ast_Bold xs)           -> Bold (List.map (process x) xs)
     | (Maylink, Ast_Link lnk)    -> Link lnk
     | (Nonlink, Ast_Link _)      -> assert false
     | (Maylink, Ast_Mref (lnk, xs)) ->
         Mref (lnk, List.map (process Nonlink) xs)
     | (Nonlink, Ast_Mref _)      -> assert false
   in List.map (process Maylink) seq
;;

(* Bad *)
type _ linkp2 = Kind : 'a linkp -> ([< inkind ] as 'a) linkp2
;;
let inlineseq_from_astseq seq =
let rec process : type a. a linkp2 -> ast_t -> a inline_t =
  fun allow_link ast ->
    match (allow_link, ast) with
    | (Kind _, Ast_Text txt)    -> Text txt
    | (x, Ast_Bold xs)           -> Bold (List.map (process x) xs)
    | (Kind Maylink, Ast_Link lnk)    -> Link lnk
    | (Kind Nonlink, Ast_Link _)      -> assert false
    | (Kind Maylink, Ast_Mref (lnk, xs)) ->
        Mref (lnk, List.map (process (Kind Nonlink)) xs)
    | (Kind Nonlink, Ast_Mref _)      -> assert false
  in List.map (process (Kind Maylink)) seq
;;
