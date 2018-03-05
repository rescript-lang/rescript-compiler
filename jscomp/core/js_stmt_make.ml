(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)






module E = Js_exp_make 

type t = J.statement 

let return ?comment e : t = 
  {statement_desc = Return {return_value = e; } ; comment}

let return_unit ?comment () : t =              
  return ?comment  E.unit 

let break ?comment () : t = 
  {comment ; statement_desc = Break }

let mk ?comment  statement_desc : t = 
  {statement_desc; comment}

let empty ?comment  () : t = { statement_desc = Block []; comment}

let throw ?comment v : t = { statement_desc = J.Throw v; comment}

(* avoid nested block *)
let  rec block ?comment  (b : J.block)   : t =  
  match b with 
  | [{statement_desc = Block bs }  ] -> block bs
  | [b] -> b
  | [] -> empty ?comment ()
  | _ -> {statement_desc = Block b  ; comment}

(* It's a statement, we can discard some values *)       
let rec exp ?comment (e : E.t) : t = 
  match e.expression_desc with 
  | (Seq( {expression_desc = Number _}, b) 
    | Seq( b, {expression_desc = Number _})) -> exp ?comment b 
  | Number _ -> block []
  (* TODO: we can do more *)      
  (* | _ when is_pure e ->  block [] *)
  |  _ -> 
    { statement_desc = Exp e; comment}

let declare_variable ?comment  ?ident_info  ~kind (v:Ident.t)  : t=
  let property : J.property =  kind in
  let ident_info  : J.ident_info  = 
    match ident_info with
    | None ->  {used_stats = NA}
    | Some x -> x in
  {statement_desc = 
     Variable { ident = v; value = None; property ; 
                ident_info ;};
   comment}

let define ?comment  ?ident_info ~kind (v:Ident.t) exp    : t=
  let property : J.property =  kind in
  let ident_info  : J.ident_info  = 
    match ident_info with
    | None ->  {used_stats = NA}
    | Some x -> x in
  {statement_desc = 
     Variable { ident = v; value =  Some exp; property ; 
                ident_info ;};
   comment}

let int_switch ?comment   ?declaration ?default (e : J.expression)  clauses : t = 
  match e.expression_desc with 
  | Number (Int {i; _}) -> 
    let continuation =  
      begin match List.find (fun (x : _ J.case_clause) -> x.case = (Int32.to_int i)) clauses
        with 
        | case -> fst case.body
        | exception Not_found -> 
          begin match default with
            | Some x ->  x 
            | None -> assert false
          end 
      end in
    begin match declaration, continuation with 
      | Some (kind, did), 
        [ {statement_desc = Exp {expression_desc = Bin(Eq,  {expression_desc = Var (Id id) ; _}, e0); _}; _}]
        when Ident.same did id 
        -> 
        define ?comment ~kind id e0
      | Some(kind,did), _ 
        -> 
        block (declare_variable ?comment ~kind did :: continuation)
      | None, _ -> block continuation
    end

  | _ -> 
    match declaration with 
    | Some (kind, did) -> 
      block [declare_variable ?comment ~kind did ;
             { statement_desc = J.Int_switch (e,clauses, default); comment}]
    | None ->  { statement_desc = J.Int_switch (e,clauses, default); comment}

let string_switch ?comment ?declaration  ?default (e : J.expression)  clauses : t= 
  match e.expression_desc with 
  | Str (_,s) -> 
    let continuation = 
      begin match List.find 
                    (fun  (x : string J.case_clause) -> x.case = s) clauses
        with 
        | case ->  (fst case.body)
        | exception Not_found -> 
          begin match default with 
            | Some x -> x 
            | None -> assert false 
          end
      end in
    begin match declaration, continuation with 
      | Some (kind, did),
        [ {statement_desc = Exp {expression_desc = Bin(Eq,  {expression_desc = Var (Id id); _}, e0);_} ; _}]
        when Ident.same did id 
        -> 
        define ?comment ~kind id e0
      | Some(kind,did), _ 
        -> 
        block @@ declare_variable ?comment ~kind did :: continuation
      | None, _ -> block continuation
    end
  | _  -> 
    match declaration with 
    | Some (kind,did) -> 
      block [declare_variable ?comment ~kind did ;
             { statement_desc = String_switch (e,clauses, default); comment}]
    | None -> { statement_desc = String_switch (e,clauses, default); comment}


(* TODO: it also make sense  to extract some common statements 
    between those two branches, it does happen since in OCaml you 
    have to write some duplicated code due to the types system restriction
    example:
   {[
     | Format_subst (pad_opt, fmtty, rest) ->
       buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
       bprint_pad_opt buf pad_opt; buffer_add_char buf '(';
       bprint_fmtty buf fmtty; buffer_add_char buf '%'; buffer_add_char buf ')';
       fmtiter rest false;

       | Scan_char_set (width_opt, char_set, rest) ->
       buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
       bprint_pad_opt buf width_opt; bprint_char_set buf char_set;
       fmtiter rest false;
   ]}
*)
let rec if_ ?comment  ?declaration ?else_ (e : J.expression) (then_ : J.block)   : t = 
  let declared = ref false in
  let rec aux ?comment (e : J.expression) (then_ : J.block) (else_ : J.block ) acc   =
    match e.expression_desc, then_, (else_ : J.block ) with 
    | _, [ {statement_desc = Return {return_value = b; _}; _}], 
      [ {statement_desc = Return {return_value = a; _}; _}]
      ->
      return (E.econd e b a ) :: acc 
    | _,  [ {statement_desc = 
               Exp {expression_desc = Bin(Eq, ({expression_desc = Var (Id id0); _} as l0), a0); _}; _}], 
      [ {statement_desc = 
           Exp ({ expression_desc = Bin(Eq, 
                                        {expression_desc = Var (Id id1); _}, b0); _}); _}]
      when Ident.same id0 id1 -> 
      begin match declaration with 
        | Some (kind,did)  when Ident.same did id0 -> 
          declared := true;
          define ~kind id0 (E.econd e a0 b0) :: acc 
        (* To hit this branch, we also need [declaration] passed down 
           TODO: check how we compile [Lifthenelse]
        *)
        | _ -> 
          exp (E.assign l0 (E.econd e a0 b0)) :: acc 
      end

    | _,  _,  
      [ {statement_desc = Exp {expression_desc = Number _}; _}]
      ->
      aux ?comment e then_ [] acc 
    | _, [ {statement_desc = Exp {expression_desc = Number _}; _}], _
      ->
      aux ?comment e [] else_ acc 

    | _,  [ {statement_desc = Exp b; _}],  [ {statement_desc = Exp a; _}]
      ->
      exp (E.econd e b a) :: acc 
    | _, [], []                                   
      -> exp e :: acc 
    | Caml_not e, _ , _ :: _
    | Js_not e, _ , _ :: _
      -> aux ?comment e else_ then_ acc
    | _, [], _
      ->
      aux ?comment (E.not e) else_ [] acc
    (* Be careful that this re-write may result in non-terminating effect *)
    | _, (y::ys),  (x::xs)
      when Js_analyzer.(eq_statement x y && no_side_effect_expression e)
      ->
      (** here we do agressive optimization, because it can help optimization later,
          move code outside of branch is generally helpful later
      *)
      aux ?comment e ys xs (y::acc)

    |  Number ( Int { i = 0l; _}) , _,  _
      ->  
      begin match else_ with 
        | [] -> acc 
        | _ -> block else_ ::acc
      end
    |  (Number _ , _, _
       | (Bin (Ge, 
               ({expression_desc = Length _;
                 _}), {expression_desc = Number (Int { i = 0l; _})})), _ , _)
      (* TODO: always 
          turn [Le] -> into [Ge]
      *)
      -> block then_ :: acc 
    | Bin (Bor , a, {expression_desc = Number (Int { i = 0l ; _})}), _, _ 
    | Bin (Bor , {expression_desc = Number (Int { i = 0l ; _})}, a), _, _ 
      -> 
      aux ?comment a  then_ else_ acc
    (* | Bin (NotEqEq, e1,  *)
    (*        {expression_desc = Var (Id ({name = "undefined"; _} as id))}), *)
    (*   _, _ *)
    (*   when Ext_ident.is_js id ->  *)
    (*   aux ?comment e1 then_ else_ acc  *)

    | ((Bin (Gt, 
             ({expression_desc = 
                 Length _;
               _} as e ), {expression_desc = Number (Int { i = 0l; _})}))

      | Int_of_boolean e), _ , _
      ->
      (** Add comment when simplified *)
      aux ?comment e then_ else_ acc 

    | _ -> 
      let e = E.ocaml_boolean_under_condition e in 
      { statement_desc = If (e, 
                             then_,
                             (match else_ with 
                              | [] -> None
                              |  v -> Some  v)); 
        comment } :: acc in
  let if_block = 
    aux ?comment e then_ (match else_ with None -> [] | Some v -> v) [] in

  match !declared, declaration with 
  | true , _ 
  | _    , None  ->  block (List.rev if_block)
  | false, Some (kind, did) -> block (declare_variable ~kind did :: List.rev if_block )



let alias_variable ?comment  ?exp (v:Ident.t)  : t=
  {statement_desc = 
     Variable {
       ident = v; value = exp; property = Alias;
       ident_info = {used_stats = NA }   };
   comment}

let assign ?comment  id e : t = 
  {
    statement_desc = J.Exp ( E.assign (E.var id) e ) ;
    comment
  }
let assign_unit ?comment  id :  t = 
  {
    statement_desc = J.Exp( E.assign (E.var id) E.unit);
    comment
  }
let declare_unit ?comment  id :  t = 
  {
    statement_desc = 
      J.Variable { ident =  id; 
                   value = Some E.unit;
                   property = Variable;
                   ident_info = {used_stats = NA}
                 };
    comment
  }

let rec while_  ?comment  ?label ?env (e : E.t) (st : J.block) : t = 
  match e with 
  (* | {expression_desc = Int_of_boolean e; _} ->  *)
  (*   while_ ?comment  ?label  e st *)
  | _ -> 
    let e = E.ocaml_boolean_under_condition e in
    let env = 
      match env with 
      | None -> Js_closure.empty ()
      | Some x -> x in
    {
      statement_desc = While (label, e, st, env);
      comment
    }

let for_ ?comment   ?env 
    for_ident_expression
    finish_ident_expression id direction (b : J.block) : t =
  let env = 
    match env with 
    | None -> Js_closure.empty ()
    | Some x -> x 
  in
  {
    statement_desc = 
      ForRange (for_ident_expression, finish_ident_expression, id, direction, b, env);
    comment
  }

let try_ ?comment   ?with_ ?finally body : t = 
  {
    statement_desc = Try (body, with_, finally) ;
    comment
  }

let unknown_lambda ?(comment="unknown")  (lam : Lam.t ) : t = 
  exp @@ E.str ~comment ~pure:false 
    (Lam_print.lambda_to_string lam) 

(* TODO: 
    actually, only loops can be labelled
*)    
let continue  ?comment   ?(label="") unit  : t = 
  { 
    statement_desc = J.Continue  label;
    comment;
  }

let debugger : t = 
  { statement_desc = J.Debugger ; 
    comment = None 
  }
