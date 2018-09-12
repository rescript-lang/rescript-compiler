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


let iter f l = 
  match (l : Lam.t) with 
    Lvar _
  | Lconst _ -> ()
  | Lapply{fn; args; _} ->
      f fn; List.iter f args
  | Lfunction{body;_} ->
      f body
  | Llet(str, id, arg, body) ->
      f arg; f body
  | Lletrec(decl, body) ->
      f body;
      List.iter (fun (id, exp) -> f exp) decl
  | Lam.Lglobal_module _ -> ()     
  | Lprim {args; _} ->
      List.iter f args
  | Lswitch(arg, sw) ->
      f arg;
      List.iter (fun (key, case) -> f case) sw.sw_consts;
      List.iter (fun (key, case) -> f case) sw.sw_blocks;
      begin match sw.sw_failaction with 
      | None -> ()
      | Some a -> f a 
      end
  | Lstringswitch (arg,cases,default) ->
      f arg ;
      List.iter (fun (_,act) -> f act) cases ;
      begin match default with 
      | None -> ()
      | Some a -> f a 
      end
  | Lstaticraise (_,args) ->
      List.iter f args
  | Lstaticcatch(e1, (_,vars), e2) ->
      f e1; f e2
  | Ltrywith(e1, exn, e2) ->
      f e1; f e2
  | Lifthenelse(e1, e2, e3) ->
      f e1; f e2; f e3
  | Lsequence(e1, e2) ->
      f e1; f e2
  | Lwhile(e1, e2) ->
      f e1; f e2
  | Lfor(v, e1, e2, dir, e3) ->
      f e1; f e2; f e3
  | Lassign(id, e) ->
      f e
  | Lsend (k, met, obj, args, _) ->
      List.iter f (met::obj::args)

