(* OCamlScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)





(* let elimnate_ref =  *)
(*   object(self) *)
(*     inherit Lam_map.map as super *)
(*     method! lambda lam =  *)
(*       match lam with  *)
(*       | Llet ((Strict | StrictOpt as kind), *)
(*               v, Lprim((Pmakeblock(0,_,Mutable) as prim), [linit],lbody) *)
(*              ) *)
(*           ->  *)
(*             let slinit = self#lambda linit in *)
(*             let sbody = self#lambda lbody in *)
(*             begin try *)
(*             Lam_util.refine_let  *)
(*               ~kind:Variable v slinit *)
(*                 (Pass_lets.eliminate_ref v sbody) *)
(*             with Real_reference ->  *)
(*               Lam_util.refine_let  *)
(*                 ~kind v (Lprim (prim,[slinit])) sbody *)
(*             end *)
(*       | _ -> super#lambda lam *)
(*   end *)
