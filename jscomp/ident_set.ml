(* BuckleScript compiler
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




(* TODO: 
    when we do the union, it would be nice that 
    we know which [delta] is there
 *)
include Set.Make(struct 
  type t = Ident.t 
  let compare  = Pervasives.compare (* TODO: fix me *)
end)

let print ppf v =
  Ext_format.(brace_vgroup ppf 0 (fun _ ->
  iter (fun v -> 
    string ppf  (Printf.sprintf "%s/%d" v.Ident.name v.stamp) ; Ext_format.space ppf  ) v ))
