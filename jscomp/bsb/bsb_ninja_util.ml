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




type override = 
  | Append of string 
  | AppendList of string list
  (* Append s 
     s
  *)
  | AppendVar of string 
  (* AppendVar s 
     $s
  *)
  | Overwrite of string 

  | OverwriteVar of string 
    (*
      OverwriteVar s 
      $s
    *)

type shadow = 
  { key : string ; op : override }

let output_build
    ?(order_only_deps=[])
    ?(implicit_deps=[])
    ?(outputs=[])
    ?(implicit_outputs=[])
    ?(inputs=[])
    ?(shadows=([] : shadow list))
    ?restat
    ~output
    ~input
    ~rule
    oc =
  let rule = Bsb_rule.get_name rule  oc in (* Trigger building if not used *)
  output_string oc "build ";
  output_string oc output ;
  outputs |> List.iter (fun s -> output_string oc Ext_string.single_space ; output_string oc s  );
  begin match implicit_outputs with
    | [] -> ()
    | _ ->
      output_string oc " | ";
      implicit_outputs |> List.iter (fun s -> output_string oc Ext_string.single_space ; output_string oc s)
  end;
  output_string oc " : ";
  output_string oc rule;
  output_string oc Ext_string.single_space;
  output_string oc input;
  inputs |> List.iter (fun s ->   output_string oc Ext_string.single_space ; output_string oc s);
  begin match implicit_deps with
    | [] -> ()
    | _ ->
      begin
        output_string oc " | ";
        implicit_deps
        |>
        List.iter (fun s -> output_string oc Ext_string.single_space; output_string oc s )
      end
  end;
  begin match order_only_deps with
    | [] -> ()
    | _ ->
      begin
        output_string oc " || ";
        order_only_deps
        |>
        List.iter (fun s -> output_string oc Ext_string.single_space ; output_string oc s)
      end
  end;
  output_string oc "\n";
  begin match shadows with
    | [] -> ()
    | xs ->
      List.iter (fun {key=k; op= v} ->
          output_string oc "  " ;
          output_string oc k ;
          output_string oc " = ";
          match v with
          | Overwrite s -> 
            output_string oc s ; 
            output_string oc "\n"
          | OverwriteVar s ->
            output_string oc "$";
            output_string oc s ; 
            output_string oc "\n"
          | AppendList ls -> 
            output_string oc "$" ;
            output_string oc k;
            List.iter 
              (fun s ->
                 output_string oc Ext_string.single_space;
                 output_string oc s 
                 ) ls;
            output_string oc "\n"
          | Append s ->
            output_string oc "$" ;
            output_string oc k;
            output_string oc Ext_string.single_space;
            output_string oc s ; output_string oc "\n"
          | AppendVar s ->   
            output_string oc "$" ;
            output_string oc k;
            output_string oc Ext_string.single_space;
            output_string oc "$";
            output_string oc s ; 
            output_string oc "\n"
        ) xs
  end;
  begin match restat with
    | None -> ()
    | Some () ->
      output_string oc Ext_string.single_space ;
      output_string oc "restat = 1 \n"
  end


let phony ?(order_only_deps=[]) ~inputs ~output oc =
  output_string oc "build ";
  output_string oc output ;
  output_string oc " : ";
  output_string oc "phony";
  output_string oc Ext_string.single_space;
  inputs |> List.iter (fun s ->   output_string oc Ext_string.single_space ; output_string oc s);
  begin match order_only_deps with
    | [] -> ()
    | _ ->
      begin
        output_string oc " || ";
        order_only_deps
        |>
        List.iter (fun s -> output_string oc Ext_string.single_space ; output_string oc s)
      end
  end;
  output_string oc "\n"

let output_kv key value oc  =
  output_string oc key ;
  output_string oc " = ";
  output_string oc value ;
  output_string oc "\n"

let output_kvs kvs oc =
  Array.iter (fun (k,v) -> output_kv k v oc) kvs


