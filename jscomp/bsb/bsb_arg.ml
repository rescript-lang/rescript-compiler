(* Copyright (C) 2020- Authors of BuckleScript
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




 type key = string
 type doc = string
 type anon_fun = rev_args:string list -> unit
 
 type string_action = 
   | String_call of (string -> unit)  
   | String_set of string ref

 type unit_action = 
    | Unit_call of (unit -> unit) 
    | Unit_set of bool ref

 type spec =
   | Unit of unit_action
   | String of string_action 
 
 
 exception Bad of string
 
 
 type error =
   | Unknown of string
   | Missing of string
 
 type t = (string * spec * string) list 
 
 let rec assoc3 (x : string) (l : t) =
   match l with
   | [] -> None
   | (y1, y2, _) :: _ when y1 = x -> Some y2
   | _ :: t -> assoc3 x t
 ;;
 
 
 let (+>) = Ext_buffer.add_string
 
 let usage_b (buf : Ext_buffer.t) ~usage speclist  =
   buf +> usage;
   buf +> "\nOptions:\n";
   let max_col = ref 0 in 
   Ext_list.iter speclist (fun (key,_,_) -> 
       if String.length key > !max_col then 
         max_col := String.length key
     );
   Ext_list.iter speclist (fun (key,_,doc) -> 
       buf +> "  ";
       buf +> key ; 
       buf +> (String.make (!max_col - String.length key + 2 ) ' ');
       let cur = ref 0 in 
       let doc_length = String.length doc in 
       while !cur < doc_length do 
         match String.index_from_opt doc !cur '\n' with 
         | None -> 
           if !cur <> 0 then begin 
             buf +>  "\n";
             buf +> String.make (!max_col + 4) ' ' ;
           end;
           buf +> String.sub doc !cur (String.length doc - !cur );
           cur := doc_length
         | Some new_line_pos -> 
           if !cur <> 0 then begin 
             buf +>  "\n";
             buf +> String.make (!max_col + 4) ' ' ;
           end;
           buf +> String.sub doc !cur (new_line_pos - !cur );
           cur := new_line_pos + 1
       done ;
       buf +> "\n"
     )
 ;;
 
 
   
 let stop_raise ~usage ~(error : error) speclist   =
   let b = Ext_buffer.create 200 in  
   begin match error with
     | Unknown ("-help" | "--help" | "-h") -> 
       usage_b b ~usage speclist ;
       Ext_buffer.output_buffer stdout b;
       exit 0      
     | Unknown s ->
       b +> "unknown option: '";
       b +> s ;
       b +> "'.\n"
     | Missing s ->
       b +> "option '";
       b +> s;
       b +> "' needs an argument.\n"      
   end;
   usage_b b ~usage speclist ;
   raise (Bad (Ext_buffer.contents b))
 
 
 let parse_exn  ~usage ~argv ?(start=1) ?(finish=Array.length argv) (speclist : t) anonfun = 
   let current = ref start in 
   let rev_list = ref [] in 
   while !current < finish do
     let s = argv.(!current) in
     incr current;  
     if s <> "" && s.[0] = '-' then begin
       match assoc3 s speclist with 
       | Some action -> begin       
           begin match action with 
             | Unit r -> 
               begin match r with 
                 | Unit_set r -> r.contents <- true
                 | Unit_call f -> f ()
               end
             | String f  ->
               if !current >= finish then stop_raise ~usage ~error:(Missing s) speclist 
               else begin                 
                 let arg = argv.(!current) in 
                 incr current;  
                 match f with 
                 | String_call f ->   
                   f arg
                 | String_set u -> u.contents <- arg
               end             
           end;      
         end;      
       | None -> stop_raise ~usage ~error:(Unknown s) speclist 
     end else begin
       rev_list := s :: !rev_list;      
     end;
   done;
   anonfun ~rev_args:!rev_list
 ;;
 
 
 