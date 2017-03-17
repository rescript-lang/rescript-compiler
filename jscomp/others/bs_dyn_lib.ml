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

(** experimental API *)

(** Generates a string representation of a Bs_dyn. value *)
let to_string (v: Bs_dyn.value) : string =
    let rec construct_string (b: Buffer.t) (v: Bs_dyn.value) (tab_level: int) =
      (* inserts spaces before string and adds to buffer *)
      let add tabs str =
        for i = 0 to tabs do
          Buffer.add_string b " ";
        done;
        Buffer.add_string b str;
      in

      begin match v with
      | Int32 x -> add tab_level ( Int32.to_string x ); add 0 "l"
      | Int64 x -> add tab_level ( Int64.to_string x ); add 0 "L"
      | Int x -> add tab_level ( string_of_int x );
      | Nativeint x -> add tab_level ( Nativeint.to_string x ); add 0 "n"
      | Bool x -> add tab_level ( string_of_bool x );
      | Float x -> add tab_level ( string_of_float x );
      | Char x -> add tab_level "\'"; add 0 ( Char.escaped x ); add 0 "\'"
      | String x -> add tab_level "\""; add 0 x; add 0 "\""
      | OptionNone -> add tab_level "None\n"
      | OptionSome x -> add tab_level "Some"; construct_string b x 0
      | Tuple x -> 
        add tab_level "(\n"; 
        Array.iteri (fun i item -> 
          construct_string b item (tab_level + 2);
          (if (i != (Array.length x) - 1 ) then
          add 0 ", " );
          add 0 "\n"
        ) x;
        add tab_level ")";
      | Array x ->
        add tab_level "[|\n";
        Array.iter (fun item -> 
          construct_string b item (tab_level + 2) ;
          add 0 ";\n"
        ) x;
        add tab_level "|]"
      | List x ->
        add tab_level "[";
        Array.iter (fun item -> construct_string b item (tab_level + 2) ) x;
        add tab_level "]\n"
      | Record (shape, values) ->
        add tab_level "{\n";
        Array.iteri (fun i item -> 
          add (tab_level + 2) item;
          add 0 " =\n" ;
          construct_string b values.(i) (tab_level + 2);
          add 0 ";\n"
        ) shape;
        add tab_level "}"
      (* Variant only prints the selected value *)
      | Variant ({constructors}, tag, values) ->
        add tab_level constructors.(tag); 
        if Array.length values <> 0 then 
          begin 
            add tab_level "(\n";
            Array.iteri (fun i item -> 
                construct_string b values.(i) (tab_level + 2)
              ) values;
            add tab_level ")"
          end
      end;
    in
    let buffer = Buffer.create 1024 in
    construct_string buffer v 0;
    Buffer.contents buffer


