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


let get_files dir = 
  let arr = 
    Sys.readdir dir 
    |> Ext_array.filter_map 
      (fun  x -> 
         if Ext_path.check_suffix_case x ".js"  then 
           let y = Ext_path.chop_all_extensions_if_any x in 
           if y <> "unix" &&
              y <> "bigarray" && 
              y <> "std_exit" &&
              y <> "unixLabels" && 
              y <> "node_process" (* does not work in browser*)
           then 
             Some ( "./stdlib/" ^ Filename.chop_extension (Filename.basename x))
           else None
         else None  )
  in
  (* Sort to guarantee it works the same across OSes *)
  Array.sort (fun (x : string) y -> Pervasives.compare x y ) arr;
  Array.to_list arr



let () = 
  Ext_pervasives.with_file_as_chan "./pre_load.js" 
    (fun chan -> 
       output_string chan 
         (Printf.sprintf "function start(gist){require([%s], function(){loadGist(gist)})}" 
            (String.concat "," 
               (Ext_list.map (Printf.sprintf "%S" )
                  (get_files "../lib/amdjs")
               ))))  
