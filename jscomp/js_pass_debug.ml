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

let log_counter = ref 0 

let dump  (prog : J.program) = 
  begin
    let () = 
      if Js_config.get_env () <> Browser 
      (* TODO: when no [Browser] detection, it will go through.. bug in js_of_ocaml? *)
      && Lam_current_unit.is_same_file ()
      then 
        begin
          incr log_counter ; 
          Ext_pervasives.with_file_as_chan       
            (Ext_filename.chop_extension ~loc:__LOC__ (Lam_current_unit.get_file()) ^
             (Printf.sprintf ".%02d.jsx" !log_counter)
            ) (fun chan -> Js_dump.dump_program prog chan )
        end in
    prog    
  end

 
