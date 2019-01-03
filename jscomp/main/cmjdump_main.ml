(* Copyright (C) 2017 Authors of BuckleScript
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

open Printf

let f fmt = fprintf stdout fmt 

let pp_cmj_case  (cmj_case : Js_cmj_format.cmj_case) : unit = 
  match cmj_case with 
  | Little_js -> 
    f  "case : little, .js \n"
  | Little_bs -> 
    f  "case : little, .bs.js \n"    
  | Upper_js -> 
    f  "case: upper, .js  \n"
  | Upper_bs -> 
    f  "case: upper, .bs.js  \n"    

let pp_cmj fmt 
    ({ values ; effect; npm_package_path ; cmj_case} :Js_cmj_format.t) = 
  f  "package info: %s\n"  
    (Format.asprintf "%a" Js_packages_info.dump_packages_info npm_package_path)        
  ;
  pp_cmj_case  cmj_case;

  f "effect: %s\n"
      (match effect with 
       | None -> "pure"
       | Some s -> sprintf "None pure due to %s" s 
    );
  values |> String_map.iter 
    (fun k ({arity; persistent_closed_lambda} : Js_cmj_format.cmj_value) -> 
       match arity with             
       | Single arity ->
         f "%s: %s\n" k (Format.asprintf "%a" Lam_arity.print arity);
         (match persistent_closed_lambda with 
          | None -> 
            f "%s: not saved\n" k 
          | Some lam -> 
            begin 
              f "%s: ======[start]\n" k ;
              f "%s\n" (Lam_print.lambda_to_string lam);
              f "%s: ======[finish]\n" k
            end );
         
       | Submodule xs -> 
         (match persistent_closed_lambda with 
          | None -> f "%s: not saved\n" k 
          | Some lam -> 
            begin 
              f "%s: ======[start]\n" k ;
              f "%s" (Lam_print.lambda_to_string lam);
              f "%s: ======[finish]\n" k
            end 
         );
         Array.iteri 
         (fun i arity -> f "%s[%i] : %s \n" 
          k i 
          (Format.asprintf "%a" Lam_arity.print arity ))
         xs


    )



let () = 
  match Sys.argv  with
  | [|_; file |] 
    -> 
      let cmj,digest = (Js_cmj_format.from_file_with_digest file) in 
      Format.fprintf Format.std_formatter "@[Digest: %s@]@." (Digest.to_hex digest);
      pp_cmj Format.std_formatter cmj
  | _ -> failwith "expect one argument"