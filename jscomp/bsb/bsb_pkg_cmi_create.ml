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


(* This would also create [cmis] directly 
  But it is a bit too intrusive currently
*)
let make package_name cunits = 
  let cmi_name = package_name in (* capital *)
  let cmi_sign : Types.signature = 
    cunits |> 
    List.map (fun (cunit : string) -> 
      Types.Sig_module(
        (Ident.create_persistent cunit),
        {md_type = 
          Types.Mty_alias (Path.Pident (Ident.create_persistent (package_name ^ "-" ^ cunit)))
        ; md_attributes = []; md_loc = Location.none}
       ,
       Trec_not 
      )
    ) in 
    let fname = (package_name ^ ".cmi") in 
    let ochan = open_out_bin fname in 
    let _digest : Digest.t = Cmi_format.output_cmi 
    fname ochan 
    {cmi_name ; cmi_sign;
     cmi_crcs = List.map (fun x -> x, None) cunits ; 
    cmi_flags = []} in
    close_out ochan

(* let () = 
  make "Pkg" ["A0"; "A1"]    *)