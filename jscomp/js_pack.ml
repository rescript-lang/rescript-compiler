let get_files dir = 
  Sys.readdir dir 
  |> Ext_array.filter_map 
    (fun  x -> if Ext_string.ends_with x ".cmj" then Some (Filename.concat dir x) else None )
  |> Array.to_list

let from_cmj files output_file = 
  let raw_to_str f str = 
    Ext_pp.string f "\""   ;
    Ext_pp.string f (Ext_string.escaped str);
    Ext_pp.string f "\""
  in  
  let v = open_out_bin output_file in
  Ext_pervasives.finally v (fun v ->   
      let f = Ext_pp.from_channel v in  
      let aux file = 
        let in_chan = open_in_bin file in
        let len = in_channel_length in_chan in
        let str = really_input_string in_chan len in
        begin
          Ext_pp.string f "(";
          raw_to_str f file ;
          Ext_pp.string f ",";
          prerr_endline 
            (Printf.sprintf "Begin Verifying %s" file);
          let _  = Js_cmj_format.from_string str in          
          prerr_endline "End";
          raw_to_str f str;         
          Ext_pp.string f ")";
          Ext_pp.string f  ";";
          Ext_pp.newline f ;          
          close_in in_chan;        
        end
      in
      Ext_pp.string f "(* -*-mode:fundamental-*- *)"  ;
      Ext_pp.newline f ;
      Ext_pp.string f "let cmj_data_sets = "    ;
      Ext_pp.bracket_vgroup f 1 (fun _ -> List.iter aux files))
    close_out      

let () = 
  from_cmj (get_files "stdlib" @ get_files "runtime") "js_cmj_datasets.ml";;  

