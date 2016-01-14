let get_files dir = 
  Sys.readdir dir 
  |> Ext_array.filter_map (fun  x -> if Ext_string.ends_with x ".cmj" then Some (Filename.concat dir x) else None )
  |> Array.to_list

let from_cmj files output_file = 
  let raw_to_str chan str = 
    output_string  chan "\""   ;
    output_string chan (Ext_string.escaped str);
    output_string chan "\""
  in  
  let v = open_out_bin output_file in
  let aux file = 
    let in_chan = open_in_bin file in
    let len = in_channel_length in_chan in
    let str = really_input_string in_chan len in
    begin
      output_string v "(";
      raw_to_str v file ;
      output_string v ",";
      raw_to_str v str;         
      output_string v ")";
      output_string v ";";
      close_in in_chan;        
    end
  in
  output_string v "let cmj_data_sets = "    ;
  output_string v "[";
  List.iter aux files ;
  output_string v "]";
  close_out v     

let () = 
  from_cmj (get_files "stdlib" @ get_files "runtime") "js_cmj_datasets.ml";;  

