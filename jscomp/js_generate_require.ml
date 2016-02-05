
let std_files = 
  String_set.elements Js_config.stdlib_set
  |> List.map (fun x -> "./stdlib/" ^ Filename.chop_extension x ) 
let runtime_files = 
  String_set.elements Js_config.runtime_set 
  |> List.map (fun x -> "./runtime/" ^ Filename.chop_extension x) 

let () = 
  Ext_pervasives.with_file_as_chan "./pre_load.js" (fun chan -> 
      output_string chan (Printf.sprintf "require([%s], function(){})" 
                            (String.concat "," 
                               (List.map (Printf.sprintf "%S" ) (std_files @ runtime_files))))      
    )  
