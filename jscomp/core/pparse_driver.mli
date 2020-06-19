

val parse_implementation:
  Format.formatter ->
  tool_name:string ->    
  string -> Parsetree.structure


val parse_interface:   
  Format.formatter ->
  tool_name:string ->    
  string -> Parsetree.signature
