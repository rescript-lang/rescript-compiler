export var to_channel: any ;
export var to_buffer:
  (buff : any, ofs : any, len : any, v : any, flags : any) => any ;
export var from_channel: any ;
export var from_bytes: (buff : any, ofs : any) => any ;
export var from_string: (buff : any, ofs : any) => any ;
export var header_size: any ;
export var data_size: (buff : any, ofs : any) => any ;
export var total_size: (buff : any, ofs : any) => any ;

