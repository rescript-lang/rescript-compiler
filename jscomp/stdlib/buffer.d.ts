export var create: (n : any) => any ;
export var contents: (b : any) => any ;
export var to_bytes: (b : any) => any ;
export var sub: (b : any, ofs : any, len : any) => any ;
export var blit:
  (src : any, srcoff : any, dst : any, dstoff : any, len : any) => any ;
export var nth: (b : any, ofs : any) => any ;
export var length: (b : any) => any ;
export var clear: (b : any) => any ;
export var reset: (b : any) => any ;
export var add_char: (b : any, c : any) => any ;
export var add_string: (b : any, s : any) => any ;
export var add_bytes: (b : any, s : any) => any ;
export var add_substring: (b : any, s : any, offset : any, len : any) => any
  ;
export var add_subbytes: (b : any, s : any, offset : any, len : any) => any ;
export var add_substitute: (b : any, f : any, s : any) => any ;
export var add_buffer: (b : any, bs : any) => any ;
export var add_channel: (b : any, ic : any, len : any) => any ;
export var output_buffer: (oc : any, b : any) => any ;

