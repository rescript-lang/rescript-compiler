type  node = 
  | Dir of string *  node list 
  | File of string *  string


val root : node list 