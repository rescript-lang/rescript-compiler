

module Res = struct
    type  node = 
        | Dir of string *  node list 
        | File of string *  string

end    


