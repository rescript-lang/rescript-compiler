module ExtUnixAll = struct
 external unused : unit -> unit = "caml_blit_string"
 module BigEndian = struct
   let get_uint8 str off = 33
 end
end

module ExtUnix = struct
 module All = ExtUnixAll
end

module Test = struct
 open ExtUnix.All
 let test_endian_string x =
   let module B = BigEndian in
   B.get_uint8 x 0
 let v = test_endian_string 1
end
