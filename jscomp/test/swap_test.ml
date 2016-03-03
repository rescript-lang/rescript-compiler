external bswap16: int -> int = "%bswap16"
external bswap32: int32 -> int32 = "%bswap_int32"
external bswap64: int64 -> int64 = "%bswap_int64"


let tests_16 =
[|(1, 256); (2, 512); (4, 1024); (8, 2048); (16, 4096); (32, 8192);
  (64, 16384); (128, 32768); (256, 1); (512, 2); (1024, 4); (2048, 8);
  (4096, 16); (8192, 32); (16384, 64); (32768, 128)|]


let tests_32 = 
  [|(1l, 16777216l); (2l, 33554432l); (4l, 67108864l); (8l, 134217728l);
  (16l, 268435456l); (32l, 536870912l); (64l, 1073741824l);
  (128l, -2147483648l); (256l, 65536l); (512l, 131072l); (1024l, 262144l);
  (2048l, 524288l); (4096l, 1048576l); (8192l, 2097152l); (16384l, 4194304l);
  (32768l, 8388608l); (65536l, 256l); (131072l, 512l); (262144l, 1024l);
  (524288l, 2048l); (1048576l, 4096l); (2097152l, 8192l); (4194304l, 16384l);
  (8388608l, 32768l); (16777216l, 1l); (33554432l, 2l); (67108864l, 4l);
  (134217728l, 8l); (268435456l, 16l); (536870912l, 32l); (1073741824l, 64l);
  (-2147483648l, 128l)|]


let suites_16 = 
  tests_16 
  |> Array.to_list  
  |> List.map 
    (fun (a,b) -> (Printf.sprintf "swap16 %d" a,
                   fun _ -> Mt.Eq(bswap16 a , b)))
let suites_32 = 
  tests_32 
  |> Array.to_list
  |> List.map 
    (fun (a,b) -> (Printf.sprintf "swap32 %d" (Int32.to_int a),
                   fun _ -> Mt.Eq(bswap32 a , b)))

;; Mt.from_pair_suites __FILE__ 
  (suites_16 @ suites_32 )
