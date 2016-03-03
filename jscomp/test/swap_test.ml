external bswap16: int -> int = "%bswap16"
external bswap32: int32 -> int32 = "%bswap_int32"
external bswap64: int64 -> int64 = "%bswap_int64"


let tests_16 =
[(1, 256); (2, 512); (4, 1024); (8, 2048); (16, 4096); (32, 8192);
  (64, 16384); (128, 32768); (256, 1); (512, 2); (1024, 4); (2048, 8);
  (4096, 16); (8192, 32); (16384, 64); (32768, 128)]

let suites_16 = 
  tests_16 
  |> List.map 
    (fun (a,b) -> (Printf.sprintf "swap16 %d" a,
                   fun _ -> Mt.Eq(bswap16 a , b)))

;; Mt.from_pair_suites __FILE__ suites_16
