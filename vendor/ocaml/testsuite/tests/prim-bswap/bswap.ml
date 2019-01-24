open Printf

external bswap16: int -> int = "%bswap16"
external bswap32: int32 -> int32 = "%bswap_int32"
external bswap64: int64 -> int64 = "%bswap_int64"

let d16 = [0x11223344;
           0x0000f0f0]
let d32 = [0x11223344l;
           0xf0f0f0f0l]
let d64 = [0x1122334455667788L;
           0xf0f0f0f0f0f0f0f0L]

let _ =
  List.iter (fun x -> printf "%x\n" (bswap16 x)) d16;
  List.iter (fun x -> printf "%lx\n" (bswap32 x)) d32;
  List.iter (fun x -> printf "%Lx\n" (bswap64 x)) d64
