open Int64

let commutative_mul result a b = Mt.Eq((result, result), (mul a b, mul b a))

let pairs = [|(-6369178941122375200L, 7864955043652308640L, 710079015173034099L);
  (8962060020293135336L, 7543586149775142988L, 4850684189325050974L);
  (-9161542197404147928L, 899158197241158908L, 8795600396453302198L);
  (3358398961571564092L, 5342919210817653169L, 4145996795967701244L);
  (-6555402073887813520L, 3465340031200066536L, 4181191505921956774L);
  (-4523677527555012358L, 1276543650539578238L, 5812826113188151491L);
  (-6934468553649362855L, 7785647313097762325L, 7241346950344076853L);
  (715203802919571152L, 2516535866514393275L, 5125117419875854192L);
  (-6686627189857547459L, 2421045490088152053L, 6111768411715304745L);
  (9005046258567688786L, 6069658940756524865L, 2129423077748838354L);
  (4936631060534413696L, 2388637287307694757L, 7350192635126770560L);
  (3901525161500002519L, 4523532416698366291L, 9214381841065479661L);
  (4134697148227395296L, 6566552400989074310L, 8132227803860192208L);
  (-2592314958001559136L, 7915289622294445728L, 7964218148684228345L);
  (7848874678168215648L, 4580311104447448800L, 4313239158344332693L);
  (-7811641560663661522L, 1060947535036669291L, 6161652641757280778L);
  (7365507927037276951L, 4600709048432326175L, 3916052862740247561L);
  (1571446117134724284L, 5218915363866200678L, 4960910992197841962L);
  (-6830326887577503896L, 5309972098777932940L, 3743051712101331294L);
  (-5736531007213089380L, 659238999324606124L, 2014771822435221301L)|]


let from_pairs prefix pairs = 
    (Array.to_list @@ Array.mapi (fun i (result,a,b) ->
    ((("" ^ prefix) ^ "_") ^ (__unsafe_cast i) ) , fun _ -> commutative_mul result a b) pairs)

let small_pairs = 
  [| 121L, 11L, 11L;
    126736L, 356L, 356L;
     137176406L, 12346L,   11111L;
     0xffff_ffff_ff_00000L, 0xffff_ffff_ffL,  0x1_00000L;
     -0xffff_ffff_ff_00000L, -0xffff_ffff_ffL,  0x1_00000L;
     (0xd9e118894c02f614L, 0x4_8100_ef7b_b9df_cc5L, 0x6d2c_68b6_0e75_2704L);
   |]

let to_floats = 
  [|(1L, 1.); (2L, 2.); (4L, 4.); (8L, 8.); (16L, 16.); (32L, 32.); (64L, 64.);
  (128L, 128.); (256L, 256.); (512L, 512.); (1024L, 1024.); (2048L, 2048.);
  (4096L, 4096.); (8192L, 8192.); (16384L, 16384.); (32768L, 32768.);
  (65536L, 65536.); (131072L, 131072.); (262144L, 262144.);
  (524288L, 524288.); (1048576L, 1048576.); (2097152L, 2097152.);
  (4194304L, 4194304.); (8388608L, 8388608.); (16777216L, 16777216.);
  (33554432L, 33554432.); (67108864L, 67108864.); (134217728L, 134217728.);
  (268435456L, 268435456.); (536870912L, 536870912.);
  (1073741824L, 1073741824.); (2147483648L, 2147483648.);
  (4294967296L, 4294967296.); (8589934592L, 8589934592.);
  (17179869184L, 17179869184.); (34359738368L, 34359738368.);
  (68719476736L, 68719476736.); (137438953472L, 137438953472.);
  (274877906944L, 274877906944.); (549755813888L, 549755813888.);
  (1099511627776L, 1099511627776.); (2199023255552L, 2199023255552.);
  (4398046511104L, 4398046511104.); (8796093022208L, 8796093022208.);
  (17592186044416L, 17592186044416.); (35184372088832L, 35184372088832.);
  (70368744177664L, 70368744177664.); (140737488355328L, 140737488355328.);
  (281474976710656L, 281474976710656.); (562949953421312L, 562949953421312.);
  (1125899906842624L, 1125899906842624.);
  (2251799813685248L, 2251799813685248.);
  (4503599627370496L, 4503599627370496.);
  (9007199254740992L, 9007199254740992.);
  (18014398509481984L, 18014398509481984.);
  (36028797018963968L, 36028797018963968.);
  (72057594037927936L, 72057594037927936.);
  (144115188075855872L, 144115188075855872.);
  (288230376151711744L, 288230376151711744.);
  (576460752303423488L, 576460752303423488.);
  (1152921504606846976L, 1.15292150460684698e+18);
  (2305843009213693952L, 2.30584300921369395e+18);
  (4611686018427387904L, 4.6116860184273879e+18);
  (-9223372036854775808L, -9.22337203685477581e+18)|]

let check_complete_compare  = 
  [| 3L >= 2L; 
    3L >= 3L;
    3L = 3L; 
    2L =  2L; 
    2L < 3L; 
    3L > 2L; 
    2L <= 3L;
    3L <= 3L
   |]
(* let f () =  Random.float (2. ** 53. -. 1.);; *)
(* Array.init 40 (fun i -> let v = if i mod 2 = 0 then f () else -. f() in  v, of_float v);; *)
let of_float_pairs = [|(6853066956871844., 6853066956871844L);
  (-8507688874782117., -8507688874782117L);
  (4083117349607451., 4083117349607451L);
  (-4860723193745655., -4860723193745655L);
  (7820020192255542., 7820020192255542L);
  (-4908619721514532., -4908619721514532L);
  (5.67685864687671e+15, 5676858646876710L);
  (-703696191048023., -703696191048023L);
  (1123586534990153.88, 1123586534990153L);
  (-4.29886533981922e+15, -4298865339819220L);
  (2.43885138012066e+15, 2438851380120660L);
  (-8011538689499494., -8011538689499494L);
  (2710072285421155., 2710072285421155L);
  (-2541457347159789.5, -2541457347159789L);
  (5012932793576708., 5012932793576708L);
  (-943066847413899.125, -943066847413899L);
  (5440257518642004., 5440257518642004L);
  (-7750676773453898., -7750676773453898L);
  (8911999221747713., 8911999221747713L);
  (-1443906702582204.25, -1443906702582204L);
  (659345820712164.875, 659345820712164L);
  (-3284023713149006.5, -3284023713149006L);
  (5062818438579988., 5062818438579988L);
  (-8904450004162331., -8904450004162331L);
  (848261089308786., 848261089308786L);
  (-6376579516657391., -6376579516657391L);
  (1337907592605664.25, 1337907592605664L);
  (-8.54733738833896e+15, -8547337388338960L);
  (2345417644172927., 2345417644172927L);
  (-2587460670129294.5, -2587460670129294L);
  (4580431718597436., 4580431718597436L);
  (-1460576044874256.25, -1460576044874256L);
  (3403657978343579.5, 3403657978343579L);
  (-7.89068917321888e+15, -7890689173218880L);
  (1683098350604788.5, 1683098350604788L);
  (-3966538891560174.5, -3966538891560174L);
  (6726025288963652., 6726025288963652L);
  (-4790410747298403., -4790410747298403L);
  (1985858071337706.25, 1985858071337706L);
  (-5281733497873409., -5281733497873409L)|]

let simple_divs  = 
  [|
    6L, 3L, 2L, 0L ;     
    120L, 11L, 10L, 10L;
    (-9223372036854775808L, 2L, -4611686018427387904L, 0L);
    (* (-9223372036854775808L, 1L, -9223372036854775808L, 0L); *)
    (* (-9223372036854775808L, -1L, -9223372036854775808L, 0L); *)

    (4778496500726974625L, 8647273786691508695L, 0L, 4778496500726974625L);
    (4224455101328713966L, 4413625152776134247L, 0L, 4224455101328713966L);
    (3039065016695402626L, 1966341652797525449L, 1L, 1072723363897877177L);
    (15875542998768846L, 1302504952487197716L, 0L, 15875542998768846L);
    (8585447339548266970L, 8057707135347178550L, 1L, 527740204201088420L);
    (4824597144699038618L, 4627874521623171050L, 1L, 196722623075867568L);
    (3317928528506081476L, 6382249289790908051L, 0L, 3317928528506081476L);
    (6173196516403597348L, 4901593223366982622L, 1L, 1271603293036614726L);
    (6697226531800097631L, 906636418737384700L, 7L, 350771600638404731L);
    (3852836788009085824L, 248478100846466152L, 15L, 125665275312093544L);
    (7609910327523130308L, 4780671708774555207L, 1L, 2829238618748575101L);
    (7299174288081342090L, 4684233015000367062L, 1L, 2614941273080975028L);
    (5583983327073390199L, 1503074154703104122L, 3L, 1074760862964077833L);
    (7923869470984467163L, 608401886811934230L, 13L, 14644942429322173L);
    (3764804524848279959L, 7022863423816677941L, 0L, 3764804524848279959L);
    (6202937893449418431L, 6471550866002918077L, 0L, 6202937893449418431L);
    (930775821804680172L, 3611973664263654542L, 0L, 930775821804680172L);
    (1336298545857306662L, 6150533189497877492L, 0L, 1336298545857306662L);
    (3016153490384035887L, 1432506027675544583L, 2L, 151141435032946721L);
    (954001560234298007L, 6051824276569621768L, 0L, 954001560234298007L)
  |]
(* let f a b = a,b, div a b, rem a b;; *)

let from xs = 
  xs 
  |> Array.to_list 
  |> List.mapi (fun  i (a , b, c, d) -> 
      ("small_divs " ^ (__unsafe_cast i)), 
      (fun _ -> Mt.Eq ((c, d), (Int64.div a b, Int64.rem a b )  )))
let to_string = 
  [| 0L, "0"
  |]

let int64_compare_tests = 
  [| 
    1L, 2L, -1;
    2L, 1L, 1; 
    2L, 1L, 1
  |]

let from_compare xs = 
  xs 
  |> Array.to_list 
  |> List.mapi (fun i (a, b,c) -> 
      ("int64_compare " ^ (__unsafe_cast i)), 
      (fun _ -> Mt.Eq(c, Int64.compare a b))
    )

let from_to_string xs = 
  xs 
  |> Array.to_list 
  |> List.mapi (fun i (a, str_a) -> 
      ("to_string " ^ (__unsafe_cast i)),
      (fun _ -> Mt.Eq(str_a, Int64.to_string a))
    )

;; Mt.from_pair_suites __MODULE__ @@
   from_pairs "random" pairs @
   
   from_pairs "small" small_pairs @
   (to_floats |> Array.to_list |> List.mapi (fun i (i64, f) ->
        ("to_float_" ^ (__unsafe_cast i)), (fun _ ->  Mt.Eq(Int64.to_float i64, f))
      )) @
   (of_float_pairs |> Array.to_list |> List.mapi (fun i (f, i64) ->
        ("of_float_" ^ (__unsafe_cast i)), (fun _ -> Mt.Eq(Int64.of_float f, i64))
      ))   @
   [
     "compare_check_complete",
     (fun _ ->
        Mt.Eq
         (Array.map (fun _ ->  true) check_complete_compare,
           check_complete_compare))
   ]
   @ from simple_divs
   @ from_compare int64_compare_tests
   @ [
     "div_rem_0", (fun _ -> Eq(Int64.div (-1L) 16L,0L));     
     "div_rem_1", (fun _ -> Eq(Int64.rem (-1L) 16L,-1L));     
     (* __LOC__, (fun _ -> Eq(Int64.of_float 2e65, -9223372036854775808L)) *)
     __LOC__, (fun _ -> Eq(Int64.to_float Int64.max_int, 9.22337203685477581e+18))
   ]     
(*   
  Undefined behaviorJ

  (* Note in ocaml [Int64.of_float] is weird
   {[
     Int64.of_float 2.e65;;
     - : int64 = -9223372036854775808L
   ]}


   {[
     Int64.of_float (Int64.to_float (Int64.sub Int64.max_int 1L));;
     - : int64 = -9223372036854775808L
   ]}
   both overflow
*)
*)