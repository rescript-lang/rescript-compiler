let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

;;
Js.log {js|你好，
世界|js}
;;

Js.log {js|\x3f\u003f\b\t\n\v\f\r\0"'|js}
;;
let convert (s : string) : int list  =
    Js_array2.fromMap
        (Js_string.castToArrayLike s)
        (fun x ->
        match Js_string.codePointAt 0 x with
        | None -> assert false
        | Some x -> x ) |> Array.to_list

let () =
    begin
    eq __LOC__ {js|你好，
世界|js} {js|你好，\n世界|js};
    eq __LOC__
     (convert {js|汉字是世界上最美丽的character|js} )
 [27721;
  23383;
  26159;
  19990;
  30028;
  19978;
  26368;
  32654;
  20029;
  30340;
  99;
  104;
  97;
  114;
  97;
  99;
  116;
  101;
  114 ];
  eq __LOC__ (convert {js|\x3f\x3fa|js})
  [63;63;97];
  eq __LOC__ (convert {js|??a|js})
  [63;63;97];
  eq __LOC__ (convert {js|\u003f\x3fa|js})
  [63;63;97];
  eq __LOC__ (convert {js|🚀🚀a|js})
  [128640;128640;97];
  eq __LOC__ (convert {js|\uD83D\uDE80a|js})
  [128640; 97];
  eq __LOC__ (convert {js|\uD83D\uDE80\x3f|js})
  [128640; 63];

  (* It is amazing Array.from(string)
    is unicode safe *)
  eq __LOC__ (convert {js|\uD83D\uDE80\uD83D\uDE80a|js})
  [128640; 128640; 97];

  eq "No inline string length" (String.length {js|\uD83D\uDE80\0|js}) 3;
  (** should not optimize
    Note in JS, the length is also not unicode safe
   *)
  (* eq __LOC__
    (Js.String.codePointAt 0 {js|\uD83D\uDE80\0|js} ) 128640; *)
   eq "No inline string access"
    (Char.code {js|\uD83D\uDE80\0|js}.[0] land 255)
    (*TODO: Char.code need normalization? *)
    61;
  (* "\uD83D\uDE80".charCodeAt(0) & 255
  61   *)
  (** Note that [char] maximum is 255  *)
  eq __LOC__ (convert {js|\uD83D\uDE80|js})
  [128640];
  eq __LOC__ (convert {js|\uD83D\uDE80\uD83D\uDE80|js}) [128640;128640];
  eq __LOC__ (convert {js| \b\t\n\v\f\ra|js})
    [ 32; 8; 9; 10; 11; 12; 13; 97];
  (* we don't need escape string double quote {|"|}and single quote{|'|}
    however when we print it, we need escape them
    there is no need for line continuation,

  *)
   eq __LOC__ (convert {js| \b\t\n\v\f\r"'\\\0a|js})
  [ 32; 8; 9; 10; 11; 12; 13; 34; 39; 92;0 ;97]
    end
let () = Mt.from_pair_suites __MODULE__ !suites