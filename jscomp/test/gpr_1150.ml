let f children =
  match children with
  | [] -> [||]
  | a0 :: children -> (
    match children with
    | [] -> [|a0|]
    | a1 :: children -> (
      match children with
      | [] -> [|a0; a1|]
      | a2 :: children -> (
        match children with
        | [] -> [|a0; a1; a2|]
        | a3 :: children -> (
          match children with
          | [] -> [|a0; a1; a2; a3|]
          | a4 :: children -> (
            match children with
            | [] -> [|a0; a1; a2; a3; a4|]
            | a5 :: children -> (
              match children with
              | [] -> [|a0; a1; a2; a3; a4; a5|]
              | a6 :: children -> (
                match children with
                | [] -> [|a0; a1; a2; a3; a4; a5; a6|]
                | a7 :: children -> (
                  match children with
                  | [] -> [|a0; a1; a2; a3; a4; a5; a6; a7|]
                  | a8 :: children -> (
                    match children with
                    | [] -> [|a0; a1; a2; a3; a4; a5; a6; a7; a8|]
                    | a9 :: children -> (
                      match children with
                      | [] -> [|a0; a1; a2; a3; a4; a5; a6; a7; a8; a9|]
                      | a10 :: children -> (
                        match children with
                        | [] -> [|a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10|]
                        | a11 :: children -> (
                          match children with
                          | [] ->
                              [| a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10
                               ; a11 |]
                          | a12 :: children -> (
                            match children with
                            | [] ->
                                [| a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10
                                 ; a11; a12 |]
                            | a13 :: children -> (
                              match children with
                              | [] ->
                                  [| a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10
                                   ; a11; a12; a13 |]
                              | a14 :: children -> (
                                match children with
                                | [] ->
                                    [| a0; a1; a2; a3; a4; a5; a6; a7; a8; a9
                                     ; a10; a11; a12; a13; a14 |]
                                | a15 :: children -> (
                                  match children with
                                  | [] ->
                                      [| a0; a1; a2; a3; a4; a5; a6; a7; a8; a9
                                       ; a10; a11; a12; a13; a14; a15 |]
                                  | a16 :: children -> assert false ) ) ) ) ) )
                      ) ) ) ) ) ) ) ) ) )

(* match children with | [] ->
   [|a0;a1;a2;a3;a4;a5;a6;a7;a8;a9;a10;a11;a12;a13;a14;a15;a16|] |
   a17::children -> match children with | [] ->
   [|a0;a1;a2;a3;a4;a5;a6;a7;a8;a9;a10;a11;a12;a13;a14;a15;a16;a17|] |
   a18::children -> match children with | [] ->
   [|a0;a1;a2;a3;a4;a5;a6;a7;a8;a9;a10;a11;a12;a13;a14;a15;a16;a17;a18|] |
   a19::children -> match children with | [] ->
   [|a0;a1;a2;a3;a4;a5;a6;a7;a8;a9;a10;a11;a12;a13;a14;a15;a16;a17;a18;a19|] |
   _ -> assert false *)

(** This function seems to trigger quadratice behavior with the length of
    [children] memory consumption is okay 17 -> 4s 18 -> 9s 19 -> 19s 20 -> 40s *)

(* let f children = match children with | [] -> [||] | a::[] -> [|a|] |
   a::b::[] -> [|a;b|] | a::b::c::[] -> [|a;b;c|] | a::b::c::d::[] ->
   [|a;b;c;d|] | a::b::c::d::e::[] -> [|a;b;c;d;e|] | a::b::c::d::e::f::[] ->
   [|a;b;c;d;e;f|] | a::b::c::d::e::f::g::[] -> [|a;b;c;d;e;f;g|] |
   a::b::c::d::e::f::g::h::[] -> [|a;b;c;d;e;f;g;h|] |
   a::b::c::d::e::f::g::h::i::[] -> [|a;b;c;d;e;f;g;h;i|] |
   a::b::c::d::e::f::g::h::i::j::[] -> [|a;b;c;d;e;f;g;h;i;j|] |
   a::b::c::d::e::f::g::h::i::j::k::[] -> [|a;b;c;d;e;f;g;h;i;j;k|] |
   a::b::c::d::e::f::g::h::i::j::k::l::[] ->

   [|a;b;c;d;e;f;g;h;i;j;k;l|] | a::b::c::d::e::f::g::h::i::j::k::l::m::[] ->

   [|a;b;c;d;e;f;g;h;i;j;k;l;m|] | a::b::c::d::e::f::g::h::i::j::k::l::m::n::[]
   ->

   [|a;b;c;d;e;f;g;h;i;j;k;l;m;n|] |
   a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::[] ->

   [|a;b;c;d;e;f;g;h;i;j;k;l;m;n;o|] |
   a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::[] ->

   [|a;b;c;d;e;f;g;h;i;j;k;l;m;n;o;p|] |
   a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::[] ->

   [|a;b;c;d;e;f;g;h;i;j;k;l;m;n;o;p;q|] |
   a0::a1::a2::a3::a4::a5::a6::a7::a8::a9::a10::a11::a12::a13::a14::a15::a16::a17::[]
   ->

   [|a0;a1;a2;a3;a4;a5;a6;a7;a8;a9;a10;a11;a12;a13;a14;a15;a16;a17|] |
   a0::a1::a2::a3::a4::a5::a6::a7::a8::a9::a10::a11::a12::a13::a14::a15::a16::a17::a18::[]
   ->

   [|a0;a1;a2;a3;a4;a5;a6;a7;a8;a9;a10;a11;a12;a13;a14;a15;a16;a17;a18|] |
   a0::a1::a2::a3::a4::a5::a6::a7::a8::a9::a10::a11::a12::a13::a14::a15::a16::a17::a18::a19::[]
   ->

   [|a0;a1;a2;a3;a4;a5;a6;a7;a8;a9;a10;a11;a12;a13;a14;a15;a16;a17;a18;a19|]

   | _ -> let msg = "Reason allows up to 17 static children (but dynamic
   children in an array can be unlimited in size); You have " ^ ((string_of_int
   (List.length children)) ^ ", please put them in an array and assign key to
   the elements. Sorry for the inconvenience!") in raise ((Invalid_argument
   (msg))[@explicit_arity ]) *)
