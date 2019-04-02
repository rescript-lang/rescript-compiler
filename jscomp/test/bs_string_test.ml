let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

module S = Belt.String

let () =
  eq __LOC__
  ("ghso ghso g"
  |. Js.String2.split " "
  |. Js.Array2.reduce (fun x y ->  x ^  "-" ^ y) ""
  ) "-ghso-ghso-g"

let () =
  eq __LOC__ (S.length "foo") 3;
  eq __LOC__ (S.length "") 0

let () =
  eq __LOC__ (S.get "foo" 1) (Some "o");
  eq __LOC__ (S.get "foo" 5) None

let () =
  eq __LOC__ (S.concat "foo" "bar") "foobar"

let () =
  eq __LOC__ (S.concatMany "foo" [|"bar"; "baz"|]) "foobarbaz";
  let array = [|"bar"; "baz"|] in
    eq __LOC__ (S.concatMany "foo" array) "foobarbaz";
  eq __LOC__ (S.concatMany "" [||]) ""

let () =
  eq __LOC__ (S.endsWith "foo" "oo") true;
  eq __LOC__ (S.endsWith "foo" "a") false

let () =
  eq __LOC__ (S.indexOf "foo" "oo") (Some 1);
  eq __LOC__ (S.indexOf "foo" "a") None

let () =
  eq __LOC__ (S.includes "foo" "oo") true;
  eq __LOC__ (S.includes "foo" "a") false

let () =
  eq __LOC__ (S.repeat "a" 3) "aaa";
  eq __LOC__ (S.repeat "a" 0) ""

let () =
  eq __LOC__ (S.replace "hello world" ~old: "world" ~by: "you") "hello you";
  eq __LOC__ (S.replace "hello world" ~old: "foo" ~by: "you") "hello world"

let () =
  eq __LOC__ (S.replaceRegex "hello world" ~old: [%re "/world/"] ~by: "you") "hello you";
  eq __LOC__ (S.replaceRegex "hello world world" ~old: [%re "/world/g"] ~by: "you") "hello you you";
  eq __LOC__ (S.replaceRegex "hello world" ~old: [%re "/foo/g"] ~by: "you") "hello world"

let () =
  (* the following test is broken down because the deepEqual fails as JS adds properties to the array *)
  eq __LOC__ (Belt.Option.isSome (S.matchRegex "hello world" [%re "/world/"])) true;
  eq __LOC__ (Belt.Option.map (S.matchRegex "hello world" [%re "/world/"]) (fun x -> Belt.Array.length x)) (Some 1);
  eq __LOC__ (Belt.Option.flatMap (S.matchRegex "hello world" [%re "/world/"]) (fun x -> Belt.Array.get x 0)) (Some "world");
  eq __LOC__ (S.matchRegex "hello world" [%re "/notfound/"]) None

let () =
  eq __LOC__ (S.split "hello world foo" " ") [|"hello"; "world"; "foo"|]

let () =
  eq __LOC__ (S.splitAtMost "hello world foo" " " 1) [|"hello"|]

let () =
  eq __LOC__ (S.startsWith "hello world" "hello") true;
  eq __LOC__ (S.startsWith "hello world" "world") false

let () =
  eq __LOC__ (S.substr "hello world" ~from: 1 ~len: 3) "ell"

let () =
  eq __LOC__ (S.substrToEnd "hello world" ~from: 1) "ello world";
  eq __LOC__ (S.substrToEnd "hello world" ~from: 11) ""

let () =
  eq __LOC__ (S.slice "hello world" ~from: 1 ~to_: 3) "el";
  eq __LOC__ (S.slice "hello world" ~from: 11 ~to_: 12) ""

let () =
  eq __LOC__ (S.sliceToEnd "hello world" ~from: 1) "ello world";
  eq __LOC__ (S.sliceToEnd "hello world" ~from: 11) ""

let () =
  eq __LOC__ (S.trim "  hello world   ") "hello world";
  eq __LOC__ (S.trim "\n\r\t hello world\n\r\t ") "hello world"

let () =
  eq __LOC__ (S.trimStart "  hello world   ") "hello world   ";
  eq __LOC__ (S.trimStart "\n\r\t hello world\n\r\t ") "hello world\n\r\t "

let () =
  eq __LOC__ (S.trimEnd "  hello world   ") "  hello world";
  eq __LOC__ (S.trimEnd "\n\r\t hello world\n\r\t ") "\n\r\t hello world"

let () =
  eq __LOC__ (S.padStart "4" 4 "x") "xxx4";
  eq __LOC__ (S.padStart "4444" 4 "x") "4444";
  eq __LOC__ (S.padStart "4" 4 "xy") "xyx4"

let () =
  eq __LOC__ (S.padEnd "4" 4 "x") "4xxx";
  eq __LOC__ (S.padEnd "4444" 4 "x") "4444";
  eq __LOC__ (S.padEnd "4" 4 "xy") "4xyx"

let () =
  eq __LOC__ (S.toLowerCase "HeLLo WorLd") "hello world"

let () =
  eq __LOC__ (S.toUpperCase "HeLLo WorLd") "HELLO WORLD"

let () = Mt.from_pair_suites __FILE__ !suites
