let ((>::),
     (>:::)) = OUnit.((>::),(>:::))


let normalize = Ext_filename.normalize_absolute_path
let (=~) x y =
  OUnit.assert_equal ~cmp:(fun x y ->   Ext_string.equal x y ) x y

let suites =
  __FILE__
  >:::
  [
    "linux path tests" >:: begin fun _ ->
      let norm =
        Array.map normalize
          [|
            "/gsho/./..";
            "/a/b/../c../d/e/f";
            "/a/b/../c/../d/e/f";
            "/gsho/./../..";
            "/a/b/c/d";
            "/a/b/c/d/";
            "/a/";
            "/a";
            "/a.txt/";
            "/a.txt"
          |] in
      OUnit.assert_equal norm
        [|
          "/";
          "/a/c../d/e/f";
          "/a/d/e/f";
          "/";
          "/a/b/c/d" ;
          "/a/b/c/d";
          "/a";
          "/a";
          "/a.txt";
          "/a.txt"
        |]
    end;
    __LOC__ >:: begin fun _ ->
      normalize "/./a/.////////j/k//../////..///././b/./c/d/./." =~ "/a/b/c/d"
    end;
    __LOC__ >:: begin fun _ ->
      normalize "/./a/.////////j/k//../////..///././b/./c/d/././../" =~ "/a/b/c"
    end;

    "windows path tests" >:: begin fun _ ->
      let norm =
        Array.map normalize
          [|
            "c:/gsho/./..";
            "d:/a/b/../c../d/e/f";
            "e:/a/b/../c/../d/e/f";
            "f:/gsho/./../..";
            "g:/a/b/c/d";
            "h:/a/b/c/d/";
            "i:/a/";
            "j:/a";
            "k:/a.txt/";
            "abc:/a.txt"
          |] in
      OUnit.assert_equal norm
        [|
          "c:/";
          "d:/a/c../d/e/f";
          "e:/a/d/e/f";
          "f:/";
          "g:/a/b/c/d" ;
          "h:/a/b/c/d";
          "i:/a";
          "j:/a";
          "k:/a.txt";
          "abc:/a.txt"
        |]
    end;
    __LOC__ >:: begin fun _ ->
      normalize
        "l:/./a/.////////j/k//../////..///././b/./c/d/./." =~ "l:/a/b/c/d"
    end;
    __LOC__ >:: begin fun _ ->
      normalize
        "m:/./a/.////////j/k//../////..///././b/./c/d/././../" =~ "m:/a/b/c"
    end;

    __LOC__ >:: begin fun _ ->
    let aux a b result =

         Ext_filename.rel_normalized_absolute_path
        a b =~ result ;

        Ext_filename.rel_normalized_absolute_path
        (String.sub a 0 (String.length a - 1))
        b  =~ result ;

        Ext_filename.rel_normalized_absolute_path
        a
        (String.sub b 0 (String.length b - 1))  =~ result
        ;


        Ext_filename.rel_normalized_absolute_path
        (String.sub a 0 (String.length a - 1 ))
        (String.sub b 0 (String.length b - 1))
        =~ result
       in
      aux
        "/a/b/c/"
        "/a/b/c/d/"  "d";
      aux
        "/a/b/c/"
        "/a/b/c/d/e/f/" "d/e/f" ;
      aux
        "/a/b/c/d/"
        "/a/b/c/"  ".."  ;
      aux
        "/a/b/c/d/"
        "/a/b/"  "../.."  ;
      aux
        "/a/b/c/d/"
        "/a/"  "../../.."  ;
      aux
        "/a/b/c/d/"
        "//"  "../../../.."  ;


    end;
    (* This is still correct just not optimal depends
      on user's perspective *)
    __LOC__ >:: begin fun _ ->
      Ext_filename.rel_normalized_absolute_path
        "/a/b/c/d"
        "/x/y" =~ "../../../../x/y"

    end;

    __LOC__ >:: begin fun _ ->
    Ext_filename.rel_normalized_absolute_path
    "/usr/local/lib/node_modules/"
    "//" =~ "../../../..";
    Ext_filename.rel_normalized_absolute_path
    "/usr/local/lib/node_modules/"
    "/" =~ "../../../.."
    end;

  ]
