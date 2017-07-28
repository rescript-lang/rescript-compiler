#use "internal_test_header.ml";;

let () = test "BasicNativeTree"
  ~options:[`no_ocamlfind]
  ~description:"Output tree for native compilation"
  ~tree:[T.f "dummy.ml"]
  ~matching:[M.Exact
                (_build
                   (M.lf
                      ["_digests";
                       "dummy.cmi";
                       "dummy.cmo";
                       "dummy.cmx";
                       "dummy.ml";
                       "dummy.ml.depends";
                       "dummy.native";
                       "dummy.o";
                       "_log"]))]
  ~targets:("dummy.native",[]) ();;

let () = test "BasicByteTree"
  ~options:[`no_ocamlfind]
  ~description:"Output tree for byte compilation"
  ~tree:[T.f "dummy.ml"]
  ~matching:[M.Exact
                (_build
                   (M.lf
                      ["_digests";
                       "dummy.cmi";
                       "dummy.cmo";
                       "dummy.ml";
                       "dummy.ml.depends";
                       "dummy.byte";
                       "_log"]))]
  ~targets:("dummy.byte",[]) ();;

let () = test "SeveralTargets"
  ~options:[`no_ocamlfind]
  ~description:"Several targets"
  ~tree:[T.f "dummy.ml"]
  ~matching:[_build (M.lf ["dummy.byte"; "dummy.native"])]
  ~targets:("dummy.byte",["dummy.native"]) ();;

let alt_build_dir = "BuIlD2";;

let () = test "BuildDir"
  ~options:[`no_ocamlfind; `build_dir alt_build_dir]
  ~description:"Different build directory"
  ~tree:[T.f "dummy.ml"]
  ~matching:[M.d alt_build_dir (M.lf ["dummy.byte"])]
  ~targets:("dummy.byte",[]) ();;

let tag_pat_msgs =
  ["*:a", "File \"_tags\", line 1, characters 0-2:\n\
           Lexing error: Invalid globbing pattern \"*\".";

   "\n<*{>:a", "File \"_tags\", line 2, characters 0-5:\n\
                Lexing error: Invalid globbing pattern \"<*{>\".";

   "<*>: ~@a,# ~a", "File \"_tags\", line 1, characters 10-11:\n\
                     Lexing error: Only ',' separated tags are alllowed."];;

List.iteri (fun i (content,failing_msg) ->
  let () = test (Printf.sprintf "TagsErrorMessage_%d" (i+1))
    ~options:[`no_ocamlfind]
    ~description:"Confirm relevance of an error message due to erronous _tags"
    ~failing_msg
    ~tree:[T.f "_tags" ~content; T.f "dummy.ml"]
    ~targets:("dummy.native",[]) ()
  in ()) tag_pat_msgs;;

let () = test "Itarget"
  ~options:[`no_ocamlfind]
  ~description:".itarget building with dependencies between the modules (PR#5686)"
  ~tree:[T.f "foo.itarget" ~content:"a.cma\nb.byte\n"; T.f "a.ml"; T.f "b.ml" ~content:"open A\n"]
  ~matching:[M.f "a.cma"; M.f "b.byte"]
  ~targets:("foo.otarget",[]) ();;

let () = test "PackAcross"
  ~options:[`no_ocamlfind]
  ~description:"Pack using a module from the other tree (PR#4592)"
  ~tree:[T.f "main.ml" ~content:"let _ = Pack.Packed.g ()\n";
         T.f "Pack.mlpack" ~content:"pack/Packed";
         T.f "_tags" ~content:"<lib>: include\n<pack/*.cmx>: for-pack(Pack)\n";
         T.d "lib" [T.f "Lib.ml" ~content:"let f()=()";
                    T.f "Lib.mli" ~content:"val f : unit -> unit"];
         T.d "pack" [T.f "Packed.ml" ~content:"let g() = Lib.f ()"]]
  ~matching:[M.f "main.byte"; M.f "main.native"]
  ~targets:("main.byte", ["main.native"])
  ();;

let () = test "PackAcross2"
  ~options:[`no_ocamlfind]
  ~description:"Pack using a module from the other tree (PR#4592)"
  ~tree:[T.f "a2.mli" ~content:"val f : unit -> unit";
         T.f "a2.ml" ~content:"let f _ = ()";
         T.f "lib.ml" ~content:"module A = A2";
         T.f "b.ml" ~content:"let g = Lib.A.f";
         T.f "sup.mlpack" ~content:"B";
         T.f "prog.ml" ~content:"Sup.B.g"]
  ~matching:[M.f "prog.byte"]
  ~targets:("prog.byte",[]) ();;

let () = test "PackAcross3"
  ~options:[`no_ocamlfind]
  ~description:"Pack using a module from the other tree (PR#4592)"
  ~tree:[T.d "foo" [ T.f "bar.ml" ~content:"let baz = Quux.xyzzy"];
         T.f "foo.mlpack" ~content:"foo/Bar";
         T.f "main.ml" ~content:"prerr_endline Foo.Bar.baz";
         T.f "myocamlbuild.ml";
         T.f "quux.ml" ~content:"let xyzzy = \"xyzzy\"";
         T.f "quux.mli" ~content:"val xyzzy : string"]
  ~matching:[M.f "main.byte"]
  ~targets:("main.byte",[]) ();;

let () = test "NativeMliCmi"
  ~options:[`no_ocamlfind; `ocamlc "toto" (*using ocamlc would fail*);
            `tags["native"]]
  ~description:"check that ocamlopt is used for .mli->.cmi \
                when tag 'native' is set (part of PR#4613)"
  ~tree:[T.f "foo.mli" ~content:"val bar : int"]
  ~matching:[_build [M.f "foo.cmi"]]
  ~targets:("foo.cmi",[]) ();;

let () = test "NoIncludeNoHygiene1"
  ~options:[`no_ocamlfind]
  ~description:"check that hygiene checks are only done in traversed directories\
                (PR#4502)"
  ~tree:[T.d "must_ignore" [ T.f "dirty.mli" ~content:"val bug : int"];
         T.f "hello.ml" ~content:"print_endline \"Hello, World!\"";
         T.f "_tags" ~content:"<must_ignore>: -traverse"]
  ~pre_cmd:"ocamlc -c must_ignore/dirty.mli"
            (* will make hygiene fail if must_ignore/ is checked *)
  ~targets:("hello.byte",[]) ();;

let () = test "NoIncludeNoHygiene2"
  ~options:[`no_ocamlfind; `build_dir "must_ignore"]
  ~description:"check that hygiene checks are not done on the -build-dir \
                (PR#4502)"
  ~tree:[T.d "must_ignore" [ T.f "dirty.mli" ~content:"val bug : int"];
         T.f "hello.ml" ~content:"print_endline \"Hello, World!\"";
         T.f "_tags" ~content:""]
  ~pre_cmd:"ocamlc -c must_ignore/dirty.mli"
            (* will make hygiene fail if must_ignore/ is checked *)
  ~targets:("hello.byte",[]) ();;

let () = test "NoIncludeNoHygiene3"
  ~options:[`no_ocamlfind; `X "must_ignore"]
  ~description:"check that hygiene checks are not done on excluded dirs (PR#4502)"
  ~tree:[T.d "must_ignore" [ T.f "dirty.mli" ~content:"val bug : int"];
         T.f "hello.ml" ~content:"print_endline \"Hello, World!\"";
         T.f "_tags" ~content:""]
  ~pre_cmd:"ocamlc -c must_ignore/dirty.mli"
            (* will make hygiene fail if must_ignore/ is checked *)
  ~targets:("hello.byte",[]) ();;

let () = test "OutputObj"
  ~options:[`no_ocamlfind]
  ~description:"output_obj targets for native and bytecode (PR #6049)"
  ~tree:[T.f "hello.ml" ~content:"print_endline \"Hello, World!\""]
  ~targets:("hello.byte.o",["hello.byte.c";"hello.native.o"]) ();;

let () = test "OutputShared"
  ~options:[`no_ocamlfind]
  ~description:"output_shared targets for native and bytecode (PR #6733)"
  ~tree:[T.f "hello.ml" ~content:"print_endline \"Hello, World!\"";
         T.f "_tags" ~content:"<*.so>: runtime_variant(_pic)"]
  ~targets:("hello.byte.so",["hello.native.so"]) ();;

let () = test "StrictSequenceFlag"
  ~options:[`no_ocamlfind; `quiet]
  ~description:"strict_sequence tag"
  ~tree:[T.f "hello.ml" ~content:"let () = 1; ()";
         T.f "_tags" ~content:"true: strict_sequence\n"]
  ~failing_msg:"File \"hello.ml\", line 1, characters 9-10:
Error: This expression has type int but an expression was expected of type
         unit\nCommand exited with code 2."
  ~targets:("hello.byte",[]) ();;

let () = test "StrictFormatsFlag"
  ~options:[`no_ocamlfind; `quiet]
  ~description:"strict_format tag"
  ~tree:[T.f "hello.ml" ~content:"let _ = Printf.printf \"%.10s\"";
         T.f "_tags" ~content:"true: strict_formats\n"]
  ~failing_msg:"File \"hello.ml\", line 1, characters 22-29:
Error: invalid format \"%.10s\": at character number 0, \
`precision' is incompatible with 's' in sub-format \"%.10s\"
Command exited with code 2."
  ~targets:("hello.byte",[]) ();;

let () = test "PrincipalFlag"
  ~options:[`no_ocamlfind; `quiet]
  ~description:"-principal tag"
  ~tree:[T.f "hello.ml"
            ~content:"type s={foo:int;bar:unit} type t={foo:int}
                      let f x = (x.bar; x.foo)";
         T.f "_tags" ~content:"true: principal\n"]
  ~failing_msg:"File \"hello.ml\", line 2, characters 42-45:
Warning 18: this type-based field disambiguation is not principal."
  ~targets:("hello.byte",[]) ();;

let () = test "ModularPlugin1"
  ~description:"test a plugin with dependency on external libraries"
  ~options:[`no_ocamlfind; `quiet; `plugin_tag "use_str"]
  ~tree:[T.f "main.ml" ~content:"let x = 1";
         T.f "myocamlbuild.ml" ~content:"ignore (Str.quote \"\");;"]
  ~matching:[M.f "main.byte"]
  ~targets:("main.byte",[]) ();;

let () = test "ModularPlugin2"
  ~description:"check that parametrized tags defined by the plugin \
                do not warn at plugin-compilation time"
  ~options:[`no_ocamlfind; `quiet]
  ~tree:[T.f "main.ml" ~content:"let x = 1";
         T.f "_tags" ~content:"<main.*>: toto(-g)";
         T.f "myocamlbuild.ml"
           ~content:"open Ocamlbuild_plugin;;
                     pflag [\"link\"] \"toto\" (fun arg -> A arg);;"]
  ~failing_msg:""
  ~matching:[M.f "main.byte"]
  ~targets:("main.byte",[]) ();;

let () = test "ModularPlugin3"
  ~description:"check that unknown parametrized tags encountered \
                during plugin compilation still warn"
  ~options:[`no_ocamlfind; `quiet; `plugin_tag "'toto(-g)'"]
  ~tree:[T.f "main.ml" ~content:"let x = 1";
         T.f "myocamlbuild.ml"
           ~content:"open Ocamlbuild_plugin;;
                     pflag [\"link\"] \"toto\" (fun arg -> A arg);;"]
  ~failing_msg:"Warning: tag \"toto\" does not expect a parameter, \
                but is used with parameter \"-g\""
  ~matching:[M.f "main.byte"]
  ~targets:("main.byte",[]) ();;

let () = test "PluginCompilation1"
  ~description:"check that the plugin is not compiled when -no-plugin is passed"
  ~options:[`no_ocamlfind; `no_plugin]
  ~tree:[T.f "main.ml" ~content:"let x = 1";
         T.f "myocamlbuild.ml" ~content:"prerr_endline \"foo\";;"]
  ~matching:[_build [M.Not (M.f "myocamlbuild")]]
  ~targets:("main.byte",[]) ();;

let () = test "PluginCompilation2"
  ~description:"check that the plugin is compiled when -just-plugin is passed"
  ~options:[`no_ocamlfind; `just_plugin]
  ~tree:[T.f "main.ml" ~content:"let x = 1";
         T.f "myocamlbuild.ml" ~content:"print_endline \"foo\";;"]
  ~matching:[_build [M.f "myocamlbuild"]]
  ~targets:("", []) ();;

let () = test "PluginCompilation3"
  ~description:"check that the plugin is not executed \
                when -just-plugin is passed"
  ~options:[`no_ocamlfind; `quiet; `just_plugin]
  ~tree:[T.f "main.ml" ~content:"let x = 1";
         T.f "myocamlbuild.ml" ~content:"print_endline \"foo\";;"]
  (* if the plugin were executed we'd get "foo" in failing_msg *)
  ~failing_msg:""
  ~targets:("main.byte", []) ();;

let () = test "PluginTagsWarning"
  ~description:"check that a warning is raised if -plugin-tags \
                is used without a plugin file"
  ~options:[`no_ocamlfind; `plugin_tag "use_str"]
  ~tree:[T.f "main.ml" ~content:""]
  ~matching:[_build [M.f "main.cmo"]]
  ~failing_msg:"Warning: option -plugin-tag(s) has no effect \
                in absence of plugin file \"myocamlbuild.ml\""
  ~targets:("main.ml", []) ();;

let () = test "TagsInNonHygienic"
  ~description:"Regression test for PR#6482, where a _tags \
                in a non-traversed directory would cause \
                ocamlbuild to abort"
  ~options:[`no_ocamlfind]
  ~tree:[
    T.f "main.ml" ~content:"";
    T.d "deps" [T.f "_tags" ~content:""];
    T.f "_tags" ~content:"<deps>: not_hygienic\n";
  ]
  ~matching:[M.f "main.byte"]
  ~targets:("main.byte",[]) ();;

let () = test "TagsNewlines"
  ~description:"Regression test for PR#6087 about placement \
                of newline-escaping backslashes"
  ~options:[`no_ocamlfind]
  ~tree:[
    T.f "main.ml" ~content:"";
    T.f "_tags" ~content:
"<foo>: debug,\\
rectypes
<bar>: \\
debug, rectypes
<baz>\\
: debug, rectypes
";
  ]
  ~matching:[M.f "main.byte"]
  ~targets:("main.byte",[]) ();;

let () = test "OpenTag"
  ~description:"Test the parametrized tag for the new -open feature"
  ~options:[`no_ocamlfind]
  ~tree:[
    T.f "test.ml" ~content:"let _ = map rev [ []; [3;2] ]";
    T.f "_tags" ~content: "<test.*>: open(List)";
  ]
  ~matching:[M.f "test.byte"]
  ~targets:("test.byte",[]) ();;

let () = test "OpenDependencies"
  ~description:"Test dependency computation for the new -open feature (PR#6584)"
  ~options:[`no_ocamlfind]
  ~tree:[
    T.f "a.ml" ~content:"let x = 1";
    T.f "b.ml" ~content:"print_int x; print_newline ()";
    T.f "_tags" ~content: "<b.*>: open(A)";
  ]
  ~matching:[M.f "b.byte"]
  ~targets:("b.byte",[]) ();;

run ~root:"_test_internal";;
