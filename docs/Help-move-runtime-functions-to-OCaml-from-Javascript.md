BuckleScript runtime implementation is currently a mix of OCaml and JavaScript. (jscomp/runtime directory). The JavaScript code is defined in the `.ml` file using the `bs.raw` syntax extension. 

The goal is to implement the runtime **purely in OCaml** and you can help contribute. 

Each new PR should include appropriate testing. 

Currently all tests are in `jscomp/test` directory and you should either add a new test file or modify an existing test which covers the part of the compiler you modified.

* Add the filename in `jscomp/test/test.mllib`

* Add a suite test

   The specification is in `jscomp/test/mt.ml`

   For example some simple tests would be like:

   ```ocaml
   let suites : _ Mt.pair_suites = Mt.[
       "hey", (fun _ -> Eq(true, 3 > 2));
       "hi", (fun _ ->  Neq(2,3);
       "hello", (fun _ -> Approx(3.0, 3.0));
       "throw", (fun _ -> ThrowAny(fun _ -> raise 3)) 
    ]
   let () = Mt.from_pair_suites __FILE__ suites
   ```

* Run the tests

   Suppose you have mocha installed, if not, try `npm install mocha`

   ```
   mocha -R list jscomp/test/your_test_file.js
   ```

* See the coverage

   ```
   npm run cover
   ```
