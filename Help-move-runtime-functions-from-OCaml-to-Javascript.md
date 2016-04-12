In the `jscomp/runtime` directory, there are lots of places where `js.raw` is used, we plan to implement all these stuff in ocaml (instead of javascript), it is appreciated if you can help move some js functions to ocaml itself.

Currently all tests are in `jscomp/test` directory, after you add a new file, 


* Add the filename in `jscomp/test/test.mllib`

* Add a  suite test

   The specification is in `jscomp/test/mt.ml`

   For example a simple tests would be like

   ```ocaml
   let suites : _ Mt.pair_suites = Mt.[
       "hey", (fun _ -> Eq(true, 3 > 2));
       "hi", (fun _ ->  Neq(2,3);
       "hello", (fun _ -> Approx(3.0, 3.0));
       "throw", (fun _ -> ThrowAny(fun _ -> raise 3)) 
    ]
   let () = Mt.from_pair_suites __FILE__ suites
   ```

* Run the test

   Suppose you have mocha installed, if not, try `npm install mocha`

   ```
   mocha -R list jscomp/test/your_test_file.js
   ```

* See the coverage


   ```
   npm run cover
   ```