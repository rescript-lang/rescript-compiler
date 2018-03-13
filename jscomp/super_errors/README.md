Hello! This is the subdirectory for the new, newcomer-friendly OCaml/Reason warning & error report system. Most of the logic are lifted from the compiler (https://github.com/BuckleScript/ocaml/tree/master). The convention here is to have a `super_foo` for each corresponding compiler's file `foo`. So, for example, `warnings.ml` becomes `super_warnings.ml`. The exception is `super_main`, the entry point, `super_reason_react`, our special handling of [ReasonReact](https://reasonml.github.io/reason-react/) errors, and `super_reason_flag`.

Feel free to submit new ones or tweak existing messages in these files! They also have more precise comments in them that tells you how they work.

### Develop

Please see [CONTRIBUTING.md](../../CONTRIBUTING.md) for the build & testing setup.

### Reason-specific Tweaks

Super-errors caters to both OCaml and Reason files' errors. Some logic are specific to each:

- The error type/value output syntax from the compiler
- Some error messages' wording and solutions

The first one is controlled by the `-bs-re-out` flag in the compiler (see `js_main.ml`). This flag is passed to the compiler by the build system's rule for compiling Reason files. The flag triggers the setup of the Reason outcome printer (`Reason_outcome_printer_main.ml`), which overrides OCaml's error printing logic to output in Reason syntax instead of the default OCaml syntax. So really, even though this process seems to be super-errors-specific, it's not.

The second one is controlled by `super_reason_flag`. Some error messages' recommended fix are OCaml/Reason-specific, so they need to know whether for which syntax we're outputting the message. The flag `Super_reason_flag.using_reason_syntax` indicates this. It's set inside `-bs-re-out` too.

#### SuperErrors-specific Tests Flow

Note: currently you can't test things with external libraries (e.g. ReasonReact).

The fixture tests are located in `jscomp/build_tests/super_errors/` and look like:
```
{some code}
/*
{the normal ocaml error output}

=====

{the supererrors output}
*/

{some more code}
/*
etc
*/
```

Files in `formattingTests` get printed with `-colors always` so we can test formatting. The other ones are printed with `-colors never` so that it's readable.

To add a new test case, add your code to the end of a file, and run `jscomp/build_tests/super_errors/rebuild.sh`. The output will be appended.

To test the changes on a dummy project, see [here](https://github.com/BuckleScript/bucklescript/blob/master/CONTRIBUTING.md#test-on-a-dummy-project).
