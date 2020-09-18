Hello! This is the subdirectory for the new, newcomer-friendly OCaml/Reason warning & error report system. Most of the logic are lifted from the compiler (https://github.com/rescript-lang/ocaml/tree/master). The convention here is to have a `super_foo` for each corresponding compiler's file `foo`. So, for example, `warnings.ml` becomes `super_warnings.ml`. The exception is `super_main`, the entry point.

Feel free to submit new ones or tweak existing messages in these files! They also have more precise comments in them that tells you how they work.

### Develop

Please see [CONTRIBUTING.md](../../CONTRIBUTING.md) for the build & testing setup.

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

To add a new test case, see `jscomp/build_tests/super_errors/README.md`.

To test the changes on a dummy project, see "Test on a Dummy Project" in [CONTRIBUTING.md](../../CONTRIBUTING.md).
