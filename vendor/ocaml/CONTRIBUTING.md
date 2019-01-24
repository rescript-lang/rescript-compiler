# How to contribute changes

:+1::tada: First off, thank you for taking time to contribute! :tada::+1:

The following is a set of guidelines for proposing changes to the
OCaml distribution. These are just guidelines, not rules, use your
best judgment and feel free to propose changes to this document itself
in a pull request.

This document assumes that you have a patch against the sources of the
compiler distribution, that you wish to submit to the OCaml
maintainers upstream. See [INSTALL.adoc](INSTALL.adoc) for details on
how to build the compiler distribution from sources. See
[HACKING.adoc](HACKING.adoc) for details on how to modify the sources.

## Contribution

Modifying its sources is far from the only way to contribute to the
OCaml distribution. Bug reports (in particular when they come with
a reproducible example), simple typos or clarifications in the
documentation also help, and help evaluating and integrating existing
change proposals also help. Providing good answers on the discussion
forums, or asking the good questions that highlight deficiencies in
existing documentations, also help. We currently have more
contributors willing to propose changes than contributors willing to
review other people's changes, so more eyes on the existing change
requests is a good way to increase the integration bandwidth of
external contributions.

There are also many valuable ways to contribute to the wider OCaml
ecosystem that do not involve changes to the OCaml distribution.

The rest of the document is concerned with the form of change
proposals against the OCaml distribution. (Code changes, but also
improvement to documentation or implementation comments, which are
valuable changes on their own.)

## Workflow

All changes to the OCaml distribution need to be processed through the
GitHub Pull Request (PR) system.  In order to propose a change, a
contributor thus needs to have a GitHub account, fork the ocaml/ocaml
repository, create a branch for the proposal on their fork and submit
it as a Pull Request on the upstream repository.  (If you are not yet
familiar with GitHub, don't worry, all these steps are actually quite
easy!)

The current rule is that a PR needs to get an explicit approval from
one of the core maintainer in order to be merged.  Reviews by
external contributors are very much appreciated.

Since core maintainers cannot push directly without going through an
approved PR, they need to be able to apply small changes to the
contributed branches themselves.  Such changes include fixing
conflicts, adjusting a Changelog entry, or applying some code changes
required by the reviewers.  Contributors are thus strongly advised to
check the [**Allow edits from maintainer**](
https://help.github.com/articles/allowing-changes-to-a-pull-request-branch-created-from-a-fork/
) flag on their PRs in the GitHub interface.  Failing to do so might
significantly delay the inclusion of an otherwise perfectly ok
contribution.


## Coding guidelines

You should not leave trailing whitespace; not have line longer than 80
columns, not use tab characters (spaces only), and not use non-ASCII
characters. These typographical rules can be checked with the script
`tools/check-typo`.

Otherwise, there are no strongly enforced guidelines specific to the
compiler -- and, as a result, the style may differ in the different
parts of the compiler. The general [OCaml Programming
Guidelines](https://ocaml.org/learn/tutorials/guidelines.html) are
good to keep in mind, and otherwise we strive for good taste and local
consistency (following the code located around your change).

If you strongly feel that a style-related change would improve quality
of the existing code (for example, giving more descriptive names to
some variables throughout a module, factoring repeated code patterns
as auxiliary functions, or adding comments to document a part of the
code that you had trouble understanding), you can have code cleanup
commits at the beginning of your patch series, or submit code cleanups
as your change proposal. Those cleanups should remain separate commits
from the functional changes in the rest of the patch series; it is
easier to review commits that are specifically marked as exactly
preserving the code semantics.


## Test you must.

Whenever applicable, merge requests must come with tests
exercising the affected features: regression tests for bug fixes,
and correctness tests for new features (including corner cases and
failure cases). For regression tests, testing other aspects of the
feature (in particular, related edge cases) that are not currently
covered is a good way to catch other instances of bugs -- this did
happen several times in the past. Warnings and errors should also
be tested.

Tests go in the sub-directories of `testsuite/tests`. Running
`make all` in `testsuite/` runs all tests (this takes
a few minutes), and you can use `make one DIR=tests/foo` to run
the tests of a specific sub-directory. There are many kind of tests
already, so the easiest way to start is to extend or copy an
existing test.

In general, running a test produces one (or several) `.result` file,
that are compared to one (or several) `.reference` file present in the
repository; the test succeeds if they are identical. If your patch
breaks a test, diffing the `.result` and `.reference` file is a way to
see what went wrong. Some reasonable compiler changes affect the
compiler output in way that make those outputs differ (for example
slight modifications of warning or error messages may break all tests
checking warnings). If you are positive that the new `.result` file
is correct (and that the change in behavior does not endanger
backward compatibility), you can replace the old `.reference` file
with it. Finally, when adding new tests, do not forget to include your
`.reference` files (but not `.result`) in the versioned repository.

Testing is also a way to make sure reviewers see working
(and failing) examples of the feature you fix, extend or
introduce, rather than just an abstract description of it.


### Run tests before sending a PR

You should run all the tests before creating the merge request or
pushing new commits (even if Travis will also do it for you): `make
tests` (this takes a few minutes).

Unfortunately some of the `lib-threads` test are non-deterministic
and fail once in a while (it's hard to test these well). If they
consistently break after your change, you should investigate, but if
you only see a transient failure once and your change has no reason
to affect threading, it's probably not your fault.


## Description of the proposed change

### In the merge request interface

The description of the merge request must contain a precise
explanation of the proposed change.

Before going in the implementation details, you should include
a summary of the change, and a high-level description of the design
of the proposed change, with example use-cases.

### In the patches

If some of the explanations you provide for the merge request would
make sense as comments in the code, or documentation in the manual,
you should include them there as well.

In-code comments help make the codebase more accessible to newcomers
(many places in the compiler could benefit from a few
extra explanations), and they are also useful to code reviewers. In
particular, any subtlety in code that cannot be made
self-explanatory should come with an explanation in comment. If you
add some non-obvious code specifically to fix a bug, include the
issue number in comments.

Do not assume that code reviewers are all experts in the existing
codebase. If you use subtle code, add a comment, even if the same
kind of code is used somewhere else in the same module. (If this is
a common and useful domain-specific idiom that is already explained
somewhere, pointing to this explanation in your commit message is
better than adding redundant explanations.)

### User documentation

Changes affecting the compiler libraries should be reflected in the
documentation comments of the relevant `.mli` files.

It is recommended to included changes to the OCaml Reference Manual
(in particular for any change in the surface language), which is now
part of the main repository (under `manual/`).

Finally, changes in command-line options should be integrated in the
manual, but also in the man pages present in the `man/` sub-directory
of the OCaml distribution.

### Changelog

Any user-visible change should have a `Changes` entry:

- in the right section (named sections if major feature, generic
  "Bug fixes" and "Feature requests" otherwise)

- using the label "`*`" if it breaks existing programs, "`-`" otherwise

- with the issue number `PR#{N}` if from mantis, `GPR#{N}` if from github
  (several numbers separated by commas can be used)

- maintaining the order: each section lists Mantis PRs first in ascending
  numerical order, followed by Github PRs in ascending numerical order,
  followed by changes that are not related to a PR.

- with a concise readable description of the change (possibly taken
  from a commit message, but it should make sense to end-users
  reading release notes)

- crediting the people that worked on the feature. The people that
  wrote the code should be credited of course, but also substantial
  code reviews or design advice, and the reporter of the bug
  (if applicable) or designer of the feature request (if novel).

- following the format

        {label} {issue number(s)}: {readable description}
                ({credits})

      note that the `{credits}` should be on their own line, aligned with the
      issue number for readability
      (`{readable description}` can be multiline to not overflow 80
      columns, and should be aligned with the issue number as well.)

This changelog can be included in the main commit, if the merge
request is just one patch, or as a separate commit, if it's
a patch series and no particular commit feels best suited to
receive the Changelog entry.

(Do not under-estimate the importance of a good changelog. Users do
 read the release notes, and things forgotten from the changelog
 will cause pain or regrets down the line.)


## Clean patch series

Clean patch series are useful, both during the review process and
for code maintenance after it has been merged. Before submitting
your request, you should rebase your patch series:

- on top of the OCaml branch in which you want to merge
  (usually `trunk`), solving any conflicts.

- into a few well-separated, self-contained patches (github PRs
  can generate gazillions of micro-changes)

- erasing history that does not make sense after the issue is merged
  (back-and-forth between different designs, etc. The PR number
  allows interested people to go back to the original discussion if
  needed.)

- bisectable: the distribution should be in a good state after
  the application of each patch (in particular, later commits that
  fix bugs in previous commits should always be squashed into the commit
  they fix)

- with readable commit messages (this is for future developers
  needing to understand a change that happened in the past). Commit
  messages should not overflow 80 columns, with the following format:

        {one-liner header description (with issue number if applicable)}
        {blank line}
        {one or several paragraphs of explanation if needed}

During review, you may make many other changes to the patch
series. You can rebase it on the fly (if you `git push -f` on the
branch of the pull request in your personal clone, Github will
update the pull request automatically; remember to always create
a new branch for any) or wait until the discussion has converged,
once we agree the request is ready for merging. Doing a good
rebase is grunt work that takes some time and care (use `git
log -u` to make sure the rebase patches make sense), but:

- It is easier and faster to do for the author of the patch than
  for others (if rebasing against the current trunk creates
  a conflict with another change you don't understand well, feel
  free to ask).

- Maintainers are usually short on time, and asking them to do
  a rebase means they have less time to review and merge other
  contributions.

- The long-term benefits of keeping a clean, bisectable history
  cannot be overstated. Imagine that in three years, under the
  pressure of a coming release, a contributor ends up somewhere in
  the middle of your patch series, wondering if or why it is the
  cause of a specific issue. Wasting his or her time then
  (with a "yolo" commit message, a big ugly commit of unrelated
  changes, or an un-testable intermediary state) is a sure way to
  generate ill will.

## Contributing to the standard library

Contributions to the standard library are very welcome.  There is some
widespread belief in the community than the stdlib is somehow "frozen"
and that its evolutions are mostly driven by the need of the OCaml
compiler itself.  Let's be clear: this is just plain wrong. The
compiler is happy with its own local utility functions, and many
recent additions to the stdlib are not used by the compiler.

Another common and wrong idea is that core OCaml maintainers don't
really care about the standard library.  This is not true, and won't
be unless one of the "alternative standard" libraries really gains
enough "market share" in the community.

So: please contribute!

Obviously, the proposals to evolve the standard library will be
evaluated with very high standards, similar to those applied to the
evolution of the surface langage, and much higher than those for
internal compiler changes (optimizations, etc).

A key property of the standard library is its stability.  Backward
compatibility is not an absolute technical requirement (any addition
to/of a module can break existing code, formally), but breakage should
be limited as much as possible (and assessed, when relevant).  A
corollary is that any addition creates a long-term support commitment.
For instance, once a concrete type or function is made public,
changing the exposed definition cannot be done easily.

There is no plan to extend dramatically the functional domain covered
by the standard library.  For instance, proposals to include support
for XML, JSON, or network protocols are very likely to be rejected.  Such
domains are better treated by external libraries.  Small additions to
existing modules are much simpler to get in, even more so (but not
necessarily) when:

  - they cannot easily be implemented externally, or when
  - they facilitate communication between independent external
    libraries, or when
  - they fill obvious gaps.

Of course, standard guidelines apply as well: proper documentation,
proper tests, portability (yes, also Windows!), good justification for
why the change is desirable and why it should go into stdlib.

So: be prepared for some serious review process!  But yes, yes,
contributions are welcome and appreciated.  Promised.


## Contributor License Agreement

We distinguish two kind of contributions:

- Small changes that do not bear a specific mark of their authors
  (another developer recreating the change without access to the
  original patch would write an indistinguishable patch), and are thus
  not protected by copyright, do not require any particular
  paperwork. This is convenient for everyone, and of course does not
  mean that those contributions are of lesser importance. (For example
  a bugfix can be obvious once a bug is understood, reported and
  reproduced, and yet invaluable for users.)

- Larger changes that are covered by copyright. For them, we require
  contributors to sign a Contributor License Agreement (CLA), which
  gives [INRIA](http://www.inria.fr/en/) (Institut National de
  Recherche en Informatique et en Automatique) the rights to integrate
  the contribution, maintain it, evolve it, and redistribute it under
  the license of its choice. This is not a copyright *assignment*
  (as requested by the Free Software Foundation for example),
  contributors retain the copyright on their contribution, and can use
  it as they see fit. The OCaml CLA is lightly adapted from [the
  CLA](https://www.apache.org/licenses/icla.txt) of the Apache
  Foundation, and is available in two versions: [for individual
  contributors](http://caml.inria.fr/pub/docs/CLA-individual.doc) and
  [for corporations](http://caml.inria.fr/pub/docs/CLA-corporate.doc).

You must understand that, by proposing a contribution for integration
in the OCaml distribution, you accept that it be considered under one
of those regimes. In particular, in all cases you give INRIA the
permission to freely re-license the OCaml distribution including the
contribution.

This ability to re-license allows INRIA to provide members of the
[Caml Consortium](http://caml.inria.fr/consortium/) with a license on
the Caml code base that is more permissive than the public license.

### How to sign the CLA

If your contribution is large enough, you should sign the CLA. If you
are contributing on your own behalf, you should sign [the individual
CLA](http://caml.inria.fr/pub/docs/CLA-individual.doc). For corporate
contributions, if your employer has not already done so, they should
sign [the corporate
CLA](http://caml.inria.fr/pub/docs/CLA-corporate.doc). Review the CLA,
sign it, and send it -- scanned PDF by email, or postail mail -- to
Xavier Leroy ([contact
info](http://gallium.inria.fr/%7Exleroy/contact.html)).
