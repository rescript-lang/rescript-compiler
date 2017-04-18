# Contributing to OCamlScript #

If you'd like to help us improve and extend OCamlScript and become a part of the OCamlScript community,
then we welcome your contributions! Below you will find some simple steps required to be able to contribute to OCamlScript. If you have
any questions about this process or any other aspect of contributing to a Bloomberg open source project, feel free to send an email to
[open-tech@bloomberg.net](mailto:open-tech@bloomberg.net) and we'll get your questions answered as quickly as we can.


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

- Larger changes that are covered by copyright. 

  Since OCamlScript is distributed under the terms of the
  [GPL Version 2.0](./LICENSE), contributions that you make to
  OCamlScript are licensed under the same terms. In order for us to be
  able to accept your contributions, we will need explicit
  confirmation from you that you are able and willing to provide them
  under these terms, and the mechanism we use to do this is called a
  Developer's Certificate of Origin
  [DCO](Developer-Certificate-of-Origin-1.1).  This is very similar to
  the process used by the Linux(R) kernel, Samba, and many other major
  open source projects.

  To participate under these terms, all that you must do is include a
  line like the following as the last line of the commit message for
  each commit in your contribution:

  ```
  Signed-Off-By: Random J. Developer <random@developer.example.org>
  ```

  You must use your real name (sorry, no pseudonyms, and no anonymous contributions).


## Coding guidelines

You should not leave trailing whitespace; not have line longer than 80
columns, not use tab characters (spaces only), and not use non-ASCII
characters. 

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


### Sending code contributions ###

Like other projects hosted on GitHub, you are of course free to fork our OCamlScript repository as many times as you wish, create branches and work
on your contributions in your fork. To prepare a specific contribution for considering (and eventually merging), there are a few steps you need
to follow:

* Ensure that your contribution is in its own branch in your fork of the OCamlScript repository (no other changes should be in the branch).
* Each commit in your branch that you are submitting must have *your* email address as the 'author'. If you are contributing on behalf of your employer (for example, if you are a Bloomberg employee) your commits must have your corporate email address in the 'author' field. If you need to configure this specifically for your clone of the OCamlScript repository (because you also work on other projects using ```git```), use ```git config user.email <address>``` in that repository clone to set the address for that single clone.
* Each commit in your branch must include a Signed-Off-By line indicating your acceptance of the contribution terms outlined above. This can be added by Git itself, by including the ```-s``` command line argument when you run ```git commit```.
* Use ```git rebase``` to ensure that your contribution applies cleanly to the *current* HEAD of the ```master``` branch in the OCamlScript repository. This also provides a good opportunity to 'squash' any commits in your branch that you'd rather not have live on in infamy!

After your pull request has been created, one of the OCamlScript team developers will review it, and may ask you questions and comment on your changes
in the GitHub issue associated with it. When they are satisfied that the code in the pull request is ready for further review and testing. When this process is complete, the contribution will be merged to ```master```
and your pull request will be closed.


