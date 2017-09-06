BuckleScript vendors OCaml sourcetree and Ninja using git subtree

https://www.atlassian.com/blog/git/alternatives-to-git-submodule-git-subtree


`rollup` is vendored only for testing at this time.

# Notes for changes to OCaml compiler

Suppose you add ocaml as remote

git remote add ocaml https://github.com/bucklescript/ocaml/

Add it:

git subtree add --prefix vendor/ocaml ocaml master

git subtree pull --prefix vendor/ocaml ocaml master

Push changes:

git subtree push --prefix vendor/ocaml ocaml master


