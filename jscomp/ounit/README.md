# OUnit

This is **NOT** the official source repository for [OUnit](http://ounit.forge.ocamlcore.org/index.php), which you can find on ocamlcore darcs [here](http://darcs.ocamlcore.org/cgi-bin/darcsweb.cgi?r=ounit;a=summary). Instead, it's a git mirror of that repository, maintained using [darcs-to-git](http://www.sanityinc.com/articles/converting-darcs-repositories-to-git).

Here's the procedure to pull patches from the upstream darcs repo:

```
git checkout upstream
curl https://raw.github.com/purcell/darcs-to-git/master/darcs-to-git > /tmp/darcs-to-git.rb
ruby -- /tmp/darcs-to-git.rb http://darcs.ocamlcore.org/repos/ounit
git checkout master
git merge upstream```

