case $XARCH in
i386)
  ./configure
  make world.opt
  sudo make install
  (cd testsuite && make all)
  git clone git://github.com/ocaml/camlp4 -b 4.02
  (cd camlp4 && ./configure && make && sudo make install)
  git clone git://github.com/ocaml/opam
  (cd opam && ./configure && make lib-ext && make && sudo make install)
  opam init -y -a git://github.com/ocaml/opam-repository
  opam install -y utop
  ;;
*)
  echo unknown arch
  exit 1
  ;;
esac
