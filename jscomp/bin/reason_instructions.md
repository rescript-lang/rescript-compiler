cp `menhir --suggest-menhirLib`/menhirLib.ml .
# cp `menhir --suggest-menhirLib`/menhirLib.mli .
# there is a bug
menhir --table reason_parser.mly
ocamllex reason_lexer.mll 

```
cat reason.ml | wc
40228  140902 2182768
```


# stand alone mode(seems even more code)
```
cat reason_parser.ml | wc
87567  610841 6882663
```

# Table mode

It depends on `Reason_toolchain` which is not stand alone any more,
depends on `Ocaml_re`

# Temporaryily removed, since it becomes more and more difficult 
to provide stand alone reason parser
