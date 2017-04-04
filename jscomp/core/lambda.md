


## Difference from Lambda

`Levent` is removed, it hurts pattern match peep-hole

Based on `ocaml/bytecomp/bytegen.ml`

```ocaml
| Lifused (_, exp) ->
    comp_expr env exp sz cont
```

`ocaml/bytecomp/simplif.ml`
```
  | Lifused(v, l) ->
      if count_var v > 0 then count bv l
```      

`ocaml/bytecomp/simplif.ml`
```
  | Lifused(v, l) ->
      if count_var v > 0 then simplif l else lambda_unit
```

`ocaml/bytecomp/translclass.ml`
```
List.fold_right
(fun (id, expr) rem ->
 lsequence (Lifused (id, set_inst_var obj id expr)) rem)
```