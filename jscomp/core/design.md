


# Global exports

Global exports identifiers are extracted from {!Translmod.get_export_identifiers} 
instead of inferred from lambda expression or cmi file.

1. We need be careful about externals.
2. Reading from fresh generated cmi is expensive.

# variable usage

Lalias-bound variables are never assigned, so it can only 
appear in `Lvar`, then it is easy to eliminate it 


# externals beta reduction

Note in general, it is fine whether we do beta reduction or not, it is just optimization. 

However, since we introduced `bs.splice` which does require the `spliced argument` to be captured

```ocaml
spliced_external a0 a1 [|b0;b1|]
```

There are two cases where get things complicated, people don't think `|>` is a function

```ocaml
x |> spliced_external a0 a1 [|b0;b1|]
```
Even though `|>` is a function, and `spliced_external` is escaped here, but people would 
expect it is equivalent to 

```ocaml
spliced_external a0 a1 [|b0;b1|] x 
```

So our optimizer needs to handle this case to make sure `spliced_external` not escaped, 
also becaues the interaction of `[@bs.splice]` and `[@bs.send]`, the spliced argument
is no longer  in tail position, so that people can write such code

```ocaml
spliced_external a0 a1 [|b0;b1|]
```
Internally in lambda layer it would be 

```ocaml
(fun c0 c1 c2 c3-> spliced_external c0 c1 c2 c3) a0 a1 [|b0;b1|]
```

We can simply do inlining, it may have side efffect in `b0`, `b1`, our optimizer also need handle such case.

Maybe in the future, we should lift the restriction about `bs.splice` (delegate to `slow` mode when we can not resolve it statically, my personal expereince is that people will complain about why it fails to compile more than why it is slow in some corner cases)

Note this also interacts with `[@bs.uncurry]`

for example

```ocaml
external filter : ('a -> bool [@bs.uncurry]) -> 'a array = "" [@@bs.send.pipe: 'a array]

let f xs =
    xs |> filter (fun x -> x > 2)
```