open GenTypeCommon;

type groupedArg =
  | Group(fields)
  | Arg(type_);

/**
 * For convenient processing turns consecutive named arguments into a
 * `NamedArgs` group, and individual non-named arguments into `Arg`s.
 */
let rec groupReversed = (~revCurGroup, ~revResult, labeledTypes) =>
  switch (revCurGroup, labeledTypes) {
  | ([], [(Nolabel, type_), ...tl]) =>
    groupReversed(
      ~revCurGroup=[],
      ~revResult=[Arg(type_), ...revResult],
      tl,
    )
  /* Add it to the current group, not result. */
  | (_, [(OptLabel(name), type_), ...tl]) =>
    groupReversed(
      ~revCurGroup=[
        {
          mutable_: Immutable,
          nameJS: name,
          nameRE: name,
          optional: Optional,
          type_,
        },
        ...revCurGroup,
      ],
      ~revResult,
      tl,
    )
  | (_, [(Label(name), type_), ...tl]) =>
    groupReversed(
      ~revCurGroup=[
        {
          mutable_: Immutable,
          nameJS: name,
          nameRE: name,
          optional: Mandatory,
          type_,
        },
        ...revCurGroup,
      ],
      ~revResult,
      tl,
    )
  | ([], []) => revResult
  | ([_grpHd, ..._grpTl], [] as _tl)
  /* Just form the group, and recurse ignoring the (None, t) in that case.
   * it will be handled in recursion. */
  | ([_grpHd, ..._grpTl], [(Nolabel, _), ..._tl]) =>
    groupReversed(
      ~revCurGroup=[],
      ~revResult=[Group(revCurGroup), ...revResult],
      labeledTypes,
    )
  };

/**
 * Special reverse that not only reverses the entire list but also the order of
 * items in the NamedArgs grouping.
 */
let rec reverse = (~soFar=[], lst) =>
  switch (lst) {
  | [] => soFar
  | [Arg(type_)] when type_ == unitT && soFar == [] =>
    // treat a single argument of type unit as no argument
    []
  | [Arg(type_), ...tl] =>
    reverse(~soFar=[{aName: "", aType: type_}, ...soFar], tl)
  | [Group(fields), ...tl] =>
    reverse(
      ~soFar=[
        {aName: "", aType: GroupOfLabeledArgs(List.rev(fields))},
        ...soFar,
      ],
      tl,
    )
  };

let group = labeledTypes =>
  labeledTypes |> groupReversed(~revCurGroup=[], ~revResult=[]) |> reverse;