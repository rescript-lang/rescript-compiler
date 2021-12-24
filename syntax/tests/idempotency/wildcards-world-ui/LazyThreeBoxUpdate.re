module type T = (module type of ThreeBoxUpdate.Main);

/*
   Needed for BuckleScript to not import the original component :
   See https://github.com/BuckleScript/bucklescript/issues/3543
 */
let unsafePlaceholder: module T = [%raw {|{}|}];

module UnsafePlaceholder = (val unsafePlaceholder);

let makeProps = UnsafePlaceholder.makeProps;

let make =
  ReLoadable.lazy_(() =>
    ReLoadable.import(UnsafePlaceholder.make, "./ThreeBoxUpdate.bs.js")
  );
