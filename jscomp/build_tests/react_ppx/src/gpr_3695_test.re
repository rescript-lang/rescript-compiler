module React = {
    type element;
}

module Test = {
  [@bs.module] [@react.component]
  external make: (~className: string=?) => React.element = "Foo";
};

let test = Test.make(
  Test.makeProps(~className, ())
);

