[@bs.module "react-tabs"] [@react.component]
external make:
  (
    ~selectedIndex: int=?,
    ~onSelect: (int, int) => bool=?,
    ~children: React.element
  ) =>
  React.element =
  "Tabs";

module Tab = {
  [@bs.module "react-tabs"] [@react.component]
  external make: (~children: React.element) => React.element = "Tab";
};
module TabList = {
  [@bs.module "react-tabs"] [@react.component]
  external make: (~children: React.element) => React.element = "TabList";
};
module TabPanel = {
  [@bs.module "react-tabs"] [@react.component]
  external make: (~children: React.element) => React.element = "TabPanel";
};
