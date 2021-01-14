type width =
  | AnyStr(string)
  | AnyStryArray(array(string))
  | Float(float)
  | FloatArray(array(float));

type wcColourString = [ | `green];

module Button = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (
      ~_as: string=?,
      ~className: string=?,
      ~href: string=?,
      ~target: string=?,
      ~mainColor: string=?,
      ~disabled: bool=?,
      ~children: React.element,
      ~m: int=?,
      ~onClick: ReactEvent.Form.t => unit=?,
      ~width: float=?
    ) =>
    React.element =
    "Button";

  module Text = {
    [@bs.module "rimble-ui"] [@bs.scope "Button"] [@react.component]
    external make:
      (
        ~_as: string=?,
        ~className: string=?,
        ~href: string=?,
        ~target: string=?,
        ~children: React.element=?,
        ~onClick: ReactEvent.Form.t => unit=?,
        ~icononly: bool=?,
        ~icon: string=?,
        ~color: string,
        ~position: string,
        ~top: int,
        ~right: int,
        ~height: string=?,
        ~size: string=?,
        ~m: int
      ) =>
      React.element =
      "Text";
  };
};

module Tooltip = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (
      ~message: string=?,
      ~placement: string=?,
      ~className: string=?,
      ~children: React.element,
      ~offset: string=?,
      ~onClick: ReactEvent.Form.t => unit=?
    ) =>
    React.element =
    "Tooltip";
};

module Icon = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (
      ~name: string=?,
      ~color: string=?,
      ~size: string=?,
      ~className: string=?
    ) =>
    React.element =
    "Icon";
};

module Input = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (
      ~_type: string,
      ~required: bool=?,
      ~placeholder: string=?,
      ~className: string=?,
      ~onChange: ReactEvent.Form.t => unit=?,
      ~value: string=?
    ) =>
    React.element =
    "Input";
};

module Loader = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (~className: string=?, ~color: [ | `green]=?, ~size: string=?) =>
    React.element =
    "Loader";
};

module Modal = {
  [@bs.module "rimble-ui"] [@react.component]
  external make: (~isOpen: bool, ~children: React.element) => React.element =
    "Modal";
};
module Card = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (
      ~width: width=?,
      ~className: string=?,
      ~p: int=?,
      ~children: React.element
    ) =>
    React.element =
    "Card";
};
module Heading = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (
      ~_as: string=?,
      ~fontSize: int=?,
      ~children: React.element,
      ~className: string=?
    ) =>
    React.element =
    "Heading";
};
module HeadingS = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (~_as: string=?, ~fontSize: int=?, ~children: string) => React.element =
    "Heading";
};
module Box = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (
      ~p: int=?,
      ~m: int=?,
      ~mb: int=?,
      ~mt: int=?,
      ~fontSize: int=?,
      ~children: React.element,
      ~width: array(float)=?,
      ~color: string=?,
      ~bg: string=?,
      ~className: string=?,
      ~onClickEvent: ReactEvent.Mouse.t => unit=?,
      ~onClick: unit => unit=?
    ) =>
    React.element =
    "Box";
};
module Text = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (
      ~children: React.element,
      ~className: string=?,
      ~color: string=?,
      ~fontFamily: string=?,
      ~fontSize: string=?,
      ~fontWeight: string=?,
      ~lineHeight: string=?,
      ~textAlign: string=?
    ) =>
    React.element =
    "Text";
};
module TextS = {
  [@bs.module "rimble-ui"] [@react.component]
  external make: (~children: string) => React.element = "Text";
};

module Flex = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (
      ~children: React.element,
      ~flexWrap: string=?,
      ~flexDirection: string=?,
      ~alignItems: string=?,
      ~alignContent: string=?,
      ~justifyContent: string=?,
      ~px: int=?,
      ~pb: int=?,
      ~pt: int=?,
      ~className: string=?
    ) =>
    React.element =
    "Flex";
};

module Table = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (~children: React.element, ~className: string=?) => React.element =
    "Table";
};

module Slider = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (
      ~value: string,
      ~className: string,
      ~onChange: ReactEvent.Form.t => unit=?,
      ~min: string,
      ~max: string,
      ~step: string
    ) =>
    React.element =
    "Slider";
};

module Form = {
  [@bs.module "rimble-ui"] [@react.component]
  external make:
    (
      ~onSubmit: ReactEvent.Form.t => unit=?,
      ~children: React.element,
      ~action: string=?,
      ~_method: string=?
    ) =>
    React.element =
    "Form";

  module Input = {
    [@bs.module "rimble-ui"] [@bs.scope "Form"] [@react.component]
    external make:
      (
        ~_type: string,
        ~required: bool=?,
        ~placeholder: string=?,
        ~name: string=?,
        ~className: string=?,
        ~onChange: ReactEvent.Form.t => unit=?,
        ~value: string=?,
        ~width: float=?
      ) =>
      React.element =
      "Input";
  };
};
