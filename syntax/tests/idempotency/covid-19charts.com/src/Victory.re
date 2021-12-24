module Chart = {
  type scale = {
    x: string,
    y: string,
  };
  [@bs.module "victory"] [@react.component]
  external make:
    (
      ~animate: bool=?,
      ~width: float=?,
      ~height: float=?,
      ~children: React.element,
      ~scale: scale,
      ~groupComponent: React.element=?,
      ~containerComponent: React.element=?,
      ~height: float=?
    ) =>
    React.element =
    "VictoryChart";
};

module Label = {
  type origin = {
    x: float,
    y: float,
  };
  type props('a) = {
    datum: 'a,
    index: int,
    y: float,
    x: float,
    origin,
  };
  [@bs.module "victory"] [@react.component]
  external make:
    (
      ~labelPlacement: string=?,
      ~angle: float=?,
      ~dx: float=?,
      ~dy: float=?,
      ~className: string=?
    ) =>
    React.element =
    "VictoryLabel";
};

module Line = {
  [@unboxed]
  type xValue('a) =
    | XValue('a);

  let ofInt = (x: int) => XValue(x);
  let ofDate = (x: Js.Date.t) => XValue(x);
  let ofString = (x: string) => XValue(x);

  [@bs.module "victory"] [@react.component]
  external make:
    (
      ~name: string=?,
      ~data: array('a),
      ~x: 'a => xValue('b)=?,
      ~y: 'a => int=?,
      ~labels: Label.props('a) => string=?,
      ~interpolation: string=?,
      ~style: Js.t(_)=?,
      ~labelComponent: React.element=?
    ) =>
    React.element =
    "VictoryLine";
};

module Axis = {
  type tick;
  external tickToDate: tick => Js.Date.t = "%identity";
  external tickToInt: tick => int = "%identity";
  [@bs.module "victory"] [@react.component]
  external make:
    (
      ~fixLabelOverlap: bool=?,
      ~dependentAxis: bool=?,
      ~tickFormat: (tick, int) => string=?,
      ~label: string=?,
      ~padding: float=?,
      ~tickLabelComponent: React.element=?,
      ~style: Js.t(_)=?
    ) =>
    React.element =
    "VictoryAxis";
};

module Stack = {
  [@bs.module "victory"] [@react.component]
  external make: (~children: React.element) => React.element = "VictoryStack";
};

module Scatter = {
  type info('a) = {
    datum: 'a,
    active: bool
  };

  [@bs.module "victory"] [@react.component]
  external make:
    (
      ~name: string=?,
      ~style: Js.t(_)=?,
      ~data: array('a),
      ~x: 'a => Line.xValue('b)=?,
      ~y: 'a => int=?,
      ~size: info('a) => float=?
    ) =>
    React.element =
    "VictoryScatter";
};

module ClipContainer = {
  type padding = {
    top: float,
    right: float,
    bottom: float,
    left: float,
  };
  [@bs.module "victory"] [@react.component]
  external make: (~clipPadding: padding) => React.element =
    "VictoryClipContainer";
};

module VoronoiContainer = {
  [@bs.module "victory"] [@react.component]
  external make:
    (
      ~mouseFollowTooltips: bool=?,
      ~voronoiDimension: string=?,
      ~labels: Label.props('a) => string=?,
      ~voronoiBlacklist: array(string)=?
    ) =>
    React.element =
    "VictoryVoronoiContainer";
};
