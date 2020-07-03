type rgb = [#Red | #Green | #Blue]
type color = [rgb | #Orange | #Yellow | #Purple];

type t = [| #variant]
type t = [| #Variant]
type t = [| #\"type"]
type t = [| #\"va r ia nt"]
type t = [| #\"Variant ⛰"]

let id = (x: [> #Red | #Green | #Blue]) => x;
let upper = (x: [< #Red | #Green]) => true
type point = [ #Point(float, float) ];
type \"type" = [ #\"Point🗿"(\"let", float) ];
type shape = [
  | #Rectangle(point, point)
  | #Circle(point, float)
];

type madness = [< | #\"type" & (\"let") & (\"Super exotic") | #\"Bad Idea" ]

let error_of_exn: exn => option<[ | #Ok(error) | #Already_displayed ]> = x

external make: (
  ~_type: @bs.string
  [
  | #basis
  | #basisClosed
  | #basisOpen
  | #linear
  | #linearClosed
  | #natural
  | #monotoneX
  | #monotoneY
  | #monotone
  | #step
  | #stepBefore
  | #stepAfter
  ]=?,
  ~dataKey: Config.dataItem => Js.null<yValue>,
  ~stroke: string=?,
  ~strokeWidth: float=?,
  ~strokeDasharray: string=?,
  ~children: React.element=?,
  ~dot: Dot.t=?,
  ~activeDot: Dot.t=?,
  ~label: string=?,
  ~name: string=?,
  ~connectNulls: bool=?,
) => React.element = "Line"
