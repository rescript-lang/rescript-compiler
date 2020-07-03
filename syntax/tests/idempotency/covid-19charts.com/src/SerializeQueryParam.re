type encoded;
type coder('a) = {
  encode: 'a => option(encoded),
  decode: encoded => option('a),
};

[@bs.module "serialize-query-params"] [@bs.val]
external float: coder(float) = "NumberParam";
[@bs.module "serialize-query-params"] [@bs.val]
external int: coder(int) = "NumberParam";
[@bs.module "serialize-query-params"] [@bs.val]
external string: coder(string) = "StringParam";
[@bs.module "serialize-query-params"] [@bs.val]
external stringArray: coder(array(string)) = "ArrayParam";
[@bs.module "serialize-query-params"] [@bs.val]
external date: coder(Js.Date.t) = "DateParam";

type locationFragments = {
  protocol: string,
  host: string,
  pathname: string,
  search: string,
  state: Window.locationState
};

[@bs.module "serialize-query-params"]
external updateInLocation: (Js.Dict.t(option(encoded)), Window.location) => locationFragments =
  "updateInLocation";

[@bs.module "serialize-query-params"]
external parse: string => Js.Dict.t(encoded) = "parse";
