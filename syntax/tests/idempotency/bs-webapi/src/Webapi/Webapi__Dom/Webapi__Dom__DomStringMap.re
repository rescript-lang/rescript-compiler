type t = Dom.domStringMap;

type key = string;

[@bs.get_index] [@bs.return nullable]
external get: (t, key) => option(string) = "";
let get = (key, map) => get(map, key);
[@bs.set_index] external set: (t, key, string) => unit = "";
let set = (key, value, map) => set(map, key, value);
let unsafeDeleteKey: (key, t) => unit =  
  [%raw (key, map) => "delete map[key];"];
  