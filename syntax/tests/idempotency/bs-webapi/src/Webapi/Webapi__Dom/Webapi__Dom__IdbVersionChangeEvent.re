type t = Dom.idbVersionChangeEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "IDBVersionChangeEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "IDBVersionChangeEvent";

[@bs.get] external oldVersion : t => int = "";
[@bs.get] external newVersion : t => int = "";
