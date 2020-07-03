type t;

[@bs.get] external _type : t => string = "type";

[@bs.get] external name : t => string = "name";

[@bs.get] external size : t => float = "size";

[@bs.get] external preview : t => string = "";
