type t;

[@bs.get] external valueMissing : t => bool = "";
[@bs.get] external typeMismatch : t => bool = "";
[@bs.get] external patternMismatch : t => bool = "";
[@bs.get] external tooLong : t => bool = "";
[@bs.get] external tooShort : t => bool = "";
[@bs.get] external rangeUnderflow : t => bool = "";
[@bs.get] external rangeOverflow : t => bool = "";
[@bs.get] external stepMismatch : t => bool = "";
[@bs.get] external badInput : t => bool = "";
[@bs.get] external customError : t => bool = "";
[@bs.get] external valid : t => bool = "";
