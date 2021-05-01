module Color: {
  type t = pri string;

  [@bs.inline "red"] let red: t;
  [@bs.inline "black"] let black: t;
} = {
  type t = string;

  [@bs.inline] let red = "red";
  [@bs.inline] let black = "black";
};

[@bs.send] external map: (array('a), 'a => 'b) => array('b) = "map";
[@bs.send] external filter: (array('a), 'a => 'b) => array('b) = "filter";
[1, 2, 3]
  ->map(a => a + 1)
  ->filter(a => modulo(a, 2) == 0)
  ->Js.log;


type t;
[@bs.new] external make: unit => t = "DOMParser";
[@bs.send.pipe: t]
external parseHtmlFromString: (string, [@bs.as "text/html"] _) => Dom.htmlDocument = "parseFromString";

Js.log(make() |> parseHtmlFromString("sdsd"));
