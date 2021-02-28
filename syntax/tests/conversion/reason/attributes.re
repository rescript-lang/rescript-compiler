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