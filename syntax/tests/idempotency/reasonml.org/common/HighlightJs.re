[@bs.deriving abstract]
type highlightResult = {value: string};

[@bs.module "highlight.js/lib/highlight"]
external highlight: (~lang: string, ~value: string) => highlightResult =
  "highlight";
