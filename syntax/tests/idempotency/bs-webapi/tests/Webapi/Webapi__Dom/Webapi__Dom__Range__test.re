open Webapi.Dom;
open Range;

let node = document |> Document.createElement("strong");

let range = make();

let _ = collapsed(range);
let _ = commonAncestorContainer(range);
let _ = endContainer(range);
let _ = endOffset(range);
let _ = startContainer(range);
let _ = startOffset(range);

setStart(node, 0, range);
setEnd(node, 0, range);
setStartBefore(node, range);
setStartAfter(node, range);
setEndBefore(node, range);
setEndAfter(node, range);
selectNode(node, range);
selectNodeContents(node, range);
collapse(range);
collapseToStart(range);
let _ = cloneContents(range);
deleteContents(range);
let _ = extractContents(range);
insertNode(node, range);
surroundContents(node, range);
let _ = compareBoundaryPoints(0, range, range);
let _ = cloneRange(range);
detach(range);
let _ = toString(range);
let _ = comparePoint(node, 0, range);
let _ = createContextualFragment("<strong>stuff</strong>", range);
let _ = getBoundingClientRect(range);
let _ = getClientRects(range);
let _ = intersectsNode(node, range);
let _ = isPointInRange(node, 0, range);
