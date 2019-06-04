"use strict";

function addLink(element) {
  var a = document.createElement("a");
  a.innerHTML
  a.className = "anchor"
  a.href = "#" + encodeURIComponent(element.id);
  element.parentNode.insertBefore(a, element);
}

function addLinks() {
  document
    .querySelectorAll("[id]")
    .forEach(addLink);
}

document.addEventListener("DOMContentLoaded", function () {
  addLinks();
});