/*
 * Copyright 2013 Laurent Bovet <laurent.bovet@windmaster.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

var scripts = document.getElementsByTagName('script');
var script = scripts[ scripts.length - 1 ];

if (script.attributes["data-schema"]) {
    var docson;
    if (script.attributes["data-docson"]) {
        docson = script.attributes["data-docson"].value;
    } else {
        docson = script.src.replace("widget.js", "index.html");
    }
    document.write("<iframe id='" + script.attributes["data-schema"].value +
        "' style='padding: 0; border: 0; width:100%; background: transparent' src='" +
        docson + "#" +
        script.attributes["data-schema"].value + "'></iframe>");
    function receiveMessage(event) {
        console.log("<", event)
        if (event.data.id && event.data.id == "docson") {
          var frame = document.getElementById(event.data.url);
          if(event.data.action == "resized") {
            frame.height = event.data.height + 18;
          }
          if(event.data.action == "ready") {
            console.log(frame.parentNode)
            frame.contentWindow.postMessage({ id: "docson", font: window.getComputedStyle(frame.parentNode).fontFamily}, "*");
          }
        }

    }
    window.addEventListener("message", receiveMessage, false);
} else {
    alert("<p style='color:red'>Missing data-schema (url to schema)</p>")
}
