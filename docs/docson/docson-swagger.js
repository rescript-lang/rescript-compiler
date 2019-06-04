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

var allDefinitions;
var counter = 0;

function createDoc(definitions, type) {
    counter++;
    var docson = "/docson/index.html";
    if(!allDefinitions) {
        allDefinitions = {};
        function receiveMessage(event) {
            if (event.data.id && event.data.id == "docson") {
                var frame = $("#docson-"+event.data.url.split("$")[2]);
                if (event.data.action == "resized") {
                    frame.get(0).width = event.data.width + 18;
                    frame.get(0).height = event.data.height + 36;
                    frame.parents("td").width(event.data.width + 24)
                }
                if (event.data.action == "ready") {
                    frame.get(0).contentWindow.postMessage({ id: "docson", action: "load", definitions: allDefinitions, type: event.data.url.split("$")[1]}, "*");
                }
            }
        }
        window.addEventListener("message", receiveMessage, false);
    }
    $.extend(allDefinitions, definitions);
    return "<iframe class='docson-frame' id='docson-" + counter + "' style='padding: 0; border: 0; width:100%; background: transparent' src='" + docson + "#$" + type + "$"+counter+"'></iframe>"
}

SwaggerOperation.prototype.getSignature = function(type, models) {
    var collectionType, isPrimitive;
    if(this.isCollectionType) {
        collectionType = this.isCollectionType(type);
    } else {
        collectionType = this.isListType(type);
    }
    isPrimitive = ((collectionType != null) && models[collectionType]) || (models[type] != null) ? false : true;
    if (isPrimitive) {
        return type;
    } else {
        if (collectionType != null) {
            return "<p class='stronger'>" + type + "</p>" + createDoc(this.resource.rawModels, collectionType);
        } else {
            return createDoc(this.resource.rawModels, type);
        }
    }
};

$(document).on("click", ".toggleOperation", function() {
    $(this).parents(".operations").find(".docson-frame").each(function(k, frame) {
        frame.contentWindow.postMessage({id: "docson", action: "resize"}, "*");
    });
});
