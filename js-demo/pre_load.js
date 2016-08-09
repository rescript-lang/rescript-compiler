function contentFromResponse(gist) {
    $
        .ajax({ url: 'https://api.github.com/gists/' + gist })
        .done(function (resp) {
            var files = resp.files;
            var content = [];
            for (var file in files) {
                content.push(files[file].content)
            }
            if (content.length > 0) {
                myCode1Mirror.setValue(content[0]);
                return;
            } else {
                return;
            }
        });
    return;
};


function start(gist) {
    require(["./stdlib/arg", "./stdlib/array", "./stdlib/arrayLabels", "./stdlib/buffer", "./stdlib/bytes", "./stdlib/bytesLabels", "./stdlib/callback", "./stdlib/camlinternalFormat", "./stdlib/camlinternalFormatBasics", "./stdlib/camlinternalLazy", "./stdlib/camlinternalMod", "./stdlib/camlinternalOO", "./stdlib/char", "./stdlib/complex", "./stdlib/digest", "./stdlib/filename", "./stdlib/format", "./stdlib/gc", "./stdlib/genlex", "./stdlib/hashtbl", "./stdlib/int32", "./stdlib/int64", "./stdlib/lazy", "./stdlib/lexing", "./stdlib/list", "./stdlib/listLabels", "./stdlib/map", "./stdlib/marshal", "./stdlib/moreLabels", "./stdlib/nativeint", "./stdlib/obj", "./stdlib/oo", "./stdlib/parsing", "./stdlib/pervasives", "./stdlib/printexc", "./stdlib/printf", "./stdlib/queue", "./stdlib/random", "./stdlib/scanf", "./stdlib/set", "./stdlib/sort", "./stdlib/stack", "./stdlib/stdLabels", "./stdlib/stream", "./stdlib/string", "./stdlib/stringLabels", "./stdlib/sys", "./stdlib/weak", "./stdlib/block", "./stdlib/caml_array", "./stdlib/caml_backtrace", "./stdlib/caml_builtin_exceptions", "./stdlib/caml_exceptions", "./stdlib/caml_float", "./stdlib/caml_format", "./stdlib/caml_gc", "./stdlib/caml_hash", "./stdlib/caml_int32", "./stdlib/caml_int64", "./stdlib/caml_io", "./stdlib/caml_lexer", "./stdlib/caml_md5", "./stdlib/caml_module", "./stdlib/caml_obj", "./stdlib/caml_oo", "./stdlib/caml_parser", "./stdlib/caml_string", "./stdlib/caml_sys", "./stdlib/caml_weak", "./stdlib/curry", "./stdlib/js_primitive"], function () {
        if(gist){
            contentFromResponse(gist)
        }
        
        $.
        ajax(
        {url : "examples/examples.json",
            dataType : "json",
            cache: true})
        .done(function (response){
            examplesDataSet = response;
            for(var k in examplesDataSet){
                examplesDropdown.appendChild(createExample(k))
            }
            if(location && location.hash ){
                var id =  location.hash.substr(1)
                switchExample(id)
            }
        })
        .fail(function(xhr, textStatus, thrown){
            console.log(arguments)
        })

    })
}
function queryGist() {
    var qd = {};
    location.search.substr(1).split("&").forEach(
        function (item) {
            var s = item.split("="), k = s[0], v = s[1] && decodeURIComponent(s[1]); (k in qd) ? qd[k].push(v) : qd[k] = [v]
        }
    );
    return qd['gist'];
}